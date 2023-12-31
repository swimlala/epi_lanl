CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-01-23T00:35:18Z creation;2018-01-23T00:35:22Z conversion to V3.1;2019-12-19T07:51:09Z update;     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7T   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     88   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8X   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8\   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8d   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8h   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8p   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8x   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        d  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  `   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  s\   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     d  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �H   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �X   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �`   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �d   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �h   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180123003518  20200115121518  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA  I2_0576_202                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @�F�}��1   @�F�8� @; [�6��dV��
=q1   GPS     A   A   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @333@�  @�  A   A   A@  A`  A�  A�  A�33A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4�fD5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DufDu� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D�|�D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�<�D�|�D�� D���D�@ D؀ D�� D�  D�@ Dـ D�� D���D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�<�D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D��3D�3D�L�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @,(�@x��@�z�@�z�A=qA>=qA^=qA~=qA��A�Q�A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_��Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4\D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt�\Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�yHD��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��HD��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�yHD̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�9HD�yHD׼{D��HD�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��HD�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�9HD�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D���D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D���D���D�IH1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��!A��-A��A��A��!A��A��!A��A��A��!A��A��!A��!A��-A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A��7A��hA�r�A��A�S�A���A���A�jA�$�A��A�
=A��TA��DA�XA���A���A�v�A�bNA�"�A���A��;A��9A�z�A�ZA�  A���A�I�A��A�\)A��A�  A�C�A�1A�z�A�33A�M�A��jA�hsA�%A��A�"�A�r�A��7A�1'A��#A�VA��jA�oA���A��A��jA��+A�G�A��/A��A�A~~�A|��A{��Az�Ay?}Aw�AwO�Av~�Au�7At��AtJAsAsC�Ar�AnZAj�Ai��Ag�Af�AfAe��AeK�Ae�Ad1AcG�AaAap�Aa;dA`��A^�9A]��A]S�A\��A\�uA\{A[�AZJAY+AX~�AW��AW|�AWO�AV��AV��AVn�AV{AT=qAR��AQ"�AP��AO�AOl�AO�AN�`ANbNAM�hAM�AL�+AK��AK
=AJ�RAJ^5AJ5?AI�AI��AIx�AH�`AH-AG?}AE�AE
=AD��ADA�AC\)AAp�A?��A?�A=�A=
=A<�+A;|�A:ĜA9�A8��A7��A7?}A5?}A4v�A2��A1��A0��A/�
A/t�A.�`A.A�A.JA-�mA-��A-K�A-
=A+��A*��A*A�A)�;A)t�A)%A({A'7LA&Q�A%��A%K�A$��A#��A"�yA"I�A" �A"bA"A!�A!�
A!�A!+A �\A =qA��A�/AE�A9XA��A�mAM�A��A�A�FAAn�A33A$�A33A�jAffA|�A%AA"�AȴA�A
�/A
z�A
M�A	�FAffA�hA��A1'A�A�AC�A�/A�\Az�Ar�A�A|�AC�A M�@���@�@��@���@��@���@�M�@��@�Z@���@�"�@�n�@�@�ƨ@��y@�n�@�@�7@�p�@���@�t�@�-@�Ĝ@睲@�(�@�ȴ@��#@��/@�1@�"�@�-@�?}@�bN@�@��@�
=@�~�@�r�@ӥ�@���@�V@���@�Ĝ@�Q�@�ƨ@�33@���@Ώ\@��@̣�@˥�@ʰ!@�J@�b@��@�V@��/@�z�@��y@�V@��
@�K�@�1'@���@��+@�{@��/@��
@�-@��@���@�Z@���@�$�@�Ĝ@��
@�dZ@�5?@��/@�Z@��m@�l�@���@�$�@��@�z�@� �@��w@���@�|�@�t�@�l�@�K�@��@��H@���@�@�V@��@��9@�z�@�9X@��P@��w@�Q�@���@�%@�p�@��h@�/@���@�j@��@�+@���@�{@��@���@�Q�@���@�33@���@���@���@�%@��H@���@�G�@��@�A�@��@���@��D@�z�@��w@��@�M�@�$�@��T@�?}@�b@��@��!@�{@�@�hs@�G�@�?}@�%@�1'@��w@�S�@�;d@�o@�o@��R@�ff@��^@�p�@�/@��@��@�(�@���@��@��@�ȴ@�ff@��#@�X@��@�Ĝ@��9@��D@�bN@�9X@�b@���@��F@��P@�C�@��H@�-@��@�$�@�$�@�$�@��@��@�{@��@�@���@���@�x�@�O�@��@��u@�j@��@�S�@�@���@�p�@�z�@�;@�w@~�@~E�@}�T@}�@{t�@{33@z�@z^5@y�#@y�7@y&�@x1'@w\)@w
=@v�y@vȴ@v��@v��@v�+@v�+@vv�@vv�@vff@vff@vV@vE�@v5?@v{@u�@u@u�h@up�@up�@t�@t��@t9X@t1@s��@s@r�\@rn�@r=q@q��@q��@q��@q7L@p�9@p�@pA�@o\)@n��@m�-@l��@l�D@l�D@lz�@lI�@l(�@kS�@j�@jJ@i�@i��@iX@iX@iX@iX@i7L@i%@hĜ@h�u@hr�@hA�@h1'@g�w@g;d@f�R@e�@ep�@eO�@eO�@e�@d�@d��@d��@d��@d��@d�/@d��@d(�@c�
@c��@ct�@b�!@bM�@a�#@a7L@`�`@`�u@`bN@`b@_�w@_�P@_;d@^��@^ff@^{@]��@]�@]`B@]V@\Z@[�
@[�F@[�F@[��@[33@Z�@Z�!@Z~�@Z�@Y��@Y��@Y��@Y��@Y�^@Y��@YX@Y�@X��@XĜ@XA�@W�;@W�@Wl�@V�@V�+@U@U/@U�@T��@Tj@T1@Sƨ@S"�@R�!@R�\@Rn�@RM�@Q��@Qhs@Q&�@P�9@Pr�@P  @O�P@N��@N�R@Nff@N5?@M�@L�@L�j@LZ@L1@K��@K@J�H@J��@J~�@Ix�@I&�@H�`@H��@H�u@H �@G�;@G�P@G\)@F��@F�@Fȴ@FE�@E/@D�/@Dz�@DI�@D1@D1@C��@Cƨ@C�F@C�F@C�F@Cƨ@C�@C33@B��@BJ@A�#@A7L@@��@@r�@@A�@?�@?�w@?��@?|�@?l�@?\)@?+@?+@?�@>�y@>ȴ@>ȴ@>�R@>v�@>$�@=�@=��@=�@=p�@=/@=V@<�/@<��@<�j@<�@<j@<�@;�F@;��@;t�@;33@:�@:^5@9�@9��@9x�@9%@8�`@8��@8�u@8 �@8  @8  @7��@7|�@7�@6�R@6�R@6$�@5@5@5�h@5?}@5V@4�j@4�@4�D@4I�@3ƨ@3��@3�@3t�@3t�@3t�@3C�@2��@2n�@1��@1��@1��@1��@1x�@1X@17L@1%@0�`@0��@0�@0�@0  @/�;@/�w@/+@.�y@.��@.�+@.ff@.V@.E�@.5?@.{@-�T@-�-@-`B@-?}@,��@,j@,(�@+t�@*�@*�!@*�!@*�!@*�!@*�!@*�!@*�\@*M�@)�@)x�@)&�@)�@)%@(��@(��@(r�@(b@'�;@'|�@';d@'
=@&��@&�R@&��@&��@&v�@&ff@&V@&E�@&$�@%�T@%�-@%p�@%`B@%p�@%p�@%`B@%�@$��@$�@$��@$Z@$(�@#�m@#�F@#�@#dZ@#S�@#S�@#S�@#"�@"�@"��@"��@"�\@"=q@!�@!�#@!�#@!�^@!��@!x�@!hs@!hs@!G�@!�@ �`@ Ĝ@ ��@ Q�@  �@�P@+@�@�+@E�@@@�@��@�@�j@�D@1@�
@�F@��@t�@"�@�@�!@M�@J@�#@��@��@��@��@�7@hs@�`@Ĝ@r�@Q�@ �@�;@�@�P@\)@+@�@�y@ȴ@�R@v�@$�@��@�h@?}@V@��@�@��@Z@9X@1@�
@�F@��@��@dZ@33@"�@o@�H@�!@�\@~�@=q@�@J@�@�@�^@�^@�^@��@x�@hs@hs@X@X@X@&�@�@��@�`@�9@��@�@ �@�;@�w@�P@l�@\)@�@�@��@ff@5?@$�@{@@��@�@O�@�@�@��@j@j@Z@(�@�
@�@t�@dZ@S�@dZ@dZ@33@@
�H@
��@
�\@
M�@
=q@
=q@
-@
J@	��@	X@	&�@�`@��@r�@r�@bN@Q�@b@��@�P@|�@l�@\)@K�@
=@V@{@@�@�1111111111111111111111111111111111111111111111111111441411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��!A��-A��A��A��!A��A��!A��A��A��!A��A��!A��!A��-A��A��A��A��A��A��A��A��A��A��A��A��A���A���A���A��7A��hA�r�A��A�S�A���A���A�jA�$�A��A�
=A��TA��DA�XA���A���A�v�A�bNA�"�A���A��;A��9A�z�A�ZA�  A���A�I�A��A�\)A��A�  A�C�A�1A�z�A�33A�M�A��jA�hsA�%A��A�"�A�r�A��7A�1'A��#A�VA��jA�oA���A��A��jA��+A�G�A��/A��A�A~~�A|��A{��Az�Ay?}Aw�AwO�Av~�Au�7At��AtJAsAsC�Ar�AnZAj�Ai��Ag�Af�AfAe��AeK�Ae�Ad1AcG�AaAap�Aa;dA`��A^�9A]��A]S�A\��A\�uA\{A[�AZJAY+AX~�AW��AW|�AWO�AV��AV��AVn�AV{AT=qAR��AQ"�AP��AO�AOl�AO�AN�`ANbNAM�hAM�AL�+AK��AK
=AJ�RAJ^5AJ5?AI�AI��AIx�AH�`AH-AG?}AE�AE
=AD��ADA�AC\)AAp�A?��A?�A=�A=
=A<�+A;|�A:ĜA9�A8��A7��A7?}A5?}A4v�A2��A1��A0��A/�
A/t�A.�`A.A�A.JA-�mA-��A-K�A-
=A+��A*��A*A�A)�;A)t�A)%A({A'7LA&Q�A%��A%K�A$��A#��A"�yA"I�A" �A"bA"A!�A!�
A!�A!+A �\A =qA��A�/AE�A9XA��A�mAM�A��A�A�FAAn�A33A$�A33A�jAffA|�A%AA"�AȴA�A
�/A
z�A
M�A	�FAffA�hA��A1'A�A�AC�A�/A�\Az�Ar�A�A|�AC�A M�@���@�@��@���@��@���@�M�@��@�Z@���@�"�@�n�@�@�ƨ@��y@�n�@�@�7@�p�@���@�t�@�-@�Ĝ@睲@�(�@�ȴ@��#@��/@�1@�"�@�-@�?}@�bN@�@��@�
=@�~�@�r�@ӥ�@���@�V@���@�Ĝ@�Q�@�ƨ@�33@���@Ώ\@��@̣�@˥�@ʰ!@�J@�b@��@�V@��/@�z�@��y@�V@��
@�K�@�1'@���@��+@�{@��/@��
@�-@��@���@�Z@���@�$�@�Ĝ@��
@�dZ@�5?@��/@�Z@��m@�l�@���@�$�@��@�z�@� �@��w@���@�|�@�t�@�l�@�K�@��@��H@���@�@�V@��@��9@�z�@�9X@��P@��w@�Q�@���@�%@�p�@��h@�/@���@�j@��@�+@���@�{@��@���@�Q�@���@�33@���@���@���@�%@��H@���@�G�@��@�A�@��@���@��D@�z�@��w@��@�M�@�$�@��T@�?}@�b@��@��!@�{@�@�hs@�G�@�?}@�%@�1'@��w@�S�@�;d@�o@�o@��R@�ff@��^@�p�@�/@��@��@�(�@���@��@��@�ȴ@�ff@��#@�X@��@�Ĝ@��9@��D@�bN@�9X@�b@���@��F@��P@�C�@��H@�-@��@�$�@�$�@�$�@��@��@�{@��@�@���@���@�x�@�O�@��@��u@�j@��@�S�@�@���@�p�@�z�@�;@�w@~�@~E�@}�T@}�@{t�@{33@z�@z^5@y�#@y�7@y&�@x1'@w\)@w
=@v�y@vȴ@v��@v��@v�+@v�+@vv�@vv�@vff@vff@vV@vE�@v5?@v{@u�@u@u�h@up�@up�@t�@t��@t9X@t1@s��@s@r�\@rn�@r=q@q��@q��@q��@q7L@p�9@p�@pA�@o\)@n��@m�-@l��@l�D@l�D@lz�@lI�@l(�@kS�@j�@jJ@i�@i��@iX@iX@iX@iX@i7L@i%@hĜ@h�u@hr�@hA�@h1'@g�w@g;d@f�R@e�@ep�@eO�@eO�@e�@d�@d��@d��@d��@d��@d�/@d��@d(�@c�
@c��@ct�@b�!@bM�@a�#@a7L@`�`@`�u@`bN@`b@_�w@_�P@_;d@^��@^ff@^{@]��@]�@]`B@]V@\Z@[�
@[�F@[�F@[��@[33@Z�@Z�!@Z~�@Z�@Y��@Y��@Y��@Y��@Y�^@Y��@YX@Y�@X��@XĜ@XA�@W�;@W�@Wl�@V�@V�+@U@U/@U�@T��@Tj@T1@Sƨ@S"�@R�!@R�\@Rn�@RM�@Q��@Qhs@Q&�@P�9@Pr�@P  @O�P@N��@N�R@Nff@N5?@M�@L�@L�j@LZ@L1@K��@K@J�H@J��@J~�@Ix�@I&�@H�`@H��@H�u@H �@G�;@G�P@G\)@F��@F�@Fȴ@FE�@E/@D�/@Dz�@DI�@D1@D1@C��@Cƨ@C�F@C�F@C�F@Cƨ@C�@C33@B��@BJ@A�#@A7L@@��@@r�@@A�@?�@?�w@?��@?|�@?l�@?\)@?+@?+@?�@>�y@>ȴ@>ȴ@>�R@>v�@>$�@=�@=��@=�@=p�@=/@=V@<�/@<��@<�j@<�@<j@<�@;�F@;��@;t�@;33@:�@:^5@9�@9��@9x�@9%@8�`@8��@8�u@8 �@8  @8  @7��@7|�@7�@6�R@6�R@6$�@5@5@5�h@5?}@5V@4�j@4�@4�D@4I�@3ƨ@3��@3�@3t�@3t�@3t�@3C�@2��@2n�@1��@1��@1��@1��@1x�@1X@17L@1%@0�`@0��@0�@0�@0  @/�;@/�w@/+@.�y@.��@.�+@.ff@.V@.E�@.5?@.{@-�T@-�-@-`B@-?}@,��@,j@,(�@+t�@*�@*�!@*�!@*�!@*�!@*�!@*�!@*�\@*M�@)�@)x�@)&�@)�@)%@(��@(��@(r�@(b@'�;@'|�@';d@'
=@&��@&�R@&��@&��@&v�@&ff@&V@&E�@&$�@%�T@%�-@%p�@%`B@%p�@%p�@%`B@%�@$��@$�@$��@$Z@$(�@#�m@#�F@#�@#dZ@#S�@#S�@#S�@#"�@"�@"��@"��@"�\@"=q@!�@!�#@!�#@!�^@!��@!x�@!hs@!hs@!G�@!�@ �`@ Ĝ@ ��@ Q�@  �@�P@+@�@�+@E�@@@�@��@�@�j@�D@1@�
@�F@��@t�@"�@�@�!@M�@J@�#@��@��@��@��@�7@hs@�`@Ĝ@r�@Q�@ �@�;@�@�P@\)@+@�@�y@ȴ@�R@v�@$�@��@�h@?}@V@��@�@��@Z@9X@1@�
@�F@��@��@dZ@33@"�@o@�H@�!@�\@~�@=q@�@J@�@�@�^@�^@�^@��@x�@hs@hs@X@X@X@&�@�@��@�`@�9@��@�@ �@�;@�w@�P@l�@\)@�@�@��@ff@5?@$�@{@@��@�@O�@�@�@��@j@j@Z@(�@�
@�@t�@dZ@S�@dZ@dZ@33@@
�H@
��@
�\@
M�@
=q@
=q@
-@
J@	��@	X@	&�@�`@��@r�@r�@bN@Q�@b@��@�P@|�@l�@\)@K�@
=@V@{@@�@�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BBBBBBBBBBBBBBBBBBBBBBBBBBBBB+BDB1B  BBB{BhB\B�B{B\B	7B%BB  B	7B1BB  B��B��B�B�BBB�B&�B%�BBx�B[#B/Be`By�Bs�Be`B`BBe`B^5BS�BI�B5?BJB
��B
�B
�mB
��B
�B
�B
��B
��B
u�B
�B
�\B
z�B
_;B
gmB
[#B
W
B
W
B
E�B
?}B
F�B
=qB
6FB
1'B
2-B
0!B
#�B
\B	�HB	��B	�TB	�)B	�B	�/B	�BB	�B	�B	ǮB	ÖB	�RB	��B	�qB	�9B	��B	��B	�B	�B	��B	��B	��B	�hB	�JB	�uB	�JB	��B	�{B	�\B	�VB	�+B	~�B	l�B	ffB	\)B	k�B	gmB	gmB	ffB	e`B	_;B	W
B	W
B	R�B	M�B	M�B	N�B	M�B	M�B	K�B	H�B	F�B	@�B	:^B	49B	-B	+B	-B	)�B	�B	PB	B	\B	B	B	B��B��B�B�B�TB�5B��BɺB��B�XB�XB�RB�qB�RB�RB�jB�qB�XB�?B�!B��B��B��B��B��B��B��B�hB�hB�bB�uB�JB�7B�1B�=B�hB�oB�oB�bB�PB�+B�B}�B~�Bu�Bm�BhsBQ�BXBVBM�B\)BR�BN�BN�BR�BI�BK�BO�BQ�BP�BH�BG�BD�B>wBF�B>wB8RB@�BA�B8RB.B1'B5?B49B0!B.B33B6FB:^B;dB:^B5?B1'B33B)�B,B+B#�BuB�B&�B'�B$�B0!B0!B.B-B,B"�B+B1'B.B.B.B)�B#�B�B�B�B\B�B!�B �B#�B"�B �B�B�B�B\B�B�B�B�B�B�B�B�B#�B+B,B-B+B%�B�B�B�B�B�B�B&�B(�B'�B�B�B�B!�B�B"�B/B-B)�B+B-B0!B:^B8RB49B2-B6FB<jBB�B?}BB�BI�BL�BK�BK�BL�BL�BS�BW
BXBZB[#B\)B\)B[#B[#B[#B[#BYB\)BdZBe`BhsBk�Bl�Bq�B{�B}�B�B�B�1B�7B�JB�bB�\B�PB�hB�oB��B�hB��B��B��B��B��B��B��B��B��B��B��B��B�!B�RB�dB�dB�^B�dB��BǮBƨBĜB��BȴBǮB��B��B�B�B�B�B�#B�HB�ZB�fB�fB�fB�`B�ZB�TB�mB��B��B��B��B��B��B��B��B��B	B	B	B	1B	DB	VB	bB	{B	�B	�B	�B	�B	�B	�B	�B	)�B	-B	.B	.B	.B	/B	0!B	0!B	2-B	5?B	7LB	7LB	8RB	8RB	9XB	>wB	=qB	B�B	C�B	C�B	B�B	C�B	H�B	S�B	W
B	XB	YB	YB	YB	dZB	ffB	hsB	k�B	m�B	m�B	m�B	q�B	u�B	w�B	x�B	x�B	y�B	y�B	y�B	y�B	y�B	z�B	z�B	z�B	z�B	z�B	{�B	|�B	|�B	~�B	~�B	� B	~�B	�B	�B	�B	�B	�B	�+B	�7B	�=B	�=B	�DB	�DB	�DB	�PB	�bB	�bB	�bB	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	�9B	�?B	�FB	�RB	�jB	�jB	�qB	�qB	�qB	�wB	�}B	��B	��B	B	B	ÖB	ƨB	ǮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�#B	�)B	�/B	�/B	�5B	�5B	�5B	�5B	�HB	�HB	�NB	�TB	�ZB	�TB	�TB	�`B	�sB	�sB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
  B
B
B
B
B
B
%B
1B
1B
+B
%B

=B
DB
JB
DB
DB
PB
PB
VB
VB
\B
\B
VB
PB
oB
uB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
!�B
 �B
 �B
!�B
"�B
#�B
#�B
#�B
%�B
%�B
&�B
(�B
)�B
+B
+B
+B
+B
.B
/B
/B
/B
/B
.B
.B
/B
/B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
2-B
33B
49B
33B
5?B
5?B
6FB
6FB
7LB
7LB
6FB
6FB
6FB
6FB
6FB
7LB
6FB
7LB
7LB
5?B
7LB
9XB
:^B
:^B
:^B
:^B
:^B
9XB
9XB
:^B
:^B
;dB
<jB
<jB
<jB
<jB
<jB
<jB
=qB
<jB
=qB
=qB
>wB
>wB
>wB
?}B
?}B
?}B
?}B
?}B
?}B
?}B
?}B
>wB
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
G�B
G�B
G�B
H�B
I�B
H�B
J�B
J�B
K�B
K�B
J�B
I�B
J�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
M�B
N�B
O�B
O�B
P�B
P�B
P�B
P�B
O�B
O�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
R�B
S�B
T�B
T�B
VB
W
B
VB
VB
VB
W
B
W
B
W
B
XB
YB
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
\)B
[#B
\)B
\)B
[#B
[#B
\)B
\)B
[#B
\)B
\)B
[#B
[#B
]/B
]/B
]/B
^5B
]/B
]/B
^5B
^5B
^5B
_;B
_;B
_;B
^5B
`BB
_;B
`BB
`BB
`BB
_;B
bNB
bNB
aHB
aHB
aHB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
cTB
cTB
dZB
e`B
e`B
e`B
dZB
cTB
dZB
e`B
e`B
ffB
ffB
gmB
gmB
ffB
ffB
e`B
hsB
gmB
hsB
gmB
gmB
ffB
e`B
hsB
iyB
jB
jB
j1111111111111111111111111111111111111111111111111111441411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   BB'BBB'B-B-BBB-BBB'B'B'BBB'BB-B'BBBBBABAB3BEB^B�BUBtB?B�BB�B�B�B�B	�B�B�B �B	�B�B�B iB�HB��B�;B��G�O�G�O�G�O�G�O�G�O�B~�Ba�B4�Bg�Bz�Bt�BgRBa|Bf2B_;BU2BK)B7�B�B
�	B
�wB
�B
�B
ۦB
�sB
��B
�RB
{B
��B
��B
}B
bNB
h�B
]/B
X_B
XB
G�B
A B
GzB
>�B
7�B
2GB
2�B
0�B
$�B
hB	��B	��B	�B	�B	רB	��B	��B	ڠB	ؓB	�B	ĶB	�B	�B	��B	�tB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�aB	�PB	��B	��B	��B	��B	��B	�B	n�B	h>B	^B	l"B	hXB	h
B	f�B	e�B	`B	XB	W�B	S�B	N�B	N�B	O\B	NVB	N<B	L0B	I7B	GEB	A�B	;B	5�B	.�B	,=B	-�B	*�B	!HB	�B	?B	.B	�B	B	B�JB��B�OB��B�B�pB�6B�BÖB�0B��B��B�(B�$B�>B��B��B��B��B��B��B�B��B��B��B��B��B��B��B�hB�FB��B�XB�RB�B��B��B��B��B��B��B��B~�B�Bv�Bn�Bi�BT�BY�BW�BO�B\�BTaBP}BO�BS�BK�BM6BQBR�BQ�BJ	BH�BE�B?�BGEB?�B9�BABBB9rB/�B2|B6`B5tB1�B/�B49B6�B:�B;�B:�B6B1�B3�B+QB,�B+�B%FBB�B'�B(�B%�B0�B0�B.�B-�B,�B$tB+�B1�B.�B.}B.cB*�B$�B �B�B�B�B�B"�B!�B$tB#�B!�B �B~B�B BBpBBdBOB!B!B�B$ZB+kB,�B-wB+�B&�B �B�BxB�BB5B'mB)_B(�B 'B�B�B"�B�B#�B/iB-�B*�B,B./B1B:�B8�B5B3hB7fB=<BCB@�BC�BJ#BM6BLdBLdBM�BM�BTaBWYBX_BZQB[WB\]B\]B[qB[qB[qB[qBY�B\�Bd�Be�Bh�Bk�BmBq�B{�B}�B��B�B�KB��B��B��B��B��B��B��B��B�:B��B�EB�OB��B��B�FB��B�B��B�NB�hB�B��B��B��B��B��B�B�B��B�B�SB�uB�B�fB�dB�TB�SB�EB�KB�eB��B�B�B�B�B�B��B��B��B��B�B�2B�8B�RB�0B�JB�PB�]B�}B	�B	{B	�B	fB	xB	�B	�B	�B	�B	�B	�B	�B	B	B	 BB	*B	-B	.B	.IB	.IB	/OB	0;B	0oB	2aB	5ZB	7�B	7�B	8�B	8�B	9�B	>�B	>B	B�B	C�B	D3B	CaB	DMB	I7B	TB	WYB	X_B	YeB	Y�B	Y�B	dtB	f�B	h�B	k�B	m�B	m�B	nB	q�B	u�B	w�B	y	B	y	B	y�B	zB	y�B	y�B	y�B	z�B	z�B	z�B	z�B	{B	|B	}"B	}B	.B	.B	�4B	.B	�;B	�AB	�-B	�GB	�gB	�_B	�RB	�rB	�rB	�xB	�xB	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�0B	�B	�QB	�B	��B	�9B	�ZB	�zB	��B	��B	�jB	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�(B	�B	�B	�B	��B	�B	� B	�B	�B	�&B	�B	�@B	�9B	�?B	�_B	�=B	�CB	�IB	�dB	�OB	�jB	ބB	ބB	�|B	�|B	�B	�B	�B	�B	�B	�B	�sB	�sB	�B	��B	�B	�B	�B	�B	�B	�B	��B	��B	�B	��B	��B	��B	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	�B	�B	�B	�B	�0B	�B	�"B	�<B	�B	�"B	�(B	�HB
 B
 OB
;B
 OB
UB
MB
MB
SB
mB
tB
KB
fB
_B
�B

rB
xB
dB
xB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
�B
 �B
!�B
 �B
!B
"B
#B
$B
$B
$B
%�B
%�B
'B
)*B
*B
+6B
+6B
+B
+QB
.IB
/B
/5B
/B
/5B
.IB
.cB
/OB
/iB
1AB
2aB
2aB
2GB
2aB
2aB
2aB
3hB
3hB
3hB
3hB
2|B
3MB
4nB
3hB
5ZB
5ZB
6zB
6zB
7fB
7LB
6zB
6`B
6`B
6`B
6`B
7�B
6�B
7�B
7�B
5�B
7�B
9�B
:xB
:xB
:^B
:^B
:xB
9�B
9rB
:�B
:�B
;B
<�B
<�B
<�B
<�B
<�B
<�B
=�B
<�B
=�B
=�B
>�B
>�B
>�B
?}B
?�B
?�B
?}B
?�B
?�B
?�B
?�B
>�B
?�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
B�B
C�B
D�B
D�B
D�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
F�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
G�B
G�B
G�B
H�B
I�B
H�B
J�B
J�B
K�B
K�B
J�B
I�B
J�B
K�B
LB
L�B
NB
M�B
NB
NB
M�B
OB
N"B
OB
O�B
O�B
Q B
Q B
Q B
Q B
O�B
P.B
QB
Q B
RB
RB
RB
RB
S&B
SB
SB
S�B
T,B
T,B
T,B
TB
S&B
TB
U2B
UB
V9B
W
B
VB
V9B
VB
W?B
W$B
W$B
XEB
Y1B
XEB
X+B
X+B
YB
YKB
Y1B
YKB
Y1B
YKB
Y1B
ZQB
Z7B
ZQB
[=B
[WB
[=B
[#B
[=B
[WB
[=B
\)B
[WB
\CB
\CB
[WB
[WB
\)B
\)B
[=B
\CB
\]B
[WB
[=B
]dB
]dB
]IB
^5B
]dB
]dB
^OB
^jB
^jB
_VB
_;B
_VB
^�B
`vB
_VB
`\B
`\B
`vB
_pB
bNB
bNB
a|B
a|B
abB
cnB
cTB
cnB
cTB
cnB
c�B
c�B
dtB
c�B
cnB
dtB
ezB
e`B
e`B
dtB
c�B
d�B
e�B
e�B
f�B
f�B
gmB
gmB
f�B
f�B
e�B
h�B
g�B
hsB
g�B
g�B
f�B
e�B
h�B
iyB
jB
j�B
j�1111111111111111111111111111111111111111111111111111444441111111111111111111131111111111111111111131111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111311111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.11(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201801270033502018012700335020180127003350201806221236412018062212364120180622123641201804050433152018040504331520180405043315  JA  ARFMdecpA19c                                                                20180123093516  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180123003518  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180123003520  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180123003521  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180123003521  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180123003521  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180123003521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180123003521  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180123003521  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180123003521  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180123003522  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180123003522                      G�O�G�O�G�O�                JA  ARUP                                                                        20180123005533                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180123153429  CV  JULD            G�O�G�O�F�5�                JM  ARSQJMQC2.0                                                                 20180124000000  CF  PSAL_ADJUSTED_QCB�  C�  G�O�                JM  ARSQJMQC2.0                                                                 20180124000000  CF  TEMP_ADJUSTED_QCB�  B�  G�O�                JM  ARCAJMQC2.0                                                                 20180126153350  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180126153350  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404193315  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622033641  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115121518                      G�O�G�O�G�O�                