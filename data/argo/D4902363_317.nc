CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2019-01-03T00:36:29Z creation;2019-01-03T00:36:34Z conversion to V3.1;2019-12-19T07:24:15Z update;     
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
resolution        =���   axis      Z        p  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \X   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  `4   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  o�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  s�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �<   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     p  �d   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     p  �D   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ۴   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �D   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �D   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �D   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �D   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �$   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �<   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �@   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �DArgo profile    3.1 1.2 19500101000000  20190103003629  20200115131517  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              =A   JA  I2_0576_317                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @؜�C � 1   @؜�333 @9�'RT`��dDH��1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CA�fCD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^�C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7�fD8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DOy�DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�C3Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�3D�331111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@x��@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA�=CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]�qC_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7\D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOr�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�?�D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D���D���D�/�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�hsA�dZA�\)A�XA�XA�XA�XA�^5A�l�A�r�A�p�A�S�A�7LA��A��A��mA��!A��DA�t�A�S�A�O�A�O�A�A�A�+A�oA���A��A��yA��;A��#A��A��A��A��A��A���A���A���A���A���A���A���A���A���A��jA��\A�M�A�33A�-A�r�A���A�z�A�1'A��hA�Q�A��7A��`A���A��A��!A�ȴA� �A��9A�%A���A�p�A���A�A���A��RA�`BA��A���A�G�A��mA��A�^5A�{A���A���A��`A�ȴA���A��A�&�A�+A���A���A��`A�+A���A�hsA��A��FA���A��\A�1A��;A��A�{A��/A��#A���A���A���A���A�#A|�Ay�AxȴAu�-Aq��AqVAo�mAn�DAmƨAml�Am7LAm�Al��AlM�Ak�TAk7LAi�Ae�TAcx�Ab�RAb�Aa;dA`  A]A[��AZ�AYAXn�AWx�AV��AV9XASARM�AQ��AQ�APM�AO�
AO�AO7LAN��AM`BALffALJAK��AK��AK�wAK|�AK%AI�;AG/AE��AEdZAD �ABr�ABjABbNABffAA��A@(�A?�A>VA=ƨA<JA:��A97LA8�jA8v�A8A6��A4$�A2�A0��A/�A.��A.I�A.=qA.A-��A-l�A-+A+A*��A(�DA&�A%��A$�+A#�A#�A"��A"��A"=qA!ƨA!XA bNAx�A�AƨA�A��AbNAbA|�Ap�AĜAJAx�Ar�A�
AG�A��AJA;dAffA�AG�A�A&�AI�AA��A�A(�A  A��A��A�7A\)AoA
{A	A	�PA	?}A�`A�uA$�A��A�A\)A��AJA/A�A ZA J@��@�;d@�o@��!@�-@��^@�7L@���@���@���@���@��@�J@��@�1@�33@�$�@�/@�j@�t�@�v�@�@�9@��`@�"�@�`B@��m@��@���@�z�@��@�
=@�-@��;@�=q@�V@���@��@�9X@�@�v�@��/@�(�@ˍP@�@�`B@���@�I�@Ǖ�@���@�@�?}@ě�@���@ÍP@���@��
@�\)@�{@�Ĝ@�j@��m@�t�@��@�j@���@���@���@�j@��w@�l�@�M�@�X@��@���@��@��@�bN@�A�@�A�@�A�@�I�@�r�@��u@��u@��w@�5?@��#@�%@��9@��@�(�@��@�K�@���@�~�@��^@���@�G�@��@�%@�9X@�S�@���@�$�@��#@�hs@���@��u@��;@��\@�V@��u@��D@��@�Z@��P@�"�@�o@��@��\@���@���@�X@��@���@��@�I�@�  @��w@���@�|�@�C�@�o@���@��@�^5@��#@�G�@�9X@��
@�t�@�;d@���@���@�=q@�Ĝ@� �@�  @��F@�;d@�
=@��R@�n�@�E�@��@�?}@�%@���@���@�9X@�b@��@�l�@�o@��y@��H@��H@���@��!@���@�ff@�V@�J@��h@�7L@���@���@���@�bN@�(�@�1@���@��m@��
@���@�K�@�
=@���@�{@��^@���@�hs@��@��u@�j@�I�@�1@��F@�S�@�33@��@�@���@��H@�ȴ@��!@�~�@�-@��@�x�@�`B@�G�@�%@��D@�@��@l�@~��@~��@~$�@}��@}�@|�D@|j@|I�@|�@{��@{dZ@{C�@{o@z��@z~�@y�#@y�7@yG�@y%@x�`@x�@xr�@xQ�@w�@w|�@w+@v�y@v5?@v{@v{@u�@t��@tz�@sƨ@s��@s@r��@rn�@rM�@q�^@q�@p  @o�@o+@m��@l�@l�D@l(�@k"�@i�@i&�@h��@hQ�@g|�@gl�@g+@g�@f�@e�T@e�h@eO�@e?}@e/@eV@d��@d�j@dz�@c�m@c�@ct�@cdZ@cS�@cC�@c"�@c@b��@b�@`��@_�;@_l�@_
=@^v�@^E�@^$�@]��@]`B@]V@\�@[�m@[ƨ@[�F@[�F@[��@[�@[t�@[C�@["�@Zn�@Y��@Y�^@Y�@X�u@X�@W�@W|�@V�@V@U@U@U�h@Up�@UV@T��@TZ@T(�@S�F@R��@R��@R�\@RM�@R�@RJ@Q��@Q�#@Q�^@Qx�@PĜ@PQ�@P  @O��@O�@O;d@N�@N��@Nff@N5?@N{@M�h@M?}@M?}@M�@L�/@L�/@L�j@L9X@Kƨ@K"�@J=q@I7L@H��@H�u@G��@G\)@GK�@G;d@G�@G
=@G
=@G
=@F��@F��@F�@F�+@F@E��@E�h@Ep�@EO�@E/@D�/@D��@DI�@D1@C��@C�m@C��@B�H@B-@BJ@A��@A�#@A�#@A��@AG�@A�@A�@A%@@��@@Ĝ@@�u@@1'@?�;@?|�@?K�@?+@?�@>��@>��@>�+@>�+@>v�@>{@=�@<z�@<I�@<1@;�
@;�
@;�
@;��@;S�@;33@;o@:��@:�\@:~�@:~�@:^5@:-@9��@9hs@9X@9X@97L@8Ĝ@8�@8bN@8Q�@81'@8 �@8  @7�w@7l�@7K�@7
=@6�@6�R@6�+@6$�@5`B@5/@5V@4�@4��@4��@41@3�
@3�F@333@3o@2��@2�\@2n�@2n�@2=q@2J@1�@1��@1��@1x�@1G�@0�`@0r�@0Q�@0A�@01'@0b@0  @/��@/�@/��@/K�@/;d@.��@.�R@.V@-��@-p�@-`B@-`B@-O�@-/@,��@,�/@,��@,�/@,��@,�j@,�D@,Z@,�@+�
@+��@+S�@+"�@+@*�H@*��@*��@*=q@*�@)��@)�^@)x�@)&�@(��@(r�@'|�@';d@'+@'�@&��@&��@&v�@&V@&V@&{@%�h@%�@%�@%p�@%p�@%p�@%/@$��@$I�@$�@#t�@"��@"n�@"n�@"n�@"M�@"-@!�#@!x�@!hs@!&�@ �`@ r�@  �@�w@l�@;d@�@��@ȴ@v�@5?@@��@`B@?}@V@�j@(�@1@�
@��@t�@C�@C�@"�@�H@��@~�@^5@M�@J@��@�7@hs@G�@7L@&�@%@��@��@��@Ĝ@r�@�w@�@��@��@��@��@�P@l�@K�@+@��@�+@V@{@@�@?}@�@��@�/@�/@�j@�@�D@Z@9X@��@ƨ@t�@"�@�!@-@��@��@x�@hs@�@Ĝ@�@bN@Q�@1'@1'@b@  @�;@�@K�@�@�y@ȴ@��@V@�T@?}@/@�@��@�/@�j@�j@�j@�@�@�@�@j@(�@�
@ƨ@�F@t�@C�@"�@
�@
�!@
~�@
M�@
-@
J@	�@	�^@	x�@	7L@	�@	%@	%@�9@�@Q�@A�@A�@A�@A�@  @�w@��@�P@|�@l�@;d@+@+@�@
=@��@�@�@ȴ@ȴ@ȴ@�R@��@V@�@�-@�h@p�@O�@�@��@�/@j@I�@(�@(�@�@1@�
@�
@ƨ@�@t�@C�@��@��@~�@n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�hsA�dZA�\)A�XA�XA�XA�XA�^5A�l�A�r�A�p�A�S�A�7LA��A��A��mA��!A��DA�t�A�S�A�O�A�O�A�A�A�+A�oA���A��A��yA��;A��#A��A��A��A��A��A���A���A���A���A���A���A���A���A���A��jA��\A�M�A�33A�-A�r�A���A�z�A�1'A��hA�Q�A��7A��`A���A��A��!A�ȴA� �A��9A�%A���A�p�A���A�A���A��RA�`BA��A���A�G�A��mA��A�^5A�{A���A���A��`A�ȴA���A��A�&�A�+A���A���A��`A�+A���A�hsA��A��FA���A��\A�1A��;A��A�{A��/A��#A���A���A���A���A�#A|�Ay�AxȴAu�-Aq��AqVAo�mAn�DAmƨAml�Am7LAm�Al��AlM�Ak�TAk7LAi�Ae�TAcx�Ab�RAb�Aa;dA`  A]A[��AZ�AYAXn�AWx�AV��AV9XASARM�AQ��AQ�APM�AO�
AO�AO7LAN��AM`BALffALJAK��AK��AK�wAK|�AK%AI�;AG/AE��AEdZAD �ABr�ABjABbNABffAA��A@(�A?�A>VA=ƨA<JA:��A97LA8�jA8v�A8A6��A4$�A2�A0��A/�A.��A.I�A.=qA.A-��A-l�A-+A+A*��A(�DA&�A%��A$�+A#�A#�A"��A"��A"=qA!ƨA!XA bNAx�A�AƨA�A��AbNAbA|�Ap�AĜAJAx�Ar�A�
AG�A��AJA;dAffA�AG�A�A&�AI�AA��A�A(�A  A��A��A�7A\)AoA
{A	A	�PA	?}A�`A�uA$�A��A�A\)A��AJA/A�A ZA J@��@�;d@�o@��!@�-@��^@�7L@���@���@���@���@��@�J@��@�1@�33@�$�@�/@�j@�t�@�v�@�@�9@��`@�"�@�`B@��m@��@���@�z�@��@�
=@�-@��;@�=q@�V@���@��@�9X@�@�v�@��/@�(�@ˍP@�@�`B@���@�I�@Ǖ�@���@�@�?}@ě�@���@ÍP@���@��
@�\)@�{@�Ĝ@�j@��m@�t�@��@�j@���@���@���@�j@��w@�l�@�M�@�X@��@���@��@��@�bN@�A�@�A�@�A�@�I�@�r�@��u@��u@��w@�5?@��#@�%@��9@��@�(�@��@�K�@���@�~�@��^@���@�G�@��@�%@�9X@�S�@���@�$�@��#@�hs@���@��u@��;@��\@�V@��u@��D@��@�Z@��P@�"�@�o@��@��\@���@���@�X@��@���@��@�I�@�  @��w@���@�|�@�C�@�o@���@��@�^5@��#@�G�@�9X@��
@�t�@�;d@���@���@�=q@�Ĝ@� �@�  @��F@�;d@�
=@��R@�n�@�E�@��@�?}@�%@���@���@�9X@�b@��@�l�@�o@��y@��H@��H@���@��!@���@�ff@�V@�J@��h@�7L@���@���@���@�bN@�(�@�1@���@��m@��
@���@�K�@�
=@���@�{@��^@���@�hs@��@��u@�j@�I�@�1@��F@�S�@�33@��@�@���@��H@�ȴ@��!@�~�@�-@��@�x�@�`B@�G�@�%@��D@�@��@l�@~��@~��@~$�@}��@}�@|�D@|j@|I�@|�@{��@{dZ@{C�@{o@z��@z~�@y�#@y�7@yG�@y%@x�`@x�@xr�@xQ�@w�@w|�@w+@v�y@v5?@v{@v{@u�@t��@tz�@sƨ@s��@s@r��@rn�@rM�@q�^@q�@p  @o�@o+@m��@l�@l�D@l(�@k"�@i�@i&�@h��@hQ�@g|�@gl�@g+@g�@f�@e�T@e�h@eO�@e?}@e/@eV@d��@d�j@dz�@c�m@c�@ct�@cdZ@cS�@cC�@c"�@c@b��@b�@`��@_�;@_l�@_
=@^v�@^E�@^$�@]��@]`B@]V@\�@[�m@[ƨ@[�F@[�F@[��@[�@[t�@[C�@["�@Zn�@Y��@Y�^@Y�@X�u@X�@W�@W|�@V�@V@U@U@U�h@Up�@UV@T��@TZ@T(�@S�F@R��@R��@R�\@RM�@R�@RJ@Q��@Q�#@Q�^@Qx�@PĜ@PQ�@P  @O��@O�@O;d@N�@N��@Nff@N5?@N{@M�h@M?}@M?}@M�@L�/@L�/@L�j@L9X@Kƨ@K"�@J=q@I7L@H��@H�u@G��@G\)@GK�@G;d@G�@G
=@G
=@G
=@F��@F��@F�@F�+@F@E��@E�h@Ep�@EO�@E/@D�/@D��@DI�@D1@C��@C�m@C��@B�H@B-@BJ@A��@A�#@A�#@A��@AG�@A�@A�@A%@@��@@Ĝ@@�u@@1'@?�;@?|�@?K�@?+@?�@>��@>��@>�+@>�+@>v�@>{@=�@<z�@<I�@<1@;�
@;�
@;�
@;��@;S�@;33@;o@:��@:�\@:~�@:~�@:^5@:-@9��@9hs@9X@9X@97L@8Ĝ@8�@8bN@8Q�@81'@8 �@8  @7�w@7l�@7K�@7
=@6�@6�R@6�+@6$�@5`B@5/@5V@4�@4��@4��@41@3�
@3�F@333@3o@2��@2�\@2n�@2n�@2=q@2J@1�@1��@1��@1x�@1G�@0�`@0r�@0Q�@0A�@01'@0b@0  @/��@/�@/��@/K�@/;d@.��@.�R@.V@-��@-p�@-`B@-`B@-O�@-/@,��@,�/@,��@,�/@,��@,�j@,�D@,Z@,�@+�
@+��@+S�@+"�@+@*�H@*��@*��@*=q@*�@)��@)�^@)x�@)&�@(��@(r�@'|�@';d@'+@'�@&��@&��@&v�@&V@&V@&{@%�h@%�@%�@%p�@%p�@%p�@%/@$��@$I�@$�@#t�@"��@"n�@"n�@"n�@"M�@"-@!�#@!x�@!hs@!&�@ �`@ r�@  �@�w@l�@;d@�@��@ȴ@v�@5?@@��@`B@?}@V@�j@(�@1@�
@��@t�@C�@C�@"�@�H@��@~�@^5@M�@J@��@�7@hs@G�@7L@&�@%@��@��@��@Ĝ@r�@�w@�@��@��@��@��@�P@l�@K�@+@��@�+@V@{@@�@?}@�@��@�/@�/@�j@�@�D@Z@9X@��@ƨ@t�@"�@�!@-@��@��@x�@hs@�@Ĝ@�@bN@Q�@1'@1'@b@  @�;@�@K�@�@�y@ȴ@��@V@�T@?}@/@�@��@�/@�j@�j@�j@�@�@�@�@j@(�@�
@ƨ@�F@t�@C�@"�@
�@
�!@
~�@
M�@
-@
J@	�@	�^@	x�@	7L@	�@	%@	%@�9@�@Q�@A�@A�@A�@A�@  @�w@��@�P@|�@l�@;d@+@+@�@
=@��@�@�@ȴ@ȴ@ȴ@�R@��@V@�@�-@�h@p�@O�@�@��@�/@j@I�@(�@(�@�@1@�
@�
@ƨ@�@t�@C�@��@��@~�@n�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BdZBcTBdZBdZBe`Be`BffBiyBm�Bo�Bx�B|�B{�B�%B�%B�B�+B�JB�PB�oB�oB�bB�bB�hB�oB�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B�{B��B�{B�{B�{B�uB�oB�hB�VB�=B�+B�7B�Bk�B<jB�B��BVBP�B\)B^5BE�BE�BA�BL�BN�BO�B;dB �BuB��BB�ZBB��B�
BÖBŢB�
B�)B��B��B�jB�B��B�!B��B�BQ�B-B>wB�B
��B
��B
��B
��B
�B
�B
z�B
��B
�hB
iyB
M�B
`BB
z�B
�B
}�B
x�B
o�B
]/B
<jB
hB
B
oB	�B	��B	��B	��B	�yB	�B	��B	��B	��B	�B	�sB	�TB	��B	�qB	�7B	�PB	��B	��B	�\B	}�B	]/B	u�B	t�B	m�B	cTB	hsB	e`B	^5B	>wB	C�B	S�B	S�B	H�B	P�B	N�B	J�B	C�B	49B	33B	?}B	D�B	?}B	>wB	5?B	&�B	{B��B	B	VB	%B�B	hB	\B		7B��B�ZB�HB�HB�)BɺB�}B��BǮBɺB�wB��B�DB�+B�DB�%B�{B�{B��B��B�bB�JB�Bk�BbNBM�BO�B]/B^5BcTBm�Bo�Br�Bk�BgmBcTBXBS�BZBN�BXB_;B^5B^5BYB\)BQ�BH�BK�BB�BF�BG�BB�BA�B>wB8RB:^B/BPB�B.B33B33B8RB<jB?}B?}B?}B>wB:^B6FB+B6FB9XB7LB49B49B2-B49B/B(�B�B	7B�ZB��B�B+B/B/B1'B/B-B,B.B0!B/B+B%�B�B�B�B"�B�B�B�B�BoB%B�B��B��B�B�B!�B(�B&�B$�B.B'�B$�B�B#�B(�B7LB1'B#�BVB\B+B8RB:^B49B?}BA�BA�B?}B>wBA�B@�BB�BA�B@�B5?B49BH�BC�BC�BP�BO�BM�BG�B:^B;dB^5BgmBe`BcTBffBbNBe`Bo�BjBaHBm�B~�B�PB�bB�hB�oB�{B�{B�bB�=B�+B�hB�hB��B��B��B��B��B��B�B��B�B�!B�-B�3B�B�B�?B�FB�^B�XB�XB�^B�?B�3B�jB��B�B��B��B��B��B�B�B�B��B�/B�/B�/B�;B�/B�BB�HB�TB�ZB�fB�`B�fB�mB�`B�HB�NB�NB�HB�B�B�B�B�B�B�sB��B��B��B��B	B	B	B	B	B	B		7B	JB	DB	PB	uB	�B	{B	�B	"�B	&�B	&�B	%�B	&�B	&�B	%�B	'�B	$�B	#�B	'�B	,B	-B	1'B	0!B	0!B	33B	49B	49B	49B	2-B	33B	6FB	6FB	:^B	?}B	D�B	D�B	C�B	I�B	N�B	N�B	N�B	P�B	S�B	ZB	\)B	\)B	]/B	]/B	^5B	_;B	_;B	_;B	cTB	dZB	k�B	k�B	iyB	hsB	l�B	t�B	w�B	v�B	x�B	x�B	{�B	{�B	~�B	�B	�B	�B	�B	�1B	�=B	�7B	�7B	�=B	�=B	�\B	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�B	�B	��B	�!B	�9B	�3B	�'B	�?B	�^B	�}B	�wB	�}B	ŢB	ĜB	ŢB	ÖB	��B	ƨB	ǮB	ȴB	ɺB	ɺB	ɺB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�;B	�HB	�HB	�HB	�HB	�HB	�BB	�BB	�5B	�HB	�TB	�TB	�ZB	�sB	�fB	�mB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
B
B
B
B
B
B
  B
B
  B
  B
B
	7B
1B
1B
DB
VB
\B
\B
bB
bB
bB
bB
\B
\B
PB
PB
bB
bB
hB
oB
oB
hB
hB
oB
oB
{B
uB
hB
bB
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
!�B
"�B
"�B
$�B
#�B
#�B
"�B
#�B
#�B
#�B
$�B
&�B
&�B
&�B
%�B
$�B
&�B
(�B
(�B
'�B
%�B
'�B
'�B
(�B
(�B
(�B
(�B
'�B
'�B
)�B
(�B
)�B
)�B
)�B
(�B
'�B
,B
-B
-B
-B
,B
)�B
-B
.B
,B
/B
/B
/B
1'B
1'B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
0!B
1'B
33B
49B
5?B
49B
49B
49B
5?B
5?B
49B
5?B
49B
49B
49B
49B
6FB
9XB
9XB
9XB
9XB
8RB
9XB
:^B
:^B
9XB
9XB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
;dB
;dB
;dB
:^B
:^B
<jB
<jB
;dB
;dB
;dB
:^B
<jB
:^B
?}B
A�B
A�B
@�B
?}B
A�B
A�B
B�B
A�B
@�B
C�B
D�B
D�B
C�B
B�B
A�B
?}B
A�B
A�B
@�B
A�B
F�B
H�B
G�B
G�B
F�B
E�B
E�B
H�B
G�B
G�B
G�B
H�B
H�B
J�B
K�B
L�B
L�B
K�B
K�B
L�B
M�B
M�B
L�B
N�B
N�B
M�B
L�B
P�B
P�B
P�B
Q�B
Q�B
R�B
R�B
Q�B
R�B
R�B
S�B
S�B
R�B
R�B
S�B
T�B
VB
W
B
VB
VB
VB
VB
W
B
VB
T�B
R�B
XB
YB
YB
YB
YB
YB
XB
XB
XB
XB
VB
XB
XB
W
B
XB
YB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
[#B
ZB
[#B
[#B
ZB
[#B
]/B
_;B
_;B
_;B
^5B
^5B
_;B
`BB
aHB
aHB
aHB
aHB
aHB
aHB
`BB
_;B
aHB
aHB
bNB
aHB
`BB
`BB
`BB
e`B
e`B
e`B
e`B
ffB
ffB
gmB
gmB
ffB
ffB
ffB
e`B
e`B
e`B
gmB
gmB
gmB
gmB
gmB
hsB
gmB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
k�B
k�B
jB
k�B
k�B
l�B
m�B
l�B
l�B
k�B
k�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
n�B
n�B
m�B
l�B
n�B
o�B
p�B
o�B
p�B
p�B
p�B
o�B
q�B
r�B
r�B
r�B
r�B
q�B
r�B
r�B
q�B
r�B
r�B
q�B
s�B
t�B
u�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111BdZBcnBdZBdZBezBe`BffBi_Bm�Bo�By$B}<B|jB�YB�?B��B�zB�~B��B�oB�oB��B��B��B��B��B��B��B�{B�{B�{B�{B��B�{B�{B��B��B��B�{B��B��B�oB��B��B��B��B��B��Bm�B@�BB��B�BS@B^B`BIBIBD3BN�BPbBQ B=qB#�B�BoB�B�
B�B�(BچBƎB�KB��B��B��B͹B��B�B��B��B��B�aBV9B1vB@�B~B
�xB �B iB
׍B
��B
��B
~�B
��B
�@B
l�B
R B
a�B
{JB
�B
~B
x�B
pB
^B
>�B
�B
1B
aB	��B	ϫB	��B	�DB	�B	�vB	�2B	�B	�B	�AB	�*B	�&B	�SB	��B	�"B	��B	��B	��B	��B	�B	`�B	wB	v+B	oB	eB	i�B	ffB	_pB	A�B	E9B	T�B	T�B	I�B	QhB	O\B	KDB	DgB	6+B	4nB	?�B	D�B	?�B	>�B	5�B	(
B	mB�JB	�B	�B	�B��B	4B	�B		�B�B�B�B�B�IB�B�oB�ABȀB�XB�}B�B�VB�RB�jB��B��B�B��B��B�B��B��Bm�BdBP�BRB^�B_�Bd�BnIBp!Br�Bl=Bh>Bd&BY�BUgBZ�BPHBX�B_�B^�B^�BY�B\�BR�BI�BL�BC�BG�BH�BC�BB�B?�B9�B;JB0�B}B�B/B4B49B9$B<�B?�B?�B?�B>�B:�B7B,WB6�B9�B7�B4�B4�B2�B4�B/�B)�B�BB�XB�<B�B+kB/�B/�B1vB/�B-wB,qB.�B0oB/iB+QB&�B �BpB�B#TB �BjBWBCB@B_B�'BϑB��B�B�B"�B)�B(
B%�B.}B(�B%�B�B$�B)�B7fB1�B$�BbB�B+�B8�B:�B5tB@ BBBB'B@B?.BB'BA BC-BB'BA B6�B5tBIBD�BD�BQhBPbBNpBH�B<6B=B^�Bg�Be�Bc�Bf�Bc:BfBo�Bk6Bb�Bn�B}B�jB��B��B��B��B��B��B�)B�1B��B� B��B��B�B�/B�B�RB�kB��B�]B��B�aB�hB��B��B��B��B��B��B��B��B�B�TB�qB�B�B�B�:BΊB�gB�7B�_BևBԯB�dB�dB�dB�pBݘB��B�B�B�B�B�B�B�B�B��B��B��B�4B��B�B��B�B��B�/B�B�+B�B�VB�6B	[B	oB	[B	GB	[B	�B		lB	dB	�B	�B	�B	�B	�B	�B	"�B	&�B	'B	&B	'B	'B	&B	($B	%,B	$ZB	(>B	,WB	-CB	1[B	0oB	0oB	3MB	4TB	4TB	4TB	2�B	3�B	6�B	6�B	:�B	?�B	D�B	D�B	DB	J	B	O(B	O(B	O(B	Q4B	TFB	ZQB	\CB	\CB	]IB	]dB	^OB	_VB	_pB	_�B	c�B	d�B	k�B	k�B	i�B	h�B	l�B	t�B	xB	wB	y	B	y	B	|B	|PB	HB	�9B	�SB	�SB	�mB	�KB	�XB	�lB	�lB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�'B	��B	� B	�:B	�B	�B	�0B	�0B	�"B	�KB	�DB	�yB	�)B	�WB	�sB	�UB	�nB	��B	��B	��B	��B	��B	��B	��B	ŢB	��B	żB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�B	��B	�B	�B	�6B	�DB	�\B	�B	�2B	�MB	�KB	�7B	�eB	�KB	�QB	�QB	چB	�pB	�HB	�HB	�|B	�bB	�bB	�vB	�vB	ޞB	�B	�nB	�B	�B	�B	�B	�B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�	B	��B	�B	��B	�B	�+B	�B	��B	�B	�"B	�0B	�"B	�B	�B	�.B
 4B	�HB
 B
-B
-B
AB
3B
'B
 OB
[B
 iB
 �B
uB
	RB
fB
�B
xB
pB
vB
�B
}B
bB
}B
bB
�B
vB
�B
�B
�B
}B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
#B
!�B
"�B
#B
$�B
#�B
$B
"�B
#�B
#�B
$B
%B
'B
&�B
'B
&B
%,B
'B
)B
(�B
(
B
&2B
(
B
(
B
)B
)B
)*B
)*B
(
B
(
B
*0B
)B
*0B
*0B
*0B
)*B
(XB
,"B
-CB
-CB
-)B
,=B
*0B
-CB
./B
,=B
/5B
/5B
/OB
1[B
1'B
0;B
0UB
1[B
1[B
1[B
1AB
1AB
0UB
1[B
3MB
4TB
5ZB
4TB
4nB
4TB
5tB
5tB
4nB
5ZB
4nB
4TB
4�B
4nB
6`B
9rB
9XB
9rB
9rB
8�B
9rB
:^B
:xB
9�B
9rB
8lB
8lB
8�B
9rB
9rB
9rB
:xB
;�B
;B
;B
:�B
:�B
<�B
<�B
;B
;B
;�B
:�B
<�B
:�B
?�B
A�B
A�B
@�B
?�B
A�B
A�B
B�B
A�B
@�B
C�B
D�B
D�B
C�B
B�B
A�B
?�B
A�B
A�B
@�B
A�B
F�B
H�B
G�B
G�B
F�B
E�B
E�B
H�B
G�B
G�B
G�B
H�B
IB
J�B
K�B
MB
MB
K�B
K�B
L�B
NB
NB
MB
N�B
OB
NB
MB
Q B
Q B
Q B
R B
R B
SB
SB
R B
SB
S&B
TB
TB
SB
S&B
TB
U2B
V9B
W$B
V9B
VB
VB
V9B
W$B
V9B
U2B
S@B
XB
Y1B
YB
Y1B
Y1B
Y1B
X+B
XEB
XEB
X+B
V9B
XEB
XEB
W?B
X+B
YKB
Z7B
[=B
[=B
[=B
[WB
[=B
[=B
[WB
[WB
[WB
ZQB
[WB
[WB
ZkB
[WB
]IB
_VB
_VB
_VB
^OB
^jB
_pB
`\B
abB
abB
aHB
abB
abB
abB
`vB
_�B
abB
abB
bhB
a|B
`vB
`�B
`vB
ezB
e`B
e�B
e�B
ffB
ffB
gmB
gmB
f�B
f�B
f�B
e�B
ezB
e�B
gmB
g�B
g�B
g�B
g�B
h�B
g�B
h�B
i�B
i�B
i�B
i�B
i�B
i�B
j�B
j�B
k�B
k�B
j�B
k�B
k�B
l�B
m�B
l�B
l�B
k�B
k�B
m�B
m�B
m�B
m�B
m�B
n�B
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
n�B
n�B
m�B
l�B
n�B
o�B
p�B
o�B
p�B
p�B
p�B
o�B
q�B
r�B
r�B
r�B
r�B
q�B
r�B
r�B
q�B
r�B
r�B
q�B
s�B
t�B
u�B
u�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.11(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201901070031442019010700314420190107003144201901070200152019010702001520190107020015201901080022282019010800222820190108002228  JA  ARFMdecpA19c                                                                20190103093627  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20190103003629  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20190103003632  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20190103003633  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20190103003634  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20190103003634  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20190103003634  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20190103003634  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20190103003634  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20190103003634                      G�O�G�O�G�O�                JA  ARUP                                                                        20190103005654                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20190103153350  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20190106153144  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20190106153144  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20190106170015  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20190107152228  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115131517                      G�O�G�O�G�O�                