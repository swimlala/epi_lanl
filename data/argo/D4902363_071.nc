CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2016-12-26T00:35:27Z creation;2016-12-26T00:35:30Z conversion to V3.1;2019-12-19T08:22:18Z update;     
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
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t(   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �P   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  ݈   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20161226003527  20200115111516  4902363                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               GA   JA  I2_0576_071                     2C  D   NAVIS_A                         0576                            ARGO 092515                     863 @��s�K� 1   @��t��@:��b���d�+I�1   GPS     A   A   A   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�33@�33A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr�Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
y�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dhy�Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @\)@��@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB��{B�ǮB�ǮB�ǮB�ǮB�ǮB���B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq�qCs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
r�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhr�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��HD��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D���D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A��A��A��A��A��A��A�{A�oA�1A��A��A��A�;dA���A��PA�VA�1'A�"�A�  A��HA���A�ƨA���A��wA��wA��wA���A��jA��RA��A��hA�XA�33A�%A�v�A�dZA�%A���A�\)A��DA��7A�"�A�E�A��`A�(�A�A�n�A��A��wA�-A���A�n�A�ffA�r�A���A�ĜA�%A��A�E�A��A���A��A���A�-A�ȴA�ƨA��!A�VA��
A��A���A��jA���A�XA�1'A���A��yA���A���A��uA��A�ƨA��mA���A��A}�A|n�A{Ay�AxbNAwAv��AvE�Au��Au�At�DAt{As�#As��AsG�Ar��Ar�Ap�HAo�Am��AlAkoAi�AhjAg��Afv�Ae+AcS�Ab��Ab5?Ab�AbJAa�#A`bNA_�wA_K�A]��A\ZA\  AZ�9AZ5?AY�AY�7AX�DAWl�AV^5ATȴASp�ARv�AQ�AQ�AP1ANQ�AL��AKoAI��AI�AHE�AG
=AE�ADM�AChsAB��AAƨA@�9A@jA@$�A?S�A>�A=�7A<�!A;�TA;G�A:ȴA9��A8�`A8{A7p�A7&�A6�RA5��A5�A5\)A5+A4Q�A3�hA2��A1�FA0��A.�RA-�TA-C�A,�!A+?}A*��A*�A*v�A)��A(�A(z�A'��A'��A'+A&5?A&JA%��A%oA$JA"��A!�A!&�A ZA`BA��A�uAr�A1'A��A�A1AS�A��A��A�RAjAZAA�A|�A  A�A�\A�wA&�A9XA�AVAbNA�;A�DA��A�/A�RA��A	x�A�+A��A��A�HA�+A1'A
=A�A��AXA�A��Ao@��w@�hs@�/@��@�l�@�I�@��#@�%@�dZ@�@�\)@�9X@땁@��@���@�r�@�\)@���@�7L@�F@�"�@�~�@�E�@�X@���@�@�7L@ܬ@�  @�K�@ڰ!@���@ف@�b@�E�@ԓu@�1@��;@���@ӥ�@�K�@�^5@�E�@�J@��@��y@ΰ!@��@̣�@�A�@��
@�t�@�E�@��/@�\)@�v�@ũ�@�&�@�&�@�&�@��`@�1'@��@å�@��@�@�E�@�x�@�O�@��j@��F@���@��-@���@���@�+@�V@��@��-@��h@�r�@�1@�|�@��@��@���@�Q�@�l�@�$�@���@�Z@�A�@��@���@�o@�@��R@��@���@�
=@���@��@��`@��
@�;d@�"�@�@��@�  @�ƨ@�S�@��@�v�@��@��@�ȴ@�V@��#@�G�@��@��
@�ȴ@�-@��#@���@�`B@�7L@���@��j@��D@�A�@��
@�C�@��H@�ȴ@�v�@��@��^@�/@�Q�@��m@��F@�dZ@�S�@�\)@�K�@�o@��@��+@�J@��T@���@��9@�A�@�b@��@���@���@�dZ@��@���@���@�v�@�=q@���@�X@��`@��u@�I�@��
@���@�t�@�\)@�C�@�33@��y@�=q@��@��^@���@���@�x�@�`B@�p�@�7L@���@�z�@��P@�G�@�j@�ƨ@��@�t�@�dZ@�\)@�K�@�;d@�33@�o@�@��y@��@��R@�n�@�-@��T@�x�@�hs@�`B@�X@�X@�O�@��/@�z�@�Q�@�1'@�1'@�1'@�b@�  @�1@�@�;@�@��@��@l�@l�@l�@\)@\)@\)@l�@|�@|�@l�@\)@+@~��@~��@~E�@}�-@}O�@}/@}`B@}O�@}O�@}?}@|�j@{�m@{��@{�@{�@{t�@{S�@{C�@{o@z=q@yX@x�`@xr�@x �@w��@w��@w��@w��@w\)@w;d@v�@v5?@v$�@u@u�h@u/@t�j@s��@sdZ@sC�@s"�@r�H@r�!@r��@rM�@r-@q�#@q7L@p�9@pbN@p �@o�@oK�@n�y@n��@n��@nv�@m@mp�@m?}@l�/@lz�@l(�@k�F@k��@k33@j��@j��@j-@i�@ihs@iG�@i&�@h��@g�@g|�@g+@e�T@ep�@eV@d1@c�@c33@b�H@b��@a�#@a�@`bN@^��@^@]O�@\��@\��@\�D@\Z@[ƨ@["�@["�@Z��@Z��@Y�@Yx�@Y&�@Y%@X�`@Xr�@X1'@W|�@W
=@V�y@Vȴ@V�+@V@UV@T��@T��@T�D@Tj@TI�@T(�@S�m@S��@S"�@R��@R=q@QG�@Q%@PĜ@P��@Pr�@Pb@O�w@Ol�@N��@N��@Nff@N5?@N$�@N{@N{@N@N@M�@M��@Mp�@MO�@L��@L�/@L��@L��@LZ@L�@L1@K��@K�
@K�F@K��@Kt�@KdZ@KdZ@KdZ@KS�@KC�@K@J�\@I��@I��@Ihs@HĜ@H �@G��@Fȴ@F5?@E�@E�-@E�h@E?}@EV@D��@Dz�@DZ@DI�@D(�@Cƨ@CC�@C@B�H@B��@BM�@B�@BJ@A��@A�@A�#@A�#@A�#@A��@A�^@A��@AX@A%@@��@@�9@@r�@@A�@@1'@@  @?��@>�@=�-@=�@<�j@<(�@;�F@;o@:�@:�H@:��@:^5@9��@9�#@9��@9G�@9%@8Ĝ@8b@7�@7K�@6�R@6��@6�+@6V@65?@5�T@5�h@5`B@5O�@5/@5V@4�D@4�@3ƨ@3t�@333@2�H@2�\@2n�@2-@1��@1�7@1G�@1�@0Ĝ@0r�@0bN@0b@0  @0  @/�;@/��@/�@/�P@/�@.�+@.ff@.ff@.{@-�h@-p�@-O�@,�@,j@+�
@+��@+dZ@*��@*~�@*n�@*=q@)�^@)��@)x�@(��@(Q�@(  @'�P@'K�@'�@&�@&��@&V@&@%��@%@%�-@%�@%p�@%O�@%/@%V@$��@$��@$�/@$�D@$�D@$j@$1@$1@#��@#�
@#dZ@#S�@#C�@#"�@"=q@!��@!��@!��@!&�@ ��@ Q�@  �@�@�P@K�@V@5?@5?@5?@E�@E�@5?@$�@$�@{@�@�h@O�@V@�j@�@��@�D@z�@z�@j@I�@1@�F@��@dZ@33@��@~�@��@hs@G�@G�@7L@%@�9@��@�@r�@Q�@��@K�@
=@ȴ@�R@�R@�R@E�@5?@@��@@�-@��@O�@�@z�@(�@�@��@��@ƨ@��@S�@"�@o@o@�@�@��@�!@~�@��@�7@x�@&�@�9@A�@1'@1'@�w@;d@+@��@ȴ@@�-@�-@��@�h@�h@�h@�@p�@O�@/@/@�@V@��@��@j@(�@1@ƨ@��@�@S�@C�@"�@@
�H@
�\@
n�@
=q@
�@
�@
�@
�@	��@	��@	��@	x�@	7L@��@bN@Q�@A�@ �@b@�;@�w@�P@\)@�@�@��@E�@@@��@��@��@p�@`B@O�@O�@O�@O�@/@��@�j@�@�@��@�D@z�@j@�m@��@t�@S�@33@�H@��@�!@~�@^5@M�@M�@M�@=q@��@��@�^@��@hs@ ��@ �`@ Ĝ@ �u@ �@ r�@ bN@ Q�@ 1'@   ?�|�?��?��?��R?��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A��A��A��A��A��A��A��A��A�{A�oA�1A��A��A��A�;dA���A��PA�VA�1'A�"�A�  A��HA���A�ƨA���A��wA��wA��wA���A��jA��RA��A��hA�XA�33A�%A�v�A�dZA�%A���A�\)A��DA��7A�"�A�E�A��`A�(�A�A�n�A��A��wA�-A���A�n�A�ffA�r�A���A�ĜA�%A��A�E�A��A���A��A���A�-A�ȴA�ƨA��!A�VA��
A��A���A��jA���A�XA�1'A���A��yA���A���A��uA��A�ƨA��mA���A��A}�A|n�A{Ay�AxbNAwAv��AvE�Au��Au�At�DAt{As�#As��AsG�Ar��Ar�Ap�HAo�Am��AlAkoAi�AhjAg��Afv�Ae+AcS�Ab��Ab5?Ab�AbJAa�#A`bNA_�wA_K�A]��A\ZA\  AZ�9AZ5?AY�AY�7AX�DAWl�AV^5ATȴASp�ARv�AQ�AQ�AP1ANQ�AL��AKoAI��AI�AHE�AG
=AE�ADM�AChsAB��AAƨA@�9A@jA@$�A?S�A>�A=�7A<�!A;�TA;G�A:ȴA9��A8�`A8{A7p�A7&�A6�RA5��A5�A5\)A5+A4Q�A3�hA2��A1�FA0��A.�RA-�TA-C�A,�!A+?}A*��A*�A*v�A)��A(�A(z�A'��A'��A'+A&5?A&JA%��A%oA$JA"��A!�A!&�A ZA`BA��A�uAr�A1'A��A�A1AS�A��A��A�RAjAZAA�A|�A  A�A�\A�wA&�A9XA�AVAbNA�;A�DA��A�/A�RA��A	x�A�+A��A��A�HA�+A1'A
=A�A��AXA�A��Ao@��w@�hs@�/@��@�l�@�I�@��#@�%@�dZ@�@�\)@�9X@땁@��@���@�r�@�\)@���@�7L@�F@�"�@�~�@�E�@�X@���@�@�7L@ܬ@�  @�K�@ڰ!@���@ف@�b@�E�@ԓu@�1@��;@���@ӥ�@�K�@�^5@�E�@�J@��@��y@ΰ!@��@̣�@�A�@��
@�t�@�E�@��/@�\)@�v�@ũ�@�&�@�&�@�&�@��`@�1'@��@å�@��@�@�E�@�x�@�O�@��j@��F@���@��-@���@���@�+@�V@��@��-@��h@�r�@�1@�|�@��@��@���@�Q�@�l�@�$�@���@�Z@�A�@��@���@�o@�@��R@��@���@�
=@���@��@��`@��
@�;d@�"�@�@��@�  @�ƨ@�S�@��@�v�@��@��@�ȴ@�V@��#@�G�@��@��
@�ȴ@�-@��#@���@�`B@�7L@���@��j@��D@�A�@��
@�C�@��H@�ȴ@�v�@��@��^@�/@�Q�@��m@��F@�dZ@�S�@�\)@�K�@�o@��@��+@�J@��T@���@��9@�A�@�b@��@���@���@�dZ@��@���@���@�v�@�=q@���@�X@��`@��u@�I�@��
@���@�t�@�\)@�C�@�33@��y@�=q@��@��^@���@���@�x�@�`B@�p�@�7L@���@�z�@��P@�G�@�j@�ƨ@��@�t�@�dZ@�\)@�K�@�;d@�33@�o@�@��y@��@��R@�n�@�-@��T@�x�@�hs@�`B@�X@�X@�O�@��/@�z�@�Q�@�1'@�1'@�1'@�b@�  @�1@�@�;@�@��@��@l�@l�@l�@\)@\)@\)@l�@|�@|�@l�@\)@+@~��@~��@~E�@}�-@}O�@}/@}`B@}O�@}O�@}?}@|�j@{�m@{��@{�@{�@{t�@{S�@{C�@{o@z=q@yX@x�`@xr�@x �@w��@w��@w��@w��@w\)@w;d@v�@v5?@v$�@u@u�h@u/@t�j@s��@sdZ@sC�@s"�@r�H@r�!@r��@rM�@r-@q�#@q7L@p�9@pbN@p �@o�@oK�@n�y@n��@n��@nv�@m@mp�@m?}@l�/@lz�@l(�@k�F@k��@k33@j��@j��@j-@i�@ihs@iG�@i&�@h��@g�@g|�@g+@e�T@ep�@eV@d1@c�@c33@b�H@b��@a�#@a�@`bN@^��@^@]O�@\��@\��@\�D@\Z@[ƨ@["�@["�@Z��@Z��@Y�@Yx�@Y&�@Y%@X�`@Xr�@X1'@W|�@W
=@V�y@Vȴ@V�+@V@UV@T��@T��@T�D@Tj@TI�@T(�@S�m@S��@S"�@R��@R=q@QG�@Q%@PĜ@P��@Pr�@Pb@O�w@Ol�@N��@N��@Nff@N5?@N$�@N{@N{@N@N@M�@M��@Mp�@MO�@L��@L�/@L��@L��@LZ@L�@L1@K��@K�
@K�F@K��@Kt�@KdZ@KdZ@KdZ@KS�@KC�@K@J�\@I��@I��@Ihs@HĜ@H �@G��@Fȴ@F5?@E�@E�-@E�h@E?}@EV@D��@Dz�@DZ@DI�@D(�@Cƨ@CC�@C@B�H@B��@BM�@B�@BJ@A��@A�@A�#@A�#@A�#@A��@A�^@A��@AX@A%@@��@@�9@@r�@@A�@@1'@@  @?��@>�@=�-@=�@<�j@<(�@;�F@;o@:�@:�H@:��@:^5@9��@9�#@9��@9G�@9%@8Ĝ@8b@7�@7K�@6�R@6��@6�+@6V@65?@5�T@5�h@5`B@5O�@5/@5V@4�D@4�@3ƨ@3t�@333@2�H@2�\@2n�@2-@1��@1�7@1G�@1�@0Ĝ@0r�@0bN@0b@0  @0  @/�;@/��@/�@/�P@/�@.�+@.ff@.ff@.{@-�h@-p�@-O�@,�@,j@+�
@+��@+dZ@*��@*~�@*n�@*=q@)�^@)��@)x�@(��@(Q�@(  @'�P@'K�@'�@&�@&��@&V@&@%��@%@%�-@%�@%p�@%O�@%/@%V@$��@$��@$�/@$�D@$�D@$j@$1@$1@#��@#�
@#dZ@#S�@#C�@#"�@"=q@!��@!��@!��@!&�@ ��@ Q�@  �@�@�P@K�@V@5?@5?@5?@E�@E�@5?@$�@$�@{@�@�h@O�@V@�j@�@��@�D@z�@z�@j@I�@1@�F@��@dZ@33@��@~�@��@hs@G�@G�@7L@%@�9@��@�@r�@Q�@��@K�@
=@ȴ@�R@�R@�R@E�@5?@@��@@�-@��@O�@�@z�@(�@�@��@��@ƨ@��@S�@"�@o@o@�@�@��@�!@~�@��@�7@x�@&�@�9@A�@1'@1'@�w@;d@+@��@ȴ@@�-@�-@��@�h@�h@�h@�@p�@O�@/@/@�@V@��@��@j@(�@1@ƨ@��@�@S�@C�@"�@@
�H@
�\@
n�@
=q@
�@
�@
�@
�@	��@	��@	��@	x�@	7L@��@bN@Q�@A�@ �@b@�;@�w@�P@\)@�@�@��@E�@@@��@��@��@p�@`B@O�@O�@O�@O�@/@��@�j@�@�@��@�D@z�@j@�m@��@t�@S�@33@�H@��@�!@~�@^5@M�@M�@M�@=q@��@��@�^@��@hs@ ��@ �`@ Ĝ@ �u@ �@ r�@ bN@ Q�@ 1'@   ?�|�?��?��?��R?��R111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BA�BA�BA�BA�BA�BA�BB�BB�BC�BC�BD�BF�BH�BM�BT�BXBVBVBT�BT�BT�BR�BR�BR�BR�BR�BR�BS�BS�BS�BS�BS�BVBW
BVBT�BS�BQ�B^5B\)BI�B:^B�B+B�;B�B��BƨB�dB��Bw�BZBD�B�B�NB�wB��B�7B�B^5BO�BM�BG�B;dB.B!�B�B�B�B�B{BbB	7B  B
��B
�B
�B
�B
�yB
�sB
�mB
�`B
�)B
��B
�9B
��B
�uB
�%B
y�B
n�B
cTB
[#B
VB
Q�B
N�B
J�B
G�B
D�B
B�B
A�B
?}B
@�B
>wB
;dB
1'B
-B
#�B
�B
{B
VB
B	��B	��B	�B	�ZB	�5B	�)B	�#B	�B	�B	��B	��B	ǮB	��B	�9B	�3B	�B	��B	��B	��B	��B	��B	�hB	�+B	� B	z�B	x�B	t�B	m�B	e`B	[#B	R�B	L�B	F�B	C�B	<jB	7LB	/B	)�B	&�B	 �B	�B	�B	�B	�B	oB	VB		7B	%B	B	  B��B��B�B�B�B�B�B�sB�sB�fB�ZB�;B�/B�B��B��BȴBŢBB�}B�dB�^B�XB�LB�3B�-B�!B�B�B��B��B��B��B��B��B��B��B��B�bB�bB�VB�PB�JB�DB�1B�+B�B�B|�B{�By�Bx�Bx�Bv�Bs�Bn�Bm�Bk�BhsBhsBe`BdZBaHBaHB^5BYBXBW
BVBO�BL�BJ�BI�BH�BF�BE�BE�BB�B@�B?}B>wB>wB;dB9XB6FB5?B49B5?B2-B0!B0!B/B.B.B/B-B.B-B,B-B,B,B,B+B)�B(�B)�B(�B'�B(�B(�B(�B)�B+B+B+B,B+B,B+B+B+B)�B+B,B+B+B-B-B,B/B0!B0!B0!B0!B2-B49B8RB9XB;dB<jB<jB<jB=qB>wB?}B?}B@�BA�BA�BC�BC�BD�BE�BF�BH�BJ�BJ�BL�BM�BM�BN�BN�BR�BR�BT�BVBYB]/B]/B`BBcTBdZBdZBe`BffBgmBhsBhsBiyBo�Bp�Bs�Bu�Bw�B{�B�B�B�B�%B�7B�=B�VB�uB�{B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�-B�LB�dB�qB��B��BBBŢBȴB��B��B��B��B��B�
B�B�B�)B�/B�;B�NB�NB�TB�B�B�B�B�B�B�B�B�B�B��B��B��B��B	  B	B	B	%B	+B	1B		7B	
=B	DB	JB	VB	VB	VB	VB	VB	VB	VB	\B	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	 �B	!�B	"�B	#�B	#�B	%�B	'�B	)�B	-B	1'B	1'B	1'B	2-B	2-B	33B	9XB	@�B	D�B	E�B	G�B	L�B	N�B	O�B	P�B	P�B	P�B	P�B	R�B	S�B	T�B	T�B	W
B	ZB	[#B	]/B	^5B	_;B	_;B	`BB	bNB	dZB	e`B	gmB	hsB	k�B	m�B	n�B	x�B	~�B	�B	�B	�B	�+B	�7B	�7B	�7B	�7B	�=B	�=B	�DB	�\B	�uB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�!B	�!B	�!B	�'B	�-B	�3B	�?B	�?B	�FB	�FB	�FB	�LB	�RB	�^B	�dB	�wB	��B	B	B	ĜB	ŢB	ǮB	��B	��B	��B	�B	�B	�B	�B	�)B	�)B	�/B	�;B	�HB	�HB	�NB	�ZB	�`B	�fB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
%B
1B
1B
1B
	7B
	7B

=B

=B
DB
JB
JB
JB
PB
PB
PB
PB
PB
PB
PB
VB
VB
VB
\B
\B
\B
\B
bB
bB
bB
bB
hB
hB
hB
oB
oB
oB
oB
oB
oB
oB
uB
{B
�B
{B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
"�B
#�B
#�B
#�B
#�B
$�B
$�B
$�B
$�B
&�B
'�B
(�B
(�B
)�B
+B
,B
,B
,B
,B
,B
-B
-B
-B
-B
-B
.B
/B
/B
0!B
0!B
0!B
0!B
0!B
1'B
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
33B
33B
33B
33B
33B
33B
33B
33B
33B
49B
49B
49B
49B
49B
49B
49B
49B
49B
5?B
6FB
7LB
7LB
7LB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
;dB
;dB
;dB
;dB
;dB
<jB
<jB
=qB
=qB
=qB
=qB
=qB
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
?}B
?}B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
C�B
C�B
C�B
B�B
C�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
C�B
C�B
D�B
D�B
E�B
E�B
F�B
F�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
W
B
W
B
W
B
W
B
W
B
W
B
VB
VB
W
B
W
B
XB
YB
YB
ZB
ZB
ZB
[#B
[#B
\)B
\)B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
aHB
aHB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
jB
jB
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
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
p�B
p�B
p�B
p�B
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 BA�BA�BA�BA�BA�BA�BB�BB�BC�BC�BD�BGBIlBN�BU�BX�BV�BVSBUMBUMBU2BS&BSBSBSBSBR�BS�BTBTBTFBTFBV�BW�BV�BV�BVBS@B_�B_�BN�B@ B�BB�vB�WB�B�^B��B� B|6B_VBL�B(sB�B��B�/B�JB��B_�BP�BOBBI�B>]B0�B"�B B!B�B�B�B�BDB�B
��B
�B
�)B
��B
��B
��B
�B
�B
�VB
��B
�B
��B
��B
�B
{�B
poB
d�B
\B
V�B
R�B
O�B
K�B
H�B
E9B
B�B
A�B
@ B
A;B
?�B
="B
3hB
/ B
%�B
�B
B
B
SB	�}B	��B	�|B	�FB	޸B	�xB	�qB	ںB	ٴB	��B	��B	ɺB	��B	�B	��B	��B	�sB	��B	�,B	�'B	�EB	�@B	��B	�;B	{�B	zB	vzB	o�B	g�B	]B	TaB	NB	G�B	ESB	>B	9$B	0;B	+B	(>B	!�B	IB	WB	�B	�B	�B	�B	
=B	B	B	�B�B��B�B�;B�iB�B�B��B��B�B�zB�\B��B��B�9B��BɠBƨB�B�B��B��B��B�RB��B��B��B� B�B�yB��B��B�FB�TB�B��B��B��B� B� B��B��B�6B�dB��B�1B�9B�aB}�B|jBzDByrBz*Bx�BuBoiBn�Bl�Bi�BiDBf�BezBbhBc B_�BY�BX�BX�BXyBQBM�BK�BJ�BIlBG�BG+BF�BCaBAB@4B?}B@�B=B:�B6�B5�B5�B72B3�B1'B1�B0�B/�B0B/�B.cB.�B-�B-B.B,�B-B,�B+�B*�B)�B+�B)�B(�B)�B)�B)�B*B+�B+�B,"B-]B,B,qB+QB+6B+QB*B+�B,qB+�B,=B-�B-wB,�B/�B0�B0�B0�B1'B3MB5ZB8�B:B;�B<�B<�B<�B>B>�B?�B@ BABBBBABC�BD3BEmBFtBG_BI�BK^BKxBMjBN"BN<BOBBO�BS[BSuBU�BV�BZB]�B^Ba-Bd&Bd�Bd�Be�Bf�Bg�Bh�Bi*Bj�Bp�Bq'BtBvzBx�B|�B�oB�oB�B��B��B��B��B��B�MB��B�yB�WB�B�!B�\B��B��B��B�yB�kB�=B�]B�IB�OB�oB�|B��B��B��B��B��B��B�B�B�RB�jB�NB� B�&B�2B�?B�+B�eB�]BݘBߤB�B�B�B��B��B��B��B��B��B��B��B��B��B�B�2B�DB�PB	 OB	uB	gB	YB	zB	fB		RB	
�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	B	�B	)B	IB	B	B	�B	�B	�B	�B	�B	 �B	 �B	!�B	#B	$B	$&B	&LB	($B	*KB	-]B	1AB	1AB	1[B	2GB	2|B	3�B	9�B	@�B	D�B	E�B	G�B	MB	OB	O�B	Q B	Q B	QB	Q B	S&B	T,B	T�B	UB	W?B	Z7B	[=B	]/B	^5B	_;B	_pB	`vB	b�B	d�B	e�B	g�B	h�B	k�B	m�B	n�B	x�B	~�B	�;B	�oB	�mB	�EB	�lB	�RB	�RB	�lB	�XB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�8B	�$B	�*B	�0B	�kB	�CB	�5B	�5B	�UB	�UB	�;B	�UB	�[B	�|B	��B	�tB	�ZB	�`B	�zB	�zB	��B	�lB	�xB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�&B	�EB	�_B	�7B	�kB	�CB	�CB	�~B	ߊB	�B	�B	��B	�B	�B	��B	��B	��B	�B	��B	��B	��B	�B	�'B	��B	��B	��B	��B	��B	��B	�B	�	B	�	B	��B	�	B	�B	�B	��B	�B	�B	�6B	�B	�<B	�BB	�.B	�.B	�HB	�HB	�HB
 B
 4B
 B
 B
 B
AB
'B
AB
GB
gB
mB
tB
fB
KB
KB
	lB
	�B

rB

rB
xB
dB
dB
dB
jB
PB
jB
PB
PB
�B
jB
�B
pB
�B
�B
�B
vB
vB
�B
bB
�B
�B
�B
�B
�B
�B
oB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
 �B
 �B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"�B
#B
$B
$B
#�B
$B
$�B
%B
%,B
%FB
'RB
($B
)DB
)DB
*0B
+QB
,=B
,=B
,"B
,"B
,=B
-CB
-CB
-CB
-)B
-]B
.IB
/iB
/OB
0oB
0UB
0UB
0;B
0;B
1[B
1AB
2aB
2aB
2aB
2GB
2aB
2|B
3hB
3MB
3hB
3MB
3hB
3MB
3MB
3�B
3hB
3hB
3hB
3hB
3hB
4TB
4nB
4TB
49B
4TB
4TB
4nB
4nB
4nB
5�B
6zB
7�B
7�B
7�B
8�B
8�B
8�B
8�B
9�B
9�B
9�B
:�B
:�B
;�B
;B
;�B
;�B
;B
<�B
<�B
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>�B
>�B
>�B
?�B
?�B
?}B
?�B
?�B
?�B
?}B
?�B
?�B
?�B
@�B
@�B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
C�B
C�B
C�B
B�B
C�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
C�B
C�B
D�B
D�B
E�B
E�B
F�B
F�B
G�B
G�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
K�B
LB
K�B
MB
N"B
NB
M�B
NB
NB
NB
N�B
N�B
O�B
PB
PB
QB
QB
QB
Q�B
RB
R B
R B
S&B
S&B
TB
S�B
TB
TB
T,B
T,B
T,B
UB
UB
UB
T�B
UB
UB
V9B
V9B
VB
VB
V9B
W
B
W?B
W$B
W$B
W?B
W$B
V9B
V9B
WYB
WYB
X+B
YKB
YKB
ZQB
ZQB
Z7B
[WB
[qB
\CB
\CB
\)B
\CB
\CB
]/B
]/B
]IB
]IB
]dB
]IB
]IB
]/B
]dB
]IB
]IB
]dB
]IB
]dB
^OB
^OB
^jB
^OB
^OB
_VB
_pB
_pB
_VB
`vB
`\B
`\B
`\B
`\B
`\B
`vB
a|B
a|B
a|B
b�B
b�B
cTB
cnB
cnB
c�B
cnB
cnB
c�B
c�B
d�B
dtB
d�B
e�B
e�B
ezB
f�B
ffB
f�B
g�B
gmB
g�B
gmB
gmB
gmB
g�B
h�B
h�B
hsB
h�B
h�B
iyB
i�B
i�B
i�B
j�B
j�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
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
p�B
p�B
p�B
p�B
p�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.11(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201612300032072016123000320720161230003207201806221218492018062212184920180622121849201804050411582018040504115820180405041158  JA  ARFMdecpA19c                                                                20161226093506  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20161226003527  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20161226003528  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20161226003528  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20161226003529  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20161226003529  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20161226003529  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20161226003529  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcpt16c                                                                20161226003529  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20161226003530                      G�O�G�O�G�O�                JA  ARUP                                                                        20161226012933                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20161226153404  CV  JULD            G�O�G�O�F�#�                JM  ARCAJMQC2.0                                                                 20161229153207  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20161229153207  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180404191158  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180622031849  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200115111516                      G�O�G�O�G�O�                