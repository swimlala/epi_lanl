CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-12-14T12:42:58Z creation;2022-12-14T12:43:01Z conversion to V3.1      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8<   JULD               standard_name         time   	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~       axis      T           8@   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8H   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�����h�   
_FillValue        A.�~            8L   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8T   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8\   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8d   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8h   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    8p   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    8t   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    8x   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8|   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9|   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  t�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �P   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �@   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �@   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �H   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20221214124258  20221214125940  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�R���w1   @�S3�ax@-#S����c��\(��1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BHffBPffBX  B`  Bg��Bo��Bx  B�  B�  B�  B�  B�  B�33B���B���B�  B�ffB�33B���B���B�33B���B�  B�33B���B�  B�ffBϙ�B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�33B���C�fC  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2�C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� DfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dyy�Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�)�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��G@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG��BO��BW�\B_�\Bg(�Bo(�Bw�\B�\B�ǮB�ǮB�ǮB�ǮB���B��{B��{B�ǮB�.B���B��{B��{B���B�aHB�ǮB���BÔ{B�ǮB�.B�aHB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB���B�ǮB�ǮB�ǮB���B��{C�=C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1�qC3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D�\Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyr�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D��D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�?�D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�&111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A��jA���A���A��A��}A�� A���A���A���A���Aѳ3Aѡ�Aџ�Aџ!Aѝ~Aќ�Aћ�Aћ�Aћ�Aћ=AњAњAї$AіSAѓ�Aщ7A�]�A�5tA��A�uA��A��A��A���A�� AО�A�B�A̚�A�XA��Aʾ�A�GA���A�P�A���A��%A�IRA�f�A���A�DA�C�A��mA��A{�.Ax4�Ary>Al�vAk�Aj�vAg�wAb��A]q�AW�mAVeAS�AP�)AKMjAI=AHu�AG��AF��AEHABbNA@�A>�WA=� A=��A=��A=VmA<	�A8��A7DgA7�A7>�A9҉A:A A9.IA2qA-�"A-��A,�.A*��A)��A'��A&1�A$��A"A %�Az�Ae,AE�A*�A�DA�6A�Au�A�gA�?A9XA�'A$�A��A?}A�@A��AMA��A��A��AtTA��A+kA��A�A]dA�|A�|A�QA�Ar�A"�A�FA9XA�4A��A��AGEA
��A
�A	�IA��APHAA�0A�OA�A\�A��AJ#A��A`�A($A��AE�A�gA
�Ao AA�A ��A ��@��w@���@���@��@��]@��[@�\)@�@���@��@�  @�a�@���@��3@���@���@�W?@�IR@���@��@��@�@��@�]�@�6@�@��@�8�@�ѷ@�@�,�@�2�@�ƨ@�b�@��?@�c�@�/�@�m]@�A@��@�ƨ@�F�@��@�c@�>�@��H@撣@�<�@���@�X@���@�*�@��'@��>@ᇔ@��@��]@��2@�1'@��)@�n/@ܾ@�  @�c�@���@�E�@٭C@�s�@ו�@և+@�
�@�+�@Ԗ�@�h
@�֡@��@ԗ�@Ԡ�@���@��c@ԇ+@�Z�@��@Ӛk@�w2@��?@�@�@х@��	@��U@ДF@��6@��m@�;�@��A@��@�%F@��@� \@���@̵�@�6@�j�@�H�@ɣn@ɀ4@��@�W�@� �@Ǘ$@�o�@�33@��m@�q�@��@�;d@�A�@�ԕ@Ó�@���@@�Ov@�@�H�@��@��$@�V�@�Q@�L0@���@��@�e�@�J@��r@��z@��0@��X@�@��m@���@���@�#:@���@��C@��4@���@��V@�Mj@��\@�_@��@���@�@@��@��B@���@�l"@�H@�
�@�:�@���@�b�@�ی@�v�@�$�@��@���@�iD@�8�@��`@��I@�c�@���@��Q@��$@�J#@�%F@���@�@��-@��h@��	@�l�@�&�@�@��@��L@�-�@�S&@�J@�rG@�9�@��@�@�>�@�Y@��@���@�m�@�!@�w2@�o@���@��	@���@���@�E�@�G@���@���@�%F@�Ĝ@�-�@��@��C@�4�@���@�1'@���@���@�]�@�0�@�V@��B@���@�4n@���@��	@�<6@���@�d�@�A�@��@���@�qv@�@O@�;@��@�[�@��@�t�@�+�@��M@��@��r@�oi@�
�@�e,@�V@��X@���@���@�bN@�7@���@�c@�zx@�`B@�<6@��U@�0U@�خ@��n@��4@�$t@���@���@�h
@�Xy@�C-@�7@��@��~@�.I@���@���@�{�@�8�@���@���@��k@��p@�l"@�5?@��@���@���@�{J@�Z�@�=@�C@��M@���@�C�@��t@�Z�@�/�@��@��@�Z�@�&�@��F@�|�@�&�@�҉@�� @�m�@�,=@��0@���@�u�@���@���@��h@���@�y>@�5?@��@���@�x@�V@��@���@�n�@�N�@�7�@�M@��Z@�@���@�qv@�4�@�ߤ@���@�Ta@�D�@���@���@��f@�u�@�<6@�o@�ی@�l�@��@��@��F@�e,@�Dg@�V@��@��,@���@�� @�n�@�	�@���@��@���@���@�a|@�?�@�{@�@U�@
=@~�@~u%@}�Z@}�z@}A @}	l@|Ɇ@|q@|4n@|b@{�m@{x@z�,@y�@ym]@y+�@x��@x��@xr�@xFt@w��@wH�@w"�@v��@u�=@u�@t֡@te�@s��@s��@r�M@r��@rh
@q��@q/@p�E@pS�@p@o]�@o�@n�@n��@nOv@m��@m@m�S@mzx@mf�@m#�@l��@l�p@lA�@kݘ@k�*@k�:@k��@ky�@k]�@ko@j��@j�@i�@i�X@im]@i�@h�[@h��@htT@h7�@g��@g@O@g(@f�]@f�r@fW�@f�@e��@eA @e�@d��@dz�@c��@c��@c��@b�y@bTa@b_@a�D@a��@ao @a�@`�@`I�@_�g@_��@_��@_��@_�{@_Z�@_@_(@^�y@^\�@^4@]�@]�@]��@]��@]*0@\�@\�@[t�@Z��@Z~�@Z1�@Y��@Y�@Y%@XɆ@X�u@XA�@W�@WE9@VZ�@V�@V
�@U�#@Uzx@U4@T�@T��@Tq@TV�@T1'@T	�@SW?@R͟@R}V@ROv@R-@Q��@Q+@P�@P��@Pm�@O�;@N�b@M�@M��@Mp�@M7L@L�v@L�@Lj@K�6@KF�@J�]@J�"@J�A@J��@IY�@H��@H�@I�@H��@HbN@G�m@G�@F?@FTa@E�)@E�"@D�P@D�z@C�K@C"�@B�!@A��@A�~@A^�@@�_@?��@?y�@?H�@?!-@>��@>�!@=�D@=��@=zx@=�7@=`B@=q@<�@<�Y@<M@;Mj@:�@:�b@:Ov@9�Z@9��@9Vm@9�@8ی@8��@7�}@7�{@7@6��@6��@6��@6}V@6E�@5��@5�X@5��@5<6@4��@4c�@3�@3�a@3��@3��@3]�@3S�@3@O@2��@2��@2a|@2)�@1�@1�^@1�~@1f�@1?}@1�@0�@0Z@/�]@/��@/��@/e�@/H�@/+@/@.��@.��@.��@._�@.�@-�@-�@-��@-<6@-#�@-@@,��@,�e@,S�@,,=@+��@*�c@*M�@)�@)`B@(�@(�j@(�@(A�@'��@',�@&�c@&�,@&��@&��@&i�@&GE@&-@&!�@%��@%�N@%��@%!�@$�K@$�_@$!@$  @#��@#�a@#J#@"��@"�]@"��@"$�@!�@!�N@!@!�t@!��@!�=@!zx@!<6@!�@ �5@ ��@ Z@ 1@˒@|�@E9@6z@�@��@�X@��@L0@�@�D@�o@��@�9@@`B@�@��@��@�I@g8@>B@$@�a@�	@v`@W?@�@ȴ@V@E�@�@��@��@u�@Q�@+�@��@�@�@�D@q@I�@M@�@�m@��@�V@iD@33@�@�@ȴ@v�@B[@@�@=q@!�@�@�@o @Vm@N<@Dg@��@ی@�@Ɇ@��@��@e�@N�@H@7�@"h@��@�a@�[@~�@e�@=@Z�@,�@)_@!-@�@�@�@�@o@��@�@�m@s�@�@�D@��@��@�@�@��@��@��@��@rG@c�@Y�@7L@%@Ɇ@��@�D@��@��@q@/�@��@��@��@�@�@��@��@�V@l�@RT@4�@�@�@ں@�<@�L@~�@^5@M�@O@�@�Z@�@��@k�@�@�@��@�@m�@D�@�@��@��@s@_p@C@
��@
�m@
��@
��@
Z�@
C�@
3�@
�@
@
�@	�@	k�@	�@��@��@�I@�@q@I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A���A��jA���A���A��A��}A�� A���A���A���A���Aѳ3Aѡ�Aџ�Aџ!Aѝ~Aќ�Aћ�Aћ�Aћ�Aћ=AњAњAї$AіSAѓ�Aщ7A�]�A�5tA��A�uA��A��A��A���A�� AО�A�B�A̚�A�XA��Aʾ�A�GA���A�P�A���A��%A�IRA�f�A���A�DA�C�A��mA��A{�.Ax4�Ary>Al�vAk�Aj�vAg�wAb��A]q�AW�mAVeAS�AP�)AKMjAI=AHu�AG��AF��AEHABbNA@�A>�WA=� A=��A=��A=VmA<	�A8��A7DgA7�A7>�A9҉A:A A9.IA2qA-�"A-��A,�.A*��A)��A'��A&1�A$��A"A %�Az�Ae,AE�A*�A�DA�6A�Au�A�gA�?A9XA�'A$�A��A?}A�@A��AMA��A��A��AtTA��A+kA��A�A]dA�|A�|A�QA�Ar�A"�A�FA9XA�4A��A��AGEA
��A
�A	�IA��APHAA�0A�OA�A\�A��AJ#A��A`�A($A��AE�A�gA
�Ao AA�A ��A ��@��w@���@���@��@��]@��[@�\)@�@���@��@�  @�a�@���@��3@���@���@�W?@�IR@���@��@��@�@��@�]�@�6@�@��@�8�@�ѷ@�@�,�@�2�@�ƨ@�b�@��?@�c�@�/�@�m]@�A@��@�ƨ@�F�@��@�c@�>�@��H@撣@�<�@���@�X@���@�*�@��'@��>@ᇔ@��@��]@��2@�1'@��)@�n/@ܾ@�  @�c�@���@�E�@٭C@�s�@ו�@և+@�
�@�+�@Ԗ�@�h
@�֡@��@ԗ�@Ԡ�@���@��c@ԇ+@�Z�@��@Ӛk@�w2@��?@�@�@х@��	@��U@ДF@��6@��m@�;�@��A@��@�%F@��@� \@���@̵�@�6@�j�@�H�@ɣn@ɀ4@��@�W�@� �@Ǘ$@�o�@�33@��m@�q�@��@�;d@�A�@�ԕ@Ó�@���@@�Ov@�@�H�@��@��$@�V�@�Q@�L0@���@��@�e�@�J@��r@��z@��0@��X@�@��m@���@���@�#:@���@��C@��4@���@��V@�Mj@��\@�_@��@���@�@@��@��B@���@�l"@�H@�
�@�:�@���@�b�@�ی@�v�@�$�@��@���@�iD@�8�@��`@��I@�c�@���@��Q@��$@�J#@�%F@���@�@��-@��h@��	@�l�@�&�@�@��@��L@�-�@�S&@�J@�rG@�9�@��@�@�>�@�Y@��@���@�m�@�!@�w2@�o@���@��	@���@���@�E�@�G@���@���@�%F@�Ĝ@�-�@��@��C@�4�@���@�1'@���@���@�]�@�0�@�V@��B@���@�4n@���@��	@�<6@���@�d�@�A�@��@���@�qv@�@O@�;@��@�[�@��@�t�@�+�@��M@��@��r@�oi@�
�@�e,@�V@��X@���@���@�bN@�7@���@�c@�zx@�`B@�<6@��U@�0U@�خ@��n@��4@�$t@���@���@�h
@�Xy@�C-@�7@��@��~@�.I@���@���@�{�@�8�@���@���@��k@��p@�l"@�5?@��@���@���@�{J@�Z�@�=@�C@��M@���@�C�@��t@�Z�@�/�@��@��@�Z�@�&�@��F@�|�@�&�@�҉@�� @�m�@�,=@��0@���@�u�@���@���@��h@���@�y>@�5?@��@���@�x@�V@��@���@�n�@�N�@�7�@�M@��Z@�@���@�qv@�4�@�ߤ@���@�Ta@�D�@���@���@��f@�u�@�<6@�o@�ی@�l�@��@��@��F@�e,@�Dg@�V@��@��,@���@�� @�n�@�	�@���@��@���@���@�a|@�?�@�{@�@U�@
=@~�@~u%@}�Z@}�z@}A @}	l@|Ɇ@|q@|4n@|b@{�m@{x@z�,@y�@ym]@y+�@x��@x��@xr�@xFt@w��@wH�@w"�@v��@u�=@u�@t֡@te�@s��@s��@r�M@r��@rh
@q��@q/@p�E@pS�@p@o]�@o�@n�@n��@nOv@m��@m@m�S@mzx@mf�@m#�@l��@l�p@lA�@kݘ@k�*@k�:@k��@ky�@k]�@ko@j��@j�@i�@i�X@im]@i�@h�[@h��@htT@h7�@g��@g@O@g(@f�]@f�r@fW�@f�@e��@eA @e�@d��@dz�@c��@c��@c��@b�y@bTa@b_@a�D@a��@ao @a�@`�@`I�@_�g@_��@_��@_��@_�{@_Z�@_@_(@^�y@^\�@^4@]�@]�@]��@]��@]*0@\�@\�@[t�@Z��@Z~�@Z1�@Y��@Y�@Y%@XɆ@X�u@XA�@W�@WE9@VZ�@V�@V
�@U�#@Uzx@U4@T�@T��@Tq@TV�@T1'@T	�@SW?@R͟@R}V@ROv@R-@Q��@Q+@P�@P��@Pm�@O�;@N�b@M�@M��@Mp�@M7L@L�v@L�@Lj@K�6@KF�@J�]@J�"@J�A@J��@IY�@H��@H�@I�@H��@HbN@G�m@G�@F?@FTa@E�)@E�"@D�P@D�z@C�K@C"�@B�!@A��@A�~@A^�@@�_@?��@?y�@?H�@?!-@>��@>�!@=�D@=��@=zx@=�7@=`B@=q@<�@<�Y@<M@;Mj@:�@:�b@:Ov@9�Z@9��@9Vm@9�@8ی@8��@7�}@7�{@7@6��@6��@6��@6}V@6E�@5��@5�X@5��@5<6@4��@4c�@3�@3�a@3��@3��@3]�@3S�@3@O@2��@2��@2a|@2)�@1�@1�^@1�~@1f�@1?}@1�@0�@0Z@/�]@/��@/��@/e�@/H�@/+@/@.��@.��@.��@._�@.�@-�@-�@-��@-<6@-#�@-@@,��@,�e@,S�@,,=@+��@*�c@*M�@)�@)`B@(�@(�j@(�@(A�@'��@',�@&�c@&�,@&��@&��@&i�@&GE@&-@&!�@%��@%�N@%��@%!�@$�K@$�_@$!@$  @#��@#�a@#J#@"��@"�]@"��@"$�@!�@!�N@!@!�t@!��@!�=@!zx@!<6@!�@ �5@ ��@ Z@ 1@˒@|�@E9@6z@�@��@�X@��@L0@�@�D@�o@��@�9@@`B@�@��@��@�I@g8@>B@$@�a@�	@v`@W?@�@ȴ@V@E�@�@��@��@u�@Q�@+�@��@�@�@�D@q@I�@M@�@�m@��@�V@iD@33@�@�@ȴ@v�@B[@@�@=q@!�@�@�@o @Vm@N<@Dg@��@ی@�@Ɇ@��@��@e�@N�@H@7�@"h@��@�a@�[@~�@e�@=@Z�@,�@)_@!-@�@�@�@�@o@��@�@�m@s�@�@�D@��@��@�@�@��@��@��@��@rG@c�@Y�@7L@%@Ɇ@��@�D@��@��@q@/�@��@��@��@�@�@��@��@�V@l�@RT@4�@�@�@ں@�<@�L@~�@^5@M�@O@�@�Z@�@��@k�@�@�@��@�@m�@D�@�@��@��@s@_p@C@
��@
�m@
��@
��@
Z�@
C�@
3�@
�@
@
�@	�@	k�@	�@��@��@�I@�@q@I�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B �B vB \B BB BB �B �B �B vB!-B!�B"�B#�B%,B%FB%FB%`B%zB%�B&B&LB&�B'B'RB'�B($B)�B2-BG_BW�B^�B`�Bb�BdtBh$Bk�BoBzDB�-B�KB�;B�B	�B	�B
�B
�B
3B	��B	�eB	�qB	��B	ʦB	�HB	��B	�B	�:B	��B	�zB	v`B	s�B	o�B	ezB	Y�B	NpB	?�B	9�B	1'B	%�B	!�B	~B	jB	�B	�B	(�B	# B	%FB	<jB	X�B	_�B	o5B	y�B	��B	}B	~B	�B	�HB	��B	��B	�2B	��B	�(B	��B	��B	��B	�XB	��B	y	B	qAB	a�B	TB	RoB	R�B	`vB	o�B	p�B	qvB	q[B	p�B	hXB	p�B	~B	��B	}�B	u?B	poB	y�B	|B	z�B	v�B	u�B	r�B	r�B	r�B	pUB	o B	o�B	s�B	x�B	}�B	�?B	��B	�2B	�$B	�yB	��B	��B	��B	��B	��B	��B	��B	��B	�9B	�4B	~�B	��B	��B	��B	��B	��B	�B	�:B	�B	��B	��B	�dB	�)B	�~B	��B	�IB	�5B	��B	�B	�OB	�pB	��B	�fB	��B	��B	��B	�*B	��B	�"B	�)B	�/B	�'B	�AB	�)B	�B	�/B	�/B	��B	�[B	�GB	��B	��B	�>B	��B	�0B	�JB	�JB	�jB	�jB	��B	�PB	�PB	�jB	��B	��B	��B	�BB	��B	�{B	�3B	ĜB	��B	�-B	��B	�9B	��B	��B	�1B	��B	ǮB	ňB	��B	ðB	��B	��B	�'B	�uB	��B	�B	�'B	ĜB	�_B	�KB	ʦB	�	B	�lB	�fB	�_B	�B	�+B	ȴB	�xB	�B	�WB	یB	�jB	�B	�B	�B	�B	��B	�`B	��B	�TB	�B	��B	��B	�'B	�vB	�B	�IB	ۦB	ںB	�WB	�B	�B	ߊB	��B	��B	�bB	ߊB	�IB	یB	�~B	��B	�IB	�VB	�-B	�B	�B	��B	�TB	�B	�B	�pB	�B	��B	��B	�NB	�TB	�B	��B	�B	��B	��B	��B	�)B	��B	��B	�B	��B	�B	��B	�|B	�B	�B	�B	�B	�+B	��B	��B	�B	�zB	�B	�`B	�FB	��B	��B	��B	�2B	�RB	�B	�8B	�lB	��B	��B	�>B	�PB	�B	��B	��B	��B	��B	�B	�B	��B
  B	�}B	�.B	��B
 B
 �B
 �B
 iB
 4B	��B	��B
 4B
 �B
 B
�B
uB
uB
[B
'B
B
uB
�B
 �B
�B
'B
-B
�B
�B
	�B

XB
DB
)B
�B
zB
�B

�B
JB
6B
6B
�B
�B
B
pB
�B
�B
�B
�B
B
<B
"B
�B
�B
vB
�B
\B
BB
BB
BB
�B
�B
�B
�B
hB
 B
B
hB
�B
�B
B
oB
[B
�B
B
B
�B
hB
�B
�B
�B
�B
aB
�B
B
MB
MB
B
$B
�B
�B
�B
�B
yB
eB
7B
�B
kB
	B
�B
�B
�B
�B
B
B
]B
�B
B
OB
�B
VB
�B
�B
�B
�B
!-B
"B
"�B
"�B
# B
#TB
#nB
#�B
#�B
#�B
$@B
$tB
$�B
%B
%�B
%�B
%�B
&LB
&�B
'B
'�B
'�B
(>B
)*B
)DB
)DB
)�B
*B
*�B
+�B
,qB
,WB
,qB
,WB
,WB
,�B
-B
-)B
-]B
./B
-�B
.}B
/5B
/OB
/iB
/�B
/�B
0oB
0�B
1'B
1'B
1�B
1�B
1�B
1�B
2-B
2-B
2aB
2|B
3B
3B
3�B
4nB
4�B
4�B
4�B
5?B
5?B
5�B
5�B
5�B
6B
6B
6+B
72B
7�B
88B
8lB
8�B
9XB
9>B
9�B
:DB
:�B
:�B
:�B
;B
;dB
;dB
<B
;�B
;�B
<6B
<jB
<jB
<jB
<�B
=B
=�B
=�B
>BB
>�B
>wB
>�B
>�B
?HB
?cB
?.B
?HB
?�B
?�B
?�B
?�B
?�B
?�B
@OB
@B
@B
@OB
@�B
@�B
A�B
AoB
A�B
AoB
A�B
BB
B'B
B'B
BAB
B[B
B[B
B[B
BuB
B�B
C-B
CGB
C-B
CGB
CaB
C{B
C{B
C�B
C�B
C�B
D3B
D�B
D3B
DB
DMB
D�B
D�B
D�B
EB
EmB
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F%B
FYB
F?B
F�B
F�B
F�B
F�B
G+B
G�B
G�B
G�B
H1B
H1B
H1B
H�B
H�B
IB
IB
I7B
IRB
I7B
I�B
I�B
IlB
IlB
J#B
JXB
JXB
JXB
JXB
JXB
J�B
J�B
J�B
KxB
K�B
LB
LdB
L�B
MB
N"B
N"B
N�B
O\B
O\B
O�B
O�B
O�B
OvB
O�B
O�B
O�B
PHB
PbB
PbB
PHB
PbB
PbB
P�B
QhB
QhB
Q�B
Q�B
RTB
RoB
R�B
RTB
RTB
R:B
RoB
R�B
R�B
R�B
R�B
S@B
S&B
S[B
S�B
T,B
U2B
U�B
VSB
W?B
U�B
U2B
U�B
V�B
W�B
XB
YB
X�B
X�B
ZkB
ZkB
ZB
YKB
YB
Y�B
Z7B
Y�B
YKB
YKB
X�B
Z�B
\�B
\�B
]B
]IB
]dB
]~B
^�B
_;B
_�B
`�B
a-B
a�B
a�B
bB
bhB
bhB
b�B
b�B
cTB
c�B
d@B
dZB
dtB
d@B
c�B
dZB
dZB
d�B
d�B
d�B
d�B
e,B
ezB
fLB
f�B
f�B
f�B
gB
g�B
g�B
h
B
h
B
h$B
h$B
h$B
h
B
h�B
h�B
h�B
h�B
iB
iDB
i_B
iyB
i�B
i�B
i�B
jKB
j�B
j�B
j�B
j�B
kB
kB
kQB
kkB
k�B
k�B
l=B
l�B
m)B
mCB
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
n�B
n�B
n�B
o B
oiB
oiB
o�B
pUB
p�B
q[B
qvB
qvB
q�B
q�B
q�B
q�B
r-B
r-B
rGB
raB
r|B
r�B
r�B
sMB
tnB
t�B
t�B
t�B
t�B
u%B
u?B
u?B
u�B
v+B
vFB
vFB
vFB
v`B
wB
w2B
wfB
wfB
wfB
wLB
wLB
xB
xRB
x�B
x�B
x�B
x�B
y	B
y$B
y>B
y�B
y�B
zB
y�B
y�B
zB
y�B
z^B
z�B
z�B
z�B
z�B
{0B
{0B
{B
{�B
{�B
{�B
{B
{�B
{�B
|PB
|PB
|�B
}B
}"B
}<B
}qB
}�B
}�B
}�B
}�B
~B
~(B
~]B
~wB
~wB
~�B
~�B
~�B
~�B
~�B
.B
HB
HB
�B
�B
�B
�B
�B
�B
�OB
��B
��B
��B
��B
� B
�;B
�UB
�UB
�UB
��B
��B
��B
�B
��B
�B
�'B
��B
��B
��B
��B
��B
�aB
�-B
�B
�B
��B
�B
�B
�B
�B
�GB
��B
�3B
��B
��B
�gB
�B
�mB
��B
�mB
��B
��B
��B
��B
��B
�B
��B
�%B
��B
��B
�zB
�zB
�_B
��B
��B
��B
�1B
�KB
�fB
��B
�fB
��B
��B
��B
�B
��B
�RB
�RB
��B
��B
��B
��B
�#B
�XB
�XB
��B
��B
��B
�B
�DB
�xB
��B
��B
��B
�B
�JB
��B
��B
�B
�B
�PB
�PB
��B
��B
��B
��B
�"B
�VB
�pB
�pB
��B
��B
�pB
��B
��B
��B
��B
��B
��B
��B
��B
�.111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B �B vB \B BB BB �B �B �B vB!-B!�B"�B#�B%,B%FB%FB%`B%zB%�B&B&LB&�B'B'RB'�B($B)�B2-BG_BW�B^�B`�Bb�BdtBh$Bk�BoBzDB�-B�KB�;B�B	�B	�B
�B
�B
3B	��B	�eB	�qB	��B	ʦB	�HB	��B	�B	�:B	��B	�zB	v`B	s�B	o�B	ezB	Y�B	NpB	?�B	9�B	1'B	%�B	!�B	~B	jB	�B	�B	(�B	# B	%FB	<jB	X�B	_�B	o5B	y�B	��B	}B	~B	�B	�HB	��B	��B	�2B	��B	�(B	��B	��B	��B	�XB	��B	y	B	qAB	a�B	TB	RoB	R�B	`vB	o�B	p�B	qvB	q[B	p�B	hXB	p�B	~B	��B	}�B	u?B	poB	y�B	|B	z�B	v�B	u�B	r�B	r�B	r�B	pUB	o B	o�B	s�B	x�B	}�B	�?B	��B	�2B	�$B	�yB	��B	��B	��B	��B	��B	��B	��B	��B	�9B	�4B	~�B	��B	��B	��B	��B	��B	�B	�:B	�B	��B	��B	�dB	�)B	�~B	��B	�IB	�5B	��B	�B	�OB	�pB	��B	�fB	��B	��B	��B	�*B	��B	�"B	�)B	�/B	�'B	�AB	�)B	�B	�/B	�/B	��B	�[B	�GB	��B	��B	�>B	��B	�0B	�JB	�JB	�jB	�jB	��B	�PB	�PB	�jB	��B	��B	��B	�BB	��B	�{B	�3B	ĜB	��B	�-B	��B	�9B	��B	��B	�1B	��B	ǮB	ňB	��B	ðB	��B	��B	�'B	�uB	��B	�B	�'B	ĜB	�_B	�KB	ʦB	�	B	�lB	�fB	�_B	�B	�+B	ȴB	�xB	�B	�WB	یB	�jB	�B	�B	�B	�B	��B	�`B	��B	�TB	�B	��B	��B	�'B	�vB	�B	�IB	ۦB	ںB	�WB	�B	�B	ߊB	��B	��B	�bB	ߊB	�IB	یB	�~B	��B	�IB	�VB	�-B	�B	�B	��B	�TB	�B	�B	�pB	�B	��B	��B	�NB	�TB	�B	��B	�B	��B	��B	��B	�)B	��B	��B	�B	��B	�B	��B	�|B	�B	�B	�B	�B	�+B	��B	��B	�B	�zB	�B	�`B	�FB	��B	��B	��B	�2B	�RB	�B	�8B	�lB	��B	��B	�>B	�PB	�B	��B	��B	��B	��B	�B	�B	��B
  B	�}B	�.B	��B
 B
 �B
 �B
 iB
 4B	��B	��B
 4B
 �B
 B
�B
uB
uB
[B
'B
B
uB
�B
 �B
�B
'B
-B
�B
�B
	�B

XB
DB
)B
�B
zB
�B

�B
JB
6B
6B
�B
�B
B
pB
�B
�B
�B
�B
B
<B
"B
�B
�B
vB
�B
\B
BB
BB
BB
�B
�B
�B
�B
hB
 B
B
hB
�B
�B
B
oB
[B
�B
B
B
�B
hB
�B
�B
�B
�B
aB
�B
B
MB
MB
B
$B
�B
�B
�B
�B
yB
eB
7B
�B
kB
	B
�B
�B
�B
�B
B
B
]B
�B
B
OB
�B
VB
�B
�B
�B
�B
!-B
"B
"�B
"�B
# B
#TB
#nB
#�B
#�B
#�B
$@B
$tB
$�B
%B
%�B
%�B
%�B
&LB
&�B
'B
'�B
'�B
(>B
)*B
)DB
)DB
)�B
*B
*�B
+�B
,qB
,WB
,qB
,WB
,WB
,�B
-B
-)B
-]B
./B
-�B
.}B
/5B
/OB
/iB
/�B
/�B
0oB
0�B
1'B
1'B
1�B
1�B
1�B
1�B
2-B
2-B
2aB
2|B
3B
3B
3�B
4nB
4�B
4�B
4�B
5?B
5?B
5�B
5�B
5�B
6B
6B
6+B
72B
7�B
88B
8lB
8�B
9XB
9>B
9�B
:DB
:�B
:�B
:�B
;B
;dB
;dB
<B
;�B
;�B
<6B
<jB
<jB
<jB
<�B
=B
=�B
=�B
>BB
>�B
>wB
>�B
>�B
?HB
?cB
?.B
?HB
?�B
?�B
?�B
?�B
?�B
?�B
@OB
@B
@B
@OB
@�B
@�B
A�B
AoB
A�B
AoB
A�B
BB
B'B
B'B
BAB
B[B
B[B
B[B
BuB
B�B
C-B
CGB
C-B
CGB
CaB
C{B
C{B
C�B
C�B
C�B
D3B
D�B
D3B
DB
DMB
D�B
D�B
D�B
EB
EmB
E�B
E�B
E�B
E�B
E�B
E�B
E�B
F%B
FYB
F?B
F�B
F�B
F�B
F�B
G+B
G�B
G�B
G�B
H1B
H1B
H1B
H�B
H�B
IB
IB
I7B
IRB
I7B
I�B
I�B
IlB
IlB
J#B
JXB
JXB
JXB
JXB
JXB
J�B
J�B
J�B
KxB
K�B
LB
LdB
L�B
MB
N"B
N"B
N�B
O\B
O\B
O�B
O�B
O�B
OvB
O�B
O�B
O�B
PHB
PbB
PbB
PHB
PbB
PbB
P�B
QhB
QhB
Q�B
Q�B
RTB
RoB
R�B
RTB
RTB
R:B
RoB
R�B
R�B
R�B
R�B
S@B
S&B
S[B
S�B
T,B
U2B
U�B
VSB
W?B
U�B
U2B
U�B
V�B
W�B
XB
YB
X�B
X�B
ZkB
ZkB
ZB
YKB
YB
Y�B
Z7B
Y�B
YKB
YKB
X�B
Z�B
\�B
\�B
]B
]IB
]dB
]~B
^�B
_;B
_�B
`�B
a-B
a�B
a�B
bB
bhB
bhB
b�B
b�B
cTB
c�B
d@B
dZB
dtB
d@B
c�B
dZB
dZB
d�B
d�B
d�B
d�B
e,B
ezB
fLB
f�B
f�B
f�B
gB
g�B
g�B
h
B
h
B
h$B
h$B
h$B
h
B
h�B
h�B
h�B
h�B
iB
iDB
i_B
iyB
i�B
i�B
i�B
jKB
j�B
j�B
j�B
j�B
kB
kB
kQB
kkB
k�B
k�B
l=B
l�B
m)B
mCB
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
n/B
n�B
n�B
n�B
o B
oiB
oiB
o�B
pUB
p�B
q[B
qvB
qvB
q�B
q�B
q�B
q�B
r-B
r-B
rGB
raB
r|B
r�B
r�B
sMB
tnB
t�B
t�B
t�B
t�B
u%B
u?B
u?B
u�B
v+B
vFB
vFB
vFB
v`B
wB
w2B
wfB
wfB
wfB
wLB
wLB
xB
xRB
x�B
x�B
x�B
x�B
y	B
y$B
y>B
y�B
y�B
zB
y�B
y�B
zB
y�B
z^B
z�B
z�B
z�B
z�B
{0B
{0B
{B
{�B
{�B
{�B
{B
{�B
{�B
|PB
|PB
|�B
}B
}"B
}<B
}qB
}�B
}�B
}�B
}�B
~B
~(B
~]B
~wB
~wB
~�B
~�B
~�B
~�B
~�B
.B
HB
HB
�B
�B
�B
�B
�B
�B
�OB
��B
��B
��B
��B
� B
�;B
�UB
�UB
�UB
��B
��B
��B
�B
��B
�B
�'B
��B
��B
��B
��B
��B
�aB
�-B
�B
�B
��B
�B
�B
�B
�B
�GB
��B
�3B
��B
��B
�gB
�B
�mB
��B
�mB
��B
��B
��B
��B
��B
�B
��B
�%B
��B
��B
�zB
�zB
�_B
��B
��B
��B
�1B
�KB
�fB
��B
�fB
��B
��B
��B
�B
��B
�RB
�RB
��B
��B
��B
��B
�#B
�XB
�XB
��B
��B
��B
�B
�DB
�xB
��B
��B
��B
�B
�JB
��B
��B
�B
�B
�PB
�PB
��B
��B
��B
��B
�"B
�VB
�pB
�pB
��B
��B
�pB
��B
��B
��B
��B
��B
��B
��B
��B
�.111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20221214124228  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8b                                                                20221214124258  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20221214124259  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20221214124301                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20221214124301  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20221214124301  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20221214125940                      G�O�G�O�G�O�                