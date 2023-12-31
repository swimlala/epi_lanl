CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:36:19Z creation;2022-06-04T17:36:20Z conversion to V3.1      
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
_FillValue                 �  I<   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M,   PRES_ADJUSTED_QC         
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
_FillValue                 �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
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
resolution        :�o     �  �D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޼   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �    HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �(   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �h   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �x   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �|   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604173619  20220610131507  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               LA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @�cL��Ř1   @�cL�r�b@0}/��w�c��;dZ1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @33@�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  A�33B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�ffB���B�  B�  B�  B�  B�  B�  B�  B̙�B�  B�  B�33B�  B�  B�33B���B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C�C�C�C�C  C�fC�fC�fC  C   C"  C$  C&  C(  C*  C,  C.  C0  C2L�C3ffC5�fC8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF�fDG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @(�@x��@�G�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A�Q�B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB���B�.B��{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�aHB�ǮB�ǮB���B�ǮB�ǮB���B�{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C�qC�qC�qC�qC��C�=C�=C�=C��C��C!��C#��C%��C'��C)��C+��C-��C/��C20�C3J=C5�=C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY�qC[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DF\DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D���D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D��D���D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�E�A�DgA�C-A�DgA�K^A�K^A�NA�B�A�A�A�EA�B'A�B�A�;dA�2-A�4�A�2�A�*0A��A��A���A��+A���A��|A��A���A���A��|A���A��A���A���A��A��A��ZA���A���A��+A��ZA���A���A��sA���A�ѷAȑ�A�V9Aǽ�AƭwA��AŶ�A�IA��?A���A��A��A�IRA��mA��A���A��A��^A��4A�4A�A�ޞA�.�A���A�lWA���A�C-A��A�]�A�Z�A��yA�,�A��'A�<6A�NpA��A�n/A�� A��A�49A���A��{A���A���A�
rA���A~C-A{u�An��Af?}Abu%A^  AZ�LAW�]AV��AU�jAR��AO�ANH�AK�6AI iAG��AD��A?�A= iA;��A9+A8	A6-A5MA3��A2w2A0�4A0��A2kQA2-wA2�A0�A.��A.=qA-�0A-�hA-ffA-�AA-�hA/4A/�FA0C�A0�A0zxA/�]A/g8A/V�A.ĜA.!A-�7A,��A,B�A,�A+tTA*��A*C-A)�'A)m�A)^5A):�A)A(��A(0UA'M�A%��A$�A$OvA$	lA#�IA"�rA"f�A"VA!�A!S�A ��A <�A�nA�^A��A/�A4�A�A_pA�A��A�oA�KA��A�+A�A��A�9A�/A�cA�yA�sAtTAZ�A>BA�AR�ARTA��APHA�A�QA��AQAeA �AP�A�AA��AY�A�A]dA^5A6�A�tAxlA�A��AP�A�`A�dA{�A�A��A�A�"AtTA�Ag8AA_A"�AA
�+A
��A
��A
VmA
2�A	˒A	�VA	�fA	w2A	QA	SA�!A��AQA�AA�A�A�1AsA($A	A�]A��A�PA��A/�A�vA�RA"�A�A��A=qA	A��A}�Au�A_pA!-A �A S�@�S�@��s@���@��@�M@���@���@�O@��1@��@���@���@�33@��e@�N�@��@�j�@���@���@�@�a�@� \@�^5@���@�H�@�%@��H@���@��o@�+k@���@��@�*�@��@�h�@���@밊@�:�@�H�@�^@��@�H�@��'@��@瞄@��@�x@�@O@�PH@�x�@��p@�1'@�+@�.�@��@�G@��@َ�@� i@��p@�v�@׵t@�&�@��|@��@��/@���@�r�@��;@�B�@ԸR@�:*@ӎ�@�^�@�֡@���@�+@���@К�@�6@ζ�@��@�IR@�@��]@̔F@�J�@�<�@�A�@ˤ@@�5�@ʦL@�6@�@���@ɇ�@�l�@�+@���@Ⱦ@�6�@��@ƿ�@��@� �@��K@�o�@�C@İ!@�:*@��@�\�@�\�@�~@��@�s@���@��*@��n@��Z@���@�@���@���@��@��@���@��I@�kQ@�dZ@��z@�kQ@�� @�p�@��@�c @�D�@�@�e,@���@���@�S�@�&@���@��`@���@�z�@�1@���@�U�@�(�@�V@���@�6@��@�a@��@���@���@�[�@�#:@���@���@�qv@�S&@�Y@��]@��X@���@�:�@���@��]@��@���@��@���@��g@���@���@�2a@�S@��?@�c @��A@��&@�M@���@���@�v�@�Q@�D�@�8�@�(�@�@���@��@��A@��A@��A@��>@��j@��N@���@��	@���@�.�@��o@��K@��*@�qv@�S&@��8@�L0@�ԕ@��	@��@���@��@�1'@��>@��*@���@�m]@�O�@�;d@�&�@��@��P@���@�ߤ@�҉@���@�9X@�33@��2@���@�h
@�e@��Q@��V@�c�@���@� �@�C�@�~(@���@��@��.@�m�@�A�@��#@�?}@���@�&�@��K@�p�@��@�͟@�u%@��&@�p�@�"�@��@��	@��B@���@���@�s�@�	@�	@��@���@�w2@�2a@��8@��H@��e@�5?@�@�-�@�ϫ@�>�@��	@��+@�x@��9@�u�@�&�@��@��]@��D@�YK@�3�@��@��;@�]�@��8@��4@�l�@�L0@�#:@�_@��@�n/@�j�@�Y�@�+@��@���@�Z�@�$�@��@��M@�
=@��p@��}@���@�u%@�	@���@��h@�^�@�O@�@@��	@��	@���@��@��}@��x@���@�C�@�A @��v@���@�kQ@�($@�#:@�,=@���@��W@�خ@��X@�J�@�͟@�q�@�"h@���@�o�@�[W@�%F@�@��@���@��x@���@�l"@�Ov@��@�`B@�@��,@�kQ@��@��@9�@~�@~E�@}�~@}�@|��@|  @{�w@{��@{�@zc @y�o@y�^@yL�@x��@x��@x|�@wK�@v�H@vz@vJ�@vH�@v_@u�@u�n@u}�@u`B@u	l@t�_@tN�@s�@sg�@r�@r��@r��@rn�@r;�@q�H@q��@qS&@p�/@p7@o�F@o��@o�	@oqv@n�@n�A@m��@m|@m7L@l"h@kj�@j��@j�A@jkQ@jn�@jff@jL0@j@�@j:*@j($@i��@io @h�f@h`�@g\)@g@f�B@f:*@e�@e�h@e7L@d��@dPH@d%�@c�@c��@c&@b}V@bTa@b;�@b#:@a�>@a@a��@a5�@`��@`I�@_�]@_S@^�s@^��@^a|@]�@]N<@\ѷ@\��@\C-@[�}@[$t@[�@Y��@X�@X"h@W��@W��@W��@W��@W��@W�$@W�k@W�V@W��@W��@W�V@W��@W+@U�C@T��@T��@T��@Te�@T6@S��@S�	@SF�@Rȴ@R_�@Re@R_@Q��@Q��@Q`B@Q4@Q�@P�@P��@O�@O�@N!�@MJ�@L�u@L_@LN�@L@KRT@J�b@Jff@I�d@I�@I�@Iw2@Ie,@I:�@Iq@I�@H�@H��@Hoi@HQ�@H4n@G��@Gv`@G@O@Fȴ@F�+@E�@D��@D��@D��@D`�@C�}@B�@Bq�@BGE@B6�@B&�@B	@A�@A��@A\�@A�@@��@@$@?��@?�@?�0@?��@?iD@>��@=�@=}�@<�@;�w@;/�@:�@:��@:i�@:Ta@9��@8��@7�Q@7��@7O@7�@6�}@6J@5L�@5@@4��@4u�@4:�@4"h@4b@3�@3��@3o@2�,@2�R@2��@2�6@2s�@2@2�@2J@1j@1�@1@0ی@0��@0q@/��@/�{@/C�@.�c@.��@.:*@-��@-��@,�P@,l"@+�@+S�@+@*�@*�c@*�]@*�@*:*@*4@)�@)�h@)N<@(�K@(�e@(��@(oi@'�@'��@'J#@&d�@&	@&{@&	@%�@%��@%+@$��@$ی@$�?@$�9@$�j@$�O@$�u@$u�@$~(@$�@$e�@$:�@#��@#�@"=q@!��@!�)@!�9@!�"@!|@!}�@!u�@!IR@!@@ ��@ ��@ >B@�W@��@��@�P@j�@.I@��@�<@@�@($@�X@@�)@2�@�r@�a@~�@�8@�@xl@p;@l�@kQ@R�@&�@O@�@&�@�@�@�@
�@�@@�Z@�@�d@�3@�t@�'@Y�@@@�@oi@PH@7@�	@�@z@?@�@��@��@ԕ@�S@<6@��@��@��@��@y>@w�@U2@-�@�@1@�@�w@qv@E9@8@�@ȴ@��@0U@�H@zx@	l@��@��@�D@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A�E�A�DgA�C-A�DgA�K^A�K^A�NA�B�A�A�A�EA�B'A�B�A�;dA�2-A�4�A�2�A�*0A��A��A���A��+A���A��|A��A���A���A��|A���A��A���A���A��A��A��ZA���A���A��+A��ZA���A���A��sA���A�ѷAȑ�A�V9Aǽ�AƭwA��AŶ�A�IA��?A���A��A��A�IRA��mA��A���A��A��^A��4A�4A�A�ޞA�.�A���A�lWA���A�C-A��A�]�A�Z�A��yA�,�A��'A�<6A�NpA��A�n/A�� A��A�49A���A��{A���A���A�
rA���A~C-A{u�An��Af?}Abu%A^  AZ�LAW�]AV��AU�jAR��AO�ANH�AK�6AI iAG��AD��A?�A= iA;��A9+A8	A6-A5MA3��A2w2A0�4A0��A2kQA2-wA2�A0�A.��A.=qA-�0A-�hA-ffA-�AA-�hA/4A/�FA0C�A0�A0zxA/�]A/g8A/V�A.ĜA.!A-�7A,��A,B�A,�A+tTA*��A*C-A)�'A)m�A)^5A):�A)A(��A(0UA'M�A%��A$�A$OvA$	lA#�IA"�rA"f�A"VA!�A!S�A ��A <�A�nA�^A��A/�A4�A�A_pA�A��A�oA�KA��A�+A�A��A�9A�/A�cA�yA�sAtTAZ�A>BA�AR�ARTA��APHA�A�QA��AQAeA �AP�A�AA��AY�A�A]dA^5A6�A�tAxlA�A��AP�A�`A�dA{�A�A��A�A�"AtTA�Ag8AA_A"�AA
�+A
��A
��A
VmA
2�A	˒A	�VA	�fA	w2A	QA	SA�!A��AQA�AA�A�A�1AsA($A	A�]A��A�PA��A/�A�vA�RA"�A�A��A=qA	A��A}�Au�A_pA!-A �A S�@�S�@��s@���@��@�M@���@���@�O@��1@��@���@���@�33@��e@�N�@��@�j�@���@���@�@�a�@� \@�^5@���@�H�@�%@��H@���@��o@�+k@���@��@�*�@��@�h�@���@밊@�:�@�H�@�^@��@�H�@��'@��@瞄@��@�x@�@O@�PH@�x�@��p@�1'@�+@�.�@��@�G@��@َ�@� i@��p@�v�@׵t@�&�@��|@��@��/@���@�r�@��;@�B�@ԸR@�:*@ӎ�@�^�@�֡@���@�+@���@К�@�6@ζ�@��@�IR@�@��]@̔F@�J�@�<�@�A�@ˤ@@�5�@ʦL@�6@�@���@ɇ�@�l�@�+@���@Ⱦ@�6�@��@ƿ�@��@� �@��K@�o�@�C@İ!@�:*@��@�\�@�\�@�~@��@�s@���@��*@��n@��Z@���@�@���@���@��@��@���@��I@�kQ@�dZ@��z@�kQ@�� @�p�@��@�c @�D�@�@�e,@���@���@�S�@�&@���@��`@���@�z�@�1@���@�U�@�(�@�V@���@�6@��@�a@��@���@���@�[�@�#:@���@���@�qv@�S&@�Y@��]@��X@���@�:�@���@��]@��@���@��@���@��g@���@���@�2a@�S@��?@�c @��A@��&@�M@���@���@�v�@�Q@�D�@�8�@�(�@�@���@��@��A@��A@��A@��>@��j@��N@���@��	@���@�.�@��o@��K@��*@�qv@�S&@��8@�L0@�ԕ@��	@��@���@��@�1'@��>@��*@���@�m]@�O�@�;d@�&�@��@��P@���@�ߤ@�҉@���@�9X@�33@��2@���@�h
@�e@��Q@��V@�c�@���@� �@�C�@�~(@���@��@��.@�m�@�A�@��#@�?}@���@�&�@��K@�p�@��@�͟@�u%@��&@�p�@�"�@��@��	@��B@���@���@�s�@�	@�	@��@���@�w2@�2a@��8@��H@��e@�5?@�@�-�@�ϫ@�>�@��	@��+@�x@��9@�u�@�&�@��@��]@��D@�YK@�3�@��@��;@�]�@��8@��4@�l�@�L0@�#:@�_@��@�n/@�j�@�Y�@�+@��@���@�Z�@�$�@��@��M@�
=@��p@��}@���@�u%@�	@���@��h@�^�@�O@�@@��	@��	@���@��@��}@��x@���@�C�@�A @��v@���@�kQ@�($@�#:@�,=@���@��W@�خ@��X@�J�@�͟@�q�@�"h@���@�o�@�[W@�%F@�@��@���@��x@���@�l"@�Ov@��@�`B@�@��,@�kQ@��@��@9�@~�@~E�@}�~@}�@|��@|  @{�w@{��@{�@zc @y�o@y�^@yL�@x��@x��@x|�@wK�@v�H@vz@vJ�@vH�@v_@u�@u�n@u}�@u`B@u	l@t�_@tN�@s�@sg�@r�@r��@r��@rn�@r;�@q�H@q��@qS&@p�/@p7@o�F@o��@o�	@oqv@n�@n�A@m��@m|@m7L@l"h@kj�@j��@j�A@jkQ@jn�@jff@jL0@j@�@j:*@j($@i��@io @h�f@h`�@g\)@g@f�B@f:*@e�@e�h@e7L@d��@dPH@d%�@c�@c��@c&@b}V@bTa@b;�@b#:@a�>@a@a��@a5�@`��@`I�@_�]@_S@^�s@^��@^a|@]�@]N<@\ѷ@\��@\C-@[�}@[$t@[�@Y��@X�@X"h@W��@W��@W��@W��@W��@W�$@W�k@W�V@W��@W��@W�V@W��@W+@U�C@T��@T��@T��@Te�@T6@S��@S�	@SF�@Rȴ@R_�@Re@R_@Q��@Q��@Q`B@Q4@Q�@P�@P��@O�@O�@N!�@MJ�@L�u@L_@LN�@L@KRT@J�b@Jff@I�d@I�@I�@Iw2@Ie,@I:�@Iq@I�@H�@H��@Hoi@HQ�@H4n@G��@Gv`@G@O@Fȴ@F�+@E�@D��@D��@D��@D`�@C�}@B�@Bq�@BGE@B6�@B&�@B	@A�@A��@A\�@A�@@��@@$@?��@?�@?�0@?��@?iD@>��@=�@=}�@<�@;�w@;/�@:�@:��@:i�@:Ta@9��@8��@7�Q@7��@7O@7�@6�}@6J@5L�@5@@4��@4u�@4:�@4"h@4b@3�@3��@3o@2�,@2�R@2��@2�6@2s�@2@2�@2J@1j@1�@1@0ی@0��@0q@/��@/�{@/C�@.�c@.��@.:*@-��@-��@,�P@,l"@+�@+S�@+@*�@*�c@*�]@*�@*:*@*4@)�@)�h@)N<@(�K@(�e@(��@(oi@'�@'��@'J#@&d�@&	@&{@&	@%�@%��@%+@$��@$ی@$�?@$�9@$�j@$�O@$�u@$u�@$~(@$�@$e�@$:�@#��@#�@"=q@!��@!�)@!�9@!�"@!|@!}�@!u�@!IR@!@@ ��@ ��@ >B@�W@��@��@�P@j�@.I@��@�<@@�@($@�X@@�)@2�@�r@�a@~�@�8@�@xl@p;@l�@kQ@R�@&�@O@�@&�@�@�@�@
�@�@@�Z@�@�d@�3@�t@�'@Y�@@@�@oi@PH@7@�	@�@z@?@�@��@��@ԕ@�S@<6@��@��@��@��@y>@w�@U2@-�@�@1@�@�w@qv@E9@8@�@ȴ@��@0U@�H@zx@	l@��@��@�D@��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B� B�B� B�:B�TB� B�B�NB�hB�hB�B�4B�B�B�|B�BߤBߊBߤBߊBߊBߊBߊB��B��B�B�B�BB��B��B�B�HB�HB�bB�B�NB�B�B�B�.B	B	�B	6�B	_B	�xB	�VB
�4B
�eB
�B
�\B
چB
� B)�B;dB@OB@�B@�BB�BI�BYeBv�B��B�B�tB�B��B��B�QBraB[qBG�B1vB
��B
��B
��B
bNB
RoB
OBB
O�B
RoB
^�B
TFB
KB
FB
,WB
,�B
�B	�B	�B	��B	n�B	U�B	;�B	$�B	NB		lB	�B��B�B��B�B׍B��BٴB�B��B�JB�}B� BߊB��B�5B�nB��B	�B	Y�B	gB	mCB	n�B	rGB	��B	��B	�2B	��B	�B	�fB	��B
�B
�B
+�B
2B
8�B
=<B
<�B
A�B
FB
J�B
L�B
L�B
M6B
OvB
O�B
M�B
J	B
L�B
R B
T,B
T�B
R�B
T�B
VB
S�B
P�B
Q�B
P�B
OB
LJB
K�B
L�B
J�B
L�B
L~B
J�B
ESB
A�B
>�B
;�B
=�B
?.B
>�B
BAB
JXB
H1B
I�B
JXB
H�B
HB
HB
HKB
N�B
R:B
RoB
T�B
UB
U�B
UMB
TFB
R B
T{B
S�B
PB
PbB
N�B
MjB
K)B
J#B
K)B
G�B
B�B
A�B
@B
>�B
G+B
K�B
KB
IRB
G�B
E�B
B[B
?�B
;0B
8�B
0�B
%�B
(
B
,B
+�B
*0B
(�B
#�B
"�B
$tB
&2B
(
B
'�B
'�B
(�B
-wB
2|B
4�B
3hB
3MB
3B
2GB
1'B
/�B
.�B
.�B
,"B
(�B
'�B
'B
&�B
%`B
%B
#�B
"hB
"4B
 vB
B
OB
�B
5B
B
!HB
#:B
"�B
"hB
!HB
!-B
 �B
 �B
 'B
!|B
!HB
!|B
"�B
#�B
$tB
$&B
#:B
!�B
�B
B
B
CB
=B
�B
�B
�B
�B
0B
�B

XB
	lB
	B
+B
tB
B
gB
�B
{B
�B
�B
�B
B
B
�B
[B
AB
�B
UB
B
 4B
 �B
 �B
�B
 B
 4B
 �B	�HB	��B	��B	�B	��B	��B	��B	�2B	��B	�B	��B	�WB	�B	�WB	�qB	�)B	�"B	�=B	�B	�B	�B	�"B	�B	��B	��B	�B	��B	�B	�oB	�;B	�;B	��B	�2B	�?B	��B	�B	�'B	��B	�B	��B	�B	��B	�ZB	��B	�TB	��B	�%B	��B	�2B	�2B	�B	��B	��B	�2B	��B	��B	�B	��B	�B	�3B	��B	��B	�B	�|B	��B	��B	�B	�B	�B	��B	�lB	��B	�B
 �B
�B
	�B
	�B
	7B
�B
mB
�B
?B
�B
B
�B
�B
�B
�B
�B
;B
 �B
 �B
 �B
;B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
UB
;B
'B
�B
�B
'B
�B
B
 �B	�HB	�HB	��B	��B	��B	�.B	�HB	�.B	�.B	�cB	��B
 iB
 OB
;B
oB
UB
�B
�B
�B
�B
9B
B
�B
mB
9B
B
B
�B
+B

#B
�B
B
&B
@B
�B
�B
�B
�B
�B
�B
�B
�B
[B
[B
uB
[B
�B
@B
�B
FB
�B
FB
B
B
�B
SB
mB
mB
�B
�B
B
2B
�B
�B
mB
YB
�B
�B
�B
QB
QB
7B
�B
�B
�B
�B
+B
�B
�B
�B
yB
_B
�B
eB
B
B
B
�B
9B
�B
,B
�B
�B
B
�B
yB
�B
KB
�B
�B
B
kB
B
kB
�B
�B
	B
�B
�B
)B
)B
�B
�B
�B
�B
B
�B
�B
�B
B
�B
�B
dB
�B
5B
�B
CB
�B
�B
�B
�B
	B
#B
=B
)B
xB
)B
�B
CB
�B
~B
�B
�B
�B
�B
�B
!B
pB
B
�B
�B
;B
 'B
 \B
!B
 �B
!-B
!HB
 �B
!|B
!HB
!�B
"B
"�B
"�B
# B
"�B
#�B
#TB
#TB
# B
#�B
#�B
$B
#�B
$@B
%�B
$�B
%,B
%FB
&B
%�B
%,B
&fB
&B
%�B
%�B
'8B
&�B
'8B
(XB
)�B
*0B
*KB
+kB
+B
+kB
+�B
+�B
+�B
+�B
+�B
,B
-B
-�B
-�B
.�B
.�B
/�B
/�B
/�B
0�B
1AB
1�B
1�B
2-B
2�B
2�B
3�B
4B
4�B
4�B
5ZB
5?B
5�B
5�B
7B
7LB
7�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
88B
8RB
8�B
9	B
9XB
8�B
9XB
9>B
9rB
9$B
9�B
9�B
9�B
:DB
;B
;B
;0B
;B
;B
;�B
;�B
<�B
<�B
<jB
=VB
=�B
=�B
>B
>(B
=�B
>B
=�B
>(B
=�B
=qB
>wB
>BB
>�B
?B
@4B
?�B
@B
@iB
@�B
@�B
@�B
AUB
AUB
AoB
A�B
A�B
A�B
B�B
B�B
CB
B�B
CB
C-B
C-B
C�B
C�B
D�B
C�B
ESB
ESB
EB
EmB
E�B
E�B
F%B
F?B
FYB
F�B
F�B
F�B
G�B
HfB
G�B
IRB
IRB
IB
HfB
H�B
H�B
H�B
H�B
H�B
H�B
H�B
HfB
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J	B
I�B
J#B
J#B
J	B
J�B
J#B
J#B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
MB
M�B
NpB
NVB
NpB
N�B
O�B
P.B
P}B
P�B
QNB
Q4B
Q�B
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
RTB
R:B
SB
R�B
R�B
S[B
R�B
S[B
TB
S�B
S&B
S�B
TB
T�B
T�B
T�B
T�B
T�B
T�B
UMB
U2B
U2B
UMB
U�B
U�B
U�B
U�B
U�B
VB
U�B
VmB
VSB
U�B
U�B
UB
U�B
UMB
U�B
UgB
U�B
T�B
T�B
T�B
U2B
UgB
U�B
VB
VSB
W�B
W$B
V�B
W?B
W�B
WYB
W�B
W�B
X+B
Y1B
X�B
Y�B
YB
Y�B
Y�B
[	B
Z�B
Z�B
\)B
[�B
[�B
[�B
[�B
\�B
\�B
]�B
]�B
]�B
^OB
^�B
^B
^jB
^�B
`'B
`�B
`�B
a|B
abB
a-B
abB
a|B
c B
b�B
cTB
c B
c:B
c�B
dZB
dtB
dtB
e`B
fB
e�B
gB
g8B
f�B
f�B
g8B
g8B
g�B
g�B
gmB
hXB
h
B
g�B
h$B
hsB
h$B
g�B
g�B
h>B
h�B
h>B
i_B
j0B
kB
kB
j�B
kB
k�B
kB
j�B
k�B
k�B
lqB
l=B
mB
lqB
l�B
m)B
mB
m�B
mwB
m�B
m�B
m�B
m]B
m�B
m�B
nIB
n�B
n�B
o�B
o�B
p;B
pUB
p;B
p!B
pUB
pUB
qB
p�B
qB
q[B
p�B
q�B
p�B
p�B
qB
qB
qB
q[B
q'B
qvB
qAB
q[B
qAB
q�B
q�B
q�B
r�B
rB
rGB
sB
s�B
tnB
t�B
t�B
t9B
t9B
t9B
uB
t�B
t�B
u%B
u�B
uB
u�B
u?B
uZB
u�B
utB
utB
u�B
utB
uZB
u�B
u%B
utB
u�B
u�B
v�B
vzB
v�B
wfB
w�B
w�B
w�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B�B� B�B� B�:B�TB� B�B�NB�hB�hB�B�4B�B�B�|B�BߤBߊBߤBߊBߊBߊBߊB��B��B�B�B�BB��B��B�B�HB�HB�bB�B�NB�B�B�B�.B	B	�B	6�B	_B	�xB	�VB
�4B
�eB
�B
�\B
چB
� B)�B;dB@OB@�B@�BB�BI�BYeBv�B��B�B�tB�B��B��B�QBraB[qBG�B1vB
��B
��B
��B
bNB
RoB
OBB
O�B
RoB
^�B
TFB
KB
FB
,WB
,�B
�B	�B	�B	��B	n�B	U�B	;�B	$�B	NB		lB	�B��B�B��B�B׍B��BٴB�B��B�JB�}B� BߊB��B�5B�nB��B	�B	Y�B	gB	mCB	n�B	rGB	��B	��B	�2B	��B	�B	�fB	��B
�B
�B
+�B
2B
8�B
=<B
<�B
A�B
FB
J�B
L�B
L�B
M6B
OvB
O�B
M�B
J	B
L�B
R B
T,B
T�B
R�B
T�B
VB
S�B
P�B
Q�B
P�B
OB
LJB
K�B
L�B
J�B
L�B
L~B
J�B
ESB
A�B
>�B
;�B
=�B
?.B
>�B
BAB
JXB
H1B
I�B
JXB
H�B
HB
HB
HKB
N�B
R:B
RoB
T�B
UB
U�B
UMB
TFB
R B
T{B
S�B
PB
PbB
N�B
MjB
K)B
J#B
K)B
G�B
B�B
A�B
@B
>�B
G+B
K�B
KB
IRB
G�B
E�B
B[B
?�B
;0B
8�B
0�B
%�B
(
B
,B
+�B
*0B
(�B
#�B
"�B
$tB
&2B
(
B
'�B
'�B
(�B
-wB
2|B
4�B
3hB
3MB
3B
2GB
1'B
/�B
.�B
.�B
,"B
(�B
'�B
'B
&�B
%`B
%B
#�B
"hB
"4B
 vB
B
OB
�B
5B
B
!HB
#:B
"�B
"hB
!HB
!-B
 �B
 �B
 'B
!|B
!HB
!|B
"�B
#�B
$tB
$&B
#:B
!�B
�B
B
B
CB
=B
�B
�B
�B
�B
0B
�B

XB
	lB
	B
+B
tB
B
gB
�B
{B
�B
�B
�B
B
B
�B
[B
AB
�B
UB
B
 4B
 �B
 �B
�B
 B
 4B
 �B	�HB	��B	��B	�B	��B	��B	��B	�2B	��B	�B	��B	�WB	�B	�WB	�qB	�)B	�"B	�=B	�B	�B	�B	�"B	�B	��B	��B	�B	��B	�B	�oB	�;B	�;B	��B	�2B	�?B	��B	�B	�'B	��B	�B	��B	�B	��B	�ZB	��B	�TB	��B	�%B	��B	�2B	�2B	�B	��B	��B	�2B	��B	��B	�B	��B	�B	�3B	��B	��B	�B	�|B	��B	��B	�B	�B	�B	��B	�lB	��B	�B
 �B
�B
	�B
	�B
	7B
�B
mB
�B
?B
�B
B
�B
�B
�B
�B
�B
;B
 �B
 �B
 �B
;B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
UB
;B
'B
�B
�B
'B
�B
B
 �B	�HB	�HB	��B	��B	��B	�.B	�HB	�.B	�.B	�cB	��B
 iB
 OB
;B
oB
UB
�B
�B
�B
�B
9B
B
�B
mB
9B
B
B
�B
+B

#B
�B
B
&B
@B
�B
�B
�B
�B
�B
�B
�B
�B
[B
[B
uB
[B
�B
@B
�B
FB
�B
FB
B
B
�B
SB
mB
mB
�B
�B
B
2B
�B
�B
mB
YB
�B
�B
�B
QB
QB
7B
�B
�B
�B
�B
+B
�B
�B
�B
yB
_B
�B
eB
B
B
B
�B
9B
�B
,B
�B
�B
B
�B
yB
�B
KB
�B
�B
B
kB
B
kB
�B
�B
	B
�B
�B
)B
)B
�B
�B
�B
�B
B
�B
�B
�B
B
�B
�B
dB
�B
5B
�B
CB
�B
�B
�B
�B
	B
#B
=B
)B
xB
)B
�B
CB
�B
~B
�B
�B
�B
�B
�B
!B
pB
B
�B
�B
;B
 'B
 \B
!B
 �B
!-B
!HB
 �B
!|B
!HB
!�B
"B
"�B
"�B
# B
"�B
#�B
#TB
#TB
# B
#�B
#�B
$B
#�B
$@B
%�B
$�B
%,B
%FB
&B
%�B
%,B
&fB
&B
%�B
%�B
'8B
&�B
'8B
(XB
)�B
*0B
*KB
+kB
+B
+kB
+�B
+�B
+�B
+�B
+�B
,B
-B
-�B
-�B
.�B
.�B
/�B
/�B
/�B
0�B
1AB
1�B
1�B
2-B
2�B
2�B
3�B
4B
4�B
4�B
5ZB
5?B
5�B
5�B
7B
7LB
7�B
7�B
7�B
7�B
7�B
7�B
7�B
7�B
88B
8RB
8�B
9	B
9XB
8�B
9XB
9>B
9rB
9$B
9�B
9�B
9�B
:DB
;B
;B
;0B
;B
;B
;�B
;�B
<�B
<�B
<jB
=VB
=�B
=�B
>B
>(B
=�B
>B
=�B
>(B
=�B
=qB
>wB
>BB
>�B
?B
@4B
?�B
@B
@iB
@�B
@�B
@�B
AUB
AUB
AoB
A�B
A�B
A�B
B�B
B�B
CB
B�B
CB
C-B
C-B
C�B
C�B
D�B
C�B
ESB
ESB
EB
EmB
E�B
E�B
F%B
F?B
FYB
F�B
F�B
F�B
G�B
HfB
G�B
IRB
IRB
IB
HfB
H�B
H�B
H�B
H�B
H�B
H�B
H�B
HfB
H�B
I�B
I�B
I�B
I�B
I�B
I�B
J	B
I�B
J#B
J#B
J	B
J�B
J#B
J#B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
MB
M�B
NpB
NVB
NpB
N�B
O�B
P.B
P}B
P�B
QNB
Q4B
Q�B
QhB
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
RTB
R:B
SB
R�B
R�B
S[B
R�B
S[B
TB
S�B
S&B
S�B
TB
T�B
T�B
T�B
T�B
T�B
T�B
UMB
U2B
U2B
UMB
U�B
U�B
U�B
U�B
U�B
VB
U�B
VmB
VSB
U�B
U�B
UB
U�B
UMB
U�B
UgB
U�B
T�B
T�B
T�B
U2B
UgB
U�B
VB
VSB
W�B
W$B
V�B
W?B
W�B
WYB
W�B
W�B
X+B
Y1B
X�B
Y�B
YB
Y�B
Y�B
[	B
Z�B
Z�B
\)B
[�B
[�B
[�B
[�B
\�B
\�B
]�B
]�B
]�B
^OB
^�B
^B
^jB
^�B
`'B
`�B
`�B
a|B
abB
a-B
abB
a|B
c B
b�B
cTB
c B
c:B
c�B
dZB
dtB
dtB
e`B
fB
e�B
gB
g8B
f�B
f�B
g8B
g8B
g�B
g�B
gmB
hXB
h
B
g�B
h$B
hsB
h$B
g�B
g�B
h>B
h�B
h>B
i_B
j0B
kB
kB
j�B
kB
k�B
kB
j�B
k�B
k�B
lqB
l=B
mB
lqB
l�B
m)B
mB
m�B
mwB
m�B
m�B
m�B
m]B
m�B
m�B
nIB
n�B
n�B
o�B
o�B
p;B
pUB
p;B
p!B
pUB
pUB
qB
p�B
qB
q[B
p�B
q�B
p�B
p�B
qB
qB
qB
q[B
q'B
qvB
qAB
q[B
qAB
q�B
q�B
q�B
r�B
rB
rGB
sB
s�B
tnB
t�B
t�B
t9B
t9B
t9B
uB
t�B
t�B
u%B
u�B
uB
u�B
u?B
uZB
u�B
utB
utB
u�B
utB
uZB
u�B
u%B
utB
u�B
u�B
v�B
vzB
v�B
wfB
w�B
w�B
w�B
w�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104915  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604173619  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604173620  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604173620                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605023627  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605023627  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131507                      G�O�G�O�G�O�                