CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:15:48Z creation;2022-06-04T19:15:48Z conversion to V3.1      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220604191548  20220610151508  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               A   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @��L�3331   @��MA��@.�(�\�d ��
=q1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@y��@���A   A   A@  A`  A�  A�33A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB133B7��B@ffBH  BPffBW33B`  Bj  Bo��Bx  B�ffB�  B���B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C 33C  C�fC  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.�C0�C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 @�]@r�]@�G�@�z�A=qA>=qA^=qA~=qA�Q�A��A��A��A��A��A��A��B�\B�\B�\B�\B'��B0B7(�B?��BG�\BO��BVB_�\Bi�\Bo(�Bw�\B�.B�ǮB��{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB��{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC 
C��C�=C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-�qC/�qC1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs�=Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D�Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��H11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A��A��fA���A���A���A���A��xA��8A���A���A��xA���A���A� iA��A��A��A�SA��A��A� iA��A��DA��A�ݘAݶ�A���A�m�AѵtA��(A�,A��Aʞ�Aû�A�A��aA���A�w�A�U�A���A���A�ΥA�A�7A�t�A�B�A��A���A�;dA��iA�+A���A��.A�M�A��A�TaA�.}A�kA�tA��FA�`A�y�A���A�H�A���A���A���A�i�A�c�A�ZQA�1�A��A�GA���A��A�X�A�_pA��A���A�D�A��A�� A���A�_;A�H�A�R�A�ncA�?HA�GzA~�.A{��A{xAx��As�\Ao�AlAh!�Ac�A]�A\bAZ��AX7LAV7�AU!�AT��AT�}AP��AL��AK��AI�XAH:�AD��ADAC��AB5�A@'�A>�A<�A:VA9cA7��A6CA35�A2bA1/�A0FA/��A.��A,�XA+�A)YKA(��A&҉A%_pA$��A"ѷA"�A ��A;�Aj�AM�A�fA�	Ap�Ar�A�A�zAI�AC-A��A��A>�A5�A��A;�A�A��A�Ag�A)�AѷAv�Ai�A��A��A��A�1A�mA��AH�Aw�A{JA�A��A�A�1Ay>A)�A͟A�A
p;A
&A	� A
�A
  A	�UA	4A��AxlAx�A��Ai�AVAPHA;ASAAG�A��A/�A!�A��Ay�A�A�A��A��A:�A9�A�A ��A ��A ��A J�A q@��H@�b�@���@���@�/�@���@�#�@�@�C@���@��M@���@��w@���@��S@��@���@�Q@��@��@�p;@�	@��@�o@�C�@��.@�@��@���@��@�W�@��@픯@��@�$�@��@�1�@��@�=@�($@�G@�@��@�PH@��@�ݘ@�rG@��@��v@�9X@��@��Q@�B�@�c�@��@�w2@�Mj@���@�y>@��&@��)@�kQ@�9X@�@�x@݈f@��@���@��@�  @�Y�@��]@ښ�@��@ٕ�@�1�@ظR@؉�@�-@�1�@ֵ�@֚�@֌@�|�@�]d@�6�@�4@�ݘ@�Vm@��`@ԂA@�K^@ӟV@���@Ҥ�@�)�@ѱ[@эP@�|�@�f�@�.I@��@Ϯ�@�P�@���@�u%@�O@͹�@�dZ@��@̿�@�m�@˹�@�Mj@�'�@ʲ�@�$�@Ɏ"@���@�Ov@�~�@��@���@ƌ@�d�@��m@�l�@�o@�YK@ò-@��@�@�@�C-@��@��'@�(@��e@�r�@��@�S@���@��R@��x@�R�@�  @���@�>�@��+@��@���@���@��@��y@���@�� @�C-@�ݘ@��@�Y�@�+�@��@�Ĝ@���@�c�@�b@��@��	@�f�@�G�@���@���@�|�@�?@���@�x@��@��v@�u%@�=q@��@��6@�Q�@��s@��@�c�@��@�n�@��]@��	@�:�@��@��m@�:*@���@��P@�/�@�@��@�� @�c�@��r@��@�G�@��@�҉@�~�@�(�@��'@�W?@��8@���@��@���@�zx@�P�@��c@���@���@�9X@���@��
@���@�<6@��@��@�|�@�>B@�G@��&@�@��7@�Dg@��@��x@�$@��m@�@��X@�y�@�Y@��@�k�@���@���@�1'@��]@��d@��=@�P�@��"@���@�g8@���@���@���@�5�@��4@�8�@��A@���@�_p@��"@���@�v�@�!�@��@��f@�iD@�"�@��,@���@�A�@�4@��@�x@�>�@�q@��@�ں@��h@���@�H@�	@��@��q@�]�@�*0@���@��@�1'@���@���@�{J@�?}@�"�@��]@�U2@���@��@�B�@��@���@�y>@��@��q@�Y�@�"�@��@���@��@��z@���@���@�u%@�$�@���@��:@�j�@�@O@��@��v@���@�L0@��@��9@��@��H@���@���@�{J@�g�@��@���@��D@�oi@�N�@�?@�)�@�	@��D@�خ@��-@�f�@��@�;�@��a@�a�@�{J@��f@��B@��@���@��M@�p�@�Y�@�<6@�Ĝ@���@�M@�%�@�3�@�:*@�9X@���@��$@�F�@��@��I@�L0@��@e�@"�@�@~�!@~$�@}�@}j@}%@|��@|��@|V�@{��@{o@z�+@zYK@z)�@y��@yc@ya�@yA @y;@x��@x,=@wF�@v��@v+k@v �@u�@u��@u=�@t�j@tA�@s�]@s�&@s�a@s��@sW?@r��@rO@q��@qe,@q*0@pĜ@p1@n�B@n��@n�+@nq�@nYK@n5?@n_@m��@m��@mw2@m7L@l��@l_@k�@kMj@j��@j�6@j��@jQ@j#:@i�Z@im]@h��@h�@h�o@h(�@g��@g�V@g>�@f�@f?@f_@e��@e�"@eQ�@e7L@e�@d�@d�@c� @b�@bp;@b&�@a��@a/@a%@`�|@`��@`Ĝ@`�D@`7�@_��@_�@^^5@]�o@]�-@]^�@]+@\�E@\�@[�0@[Mj@[,�@Z�B@ZE�@Z	@Y�N@Y�t@Ym]@Xz�@W�]@Wƨ@W�:@W6z@V��@Vq�@V_@U�@UN<@T��@T��@T�@S��@St�@R�@R��@RW�@Q�T@Q�@Q�@P�O@Pb@O�}@O�	@OX�@N��@N:*@M��@M�@L�z@L4n@K��@K!-@J�@J�\@I�Z@I}�@IrG@I\�@I+�@H�[@H|�@Gݘ@G;d@F��@F��@FJ�@E��@E�C@Ehs@EV@D�4@C��@CH�@B�,@B��@Bff@B4@A��@@�@@�/@?��@?�{@>�M@>��@>}V@>	@>J@=�.@=��@=��@=��@=J�@<�_@;�@;��@:�2@:s�@:E�@:?@: �@9�#@9�t@9Vm@8��@8��@8�o@8Ft@8-�@8M@7�@7t�@7@O@6��@6n�@6YK@61�@5�@5\�@4��@4l"@44n@3�6@3~�@3\)@34�@2�2@2��@2kQ@2C�@2
�@1�N@1��@1��@1�@1G�@1(�@0�/@0y>@04n@01@/�m@/��@/��@/\)@/)_@/Y@/@/�@.ں@.�r@.L0@.B[@.#:@-�)@-@-}�@-4@- \@-;@,�e@,*�@,�@+��@+�q@+�4@+6z@*�2@*@�@)�)@)��@)��@)?}@)@)+@)�@(��@(�e@(r�@(<�@'��@'33@'C@'�@&��@&~�@&Q@&.�@%�o@%�@%c�@%F@%�@$�`@$�@$%�@$G@#��@#�@@#j�@#>�@#Y@"�@"��@"�6@"�x@"�r@"�@!�3@!�@!%F@ ��@ �v@ ی@ �[@ ��@ �o@ 4n@�@��@~�@"�@��@�2@��@�\@n�@B[@�@��@�@�7@c@J�@#�@�K@'R@�6@ƨ@�@S�@�@�}@J�@!�@	@�j@��@ϫ@�d@��@@�'@N<@��@��@�I@U2@!@  @�@��@v`@+@��@h
@�@��@�Z@��@x�@?}@�@	l@��@��@�@�u@y>@/�@  @��@��@g�@33@�@��@��@�\@ff@
�@�)@�@�^@L�@2a@�@�e@S�@*�@�@�F@�@S�@�y@�@��@�6@��@u%@:*@ �@�z@�~@f�@+@�@��@h�@6@@��@��@��@]�@Mj11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 A���A��A��fA���A���A���A���A��xA��8A���A���A��xA���A���A� iA��A��A��A�SA��A��A� iA��A��DA��A�ݘAݶ�A���A�m�AѵtA��(A�,A��Aʞ�Aû�A�A��aA���A�w�A�U�A���A���A�ΥA�A�7A�t�A�B�A��A���A�;dA��iA�+A���A��.A�M�A��A�TaA�.}A�kA�tA��FA�`A�y�A���A�H�A���A���A���A�i�A�c�A�ZQA�1�A��A�GA���A��A�X�A�_pA��A���A�D�A��A�� A���A�_;A�H�A�R�A�ncA�?HA�GzA~�.A{��A{xAx��As�\Ao�AlAh!�Ac�A]�A\bAZ��AX7LAV7�AU!�AT��AT�}AP��AL��AK��AI�XAH:�AD��ADAC��AB5�A@'�A>�A<�A:VA9cA7��A6CA35�A2bA1/�A0FA/��A.��A,�XA+�A)YKA(��A&҉A%_pA$��A"ѷA"�A ��A;�Aj�AM�A�fA�	Ap�Ar�A�A�zAI�AC-A��A��A>�A5�A��A;�A�A��A�Ag�A)�AѷAv�Ai�A��A��A��A�1A�mA��AH�Aw�A{JA�A��A�A�1Ay>A)�A͟A�A
p;A
&A	� A
�A
  A	�UA	4A��AxlAx�A��Ai�AVAPHA;ASAAG�A��A/�A!�A��Ay�A�A�A��A��A:�A9�A�A ��A ��A ��A J�A q@��H@�b�@���@���@�/�@���@�#�@�@�C@���@��M@���@��w@���@��S@��@���@�Q@��@��@�p;@�	@��@�o@�C�@��.@�@��@���@��@�W�@��@픯@��@�$�@��@�1�@��@�=@�($@�G@�@��@�PH@��@�ݘ@�rG@��@��v@�9X@��@��Q@�B�@�c�@��@�w2@�Mj@���@�y>@��&@��)@�kQ@�9X@�@�x@݈f@��@���@��@�  @�Y�@��]@ښ�@��@ٕ�@�1�@ظR@؉�@�-@�1�@ֵ�@֚�@֌@�|�@�]d@�6�@�4@�ݘ@�Vm@��`@ԂA@�K^@ӟV@���@Ҥ�@�)�@ѱ[@эP@�|�@�f�@�.I@��@Ϯ�@�P�@���@�u%@�O@͹�@�dZ@��@̿�@�m�@˹�@�Mj@�'�@ʲ�@�$�@Ɏ"@���@�Ov@�~�@��@���@ƌ@�d�@��m@�l�@�o@�YK@ò-@��@�@�@�C-@��@��'@�(@��e@�r�@��@�S@���@��R@��x@�R�@�  @���@�>�@��+@��@���@���@��@��y@���@�� @�C-@�ݘ@��@�Y�@�+�@��@�Ĝ@���@�c�@�b@��@��	@�f�@�G�@���@���@�|�@�?@���@�x@��@��v@�u%@�=q@��@��6@�Q�@��s@��@�c�@��@�n�@��]@��	@�:�@��@��m@�:*@���@��P@�/�@�@��@�� @�c�@��r@��@�G�@��@�҉@�~�@�(�@��'@�W?@��8@���@��@���@�zx@�P�@��c@���@���@�9X@���@��
@���@�<6@��@��@�|�@�>B@�G@��&@�@��7@�Dg@��@��x@�$@��m@�@��X@�y�@�Y@��@�k�@���@���@�1'@��]@��d@��=@�P�@��"@���@�g8@���@���@���@�5�@��4@�8�@��A@���@�_p@��"@���@�v�@�!�@��@��f@�iD@�"�@��,@���@�A�@�4@��@�x@�>�@�q@��@�ں@��h@���@�H@�	@��@��q@�]�@�*0@���@��@�1'@���@���@�{J@�?}@�"�@��]@�U2@���@��@�B�@��@���@�y>@��@��q@�Y�@�"�@��@���@��@��z@���@���@�u%@�$�@���@��:@�j�@�@O@��@��v@���@�L0@��@��9@��@��H@���@���@�{J@�g�@��@���@��D@�oi@�N�@�?@�)�@�	@��D@�خ@��-@�f�@��@�;�@��a@�a�@�{J@��f@��B@��@���@��M@�p�@�Y�@�<6@�Ĝ@���@�M@�%�@�3�@�:*@�9X@���@��$@�F�@��@��I@�L0@��@e�@"�@�@~�!@~$�@}�@}j@}%@|��@|��@|V�@{��@{o@z�+@zYK@z)�@y��@yc@ya�@yA @y;@x��@x,=@wF�@v��@v+k@v �@u�@u��@u=�@t�j@tA�@s�]@s�&@s�a@s��@sW?@r��@rO@q��@qe,@q*0@pĜ@p1@n�B@n��@n�+@nq�@nYK@n5?@n_@m��@m��@mw2@m7L@l��@l_@k�@kMj@j��@j�6@j��@jQ@j#:@i�Z@im]@h��@h�@h�o@h(�@g��@g�V@g>�@f�@f?@f_@e��@e�"@eQ�@e7L@e�@d�@d�@c� @b�@bp;@b&�@a��@a/@a%@`�|@`��@`Ĝ@`�D@`7�@_��@_�@^^5@]�o@]�-@]^�@]+@\�E@\�@[�0@[Mj@[,�@Z�B@ZE�@Z	@Y�N@Y�t@Ym]@Xz�@W�]@Wƨ@W�:@W6z@V��@Vq�@V_@U�@UN<@T��@T��@T�@S��@St�@R�@R��@RW�@Q�T@Q�@Q�@P�O@Pb@O�}@O�	@OX�@N��@N:*@M��@M�@L�z@L4n@K��@K!-@J�@J�\@I�Z@I}�@IrG@I\�@I+�@H�[@H|�@Gݘ@G;d@F��@F��@FJ�@E��@E�C@Ehs@EV@D�4@C��@CH�@B�,@B��@Bff@B4@A��@@�@@�/@?��@?�{@>�M@>��@>}V@>	@>J@=�.@=��@=��@=��@=J�@<�_@;�@;��@:�2@:s�@:E�@:?@: �@9�#@9�t@9Vm@8��@8��@8�o@8Ft@8-�@8M@7�@7t�@7@O@6��@6n�@6YK@61�@5�@5\�@4��@4l"@44n@3�6@3~�@3\)@34�@2�2@2��@2kQ@2C�@2
�@1�N@1��@1��@1�@1G�@1(�@0�/@0y>@04n@01@/�m@/��@/��@/\)@/)_@/Y@/@/�@.ں@.�r@.L0@.B[@.#:@-�)@-@-}�@-4@- \@-;@,�e@,*�@,�@+��@+�q@+�4@+6z@*�2@*@�@)�)@)��@)��@)?}@)@)+@)�@(��@(�e@(r�@(<�@'��@'33@'C@'�@&��@&~�@&Q@&.�@%�o@%�@%c�@%F@%�@$�`@$�@$%�@$G@#��@#�@@#j�@#>�@#Y@"�@"��@"�6@"�x@"�r@"�@!�3@!�@!%F@ ��@ �v@ ی@ �[@ ��@ �o@ 4n@�@��@~�@"�@��@�2@��@�\@n�@B[@�@��@�@�7@c@J�@#�@�K@'R@�6@ƨ@�@S�@�@�}@J�@!�@	@�j@��@ϫ@�d@��@@�'@N<@��@��@�I@U2@!@  @�@��@v`@+@��@h
@�@��@�Z@��@x�@?}@�@	l@��@��@�@�u@y>@/�@  @��@��@g�@33@�@��@��@�\@ff@
�@�)@�@�^@L�@2a@�@�e@S�@*�@�@�F@�@S�@�y@�@��@�6@��@u%@:*@ �@�z@�~@f�@+@�@��@h�@6@@��@��@��@]�@Mj11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�lB	ȀB	ȴB	ȚB	ȴB	ȚB	ȴB	ȚB	ȚB	�fB	ȀB	ȀB	ȀB	ȀB	ȚB	ȀB	ȚB	��B	̘B	͹B	�B	�TB	��B	�
B	چB
 OB
)*B
��BgB B,"B��B�~B�zB�B�BB��B�kBB�B��B�B��B��B��B  B��B�B�@B�VBیB�9B�jB��BĶBðB�{B�'BĜB��B�^B�LB��B��B�DB��B�XB�B��B�_B�4Bb�B?�B1�BDgB8�B($BB
��B
�<B
�YB
��B
��B
��B
��B
x8B
ffB
R:B
DB
9�B
/5B
+B
kB	�!B	��B	��B	�jB	kB	;JB	6�B	0�B	#�B	�B	.B	pB	KB��B�FB��BߤBٚB�bB�~B�B�fB�aB��B��B��B��B�AB��B��B�[B��B��B��B��B��B�B�zB��B�RB��B��B�LB��B�|B�qB�xB��B�JB�BҽB��B�qB�B�	B� B��B��B��B��B��B	B	=B	5?B	5tB	CB	�PB	�B	��B	�&B	��B	��B	�iB	�aB	��B	��B	��B	�B	�B	�*B	�WB	�wB	��B	��B	�IB	�B	��B	�B	��B	��B	��B	ȴB	̈́B	ɠB	�dB	�jB	��B	�B	�B	��B	��B	��B	�B	��B	�9B	ѝB	�&B	��B	�pB	�B	҉B	��B	��B	�	B	��B	ބB	�;B	ߤB	ߤB	߾B	�B	�B	�HB	�B	�B	��B	�B	��B	�QB	��B	�B	�B	�=B	��B	�kB	�_B	�B	��B	�yB	�B	�/B	�B	�B	�cB	�IB	��B	�"B	�B	��B	�B	�RB	�FB	�`B	�sB	�B	��B	�LB	�B	�B	��B	� B	��B	�B	��B	�B	��B	�nB	�B	�tB	�B	�,B	�FB	�&B	��B	�ZB	�&B	�B	�B	��B	�B	�
B	��B	��B	��B	�eB	��B	�)B	�B	��B	�WB	�B	�B	�WB	�kB	�kB	��B	�B	�=B	�B	��B	�B	�/B	��B	��B	��B	��B	�/B	�B	��B	�5B	�B	��B	�B	��B	�B	�hB	��B	�B	�MB	�B	�B	�B	�TB	�ZB	�%B	�ZB	�?B	��B	��B	��B	��B	��B	�zB	��B	��B	�B	��B	�B	��B	�$B	�rB	�XB	�$B	�$B	��B	�	B	��B	��B	��B	�lB	�lB	�RB	�B	�rB	��B	�B	�"B	�B	�6B	�JB	��B	��B	��B	�>B	��B	��B	��B	��B	�0B	��B	�^B	��B	��B	��B	�JB	�dB	��B	�B	�PB	��B	�.B	�HB	��B
 B
 4B
 OB
 �B
UB
�B
�B
B
AB
�B
-B
�B
GB
�B
�B
�B
�B
�B
�B
�B
MB
B
�B
�B
�B
�B
�B
;B
 �B
 �B
 �B
 �B
 B
;B
 �B
 �B
�B
�B
�B
�B
�B
�B
B
3B
�B
�B
�B
�B
�B
{B
aB
�B
9B
9B
mB
tB
�B
B
zB
B
�B
�B
	B
	RB
	�B
	�B

rB
0B
�B
�B
B
PB
PB
B
"B
VB
pB
�B
\B
B
�B

=B
	B
	B
	RB
	7B
	RB
	RB
	lB
	�B
	�B

	B
DB
xB
)B
xB
�B
�B
�B
B
6B
�B
�B
�B
�B
(B
�B
�B
.B
�B
HB
�B
�B
 B
�B
B
 B
:B
oB
�B
�B
@B
@B
[B
[B
�B
�B
aB
�B
B
gB
MB
�B
SB
SB
�B

B
sB
�B
_B
�B
1B
�B
B
7B
QB
7B
QB
7B
QB
kB
kB
�B
kB
7B
kB
�B
�B
�B
�B
5B
OB
�B
VB
;B
VB
;B
VB
�B
�B
�B
 �B
!B
!-B
!-B
!bB
!HB
!HB
!|B
!�B
!�B
!bB
!�B
!�B
�B
B
OB
 BB
!�B
!B
;B
�B
 B
!�B
#�B
$�B
$�B
$ZB
$�B
%�B
'B
'RB
'�B
)yB
)�B
)�B
)�B
)�B
*B
+B
+�B
+�B
+�B
,�B
-)B
.IB
.�B
.�B
/OB
/�B
/�B
/�B
0UB
0UB
0�B
0�B
1�B
1�B
1�B
1�B
2-B
2|B
2�B
3B
3B
2�B
33B
3MB
3�B
3�B
4TB
4�B
5�B
5�B
5�B
5�B
5�B
5�B
6+B
5�B
6B
6zB
6�B
7LB
8RB
8lB
8lB
8lB
8lB
8�B
8�B
8�B
8�B
9	B
9$B
9>B
9>B
9�B
:DB
:*B
:B
:*B
:�B
:�B
:�B
:^B
:xB
:�B
;�B
<6B
<jB
<�B
<�B
<�B
<�B
<�B
=B
="B
=�B
>]B
>BB
>BB
>BB
>wB
?�B
?�B
@ B
@iB
@�B
@�B
@�B
@�B
@�B
A B
AoB
A�B
BAB
BuB
B�B
BuB
B�B
B�B
B�B
B�B
C�B
C�B
C�B
DMB
D�B
D�B
EB
D�B
EB
FB
F%B
F?B
F?B
F�B
F�B
G+B
G�B
H1B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J	B
J#B
JrB
J�B
J�B
K)B
KxB
K�B
K�B
K�B
L0B
L0B
L�B
M6B
M�B
M�B
M�B
N"B
N"B
N<B
N�B
OB
OB
OB
OB
OBB
OBB
O�B
O�B
PB
PbB
P�B
P�B
QB
QNB
Q�B
Q�B
R B
RoB
R�B
R�B
R�B
R�B
S@B
S�B
SuB
T�B
T�B
U2B
UMB
UMB
U�B
U�B
U�B
U�B
U�B
U�B
VB
V�B
W$B
WYB
W$B
WYB
WsB
XB
XEB
X�B
X�B
X�B
Y1B
YeB
YeB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
Z�B
[	B
[WB
[�B
[�B
\CB
\]B
\xB
\xB
\�B
]B
]B
]/B
]~B
]�B
]�B
]�B
]�B
^B
^B
^5B
^�B
^�B
_!B
_VB
_VB
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`BB
`vB
`\B
`vB
`�B
`�B
`�B
a-B
a-B
a-B
a|B
a�B
a�B
a�B
bB
b4B
bhB
b�B
c:B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d@B
d@B
dZB
dtB
eFB
e`B
e�B
e`B
e�B
e�B
fB
f2B
ffB
f�B
f�B
f�B
gB
gB
gmB
g�B
g�B
g�B
h>B
h�B
h�B
h�B
h�B
h�B
i*B
iB
iB
i�B
i�B
i�B
jKB
jeB
jeB
jB
jeB
jB
jB
j�B
k6B
k6B
kkB
k�B
k�B
k�B
k�B
l"B
l=B
l=B
l�B
l�B
l�B
l�B
l�B
m)B
m)B
m]B
nB
n/B
n/B
nB
n}B
n�B
n�B
oOB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p!B
p�B
p�B
p�B
qB
q'B
q[B
q[B
q�B
q�B
q�B
rGB
r|B
r�B
r�B
r�B
s3B
shB
s�B
s�B
s�B
tB
tB
tB
tB
t9B
t�B
t�B
t�B
t�B
uB
u?B
utB
u�B
u�B
u�B
u�B
vFB
vzB
vzB
v�B
wB
wB
wLB
w�B
w�B
xB
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
zDB
zxB
z�B
z�B
z�B
{JB
{dB
{�B
{�B
|6B
|PB
|PB
|�B
|�B
|�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 B	�lB	ȀB	ȴB	ȚB	ȴB	ȚB	ȴB	ȚB	ȚB	�fB	ȀB	ȀB	ȀB	ȀB	ȚB	ȀB	ȚB	��B	̘B	͹B	�B	�TB	��B	�
B	چB
 OB
)*B
��BgB B,"B��B�~B�zB�B�BB��B�kBB�B��B�B��B��B��B  B��B�B�@B�VBیB�9B�jB��BĶBðB�{B�'BĜB��B�^B�LB��B��B�DB��B�XB�B��B�_B�4Bb�B?�B1�BDgB8�B($BB
��B
�<B
�YB
��B
��B
��B
��B
x8B
ffB
R:B
DB
9�B
/5B
+B
kB	�!B	��B	��B	�jB	kB	;JB	6�B	0�B	#�B	�B	.B	pB	KB��B�FB��BߤBٚB�bB�~B�B�fB�aB��B��B��B��B�AB��B��B�[B��B��B��B��B��B�B�zB��B�RB��B��B�LB��B�|B�qB�xB��B�JB�BҽB��B�qB�B�	B� B��B��B��B��B��B	B	=B	5?B	5tB	CB	�PB	�B	��B	�&B	��B	��B	�iB	�aB	��B	��B	��B	�B	�B	�*B	�WB	�wB	��B	��B	�IB	�B	��B	�B	��B	��B	��B	ȴB	̈́B	ɠB	�dB	�jB	��B	�B	�B	��B	��B	��B	�B	��B	�9B	ѝB	�&B	��B	�pB	�B	҉B	��B	��B	�	B	��B	ބB	�;B	ߤB	ߤB	߾B	�B	�B	�HB	�B	�B	��B	�B	��B	�QB	��B	�B	�B	�=B	��B	�kB	�_B	�B	��B	�yB	�B	�/B	�B	�B	�cB	�IB	��B	�"B	�B	��B	�B	�RB	�FB	�`B	�sB	�B	��B	�LB	�B	�B	��B	� B	��B	�B	��B	�B	��B	�nB	�B	�tB	�B	�,B	�FB	�&B	��B	�ZB	�&B	�B	�B	��B	�B	�
B	��B	��B	��B	�eB	��B	�)B	�B	��B	�WB	�B	�B	�WB	�kB	�kB	��B	�B	�=B	�B	��B	�B	�/B	��B	��B	��B	��B	�/B	�B	��B	�5B	�B	��B	�B	��B	�B	�hB	��B	�B	�MB	�B	�B	�B	�TB	�ZB	�%B	�ZB	�?B	��B	��B	��B	��B	��B	�zB	��B	��B	�B	��B	�B	��B	�$B	�rB	�XB	�$B	�$B	��B	�	B	��B	��B	��B	�lB	�lB	�RB	�B	�rB	��B	�B	�"B	�B	�6B	�JB	��B	��B	��B	�>B	��B	��B	��B	��B	�0B	��B	�^B	��B	��B	��B	�JB	�dB	��B	�B	�PB	��B	�.B	�HB	��B
 B
 4B
 OB
 �B
UB
�B
�B
B
AB
�B
-B
�B
GB
�B
�B
�B
�B
�B
�B
�B
MB
B
�B
�B
�B
�B
�B
;B
 �B
 �B
 �B
 �B
 B
;B
 �B
 �B
�B
�B
�B
�B
�B
�B
B
3B
�B
�B
�B
�B
�B
{B
aB
�B
9B
9B
mB
tB
�B
B
zB
B
�B
�B
	B
	RB
	�B
	�B

rB
0B
�B
�B
B
PB
PB
B
"B
VB
pB
�B
\B
B
�B

=B
	B
	B
	RB
	7B
	RB
	RB
	lB
	�B
	�B

	B
DB
xB
)B
xB
�B
�B
�B
B
6B
�B
�B
�B
�B
(B
�B
�B
.B
�B
HB
�B
�B
 B
�B
B
 B
:B
oB
�B
�B
@B
@B
[B
[B
�B
�B
aB
�B
B
gB
MB
�B
SB
SB
�B

B
sB
�B
_B
�B
1B
�B
B
7B
QB
7B
QB
7B
QB
kB
kB
�B
kB
7B
kB
�B
�B
�B
�B
5B
OB
�B
VB
;B
VB
;B
VB
�B
�B
�B
 �B
!B
!-B
!-B
!bB
!HB
!HB
!|B
!�B
!�B
!bB
!�B
!�B
�B
B
OB
 BB
!�B
!B
;B
�B
 B
!�B
#�B
$�B
$�B
$ZB
$�B
%�B
'B
'RB
'�B
)yB
)�B
)�B
)�B
)�B
*B
+B
+�B
+�B
+�B
,�B
-)B
.IB
.�B
.�B
/OB
/�B
/�B
/�B
0UB
0UB
0�B
0�B
1�B
1�B
1�B
1�B
2-B
2|B
2�B
3B
3B
2�B
33B
3MB
3�B
3�B
4TB
4�B
5�B
5�B
5�B
5�B
5�B
5�B
6+B
5�B
6B
6zB
6�B
7LB
8RB
8lB
8lB
8lB
8lB
8�B
8�B
8�B
8�B
9	B
9$B
9>B
9>B
9�B
:DB
:*B
:B
:*B
:�B
:�B
:�B
:^B
:xB
:�B
;�B
<6B
<jB
<�B
<�B
<�B
<�B
<�B
=B
="B
=�B
>]B
>BB
>BB
>BB
>wB
?�B
?�B
@ B
@iB
@�B
@�B
@�B
@�B
@�B
A B
AoB
A�B
BAB
BuB
B�B
BuB
B�B
B�B
B�B
B�B
C�B
C�B
C�B
DMB
D�B
D�B
EB
D�B
EB
FB
F%B
F?B
F?B
F�B
F�B
G+B
G�B
H1B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J	B
J#B
JrB
J�B
J�B
K)B
KxB
K�B
K�B
K�B
L0B
L0B
L�B
M6B
M�B
M�B
M�B
N"B
N"B
N<B
N�B
OB
OB
OB
OB
OBB
OBB
O�B
O�B
PB
PbB
P�B
P�B
QB
QNB
Q�B
Q�B
R B
RoB
R�B
R�B
R�B
R�B
S@B
S�B
SuB
T�B
T�B
U2B
UMB
UMB
U�B
U�B
U�B
U�B
U�B
U�B
VB
V�B
W$B
WYB
W$B
WYB
WsB
XB
XEB
X�B
X�B
X�B
Y1B
YeB
YeB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
Z�B
[	B
[WB
[�B
[�B
\CB
\]B
\xB
\xB
\�B
]B
]B
]/B
]~B
]�B
]�B
]�B
]�B
^B
^B
^5B
^�B
^�B
_!B
_VB
_VB
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`BB
`vB
`\B
`vB
`�B
`�B
`�B
a-B
a-B
a-B
a|B
a�B
a�B
a�B
bB
b4B
bhB
b�B
c:B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d@B
d@B
dZB
dtB
eFB
e`B
e�B
e`B
e�B
e�B
fB
f2B
ffB
f�B
f�B
f�B
gB
gB
gmB
g�B
g�B
g�B
h>B
h�B
h�B
h�B
h�B
h�B
i*B
iB
iB
i�B
i�B
i�B
jKB
jeB
jeB
jB
jeB
jB
jB
j�B
k6B
k6B
kkB
k�B
k�B
k�B
k�B
l"B
l=B
l=B
l�B
l�B
l�B
l�B
l�B
m)B
m)B
m]B
nB
n/B
n/B
nB
n}B
n�B
n�B
oOB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p!B
p�B
p�B
p�B
qB
q'B
q[B
q[B
q�B
q�B
q�B
rGB
r|B
r�B
r�B
r�B
s3B
shB
s�B
s�B
s�B
tB
tB
tB
tB
t9B
t�B
t�B
t�B
t�B
uB
u?B
utB
u�B
u�B
u�B
u�B
vFB
vzB
vzB
v�B
wB
wB
wLB
w�B
w�B
xB
x�B
x�B
x�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
zDB
zxB
z�B
z�B
z�B
{JB
{dB
{�B
{�B
|6B
|PB
|PB
|�B
|�B
|�B
|�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105232  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604191548  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604191548  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604191548                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605041555  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605041555  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610151508                      G�O�G�O�G�O�                