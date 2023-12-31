CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T19:31:47Z creation;2022-06-04T19:31:48Z conversion to V3.1      
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
_FillValue                 �  I8   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M(   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tx   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �    PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ޘ   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �    HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �D   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �T   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �X   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �h   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �l   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220604193147  20220610161506  5905855                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               uA   JA                                  2B  A   APEX                            8423                            2.11.2                          846 @�˅O��P1   @�ˆf���@-&�x����c���vȴ1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B���B���B���B�  B�33B�  B�  B�  B�  B���B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33BB���B���B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*�C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd�Cf�Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DUfDU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds�fDt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D�|�D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�y�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�]@\)@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�aHB��{B��{B�ǮB���B�ǮB�ǮB�ǮB�ǮB��{B���BÔ{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB���B�aHB�{B��{B�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)�qC+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc�qCe�qCg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT�\DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Ds\Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�yHDܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�v1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�2aA�*�A�'�A��A��?Aϱ�A�t�A�6FA�IA��A�~A�A�AA�;A���A���A���A���A��+A���A��A��"A��A��KA���A���A��A��A���A���A���A��A�� A��/A��ZA��AΦAΗ�A�VmA�d�A�7�A��A�9$A���A��A���A�L�A�רA���A�YKA�:�A�ںA�iyA��TA��DA�;dA���A��A�A�A�%�A���A���A���A��A�D�A��iA���A��)A��A��A��A�j�A�`�A�,qA���A�m�A� \A��(A�<A���A�~�A���A�t�A�RTA}ںA|  Az�Ax^�Au�!As=Ap~�Ak��Ah� Ag��Af�mAd�9Ab� A`+A[�AX�AUa�AR�OAMs�AJ��AI��AH�EAG'RAD�AC�7AB�'AA�PA?��A=��A;J�A9;A7jA6jA3�>A2��A2[�A1�hA0��A0�+A0��A0qA/W?A. �A,�A+ȴA*�}A*�zA*:�A)FA(p�A&�A%-A%RTA#0UA ��AB[A'RA�`A{JA+A�mAAx�A1A��A�hA�.AW?A��A� A
�An�A;dA��A��AQ�A�A��A��A��A��Ar�A�A.�A�A1A��AB[A�A
=A��Av`A��A�A
�dA
�A	�0A��A\�A�oA&�A�A�OA�^A�MAc A�+A+kA��A��A�A{A�A��A|A�A�TA��AA AA ��A �A @���@��g@��N@�F�@��o@�:*@���@�&@���@���@�?}@� i@�Xy@�T�@���@�S&@�S&@�Vm@�*�@�@�ߤ@�m�@��@�X�@�6@��@�o�@��H@�R@��'@�/�@��@퇔@��[@�m]@�l�@��r@�:�@�%F@��@�8�@�7@�ߤ@蒣@��}@��@�%�@�j�@�l�@�$@�
=@�J@�RT@�0�@�ȴ@�&�@��W@޾@��@ݿH@��@�B[@���@۞�@ۏ�@�S&@ڣ�@�M@ٯ�@�X�@��@�e�@�@��p@ո�@�s�@�+�@ԝI@��d@�H�@���@���@�{�@���@�Y�@��@Ќ�@��D@ϊ�@�!-@���@Χ�@΃@͹�@�4�@���@̄�@�-�@�33@�n�@�2�@�{@ɮ�@�o@Ƞ�@�^5@��9@�G�@ƹ�@�x@��d@�J�@�@��@�l�@�L�@�7L@�q@��,@ªe@�h
@�E�@���@�+�@��@���@�h
@��K@��@���@��P@�ں@�q@���@�C@��@��p@�kQ@�	@���@���@�E9@��,@�z�@���@���@��@���@���@� �@�j�@��s@�I�@���@��@��{@�A�@�ی@�n�@�u@���@�RT@�Dg@�0�@���@���@�Xy@��@�ƨ@�rG@��@��/@��[@�|�@�	@�O�@��@�u%@���@�@��c@��b@�I�@�@��@��@��b@���@�q�@�C�@��d@�hs@�J�@�H�@�IR@�B�@�/�@��@�	l@��?@�c @�ԕ@�U�@��@��e@�^5@�7�@���@��@��@�9�@�_�@�J@���@���@�@@�h
@�%�@��N@�b�@���@�C�@��@��@@�0�@��/@��,@���@�#:@�G@��]@��X@�dZ@�&@��@�@�@��-@�|@�(�@��"@���@�c @�-�@��Z@��X@��7@�u�@�T�@�C�@�@��'@��I@���@�Z�@�(�@��)@��@@�o�@� i@��1@�S�@� �@��9@��~@�hs@�V@�֡@���@���@�_@�
�@��@�p�@��/@���@�2�@��q@���@�-w@���@�}V@��#@���@��S@�e�@�<6@�+@��X@���@�$�@���@���@�rG@�X@�IR@�<6@�Y@��@��@�S�@�M@���@�a�@�-w@���@�͟@��@��o@�-@��&@���@��@��C@��@�K�@�5�@��@���@��U@���@���@�oi@�.�@��
@���@��"@�a�@�)_@��@���@��F@�p;@�b@�u�@�+@��2@���@��@�i�@�I�@�	@�G@��@��a@���@�U�@�/@��@��5@��e@�i�@�R�@�A�@�)�@��z@�zx@�c�@��@���@���@��6@��r@�e�@�Q�@�1�@�x@��@�&@��@o�@J#@~��@}�"@}�@|u�@|�@{˒@{�@zc @z$�@y��@y�M@y	l@x��@xS�@w��@w�@v�@v�X@v.�@u�@uQ�@t��@t��@tU2@s�+@s��@s�{@sW?@sC@r�x@rM�@q�n@qX@p�@pr�@pb@o�k@n��@nq�@n?@m��@m+�@l�u@kخ@ka@j�@j�@i��@i/@h֡@hPH@g�a@g6z@f��@f�@eVm@d�)@d_@c��@cl�@b͟@b�@a��@aQ�@`�@_��@_��@_��@__p@_)_@^��@^p;@]��@]�3@]@]��@]�@\�@\�.@\2�@[�
@[iD@Z�L@Z�@Y��@Yc�@Y�@Xu�@X	�@W�q@W1�@V��@V�@U�@U}�@U�@T�4@Tz�@S��@Sl�@S4�@S$t@SC@R�M@R�x@R�@Q�@Q�C@Q5�@P��@PD�@P<�@P,=@O��@O/�@N��@Nc @N=q@N�@M�)@M��@M��@MQ�@M�@L��@L�@L��@L�@L$@K�P@KA�@K8@K.I@K�@JJ�@I�C@I-w@Hm�@G�@Ga@F��@F\�@F��@Fd�@F)�@E�H@EDg@Dq@D  @C��@CO@Cn/@C�@Bff@A��@Am]@A�@@�@@>B@@@@7@?��@?\)@>�2@>�\@>��@>kQ@>6�@=�)@=�t@=�~@=N<@<�@<��@<D�@<~@;�@;�@@;�P@;�{@;x@;@O@:��@:��@:��@:h
@:8�@:e@:�@9��@9@9j@9%@8֡@8�@8�@8,=@7�@7��@7\)@6�M@6�6@6��@6{�@6�@5��@5T�@4�@4�9@4j@4K^@4*�@4b@3��@3g�@2��@2��@2��@2p;@1��@1��@1Vm@1<6@0��@0oi@0N�@/�@/��@/\)@.�@.�@.p;@.	@-rG@-Dg@-@,�|@,N�@,M@+�;@+��@+C�@+�@*��@*��@*~�@*h
@*.�@*�@)��@)@)�X@)o @)4@(�@(��@(c�@(	�@'�6@'��@'�:@'y�@'O@')_@&��@&�}@&~�@&W�@&.�@&
�@%��@% \@$�	@$�@$�@$��@$w�@$!@#�@#��@#!-@#�@#
=@"��@"�@"d�@"($@!�.@!��@!G�@!�@ �@ �@ ��@ |�@ <�@ M@��@�$@{J@Mj@
=@�h@��@_�@@�@�@�3@hs@L�@(�@V@�@�@��@�o@U2@1'@!@�@� @�q@O@�@�]@�!@a|@!�@��@�H@�7@c�@0�@�@�v@��@�$@�u@V�@,=@1@��@��@��@{J@C�@�@�,@��@�+@kQ@a|@	@�@��@ϫ@��@%F@�@��@�9@�@��@e�@	�@��@�6@��@��@�	@]�@C@͟@��@s�@i�@&�@�@��@�9@��@j@L�@�@�@�@��@�@e�@1@خ@�0@�4@W?@�@�@�X@kQ@1�@@��@ԕ@�3@�@F@�@ی@�.@c�@I�@?�@4n@2�@@��@خ@�*@n/@o�@o�@b�@@O@�@
=@
҉@
��@
�@
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A�2aA�*�A�'�A��A��?Aϱ�A�t�A�6FA�IA��A�~A�A�AA�;A���A���A���A���A��+A���A��A��"A��A��KA���A���A��A��A���A���A���A��A�� A��/A��ZA��AΦAΗ�A�VmA�d�A�7�A��A�9$A���A��A���A�L�A�רA���A�YKA�:�A�ںA�iyA��TA��DA�;dA���A��A�A�A�%�A���A���A���A��A�D�A��iA���A��)A��A��A��A�j�A�`�A�,qA���A�m�A� \A��(A�<A���A�~�A���A�t�A�RTA}ںA|  Az�Ax^�Au�!As=Ap~�Ak��Ah� Ag��Af�mAd�9Ab� A`+A[�AX�AUa�AR�OAMs�AJ��AI��AH�EAG'RAD�AC�7AB�'AA�PA?��A=��A;J�A9;A7jA6jA3�>A2��A2[�A1�hA0��A0�+A0��A0qA/W?A. �A,�A+ȴA*�}A*�zA*:�A)FA(p�A&�A%-A%RTA#0UA ��AB[A'RA�`A{JA+A�mAAx�A1A��A�hA�.AW?A��A� A
�An�A;dA��A��AQ�A�A��A��A��A��Ar�A�A.�A�A1A��AB[A�A
=A��Av`A��A�A
�dA
�A	�0A��A\�A�oA&�A�A�OA�^A�MAc A�+A+kA��A��A�A{A�A��A|A�A�TA��AA AA ��A �A @���@��g@��N@�F�@��o@�:*@���@�&@���@���@�?}@� i@�Xy@�T�@���@�S&@�S&@�Vm@�*�@�@�ߤ@�m�@��@�X�@�6@��@�o�@��H@�R@��'@�/�@��@퇔@��[@�m]@�l�@��r@�:�@�%F@��@�8�@�7@�ߤ@蒣@��}@��@�%�@�j�@�l�@�$@�
=@�J@�RT@�0�@�ȴ@�&�@��W@޾@��@ݿH@��@�B[@���@۞�@ۏ�@�S&@ڣ�@�M@ٯ�@�X�@��@�e�@�@��p@ո�@�s�@�+�@ԝI@��d@�H�@���@���@�{�@���@�Y�@��@Ќ�@��D@ϊ�@�!-@���@Χ�@΃@͹�@�4�@���@̄�@�-�@�33@�n�@�2�@�{@ɮ�@�o@Ƞ�@�^5@��9@�G�@ƹ�@�x@��d@�J�@�@��@�l�@�L�@�7L@�q@��,@ªe@�h
@�E�@���@�+�@��@���@�h
@��K@��@���@��P@�ں@�q@���@�C@��@��p@�kQ@�	@���@���@�E9@��,@�z�@���@���@��@���@���@� �@�j�@��s@�I�@���@��@��{@�A�@�ی@�n�@�u@���@�RT@�Dg@�0�@���@���@�Xy@��@�ƨ@�rG@��@��/@��[@�|�@�	@�O�@��@�u%@���@�@��c@��b@�I�@�@��@��@��b@���@�q�@�C�@��d@�hs@�J�@�H�@�IR@�B�@�/�@��@�	l@��?@�c @�ԕ@�U�@��@��e@�^5@�7�@���@��@��@�9�@�_�@�J@���@���@�@@�h
@�%�@��N@�b�@���@�C�@��@��@@�0�@��/@��,@���@�#:@�G@��]@��X@�dZ@�&@��@�@�@��-@�|@�(�@��"@���@�c @�-�@��Z@��X@��7@�u�@�T�@�C�@�@��'@��I@���@�Z�@�(�@��)@��@@�o�@� i@��1@�S�@� �@��9@��~@�hs@�V@�֡@���@���@�_@�
�@��@�p�@��/@���@�2�@��q@���@�-w@���@�}V@��#@���@��S@�e�@�<6@�+@��X@���@�$�@���@���@�rG@�X@�IR@�<6@�Y@��@��@�S�@�M@���@�a�@�-w@���@�͟@��@��o@�-@��&@���@��@��C@��@�K�@�5�@��@���@��U@���@���@�oi@�.�@��
@���@��"@�a�@�)_@��@���@��F@�p;@�b@�u�@�+@��2@���@��@�i�@�I�@�	@�G@��@��a@���@�U�@�/@��@��5@��e@�i�@�R�@�A�@�)�@��z@�zx@�c�@��@���@���@��6@��r@�e�@�Q�@�1�@�x@��@�&@��@o�@J#@~��@}�"@}�@|u�@|�@{˒@{�@zc @z$�@y��@y�M@y	l@x��@xS�@w��@w�@v�@v�X@v.�@u�@uQ�@t��@t��@tU2@s�+@s��@s�{@sW?@sC@r�x@rM�@q�n@qX@p�@pr�@pb@o�k@n��@nq�@n?@m��@m+�@l�u@kخ@ka@j�@j�@i��@i/@h֡@hPH@g�a@g6z@f��@f�@eVm@d�)@d_@c��@cl�@b͟@b�@a��@aQ�@`�@_��@_��@_��@__p@_)_@^��@^p;@]��@]�3@]@]��@]�@\�@\�.@\2�@[�
@[iD@Z�L@Z�@Y��@Yc�@Y�@Xu�@X	�@W�q@W1�@V��@V�@U�@U}�@U�@T�4@Tz�@S��@Sl�@S4�@S$t@SC@R�M@R�x@R�@Q�@Q�C@Q5�@P��@PD�@P<�@P,=@O��@O/�@N��@Nc @N=q@N�@M�)@M��@M��@MQ�@M�@L��@L�@L��@L�@L$@K�P@KA�@K8@K.I@K�@JJ�@I�C@I-w@Hm�@G�@Ga@F��@F\�@F��@Fd�@F)�@E�H@EDg@Dq@D  @C��@CO@Cn/@C�@Bff@A��@Am]@A�@@�@@>B@@@@7@?��@?\)@>�2@>�\@>��@>kQ@>6�@=�)@=�t@=�~@=N<@<�@<��@<D�@<~@;�@;�@@;�P@;�{@;x@;@O@:��@:��@:��@:h
@:8�@:e@:�@9��@9@9j@9%@8֡@8�@8�@8,=@7�@7��@7\)@6�M@6�6@6��@6{�@6�@5��@5T�@4�@4�9@4j@4K^@4*�@4b@3��@3g�@2��@2��@2��@2p;@1��@1��@1Vm@1<6@0��@0oi@0N�@/�@/��@/\)@.�@.�@.p;@.	@-rG@-Dg@-@,�|@,N�@,M@+�;@+��@+C�@+�@*��@*��@*~�@*h
@*.�@*�@)��@)@)�X@)o @)4@(�@(��@(c�@(	�@'�6@'��@'�:@'y�@'O@')_@&��@&�}@&~�@&W�@&.�@&
�@%��@% \@$�	@$�@$�@$��@$w�@$!@#�@#��@#!-@#�@#
=@"��@"�@"d�@"($@!�.@!��@!G�@!�@ �@ �@ ��@ |�@ <�@ M@��@�$@{J@Mj@
=@�h@��@_�@@�@�@�3@hs@L�@(�@V@�@�@��@�o@U2@1'@!@�@� @�q@O@�@�]@�!@a|@!�@��@�H@�7@c�@0�@�@�v@��@�$@�u@V�@,=@1@��@��@��@{J@C�@�@�,@��@�+@kQ@a|@	@�@��@ϫ@��@%F@�@��@�9@�@��@e�@	�@��@�6@��@��@�	@]�@C@͟@��@s�@i�@&�@�@��@�9@��@j@L�@�@�@�@��@�@e�@1@خ@�0@�4@W?@�@�@�X@kQ@1�@@��@ԕ@�3@�@F@�@ی@�.@c�@I�@?�@4n@2�@@��@خ@�*@n/@o�@o�@b�@@O@�@
=@
҉@
��@
�@
q�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�OB	�cB	�cB	�)B	�QB	�B	�DB	��B	�B	��B	��B	��B	�B	�B	�B	�8B	��B	��B	��B	��B	��B	��B	��B	�_B	�_B	�B	�QB	�qB	�]B	�/B	��B	��B	��B	�OB	�OB	��B	��B	�B	�B	�pB	�@B
�B
u�B�BĜB��BЗB��B�B�|B��B��B�eB�?B�KB�lB��B��B�VB��B�Bz�Bd@BCaB0�B
�BB
�IB
��B
��B!�B,qB.�B%zBaBoB
�B
ɆB
�-B
�B
�oB
v�B
e`B
8�B
#�B
{B
tB	�jB	�-B	�B	�HB	��B	�yB	��B	��B	��B	�B	u�B	iDB	S�B	D�B	9$B	,�B	 B	gB	�B	
XB	tB	�B	 4B	�B	�B	aB	�B�xB�MB��B�B�}B�B�B�B�tB��B�%B�]B	�B	�B	�B	?B	eB	 \B	#TB	)yB	0�B	/iB	9XB	IlB	@�B	1vB	,�B	:^B	O�B	Q�B	S[B	R�B	O�B	F?B	<6B	C�B	J�B	FB	K�B	QNB	OBB	Q�B	H�B	L0B	R�B	]�B	^OB	`'B	cB	d�B	`�B	cB	bhB	`�B	b�B	k6B	p�B	xlB	w�B	yXB	|�B	~(B	z�B	w�B	vzB	{JB	�[B	}<B	xB	w�B	}�B	�B	��B	��B	��B	��B	�&B	��B	��B	�dB	�,B	�eB	�B	�4B	�nB	� B	��B	��B	�)B	��B	��B	�]B	��B	��B	�XB	�fB	�B	��B	�iB	��B	��B	��B	�MB	��B	�JB	�0B	��B	��B	�"B	��B	��B	��B	ĶB	żB	�B	�zB	�1B	��B	�B	��B	ȚB	�	B	��B	�.B	�HB	ЗB	�4B	��B	��B	ԯB	�,B	� B	�B	� B	�hB	� B	�B	�}B	�\B	�pB	͹B	�6B	̈́B	ΊB	�HB	�NB	ѝB	� B	�[B	ңB	��B	�&B	��B	��B	��B	�B	�B	�{B	ԕB	��B	��B	ܬB	��B	�#B	چB	�kB	�7B	�)B	�dB	�dB	�B	�'B	�-B	�-B	�HB	��B	��B	��B	�vB	�pB	�jB	��B	��B	��B	�5B	�!B	�\B	��B	��B	�B	�nB	�B	�sB	��B	�>B	��B	�$B	��B	�B	�kB	�B	�B	�B	�B	�kB	�"B	��B	�wB	��B	�wB	�]B	�CB	�CB	�B	�B	��B	�B	�)B	��B	�UB	�B	�zB	��B	�"B	�B	�B	�B	�B	�"B	��B	�"B	�qB	��B	�]B	�B	��B	��B	�HB	��B	��B	��B	�}B	�}B	��B	��B
 B
 �B
 �B
 �B
 �B
 B
oB
�B
[B
�B
�B
�B
B
GB
�B
MB
�B
{B
�B
{B
�B
�B
�B
YB
%B
EB
�B
zB
�B
�B
�B
�B
�B
1B
	�B

XB

�B
DB
�B
~B
jB
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
B
<B
�B
BB
vB
�B
}B
hB
4B
TB
�B
�B
�B
TB
B
�B
@B
�B
�B
�B
2B
gB
B
B
B
B
�B
�B
QB
	B
�B
�B
�B
WB
WB
=B
WB
xB
B
IB
dB
�B
�B
dB
�B
~B
�B
�B
B
 B
 �B
 �B
!-B
!bB
!�B
!�B
"�B
"�B
"�B
#nB
$B
$�B
$tB
$�B
$�B
%`B
%FB
%zB
%�B
%�B
%zB
%zB
%�B
&fB
'B
&�B
&�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
(�B
)_B
)yB
)�B
*eB
*�B
+B
+B
+6B
+B
+QB
+B
+B
+�B
+�B
+�B
+�B
,qB
-)B
-)B
-B
-CB
-�B
-�B
.B
.B
-�B
-�B
-�B
-�B
.IB
.cB
.cB
.�B
.�B
.�B
/B
/5B
/5B
/OB
/�B
.�B
0B
0!B
0;B
/�B
/�B
0B
0;B
/�B
/�B
0B
0UB
1AB
2-B
2�B
2�B
3B
3MB
3�B
3�B
3�B
3�B
3�B
49B
4�B
4�B
4�B
4�B
4nB
4�B
4TB
4TB
4�B
6+B
6FB
6FB
6�B
7B
6�B
6�B
6�B
6�B
7LB
7B
7�B
7�B
7�B
7�B
7�B
7fB
7�B
7�B
7�B
7�B
8RB
7�B
8�B
9XB
9$B
9	B
9	B
9�B
9�B
:^B
:�B
:�B
:�B
;0B
;�B
;�B
;�B
;�B
<6B
<�B
<jB
=B
="B
=VB
=�B
=�B
=�B
>�B
>�B
>�B
>�B
?HB
?�B
@4B
@iB
@�B
AoB
A�B
A�B
BB
B[B
B�B
CB
CGB
C�B
D3B
D�B
D�B
EB
EmB
E�B
F%B
F�B
F�B
GEB
G�B
G�B
HB
HB
HB
HB
H�B
H�B
H�B
H�B
IB
IRB
I7B
I�B
I�B
J#B
JXB
J�B
K)B
KDB
K^B
KxB
K�B
LB
LJB
L�B
L�B
MPB
M�B
MjB
M�B
N"B
N<B
OB
OB
O(B
OBB
O(B
OBB
O\B
O�B
O�B
O�B
PHB
P�B
P�B
P�B
P�B
QB
Q�B
R B
RTB
RoB
R�B
R�B
R�B
SB
S&B
S@B
S[B
S[B
S[B
S[B
S�B
S[B
SuB
S�B
S�B
TB
T{B
TB
T�B
T�B
T�B
T�B
UgB
U�B
VSB
WYB
W�B
W�B
W�B
WsB
V�B
V�B
W?B
X_B
XyB
X+B
W�B
XEB
X�B
X�B
Y1B
Z�B
Z�B
[=B
[�B
[=B
[	B
[#B
[qB
[�B
[�B
\B
\B
\CB
\CB
\B
\]B
]IB
]dB
]IB
]/B
]/B
]dB
]�B
]dB
]IB
]~B
]�B
]�B
]�B
]�B
^B
^B
^�B
^OB
^�B
^�B
^�B
^�B
_VB
_pB
_�B
`B
`B
`'B
`'B
`�B
a-B
abB
a�B
a�B
a�B
a�B
a�B
a�B
bB
b�B
b�B
cB
c:B
c�B
dB
dZB
d�B
dtB
d�B
eB
eB
e`B
ezB
e�B
e�B
fLB
fLB
f�B
gB
g8B
gB
g8B
h>B
h>B
hsB
h�B
h�B
iB
iDB
iyB
iyB
i�B
i�B
i�B
i�B
j0B
jB
j0B
jeB
j�B
j�B
k6B
kkB
k�B
k�B
k�B
k�B
lB
lB
lqB
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
m�B
m�B
m�B
nB
oB
oOB
o�B
o�B
oOB
o�B
poB
p�B
p�B
qB
qvB
q�B
q�B
q�B
q�B
rB
raB
raB
r|B
r�B
r�B
sB
sMB
s�B
s�B
tB
tB
t9B
t�B
uB
uB
u?B
u?B
uZB
utB
utB
u�B
u�B
u�B
vB
vFB
vFB
vzB
v�B
wB
w2B
wLB
w�B
w�B
xB
x8B
xlB
x�B
x�B
x�B
y$B
y$B
y$B
y>B
y�B
y�B
y�B
y�B
y�B
z*B
zDB
z�B
z�B
z�B
{B
{JB
{JB
{dB
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}<B
}�B
}�B
}�B
}�B
}�B
~B
~(B
~wB
~�B
~�B
~�B
~�B
HB
HB
HB
cB
�B
�B
�B
�4B
�4B
�iB
��B
��B
��B
��B
�B
� B
�UB
�oB
��B
��B
��B
�B
�B
�'B
�AB
�[B
�[B
�[B
�uB
��B
��B
�-B
�aB
�aB
�{B
��B
��B
��B
��B
��B
�3B
�gB
��B
��B
��B
�B
�B
�B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	�OB	�cB	�cB	�)B	�QB	�B	�DB	��B	�B	��B	��B	��B	�B	�B	�B	�8B	��B	��B	��B	��B	��B	��B	��B	�_B	�_B	�B	�QB	�qB	�]B	�/B	��B	��B	��B	�OB	�OB	��B	��B	�B	�B	�pB	�@B
�B
u�B�BĜB��BЗB��B�B�|B��B��B�eB�?B�KB�lB��B��B�VB��B�Bz�Bd@BCaB0�B
�BB
�IB
��B
��B!�B,qB.�B%zBaBoB
�B
ɆB
�-B
�B
�oB
v�B
e`B
8�B
#�B
{B
tB	�jB	�-B	�B	�HB	��B	�yB	��B	��B	��B	�B	u�B	iDB	S�B	D�B	9$B	,�B	 B	gB	�B	
XB	tB	�B	 4B	�B	�B	aB	�B�xB�MB��B�B�}B�B�B�B�tB��B�%B�]B	�B	�B	�B	?B	eB	 \B	#TB	)yB	0�B	/iB	9XB	IlB	@�B	1vB	,�B	:^B	O�B	Q�B	S[B	R�B	O�B	F?B	<6B	C�B	J�B	FB	K�B	QNB	OBB	Q�B	H�B	L0B	R�B	]�B	^OB	`'B	cB	d�B	`�B	cB	bhB	`�B	b�B	k6B	p�B	xlB	w�B	yXB	|�B	~(B	z�B	w�B	vzB	{JB	�[B	}<B	xB	w�B	}�B	�B	��B	��B	��B	��B	�&B	��B	��B	�dB	�,B	�eB	�B	�4B	�nB	� B	��B	��B	�)B	��B	��B	�]B	��B	��B	�XB	�fB	�B	��B	�iB	��B	��B	��B	�MB	��B	�JB	�0B	��B	��B	�"B	��B	��B	��B	ĶB	żB	�B	�zB	�1B	��B	�B	��B	ȚB	�	B	��B	�.B	�HB	ЗB	�4B	��B	��B	ԯB	�,B	� B	�B	� B	�hB	� B	�B	�}B	�\B	�pB	͹B	�6B	̈́B	ΊB	�HB	�NB	ѝB	� B	�[B	ңB	��B	�&B	��B	��B	��B	�B	�B	�{B	ԕB	��B	��B	ܬB	��B	�#B	چB	�kB	�7B	�)B	�dB	�dB	�B	�'B	�-B	�-B	�HB	��B	��B	��B	�vB	�pB	�jB	��B	��B	��B	�5B	�!B	�\B	��B	��B	�B	�nB	�B	�sB	��B	�>B	��B	�$B	��B	�B	�kB	�B	�B	�B	�B	�kB	�"B	��B	�wB	��B	�wB	�]B	�CB	�CB	�B	�B	��B	�B	�)B	��B	�UB	�B	�zB	��B	�"B	�B	�B	�B	�B	�"B	��B	�"B	�qB	��B	�]B	�B	��B	��B	�HB	��B	��B	��B	�}B	�}B	��B	��B
 B
 �B
 �B
 �B
 �B
 B
oB
�B
[B
�B
�B
�B
B
GB
�B
MB
�B
{B
�B
{B
�B
�B
�B
YB
%B
EB
�B
zB
�B
�B
�B
�B
�B
1B
	�B

XB

�B
DB
�B
~B
jB
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
B
<B
�B
BB
vB
�B
}B
hB
4B
TB
�B
�B
�B
TB
B
�B
@B
�B
�B
�B
2B
gB
B
B
B
B
�B
�B
QB
	B
�B
�B
�B
WB
WB
=B
WB
xB
B
IB
dB
�B
�B
dB
�B
~B
�B
�B
B
 B
 �B
 �B
!-B
!bB
!�B
!�B
"�B
"�B
"�B
#nB
$B
$�B
$tB
$�B
$�B
%`B
%FB
%zB
%�B
%�B
%zB
%zB
%�B
&fB
'B
&�B
&�B
%�B
%�B
&�B
&�B
&�B
'�B
(�B
(�B
)_B
)yB
)�B
*eB
*�B
+B
+B
+6B
+B
+QB
+B
+B
+�B
+�B
+�B
+�B
,qB
-)B
-)B
-B
-CB
-�B
-�B
.B
.B
-�B
-�B
-�B
-�B
.IB
.cB
.cB
.�B
.�B
.�B
/B
/5B
/5B
/OB
/�B
.�B
0B
0!B
0;B
/�B
/�B
0B
0;B
/�B
/�B
0B
0UB
1AB
2-B
2�B
2�B
3B
3MB
3�B
3�B
3�B
3�B
3�B
49B
4�B
4�B
4�B
4�B
4nB
4�B
4TB
4TB
4�B
6+B
6FB
6FB
6�B
7B
6�B
6�B
6�B
6�B
7LB
7B
7�B
7�B
7�B
7�B
7�B
7fB
7�B
7�B
7�B
7�B
8RB
7�B
8�B
9XB
9$B
9	B
9	B
9�B
9�B
:^B
:�B
:�B
:�B
;0B
;�B
;�B
;�B
;�B
<6B
<�B
<jB
=B
="B
=VB
=�B
=�B
=�B
>�B
>�B
>�B
>�B
?HB
?�B
@4B
@iB
@�B
AoB
A�B
A�B
BB
B[B
B�B
CB
CGB
C�B
D3B
D�B
D�B
EB
EmB
E�B
F%B
F�B
F�B
GEB
G�B
G�B
HB
HB
HB
HB
H�B
H�B
H�B
H�B
IB
IRB
I7B
I�B
I�B
J#B
JXB
J�B
K)B
KDB
K^B
KxB
K�B
LB
LJB
L�B
L�B
MPB
M�B
MjB
M�B
N"B
N<B
OB
OB
O(B
OBB
O(B
OBB
O\B
O�B
O�B
O�B
PHB
P�B
P�B
P�B
P�B
QB
Q�B
R B
RTB
RoB
R�B
R�B
R�B
SB
S&B
S@B
S[B
S[B
S[B
S[B
S�B
S[B
SuB
S�B
S�B
TB
T{B
TB
T�B
T�B
T�B
T�B
UgB
U�B
VSB
WYB
W�B
W�B
W�B
WsB
V�B
V�B
W?B
X_B
XyB
X+B
W�B
XEB
X�B
X�B
Y1B
Z�B
Z�B
[=B
[�B
[=B
[	B
[#B
[qB
[�B
[�B
\B
\B
\CB
\CB
\B
\]B
]IB
]dB
]IB
]/B
]/B
]dB
]�B
]dB
]IB
]~B
]�B
]�B
]�B
]�B
^B
^B
^�B
^OB
^�B
^�B
^�B
^�B
_VB
_pB
_�B
`B
`B
`'B
`'B
`�B
a-B
abB
a�B
a�B
a�B
a�B
a�B
a�B
bB
b�B
b�B
cB
c:B
c�B
dB
dZB
d�B
dtB
d�B
eB
eB
e`B
ezB
e�B
e�B
fLB
fLB
f�B
gB
g8B
gB
g8B
h>B
h>B
hsB
h�B
h�B
iB
iDB
iyB
iyB
i�B
i�B
i�B
i�B
j0B
jB
j0B
jeB
j�B
j�B
k6B
kkB
k�B
k�B
k�B
k�B
lB
lB
lqB
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
m�B
m�B
m�B
nB
oB
oOB
o�B
o�B
oOB
o�B
poB
p�B
p�B
qB
qvB
q�B
q�B
q�B
q�B
rB
raB
raB
r|B
r�B
r�B
sB
sMB
s�B
s�B
tB
tB
t9B
t�B
uB
uB
u?B
u?B
uZB
utB
utB
u�B
u�B
u�B
vB
vFB
vFB
vzB
v�B
wB
w2B
wLB
w�B
w�B
xB
x8B
xlB
x�B
x�B
x�B
y$B
y$B
y$B
y>B
y�B
y�B
y�B
y�B
y�B
z*B
zDB
z�B
z�B
z�B
{B
{JB
{JB
{dB
{�B
{�B
{�B
{�B
{�B
|�B
|�B
|�B
|�B
|�B
|�B
}<B
}�B
}�B
}�B
}�B
}�B
~B
~(B
~wB
~�B
~�B
~�B
~�B
HB
HB
HB
cB
�B
�B
�B
�4B
�4B
�iB
��B
��B
��B
��B
�B
� B
�UB
�oB
��B
��B
��B
�B
�B
�'B
�AB
�[B
�[B
�[B
�uB
��B
��B
�-B
�aB
�aB
�{B
��B
��B
��B
��B
��B
�3B
�gB
��B
��B
��B
�B
�B
�B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604105252  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604193147  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604193148  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604193148                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605043155  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605043155  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610161506                      G�O�G�O�G�O�                