CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-08-11T00:45:11Z creation;2022-08-11T00:45:16Z conversion to V3.1      
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
_FillValue                    �tArgo profile    3.1 1.2 19500101000000  20220811004511  20220811010108  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               �A   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @����q�1   @���J�A�@,��C���c�1&�x�1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx��B�ffB�  B�33B���B���B���B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C�C�C  C
  C  C  C�fC  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2L�C4  C5�fC8  C:  C<  C>  C?�fCB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CY�fC\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl�Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DIfDI�fDJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��3D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D�|�D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�ff1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@x��@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo��Bx\)B�.B�ǮB���B�aHB��{B��{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�aHB��{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC�qC�qC�qC��C	��C��C��C�=C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C20�C3��C5�=C7��C9��C;��C=��C?�=CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY�=C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck�qCm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH�\DI\DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D���D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�yHDμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�b�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aժ0AհUAյ�AնzAջ�Aռ6Aս�Aս�A���A���A��tA��RA��dA��A��NA���A��gA��A���A�ݘA��jA��;A��A��A��|A���A��?Aճ�AԄAӧA�(�A��AҀ�A̳�A���A��PAǓ�A�kA�kQA�N<A�J�A���A�\]A�=�A���A��A�e�A�[�A��LA���A�EA��kA�@�A�`vA�V9A�.}A��lA��*A�e`A��sA��jA��A�kA��-A��A�2�A�Y�A�o A��@A�D�A�X�A�^5A��A�چA�E9A�4A���A��A�S�A���A���A�NA���A�/OA�k�A��A��vA���A�qAA�_A�6zA~��A{�tAx9�Av�Ao�Ae��A`:�A\�$AY��ASjAO��ALH�AGq�ADFAB�vAA�&AA_A@2aA?��A>��A<�>A;�3A8aA7"hA7c�A5�XA4W�A4H�A2��A0�A/��A0ȴA/	�A,K�A*��A)�'A(ĜA(l"A(3�A'OA%��A$�HA$�A!��Al�AbNA9�AZ�AaA�AffAhsAA$tA�4A_�A��A�AϫA�7AVAv`A�A
=A4nA-wA��A�~A��A\�Af�Aw�AJ�A�TA@Ap�A�PAѷAݘA	�A=qA�6AkQA��A?A	A7�AM�AJ�A5?AB[AA��A��Ax�A�A�$A5?A��AخA��A#:A
�3A	�EA	 �A`�A�AO�A�A8�AɆAxAS&A�A�{AT�AB�A�A�XA�A �UA A�A @�`B@��@�{@�Y@��@���@�4@��,@��L@�{@���@���@���@�E9@�IR@�h�@�!@���@���@�iD@��@�m�@�@�s�@��`@�c @��Q@�Ft@�u%@�k@�@�7L@�9X@��@�֡@�Ft@�~�@�2�@�>�@�\@�7�@��Z@��@�B�@�ݘ@��@��@�"h@ߌ~@ޞ�@��@��@ܳh@�?�@���@۲�@�S&@��]@�xl@��K@�Mj@�Mj@�8@ذ!@؇�@���@�a@��`@�z�@� �@�˒@�}�@��@���@�^�@�$t@���@҇+@�خ@��@в�@Ёo@�u%@�U2@�@��&@ϩ�@�b�@��B@��@�ϫ@�E9@̌@�Ta@˓@���@ʬ�@�u�@�,�@��?@ȉ�@�K^@�ݘ@�IR@��2@�Q�@��K@ňf@�(@�YK@���@�x@�1�@��y@©�@�j@�(�@��d@�o�@�+@��B@�g8@��
@�6z@�֡@�7�@�S�@���@�%�@��6@���@� \@��}@��.@�2�@���@��@��@�9X@��@�/@�L0@�{@��@��@���@�y�@��@��/@��@�[�@�%�@���@�zx@�4�@��R@���@�GE@��@��@�zx@�
=@�]d@��d@���@��@�g�@�@O@��5@�c @��k@�C@�s�@��@���@�A @��@��@��@��@�l�@��@��@��t@�[W@�/�@���@�-�@�4@�1@��4@�*0@��@�u�@�7�@��@�b@���@���@�Vm@���@��@�H@���@��0@�v`@�4@�͟@��D@�i�@�V�@�:�@�x@��@�[W@���@�1�@�&�@��@�S�@�"�@��@�{�@�.�@�_@���@��9@��V@�W?@�q@�@���@�,=@�	�@���@��@���@�v�@�J�@��@��a@���@�L�@��@��@��@�:�@�x@��@�@�8�@��[@��I@�q@�:�@��]@��}@��[@�hs@��@���@���@�A�@��@���@���@�!�@�}V@��@���@��@�RT@�$t@��s@��_@��@��:@�P�@��@���@��@�\�@�7�@��4@���@���@�I�@��@�خ@�X@��@�Ĝ@�tT@��@���@��@�A�@���@��b@�W�@��@���@�L�@�͟@�v�@�H@�4@��T@��K@��*@�X@�@���@��/@��6@�kQ@�J@��@��7@�[W@�,�@�	l@�ں@���@�~�@�m�@�=q@��o@��@��"@�O@�:�@�C@��s@���@��R@���@��.@�w�@�?@�G@~�@~GE@~4@}�9@}L�@}%@|�P@|�@|�@|<�@{��@{S@z�R@z��@zJ�@y�@yhs@x�@x�_@xD�@x�@wdZ@v��@v��@v͟@v� @vOv@u��@t�@t��@t7@s6z@rW�@ru@q��@q��@q!�@p��@pM@o�f@n�@nv�@m�@m�@l�|@l`�@k��@k��@ko�@kRT@j�"@j��@j@�@i��@i��@i��@i�3@i�@i��@iQ�@i%F@h%�@gn/@fOv@e�@d��@d4n@d@c�@b@a@a�M@a�M@a7L@`�[@`M@`7@`Xy@`��@`PH@_�A@_�@^�6@^C�@^�@]��@]��@]��@]IR@\�v@\��@\>B@[��@["�@Z��@Z��@Zi�@Z-@Y��@Y��@Yu�@Y[W@Y:�@Y�@XFt@Wخ@W>�@V�<@VM�@U�@UO�@T��@T9X@T'R@S�
@S�P@S6z@R��@R�F@R_�@R-@R �@Q��@Qk�@P�K@P�u@Poi@PI�@O�@OJ#@O i@N��@N��@Nc @N;�@N_@M@MG�@LĜ@L��@L��@L>B@K�g@K�@KX�@J��@J��@J��@J0U@I�@I:�@I�@Hی@H�_@H1@G��@GJ#@F��@F��@FW�@F;�@F�@E�H@E��@E\�@D�f@Du�@D(�@C�@C�F@Cqv@C/�@B�"@B�L@Bh
@BJ�@As�@@��@@�z@@@?��@?C�@>��@>a|@>@=@=�"@=f�@<�f@<M@;��@;��@;1�@:� @:H�@9��@9�@9L�@9+@8�p@8�O@8��@8g8@8 �@7�}@7iD@7
=@6��@6 �@5��@5m]@5�@4�@4_@4!@3خ@3�@@3F�@2��@2�X@2^5@2Ov@2J@1ԕ@1�t@1��@1e,@1V@0��@0�/@0�4@0�@0bN@0Ft@0~@/�K@/��@/��@/\)@/33@.�r@.M�@.;�@-��@-��@-j@-=�@,�)@,D�@+�]@+�@+��@+e�@+,�@*�m@*Z�@*�@)�#@)�@)��@)|@)!�@(�@(�@(_@(	�@'ݘ@'g�@'Mj@'A�@'�@&��@&��@&p;@%��@%��@%u�@%?}@$�O@$�Y@$]d@$9X@$1@#�0@#�f@#]�@#4�@#$t@"��@"ff@"a|@"i�@"YK@"?@!�)@!��@!o @!c�@!�@ �@ *�@��@�6@��@\)@A�@��@��@l�@C�@�o@�S@Vm@2a@�@ی@�@�p@��@֡@�I@�D@2�@��@Z�@�M@_�@�@�h@f�@	l@Ɇ@�E@��@y>@,=@��@�w@Z�@�@�M@��@1�@	@�@�"@+�@�@��@�.@h�@b@�@�A@��@�@>�@o@�2@�@ߤ@��@O@�@�@��@��@G�@�@��@��@u�@h�@>B@�@�+@�;@ƨ@��@��@��@�{@4�@͟@��@��@.�@�j@�N@�@�@��@x�@}�@�"@\�@<6@+�@�@;@�|@��@�@�u@��@�@oi@'R@� @��@��@iD@(@
��@
��@
v�@
@�@
4@	�@	`B@	Y�@	w2@	c@	a�@	5�@	�@�@�p@�.@`�@9X@~@�]@�Q@��@@(�@!@�@��@�@��@��@t�@Mj@=@,�@ i@��@}V@i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  Aժ0AհUAյ�AնzAջ�Aռ6Aս�Aս�A���A���A��tA��RA��dA��A��NA���A��gA��A���A�ݘA��jA��;A��A��A��|A���A��?Aճ�AԄAӧA�(�A��AҀ�A̳�A���A��PAǓ�A�kA�kQA�N<A�J�A���A�\]A�=�A���A��A�e�A�[�A��LA���A�EA��kA�@�A�`vA�V9A�.}A��lA��*A�e`A��sA��jA��A�kA��-A��A�2�A�Y�A�o A��@A�D�A�X�A�^5A��A�چA�E9A�4A���A��A�S�A���A���A�NA���A�/OA�k�A��A��vA���A�qAA�_A�6zA~��A{�tAx9�Av�Ao�Ae��A`:�A\�$AY��ASjAO��ALH�AGq�ADFAB�vAA�&AA_A@2aA?��A>��A<�>A;�3A8aA7"hA7c�A5�XA4W�A4H�A2��A0�A/��A0ȴA/	�A,K�A*��A)�'A(ĜA(l"A(3�A'OA%��A$�HA$�A!��Al�AbNA9�AZ�AaA�AffAhsAA$tA�4A_�A��A�AϫA�7AVAv`A�A
=A4nA-wA��A�~A��A\�Af�Aw�AJ�A�TA@Ap�A�PAѷAݘA	�A=qA�6AkQA��A?A	A7�AM�AJ�A5?AB[AA��A��Ax�A�A�$A5?A��AخA��A#:A
�3A	�EA	 �A`�A�AO�A�A8�AɆAxAS&A�A�{AT�AB�A�A�XA�A �UA A�A @�`B@��@�{@�Y@��@���@�4@��,@��L@�{@���@���@���@�E9@�IR@�h�@�!@���@���@�iD@��@�m�@�@�s�@��`@�c @��Q@�Ft@�u%@�k@�@�7L@�9X@��@�֡@�Ft@�~�@�2�@�>�@�\@�7�@��Z@��@�B�@�ݘ@��@��@�"h@ߌ~@ޞ�@��@��@ܳh@�?�@���@۲�@�S&@��]@�xl@��K@�Mj@�Mj@�8@ذ!@؇�@���@�a@��`@�z�@� �@�˒@�}�@��@���@�^�@�$t@���@҇+@�خ@��@в�@Ёo@�u%@�U2@�@��&@ϩ�@�b�@��B@��@�ϫ@�E9@̌@�Ta@˓@���@ʬ�@�u�@�,�@��?@ȉ�@�K^@�ݘ@�IR@��2@�Q�@��K@ňf@�(@�YK@���@�x@�1�@��y@©�@�j@�(�@��d@�o�@�+@��B@�g8@��
@�6z@�֡@�7�@�S�@���@�%�@��6@���@� \@��}@��.@�2�@���@��@��@�9X@��@�/@�L0@�{@��@��@���@�y�@��@��/@��@�[�@�%�@���@�zx@�4�@��R@���@�GE@��@��@�zx@�
=@�]d@��d@���@��@�g�@�@O@��5@�c @��k@�C@�s�@��@���@�A @��@��@��@��@�l�@��@��@��t@�[W@�/�@���@�-�@�4@�1@��4@�*0@��@�u�@�7�@��@�b@���@���@�Vm@���@��@�H@���@��0@�v`@�4@�͟@��D@�i�@�V�@�:�@�x@��@�[W@���@�1�@�&�@��@�S�@�"�@��@�{�@�.�@�_@���@��9@��V@�W?@�q@�@���@�,=@�	�@���@��@���@�v�@�J�@��@��a@���@�L�@��@��@��@�:�@�x@��@�@�8�@��[@��I@�q@�:�@��]@��}@��[@�hs@��@���@���@�A�@��@���@���@�!�@�}V@��@���@��@�RT@�$t@��s@��_@��@��:@�P�@��@���@��@�\�@�7�@��4@���@���@�I�@��@�خ@�X@��@�Ĝ@�tT@��@���@��@�A�@���@��b@�W�@��@���@�L�@�͟@�v�@�H@�4@��T@��K@��*@�X@�@���@��/@��6@�kQ@�J@��@��7@�[W@�,�@�	l@�ں@���@�~�@�m�@�=q@��o@��@��"@�O@�:�@�C@��s@���@��R@���@��.@�w�@�?@�G@~�@~GE@~4@}�9@}L�@}%@|�P@|�@|�@|<�@{��@{S@z�R@z��@zJ�@y�@yhs@x�@x�_@xD�@x�@wdZ@v��@v��@v͟@v� @vOv@u��@t�@t��@t7@s6z@rW�@ru@q��@q��@q!�@p��@pM@o�f@n�@nv�@m�@m�@l�|@l`�@k��@k��@ko�@kRT@j�"@j��@j@�@i��@i��@i��@i�3@i�@i��@iQ�@i%F@h%�@gn/@fOv@e�@d��@d4n@d@c�@b@a@a�M@a�M@a7L@`�[@`M@`7@`Xy@`��@`PH@_�A@_�@^�6@^C�@^�@]��@]��@]��@]IR@\�v@\��@\>B@[��@["�@Z��@Z��@Zi�@Z-@Y��@Y��@Yu�@Y[W@Y:�@Y�@XFt@Wخ@W>�@V�<@VM�@U�@UO�@T��@T9X@T'R@S�
@S�P@S6z@R��@R�F@R_�@R-@R �@Q��@Qk�@P�K@P�u@Poi@PI�@O�@OJ#@O i@N��@N��@Nc @N;�@N_@M@MG�@LĜ@L��@L��@L>B@K�g@K�@KX�@J��@J��@J��@J0U@I�@I:�@I�@Hی@H�_@H1@G��@GJ#@F��@F��@FW�@F;�@F�@E�H@E��@E\�@D�f@Du�@D(�@C�@C�F@Cqv@C/�@B�"@B�L@Bh
@BJ�@As�@@��@@�z@@@?��@?C�@>��@>a|@>@=@=�"@=f�@<�f@<M@;��@;��@;1�@:� @:H�@9��@9�@9L�@9+@8�p@8�O@8��@8g8@8 �@7�}@7iD@7
=@6��@6 �@5��@5m]@5�@4�@4_@4!@3خ@3�@@3F�@2��@2�X@2^5@2Ov@2J@1ԕ@1�t@1��@1e,@1V@0��@0�/@0�4@0�@0bN@0Ft@0~@/�K@/��@/��@/\)@/33@.�r@.M�@.;�@-��@-��@-j@-=�@,�)@,D�@+�]@+�@+��@+e�@+,�@*�m@*Z�@*�@)�#@)�@)��@)|@)!�@(�@(�@(_@(	�@'ݘ@'g�@'Mj@'A�@'�@&��@&��@&p;@%��@%��@%u�@%?}@$�O@$�Y@$]d@$9X@$1@#�0@#�f@#]�@#4�@#$t@"��@"ff@"a|@"i�@"YK@"?@!�)@!��@!o @!c�@!�@ �@ *�@��@�6@��@\)@A�@��@��@l�@C�@�o@�S@Vm@2a@�@ی@�@�p@��@֡@�I@�D@2�@��@Z�@�M@_�@�@�h@f�@	l@Ɇ@�E@��@y>@,=@��@�w@Z�@�@�M@��@1�@	@�@�"@+�@�@��@�.@h�@b@�@�A@��@�@>�@o@�2@�@ߤ@��@O@�@�@��@��@G�@�@��@��@u�@h�@>B@�@�+@�;@ƨ@��@��@��@�{@4�@͟@��@��@.�@�j@�N@�@�@��@x�@}�@�"@\�@<6@+�@�@;@�|@��@�@�u@��@�@oi@'R@� @��@��@iD@(@
��@
��@
v�@
@�@
4@	�@	`B@	Y�@	w2@	c@	a�@	5�@	�@�@�p@�.@`�@9X@~@�]@�Q@��@@(�@!@�@��@�@��@��@t�@Mj@=@,�@ i@��@}V@i�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
,"B
2�B
88B
8�B
@4B
?HB
@iB
?�B
B�B
I�B
I�B
L~B
O�B
P�B
S�B
U2B
U�B
X_B
\�B
^5B
^�B
`BB
aB
c B
dZB
i_B
j�B
i�B
`�B
f�B
q'B
r�B
��B$�B_B�YB�,B)DBDB[qBz�B�fB��B��B��B�-B��B��B��B��B��B�}B��B�B��B�B�XB� Bt�Bs3Bk6Be�B\�BX�BS�BOvBI�BAUB1'B!�B�B�XB��B��B�$B��Bo�BK�B4nB(sB�BpB
��B
�OB
� B
ªB
�B
�fB
u�B
lWB
E9B
\B	��B	��B	ΥB	�TB	jB	LB	DgB	8�B	'�B	�B	'B��B��B��B�B��B�eB�B�B�B�6B��B�8B	fB	B�}B	
rB	mB�dB	AB	SB	 �B	�B	B	�B	�B	�B	VB	�B		B	B	'�B	B	B	gB	uB	-B	�B	B	&LB	(�B	/iB	D�B	l�B	�B	�GB	��B	��B	�OB	��B	��B	�aB	��B	��B	��B	��B	��B	�hB	��B	��B	�RB	��B	�6B	�RB	ּB	�aB	�vB	�,B	��B	�4B	�B	�pB	�OB	�	B	��B	�VB	��B	�B	�B	�UB	��B	�;B	�GB	��B	��B	��B	�"B	�QB	��B	��B	��B	�B	��B	��B	�B	��B	�B	��B	��B	��B	ޞB	��B	ٚB	�B	�B	�#B	�B	��B	�
B	�YB	յB	��B	��B	�B	�yB	ּB	��B	ևB	�qB	��B	ߊB	�BB	�B	��B	�'B	�BB	޸B	�=B	ܬB	��B	�xB	��B	�7B	�eB	�eB	�B	��B	�B	�B	ܒB	ܒB	�kB	�7B	��B	՛B	�SB	�MB	յB	՛B	�mB	��B	�KB	��B	��B	�qB	یB	�CB	�=B	�#B	�	B	�	B	�)B	�WB	�kB	�#B	�=B	�	B	��B	�dB	��B	��B	�B	ܒB	߾B	�B	� B	�,B	�`B	�B	�,B	�B	��B	�B	�B	�B	��B	�B	��B	�B	�B	�0B	�eB	�B	�B	�B	��B	�B	�QB	�B	��B	�IB	�B	��B	�iB	�'B	�B	�GB	�3B	�B	��B	�vB	�[B	��B	�-B	�B	�B	�MB	��B	��B	��B	�2B	��B	��B	�JB	�JB	�0B	�dB	�0B	�B	�dB	��B	�B	��B	��B	�B	�6B	�B	�jB	�PB	��B	��B	��B	��B	�B	�"B	�B	�"B	��B	��B	�]B	�]B	��B	��B	��B	�}B	�}B	��B
 �B
oB
�B
�B
�B
�B
�B
�B
GB
SB
B
?B
�B
�B
�B
�B
YB
�B
	RB
	�B
	�B
	�B

XB

�B
	�B
B
	RB
	�B
	�B
	RB
�B
KB
fB
�B
	7B
	�B

�B

�B

�B
)B
�B
�B
�B
�B
�B
0B
dB
�B
�B
B
B
"B
"B
�B
�B
jB
�B
�B
BB
�B
�B
B
HB
�B
�B
�B
}B
}B
�B
�B
�B
�B
�B
B
NB
NB
NB
�B
�B
�B
:B
�B
TB
�B
�B
�B
�B
�B
[B
�B
B
FB
�B
�B
2B
�B
�B
�B
9B
B
B
�B
�B
�B
?B
B
_B
�B
�B
�B
1B
KB
KB
B
kB
�B
�B
�B
CB
)B
)B
xB
�B
�B
;B
�B
�B
�B
 BB
 'B
 �B
!bB
 �B
 �B
!|B
!�B
"�B
"hB
#�B
#�B
#�B
$B
#�B
#�B
$tB
$�B
%�B
%�B
%�B
%`B
%zB
%�B
&�B
'RB
'mB
'�B
(�B
(�B
(�B
)_B
)_B
)_B
)�B
)�B
*B
*�B
+kB
+�B
,�B
,�B
,�B
-CB
-]B
-�B
.IB
.�B
.�B
/OB
/�B
/�B
/�B
/�B
0oB
0�B
0�B
1�B
1�B
1�B
2aB
2-B
3MB
3�B
3�B
3�B
3�B
3�B
3MB
2�B
4B
4�B
4�B
4TB
4�B
5B
5ZB
5�B
6�B
6�B
6�B
6�B
72B
7fB
7�B
7�B
7�B
8B
8�B
9XB
9$B
9$B
9$B
9>B
9XB
9rB
9�B
:DB
:xB
;JB
;�B
;�B
;�B
;�B
<B
<6B
<B
<B
<PB
<jB
<jB
<�B
<jB
<B
;�B
<6B
<�B
=�B
=�B
=�B
>�B
>B
=�B
>�B
?�B
@4B
AUB
A�B
AoB
A�B
AUB
@iB
?cB
>�B
>�B
>�B
>�B
@B
@�B
@OB
@OB
@iB
@�B
@�B
A�B
DMB
GzB
G�B
G�B
G�B
G�B
HKB
H�B
IRB
I�B
I�B
I�B
J	B
JXB
JXB
J�B
KDB
KxB
K�B
K�B
K�B
K�B
L0B
LJB
LB
L0B
L0B
L�B
MB
MPB
M�B
M�B
N<B
N�B
OB
OBB
OB
O\B
OvB
O�B
PB
PHB
PbB
P�B
P�B
P�B
Q4B
RB
R B
RB
R B
RoB
R�B
R�B
R�B
R�B
SB
SuB
S�B
S�B
S�B
S�B
S�B
TB
T,B
T,B
T�B
UB
U�B
U�B
U�B
VB
V9B
VSB
VSB
VmB
VmB
W$B
WYB
WsB
W?B
V�B
W$B
W
B
W
B
WsB
XB
XyB
XEB
XB
X�B
YB
ZB
Z7B
ZB
Y�B
Y�B
YB
Z7B
Z�B
Y�B
Y�B
ZB
Y�B
Y�B
Y�B
ZkB
[WB
[qB
\B
\B
\)B
\]B
\�B
\�B
]IB
]�B
^B
^jB
^jB
^�B
^�B
_;B
_;B
_VB
_�B
_�B
_�B
_�B
`B
`B
`BB
`\B
`vB
`�B
a-B
aB
abB
a�B
a�B
b4B
b�B
b�B
cTB
c�B
d&B
c�B
d&B
d�B
e�B
e�B
fB
f2B
f�B
f�B
f�B
f�B
gB
gB
gB
gB
gB
g�B
h$B
h
B
hsB
h$B
hsB
h�B
h�B
h�B
i*B
i�B
jB
j�B
jB
jB
jB
i�B
i�B
i�B
j0B
jKB
jKB
j�B
j�B
j�B
j�B
j�B
j�B
kB
k6B
kkB
k�B
k�B
l"B
lB
l=B
k�B
k�B
k�B
l�B
m]B
mwB
mCB
m�B
m�B
m�B
mwB
m]B
m�B
nIB
nB
n�B
o�B
o�B
o�B
o�B
p!B
p�B
q'B
qvB
q�B
q�B
q�B
q�B
r-B
rGB
rGB
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
s�B
tB
tB
t�B
t�B
u�B
u�B
vzB
vFB
v�B
v�B
v�B
vzB
v�B
xB
y>B
y	B
y�B
y�B
y�B
y�B
y>B
y	B
x�B
xRB
xRB
y$B
y�B
y�B
y�B
y�B
zDB
zDB
z�B
z�B
{B
{�B
{�B
|�B
|�B
|�B
|jB
|jB
|�B
|�B
|�B
|�B
}VB
}�B
}�B
}�B
}�B
~(B
~]B
~]B
~�B
~�B
~�B
~�B
B
.B
.B
HB
cB
cB
}B
�B
� B
��B
��B
��B
�UB
��B
��B
�B
��B
��B
�B
�9B
��B
�?B
�YB
�tB
�tB
��B
��B
��B
��B
�+B
�B
�B
��B
��B
��B
��B
��B
�B
�EB
�B
�+B
�_B
�_B
�+B
��B
�_B
�+B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�KB
�fB
�7B
�rB
�B
��B
��B
��B
�^B
��B
��B
��B
�B
�0B
�JB
�dB
�dB
�~B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B
,"B
2�B
88B
8�B
@4B
?HB
@iB
?�B
B�B
I�B
I�B
L~B
O�B
P�B
S�B
U2B
U�B
X_B
\�B
^5B
^�B
`BB
aB
c B
dZB
i_B
j�B
i�B
`�B
f�B
q'B
r�B
��B$�B_B�YB�,B)DBDB[qBz�B�fB��B��B��B�-B��B��B��B��B��B�}B��B�B��B�B�XB� Bt�Bs3Bk6Be�B\�BX�BS�BOvBI�BAUB1'B!�B�B�XB��B��B�$B��Bo�BK�B4nB(sB�BpB
��B
�OB
� B
ªB
�B
�fB
u�B
lWB
E9B
\B	��B	��B	ΥB	�TB	jB	LB	DgB	8�B	'�B	�B	'B��B��B��B�B��B�eB�B�B�B�6B��B�8B	fB	B�}B	
rB	mB�dB	AB	SB	 �B	�B	B	�B	�B	�B	VB	�B		B	B	'�B	B	B	gB	uB	-B	�B	B	&LB	(�B	/iB	D�B	l�B	�B	�GB	��B	��B	�OB	��B	��B	�aB	��B	��B	��B	��B	��B	�hB	��B	��B	�RB	��B	�6B	�RB	ּB	�aB	�vB	�,B	��B	�4B	�B	�pB	�OB	�	B	��B	�VB	��B	�B	�B	�UB	��B	�;B	�GB	��B	��B	��B	�"B	�QB	��B	��B	��B	�B	��B	��B	�B	��B	�B	��B	��B	��B	ޞB	��B	ٚB	�B	�B	�#B	�B	��B	�
B	�YB	յB	��B	��B	�B	�yB	ּB	��B	ևB	�qB	��B	ߊB	�BB	�B	��B	�'B	�BB	޸B	�=B	ܬB	��B	�xB	��B	�7B	�eB	�eB	�B	��B	�B	�B	ܒB	ܒB	�kB	�7B	��B	՛B	�SB	�MB	յB	՛B	�mB	��B	�KB	��B	��B	�qB	یB	�CB	�=B	�#B	�	B	�	B	�)B	�WB	�kB	�#B	�=B	�	B	��B	�dB	��B	��B	�B	ܒB	߾B	�B	� B	�,B	�`B	�B	�,B	�B	��B	�B	�B	�B	��B	�B	��B	�B	�B	�0B	�eB	�B	�B	�B	��B	�B	�QB	�B	��B	�IB	�B	��B	�iB	�'B	�B	�GB	�3B	�B	��B	�vB	�[B	��B	�-B	�B	�B	�MB	��B	��B	��B	�2B	��B	��B	�JB	�JB	�0B	�dB	�0B	�B	�dB	��B	�B	��B	��B	�B	�6B	�B	�jB	�PB	��B	��B	��B	��B	�B	�"B	�B	�"B	��B	��B	�]B	�]B	��B	��B	��B	�}B	�}B	��B
 �B
oB
�B
�B
�B
�B
�B
�B
GB
SB
B
?B
�B
�B
�B
�B
YB
�B
	RB
	�B
	�B
	�B

XB

�B
	�B
B
	RB
	�B
	�B
	RB
�B
KB
fB
�B
	7B
	�B

�B

�B

�B
)B
�B
�B
�B
�B
�B
0B
dB
�B
�B
B
B
"B
"B
�B
�B
jB
�B
�B
BB
�B
�B
B
HB
�B
�B
�B
}B
}B
�B
�B
�B
�B
�B
B
NB
NB
NB
�B
�B
�B
:B
�B
TB
�B
�B
�B
�B
�B
[B
�B
B
FB
�B
�B
2B
�B
�B
�B
9B
B
B
�B
�B
�B
?B
B
_B
�B
�B
�B
1B
KB
KB
B
kB
�B
�B
�B
CB
)B
)B
xB
�B
�B
;B
�B
�B
�B
 BB
 'B
 �B
!bB
 �B
 �B
!|B
!�B
"�B
"hB
#�B
#�B
#�B
$B
#�B
#�B
$tB
$�B
%�B
%�B
%�B
%`B
%zB
%�B
&�B
'RB
'mB
'�B
(�B
(�B
(�B
)_B
)_B
)_B
)�B
)�B
*B
*�B
+kB
+�B
,�B
,�B
,�B
-CB
-]B
-�B
.IB
.�B
.�B
/OB
/�B
/�B
/�B
/�B
0oB
0�B
0�B
1�B
1�B
1�B
2aB
2-B
3MB
3�B
3�B
3�B
3�B
3�B
3MB
2�B
4B
4�B
4�B
4TB
4�B
5B
5ZB
5�B
6�B
6�B
6�B
6�B
72B
7fB
7�B
7�B
7�B
8B
8�B
9XB
9$B
9$B
9$B
9>B
9XB
9rB
9�B
:DB
:xB
;JB
;�B
;�B
;�B
;�B
<B
<6B
<B
<B
<PB
<jB
<jB
<�B
<jB
<B
;�B
<6B
<�B
=�B
=�B
=�B
>�B
>B
=�B
>�B
?�B
@4B
AUB
A�B
AoB
A�B
AUB
@iB
?cB
>�B
>�B
>�B
>�B
@B
@�B
@OB
@OB
@iB
@�B
@�B
A�B
DMB
GzB
G�B
G�B
G�B
G�B
HKB
H�B
IRB
I�B
I�B
I�B
J	B
JXB
JXB
J�B
KDB
KxB
K�B
K�B
K�B
K�B
L0B
LJB
LB
L0B
L0B
L�B
MB
MPB
M�B
M�B
N<B
N�B
OB
OBB
OB
O\B
OvB
O�B
PB
PHB
PbB
P�B
P�B
P�B
Q4B
RB
R B
RB
R B
RoB
R�B
R�B
R�B
R�B
SB
SuB
S�B
S�B
S�B
S�B
S�B
TB
T,B
T,B
T�B
UB
U�B
U�B
U�B
VB
V9B
VSB
VSB
VmB
VmB
W$B
WYB
WsB
W?B
V�B
W$B
W
B
W
B
WsB
XB
XyB
XEB
XB
X�B
YB
ZB
Z7B
ZB
Y�B
Y�B
YB
Z7B
Z�B
Y�B
Y�B
ZB
Y�B
Y�B
Y�B
ZkB
[WB
[qB
\B
\B
\)B
\]B
\�B
\�B
]IB
]�B
^B
^jB
^jB
^�B
^�B
_;B
_;B
_VB
_�B
_�B
_�B
_�B
`B
`B
`BB
`\B
`vB
`�B
a-B
aB
abB
a�B
a�B
b4B
b�B
b�B
cTB
c�B
d&B
c�B
d&B
d�B
e�B
e�B
fB
f2B
f�B
f�B
f�B
f�B
gB
gB
gB
gB
gB
g�B
h$B
h
B
hsB
h$B
hsB
h�B
h�B
h�B
i*B
i�B
jB
j�B
jB
jB
jB
i�B
i�B
i�B
j0B
jKB
jKB
j�B
j�B
j�B
j�B
j�B
j�B
kB
k6B
kkB
k�B
k�B
l"B
lB
l=B
k�B
k�B
k�B
l�B
m]B
mwB
mCB
m�B
m�B
m�B
mwB
m]B
m�B
nIB
nB
n�B
o�B
o�B
o�B
o�B
p!B
p�B
q'B
qvB
q�B
q�B
q�B
q�B
r-B
rGB
rGB
r|B
r�B
r�B
r�B
r�B
r�B
r�B
r�B
sB
s�B
tB
tB
t�B
t�B
u�B
u�B
vzB
vFB
v�B
v�B
v�B
vzB
v�B
xB
y>B
y	B
y�B
y�B
y�B
y�B
y>B
y	B
x�B
xRB
xRB
y$B
y�B
y�B
y�B
y�B
zDB
zDB
z�B
z�B
{B
{�B
{�B
|�B
|�B
|�B
|jB
|jB
|�B
|�B
|�B
|�B
}VB
}�B
}�B
}�B
}�B
~(B
~]B
~]B
~�B
~�B
~�B
~�B
B
.B
.B
HB
cB
cB
}B
�B
� B
��B
��B
��B
�UB
��B
��B
�B
��B
��B
�B
�9B
��B
�?B
�YB
�tB
�tB
��B
��B
��B
��B
�+B
�B
�B
��B
��B
��B
��B
��B
�B
�EB
�B
�+B
�_B
�_B
�+B
��B
�_B
�+B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�KB
�fB
�7B
�rB
�B
��B
��B
��B
�^B
��B
��B
��B
�B
�0B
�JB
�dB
�dB
�~B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220811004453  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220811004511  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220811004516  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220811004516                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220811094529  QCP$                G�O�G�O�G�O�         20DF35EJA  ARGQrqcpc3.6                                                                20220811094529  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220811010108                      G�O�G�O�G�O�                