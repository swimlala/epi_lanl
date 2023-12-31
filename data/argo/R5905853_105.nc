CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       E2022-06-04T17:41:34Z creation;2022-06-04T17:41:34Z conversion to V3.1      
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
_FillValue                 �  I4   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M$   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  p|   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tl   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �h   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �X   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ޤ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20220604174134  20220610131508  5905853                                                                 JAMSTEC                                                         PRES            TEMP            PSAL               iA   JA                                  2B  A   APEX                            8421                            2.11.2                          846 @٬(����1   @٬(��S@/e�S����cE�+1   GPS     A   A   A   Primary sampling: averaged [2 dbar bin average for 1 Hz CTD]                                                                                                                                                                                                       @��@�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�ffB���B�  B�  B�  B�  B�  B�  B�  B�  B���B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  C   C  C  C  C  C
  C  C  C  C  C�fC  C  C  C�fC  C   C"  C$  C&  C(  C*  C,  C.�C033C1�fC3�fC6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ�C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D�fDfD� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH�fDI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D��3D�  D�@ Dڃ3D��3D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D��3D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�]@x��@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�.B��{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�aHB��{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB���B���B�ǮB�ǮC��C��C��C��C	��C��C��C��C��C�=C��C��C��C�=C��C��C!��C#��C%��C'��C)��C+��C-�qC0
C1�=C3�=C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY�qC[�qC]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D\D�\Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DH\DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D���D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dٿ�D��{D�<{D��Dڿ�D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D��D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aɀ�AɗYAɗ$Aɝ�AɎ�AɓuAɧ�A��Aʕ�A��A�-CA�.IA�/�A�?}A�f2Aˌ�A˷LA�ϫA��A�G�A�X�A�v�A�v�A�qAA�poA�rA�s�A�t�A�v`A�w�A�Z�A�$�A�R�Aʷ�A�EmAɺ�AȤ@Aǎ�A�E�A��A��A�6�A��=A�y>A�h�A��|A�|A�[�A�J�A��PA�T�A�U�A��|A�M�A�:A�[WA�cTA�h�A�HKA�o5A��A�ZQA�IA��rA���A��'A��8A���A�[�A��A���A�J�A��%A�_A��A�9XA��KA���A�[�A�=qA��}A�a|A�(�A���A��A�|PA�uZA��Ax�Aus�At�gAt�:As�MAr�zAn�Ak�Ak;Ai^5Ag/�Aa_AZߤAU�wAS�$AO�oAN�AI	�AEv�AC��AAg8A?�"A<�`A:(�A9h
A97�A9+A8�uA8!�A8�A7��A7C-A6S&A5�A3�A1ɆA/��A.�5A.[�A/�7A.kQA-��A-#�A-oA-�A,�6A,;�A*��A)�,A)�A)�A)��A){�A)�A(�DA'=qA%��A%7�A$�A$�A"��A!��A ȴAg8AԕA�+Ax�A�A1A�A�PAk�A($A�Ar�A�[A��AW?A)_AɆA��Ai�A��A8A�8A��AbA�3A�A�CAk�A�A�kAoiAR�A�]Ab�AAcA�wAM�AںAv�AtTA�A�YA��A
҉A
QA
  A	�~A	_�A	OA��A��AخA��A~�AA�A�fA��A|�A��Ax�A.IAiDA	A�AB[AVA��A(A  A�}A��A�;A��A��A��A|As�AkQAXyA�Ag�A�A ��A ^5@��h@��f@�(�@���@�|@�Mj@���@��>@���@�c�@��-@��@��@���@��@��N@�!�@��+@�Y@�}�@��y@���@��8@�@�E�@@�X@��T@�Q�@���@���@���@���@�Y�@�&�@�Ft@�N<@�J�@��@��@���@ގ�@�tT@��@�hs@��@ܤ�@�{@ہ�@ڥz@�:*@��@ٸ�@�o�@�ff@�F@�z@�@�"�@��M@�]d@�J�@��H@Ґ.@��D@�A @��m@�Ta@�1�@�R�@�@��y@��@�H�@��[@�W�@���@˨X@�|�@��B@�_�@���@�hs@Ⱦ@�$�@��]@ǵt@�{J@��@��?@�L0@��.@Ŋ	@�C@���@ģ�@�{�@�I�@��@Ê	@�P�@°�@��@�?}@��@��@�4n@�� @�L0@��j@���@�o @���@�Ft@���@��t@��y@��@�q�@�@���@��3@�b�@�!�@��@��@�M@��m@���@�j@��@���@��F@�V@�1'@�@�ƨ@�Mj@��h@�`�@�@�j@��@� i@�W�@��@�A @�|�@�J@��H@�+�@���@�a|@�V@�1'@��A@�RT@��<@��@��@�A�@�ѷ@��@�l�@�A�@�/�@���@���@�@O@��@���@�d�@���@�s�@�7L@��@���@�.�@��@���@��	@�S&@�&@��@��@��@�Y�@��y@���@��z@���@�4n@��@���@��~@�x�@�j�@�)_@��y@��e@�=q@���@�&�@��'@�L0@�{@���@��n@���@���@�U2@�	@�N<@�ȴ@�~�@�$�@���@���@���@�o�@��@��<@��e@��u@�/�@��@��
@��}@���@�L�@��@��@��x@���@�x�@�o@�֡@��m@���@�Ta@��d@��@�k�@�O@��@��@��@���@��$@�V�@���@��@@�Y�@�q@�	l@���@�j@��@��@���@�_p@�4@���@��B@��.@�%�@��w@��@���@�YK@�e@��#@���@�iD@��@�e�@���@�[W@� \@���@�]d@��Z@��@��X@�y�@�N<@�9�@���@��@�PH@�!@��@��Q@���@���@�O�@�+@��@��@���@�c�@�	�@�@���@�g�@�b�@�RT@�'�@���@���@��@��D@�u%@�Z@�7�@�@���@�p�@�4�@�%F@���@�Ɇ@���@��+@�@�@�6@���@��S@�u�@�4@�ߤ@���@�l�@�PH@�B[@�4@~�@~�}@~h
@~B[@~ �@}�t@|��@{�@{�@{S@z�y@z��@z��@z5?@y�t@y��@yVm@x��@x�@xc�@xK^@w�+@wy�@w i@v~�@u@u�@tĜ@tZ@t*�@s��@sS@rq�@q�@q \@p��@p  @o��@o8@o,�@n��@nff@n{@m�H@m	l@l�o@lC-@l�@k��@k˒@ko�@k�@j�m@j��@jq�@j�@i��@io @i[W@h�@h�o@h9X@g�@g�w@g|�@f��@f��@f��@f
�@e�z@e�@e%F@d��@d��@dXy@c�q@cg�@c�@b�x@b�@as�@`�/@`�@_�@_��@_�f@_��@_v`@_g�@_W?@_=@_�@^҉@^R�@]}�@\��@\�@[��@Z�c@Z�@Z��@Z��@Zh
@Y��@Y�7@Y@@X�?@XK^@W�K@W|�@WS�@W"�@W�@V�@V0U@Uϫ@U��@Us�@U\�@U7L@TɆ@T�o@T9X@T�@T�@T�@S�A@S~�@R��@Rc @R�@Q�@Q�@Q��@Qj@Q�@P�e@P�o@P�@O�P@O
=@Nȴ@N��@NGE@N �@M��@M�@M^�@M(�@L��@L�@K�}@KE9@J��@J��@JO@Iԕ@Iw2@HĜ@HA�@G�}@GW?@Fq�@E�@E��@E`B@D�E@D�@D<�@C��@Co�@B�s@BW�@B@A��@A�>@A@A��@Azx@A0�@@��@@l"@@<�@?��@?�6@?x@?.I@>�x@>ff@>8�@=�@=��@=2a@<�9@<h�@<!@;�m@;��@;H�@;6z@;"�@:�y@:V@9�"@95�@9@8��@8�@8��@8|�@7�w@6�@6 �@5	l@4�)@4/�@3��@3��@2�y@2��@2a|@1��@1�@1m]@0��@0h�@0!@/�+@/�0@/��@/S�@/�@.�@.�L@.z@.B[@-��@-�n@-c�@,��@,z�@,K^@,$@,�@+��@+�w@+]�@*�@*��@*��@*u%@*L0@*�@)��@)/@(ی@(�O@(��@(�I@(H@'��@'RT@'.I@&��@&҉@&s�@%�D@%�t@%w2@%Dg@$�|@$�$@$�@$`�@$*�@#�w@#��@#��@#S�@"�H@"�@"ff@"�@!�X@!p�@!Y�@!V@ ��@ �?@ ��@ ��@ A�@��@��@W?@C@�y@ȴ@��@�x@\�@�@p�@4@%@�`@��@��@z�@�@�K@qv@$t@�@�@��@��@�\@c @Ta@C�@	@�C@�@8�@�v@�@6@˒@��@��@�@y�@1�@�]@�1@W�@L0@;�@$�@�D@�>@�@��@j@T�@+�@�@�`@��@u�@>B@�@��@�[@�:@dZ@X�@A�@
=@�m@�R@�F@M�@	@�#@��@c@[W@J�@#�@�[@�Y@7�@�@�@	�@��@�@�*@�@\)@6z@�@��@ȴ@��@�1@� @Z�@$�@�D@�t@f�@8�@@�@�@�u@��@c�@1'@b@�;@�@b�@ i@
��@
� @
�A@
s�@
;�@
@	�N@	x�@	O�@	*0@�/@��@�@u�@H@'R@�@b@�@�&@�@�@&@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   Aɀ�AɗYAɗ$Aɝ�AɎ�AɓuAɧ�A��Aʕ�A��A�-CA�.IA�/�A�?}A�f2Aˌ�A˷LA�ϫA��A�G�A�X�A�v�A�v�A�qAA�poA�rA�s�A�t�A�v`A�w�A�Z�A�$�A�R�Aʷ�A�EmAɺ�AȤ@Aǎ�A�E�A��A��A�6�A��=A�y>A�h�A��|A�|A�[�A�J�A��PA�T�A�U�A��|A�M�A�:A�[WA�cTA�h�A�HKA�o5A��A�ZQA�IA��rA���A��'A��8A���A�[�A��A���A�J�A��%A�_A��A�9XA��KA���A�[�A�=qA��}A�a|A�(�A���A��A�|PA�uZA��Ax�Aus�At�gAt�:As�MAr�zAn�Ak�Ak;Ai^5Ag/�Aa_AZߤAU�wAS�$AO�oAN�AI	�AEv�AC��AAg8A?�"A<�`A:(�A9h
A97�A9+A8�uA8!�A8�A7��A7C-A6S&A5�A3�A1ɆA/��A.�5A.[�A/�7A.kQA-��A-#�A-oA-�A,�6A,;�A*��A)�,A)�A)�A)��A){�A)�A(�DA'=qA%��A%7�A$�A$�A"��A!��A ȴAg8AԕA�+Ax�A�A1A�A�PAk�A($A�Ar�A�[A��AW?A)_AɆA��Ai�A��A8A�8A��AbA�3A�A�CAk�A�A�kAoiAR�A�]Ab�AAcA�wAM�AںAv�AtTA�A�YA��A
҉A
QA
  A	�~A	_�A	OA��A��AخA��A~�AA�A�fA��A|�A��Ax�A.IAiDA	A�AB[AVA��A(A  A�}A��A�;A��A��A��A|As�AkQAXyA�Ag�A�A ��A ^5@��h@��f@�(�@���@�|@�Mj@���@��>@���@�c�@��-@��@��@���@��@��N@�!�@��+@�Y@�}�@��y@���@��8@�@�E�@@�X@��T@�Q�@���@���@���@���@�Y�@�&�@�Ft@�N<@�J�@��@��@���@ގ�@�tT@��@�hs@��@ܤ�@�{@ہ�@ڥz@�:*@��@ٸ�@�o�@�ff@�F@�z@�@�"�@��M@�]d@�J�@��H@Ґ.@��D@�A @��m@�Ta@�1�@�R�@�@��y@��@�H�@��[@�W�@���@˨X@�|�@��B@�_�@���@�hs@Ⱦ@�$�@��]@ǵt@�{J@��@��?@�L0@��.@Ŋ	@�C@���@ģ�@�{�@�I�@��@Ê	@�P�@°�@��@�?}@��@��@�4n@�� @�L0@��j@���@�o @���@�Ft@���@��t@��y@��@�q�@�@���@��3@�b�@�!�@��@��@�M@��m@���@�j@��@���@��F@�V@�1'@�@�ƨ@�Mj@��h@�`�@�@�j@��@� i@�W�@��@�A @�|�@�J@��H@�+�@���@�a|@�V@�1'@��A@�RT@��<@��@��@�A�@�ѷ@��@�l�@�A�@�/�@���@���@�@O@��@���@�d�@���@�s�@�7L@��@���@�.�@��@���@��	@�S&@�&@��@��@��@�Y�@��y@���@��z@���@�4n@��@���@��~@�x�@�j�@�)_@��y@��e@�=q@���@�&�@��'@�L0@�{@���@��n@���@���@�U2@�	@�N<@�ȴ@�~�@�$�@���@���@���@�o�@��@��<@��e@��u@�/�@��@��
@��}@���@�L�@��@��@��x@���@�x�@�o@�֡@��m@���@�Ta@��d@��@�k�@�O@��@��@��@���@��$@�V�@���@��@@�Y�@�q@�	l@���@�j@��@��@���@�_p@�4@���@��B@��.@�%�@��w@��@���@�YK@�e@��#@���@�iD@��@�e�@���@�[W@� \@���@�]d@��Z@��@��X@�y�@�N<@�9�@���@��@�PH@�!@��@��Q@���@���@�O�@�+@��@��@���@�c�@�	�@�@���@�g�@�b�@�RT@�'�@���@���@��@��D@�u%@�Z@�7�@�@���@�p�@�4�@�%F@���@�Ɇ@���@��+@�@�@�6@���@��S@�u�@�4@�ߤ@���@�l�@�PH@�B[@�4@~�@~�}@~h
@~B[@~ �@}�t@|��@{�@{�@{S@z�y@z��@z��@z5?@y�t@y��@yVm@x��@x�@xc�@xK^@w�+@wy�@w i@v~�@u@u�@tĜ@tZ@t*�@s��@sS@rq�@q�@q \@p��@p  @o��@o8@o,�@n��@nff@n{@m�H@m	l@l�o@lC-@l�@k��@k˒@ko�@k�@j�m@j��@jq�@j�@i��@io @i[W@h�@h�o@h9X@g�@g�w@g|�@f��@f��@f��@f
�@e�z@e�@e%F@d��@d��@dXy@c�q@cg�@c�@b�x@b�@as�@`�/@`�@_�@_��@_�f@_��@_v`@_g�@_W?@_=@_�@^҉@^R�@]}�@\��@\�@[��@Z�c@Z�@Z��@Z��@Zh
@Y��@Y�7@Y@@X�?@XK^@W�K@W|�@WS�@W"�@W�@V�@V0U@Uϫ@U��@Us�@U\�@U7L@TɆ@T�o@T9X@T�@T�@T�@S�A@S~�@R��@Rc @R�@Q�@Q�@Q��@Qj@Q�@P�e@P�o@P�@O�P@O
=@Nȴ@N��@NGE@N �@M��@M�@M^�@M(�@L��@L�@K�}@KE9@J��@J��@JO@Iԕ@Iw2@HĜ@HA�@G�}@GW?@Fq�@E�@E��@E`B@D�E@D�@D<�@C��@Co�@B�s@BW�@B@A��@A�>@A@A��@Azx@A0�@@��@@l"@@<�@?��@?�6@?x@?.I@>�x@>ff@>8�@=�@=��@=2a@<�9@<h�@<!@;�m@;��@;H�@;6z@;"�@:�y@:V@9�"@95�@9@8��@8�@8��@8|�@7�w@6�@6 �@5	l@4�)@4/�@3��@3��@2�y@2��@2a|@1��@1�@1m]@0��@0h�@0!@/�+@/�0@/��@/S�@/�@.�@.�L@.z@.B[@-��@-�n@-c�@,��@,z�@,K^@,$@,�@+��@+�w@+]�@*�@*��@*��@*u%@*L0@*�@)��@)/@(ی@(�O@(��@(�I@(H@'��@'RT@'.I@&��@&҉@&s�@%�D@%�t@%w2@%Dg@$�|@$�$@$�@$`�@$*�@#�w@#��@#��@#S�@"�H@"�@"ff@"�@!�X@!p�@!Y�@!V@ ��@ �?@ ��@ ��@ A�@��@��@W?@C@�y@ȴ@��@�x@\�@�@p�@4@%@�`@��@��@z�@�@�K@qv@$t@�@�@��@��@�\@c @Ta@C�@	@�C@�@8�@�v@�@6@˒@��@��@�@y�@1�@�]@�1@W�@L0@;�@$�@�D@�>@�@��@j@T�@+�@�@�`@��@u�@>B@�@��@�[@�:@dZ@X�@A�@
=@�m@�R@�F@M�@	@�#@��@c@[W@J�@#�@�[@�Y@7�@�@�@	�@��@�@�*@�@\)@6z@�@��@ȴ@��@�1@� @Z�@$�@�D@�t@f�@8�@@�@�@�u@��@c�@1'@b@�;@�@b�@ i@
��@
� @
�A@
s�@
;�@
@	�N@	x�@	O�@	*0@�/@��@�@u�@H@'R@�@b@�@�&@�@�@&@��@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	��B	��B	�B	�jB	��B	�@B	��B	��B
?�B
�	B
�}B
��B
�B
B
߾B
��B%B�B&�BGEBNVB_�Bf�BkQBl�BmCBm�Bn/BncBn�Bp�Bo�Bq�BsMBrBoBk�Bl�Bo�Bt�Bw2BzBz�B~�B��B�B��B�B��B��B �B�B��B��B[B2-B:DB>�B7�B�B%B[B�B�wB�$B�B��B�8B�nB޸B�?B�mB��B�4BfLBDB#:BB
��B
�_B
y�B
jB
ZQB
IRB
;JB
$�B
�B
 �B	��B	̳B	�1B	�?B	��B	��B	��B	�sB	��B	��B	x�B	[=B	@4B	)yB	$&B	B	B	B	�B	yB	%zB	'B	.cB	9�B	?cB	EB	J	B	S@B	YB	a-B	i�B	p�B	p�B	r|B	o�B	yXB	��B	�kB	�cB	�gB	ևB	ߤB	�B	��B	�]B	��B	�)B	�B
EB
�B

�B
�B
�B
YB
�B
?B
�B
%FB
*�B
-)B
(�B
$�B
 \B
�B
dB
�B
�B
�B
EB
DB
&B
�B
�B
B
�B
�B
�B
�B

�B
1B
�B
 iB	�B	�fB	�zB	��B	��B	�FB
 �B
	�B
B
<B
&B
�B
�B
�B
&B
�B
}B
�B
dB
DB
JB
	lB
�B
B
B	�B	�cB
B
 �B	�.B	��B	�wB	��B	��B	��B	�B
�B
�B
�B
[B
 B	��B	��B	��B	�B
�B
�B
pB
�B

�B
B
�B
jB
�B
�B
1B
�B
�B
 �B
 \B
!bB
"�B
 �B
�B
 BB
B
�B
kB
eB
�B
+B
�B

B
,B
NB
�B
"B
B
�B
\B
"B
B
�B
B
mB
B
 �B	�(B	��B	�xB	�	B	��B	��B	�B	�sB	��B	��B	��B	�B	ބB	��B	�B	�/B	�!B	��B	�B	��B	�2B	�2B	�LB	�B	�B	�B	��B	��B	�B	�8B	�B	��B	�B	��B	�`B	�ZB	�B	�4B	�B	�B	�B	��B	��B	�B	��B	�B	�ZB	��B	�0B	�B	�B	�2B	�RB	�
B	��B	�DB	�0B	�B	�B	�B	�"B	�B	��B	�6B	��B	�CB	�]B	�B	��B	�;B	�B	�UB	�UB	�oB	�;B	�!B	��B	��B	�B	�-B	�B	�B	��B	�B	��B	�B	�hB	��B	��B	��B	�B	�MB	��B	�|B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�jB	�PB	��B	�qB	��B	�B	�B	�XB	��B	��B	��B	��B	�B	�+B	��B	��B	�B	��B	��B	�cB	��B	��B	�HB	��B
 iB
 �B
AB
{B
�B
MB
3B
B
�B
�B
�B
�B
�B
GB
�B
GB
GB
{B
�B
�B
{B
{B
B
[B
 B
 �B
 �B
 OB
 �B
 4B
 �B
 �B
 �B
 �B
 B
 �B
 �B
 �B
�B
AB
�B
GB
�B
�B
�B
�B
�B
�B
�B
�B
KB
�B
	B
	�B
	�B
	�B
	�B
dB
jB
PB
�B
�B
B
pB
B
}B
hB
hB
�B
�B
�B
�B
�B
�B
TB
�B
�B
�B
�B
B
B
�B
�B
2B
�B
�B
1B
1B
�B
B
]B
xB
�B
IB
�B
�B
�B
�B
�B
�B
~B
jB
;B
�B
 'B
 �B
!�B
"B
!�B
!�B
!�B
"�B
#B
#:B
#�B
#�B
#�B
%B
%zB
%zB
%�B
&LB
&fB
&fB
&�B
'B
($B
(>B
(sB
(�B
(�B
)*B
)_B
)�B
)�B
)�B
)�B
)�B
*0B
*�B
*�B
+B
+kB
,"B
,WB
,�B
,�B
-B
-B
-)B
-wB
-�B
.B
/B
/OB
/�B
/�B
/�B
0;B
0;B
0;B
0�B
0;B
1vB
1vB
1�B
2|B
33B
3MB
4B
4B
3�B
49B
4�B
4�B
4�B
4nB
49B
4nB
5ZB
5%B
5?B
5ZB
5?B
5ZB
5B
5tB
5�B
5�B
5�B
6B
5�B
6FB
6FB
6�B
7B
7LB
7�B
8B
88B
8RB
8�B
8lB
8�B
8�B
9$B
9�B
9�B
9�B
:*B
:�B
:�B
:�B
:�B
:�B
;B
;B
;�B
<B
<B
<B
<6B
<jB
<�B
="B
=B
=B
=<B
=qB
=�B
=�B
=�B
>BB
>(B
>BB
>wB
>�B
>�B
?�B
?�B
@ B
@�B
@�B
@�B
AoB
AUB
A�B
A�B
B�B
B�B
B�B
B�B
C-B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
EB
D�B
E9B
E9B
E�B
F%B
F�B
GEB
G+B
G+B
G+B
F�B
GB
G+B
G�B
G�B
G�B
HKB
H�B
IB
IB
IlB
I�B
I�B
J=B
JrB
J�B
J�B
J�B
J�B
KDB
KDB
K�B
K�B
K�B
K�B
K�B
L0B
LdB
L~B
L�B
L�B
L�B
L�B
L�B
M6B
MPB
MPB
M6B
M6B
M�B
M�B
NVB
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N<B
N"B
NVB
N"B
NB
NVB
N<B
N<B
N<B
N�B
N�B
NpB
N�B
N�B
OBB
OvB
O�B
P�B
P}B
PbB
QB
QNB
Q�B
RoB
R�B
R�B
SB
S[B
S[B
S�B
T�B
UMB
UB
U�B
U�B
U�B
U�B
VB
VB
V9B
VSB
VSB
V�B
W
B
W$B
WsB
W�B
X+B
X�B
X�B
X�B
X�B
Y�B
Z�B
Z�B
Z�B
[	B
Z�B
[	B
[#B
\B
\CB
\�B
]dB
]~B
^B
^B
^5B
_B
_VB
_�B
_�B
`vB
`vB
`�B
aB
a|B
a�B
a�B
a�B
b4B
bhB
b�B
b�B
b�B
c B
cTB
c�B
c�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e�B
fB
f2B
ffB
ffB
f�B
f�B
gB
gmB
g�B
g�B
g�B
g�B
h
B
h
B
hXB
hsB
h�B
hXB
h�B
iB
i*B
i_B
i_B
i�B
i�B
jB
jKB
jKB
j�B
j�B
j�B
k6B
kkB
k�B
k�B
l=B
l�B
l�B
l�B
m]B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
oB
o B
oB
oiB
o�B
o�B
o�B
o�B
pB
p!B
pUB
p�B
p�B
p�B
p�B
p�B
qvB
rGB
raB
raB
r�B
sB
shB
s�B
s�B
s�B
s�B
s�B
s�B
tB
tTB
t�B
uB
uZB
uZB
utB
utB
utB
u�B
u�B
vFB
v�B
v�B
v�B
v�B
v�B
v�B
wLB
wLB
w�B
w�B
w�B
w�B
w�B
x8B
x�B
x�B
x�B
y$B
yrB
y�B
zB
zB
z^B
z�B
z�B
{B
{0B
{B
{JB
{JB
{�B
{�B
{�B
{�B
{�B
|jB
|�B
|�B
}B
|�B
}B
}"B
}VB
~B
~BB
~]B
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
cB
}B
�B
�4B
�4B
�4B
�4B
��B
��B
��B
�B
�oB
�oB
��B
��B
�'B
�AB
��B
��B
��B
��B
�-B
�aB
��B
��B
��B
�3B
��B
��B
��B
��B
�9B
�9B
�SB
�mB
�mB
��B
��B
��B
��B
��B
��B
�%111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	��B	��B	�B	�jB	��B	�@B	��B	��B
?�B
�	B
�}B
��B
�B
B
߾B
��B%B�B&�BGEBNVB_�Bf�BkQBl�BmCBm�Bn/BncBn�Bp�Bo�Bq�BsMBrBoBk�Bl�Bo�Bt�Bw2BzBz�B~�B��B�B��B�B��B��B �B�B��B��B[B2-B:DB>�B7�B�B%B[B�B�wB�$B�B��B�8B�nB޸B�?B�mB��B�4BfLBDB#:BB
��B
�_B
y�B
jB
ZQB
IRB
;JB
$�B
�B
 �B	��B	̳B	�1B	�?B	��B	��B	��B	�sB	��B	��B	x�B	[=B	@4B	)yB	$&B	B	B	B	�B	yB	%zB	'B	.cB	9�B	?cB	EB	J	B	S@B	YB	a-B	i�B	p�B	p�B	r|B	o�B	yXB	��B	�kB	�cB	�gB	ևB	ߤB	�B	��B	�]B	��B	�)B	�B
EB
�B

�B
�B
�B
YB
�B
?B
�B
%FB
*�B
-)B
(�B
$�B
 \B
�B
dB
�B
�B
�B
EB
DB
&B
�B
�B
B
�B
�B
�B
�B

�B
1B
�B
 iB	�B	�fB	�zB	��B	��B	�FB
 �B
	�B
B
<B
&B
�B
�B
�B
&B
�B
}B
�B
dB
DB
JB
	lB
�B
B
B	�B	�cB
B
 �B	�.B	��B	�wB	��B	��B	��B	�B
�B
�B
�B
[B
 B	��B	��B	��B	�B
�B
�B
pB
�B

�B
B
�B
jB
�B
�B
1B
�B
�B
 �B
 \B
!bB
"�B
 �B
�B
 BB
B
�B
kB
eB
�B
+B
�B

B
,B
NB
�B
"B
B
�B
\B
"B
B
�B
B
mB
B
 �B	�(B	��B	�xB	�	B	��B	��B	�B	�sB	��B	��B	��B	�B	ބB	��B	�B	�/B	�!B	��B	�B	��B	�2B	�2B	�LB	�B	�B	�B	��B	��B	�B	�8B	�B	��B	�B	��B	�`B	�ZB	�B	�4B	�B	�B	�B	��B	��B	�B	��B	�B	�ZB	��B	�0B	�B	�B	�2B	�RB	�
B	��B	�DB	�0B	�B	�B	�B	�"B	�B	��B	�6B	��B	�CB	�]B	�B	��B	�;B	�B	�UB	�UB	�oB	�;B	�!B	��B	��B	�B	�-B	�B	�B	��B	�B	��B	�B	�hB	��B	��B	��B	�B	�MB	��B	�|B	�B	��B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	�B	�jB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�jB	�PB	��B	�qB	��B	�B	�B	�XB	��B	��B	��B	��B	�B	�+B	��B	��B	�B	��B	��B	�cB	��B	��B	�HB	��B
 iB
 �B
AB
{B
�B
MB
3B
B
�B
�B
�B
�B
�B
GB
�B
GB
GB
{B
�B
�B
{B
{B
B
[B
 B
 �B
 �B
 OB
 �B
 4B
 �B
 �B
 �B
 �B
 B
 �B
 �B
 �B
�B
AB
�B
GB
�B
�B
�B
�B
�B
�B
�B
�B
KB
�B
	B
	�B
	�B
	�B
	�B
dB
jB
PB
�B
�B
B
pB
B
}B
hB
hB
�B
�B
�B
�B
�B
�B
TB
�B
�B
�B
�B
B
B
�B
�B
2B
�B
�B
1B
1B
�B
B
]B
xB
�B
IB
�B
�B
�B
�B
�B
�B
~B
jB
;B
�B
 'B
 �B
!�B
"B
!�B
!�B
!�B
"�B
#B
#:B
#�B
#�B
#�B
%B
%zB
%zB
%�B
&LB
&fB
&fB
&�B
'B
($B
(>B
(sB
(�B
(�B
)*B
)_B
)�B
)�B
)�B
)�B
)�B
*0B
*�B
*�B
+B
+kB
,"B
,WB
,�B
,�B
-B
-B
-)B
-wB
-�B
.B
/B
/OB
/�B
/�B
/�B
0;B
0;B
0;B
0�B
0;B
1vB
1vB
1�B
2|B
33B
3MB
4B
4B
3�B
49B
4�B
4�B
4�B
4nB
49B
4nB
5ZB
5%B
5?B
5ZB
5?B
5ZB
5B
5tB
5�B
5�B
5�B
6B
5�B
6FB
6FB
6�B
7B
7LB
7�B
8B
88B
8RB
8�B
8lB
8�B
8�B
9$B
9�B
9�B
9�B
:*B
:�B
:�B
:�B
:�B
:�B
;B
;B
;�B
<B
<B
<B
<6B
<jB
<�B
="B
=B
=B
=<B
=qB
=�B
=�B
=�B
>BB
>(B
>BB
>wB
>�B
>�B
?�B
?�B
@ B
@�B
@�B
@�B
AoB
AUB
A�B
A�B
B�B
B�B
B�B
B�B
C-B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
D�B
EB
D�B
E9B
E9B
E�B
F%B
F�B
GEB
G+B
G+B
G+B
F�B
GB
G+B
G�B
G�B
G�B
HKB
H�B
IB
IB
IlB
I�B
I�B
J=B
JrB
J�B
J�B
J�B
J�B
KDB
KDB
K�B
K�B
K�B
K�B
K�B
L0B
LdB
L~B
L�B
L�B
L�B
L�B
L�B
M6B
MPB
MPB
M6B
M6B
M�B
M�B
NVB
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N<B
N"B
NVB
N"B
NB
NVB
N<B
N<B
N<B
N�B
N�B
NpB
N�B
N�B
OBB
OvB
O�B
P�B
P}B
PbB
QB
QNB
Q�B
RoB
R�B
R�B
SB
S[B
S[B
S�B
T�B
UMB
UB
U�B
U�B
U�B
U�B
VB
VB
V9B
VSB
VSB
V�B
W
B
W$B
WsB
W�B
X+B
X�B
X�B
X�B
X�B
Y�B
Z�B
Z�B
Z�B
[	B
Z�B
[	B
[#B
\B
\CB
\�B
]dB
]~B
^B
^B
^5B
_B
_VB
_�B
_�B
`vB
`vB
`�B
aB
a|B
a�B
a�B
a�B
b4B
bhB
b�B
b�B
b�B
c B
cTB
c�B
c�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e�B
fB
f2B
ffB
ffB
f�B
f�B
gB
gmB
g�B
g�B
g�B
g�B
h
B
h
B
hXB
hsB
h�B
hXB
h�B
iB
i*B
i_B
i_B
i�B
i�B
jB
jKB
jKB
j�B
j�B
j�B
k6B
kkB
k�B
k�B
l=B
l�B
l�B
l�B
m]B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
n�B
oB
o B
oB
oiB
o�B
o�B
o�B
o�B
pB
p!B
pUB
p�B
p�B
p�B
p�B
p�B
qvB
rGB
raB
raB
r�B
sB
shB
s�B
s�B
s�B
s�B
s�B
s�B
tB
tTB
t�B
uB
uZB
uZB
utB
utB
utB
u�B
u�B
vFB
v�B
v�B
v�B
v�B
v�B
v�B
wLB
wLB
w�B
w�B
w�B
w�B
w�B
x8B
x�B
x�B
x�B
y$B
yrB
y�B
zB
zB
z^B
z�B
z�B
{B
{0B
{B
{JB
{JB
{�B
{�B
{�B
{�B
{�B
|jB
|�B
|�B
}B
|�B
}B
}"B
}VB
~B
~BB
~]B
~wB
~�B
~�B
~�B
~�B
~�B
~�B
~�B
cB
}B
�B
�4B
�4B
�4B
�4B
��B
��B
��B
�B
�oB
�oB
��B
��B
�'B
�AB
��B
��B
��B
��B
�-B
�aB
��B
��B
��B
�3B
��B
��B
��B
��B
�9B
�9B
�SB
�mB
�mB
��B
��B
��B
��B
��B
��B
�%111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            JA  ARFMdecpA30a                                                                20220604104927  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20220604174134  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20220604174134  IP  PRES            G�O�G�O�G�O�                JA      jafc1.0                                                                 20220604174134                      G�O�G�O�G�O�                JA  ARGQrqcpc3.6                                                                20220605024141  QCP$                G�O�G�O�G�O�         20DF37EJA  ARGQrqcpc3.6                                                                20220605024141  QCF$                G�O�G�O�G�O�               0JA  ARUP                                                                        20220610131508                      G�O�G�O�G�O�                