CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       JMA    source        
Argo float     history       b2018-09-16T21:35:14Z creation;2018-09-16T21:35:18Z conversion to V3.1;2019-12-19T07:28:42Z update;     
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
_FillValue                 �  ID   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M0   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  `�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  pl   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     
_FillValue        G�O�   	valid_min         �      	valid_max         B      C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  tX   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    
_FillValue        G�O�   	valid_min         @      	valid_max         B$     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %9.3f      FORTRAN_format        F9.3   
resolution        :�o     �  �d   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  �  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                 	   ޜ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                 	   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                 	   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  �  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �    HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �$   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �(   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �l   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �|   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180916213514  20200116231517  5905048                                                                 JAMSTEC                                                         PRES            TEMP            PSAL              A   JA  I2_0577_282                     2C  D   NAVIS_A                         0577                            ARGO 102115                     863 @؁�W� 1   @؁���J @4�� ѷ�d[�Y��}1   GPS     A   B   B   Primary sampling: averaged [bin average for 1 Hz CTD]                                                                                                                                                                                                              @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  BpffBx��B~ffB�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dm��Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�<�D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D��3D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D���D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��H@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo��Bx\)B}��B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm�Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�9HD�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D修D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D��D��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��PA��PA��uA��uA��\A��hA��PA��\A��hA��hA��uA��uA��uA���A��uA��hA��uA���A���A���A���A���A���A���A���A���A���A���A���A���A�K�A�9XA�dZAѥ�AϬA�M�A���A�A�VA��
Aư!A�p�A� �A��`A�r�A���A�7LA�ȴA�A�A��7A��A���A�?}A�VA��A�?}A��A��-A��A��A�1'A��A��uA�;dA�A�/A�z�A�?}A�/A��jA�p�A�jA�oA��+A�A�&�A��yA��9A��A��^A�I�A�r�A���A�;dA��DA�dZA�r�A�
=A���A���A��+A��A���A��A���A���A�~�A��A���A�ZA���A���A�l�A�ffA�G�A�ȴA�A���A�\)A��^A�p�A�  A�K�A�?}A�oA��A�(�A���A��;A���A��mA�
=A��hA{�PAvȴAuVAr�uAi�AdbAaA_+A\v�AZE�AY�hAWXAT�`AS"�AQdZAP$�AL��AKƨAK\)AH^5AE�AC��AAO�A?�PA=��A=%A<r�A;oA7�wA5�;A5`BA5VA3��A1��A0�A0  A/�PA/XA/G�A/?}A/C�A/;dA-�A+�A*VA)"�A(bNA'�A'G�A&ffA$(�A"�/A!�;A ��A��A��A��AC�Ar�A
=A|�A�A�AJA�A�A;dA��A��A�#A&�AbNA�PA��Az�A�A33AVAp�A��A��AS�A
��A
bA	��A	dZA	�A��A��AA&�A�!A5?A|�A�Ar�A-A�#AhsA�\A��A%A I�@���@��h@���@�ƨ@�dZ@��!@�J@��@�hs@��@�I�@��@�5?@��@�\)@�x�@�@�  @��@��
@ꟾ@�r�@�`B@��@��/@އ+@��/@ە�@�M�@��@�Q�@ָR@Ցh@�1'@�dZ@�@ҧ�@ӕ�@ӶF@Ӿw@ӥ�@��@��#@�O�@�
=@Ο�@�@�{@��@��@���@�M�@͉7@�j@ʟ�@��@�hs@ȣ�@�Q�@Ǯ@�E�@�hs@�33@§�@�n�@¸R@���@� �@�S�@�
=@�C�@�@��!@��@���@�n�@���@�/@��@�o@�"�@�-@��@�j@�Q�@��9@��@��@�%@��D@��@��
@��F@�33@�x�@���@���@��P@�M�@���@�/@��j@���@�\)@��y@��\@�E�@�=q@�M�@��T@�V@�j@��@��P@�+@��!@�n�@���@���@�M�@�M�@��@��h@��@��@�ƨ@�t�@�;d@�o@��y@��@�ȴ@��+@�$�@���@�p�@��@���@�1@��@��@�dZ@�S�@�C�@��H@���@�ff@�5?@�{@��#@��-@�`B@�%@��u@�bN@�b@��w@���@�
=@�@���@�ȴ@��!@�^5@�@��@��^@��h@��@��`@��@��u@��D@�j@�(�@�  @���@���@�
=@��R@�^5@�E�@�=q@�-@���@���@��7@�x�@�O�@���@��/@��j@��@�1'@�b@��@��P@�dZ@�dZ@�;d@�+@�
=@���@���@�~�@�{@���@���@���@��-@�p�@�X@��@��j@�j@��u@���@���@�Z@� �@�b@��
@���@��P@��P@�|�@�l�@�33@��@�ȴ@��!@�ff@�M�@�E�@�=q@�-@�-@�5?@��@�J@�@��@��#@��^@��7@�V@���@���@�z�@�Q�@�A�@��@���@��
@��P@�\)@�;d@�ff@�J@�x�@�7L@��D@���@�j@���@�C�@��@��m@��
@�l�@��+@�M�@�E�@�-@��@��T@�5?@�-@�{@��@�@���@���@�9X@��P@�l�@�\)@�K�@�o@���@�n�@�{@��T@�@�x�@�hs@�X@�?}@��@���@���@��9@���@�z�@�9X@�Q�@�bN@�(�@�(�@� �@�P@l�@l�@K�@~�y@~ff@~E�@}�@}p�@}�@}�-@}V@|�@|z�@|z�@|Z@|9X@|�@{��@z��@zM�@z=q@y��@x�`@x1'@w�P@w|�@w;d@w�@w
=@v�y@v�@v�R@v��@v�+@vE�@t��@tZ@sƨ@s��@sdZ@r�@rn�@q�^@qhs@p�`@pbN@pA�@p  @o�P@n�@nV@n$�@m�T@m��@m?}@l��@l�D@k�F@kS�@i��@i�7@i%@h�u@h �@h  @g�P@g\)@g
=@f�R@fV@f5?@f$�@f{@e�T@e�h@e`B@e`B@e�@dz�@c��@cdZ@b�H@b�\@b=q@a��@a�#@ahs@a%@`��@`��@`A�@_�@_�P@_K�@^��@^�R@^v�@]�@]p�@]O�@]/@\�/@\�@\��@\Z@\�@[ƨ@[t�@["�@[33@[33@[o@Z-@X�`@X1'@W�w@W�P@W|�@W\)@W;d@V�@Vv�@U�-@T��@T�/@T��@T�D@Tj@T9X@T(�@Tj@T(�@S��@S�m@S�F@St�@S33@R�\@R=q@RJ@Qx�@P��@P�`@P��@P��@P�u@PbN@PbN@P1'@Pb@O�w@OK�@N�@Nv�@N{@M�-@M�@L�j@Lz�@L�@K��@K�m@K��@K33@Ko@J��@J��@J�\@J~�@J^5@I��@I&�@H��@Hr�@H1'@H �@H �@H  @G�w@G|�@Gl�@Gl�@Gl�@G\)@GK�@F��@F�R@FV@F{@E�@E�@E��@E��@EO�@EV@D��@D�j@C�m@CC�@Co@B�H@B��@BM�@A�@A��@A��@AX@@��@@Q�@@ �@@  @?�;@?��@?�w@?��@?\)@>ȴ@>5?@>{@>{@=�@=`B@<��@<�j@<��@<�@;�
@;�F@;�@;S�@;@:�@:�@:�H@:�!@:n�@:M�@:M�@:-@9��@9�^@97L@8��@8��@8r�@7�@7�@7|�@7;d@6��@6�@6E�@5�@5�-@5?}@4��@4�j@4Z@41@3��@3ƨ@3��@3dZ@2�H@2�\@2M�@2�@2J@1��@1�@1�#@1�#@1�^@1hs@1�@0�`@0�@0r�@0r�@0Q�@/�@/�;@/�w@/�@/��@/|�@/\)@/
=@.�@.ff@.@-p�@-V@,�@,��@,Z@,I�@,(�@+�
@+t�@+o@*��@*^5@*=q@)�#@)�^@)�^@)��@)�7@)hs@(�`@(Ĝ@(��@(�@(bN@(bN@(1'@(  @'�w@'l�@'\)@'K�@'+@'�@'�@'�@&��@&��@&ff@&5?@&$�@%�@%�@%�@%O�@%�@%V@$��@$��@$��@$Z@$�@$�@$�@#�m@#�F@#��@#t�@#dZ@#C�@#33@#33@#o@#o@"�@"�!@"~�@"M�@"M�@"=q@"M�@"=q@!�@!��@!�^@!��@!��@!x�@!x�@!x�@!X@!&�@ ��@ �9@ Q�@ b@�w@K�@�y@��@E�@{@��@�-@�@��@�D@z�@Z@��@��@��@t�@S�@33@"�@o@�H@��@��@n�@M�@�@��@��@hs@7L@&�@%@��@��@�`@��@bN@1'@b@�@�@�;@��@�w@��@l�@\)@;d@��@ȴ@��@�+@ff@$�@�T@�@`B@`B@`B@?}@/@�@�/@Z@�@�F@33@@@n�@�#@X@7L@&�@�@%@Ĝ@Q�@Q�@1'@  @�@��@l�@l�@K�@;d@+111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��PA��PA��uA��uA��\A��hA��PA��\A��hA��hA��uA��uA��uA���A��uA��hA��uA���A���A���A���A���A���A���A���A���A���A���A���G�O�G�O�A�9XA�dZAѥ�AϬA�M�A���A�A�VA��
Aư!A�p�A� �A��`A�r�A���A�7LA�ȴA�A�A��7A��A���A�?}A�VA��A�?}A��A��-A��A��A�1'A��A��uA�;dA�A�/A�z�A�?}A�/A��jA�p�A�jA�oA��+A�A�&�A��yA��9A��A��^A�I�A�r�A���A�;dA��DA�dZA�r�A�
=A���A���A��+A��A���A��A���A���A�~�A��A���A�ZA���A���A�l�A�ffA�G�A�ȴA�A���A�\)A��^A�p�A�  A�K�A�?}A�oA��A�(�A���A��;A���A��mA�
=A��hA{�PAvȴG�O�G�O�Ai�AdbAaA_+A\v�AZE�AY�hAWXAT�`AS"�AQdZAP$�AL��AKƨAK\)AH^5AE�AC��AAO�A?�PA=��A=%A<r�A;oA7�wA5�;A5`BA5VA3��A1��A0�A0  A/�PA/XA/G�A/?}A/C�A/;dA-�A+�A*VA)"�A(bNA'�A'G�A&ffA$(�A"�/A!�;A ��A��A��A��AC�Ar�A
=A|�A�A�AJA�A�A;dA��A��A�#A&�AbNA�PA��Az�A�A33AVAp�A��A��AS�A
��A
bA	��A	dZA	�A��A��AA&�A�!A5?A|�A�Ar�A-A�#AhsA�\A��A%A I�@���@��h@���@�ƨ@�dZ@��!@�J@��@�hs@��@�I�@��@�5?@��@�\)@�x�@�@�  @��@��
@ꟾ@�r�@�`B@��@��/@އ+@��/@ە�@�M�@��@�Q�@ָR@Ցh@�1'@�dZ@�@ҧ�@ӕ�@ӶF@Ӿw@ӥ�@��@��#@�O�@�
=@Ο�@�@�{@��@��@���@�M�@͉7@�j@ʟ�@��@�hs@ȣ�@�Q�@Ǯ@�E�@�hs@�33@§�@�n�@¸R@���@� �@�S�@�
=@�C�@�@��!@��@���@�n�@���@�/@��@�o@�"�@�-@��@�j@�Q�@��9@��@��@�%@��D@��@��
@��F@�33@�x�@���@���@��P@�M�@���@�/@��j@���@�\)@��y@��\@�E�@�=q@�M�@��T@�V@�j@��@��P@�+@��!@�n�@���@���@�M�@�M�@��@��h@��@��@�ƨ@�t�@�;d@�o@��y@��@�ȴ@��+@�$�@���@�p�@��@���@�1@��@��@�dZ@�S�@�C�@��H@���@�ff@�5?@�{@��#@��-@�`B@�%@��u@�bN@�b@��w@���@�
=@�@���@�ȴ@��!@�^5@�@��@��^@��h@��@��`@��@��u@��D@�j@�(�@�  @���@���@�
=@��R@�^5@�E�@�=q@�-@���@���@��7@�x�@�O�@���@��/@��j@��@�1'@�b@��@��P@�dZ@�dZ@�;d@�+@�
=@���@���@�~�@�{@���@���@���@��-@�p�@�X@��@��j@�j@��u@���@���@�Z@� �@�b@��
@���@��P@��P@�|�@�l�@�33@��@�ȴ@��!@�ff@�M�@�E�@�=q@�-@�-@�5?@��@�J@�@��@��#@��^@��7@�V@���@���@�z�@�Q�@�A�@��@���@��
@��P@�\)@�;d@�ff@�J@�x�@�7L@��D@���@�j@���@�C�@��@��m@��
@�l�@��+@�M�@�E�@�-@��@��T@�5?@�-@�{@��@�@���@���@�9X@��P@�l�@�\)@�K�@�o@���@�n�@�{@��T@�@�x�@�hs@�X@�?}@��@���@���@��9@���@�z�@�9X@�Q�@�bN@�(�@�(�@� �@�P@l�@l�@K�@~�y@~ff@~E�@}�@}p�@}�@}�-@}V@|�@|z�@|z�@|Z@|9X@|�@{��@z��@zM�@z=q@y��@x�`@x1'@w�P@w|�@w;d@w�@w
=@v�y@v�@v�R@v��@v�+@vE�@t��@tZ@sƨ@s��@sdZ@r�@rn�@q�^@qhs@p�`@pbN@pA�@p  @o�P@n�@nV@n$�@m�T@m��@m?}@l��@l�D@k�F@kS�@i��@i�7@i%@h�u@h �@h  @g�P@g\)@g
=@f�R@fV@f5?@f$�@f{@e�T@e�h@e`B@e`B@e�@dz�@c��@cdZ@b�H@b�\@b=q@a��@a�#@ahs@a%@`��@`��@`A�@_�@_�P@_K�@^��@^�R@^v�@]�@]p�@]O�@]/@\�/@\�@\��@\Z@\�@[ƨ@[t�@["�@[33@[33@[o@Z-@X�`@X1'@W�w@W�P@W|�@W\)@W;d@V�@Vv�@U�-@T��@T�/@T��@T�D@Tj@T9X@T(�@Tj@T(�@S��@S�m@S�F@St�@S33@R�\@R=q@RJ@Qx�@P��@P�`@P��@P��@P�u@PbN@PbN@P1'@Pb@O�w@OK�@N�@Nv�@N{@M�-@M�@L�j@Lz�@L�@K��@K�m@K��@K33@Ko@J��@J��@J�\@J~�@J^5@I��@I&�@H��@Hr�@H1'@H �@H �@H  @G�w@G|�@Gl�@Gl�@Gl�@G\)@GK�@F��@F�R@FV@F{@E�@E�@E��@E��@EO�@EV@D��@D�j@C�m@CC�@Co@B�H@B��@BM�@A�@A��@A��@AX@@��@@Q�@@ �@@  @?�;@?��@?�w@?��@?\)@>ȴ@>5?@>{@>{@=�@=`B@<��@<�j@<��@<�@;�
@;�F@;�@;S�@;@:�@:�@:�H@:�!@:n�@:M�@:M�@:-@9��@9�^@97L@8��@8��@8r�@7�@7�@7|�@7;d@6��@6�@6E�@5�@5�-@5?}@4��@4�j@4Z@41@3��@3ƨ@3��@3dZ@2�H@2�\@2M�@2�@2J@1��@1�@1�#@1�#@1�^@1hs@1�@0�`@0�@0r�@0r�@0Q�@/�@/�;@/�w@/�@/��@/|�@/\)@/
=@.�@.ff@.@-p�@-V@,�@,��@,Z@,I�@,(�@+�
@+t�@+o@*��@*^5@*=q@)�#@)�^@)�^@)��@)�7@)hs@(�`@(Ĝ@(��@(�@(bN@(bN@(1'@(  @'�w@'l�@'\)@'K�@'+@'�@'�@'�@&��@&��@&ff@&5?@&$�@%�@%�@%�@%O�@%�@%V@$��@$��@$��@$Z@$�@$�@$�@#�m@#�F@#��@#t�@#dZ@#C�@#33@#33@#o@#o@"�@"�!@"~�@"M�@"M�@"=q@"M�@"=q@!�@!��@!�^@!��@!��@!x�@!x�@!x�@!X@!&�@ ��@ �9@ Q�@ b@�w@K�@�y@��@E�@{@��@�-@�@��@�D@z�@Z@��@��@��@t�@S�@33@"�@o@�H@��@��@n�@M�@�@��@��@hs@7L@&�@%@��@��@�`@��@bN@1'@b@�@�@�;@��@�w@��@l�@\)@;d@��@ȴ@��@�+@ff@$�@�T@�@`B@`B@`B@?}@/@�@�/@Z@�@�F@33@@@n�@�#@X@7L@&�@�@%@Ĝ@Q�@Q�@1'@  @�@��@l�@l�@K�@;d@+111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	��B	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	ǮB	}�B
�B
O�B
�B
�\B
�ZBC�BYBq�B�\B��B��B��B�sBhB �BuBoB-BI�BP�BH�B5?BB��B�BuB)�B.B!�B+B\B�B�BVB%B\BuBVBJB\B%B��B�B��BBB	7B\B�B�B.B/B5?B.B$�B!�B��B�B�3B�B��Bm�Be`Bo�B\)BK�BaHBbNB_;BK�BXB\)BVBE�B33B�B
�BVB{BDB
��BB
��B
�
B
�9B
��B
�uB
� B
M�B
'�B
1B	��B	��B	�B	VB��B��B�dB�RB�FB�3BĜB�3B�B�B�B�3B��B�'B�RB��B��B��B��B�!B�FB��B�}B�9B��B�qB��B��B�wB�B�XB�}B��BǮBȴBȴBĜB�qB��B��B��B��B��B��B��B��B�B�B}�Bu�Bt�Bu�Bs�B{�Bs�Bk�Bl�B|�B�B�B�DB�DB�B{�Bk�Br�Bu�Bz�Bu�B{�Bz�Bt�Bw�Bs�Bp�Bk�B|�B� B{�B� B�B�7B�1B�=B�%B�B~�B�JB�7B�\B��B��B��B��B��B��B��B��B��B��B��B��B�9B�3B�'B�!B�!B�RB��BBÖB�qB�XB�?B�B�LB�FB�B��B��B��B�1B�bB� B{�B�B�B�7B�1B�PB�+B�DB�JB��B��B��B�FB�}B�}B�jB�?B�dB�XB�B�dBÖB��B��B��B��B�NB�B�B��B�NB�mB�yB�yB�fB�)B�BB�B�HB�mB�B�mB�HB�B�B�B�B�B�B�B�B��B��B�B�B��B��B��B��B	%B	DB	uB	hB	VB	bB	bB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	 �B	�B	"�B	$�B	&�B	)�B	.B	1'B	0!B	1'B	33B	9XB	;dB	?}B	C�B	G�B	I�B	O�B	VB	YB	XB	YB	W
B	XB	\)B	ffB	jB	n�B	o�B	t�B	u�B	v�B	x�B	{�B	{�B	� B	�B	�B	�DB	�VB	�bB	�hB	�hB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�!B	�'B	�!B	�3B	�LB	�LB	�LB	�FB	�dB	�qB	�wB	�wB	�wB	�qB	�}B	�}B	�wB	�jB	��B	��B	ĜB	ŢB	ĜB	ÖB	ƨB	ɺB	ȴB	ȴB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�)B	�HB	�ZB	�ZB	�TB	�ZB	�fB	�`B	�`B	�sB	�sB	�mB	�mB	�fB	�fB	�sB	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B
B
  B	��B	��B	��B	��B	��B	��B	��B
B
B
B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
%B
	7B
DB
1B
DB
DB

=B
JB
VB
VB
VB
VB
\B
\B
\B
hB
{B
oB
oB
�B
�B
�B
�B
�B
uB
oB
�B
�B
{B
oB
oB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
{B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
 �B
 �B
 �B
"�B
!�B
"�B
!�B
"�B
"�B
$�B
$�B
$�B
#�B
#�B
$�B
%�B
#�B
!�B
"�B
#�B
$�B
%�B
&�B
'�B
'�B
&�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
)�B
)�B
(�B
)�B
,B
,B
,B
-B
.B
-B
-B
-B
-B
.B
0!B
/B
-B
)�B
)�B
.B
0!B
33B
33B
33B
2-B
1'B
0!B
/B
/B
2-B
2-B
49B
5?B
5?B
6FB
8RB
7LB
7LB
9XB
7LB
8RB
8RB
7LB
8RB
:^B
9XB
9XB
>wB
=qB
=qB
<jB
=qB
=qB
=qB
=qB
<jB
;dB
<jB
<jB
<jB
<jB
<jB
>wB
>wB
>wB
@�B
@�B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
@�B
?}B
@�B
B�B
A�B
B�B
D�B
D�B
C�B
C�B
C�B
D�B
E�B
D�B
D�B
D�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
D�B
D�B
D�B
E�B
D�B
B�B
D�B
G�B
F�B
F�B
F�B
G�B
H�B
H�B
G�B
G�B
F�B
I�B
J�B
J�B
J�B
I�B
I�B
H�B
G�B
H�B
J�B
K�B
J�B
H�B
H�B
K�B
K�B
I�B
K�B
L�B
L�B
L�B
L�B
M�B
N�B
M�B
M�B
M�B
N�B
N�B
N�B
M�B
M�B
L�B
N�B
N�B
O�B
N�B
O�B
P�B
P�B
P�B
P�B
O�B
P�B
Q�B
P�B
Q�B
Q�B
Q�B
R�B
S�B
S�B
S�B
R�B
R�B
S�B
T�B
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
VB
VB
T�B
W
B
VB
XB
XB
W
B
W
B
XB
XB
XB
XB
XB
W
B
W
B
W
B
W
B
VB
W
B
XB
YB
ZB
ZB
ZB
ZB
YB
YB
ZB
ZB
[#B
\)B
[#B
]/B
^5B
]/B
]/B
]/B
\)B
^5B
^5B
_;B
_;B
_;B
^5B
^5B
^5B
^5B
`BB
`BB
`BB
`BB
`BB
`BB
_;B
^5B
_;B
_;B
`BB
_;B
_;B
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
cTB
cTB
bNB
bNB
bNB
cTB
cTB
cTB
dZB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
e`B
dZB
dZB
dZB
cTB
dZB
e`B
e`B
e`B
e`B
e`B
e`B
dZB
dZB
dZB
dZB
cTB
dZB
dZB
dZB
dZB
e`B
ffB
ffB
ffB
ffB
e`B
gmB
hsB
iyB
iyB
hsB
iyB
jB
jB
jB
jB
k�B
k�B
jB
jB
jB
jB
k�B
jB
k�B
jB
k�B
l�B
l�B
l�B
m�B
m�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
m�B
m�B
m�B
n�B
m�B
m�B
n�B
n�B
n�B
n�B
m�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
n�B
o�B
o�B
o�B
q�B
q�B
o�B
o�B
p�B
s�B
s�B
s�B
s�B
r�B
r�B
t�B
t�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
v�111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�JB	̈́G�O�G�O�B
&B
W
B
�B
��B
��BD�BZ�Br�B��B��B�>B�B�QBaB#B?B�B0!BJ�BQ�BJ	B8BKB��B�AB�B*KB.}B#:B
�BhB�BxBB�B�B�BBB�B�BB�<B�|B�B{B{B	lB�B�B�B0UB1�B6�B0�B&�B#nB �B�jB��B��B�eBq�Bh
Bp�B^�BNVBb�Bc�B`�BN"BX�B\xBV�BGEB5�B B
�9B�B2B~B
�}B�B
��B
�7B
�RB
��B
�SB
�B
RoB
,�B
�B	�&B	�	B	��G�O�G�O�B�RB��B�B�rB��B��B�+B��B�]B�iB�%B��B��B��B��B��B��B�B�aB�8B��B��B��B�!B�cB�~B̳B�iB��B��B�OB� B��B��B��B��B�(B�WB�'B��B�B��B��B��B�EB�B��BcBwLBv`Bw2BuB|�Bu%Bm�Bn�B}�B��B��B�xB��B�B}<Bm�BtBv�B|Bv�B|�B{�Bu�Bx�BuBrBmwB}�B��B|�B��B��B��B��B��B��B�3B�OB��B�=B�}B�B�\B�ZB��B��B�B� B��B��B�B�FB��B�nB��B��B��B��B�lB�'B�B�3B��B�xB�zB�iB�B�B��B�B�B�VB�#B��B�AB}�B�MB�%B�#B�B�B�KB�0B�jB�$B�B�B��B�}B��B��B�FB��B�B�wB��B�B��B�B�@B�,B�B�	B�B�MB��B��B�B��B�B�IB�B�YB��B�B�B�XB�NB�B��B��B��B�)B��B�B�B�LB�tB�nB��B�	B��B��B�]B	?B	)B	&B	�B	�B	�B	�B	�B	�B	7B	�B	B	B	sB	SB	5B	VB	!-B	 \B	#TB	%FB	'8B	*0B	.IB	1[B	0�B	1�B	3�B	9�B	;�B	?�B	C�B	G�B	J#B	O�B	U�B	Y1B	X_B	YB	W�B	X�B	\�B	f�B	j�B	n�B	o�B	t�B	vB	w2B	y>B	|B	|jB	��B	�{B	��B	�xB	��B	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�B	�@B	�FB	�8B	�LB	�B	�]B	�;B	�[B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	żB	ĶB	��B	��B	ɺB	��B	�B	�B	��B	��B	�B	�B	�B	�"B	�<B	�B	��B	� B	�B	� B	�.B	�:B	�2B	�aB	�9B	�B	�7B	�7B	�eB	�QB	�KB	�yB	�]B	�-B	�tB	�tB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	�B	��B	�B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	�B	�B	��B	��B	�B	��B	�B	�B	��B	��B	��B	��B	�)B	��B	�B	��B	��B	�B	�!B	�B	��B	��B	��B	�*B	�2B	�MB	��B	��B	��B	��B	��B
 �B
 B	�B	�.B	�"B	��B	�6B	�PB	�PB
 B
AB
 B	�.B	�VB	�B	�BB
 4B
 B
;B
-B
GB
'B
GB
'B
GB
MB
9B
9B
tB
	7B
DB
�B
^B
xB

�B
~B
pB
pB
�B
�B
�B
vB
�B
hB
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
B
�B
=B
�B
 �B
 �B
!B
"�B
"B
#B
!�B
"�B
# B
$�B
$�B
$�B
$B
$B
$�B
%�B
$B
"4B
#B
$&B
%B
%�B
'B
($B
(
B
'B
($B
)*B
)B
)*B
)*B
)*B
*B
*B
*0B
*0B
)DB
*0B
,=B
,=B
,"B
-)B
.IB
-CB
-CB
-)B
-)B
./B
0!B
/5B
-]B
*eB
*�B
.IB
0oB
3hB
3hB
3MB
2aB
1vB
0UB
/iB
/iB
2aB
2aB
4TB
5tB
5ZB
6`B
8lB
7fB
7fB
9XB
7�B
8lB
8�B
7�B
8lB
:xB
9�B
9�B
>wB
=qB
=�B
<�B
=�B
=qB
=�B
=�B
<�B
;�B
<�B
<�B
<�B
<�B
<�B
>�B
>�B
>�B
@�B
@�B
?�B
?�B
@�B
@�B
A�B
A�B
A�B
@�B
?�B
@�B
B�B
A�B
B�B
D�B
D�B
C�B
C�B
C�B
D�B
E�B
D�B
D�B
D�B
C�B
C�B
C�B
D�B
E�B
E�B
E�B
D�B
D�B
D�B
E�B
D�B
B�B
D�B
G�B
F�B
F�B
F�B
G�B
H�B
H�B
G�B
G�B
F�B
I�B
J�B
J�B
J�B
I�B
I�B
H�B
G�B
H�B
J�B
K�B
J�B
H�B
IB
K�B
K�B
I�B
K�B
MB
L�B
L�B
L�B
M�B
N�B
NB
M�B
M�B
N�B
N�B
OB
M�B
M�B
MB
OB
OB
PB
OB
PB
Q B
Q B
QB
QB
PB
QB
R B
QB
R B
R B
R B
SB
TB
TB
TB
S&B
S@B
TB
UB
V9B
W$B
W
B
W$B
W
B
W$B
V9B
VB
U2B
W?B
V9B
XB
X+B
W?B
W?B
X+B
XEB
X+B
X+B
XEB
W$B
W?B
W$B
W?B
VSB
WYB
XEB
Y1B
ZB
ZQB
Z7B
ZQB
YeB
YKB
Z7B
Z7B
[WB
\]B
[=B
]IB
^5B
]IB
]IB
]dB
\xB
^OB
^OB
_VB
_pB
_VB
^jB
^jB
^jB
^jB
`\B
`BB
`vB
`BB
`\B
`\B
_pB
^OB
_pB
_VB
`BB
_VB
_VB
abB
abB
a|B
abB
abB
a|B
a|B
a|B
a|B
cTB
cTB
b�B
bhB
bhB
cTB
cnB
cnB
dZB
cTB
c�B
cTB
cnB
cnB
cnB
cnB
ezB
dtB
dtB
dtB
c�B
d�B
ezB
e�B
ezB
ezB
e`B
e`B
dtB
d�B
d�B
dtB
c�B
d�B
d�B
d�B
d�B
ezB
f�B
f�B
f�B
f�B
e�B
g�B
h�B
i�B
i�B
h�B
i�B
jB
j�B
j�B
j�B
k�B
k�B
j�B
j�B
j�B
j�B
k�B
j�B
k�B
j�B
k�B
l�B
l�B
l�B
m�B
m�B
l�B
l�B
l�B
l�B
m�B
m�B
n�B
n�B
n�B
m�B
m�B
m�B
n�B
m�B
m�B
n�B
n�B
n�B
n�B
m�B
n�B
n�B
o�B
p�B
p�B
p�B
p�B
o�B
o�B
n�B
o�B
o�B
o�B
q�B
q�B
o�B
o�B
p�B
s�B
s�B
s�B
s�B
r�B
r�B
t�B
t�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
v�111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�G�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES            TEMP            PSAL            PRES_ADJ=PRES-SP,  where SP is SURFACE PRESSURE from next cycle; PRES_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                      TEMP_ADJ=TEMP; TEMP_ADJ_ERR=Manufacturer sensor accuracy                                                                                                                                                                                                        PSAL_ADJ = RecalS= psal(PRES_ADJ,TEMP,Conductivity)                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ = celltm_sbe41(RecalS,TEMP,P,elapsed_time,alpha,tau); elapsed_time=P/mean_rise_rate; P=dbar since the start of the profile for each samples                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            PSAL_ADJ_ERR=max(RecalS & CTM error, 0.01(PSS-78))                                                                                                                                                                                                              SP=0.11(dbar)                                                                                                                                                                                                                                                   None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            alpha=0.0267C, tau=18.6s, mean_rise_rate = 0.09 dbar/second                                                                                                                                                                                                     None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Pressure Correction using reported SURFACE PRESSURE                                                                                                                                                                                                             None                                                                                                                                                                                                                                                            Salinity Recalculation using PRES_ADJ                                                                                                                                                                                                                           None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            Correction for conductivity cell thermal mass error(CTM), Johnson et al., 2007, JAOT                                                                                                                                                                            None                                                                                                                                                                                                                                                            None                                                                                                                                                                                                                                                            No adjustment is needed                                                                                                                                                                                                                                         201809210035322018092100353220180921003532201809210200252018092102002520180921020025201809220029272018092200292720180922002927  JA  ARFMdecpA19c                                                                20180917063513  IP                  G�O�G�O�G�O�                JA  ARFMfmtp2.8                                                                 20180916213514  IP                  G�O�G�O�G�O�                JA  ARCAsspa3.1b                                                                20180916213517  IP  PRES            G�O�G�O�G�O�                JA  ARGQrqcppo_c                                                                20180916213517  QCP$                G�O�G�O�G�O�              3CJA  ARGQrqcpt19d                                                                20180916213518  QCP$                G�O�G�O�G�O�           80000JA  ARGQaqcpt19d                                                                20180916213518  QCP$                G�O�G�O�G�O�           80000JA  ARGQrqcp2.8e                                                                20180916213518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQrqcp2.8e                                                                20180916213518  QCF$                G�O�G�O�G�O�            4000JA  ARGQaqcp2.8e                                                                20180916213518  QCP$                G�O�G�O�G�O�            FB40JA  ARGQaqcp2.8e                                                                20180916213518  QCF$                G�O�G�O�G�O�            4000JA  ARGQrqcpt16c                                                                20180916213518  QCP$                G�O�G�O�G�O�           10000JA      jafc1.0                                                                 20180916213518                      G�O�G�O�G�O�                JA  ARUP                                                                        20180916215558                      G�O�G�O�G�O�                JM  ARGQJMQC2.0                                                                 20180917153530  CV  JULD            G�O�G�O�F��                JM  ARCAJMQC2.0                                                                 20180920153532  CV  PRES_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMQC2.0                                                                 20180920153532  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARCAJMTM1.0                                                                 20180920170025  CV  PSAL_ADJUSTED   G�O�G�O�G�O�                JM  ARSQOW  1.1 2017V1                                                          20180921152927  IP  PSAL_ADJUSTED   G�O�G�O�G�O�                JA  ARDU                                                                        20200116231517                      G�O�G�O�G�O�                