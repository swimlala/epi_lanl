CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-02-13T10:01:12Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7(   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7h   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     7�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8,   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           80   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    88   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8<   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8D   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8L   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8T   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8X   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8`   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9`   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9d   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9h   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9l   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �L   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �8   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20220213100112  20220213100112  4903032 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7212                            2B  A   NAVIS_A                         0942                            170425                          863 @ٹT_bDm1   @ٹU	{L�@<Y������c�^5?|�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   F   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�3D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�3D�C3D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D���D�@ D� D�� D�  D�@ D�|�D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�<�D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�{@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_�\Bg�\Bo�\Bw�\B�\B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�yHD��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D���D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D���D�?�D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��HD�<{D�|{D�{D��{D�<{D�yHD�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�9HD�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D��D��H111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A� �A�"�A�"�A�$�A�&�A�&�A�(�A�(�A�(�A�+A�(�A�+A�+A�-A�/A�/A�33A�33A�5?A�5?A�5?A�5?A�1'A�-A�(�A�-A�1'A�1'A�/A�+A� �A��A��A�bA���A���A���A��\A�^5A�A�5?A���A�t�A�33A�33A��A���A���A�A��wA���A�;dA��
A���A��+A�|�A�1'A�ĜA�ffA���A���A���A�$�A�-A�&�A�9XA��A�C�A�dZA���A��A���A�-A�~�A���A��DA�1A���A�bNA��A��wA�;dA���A��PA��9A���A���A�bA�5?A�C�A��!A���A�O�A�(�A�r�A��uA��A�XA��FA��A|�A|��A{�
A{G�Aw|�AvjAv1'Au��At1'Aq��Aq%Ap�Ao7LAn  Amt�AlZAk"�Ai�Ai;dAhQ�Ag��Af�RAe;dAc�Aa|�A`��A_�wA]|�A\��A\z�AZ��AX=qAW"�AVVAU��ASdZARVAQ�AP �ANVAN1'AM�AMp�AM?}AM7LAM+ALv�AKVAI�mAI��AIAH�RAH-AG\)AGVAE�AEO�AEoAD�AD�DACXACVAB��AA�wA@ffA?�7A?K�A>��A=A;��A:��A9�wA8��A7��A7x�A5�-A5p�A5
=A4ffA3�A3�A2bA0��A0�RA0z�A/�#A.��A-�hA,�jA,�A+�A*I�A)�TA(��A(^5A( �A'�^A'��A'�hA'C�A&�DA&E�A%�PA$�RA#�A"��A"E�A!�PA�#A��A�An�AS�A��A�A^5Ap�A~�A�#A�RAhsA�/A��A�AJA��AA�A~�A�hA
�RA
ffA	��A	`BA	+AȴA�mA"�AE�AbA�HAVA7LAr�A�-A 1@��@�z�@�  @��@�$�@�&�@��@�C�@���@�&�@�7@�~�@홚@� �@�  @��m@��@�^@��@��/@��@�^5@�1'@��
@�hs@��@�/@�Q�@ۅ@�E�@�z�@�C�@�
=@���@և+@�^5@��@��@ա�@Չ7@��@ӕ�@Ѻ^@д9@�1'@϶F@ϥ�@ϕ�@�l�@�C�@�+@�"�@�ȴ@�J@�X@��/@�b@�=q@�z�@�@őh@�G�@ēu@���@�|�@�33@°!@�-@�hs@��u@��m@��R@��@��D@� �@�v�@�x�@�Q�@�33@�E�@�%@��@�I�@��P@��@��!@�E�@���@�9X@�\)@���@�5?@��^@��7@�p�@�X@��`@��R@���@�`B@�G�@���@���@��@���@�n�@�=q@���@�&�@�1'@��@�\)@�"�@��@��@�v�@���@���@�r�@�9X@�1@�ƨ@�dZ@�+@���@��+@�{@�@���@��T@���@�%@�z�@�ƨ@�|�@�C�@��H@���@�n�@�M�@��@���@���@�bN@��@���@�K�@���@��\@�ff@�E�@�$�@���@�O�@�  @��
@�ƨ@��P@�S�@��@���@��@��#@���@�X@�/@�V@���@��/@��@���@�"�@���@�~�@�n�@�V@�-@��@���@�p�@��`@��u@�(�@�ƨ@�|�@�33@�@���@��y@�ff@���@��@���@�j@�A�@��@��w@�|�@�S�@�33@�@��R@�-@��@��#@���@��^@��@��@��9@� �@��@���@�ȴ@�E�@�O�@�V@���@��D@�1'@�  @�@}�@}?}@}/@|��@|z�@|(�@{��@{ƨ@{C�@z��@y�#@y&�@x��@x�u@x�u@x1'@w�;@w��@w��@w;d@v�@v��@vv�@u�T@v@vv�@v��@vV@u�T@u�@t9X@s��@s�@s33@so@r��@rn�@rn�@rM�@q�@q�7@p��@o+@nv�@n5?@n5?@n{@m�@m��@mV@l�@lz�@k��@kƨ@kdZ@k"�@j�!@j=q@i�#@i��@i�^@iX@i7L@i&�@i�@i�@h��@h��@h�u@hbN@hQ�@h1'@g�w@g\)@gK�@g;d@fȴ@f5?@e��@e��@e�h@ep�@e/@d��@d�/@d�D@c��@c�@cC�@cC�@c33@b�@b��@b^5@a��@aG�@`�u@`Q�@` �@_��@^��@^�+@^ff@^E�@^$�@]@]`B@\��@[�m@[t�@[o@Z��@Z�\@Y��@Y�^@Y�^@Y�^@Y��@Yhs@Y7L@X�`@X�u@X�@W�;@W�@Wl�@V��@VE�@U�@U��@U�@T��@T�@T��@TZ@S��@SC�@S33@R��@R�\@RM�@R-@Q�^@Q7L@Q%@P��@P�u@PQ�@Pb@P  @O�@O�@O;d@O+@O
=@N�@N�R@NV@N{@M��@M�@M?}@L�@Lz�@L�@K��@KS�@K33@J��@J��@J~�@Jn�@JM�@J-@I��@I�@I��@I��@IX@H�9@HA�@G�;@G��@G�w@G��@GK�@G�@G�@G�@G
=@F��@F��@F�y@F�@F��@F�+@FE�@F{@E�@E��@E�@E�@D�/@D��@Dz�@DZ@D(�@D1@C�
@C��@C��@CdZ@C33@C"�@Co@B��@BM�@A�#@A��@Ax�@Ahs@A&�@A�@A%@@��@@�u@@bN@?�@?�P@>�y@>�+@>ff@>@=`B@=/@<��@<�/@<��@<�@<1@;��@;S�@;@:��@:�!@:~�@:^5@:=q@:-@9�7@8Ĝ@8r�@7��@7�@6�y@6ȴ@6�R@6��@6V@6{@5@5�@5?}@5�@4��@4j@3��@3�F@3t�@3"�@3@2�@2��@2��@2�\@2n�@2-@1��@1�#@1�#@1��@1��@1�^@1��@1��@1��@1x�@1X@1%@0r�@/��@/|�@/|�@/;d@.�y@.�R@.��@.5?@.@-�-@-�h@-p�@-�@,�/@,z�@,�@+ƨ@+��@+"�@*�H@*M�@*-@)��@)�@)��@)%@(��@(�u@(r�@(Q�@(A�@(1'@(b@'��@'l�@';d@'+@&��@&�R@&��@&v�@&E�@%�@%@%�-@%��@%�@%p�@%�@$�j@$z�@$(�@$1@#ƨ@#t�@#S�@#@"��@"-@!�#@!�^@!��@!��@!��@!�@ ��@ �`@ ��@ bN@ A�@ b@�w@|�@\)@K�@�@�y@ȴ@��@�+@ff@ff@ff@V@$�@��@��@�@p�@�@�@p�@O�@O�@/@��@�@�j@j@(�@ƨ@S�@�@�!@�\@^5@=q@-@J@�@��@hs@��@�9@�@ �@�;@�w@�@l�@ȴ@�+@v�@ff@V@V@V@V@V@V@E�@$�@��@��@�@?}@V@��@��@z�@�D@z�@Z@Z@I�@1@ƨ@@�!@n�@n�@M�@=q@�#@�#@�#@��@��@�^@��@x�@hs@X@7L@7L@7L@&�@&�@&�@�@�`@Ĝ@�9@�@�@�@�@bN@A�@ �@�P@\)@��@�R@�+@v�@ff@5?@$�@@/@�j@z�@j@Z@I�@I�@9X@(�@�@��@�m@�m@ƨ@�@dZ@o@
��@
��@
n�@
=q@
-@
�@
J@	�@	�#@	��@	X@	%@Ĝ@��@�@r�@r�@A�@  @�@�@|�@\)@;d@
=@ȴ@��@�+@ff@E�@5?@��@�-@@�-@��@`B111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A� �A�"�A�"�A�$�A�&�A�&�A�(�A�(�A�(�A�+A�(�A�+A�+A�-A�/A�/A�33A�33A�5?A�5?A�5?A�5?A�1'A�-A�(�A�-A�1'A�1'A�/A�+A� �A��A��A�bA���A���A���A��\A�^5A�A�5?A���A�t�A�33A�33A��A���A���A�A��wA���A�;dA��
A���A��+A�|�A�1'A�ĜA�ffA���A���A���A�$�A�-A�&�A�9XA��A�C�A�dZA���A��A���A�-A�~�A���A��DA�1A���A�bNA��A��wA�;dA���A��PA��9A���A���A�bA�5?A�C�A��!A���A�O�A�(�A�r�A��uA��A�XA��FA��A|�A|��A{�
A{G�Aw|�AvjAv1'Au��At1'Aq��Aq%Ap�Ao7LAn  Amt�AlZAk"�Ai�Ai;dAhQ�Ag��Af�RAe;dAc�Aa|�A`��A_�wA]|�A\��A\z�AZ��AX=qAW"�AVVAU��ASdZARVAQ�AP �ANVAN1'AM�AMp�AM?}AM7LAM+ALv�AKVAI�mAI��AIAH�RAH-AG\)AGVAE�AEO�AEoAD�AD�DACXACVAB��AA�wA@ffA?�7A?K�A>��A=A;��A:��A9�wA8��A7��A7x�A5�-A5p�A5
=A4ffA3�A3�A2bA0��A0�RA0z�A/�#A.��A-�hA,�jA,�A+�A*I�A)�TA(��A(^5A( �A'�^A'��A'�hA'C�A&�DA&E�A%�PA$�RA#�A"��A"E�A!�PA�#A��A�An�AS�A��A�A^5Ap�A~�A�#A�RAhsA�/A��A�AJA��AA�A~�A�hA
�RA
ffA	��A	`BA	+AȴA�mA"�AE�AbA�HAVA7LAr�A�-A 1@��@�z�@�  @��@�$�@�&�@��@�C�@���@�&�@�7@�~�@홚@� �@�  @��m@��@�^@��@��/@��@�^5@�1'@��
@�hs@��@�/@�Q�@ۅ@�E�@�z�@�C�@�
=@���@և+@�^5@��@��@ա�@Չ7@��@ӕ�@Ѻ^@д9@�1'@϶F@ϥ�@ϕ�@�l�@�C�@�+@�"�@�ȴ@�J@�X@��/@�b@�=q@�z�@�@őh@�G�@ēu@���@�|�@�33@°!@�-@�hs@��u@��m@��R@��@��D@� �@�v�@�x�@�Q�@�33@�E�@�%@��@�I�@��P@��@��!@�E�@���@�9X@�\)@���@�5?@��^@��7@�p�@�X@��`@��R@���@�`B@�G�@���@���@��@���@�n�@�=q@���@�&�@�1'@��@�\)@�"�@��@��@�v�@���@���@�r�@�9X@�1@�ƨ@�dZ@�+@���@��+@�{@�@���@��T@���@�%@�z�@�ƨ@�|�@�C�@��H@���@�n�@�M�@��@���@���@�bN@��@���@�K�@���@��\@�ff@�E�@�$�@���@�O�@�  @��
@�ƨ@��P@�S�@��@���@��@��#@���@�X@�/@�V@���@��/@��@���@�"�@���@�~�@�n�@�V@�-@��@���@�p�@��`@��u@�(�@�ƨ@�|�@�33@�@���@��y@�ff@���@��@���@�j@�A�@��@��w@�|�@�S�@�33@�@��R@�-@��@��#@���@��^@��@��@��9@� �@��@���@�ȴ@�E�@�O�@�V@���@��D@�1'@�  @�@}�@}?}@}/@|��@|z�@|(�@{��@{ƨ@{C�@z��@y�#@y&�@x��@x�u@x�u@x1'@w�;@w��@w��@w;d@v�@v��@vv�@u�T@v@vv�@v��@vV@u�T@u�@t9X@s��@s�@s33@so@r��@rn�@rn�@rM�@q�@q�7@p��@o+@nv�@n5?@n5?@n{@m�@m��@mV@l�@lz�@k��@kƨ@kdZ@k"�@j�!@j=q@i�#@i��@i�^@iX@i7L@i&�@i�@i�@h��@h��@h�u@hbN@hQ�@h1'@g�w@g\)@gK�@g;d@fȴ@f5?@e��@e��@e�h@ep�@e/@d��@d�/@d�D@c��@c�@cC�@cC�@c33@b�@b��@b^5@a��@aG�@`�u@`Q�@` �@_��@^��@^�+@^ff@^E�@^$�@]@]`B@\��@[�m@[t�@[o@Z��@Z�\@Y��@Y�^@Y�^@Y�^@Y��@Yhs@Y7L@X�`@X�u@X�@W�;@W�@Wl�@V��@VE�@U�@U��@U�@T��@T�@T��@TZ@S��@SC�@S33@R��@R�\@RM�@R-@Q�^@Q7L@Q%@P��@P�u@PQ�@Pb@P  @O�@O�@O;d@O+@O
=@N�@N�R@NV@N{@M��@M�@M?}@L�@Lz�@L�@K��@KS�@K33@J��@J��@J~�@Jn�@JM�@J-@I��@I�@I��@I��@IX@H�9@HA�@G�;@G��@G�w@G��@GK�@G�@G�@G�@G
=@F��@F��@F�y@F�@F��@F�+@FE�@F{@E�@E��@E�@E�@D�/@D��@Dz�@DZ@D(�@D1@C�
@C��@C��@CdZ@C33@C"�@Co@B��@BM�@A�#@A��@Ax�@Ahs@A&�@A�@A%@@��@@�u@@bN@?�@?�P@>�y@>�+@>ff@>@=`B@=/@<��@<�/@<��@<�@<1@;��@;S�@;@:��@:�!@:~�@:^5@:=q@:-@9�7@8Ĝ@8r�@7��@7�@6�y@6ȴ@6�R@6��@6V@6{@5@5�@5?}@5�@4��@4j@3��@3�F@3t�@3"�@3@2�@2��@2��@2�\@2n�@2-@1��@1�#@1�#@1��@1��@1�^@1��@1��@1��@1x�@1X@1%@0r�@/��@/|�@/|�@/;d@.�y@.�R@.��@.5?@.@-�-@-�h@-p�@-�@,�/@,z�@,�@+ƨ@+��@+"�@*�H@*M�@*-@)��@)�@)��@)%@(��@(�u@(r�@(Q�@(A�@(1'@(b@'��@'l�@';d@'+@&��@&�R@&��@&v�@&E�@%�@%@%�-@%��@%�@%p�@%�@$�j@$z�@$(�@$1@#ƨ@#t�@#S�@#@"��@"-@!�#@!�^@!��@!��@!��@!�@ ��@ �`@ ��@ bN@ A�@ b@�w@|�@\)@K�@�@�y@ȴ@��@�+@ff@ff@ff@V@$�@��@��@�@p�@�@�@p�@O�@O�@/@��@�@�j@j@(�@ƨ@S�@�@�!@�\@^5@=q@-@J@�@��@hs@��@�9@�@ �@�;@�w@�@l�@ȴ@�+@v�@ff@V@V@V@V@V@V@E�@$�@��@��@�@?}@V@��@��@z�@�D@z�@Z@Z@I�@1@ƨ@@�!@n�@n�@M�@=q@�#@�#@�#@��@��@�^@��@x�@hs@X@7L@7L@7L@&�@&�@&�@�@�`@Ĝ@�9@�@�@�@�@bN@A�@ �@�P@\)@��@�R@�+@v�@ff@5?@$�@@/@�j@z�@j@Z@I�@I�@9X@(�@�@��@�m@�m@ƨ@�@dZ@o@
��@
��@
n�@
=q@
-@
�@
J@	�@	�#@	��@	X@	%@Ĝ@��@�@r�@r�@A�@  @�@�@|�@\)@;d@
=@ȴ@��@�+@ff@E�@5?@��@�-@@�-@��@`B111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�PB�JB�=B�7B�B�B�7B�=B�=B�DB�=B�=B�7B�1B�7B�Bx�BiyBS�BA�BB�BĜB�9B�uBv�BiyBXB:^BhB��B�B�TB�#B��B��BĜB�}B�dB�9B�B��B�Bt�BiyBbNB\)BN�B@�B6FB%�B�BB��B�B�fB�5B�B��B�XB�FB�!B��B��B�hB�\B�JB�Bv�Bs�Bu�Bm�BffBbNB]/BW
BR�BN�BL�BI�BF�BA�B<jB33B/B'�B �B�B�BhB
=BBB��B��B�B�B�sB�/B�#B�B�#B�B�B�B�
B��B��B��B��B��B��BǮBƨBĜB��B��B�wB�qB�RB�LB�9B�!B��B��B��B��B��B�bB�DB�B~�B|�Bz�Bu�Bt�Bs�Bq�Bt�Bp�Bk�BdZBbNBaHB`BB\)BS�BL�BJ�BE�BB�B@�B=qB:^B8RB7LB6FB6FB7LB5?B33B0!B-B'�B"�B �B�B�BuBVBDB1BBB
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�sB
�fB
�ZB
�TB
�NB
�BB
�BB
�5B
�/B
�/B
�#B
�#B
�B
�B
�B
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
ɺB
ɺB
ȴB
ǮB
ŢB
ŢB
ÖB
B
B
B
��B
��B
��B
�}B
�wB
�qB
�qB
�jB
�^B
�jB
�jB
�dB
�dB
�dB
�dB
�jB
�qB
�qB
�qB
�qB
�qB
�wB
�qB
�qB
�qB
�jB
�wB
�}B
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
B
B
B
B
ĜB
ƨB
ȴB
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
��B
�B
�B
�B
�#B
�#B
�)B
�;B
�HB
�ZB
�`B
�fB
�sB
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B+BDBDBDBPBhB�B�B�B�B�B�B!�B$�B%�B&�B'�B'�B)�B.B2-B49B6FB6FB7LB:^B;dB=qB>wB@�BA�B@�BA�BC�BF�BI�BN�BP�BQ�BR�BT�BT�BVBW
BXBXBcTBdZBgmBiyBk�Bn�Bo�Bo�Bp�Bq�Bt�B}�B~�B� B�B�B�B�7B�PB�\B�hB�oB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�3B�FB�^B�wB��BÖBĜBĜBĜBɺB��B��B�B�B�B�#B�5B�HB�NB�NB�ZB�fB�B�B�B�B�B�B�B��B��B��B��BB1B\BhBhB�B�B�B�B �B$�B$�B%�B'�B(�B(�B)�B+B-B1'B2-B33B49B5?B7LB9XB9XB:^B;dB<jB<jB=qB?}B@�BB�BD�BE�BH�BJ�BJ�BI�BJ�BL�BM�BO�BP�BP�BP�BT�B[#B_;Be`BgmBhsBiyBjBk�Bl�Bm�Bm�Bo�Bo�Bo�Bq�Br�Bs�Bu�Bv�Bv�Bv�Bw�Bw�Bw�Bw�Bx�Bx�By�B{�B}�B~�B� B�B�B�B�B�B�+B�7B�7B�7B�=B�DB�JB�PB�VB�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�!B�-B�?B�FB�LB�LB�LB�XB�XB�dB�jB�jB�wB�wB�}B�}B��B��BBÖBŢBŢBŢBŢBǮBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�B�B�B�B�#B�)B�/B�5B�5B�;B�BB�BB�BB�HB�HB�HB�NB�NB�NB�TB�ZB�`B�fB�fB�fB�mB�mB�sB�sB�sB�sB�sB�sB�sB�yB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  BBBBBBBBBBB%B+B+B	7B
=B
=B
=B
=BDBDBJBPBPBPBPBVBVB\B\BbBhBhBhBhBoBoBoBuBuBuBuBuB{B{B{B{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B �B!�B!�B!�B#�B#�B#�B#�B#�B$�B$�B$�B$�B%�B%�B%�B%�B&�B&�B&�B'�B'�B(�B(�B(�B(�B(�B)�B)�B)�B+B+B,B,B,B-B-B.B.B/B/B/B/B0!B0!B0!B1'B1'B1'B1'B2-B2-B33B2-B33B33B33B49B49B49B49B49B49B5?B5?B5?B6FB6FB6FB6FB6FB6FB6FB6FB7LB7LB7LB8RB7LB8RB9XB:^B:^B:^B:^B;dB;dB;dB;dB<jB<jB=qB=qB>wB>wB?}B?}B?}B?}BA�BA�BA�BA�BA�BA�BA�BB�BA�BA�BB�BB�BB�BC�BB�BC�BC�BC�BD�BE�BD�BD�BE�BD�BE�BE�BF�BG�BG�BH�BG�BH�BH�BI�BI�BI�BI�BI�BI�BI�BJ�BI�BJ�BJ�BJ�BI�BJ�BJ�BJ�BJ�BK�BK�BK�BK�BL�BK�BK�BL�BL�BL�BM�BM�BN�BN�BO�BO�BO�BO�BO�BP�BQ�BQ�BR�BR�BR�BR�BR�BR�BR�BS�BS�BS�BS�BS�BT�BT�BT�BVBVBVBW
BW
BW
BW
BW
BW
BXBXBXBYBYBYBYBZBZBZBZB[#B[#B[#B[#B\)B\)B\)B]/B]/B]/B]/B^5B^5B^5B^5B^5B_;444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�PB�JB�=B�7B�B�B�7B�=B�=B�DB�=B�=B�7B�1B�7B�Bx�BiyBS�BA�BB�BĜB�9B�uBv�BiyBXB:^BhB��B�B�TB�#B��B��BĜB�}B�dB�9B�B��B�Bt�BiyBbNB\)BN�B@�B6FB%�B�BB��B�B�fB�5B�B��B�XB�FB�!B��B��B�hB�\B�JB�Bv�Bs�Bu�Bm�BffBbNB]/BW
BR�BN�BL�BI�BF�BA�B<jB33B/B'�B �B�B�BhB
=BBB��B��B�B�B�sB�/B�#B�B�#B�B�B�B�
B��B��B��B��B��B��BǮBƨBĜB��B��B�wB�qB�RB�LB�9B�!B��B��B��B��B��B�bB�DB�B~�B|�Bz�Bu�Bt�Bs�Bq�Bt�Bp�Bk�BdZBbNBaHB`BB\)BS�BL�BJ�BE�BB�B@�B=qB:^B8RB7LB6FB6FB7LB5?B33B0!B-B'�B"�B �B�B�BuBVBDB1BBB
��B
��B
��B
��B
��B
�B
�B
�B
�B
�B
�B
�sB
�fB
�ZB
�TB
�NB
�BB
�BB
�5B
�/B
�/B
�#B
�#B
�B
�B
�B
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
ɺB
ɺB
ȴB
ǮB
ŢB
ŢB
ÖB
B
B
B
��B
��B
��B
�}B
�wB
�qB
�qB
�jB
�^B
�jB
�jB
�dB
�dB
�dB
�dB
�jB
�qB
�qB
�qB
�qB
�qB
�wB
�qB
�qB
�qB
�jB
�wB
�}B
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
B
B
B
B
ĜB
ƨB
ȴB
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
��B
�B
�B
�B
�#B
�#B
�)B
�;B
�HB
�ZB
�`B
�fB
�sB
�B
�B
�B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B+BDBDBDBPBhB�B�B�B�B�B�B!�B$�B%�B&�B'�B'�B)�B.B2-B49B6FB6FB7LB:^B;dB=qB>wB@�BA�B@�BA�BC�BF�BI�BN�BP�BQ�BR�BT�BT�BVBW
BXBXBcTBdZBgmBiyBk�Bn�Bo�Bo�Bp�Bq�Bt�B}�B~�B� B�B�B�B�7B�PB�\B�hB�oB�{B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�3B�FB�^B�wB��BÖBĜBĜBĜBɺB��B��B�B�B�B�#B�5B�HB�NB�NB�ZB�fB�B�B�B�B�B�B�B��B��B��B��BB1B\BhBhB�B�B�B�B �B$�B$�B%�B'�B(�B(�B)�B+B-B1'B2-B33B49B5?B7LB9XB9XB:^B;dB<jB<jB=qB?}B@�BB�BD�BE�BH�BJ�BJ�BI�BJ�BL�BM�BO�BP�BP�BP�BT�B[#B_;Be`BgmBhsBiyBjBk�Bl�Bm�Bm�Bo�Bo�Bo�Bq�Br�Bs�Bu�Bv�Bv�Bv�Bw�Bw�Bw�Bw�Bx�Bx�By�B{�B}�B~�B� B�B�B�B�B�B�+B�7B�7B�7B�=B�DB�JB�PB�VB�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�!B�-B�?B�FB�LB�LB�LB�XB�XB�dB�jB�jB�wB�wB�}B�}B��B��BBÖBŢBŢBŢBŢBǮBȴBȴBɺB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�
B�B�B�B�B�#B�)B�/B�5B�5B�;B�BB�BB�BB�HB�HB�HB�NB�NB�NB�TB�ZB�`B�fB�fB�fB�mB�mB�sB�sB�sB�sB�sB�sB�sB�yB�yB�yB�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B  B  B  BBBBBBBBBBB%B+B+B	7B
=B
=B
=B
=BDBDBJBPBPBPBPBVBVB\B\BbBhBhBhBhBoBoBoBuBuBuBuBuB{B{B{B{B{B{B{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B �B!�B!�B!�B#�B#�B#�B#�B#�B$�B$�B$�B$�B%�B%�B%�B%�B&�B&�B&�B'�B'�B(�B(�B(�B(�B(�B)�B)�B)�B+B+B,B,B,B-B-B.B.B/B/B/B/B0!B0!B0!B1'B1'B1'B1'B2-B2-B33B2-B33B33B33B49B49B49B49B49B49B5?B5?B5?B6FB6FB6FB6FB6FB6FB6FB6FB7LB7LB7LB8RB7LB8RB9XB:^B:^B:^B:^B;dB;dB;dB;dB<jB<jB=qB=qB>wB>wB?}B?}B?}B?}BA�BA�BA�BA�BA�BA�BA�BB�BA�BA�BB�BB�BB�BC�BB�BC�BC�BC�BD�BE�BD�BD�BE�BD�BE�BE�BF�BG�BG�BH�BG�BH�BH�BI�BI�BI�BI�BI�BI�BI�BJ�BI�BJ�BJ�BJ�BI�BJ�BJ�BJ�BJ�BK�BK�BK�BK�BL�BK�BK�BL�BL�BL�BM�BM�BN�BN�BO�BO�BO�BO�BO�BP�BQ�BQ�BR�BR�BR�BR�BR�BR�BR�BS�BS�BS�BS�BS�BT�BT�BT�BVBVBVBW
BW
BW
BW
BW
BW
BXBXBXBYBYBYBYBZBZBZBZB[#B[#B[#B[#B\)B\)B\)B]/B]/B]/B]/B^5B^5B^5B^5B^5B_;444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.11 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20220213100112                              AO  ARCAADJP                                                                    20220213100112    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20220213100112  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20220213100112  QCF$                G�O�G�O�G�O�8000            