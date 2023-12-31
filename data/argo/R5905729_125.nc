CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-09-28T09:01:13Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20210928090113  20210928090113  5905729 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               }A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @ٖ����1   @ٖ�O�@(���n��d[�l�C�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         }A   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B ffB'33B/��B8  B@  BH  BP  BXffB^��Bg��Bp  Bw��B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  C   C  C  C  C�C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*y�D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�G�@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B��B&B/(�B7�\B?�\BG�\BO�\BW��B^\)Bg(�Bo�\Bw(�B�\B���B��{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB��{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�{B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C�qC	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(x�D(��D)x�D)��D*r�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��HD��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��HD�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D�|{DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʼ{D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D�|{Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D��D��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A��A��A��A���A��`A��;A��HA��
A�7A�l�A�A��`A��\A�A�9A���A�t�Aߣ�A���A���AڼjA��A���A�%A�bAΣ�A�bA��A�5?A�v�A��!A��A���A��^A�jA�|�A���A���A���A�"�A�|�A�jA���A�A��A���A�  A�"�A� �A�^5A�A��A��A�\)A�-A�S�Al�A}��Ayp�Au�#AtbAm�PAgƨAc|�A_hsA]x�AY�#AS�-AR��ARĜAQ%AO33AM�TAMS�ALv�AJ�uAH�\AG��AFn�AC�TA?�A<�jA:1'A8��A8�jA7l�A4�A4 �A3x�A3��A5;dA7oA7x�A8JA7�A7�A8A77LA1G�A/�A-�#A-��A-�A,�/A+�;A*�A*jA*�A)hsA)"�A'�A'�
A&��A%�
A%�hA$��A#�TA#�A%33A%��A%��A%?}A$�A#K�A#7LA"ĜA"r�A"�A"Q�A"v�A"z�A"~�A"ffA"=qA"JA!�TA!�-A!�A!XA!A �uA JA�TAA|�A
=A��A=qA|�AC�AVA�yA��A�!Az�AhsA�A�RA�AVAbA�A��A|�A�AĜA�\AffA9XA�
A��A7LAA��A|�A?}A�/A��A�A�wAO�AĜAr�AI�A1A�A�A��A5?A��A��A33A��AZA�A��A��AM�A�;Al�A
=A
�9A
ZA
A	�FA	dZA��A �A��A�A�^A��A��AhsA�A�A��An�A=qA��A|�A�HA��A�uA~�AQ�A�A��Al�A
=A �uA (�@��w@��@��@���@��#@�p�@�X@�G�@�&�@�Ĝ@�  @��@�33@��!@�n�@���@���@�G�@���@�z�@� �@�@�V@�@�`B@�7L@��u@� �@�P@�!@�@���@�Z@�b@��
@�w@��@�dZ@�\@��^@�G�@��`@�9X@�S�@��H@�+@�{@陚@�&�@�ƨ@�ȴ@���@��@�V@�A�@��m@�dZ@�33@�@�n�@��T@�O�@��@��@�Ĝ@���@�@�^5@�$�@��#@ݑh@�O�@��/@�(�@��;@۶F@�\)@�33@��y@ڟ�@���@�V@ؓu@��;@׍P@�+@և+@��T@Դ9@Ӿw@ҏ\@��@�p�@ЋD@���@ϝ�@�+@��#@́@��@��m@���@��@ʸR@ʏ\@�$�@ə�@Ɂ@�hs@�O�@��@��@Ȭ@�z�@�b@��m@��;@���@�;d@��@Ɨ�@Ƈ+@�^5@��@���@�?}@��`@�(�@�ƨ@�33@°!@�v�@��@��h@�/@��`@���@�1'@�|�@��@�ȴ@�M�@���@��h@��@��@���@���@��F@���@��@�S�@���@���@�j@�A�@�9X@�1'@���@�V@���@�G�@�V@���@��9@��9@��@��u@�b@���@�dZ@��H@��+@�J@�O�@���@�
=@���@��+@�5?@��7@���@�Z@�1@���@�\)@�33@�ȴ@�n�@�^5@�5?@���@�O�@��u@�@�V@��#@�@�@��h@�O�@�V@��`@���@�(�@�o@���@�^5@�=q@���@���@���@���@��h@��@�G�@���@��j@��@�1@��
@��w@���@�|�@�C�@�@�ȴ@��R@���@���@���@���@��@�hs@�G�@��@�  @�l�@�"�@��y@��R@���@�^5@�$�@��T@���@�p�@�O�@�?}@�/@��@���@���@���@�bN@��@���@�V@��@���@�x�@��@��@�Z@�b@�ƨ@��@�S�@��@���@�E�@��h@�?}@�&�@�%@��@��/@���@�Ĝ@��9@���@��D@�j@�1'@�  @��w@��@�S�@���@���@�^5@��@���@�x�@�7L@��@��/@�r�@�Q�@�9X@�9X@�1'@��@��F@�dZ@�
=@���@��!@�ff@���@��#@��^@��@��@���@��j@��@���@�Q�@�(�@��m@��@�S�@��H@���@��\@�v�@�M�@�{@��T@��@��#@���@��@���@�Q�@���@�
=@���@�M�@��@��@��T@��-@�G�@���@�r�@�Z@� �@��@\)@~v�@~��@}�T@}�-@}`B@|�@{ƨ@{t�@{S�@{33@{o@z�!@y��@xĜ@xA�@w�@vȴ@v5?@u@t��@tj@s�m@s33@r-@q�@qhs@pĜ@o�@o�@o|�@oK�@oK�@o+@n�y@nE�@m�@l1@j��@j�\@jn�@j�@i��@i�#@i�@i�^@h��@g�w@g�P@g|�@gK�@g
=@f�+@fE�@e�T@e`B@eV@dz�@d9X@c��@c�m@c�
@c�F@ct�@cS�@c"�@b�H@b��@bJ@a�#@a��@ax�@a%@`��@`�9@`�u@`r�@`A�@` �@_��@^��@_;d@^��@^��@^{@\�@\I�@[�
@[��@[dZ@Z��@ZM�@ZJ@Y��@Y�^@Y�^@Y�^@Y��@Yx�@YX@YG�@Y7L@Y�@Y�@X��@X��@XĜ@X�u@XbN@W�@W�w@WK�@VV@U@U�@Tj@T1@S�F@SS�@R�@R�H@R��@R��@R�\@Rn�@RM�@Q��@P��@Pr�@P  @O
=@NV@NE�@NV@N$�@N{@M�@M/@L��@Lj@K��@K��@KdZ@KC�@K@J��@Jn�@I�#@Ihs@H��@H�@H1'@G+@Fff@E�-@E�@D��@DI�@D1@C�m@C��@B��@A��@Ax�@A�@@�`@@�@@ �@?��@?K�@>��@>@=�@<��@<9X@;�m@;�F@:�!@:�@:J@9�#@9�^@9��@9��@9��@97L@8Ĝ@81'@7K�@7;d@7
=@6��@6V@6$�@5�T@5��@5?}@4j@41@3ƨ@3o@2n�@2-@1�#@1�7@0�`@0�u@0r�@0Q�@0Q�@0b@/�w@/�@/��@/\)@.�y@.��@.E�@-�@-��@-�@-`B@-/@-�@-V@,��@,�D@+�m@+�F@+�@+C�@+o@+@*�H@*�!@*�\@*�\@*~�@*-@*J@)�#@)�7@)X@(�9@'�@'��@'�@'��@'|�@'\)@'K�@';d@'
=@&�y@&��@&E�@%�@%�@$��@$�j@$z�@$9X@#��@#t�@#"�@"^5@"=q@"�@!��@!�@!��@!hs@ ��@ �9@ A�@ b@�@��@�@|�@
=@��@�+@E�@��@��@�h@�@�@`B@O�@O�@/@�@�@�j@�j@��@z�@j@1@��@t�@S�@33@"�@"�@"�@o@�@��@��@��@��@~�@n�@^5@=q@��@��@��@��@�7@G�@�u@r�@bN@bN@Q�@Q�@A�@�;@��@K�@
=@�y@ȴ@�R@��@V@@�@��@�h@O�@/@��@�/@��@(�@1@1@�
@33@o@o@o@@�!@~�@^5@M�@-@��@��@G�@�@��@r�@  @�@��@�w@��@l�@;d@�@�@�@
=@��@�R@v�@ff@5?@@�T@��@p�@`B@/@�@V@V@�/@�D@(�@�
@��@�@t�@dZ@C�@"�@"�@o@o@@
�@
�@
��@
��@
��@
~�@
n�@
n�@
�@
J111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A��A��A��A��A��A���A��`A��;A��HA��
A�7A�l�A�A��`A��\A�A�9A���A�t�Aߣ�A���A���AڼjA��A���A�%A�bAΣ�A�bA��A�5?A�v�A��!A��A���A��^A�jA�|�A���A���A���A�"�A�|�A�jA���A�A��A���A�  A�"�A� �A�^5A�A��A��A�\)A�-A�S�Al�A}��Ayp�Au�#AtbAm�PAgƨAc|�A_hsA]x�AY�#AS�-AR��ARĜAQ%AO33AM�TAMS�ALv�AJ�uAH�\AG��AFn�AC�TA?�A<�jA:1'A8��A8�jA7l�A4�A4 �A3x�A3��A5;dA7oA7x�A8JA7�A7�A8A77LA1G�A/�A-�#A-��A-�A,�/A+�;A*�A*jA*�A)hsA)"�A'�A'�
A&��A%�
A%�hA$��A#�TA#�A%33A%��A%��A%?}A$�A#K�A#7LA"ĜA"r�A"�A"Q�A"v�A"z�A"~�A"ffA"=qA"JA!�TA!�-A!�A!XA!A �uA JA�TAA|�A
=A��A=qA|�AC�AVA�yA��A�!Az�AhsA�A�RA�AVAbA�A��A|�A�AĜA�\AffA9XA�
A��A7LAA��A|�A?}A�/A��A�A�wAO�AĜAr�AI�A1A�A�A��A5?A��A��A33A��AZA�A��A��AM�A�;Al�A
=A
�9A
ZA
A	�FA	dZA��A �A��A�A�^A��A��AhsA�A�A��An�A=qA��A|�A�HA��A�uA~�AQ�A�A��Al�A
=A �uA (�@��w@��@��@���@��#@�p�@�X@�G�@�&�@�Ĝ@�  @��@�33@��!@�n�@���@���@�G�@���@�z�@� �@�@�V@�@�`B@�7L@��u@� �@�P@�!@�@���@�Z@�b@��
@�w@��@�dZ@�\@��^@�G�@��`@�9X@�S�@��H@�+@�{@陚@�&�@�ƨ@�ȴ@���@��@�V@�A�@��m@�dZ@�33@�@�n�@��T@�O�@��@��@�Ĝ@���@�@�^5@�$�@��#@ݑh@�O�@��/@�(�@��;@۶F@�\)@�33@��y@ڟ�@���@�V@ؓu@��;@׍P@�+@և+@��T@Դ9@Ӿw@ҏ\@��@�p�@ЋD@���@ϝ�@�+@��#@́@��@��m@���@��@ʸR@ʏ\@�$�@ə�@Ɂ@�hs@�O�@��@��@Ȭ@�z�@�b@��m@��;@���@�;d@��@Ɨ�@Ƈ+@�^5@��@���@�?}@��`@�(�@�ƨ@�33@°!@�v�@��@��h@�/@��`@���@�1'@�|�@��@�ȴ@�M�@���@��h@��@��@���@���@��F@���@��@�S�@���@���@�j@�A�@�9X@�1'@���@�V@���@�G�@�V@���@��9@��9@��@��u@�b@���@�dZ@��H@��+@�J@�O�@���@�
=@���@��+@�5?@��7@���@�Z@�1@���@�\)@�33@�ȴ@�n�@�^5@�5?@���@�O�@��u@�@�V@��#@�@�@��h@�O�@�V@��`@���@�(�@�o@���@�^5@�=q@���@���@���@���@��h@��@�G�@���@��j@��@�1@��
@��w@���@�|�@�C�@�@�ȴ@��R@���@���@���@���@��@�hs@�G�@��@�  @�l�@�"�@��y@��R@���@�^5@�$�@��T@���@�p�@�O�@�?}@�/@��@���@���@���@�bN@��@���@�V@��@���@�x�@��@��@�Z@�b@�ƨ@��@�S�@��@���@�E�@��h@�?}@�&�@�%@��@��/@���@�Ĝ@��9@���@��D@�j@�1'@�  @��w@��@�S�@���@���@�^5@��@���@�x�@�7L@��@��/@�r�@�Q�@�9X@�9X@�1'@��@��F@�dZ@�
=@���@��!@�ff@���@��#@��^@��@��@���@��j@��@���@�Q�@�(�@��m@��@�S�@��H@���@��\@�v�@�M�@�{@��T@��@��#@���@��@���@�Q�@���@�
=@���@�M�@��@��@��T@��-@�G�@���@�r�@�Z@� �@��@\)@~v�@~��@}�T@}�-@}`B@|�@{ƨ@{t�@{S�@{33@{o@z�!@y��@xĜ@xA�@w�@vȴ@v5?@u@t��@tj@s�m@s33@r-@q�@qhs@pĜ@o�@o�@o|�@oK�@oK�@o+@n�y@nE�@m�@l1@j��@j�\@jn�@j�@i��@i�#@i�@i�^@h��@g�w@g�P@g|�@gK�@g
=@f�+@fE�@e�T@e`B@eV@dz�@d9X@c��@c�m@c�
@c�F@ct�@cS�@c"�@b�H@b��@bJ@a�#@a��@ax�@a%@`��@`�9@`�u@`r�@`A�@` �@_��@^��@_;d@^��@^��@^{@\�@\I�@[�
@[��@[dZ@Z��@ZM�@ZJ@Y��@Y�^@Y�^@Y�^@Y��@Yx�@YX@YG�@Y7L@Y�@Y�@X��@X��@XĜ@X�u@XbN@W�@W�w@WK�@VV@U@U�@Tj@T1@S�F@SS�@R�@R�H@R��@R��@R�\@Rn�@RM�@Q��@P��@Pr�@P  @O
=@NV@NE�@NV@N$�@N{@M�@M/@L��@Lj@K��@K��@KdZ@KC�@K@J��@Jn�@I�#@Ihs@H��@H�@H1'@G+@Fff@E�-@E�@D��@DI�@D1@C�m@C��@B��@A��@Ax�@A�@@�`@@�@@ �@?��@?K�@>��@>@=�@<��@<9X@;�m@;�F@:�!@:�@:J@9�#@9�^@9��@9��@9��@97L@8Ĝ@81'@7K�@7;d@7
=@6��@6V@6$�@5�T@5��@5?}@4j@41@3ƨ@3o@2n�@2-@1�#@1�7@0�`@0�u@0r�@0Q�@0Q�@0b@/�w@/�@/��@/\)@.�y@.��@.E�@-�@-��@-�@-`B@-/@-�@-V@,��@,�D@+�m@+�F@+�@+C�@+o@+@*�H@*�!@*�\@*�\@*~�@*-@*J@)�#@)�7@)X@(�9@'�@'��@'�@'��@'|�@'\)@'K�@';d@'
=@&�y@&��@&E�@%�@%�@$��@$�j@$z�@$9X@#��@#t�@#"�@"^5@"=q@"�@!��@!�@!��@!hs@ ��@ �9@ A�@ b@�@��@�@|�@
=@��@�+@E�@��@��@�h@�@�@`B@O�@O�@/@�@�@�j@�j@��@z�@j@1@��@t�@S�@33@"�@"�@"�@o@�@��@��@��@��@~�@n�@^5@=q@��@��@��@��@�7@G�@�u@r�@bN@bN@Q�@Q�@A�@�;@��@K�@
=@�y@ȴ@�R@��@V@@�@��@�h@O�@/@��@�/@��@(�@1@1@�
@33@o@o@o@@�!@~�@^5@M�@-@��@��@G�@�@��@r�@  @�@��@�w@��@l�@;d@�@�@�@
=@��@�R@v�@ff@5?@@�T@��@p�@`B@/@�@V@V@�/@�D@(�@�
@��@�@t�@dZ@C�@"�@"�@o@o@@
�@
�@
��@
��@
��@
~�@
n�@
n�@
�@
J111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�^B�^B�XB�XB�XB�XB�FB�?B�9B�!B�B��B��B��B��B��B��B�B�5B	<jB	�B
�B
^5B
p�B
�1B
��B
��B
o�B
p�B
ffB
��B
��B
�\B
��B
��B
��B
�5B
�B%B
=B
��B-B1'B�BoB
��B
��B
�B
�B
�#B
�!B
o�B
e`B
R�B
-B
#�B
$�B
�B
bB	��B	�B	�mB	ȴB	��B	��B	�1B	�B	u�B	n�B	�=B	�JB	�B	�=B	�oB	�dB	�qB	�^B	��B	��B	�B	��B	��B	�dB	��B	��B	��B	��B	��B	�B	�LB	��B	�B
PB
-B
5?B
=qB
=qB
>wB
8RB
�B	��B	��B	��B	��B
  B
+B
hB
!�B
+B
+B
.B
:^B
L�B
N�B
R�B
]/B
]/B
[#B
ffB
q�B
�B
�B
�B
~�B
z�B
�%B
�DB
�oB
�oB
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
�{B
�oB
�oB
�oB
�oB
�hB
�hB
�VB
�VB
�JB
�JB
�1B
�+B
�+B
�B
�B
� B
�B
�B
~�B
� B
� B
~�B
|�B
z�B
z�B
w�B
w�B
v�B
t�B
s�B
r�B
q�B
n�B
k�B
k�B
jB
hsB
gmB
ffB
e`B
dZB
cTB
bNB
`BB
[#B
T�B
S�B
VB
VB
VB
T�B
S�B
R�B
R�B
P�B
P�B
N�B
L�B
J�B
K�B
L�B
K�B
J�B
H�B
G�B
F�B
D�B
C�B
B�B
B�B
C�B
B�B
@�B
@�B
@�B
A�B
A�B
@�B
>wB
=qB
>wB
=qB
=qB
=qB
<jB
<jB
<jB
<jB
;dB
;dB
9XB
:^B
:^B
;dB
;dB
9XB
9XB
8RB
7LB
7LB
6FB
6FB
8RB
8RB
8RB
7LB
6FB
33B
2-B
33B
33B
1'B
1'B
2-B
1'B
1'B
0!B
/B
,B
,B
,B
-B
+B
)�B
,B
+B
,B
+B
(�B
(�B
(�B
)�B
)�B
(�B
'�B
"�B
&�B
'�B
'�B
'�B
&�B
%�B
%�B
%�B
&�B
%�B
%�B
$�B
$�B
"�B
"�B
"�B
!�B
"�B
"�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
!�B
 �B
 �B
�B
 �B
 �B
 �B
�B
!�B
 �B
!�B
 �B
 �B
!�B
"�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
�B
�B
�B
�B
!�B
"�B
$�B
$�B
$�B
#�B
#�B
$�B
#�B
#�B
"�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
&�B
&�B
&�B
'�B
&�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
)�B
)�B
+B
,B
,B
,B
,B
+B
)�B
+B
,B
-B
-B
.B
.B
.B
.B
.B
/B
/B
/B
/B
/B
/B
.B
.B
-B
,B
-B
/B
/B
0!B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
/B
1'B
0!B
0!B
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
33B
49B
33B
49B
33B
49B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
7LB
8RB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
9XB
:^B
:^B
:^B
:^B
:^B
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
<jB
=qB
>wB
=qB
=qB
>wB
?}B
?}B
A�B
@�B
@�B
?}B
@�B
B�B
B�B
B�B
A�B
B�B
B�B
D�B
E�B
E�B
D�B
D�B
C�B
D�B
E�B
F�B
F�B
F�B
E�B
E�B
G�B
G�B
G�B
G�B
G�B
F�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
J�B
L�B
M�B
M�B
M�B
M�B
L�B
K�B
J�B
J�B
K�B
M�B
M�B
M�B
N�B
P�B
Q�B
R�B
R�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
O�B
O�B
P�B
P�B
R�B
R�B
R�B
R�B
R�B
Q�B
R�B
R�B
Q�B
R�B
Q�B
R�B
S�B
R�B
S�B
T�B
T�B
T�B
T�B
T�B
S�B
S�B
S�B
W
B
W
B
YB
ZB
YB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
]/B
^5B
_;B
^5B
_;B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
^5B
^5B
^5B
^5B
^5B
]/B
^5B
]/B
]/B
]/B
^5B
^5B
_;B
`BB
aHB
`BB
`BB
`BB
`BB
_;B
`BB
`BB
aHB
`BB
`BB
aHB
aHB
aHB
bNB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
bNB
bNB
cTB
cTB
bNB
bNB
cTB
dZB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
hsB
hsB
iyB
hsB
iyB
iyB
iyB
iyB
iyB
jB
k�B
k�B
l�B
l�B
k�B
l�B
m�B
m�B
n�B
n�B
n�B
m�B
m�B
m�B
m�B
m�B
o�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
w�B
w�B
v�B
v�B
w�B
x�B
x�B
x�B
y�B
x�B
y�B
y�B
y�B
y�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
{�B
{�B
{�B
{�B
{�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
}�B
}�B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�+B
�7B
�7B
�7B
�7B
�7B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�DB
�DB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
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
��B
��B
��B
��B
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�^B�^B�XB�XB�XB�XB�FB�?B�9B�!B�B��B��B��B��B��B��B�B�5B	<jB	�B
�B
^5B
p�B
�1B
��B
��B
o�B
p�B
ffB
��B
��B
�\B
��B
��B
��B
�5B
�B%B
=B
��B-B1'B�BoB
��B
��B
�B
�B
�#B
�!B
o�B
e`B
R�B
-B
#�B
$�B
�B
bB	��B	�B	�mB	ȴB	��B	��B	�1B	�B	u�B	n�B	�=B	�JB	�B	�=B	�oB	�dB	�qB	�^B	��B	��B	�B	��B	��B	�dB	��B	��B	��B	��B	��B	�B	�LB	��B	�B
PB
-B
5?B
=qB
=qB
>wB
8RB
�B	��B	��B	��B	��B
  B
+B
hB
!�B
+B
+B
.B
:^B
L�B
N�B
R�B
]/B
]/B
[#B
ffB
q�B
�B
�B
�B
~�B
z�B
�%B
�DB
�oB
�oB
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
�{B
�oB
�oB
�oB
�oB
�hB
�hB
�VB
�VB
�JB
�JB
�1B
�+B
�+B
�B
�B
� B
�B
�B
~�B
� B
� B
~�B
|�B
z�B
z�B
w�B
w�B
v�B
t�B
s�B
r�B
q�B
n�B
k�B
k�B
jB
hsB
gmB
ffB
e`B
dZB
cTB
bNB
`BB
[#B
T�B
S�B
VB
VB
VB
T�B
S�B
R�B
R�B
P�B
P�B
N�B
L�B
J�B
K�B
L�B
K�B
J�B
H�B
G�B
F�B
D�B
C�B
B�B
B�B
C�B
B�B
@�B
@�B
@�B
A�B
A�B
@�B
>wB
=qB
>wB
=qB
=qB
=qB
<jB
<jB
<jB
<jB
;dB
;dB
9XB
:^B
:^B
;dB
;dB
9XB
9XB
8RB
7LB
7LB
6FB
6FB
8RB
8RB
8RB
7LB
6FB
33B
2-B
33B
33B
1'B
1'B
2-B
1'B
1'B
0!B
/B
,B
,B
,B
-B
+B
)�B
,B
+B
,B
+B
(�B
(�B
(�B
)�B
)�B
(�B
'�B
"�B
&�B
'�B
'�B
'�B
&�B
%�B
%�B
%�B
&�B
%�B
%�B
$�B
$�B
"�B
"�B
"�B
!�B
"�B
"�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
 �B
 �B
 �B
!�B
 �B
 �B
�B
 �B
 �B
 �B
�B
!�B
 �B
!�B
 �B
 �B
!�B
"�B
!�B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
 �B
�B
�B
�B
�B
!�B
"�B
$�B
$�B
$�B
#�B
#�B
$�B
#�B
#�B
"�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
'�B
'�B
&�B
&�B
&�B
'�B
&�B
(�B
)�B
)�B
)�B
)�B
)�B
)�B
+B
)�B
)�B
+B
,B
,B
,B
,B
+B
)�B
+B
,B
-B
-B
.B
.B
.B
.B
.B
/B
/B
/B
/B
/B
/B
.B
.B
-B
,B
-B
/B
/B
0!B
/B
/B
/B
0!B
0!B
0!B
0!B
0!B
/B
1'B
0!B
0!B
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
33B
49B
33B
49B
33B
49B
5?B
5?B
5?B
6FB
6FB
6FB
7LB
7LB
7LB
7LB
6FB
6FB
6FB
7LB
7LB
8RB
8RB
7LB
8RB
8RB
8RB
8RB
9XB
:^B
:^B
:^B
9XB
:^B
:^B
:^B
:^B
:^B
;dB
<jB
<jB
<jB
<jB
<jB
=qB
=qB
=qB
<jB
=qB
>wB
=qB
=qB
>wB
?}B
?}B
A�B
@�B
@�B
?}B
@�B
B�B
B�B
B�B
A�B
B�B
B�B
D�B
E�B
E�B
D�B
D�B
C�B
D�B
E�B
F�B
F�B
F�B
E�B
E�B
G�B
G�B
G�B
G�B
G�B
F�B
G�B
G�B
G�B
H�B
I�B
I�B
I�B
J�B
L�B
M�B
M�B
M�B
M�B
L�B
K�B
J�B
J�B
K�B
M�B
M�B
M�B
N�B
P�B
Q�B
R�B
R�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
O�B
O�B
P�B
P�B
R�B
R�B
R�B
R�B
R�B
Q�B
R�B
R�B
Q�B
R�B
Q�B
R�B
S�B
R�B
S�B
T�B
T�B
T�B
T�B
T�B
S�B
S�B
S�B
W
B
W
B
YB
ZB
YB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
]/B
^5B
_;B
^5B
_;B
^5B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
_;B
^5B
^5B
^5B
^5B
^5B
]/B
^5B
]/B
]/B
]/B
^5B
^5B
_;B
`BB
aHB
`BB
`BB
`BB
`BB
_;B
`BB
`BB
aHB
`BB
`BB
aHB
aHB
aHB
bNB
aHB
bNB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
bNB
bNB
cTB
cTB
bNB
bNB
cTB
dZB
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
hsB
hsB
iyB
hsB
iyB
iyB
iyB
iyB
iyB
jB
k�B
k�B
l�B
l�B
k�B
l�B
m�B
m�B
n�B
n�B
n�B
m�B
m�B
m�B
m�B
m�B
o�B
o�B
o�B
o�B
p�B
p�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
s�B
s�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
u�B
u�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
x�B
w�B
w�B
v�B
v�B
w�B
x�B
x�B
x�B
y�B
x�B
y�B
y�B
y�B
y�B
x�B
y�B
y�B
y�B
y�B
y�B
y�B
{�B
{�B
{�B
{�B
{�B
|�B
|�B
{�B
{�B
{�B
{�B
{�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
}�B
}�B
� B
� B
� B
� B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�+B
�1B
�1B
�1B
�1B
�+B
�7B
�7B
�7B
�7B
�7B
�1B
�1B
�1B
�1B
�1B
�7B
�7B
�7B
�7B
�7B
�7B
�=B
�=B
�=B
�=B
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�DB
�DB
�PB
�PB
�PB
�PB
�PB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�\B
�\B
�bB
�bB
�bB
�hB
�hB
�hB
�hB
�hB
�oB
�oB
�hB
�oB
�oB
�oB
�oB
�oB
�oB
�oB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�uB
�{B
�{B
�{B
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
��B
��B
��B
��B
��B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.11 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20210928090113                              AO  ARCAADJP                                                                    20210928090113    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210928090113  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210928090113  QCF$                G�O�G�O�G�O�0               