CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-12-22T10:01:28Z creation      
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
_FillValue                 �  I   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  `�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �X   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ݀   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ݰ   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �,   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �<   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �P   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �T   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20221222100128  20221222100128  5905729 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               �A   AO  7160                            2B  A   NAVIS_A                         0838                            170425                          863 @�S�*1   @�T}'ܐ@)D���S��d�$�/1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         �A   A   A   @�ff@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`ffBh  BpffBw��B��B���B���B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(�fD)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�C3D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�|�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǃ3D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D��3D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̓3D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�3D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�3D�9�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��G@�z�@�z�A=qA>=qA^=qA~=qA��A��A��A��A��A��A��A��B�\B�\B�\B�\B'�\B/�\B7�\B?�\BG�\BO�\BW�\B_��Bg�\Bo��Bw(�B(�B�aHB��{B�ǮB�ǮB�ǮB�ǮB�ǮB���B�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮB�ǮC��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C��C���C���C���C���C���D x�D ��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D	x�D	��D
x�D
��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��Dx�D��D x�D ��D!x�D!��D"x�D"��D#x�D#��D$x�D$��D%x�D%��D&x�D&��D'x�D'��D(\D(��D)x�D)��D*x�D*��D+x�D+��D,x�D,��D-x�D-��D.x�D.��D/x�D/��D0x�D0��D1x�D1��D2x�D2��D3x�D3��D4x�D4��D5x�D5��D6x�D6��D7x�D7��D8x�D8��D9x�D9��D:x�D:��D;x�D;��D<x�D<��D=x�D=��D>x�D>��D?x�D?��D@x�D@��DAx�DA��DBx�DB��DCx�DC��DDx�DD��DEx�DE��DFx�DF��DGx�DG��DHx�DH��DIx�DI��DJx�DJ��DKx�DK��DLx�DL��DMx�DM��DNx�DN��DOx�DO��DPx�DP��DQx�DQ��DRx�DR��DSx�DS��DTx�DT��DUx�DU��DVx�DV��DWx�DW��DXx�DX��DYx�DY��DZx�DZ��D[x�D[��D\x�D\��D]x�D]��D^x�D^��D_x�D_��D`x�D`��Dax�Da��Dbx�Db��Dcx�Dc��Ddx�Dd��Dex�De��Dfx�Df��Dgx�Dg��Dhx�Dh��Dix�Di��Djx�Dj��Dkx�Dk��Dlx�Dl��Dmx�Dm��Dnx�Dn��Dox�Do��Dpx�Dp��Dqx�Dq��Drx�Dr��Dsx�Ds��Dtx�Dt��Dux�Du��Dvx�Dv��Dwx�Dw��Dxx�Dx��Dyx�Dy��Dzx�Dz��D{x�D{��D|x�D|��D}x�D}��D~x�D~��Dx�D��D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�?�D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�yHD��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D¼{D��{D�<{D�|{Dü{D��{D�<{D�|{Dļ{D��{D�<{D�|{Dż{D��{D�<{D�|{DƼ{D��{D�<{D��DǼ{D��{D�<{D�|{Dȼ{D��{D�<{D�|{Dɼ{D��{D�<{D�|{Dʿ�D��{D�<{D�|{D˼{D��{D�<{D�|{D̼{D��{D�<{D��Dͼ{D��{D�<{D�|{Dμ{D��{D�<{D�|{Dϼ{D��{D�<{D�|{Dм{D��{D�<{D�|{DѼ{D��{D�<{D�|{DҼ{D��{D�<{D�|{DӼ{D��{D�<{D�|{DԼ{D��{D�<{D�|{Dռ{D��{D�<{D�|{Dּ{D��{D�<{D�|{D׼{D��{D�<{D�|{Dؼ{D��{D�<{D�|{Dټ{D��{D�<{D�|{Dڼ{D��{D�<{D�|{Dۼ{D��{D�<{D�|{Dܼ{D��{D�<{D�|{Dݼ{D��{D�<{D�|{D޼{D��{D�<{D�|{D߼{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D���D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D�{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D��{D�<{D�|{D��{D���D�61111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�\)A�`BA�bNA�`BA�\)A�ZA�\)A�`BA�ZA�dZA�bNA�`BA�`BA�`BA�XA�O�A�C�A��A�A��TA��#A���A�ƨA�A�A�A�ĜA�ƨA�A֥�A�5?A�/A��/A�Q�Aɗ�Aȩ�A�A��A�^5A�A���A��A��
A�A�A��+A��`A�/A���A���A��7A�bA��A�VA�ZA�\)A�M�A�E�A�bNA�r�A�33A�I�A�  A���A���A��A�%A��+A���A��DA��RA�\)A�^5A���A�ĜA��/A�G�A�z�Ay%Au\)Asl�Ap�HAk�Ag7LA_�A\bAX�\API�AMVAJ�jAHffACK�AAoA<�`A:�HA9�FA8~�A8Q�A77LA5A4-A3l�A2  A1O�A0M�A/�A.��A.�!A.z�A-�TA-"�A+�mA*v�A'��A$�RA#`BA ��A�wA =qA!�-A#�-A#�^A�TA��A �A�A�
A �AI�A �At�Av�Al�A��AA�A�mA��A��A�\A�
A�hAC�AVA�RAM�A?}AJA��AXAQ�A��A�A��A�!A1'A�PAXA?}A
�RA
ffA	�A�AbA��A�A	��A
-A%AK�AXA
�A	t�A	7LA�A��A(�A�PAS�A�AffA1Ax�A"�A�`An�A�wA�yA�9A�RA�+AE�A�FA?}A ��A �A =q@���@��R@�+@���@�{@���@�ƨ@��y@�M�@���@�ȴ@��+@�~�@���@�/@���@���@�r�@�D@�b@�@�!@�Ĝ@�1@�33@�ȴ@���@���@�A�@�b@�dZ@��H@�M�@陚@��/@�(�@��m@���@畁@�n�@��@�hs@�j@�(�@�K�@�+@�-@ᙚ@�V@���@���@�;d@���@�^5@�M�@��T@���@� �@�|�@���@�v�@�5?@�`B@؋D@�  @ץ�@���@�M�@���@�`B@�%@ԋD@� �@��;@�+@���@��y@��y@���@���@��@���@�@���@�`B@�/@д9@�  @�C�@Χ�@���@��@��@�ƨ@�ƨ@���@��@�Q�@̛�@�Ĝ@��@̣�@�(�@�t�@�C�@�+@��@�~�@ə�@�Z@��
@�o@���@Ə\@�V@�5?@�J@��@�hs@���@�Q�@��;@�C�@�@�~�@�5?@��^@�%@��D@�I�@���@���@��T@��/@�(�@��P@�;d@���@��!@�
=@��@��F@���@�5?@��#@��T@���@�O�@�X@�/@���@��@�Q�@�Q�@�Q�@��w@��@�@�ȴ@�v�@�5?@���@��@�p�@�O�@�&�@��`@��u@�1'@���@�\)@��@���@�{@���@�hs@�V@���@�j@�  @�dZ@���@�n�@�E�@�p�@��@��D@�9X@��@�ȴ@�ff@��7@�X@��@��F@��@�
=@�@��H@���@��@��@��D@�j@�A�@���@�;d@��@��!@�ff@�-@���@��#@���@�/@��/@��9@���@��@�Q�@�1'@�b@�ƨ@�@�E�@�{@�J@�@�@���@���@��@�G�@��@��/@�Ĝ@��u@�r�@�bN@� �@���@��R@�E�@��@�@�@�@�@��^@���@�hs@�Ĝ@�Q�@��
@��P@�+@�E�@���@�O�@���@��`@��9@���@�t�@��@��R@���@�~�@�J@��h@�`B@�?}@��@��`@���@�j@�9X@�dZ@��H@��+@�@���@���@��/@�r�@���@��;@���@���@�dZ@��@�5?@�`B@�?}@�%@��`@���@���@�bN@��;@�S�@�
=@���@��+@�{@��@���@�1'@�  @��
@��@�33@��R@�ff@�-@��@�?}@���@��@�j@�(�@�;@;d@~v�@}�@}/@|��@|�/@|�j@|�D@|(�@{�@z��@z��@z��@z^5@y�#@y��@yG�@x�9@xA�@xA�@xb@w\)@w
=@v�@vȴ@v�R@v$�@u�-@u`B@uV@t�/@t��@t9X@s�m@s��@s33@r�H@r�!@r-@q��@q�7@qG�@q&�@p��@o�@o�P@oK�@n��@m��@m�-@m�h@l��@lZ@l9X@k��@k��@j�@j�@i��@i�^@ihs@iG�@i7L@i&�@h��@h��@hQ�@hA�@h �@hb@g��@f��@f�R@f��@f�+@fff@fE�@f5?@f@e�-@e�h@e�@d�@d��@d�j@dj@c�
@cS�@b�!@bJ@a�@a�^@a�7@aX@a%@`�u@` �@_�@_K�@^�y@^�y@^ȴ@^��@^$�@]�h@]�h@]?}@\��@\��@\�D@\Z@\(�@[�m@[�@Z�@Z~�@Zn�@Z-@Y��@Y7L@XĜ@X�@X1'@WK�@V�y@Vȴ@Vv�@U�@U�@T�/@Tz�@S��@S�F@S"�@R�\@R�@QG�@P�9@Pb@O��@O;d@N�@N��@Nff@M�T@M�h@L��@L�D@Lj@L9X@K�
@Kt�@K@J��@J�@I�@I��@IX@H �@G�@G��@Gl�@G
=@F�R@Fv�@F5?@E�@E�@D��@Dz�@DI�@D(�@D�@C�m@Ct�@C33@B�@B��@B^5@B�@A��@A��@Ahs@A�@@�`@@A�@@  @?�w@?K�@?�@?
=@>��@>�R@>v�@>V@=p�@=/@<�@<�/@<��@<�D@<(�@<1@;��@;�
@;�F@;�@;"�@:n�@:-@9��@9hs@9&�@8��@8bN@8A�@8b@7�@7�@7+@6�@6�R@6��@6�+@6v�@6E�@65?@6$�@6@5��@5�@5`B@5?}@4�/@4�j@4��@4�D@4z�@4Z@4(�@3�m@3��@2�@2M�@1��@1�7@1G�@0�9@01'@/�w@/+@.�@.v�@-�T@-�T@-��@-��@-��@-?}@,�/@,�j@,�D@+�m@+dZ@+o@*�H@*��@*�!@*~�@*J@)G�@)%@(�9@(A�@( �@(  @'�@'��@'l�@';d@&�y@&ȴ@&��@&E�@%�T@%��@%�h@%�h@%�h@%�@%`B@$�/@$z�@$�@#��@#"�@"��@"^5@"-@!��@!��@!�7@!x�@!G�@!&�@!%@ �`@ �9@ �u@ Q�@  �@�@l�@\)@;d@��@ȴ@��@�+@�+@v�@ff@E�@$�@@�@�@p�@O�@O�@�@�@j@(�@(�@��@33@��@~�@^5@�@�@��@��@��@��@�7@X@7L@�@��@�u@Q�@ �@��@K�@�@�R@��@v�@ff@E�@$�@{@�@�-@�@?}@��@�@�
@�
@ƨ@�F@��@��@�@t�@dZ@C�@33@33@�@M�@��@�#@�^@��@�7@hs@�@��@bN@  @�P@l�@�@�y@ȴ@�R@�R@��@��@��@E�@�T@@�-@�-@��@�@�@p�@O�@�@�D@Z@�
@��@��@��@��@��@33@o@@
�@
�H@
�\@
=q@
�@
J@	�#@	�7@	&�@��@Ĝ@��@��@��@��@��@�u@r�@bN@1'@ �@b@b@  @�@�@|�@l�@K�@�R@��@v�@E�@{@@�@�@�@�@�@�T@�T@�T@�T@��@�T@�T@�T@��@��@@�h@p�@p�@`B@`B@`B1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�\)A�`BA�bNA�`BA�\)A�ZA�\)A�`BA�ZA�dZA�bNA�`BA�`BA�`BA�XA�O�A�C�A��A�A��TA��#A���A�ƨA�A�A�A�ĜA�ƨA�A֥�A�5?A�/A��/A�Q�Aɗ�Aȩ�A�A��A�^5A�A���A��A��
A�A�A��+A��`A�/A���A���A��7A�bA��A�VA�ZA�\)A�M�A�E�A�bNA�r�A�33A�I�A�  A���A���A��A�%A��+A���A��DA��RA�\)A�^5A���A�ĜA��/A�G�A�z�Ay%Au\)Asl�Ap�HAk�Ag7LA_�A\bAX�\API�AMVAJ�jAHffACK�AAoA<�`A:�HA9�FA8~�A8Q�A77LA5A4-A3l�A2  A1O�A0M�A/�A.��A.�!A.z�A-�TA-"�A+�mA*v�A'��A$�RA#`BA ��A�wA =qA!�-A#�-A#�^A�TA��A �A�A�
A �AI�A �At�Av�Al�A��AA�A�mA��A��A�\A�
A�hAC�AVA�RAM�A?}AJA��AXAQ�A��A�A��A�!A1'A�PAXA?}A
�RA
ffA	�A�AbA��A�A	��A
-A%AK�AXA
�A	t�A	7LA�A��A(�A�PAS�A�AffA1Ax�A"�A�`An�A�wA�yA�9A�RA�+AE�A�FA?}A ��A �A =q@���@��R@�+@���@�{@���@�ƨ@��y@�M�@���@�ȴ@��+@�~�@���@�/@���@���@�r�@�D@�b@�@�!@�Ĝ@�1@�33@�ȴ@���@���@�A�@�b@�dZ@��H@�M�@陚@��/@�(�@��m@���@畁@�n�@��@�hs@�j@�(�@�K�@�+@�-@ᙚ@�V@���@���@�;d@���@�^5@�M�@��T@���@� �@�|�@���@�v�@�5?@�`B@؋D@�  @ץ�@���@�M�@���@�`B@�%@ԋD@� �@��;@�+@���@��y@��y@���@���@��@���@�@���@�`B@�/@д9@�  @�C�@Χ�@���@��@��@�ƨ@�ƨ@���@��@�Q�@̛�@�Ĝ@��@̣�@�(�@�t�@�C�@�+@��@�~�@ə�@�Z@��
@�o@���@Ə\@�V@�5?@�J@��@�hs@���@�Q�@��;@�C�@�@�~�@�5?@��^@�%@��D@�I�@���@���@��T@��/@�(�@��P@�;d@���@��!@�
=@��@��F@���@�5?@��#@��T@���@�O�@�X@�/@���@��@�Q�@�Q�@�Q�@��w@��@�@�ȴ@�v�@�5?@���@��@�p�@�O�@�&�@��`@��u@�1'@���@�\)@��@���@�{@���@�hs@�V@���@�j@�  @�dZ@���@�n�@�E�@�p�@��@��D@�9X@��@�ȴ@�ff@��7@�X@��@��F@��@�
=@�@��H@���@��@��@��D@�j@�A�@���@�;d@��@��!@�ff@�-@���@��#@���@�/@��/@��9@���@��@�Q�@�1'@�b@�ƨ@�@�E�@�{@�J@�@�@���@���@��@�G�@��@��/@�Ĝ@��u@�r�@�bN@� �@���@��R@�E�@��@�@�@�@�@��^@���@�hs@�Ĝ@�Q�@��
@��P@�+@�E�@���@�O�@���@��`@��9@���@�t�@��@��R@���@�~�@�J@��h@�`B@�?}@��@��`@���@�j@�9X@�dZ@��H@��+@�@���@���@��/@�r�@���@��;@���@���@�dZ@��@�5?@�`B@�?}@�%@��`@���@���@�bN@��;@�S�@�
=@���@��+@�{@��@���@�1'@�  @��
@��@�33@��R@�ff@�-@��@�?}@���@��@�j@�(�@�;@;d@~v�@}�@}/@|��@|�/@|�j@|�D@|(�@{�@z��@z��@z��@z^5@y�#@y��@yG�@x�9@xA�@xA�@xb@w\)@w
=@v�@vȴ@v�R@v$�@u�-@u`B@uV@t�/@t��@t9X@s�m@s��@s33@r�H@r�!@r-@q��@q�7@qG�@q&�@p��@o�@o�P@oK�@n��@m��@m�-@m�h@l��@lZ@l9X@k��@k��@j�@j�@i��@i�^@ihs@iG�@i7L@i&�@h��@h��@hQ�@hA�@h �@hb@g��@f��@f�R@f��@f�+@fff@fE�@f5?@f@e�-@e�h@e�@d�@d��@d�j@dj@c�
@cS�@b�!@bJ@a�@a�^@a�7@aX@a%@`�u@` �@_�@_K�@^�y@^�y@^ȴ@^��@^$�@]�h@]�h@]?}@\��@\��@\�D@\Z@\(�@[�m@[�@Z�@Z~�@Zn�@Z-@Y��@Y7L@XĜ@X�@X1'@WK�@V�y@Vȴ@Vv�@U�@U�@T�/@Tz�@S��@S�F@S"�@R�\@R�@QG�@P�9@Pb@O��@O;d@N�@N��@Nff@M�T@M�h@L��@L�D@Lj@L9X@K�
@Kt�@K@J��@J�@I�@I��@IX@H �@G�@G��@Gl�@G
=@F�R@Fv�@F5?@E�@E�@D��@Dz�@DI�@D(�@D�@C�m@Ct�@C33@B�@B��@B^5@B�@A��@A��@Ahs@A�@@�`@@A�@@  @?�w@?K�@?�@?
=@>��@>�R@>v�@>V@=p�@=/@<�@<�/@<��@<�D@<(�@<1@;��@;�
@;�F@;�@;"�@:n�@:-@9��@9hs@9&�@8��@8bN@8A�@8b@7�@7�@7+@6�@6�R@6��@6�+@6v�@6E�@65?@6$�@6@5��@5�@5`B@5?}@4�/@4�j@4��@4�D@4z�@4Z@4(�@3�m@3��@2�@2M�@1��@1�7@1G�@0�9@01'@/�w@/+@.�@.v�@-�T@-�T@-��@-��@-��@-?}@,�/@,�j@,�D@+�m@+dZ@+o@*�H@*��@*�!@*~�@*J@)G�@)%@(�9@(A�@( �@(  @'�@'��@'l�@';d@&�y@&ȴ@&��@&E�@%�T@%��@%�h@%�h@%�h@%�@%`B@$�/@$z�@$�@#��@#"�@"��@"^5@"-@!��@!��@!�7@!x�@!G�@!&�@!%@ �`@ �9@ �u@ Q�@  �@�@l�@\)@;d@��@ȴ@��@�+@�+@v�@ff@E�@$�@@�@�@p�@O�@O�@�@�@j@(�@(�@��@33@��@~�@^5@�@�@��@��@��@��@�7@X@7L@�@��@�u@Q�@ �@��@K�@�@�R@��@v�@ff@E�@$�@{@�@�-@�@?}@��@�@�
@�
@ƨ@�F@��@��@�@t�@dZ@C�@33@33@�@M�@��@�#@�^@��@�7@hs@�@��@bN@  @�P@l�@�@�y@ȴ@�R@�R@��@��@��@E�@�T@@�-@�-@��@�@�@p�@O�@�@�D@Z@�
@��@��@��@��@��@33@o@@
�@
�H@
�\@
=q@
�@
J@	�#@	�7@	&�@��@Ĝ@��@��@��@��@��@�u@r�@bN@1'@ �@b@b@  @�@�@|�@l�@K�@�R@��@v�@E�@{@@�@�@�@�@�@�T@�T@�T@�T@��@�T@�T@�T@��@��@@�h@p�@p�@`B@`B@`B1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B}�B}�B{�B}�B�B�VB�{B��B��B��B��B��B��B��B�B	�B!�B�B�BB�HB��B�BC�BL�BM�BN�BYBM�BE�BQ�BB�B%�B$�B �B��BB�HB��B��B��B��B��B�7By�Bz�BaHB:^B0!BB
��B
��B
�TB
��B
�3B
u�B
I�B
F�B
8RB
'�B
B	�RB	�jB	B	��B	}�B	m�B	G�B	,B	#�B�B�B�B�TB��BÖB�^B��B��BŢB��B��B�9B�}B�}B�dBĜBŢBB�B�
B��B��BƨB�XB�B��B��BƨBǮB�5B��B	VB	 �B	8RB	$�B	B�B	>wB	T�B	cTB	l�B	y�B	~�B	}�B	�B	�B	�JB	�PB	�JB	�=B	z�B	�B	�bB	��B	��B	��B	��B	�hB	�JB	�%B	�\B	�oB	�PB	�bB	�hB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�3B	�?B	ÖB	�
B	�NB	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
  B	��B	��B	��B
  B
B
  B
B
PB
DB

=B
%B
B
  B	��B	��B	�B	��B	��B	��B	��B	��B
B
B
+B
+B
%B
B	��B
B
B
B
B
B
B
%B
B
B
B
B
B
B
B
+B
%B
B
B
B
B
B
  B
  B
B
B
  B
B	��B
  B
B
B
B
B	��B
B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B
B
B
B
%B
1B
JB
PB
PB
DB
JB
JB
\B
\B
VB
JB
	7B
+B
JB
DB
\B
bB
bB
bB
bB
\B
PB
PB
VB
VB
VB
\B
oB
hB
\B
VB
\B
bB
PB
PB
JB
JB
VB
VB
bB
hB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
#�B
"�B
 �B
�B
�B
!�B
$�B
$�B
#�B
!�B
$�B
%�B
&�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
'�B
'�B
&�B
%�B
"�B
$�B
(�B
+B
+B
+B
+B
)�B
)�B
,B
-B
.B
/B
.B
.B
.B
,B
)�B
(�B
-B
0!B
1'B
33B
2-B
2-B
1'B
0!B
/B
,B
.B
.B
/B
/B
,B
/B
2-B
33B
49B
33B
0!B
1'B
33B
7LB
7LB
6FB
49B
49B
7LB
7LB
7LB
7LB
6FB
6FB
6FB
33B
5?B
6FB
6FB
7LB
7LB
:^B
9XB
9XB
=qB
<jB
<jB
:^B
9XB
6FB
8RB
>wB
>wB
?}B
>wB
>wB
<jB
;dB
;dB
>wB
=qB
?}B
<jB
9XB
>wB
@�B
A�B
B�B
B�B
B�B
B�B
B�B
D�B
C�B
A�B
A�B
D�B
C�B
D�B
D�B
C�B
D�B
D�B
H�B
H�B
H�B
H�B
H�B
H�B
G�B
H�B
K�B
K�B
K�B
K�B
L�B
K�B
K�B
L�B
N�B
M�B
L�B
N�B
O�B
O�B
O�B
M�B
N�B
N�B
O�B
O�B
O�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
Q�B
Q�B
Q�B
P�B
N�B
Q�B
R�B
Q�B
P�B
S�B
S�B
Q�B
R�B
T�B
S�B
S�B
R�B
S�B
W
B
W
B
W
B
XB
XB
XB
XB
W
B
XB
YB
YB
YB
W
B
W
B
ZB
[#B
ZB
ZB
[#B
ZB
ZB
ZB
[#B
ZB
ZB
[#B
[#B
ZB
YB
YB
YB
ZB
]/B
]/B
\)B
\)B
\)B
[#B
\)B
\)B
\)B
]/B
^5B
^5B
^5B
]/B
]/B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
_;B
_;B
_;B
`BB
aHB
`BB
`BB
`BB
`BB
aHB
`BB
_;B
aHB
bNB
bNB
aHB
bNB
aHB
bNB
bNB
cTB
bNB
bNB
cTB
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
ffB
gmB
iyB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
hsB
gmB
iyB
jB
jB
iyB
jB
jB
jB
jB
iyB
iyB
iyB
jB
k�B
k�B
k�B
jB
k�B
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
m�B
m�B
m�B
n�B
n�B
n�B
n�B
m�B
n�B
m�B
o�B
o�B
p�B
p�B
p�B
o�B
p�B
q�B
p�B
p�B
p�B
o�B
n�B
p�B
o�B
p�B
q�B
q�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
t�B
s�B
t�B
t�B
u�B
u�B
t�B
t�B
u�B
u�B
v�B
v�B
w�B
y�B
y�B
y�B
x�B
x�B
x�B
y�B
x�B
w�B
x�B
z�B
z�B
{�B
{�B
z�B
z�B
y�B
{�B
|�B
{�B
}�B
}�B
}�B
|�B
|�B
}�B
|�B
}�B
}�B
|�B
}�B
~�B
~�B
� B
� B
~�B
}�B
|�B
}�B
}�B
}�B
~�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�B
�B
�%B
�%B
�%B
�B
�B
�%B
�+B
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
�7B
�7B
�7B
�=B
�=B
�7B
�7B
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�DB
�DB
�DB
�DB
�=B
�DB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�PB
�JB
�PB
�\B
�\B
�\B
�\B
�\B
�VB
�VB
�VB
�\B
�\B
�hB
�hB
�oB
�oB
�uB
�uB
�uB
�oB
�oB
�hB
�hB
�uB
�uB
�{B
�uB
�uB
�{B
�uB
�uB
�oB
�oB
�{B
�uB
��B
��B
��B
��B
��B
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
��B
��B
��B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B}�B}�B{�B}�B�B�VB�{B��B��B��B��B��B��B��B�B	�B!�B�B�BB�HB��B�BC�BL�BM�BN�BYBM�BE�BQ�BB�B%�B$�B �B��BB�HB��B��B��B��B��B�7By�Bz�BaHB:^B0!BB
��B
��B
�TB
��B
�3B
u�B
I�B
F�B
8RB
'�B
B	�RB	�jB	B	��B	}�B	m�B	G�B	,B	#�B�B�B�B�TB��BÖB�^B��B��BŢB��B��B�9B�}B�}B�dBĜBŢBB�B�
B��B��BƨB�XB�B��B��BƨBǮB�5B��B	VB	 �B	8RB	$�B	B�B	>wB	T�B	cTB	l�B	y�B	~�B	}�B	�B	�B	�JB	�PB	�JB	�=B	z�B	�B	�bB	��B	��B	��B	��B	�hB	�JB	�%B	�\B	�oB	�PB	�bB	�hB	�bB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�3B	�?B	ÖB	�
B	�NB	�B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
  B	��B	��B	��B
  B
B
  B
B
PB
DB

=B
%B
B
  B	��B	��B	�B	��B	��B	��B	��B	��B
B
B
+B
+B
%B
B	��B
B
B
B
B
B
B
%B
B
B
B
B
B
B
B
+B
%B
B
B
B
B
B
  B
  B
B
B
  B
B	��B
  B
B
B
B
B	��B
B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B	��B	��B
B
B
B
B
B
B
  B	��B	��B	��B	��B	��B	��B
B
B
B
%B
1B
JB
PB
PB
DB
JB
JB
\B
\B
VB
JB
	7B
+B
JB
DB
\B
bB
bB
bB
bB
\B
PB
PB
VB
VB
VB
\B
oB
hB
\B
VB
\B
bB
PB
PB
JB
JB
VB
VB
bB
hB
uB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
�B
�B
�B
�B
�B
�B
�B
#�B
"�B
 �B
�B
�B
!�B
$�B
$�B
#�B
!�B
$�B
%�B
&�B
%�B
&�B
&�B
&�B
&�B
&�B
'�B
(�B
(�B
(�B
'�B
'�B
&�B
%�B
"�B
$�B
(�B
+B
+B
+B
+B
)�B
)�B
,B
-B
.B
/B
.B
.B
.B
,B
)�B
(�B
-B
0!B
1'B
33B
2-B
2-B
1'B
0!B
/B
,B
.B
.B
/B
/B
,B
/B
2-B
33B
49B
33B
0!B
1'B
33B
7LB
7LB
6FB
49B
49B
7LB
7LB
7LB
7LB
6FB
6FB
6FB
33B
5?B
6FB
6FB
7LB
7LB
:^B
9XB
9XB
=qB
<jB
<jB
:^B
9XB
6FB
8RB
>wB
>wB
?}B
>wB
>wB
<jB
;dB
;dB
>wB
=qB
?}B
<jB
9XB
>wB
@�B
A�B
B�B
B�B
B�B
B�B
B�B
D�B
C�B
A�B
A�B
D�B
C�B
D�B
D�B
C�B
D�B
D�B
H�B
H�B
H�B
H�B
H�B
H�B
G�B
H�B
K�B
K�B
K�B
K�B
L�B
K�B
K�B
L�B
N�B
M�B
L�B
N�B
O�B
O�B
O�B
M�B
N�B
N�B
O�B
O�B
O�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
Q�B
Q�B
Q�B
P�B
N�B
Q�B
R�B
Q�B
P�B
S�B
S�B
Q�B
R�B
T�B
S�B
S�B
R�B
S�B
W
B
W
B
W
B
XB
XB
XB
XB
W
B
XB
YB
YB
YB
W
B
W
B
ZB
[#B
ZB
ZB
[#B
ZB
ZB
ZB
[#B
ZB
ZB
[#B
[#B
ZB
YB
YB
YB
ZB
]/B
]/B
\)B
\)B
\)B
[#B
\)B
\)B
\)B
]/B
^5B
^5B
^5B
]/B
]/B
^5B
_;B
_;B
`BB
`BB
`BB
`BB
_;B
_;B
_;B
`BB
aHB
`BB
`BB
`BB
`BB
aHB
`BB
_;B
aHB
bNB
bNB
aHB
bNB
aHB
bNB
bNB
cTB
bNB
bNB
cTB
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
ffB
gmB
iyB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
hsB
gmB
iyB
jB
jB
iyB
jB
jB
jB
jB
iyB
iyB
iyB
jB
k�B
k�B
k�B
jB
k�B
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
m�B
m�B
m�B
n�B
n�B
n�B
n�B
m�B
n�B
m�B
o�B
o�B
p�B
p�B
p�B
o�B
p�B
q�B
p�B
p�B
p�B
o�B
n�B
p�B
o�B
p�B
q�B
q�B
r�B
s�B
s�B
s�B
s�B
s�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
t�B
s�B
t�B
t�B
u�B
u�B
t�B
t�B
u�B
u�B
v�B
v�B
w�B
y�B
y�B
y�B
x�B
x�B
x�B
y�B
x�B
w�B
x�B
z�B
z�B
{�B
{�B
z�B
z�B
y�B
{�B
|�B
{�B
}�B
}�B
}�B
|�B
|�B
}�B
|�B
}�B
}�B
|�B
}�B
~�B
~�B
� B
� B
~�B
}�B
|�B
}�B
}�B
}�B
~�B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�%B
�%B
�%B
�%B
�B
�B
�%B
�%B
�%B
�B
�B
�%B
�+B
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
�7B
�7B
�7B
�=B
�=B
�7B
�7B
�DB
�DB
�DB
�DB
�DB
�DB
�JB
�DB
�DB
�DB
�DB
�=B
�DB
�PB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�VB
�PB
�JB
�PB
�\B
�\B
�\B
�\B
�\B
�VB
�VB
�VB
�\B
�\B
�hB
�hB
�oB
�oB
�uB
�uB
�uB
�oB
�oB
�hB
�hB
�uB
�uB
�{B
�uB
�uB
�{B
�uB
�uB
�oB
�oB
�{B
�uB
��B
��B
��B
��B
��B
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
��B
��B
��B
��B
��B
��B
��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.11 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20221222100128                              AO  ARCAADJP                                                                    20221222100128    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20221222100128  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20221222100128  QCF$                G�O�G�O�G�O�4000            