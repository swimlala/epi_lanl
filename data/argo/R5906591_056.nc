CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2022-11-21T12:01:16Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20221121120116  20221121120116  5906591 Argo PMEL                                                       GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               8A   AO  8753                            2B  A   NAVIS_A                         1281                            170425                          863 @���'�V1   @����r@1bM����dE���l�1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         8A   A   A   @�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B��B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�33B�33B�  B�  B���C   C  C  C  C  C
  C  C  C  C�fC  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Cs�fCv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)�fD*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̓3D��3D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�3D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D��3D��f111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�z�@�G�@�z�A=qA:=qAZ=qAz=qA��A��A��A��A��A��A��A��B�\B�\B(�B�\B&�\B.�\B6�\B>�\BF�\BN�\BV�\B^�\Bf�\Bn�\Bv�\B~�\B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�z�B�z�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�G�B�z�B�G�B�z�B�z�B�G�B�G�B�{B�G�C��C��C��C��C	��C��C��C��C�=C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[�qC]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs�=Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�޹C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�޹C���C���C���C���D h�D ��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��D	h�D	��D
h�D
��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��Dh�D��D h�D ��D!h�D!��D"h�D"��D#h�D#��D$h�D$��D%h�D%��D&h�D&��D'h�D'��D(h�D(��D)o\D)��D*h�D*��D+h�D+��D,h�D,��D-h�D-��D.h�D.��D/h�D/��D0h�D0��D1h�D1��D2h�D2��D3h�D3��D4h�D4��D5h�D5��D6h�D6��D7h�D7��D8h�D8��D9h�D9��D:h�D:��D;h�D;��D<h�D<��D=h�D=��D>h�D>��D?h�D?��D@h�D@��DAh�DA��DBh�DB��DCh�DC��DDh�DD��DEh�DE��DFh�DF��DGh�DG��DHh�DH��DIh�DI��DJh�DJ��DKh�DK��DLh�DL��DMh�DM��DNh�DN��DOh�DO��DPh�DP��DQh�DQ��DRh�DR��DSh�DS��DTh�DT��DUh�DU��DVh�DV��DWh�DW��DXh�DX��DYh�DY��DZh�DZ��D[h�D[��D\h�D\��D]h�D]��D^h�D^��D_h�D_��D`h�D`��Dah�Da��Dbh�Db��Dch�Dc��Ddh�Dd��Deh�De��Dfh�Df��Dgh�Dg��Dhh�Dh��Dih�Di��Djh�Dj��Dkh�Dk��Dlh�Dl��Dmh�Dm��Dnh�Dn��Doh�Do��Dph�Dp��Dqh�Dq��Drh�Dr��Dsh�Ds��Dth�Dt��Duh�Du��Dvh�Dv��Dwh�Dw��Dxh�Dx��Dyh�Dy��Dzh�Dz��D{h�D{��D|h�D|��D}h�D}��D~h�D~��Dh�D��D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D´{D��{D�4{D�t{Dô{D��{D�4{D�t{DĴ{D��{D�4{D�t{DŴ{D��{D�4{D�t{Dƴ{D��{D�4{D�t{DǴ{D��{D�4{D�t{Dȴ{D��{D�4{D�t{Dɴ{D��{D�4{D�t{Dʴ{D��{D�4{D�t{D˴{D��{D�4{D�t{D̴{D��{D�4{D�w�Dͷ�D��{D�4{D�t{Dδ{D��{D�4{D�t{Dϴ{D��{D�4{D�t{Dд{D��{D�4{D�t{DѴ{D��{D�4{D�t{DҴ{D��{D�4{D�t{DӴ{D��{D�4{D�t{DԴ{D��{D�4{D�t{Dմ{D��{D�4{D�t{Dִ{D��{D�4{D�t{D״{D��{D�4{D�t{Dش{D��{D�4{D�t{Dٴ{D��{D�4{D�t{Dڴ{D��{D�4{D�t{D۴{D��{D�4{D�t{Dܴ{D��{D�4{D�t{Dݴ{D��{D�4{D�t{D޴{D��{D�4{D�t{Dߴ{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�w�D�{D��{D�4{D�t{D��{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D�{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�t{D��{D��{D�4{D�w�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A�A�%A�1A�1A�
=A�JA�VA�VA�oA�oA�oA�{A�oA��A��A�{A�bA�oA�{A�VA�bA�JA�VA�VA�bA�JA��A��#A��A��yA��;A���Aغ^Aش9Aش9Aش9Aذ!Aء�AؓuA؋DA؃A�5?Aհ!A�33A�dZAд9A�ȴA��A��A�=qAˉ7A��A��yA�|�A��TA���A�{A�ȴA��A��A��PA�1'A�{A�oA��uA��7A�x�A��A��\A�O�A�&�A���A�n�A��A���A���A�C�A��yA��A��
A�;dA�v�A��FA�&�A��A�ZA�ffA��RA�p�A�A���A�/A�G�A���A��A���A�9XA��A�Q�A�;dA� �A�M�A�A�+A�-A�I�A}�hAw��At1AoK�AiƨAd�`A_�A]�wAXI�AV��AR��AL�AI/AF��AE?}ADv�AB^5AA�AAK�A@��A?VA<~�A;��A:�RA:z�A:=qA:  A9%A7+A5+A3�A2�+A0��A05?A.��A,A+"�A*��A*ZA*�A)�FA)�A'A%��A%x�A#l�A!�;A ��A $�A�A�A�A;dAJAx�A33A�yA��A�9Av�A�A�yA�\AoA��AI�AAl�A�/A�AVA�;A1A��A��A�!A�+Al�AVA��A��A�^AVA�\AƨA7LA
~�A	��A	+A	VA	AĜAQ�A��AK�A�`AA�A{A1A��A33A�A��A�A ��A {A b@��@��T@�hs@��@��m@��
@��@��R@���@�V@�%@���@��@���@��y@��!@�ȴ@�5?@��h@�`B@�z�@�
=@��T@�j@@�~�@�G�@��^@��@�E�@�5?@�@�I�@�l�@�^5@�D@�Z@�A�@�1@���@�+@���@�9@���@�ff@�h@�h@��`@�l�@�{@���@��;@�Z@� �@���@�@�V@�{@��@Ձ@���@��m@�~�@�&�@� �@ύP@��@���@Η�@���@̬@̓u@�r�@�1'@��@˝�@�@�v�@�E�@�-@���@�@�x�@�V@���@�ƨ@ƸR@ũ�@ě�@ÍP@�
=@�@�ff@�E�@�-@��^@��@���@���@��@���@��u@�Z@�(�@�o@���@�X@���@�(�@���@��P@�S�@�o@��@��!@�5?@���@���@��`@� �@��@��@���@�M�@���@�@��@�bN@�(�@��@���@�
=@��^@�/@�Ĝ@���@�z�@�A�@��@��!@�E�@�@���@���@�X@�7L@��`@���@�33@���@��!@�=q@��T@��@�V@��/@��j@��@�r�@��
@���@�v�@�M�@�$�@���@��-@�x�@�G�@��@��9@�j@�1'@�(�@�1@��@�K�@��H@�V@�5?@��#@��^@�`B@���@�Q�@��w@�;d@��y@���@�V@�-@��@��T@��7@�G�@�V@�Ĝ@��@�A�@�1@��w@���@�dZ@��@��\@�ff@��@��@���@��-@���@�x�@�%@��@�bN@�I�@��@���@��@�C�@�
=@�ȴ@��R@���@�ff@�V@�-@��@��#@��h@�x�@�7L@�z�@�A�@�b@��;@���@�S�@��y@���@�ff@�=q@�-@��#@�x�@�G�@�7L@�%@���@��9@��u@�I�@�  @���@�l�@�S�@�;d@��@�@��@���@��+@�{@��#@��-@���@�p�@��@��@�r�@�Z@�9X@�1@��@�C�@�@���@���@��!@���@�M�@�5?@�{@���@��@��@���@��@��@��@�V@�%@�%@��/@���@� �@��F@�"�@�ȴ@��R@���@��\@�M�@���@���@�X@�`B@��@���@�j@�1'@�  @�ƨ@�|�@�S�@�33@�
=@���@�n�@�$�@��-@���@�x�@�X@��@��@���@��9@�A�@�  @�w@~ȴ@}�@}�-@}�@|Z@{�F@{"�@z��@z=q@y�@y��@y7L@y�@y�@x��@x�u@wl�@v�R@v@t��@t�@sdZ@s33@so@s@r��@r~�@r�@q�#@q�^@qX@p�@o�;@o�P@o;d@o+@o
=@n��@n5?@m@mO�@m�@l�@l�j@lz�@l(�@k��@k�@k@j��@jn�@jJ@i��@h��@h��@hA�@g;d@fv�@fV@e�-@d��@d��@dZ@c�m@c"�@b�!@b^5@b=q@a�@a�7@a�@`�`@`�@`A�@`b@_�;@_l�@_�@^�y@^�@^ff@^$�@^@]�-@]`B@]�@\�/@\�D@\I�@\1@[dZ@[33@Z�@Z�!@Z=q@Y��@Yhs@X�`@XbN@XA�@XA�@X �@W�P@W
=@V�y@V�R@V��@Vv�@U�T@U?}@T��@TI�@S�
@S��@SC�@S@R-@Q��@Q�7@Qx�@PĜ@P �@O��@O�w@O�P@N��@NV@N@M�@M?}@L��@LZ@L1@K��@K"�@J�!@Jn�@JJ@Ihs@I&�@H�`@H�u@HA�@G��@G|�@G;d@G
=@F�y@Fv�@E�T@E�h@E?}@D�j@Dz�@D9X@D(�@D�@D1@C�F@Ct�@C"�@B�@BJ@A��@A�@A�#@A�^@@�`@@b@?�@?�@?�@?�@?�@>�y@>�@>V@=�@=�h@=`B@=�@<��@<�D@<(�@;�m@;dZ@;S�@;"�@:��@:��@:-@9hs@8��@8�`@8Ĝ@8�9@8r�@7�;@7�w@7��@7l�@7\)@6��@6��@6��@6E�@6@5�@5p�@5O�@5O�@5/@4�j@4�@4�@4�D@4Z@3�
@3t�@333@3o@2�@2�H@2�!@2n�@2M�@2-@1��@1�#@1&�@1�@0�9@0r�@0b@/�@.��@.�y@.ȴ@.��@.�+@.ff@-�@-@-�h@-p�@-?}@-V@,�j@,�D@,(�@+��@+�m@+ƨ@+ƨ@+��@+"�@+@*�@*�!@*=q@*J@)�#@)�7@)X@)7L@(��@(Ĝ@(Ĝ@(r�@( �@'�;@'�@'�P@'\)@';d@&��@&ff@&E�@&{@%/@$�j@$z�@$j@$Z@$I�@$9X@$�@#�m@#C�@"�H@"��@"n�@"�@!�#@!�7@!7L@ ��@ Ĝ@ Q�@  �@ b@�@�w@|�@K�@;d@;d@+@
=@��@v�@ff@V@5?@�@�-@�@��@�/@�j@�@z�@I�@1@�m@�F@dZ@33@�H@�!@n�@J@�^@x�@hs@hs@%@Ĝ@�@bN@Q�@1'@ �@�;@�@��@|�@�@�+@v�@v�@ff@V@E�@{@��@��@p�@/@�@��@�j@�@��@j@I�@9X@9X@��@�F@�@C�@33@o@�\@M�@-@��@��@��@�7@hs@G�@7L@�`@�9@r�@1'@  @�;@��@|�@l�@
=@ȴ@�R@��@ff@ff@V@5?@{@�-@�@`B@�@��@��@�j@��@�D@I�@�@�m@�F@��@�@dZ@33@
��@
�\@
^5@
J@	��@	�^@	�7@	x�@	hs@	&�@��@Ĝ@��@��@�@Q�@A�@�@�@��@�P@;d@
=@�y@��@ff@{@�T@��@�-@��@�@?}@�@�@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  A���A���A���A�A�%A�1A�1A�
=A�JA�VA�VA�oA�oA�oA�{A�oA��A��A�{A�bA�oA�{A�VA�bA�JA�VA�VA�bA�JA��A��#A��A��yA��;A���Aغ^Aش9Aش9Aش9Aذ!Aء�AؓuA؋DA؃A�5?Aհ!A�33A�dZAд9A�ȴA��A��A�=qAˉ7A��A��yA�|�A��TA���A�{A�ȴA��A��A��PA�1'A�{A�oA��uA��7A�x�A��A��\A�O�A�&�A���A�n�A��A���A���A�C�A��yA��A��
A�;dA�v�A��FA�&�A��A�ZA�ffA��RA�p�A�A���A�/A�G�A���A��A���A�9XA��A�Q�A�;dA� �A�M�A�A�+A�-A�I�A}�hAw��At1AoK�AiƨAd�`A_�A]�wAXI�AV��AR��AL�AI/AF��AE?}ADv�AB^5AA�AAK�A@��A?VA<~�A;��A:�RA:z�A:=qA:  A9%A7+A5+A3�A2�+A0��A05?A.��A,A+"�A*��A*ZA*�A)�FA)�A'A%��A%x�A#l�A!�;A ��A $�A�A�A�A;dAJAx�A33A�yA��A�9Av�A�A�yA�\AoA��AI�AAl�A�/A�AVA�;A1A��A��A�!A�+Al�AVA��A��A�^AVA�\AƨA7LA
~�A	��A	+A	VA	AĜAQ�A��AK�A�`AA�A{A1A��A33A�A��A�A ��A {A b@��@��T@�hs@��@��m@��
@��@��R@���@�V@�%@���@��@���@��y@��!@�ȴ@�5?@��h@�`B@�z�@�
=@��T@�j@@�~�@�G�@��^@��@�E�@�5?@�@�I�@�l�@�^5@�D@�Z@�A�@�1@���@�+@���@�9@���@�ff@�h@�h@��`@�l�@�{@���@��;@�Z@� �@���@�@�V@�{@��@Ձ@���@��m@�~�@�&�@� �@ύP@��@���@Η�@���@̬@̓u@�r�@�1'@��@˝�@�@�v�@�E�@�-@���@�@�x�@�V@���@�ƨ@ƸR@ũ�@ě�@ÍP@�
=@�@�ff@�E�@�-@��^@��@���@���@��@���@��u@�Z@�(�@�o@���@�X@���@�(�@���@��P@�S�@�o@��@��!@�5?@���@���@��`@� �@��@��@���@�M�@���@�@��@�bN@�(�@��@���@�
=@��^@�/@�Ĝ@���@�z�@�A�@��@��!@�E�@�@���@���@�X@�7L@��`@���@�33@���@��!@�=q@��T@��@�V@��/@��j@��@�r�@��
@���@�v�@�M�@�$�@���@��-@�x�@�G�@��@��9@�j@�1'@�(�@�1@��@�K�@��H@�V@�5?@��#@��^@�`B@���@�Q�@��w@�;d@��y@���@�V@�-@��@��T@��7@�G�@�V@�Ĝ@��@�A�@�1@��w@���@�dZ@��@��\@�ff@��@��@���@��-@���@�x�@�%@��@�bN@�I�@��@���@��@�C�@�
=@�ȴ@��R@���@�ff@�V@�-@��@��#@��h@�x�@�7L@�z�@�A�@�b@��;@���@�S�@��y@���@�ff@�=q@�-@��#@�x�@�G�@�7L@�%@���@��9@��u@�I�@�  @���@�l�@�S�@�;d@��@�@��@���@��+@�{@��#@��-@���@�p�@��@��@�r�@�Z@�9X@�1@��@�C�@�@���@���@��!@���@�M�@�5?@�{@���@��@��@���@��@��@��@�V@�%@�%@��/@���@� �@��F@�"�@�ȴ@��R@���@��\@�M�@���@���@�X@�`B@��@���@�j@�1'@�  @�ƨ@�|�@�S�@�33@�
=@���@�n�@�$�@��-@���@�x�@�X@��@��@���@��9@�A�@�  @�w@~ȴ@}�@}�-@}�@|Z@{�F@{"�@z��@z=q@y�@y��@y7L@y�@y�@x��@x�u@wl�@v�R@v@t��@t�@sdZ@s33@so@s@r��@r~�@r�@q�#@q�^@qX@p�@o�;@o�P@o;d@o+@o
=@n��@n5?@m@mO�@m�@l�@l�j@lz�@l(�@k��@k�@k@j��@jn�@jJ@i��@h��@h��@hA�@g;d@fv�@fV@e�-@d��@d��@dZ@c�m@c"�@b�!@b^5@b=q@a�@a�7@a�@`�`@`�@`A�@`b@_�;@_l�@_�@^�y@^�@^ff@^$�@^@]�-@]`B@]�@\�/@\�D@\I�@\1@[dZ@[33@Z�@Z�!@Z=q@Y��@Yhs@X�`@XbN@XA�@XA�@X �@W�P@W
=@V�y@V�R@V��@Vv�@U�T@U?}@T��@TI�@S�
@S��@SC�@S@R-@Q��@Q�7@Qx�@PĜ@P �@O��@O�w@O�P@N��@NV@N@M�@M?}@L��@LZ@L1@K��@K"�@J�!@Jn�@JJ@Ihs@I&�@H�`@H�u@HA�@G��@G|�@G;d@G
=@F�y@Fv�@E�T@E�h@E?}@D�j@Dz�@D9X@D(�@D�@D1@C�F@Ct�@C"�@B�@BJ@A��@A�@A�#@A�^@@�`@@b@?�@?�@?�@?�@?�@>�y@>�@>V@=�@=�h@=`B@=�@<��@<�D@<(�@;�m@;dZ@;S�@;"�@:��@:��@:-@9hs@8��@8�`@8Ĝ@8�9@8r�@7�;@7�w@7��@7l�@7\)@6��@6��@6��@6E�@6@5�@5p�@5O�@5O�@5/@4�j@4�@4�@4�D@4Z@3�
@3t�@333@3o@2�@2�H@2�!@2n�@2M�@2-@1��@1�#@1&�@1�@0�9@0r�@0b@/�@.��@.�y@.ȴ@.��@.�+@.ff@-�@-@-�h@-p�@-?}@-V@,�j@,�D@,(�@+��@+�m@+ƨ@+ƨ@+��@+"�@+@*�@*�!@*=q@*J@)�#@)�7@)X@)7L@(��@(Ĝ@(Ĝ@(r�@( �@'�;@'�@'�P@'\)@';d@&��@&ff@&E�@&{@%/@$�j@$z�@$j@$Z@$I�@$9X@$�@#�m@#C�@"�H@"��@"n�@"�@!�#@!�7@!7L@ ��@ Ĝ@ Q�@  �@ b@�@�w@|�@K�@;d@;d@+@
=@��@v�@ff@V@5?@�@�-@�@��@�/@�j@�@z�@I�@1@�m@�F@dZ@33@�H@�!@n�@J@�^@x�@hs@hs@%@Ĝ@�@bN@Q�@1'@ �@�;@�@��@|�@�@�+@v�@v�@ff@V@E�@{@��@��@p�@/@�@��@�j@�@��@j@I�@9X@9X@��@�F@�@C�@33@o@�\@M�@-@��@��@��@�7@hs@G�@7L@�`@�9@r�@1'@  @�;@��@|�@l�@
=@ȴ@�R@��@ff@ff@V@5?@{@�-@�@`B@�@��@��@�j@��@�D@I�@�@�m@�F@��@�@dZ@33@
��@
�\@
^5@
J@	��@	�^@	�7@	x�@	hs@	&�@��@Ĝ@��@��@�@Q�@A�@�@�@��@�P@;d@
=@�y@��@ff@{@�T@��@�-@��@�@?}@�@�@��@��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�VB�VB�VB�VB�VB�VB�VB�VB�VB�VB�VB�VB�VB�\B�VB�\B�VB�VB�\B�\B�\B�\B�bB�\B�bB�bB�bB�bB�bB�hB�oB�hB�hB�oB�oB�uB�uB�uB�uB�uB�uB�uB�uB�oB�{B�-B�5B�B+B)�B/B:^BG�BM�BN�BN�BS�BR�BL�BC�B<jB,B"�B �B!�B �B#�B#�B'�B)�B)�BJ�BC�B<jB5?B:^B?}B>wB<jB6FB"�B�BB�B��B�jB�-B�3B��B�%B^5B?}B�B
�;B
��B
ȴB
�LB
��B
��B
�PB
�B
q�B
_;B
F�B
7LB
2-B
%�B
�B
JB	��B	�B	�qB	��B	�B	iyB	N�B	A�B	-B	 �B	{B	  B��B�B�BB�)B�5B�#B�B�B��B�B��B�B�
B�B�B�/B�ZB�TB�mB�sB�fB�BB�5B�B�/B�HB�mB�yB�B�B��B��B��B��B�B�B��B��B	  B	B	B	B	DB	{B	�B	�B	!�B	"�B	"�B	 �B	�B	�B	�B	�B	�B	�B	�B	�B	%�B	D�B	W
B	o�B	u�B	z�B	v�B	s�B	u�B	v�B	u�B	x�B	x�B	u�B	u�B	u�B	v�B	v�B	y�B	z�B	z�B	y�B	x�B	}�B	�B	�B	�B	� B	�B	�B	�B	}�B	z�B	u�B	k�B	hsB	p�B	m�B	iyB	hsB	gmB	e`B	iyB	m�B	m�B	n�B	l�B	o�B	p�B	r�B	r�B	s�B	t�B	w�B	z�B	z�B	{�B	z�B	v�B	s�B	p�B	o�B	n�B	m�B	t�B	�%B	�B	�B	�B	�B	�B	�B	� B	�B	�+B	�7B	�PB	�hB	�\B	�DB	�=B	�1B	�+B	�+B	�+B	�B	� B	�B	� B	�%B	�DB	�B	}�B	{�B	|�B	|�B	}�B	�B	�B	�B	� B	}�B	{�B	}�B	�B	�B	�B	�B	�B	�%B	�=B	�=B	�7B	�7B	�=B	�=B	�=B	�=B	�=B	�DB	�JB	�VB	�oB	�bB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�'B	�'B	�'B	�'B	�'B	�'B	�-B	�3B	�9B	�?B	�LB	�XB	�jB	�}B	��B	�}B	��B	ŢB	ĜB	ĜB	ĜB	ĜB	ƨB	ȴB	ɺB	ɺB	ɺB	ǮB	ǮB	ƨB	ƨB	ƨB	ƨB	ǮB	ɺB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�/B	�/B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�ZB	�`B	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
1B
1B
1B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B
DB
DB
JB
PB
PB
PB
VB
VB
\B
\B
\B
\B
\B
\B
\B
\B
bB
bB
hB
bB
hB
oB
oB
oB
oB
oB
oB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
#�B
#�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
+B
,B
,B
-B
.B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
49B
49B
49B
5?B
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
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
W
B
XB
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
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
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
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
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
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
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
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
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  B�VB�VB�VB�VB�VB�VB�VB�VB�VB�VB�VB�VB�VB�\B�VB�\B�VB�VB�\B�\B�\B�\B�bB�\B�bB�bB�bB�bB�bB�hB�oB�hB�hB�oB�oB�uB�uB�uB�uB�uB�uB�uB�uB�oB�{B�-B�5B�B+B)�B/B:^BG�BM�BN�BN�BS�BR�BL�BC�B<jB,B"�B �B!�B �B#�B#�B'�B)�B)�BJ�BC�B<jB5?B:^B?}B>wB<jB6FB"�B�BB�B��B�jB�-B�3B��B�%B^5B?}B�B
�;B
��B
ȴB
�LB
��B
��B
�PB
�B
q�B
_;B
F�B
7LB
2-B
%�B
�B
JB	��B	�B	�qB	��B	�B	iyB	N�B	A�B	-B	 �B	{B	  B��B�B�BB�)B�5B�#B�B�B��B�B��B�B�
B�B�B�/B�ZB�TB�mB�sB�fB�BB�5B�B�/B�HB�mB�yB�B�B��B��B��B��B�B�B��B��B	  B	B	B	B	DB	{B	�B	�B	!�B	"�B	"�B	 �B	�B	�B	�B	�B	�B	�B	�B	�B	%�B	D�B	W
B	o�B	u�B	z�B	v�B	s�B	u�B	v�B	u�B	x�B	x�B	u�B	u�B	u�B	v�B	v�B	y�B	z�B	z�B	y�B	x�B	}�B	�B	�B	�B	� B	�B	�B	�B	}�B	z�B	u�B	k�B	hsB	p�B	m�B	iyB	hsB	gmB	e`B	iyB	m�B	m�B	n�B	l�B	o�B	p�B	r�B	r�B	s�B	t�B	w�B	z�B	z�B	{�B	z�B	v�B	s�B	p�B	o�B	n�B	m�B	t�B	�%B	�B	�B	�B	�B	�B	�B	� B	�B	�+B	�7B	�PB	�hB	�\B	�DB	�=B	�1B	�+B	�+B	�+B	�B	� B	�B	� B	�%B	�DB	�B	}�B	{�B	|�B	|�B	}�B	�B	�B	�B	� B	}�B	{�B	}�B	�B	�B	�B	�B	�B	�%B	�=B	�=B	�7B	�7B	�=B	�=B	�=B	�=B	�=B	�DB	�JB	�VB	�oB	�bB	�bB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�'B	�'B	�'B	�'B	�'B	�'B	�'B	�-B	�3B	�9B	�?B	�LB	�XB	�jB	�}B	��B	�}B	��B	ŢB	ĜB	ĜB	ĜB	ĜB	ƨB	ȴB	ɺB	ɺB	ɺB	ǮB	ǮB	ƨB	ƨB	ƨB	ƨB	ǮB	ɺB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�)B	�)B	�)B	�/B	�/B	�/B	�/B	�5B	�;B	�BB	�BB	�HB	�NB	�TB	�ZB	�`B	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
+B
+B
1B
1B
1B
1B
1B
1B
	7B
	7B

=B

=B

=B

=B
DB
DB
JB
PB
PB
PB
VB
VB
\B
\B
\B
\B
\B
\B
\B
\B
bB
bB
hB
bB
hB
oB
oB
oB
oB
oB
oB
oB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
#�B
$�B
#�B
#�B
$�B
$�B
%�B
&�B
&�B
&�B
&�B
'�B
'�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
+B
+B
+B
+B
+B
,B
,B
-B
.B
/B
/B
/B
/B
/B
/B
/B
/B
/B
/B
0!B
1'B
1'B
1'B
1'B
2-B
2-B
33B
33B
49B
49B
49B
5?B
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
7LB
8RB
8RB
8RB
8RB
8RB
8RB
9XB
9XB
9XB
:^B
:^B
:^B
:^B
:^B
;dB
;dB
;dB
;dB
<jB
=qB
=qB
=qB
=qB
=qB
>wB
>wB
>wB
>wB
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
E�B
E�B
F�B
F�B
F�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
J�B
I�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
L�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
T�B
T�B
T�B
T�B
T�B
VB
VB
VB
VB
VB
W
B
XB
XB
XB
XB
YB
YB
YB
YB
YB
YB
YB
YB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
\)B
\)B
\)B
\)B
\)B
\)B
]/B
\)B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
_;B
_;B
`BB
`BB
`BB
`BB
`BB
`BB
`BB
`BB
aHB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
bNB
cTB
dZB
dZB
dZB
dZB
dZB
e`B
e`B
e`B
e`B
ffB
ffB
ffB
ffB
ffB
gmB
gmB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
hsB
hsB
iyB
iyB
iyB
iyB
iyB
iyB
jB
jB
jB
jB
jB
k�B
k�B
k�B
k�B
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
n�B
n�B
n�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
o�B
o�B
p�B
p�B
p�B
p�B
p�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
q�B
r�B
r�B
r�B
r�B
r�B
s�B
s�B
s�B
t�B
t�B
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
v�B
v�B
v�B
v�B
v�B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
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
y�B
y�B
y�B
z�B
z�B
z�B
z�B
z�B
z�B
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
|�B
|�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
� B
� B
� B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.36 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20221121120116                              AO  ARCAADJP                                                                    20221121120116    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20221121120116  QCP$                G�O�G�O�G�O�1F83E           AO  ARGQQCPL                                                                    20221121120116  QCF$                G�O�G�O�G�O�4000            