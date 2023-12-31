CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-05-29T10:00:33Z creation      
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
_FillValue                    �\Argo profile    3.1 1.2 19500101000000  20200529100033  20200529100033  5906156 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               )A   AO  7912                            2B  A   NAVIS_A                         1020                            170425                          863 @����1   @�ww��@8�=p��
�cF~��"�1   GPS     Primary sampling: averaged []                                                                                                                                                                                                                                      )A   A   A   @�  @�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C�C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn�Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�	�D�)�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@���@���@���A�GA8z�AXz�Axz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B�B�B�B&�B.�B6�B>�BF�BN�BV�B^�Bf�Bn�Bv�B~�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C��C��C�HC��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm�HCo��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D a�D ��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��D	a�D	��D
a�D
��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��D a�D ��D!a�D!��D"a�D"��D#a�D#��D$a�D$��D%a�D%��D&a�D&��D'a�D'��D(a�D(��D)a�D)��D*a�D*��D+a�D+��D,a�D,��D-a�D-��D.a�D.��D/a�D/��D0a�D0��D1a�D1��D2a�D2��D3a�D3��D4a�D4��D5a�D5��D6a�D6��D7a�D7��D8a�D8��D9a�D9��D:a�D:��D;a�D;��D<a�D<��D=a�D=��D>a�D>��D?a�D?��D@a�D@��DAa�DA��DBa�DB��DCa�DC��DDa�DD��DEa�DE��DFa�DF��DGa�DG��DHa�DH��DIa�DI��DJa�DJ��DKa�DK��DLa�DL��DMa�DM��DNa�DN��DOa�DO��DPa�DP��DQa�DQ��DRa�DR��DSa�DS��DTa�DT��DUa�DU��DVa�DV��DWa�DW��DXa�DX��DYa�DY��DZa�DZ��D[a�D[��D\a�D\��D]a�D]��D^a�D^��D_a�D_��D`a�D`��Daa�Da��Dba�Db��Dca�Dc��Dda�Dd��Dea�De��Dfa�Df��Dga�Dg��Dha�Dh��Dia�Di��Dja�Dj��Dka�Dk��Dla�Dl��Dma�Dm��Dna�Dn��Doa�Do��Dpa�Dp��Dqa�Dq��Dra�Dr��Dsa�Ds��Dta�Dt��Dua�Du��Dva�Dv��Dwa�Dw��Dxa�Dx��Dya�Dy��Dza�Dz��D{a�D{��D|a�D|��D}a�D}��D~a�D~��Da�D��D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D°�D���D�0�D�p�Dð�D���D�0�D�p�Dİ�D���D�0�D�p�DŰ�D���D�0�D�p�Dư�D���D�0�D�p�Dǰ�D���D�0�D�p�DȰ�D���D�0�D�p�Dɰ�D���D�0�D�p�Dʰ�D���D�0�D�p�D˰�D���D�0�D�p�D̰�D���D�0�D�p�DͰ�D���D�0�D�p�Dΰ�D���D�0�D�p�Dϰ�D���D�0�D�p�Dа�D���D�0�D�p�DѰ�D���D�0�D�p�DҰ�D���D�0�D�p�DӰ�D���D�0�D�p�D԰�D���D�0�D�p�Dհ�D���D�0�D�p�Dְ�D���D�0�D�p�Dװ�D���D�0�D�p�Dذ�D���D�0�D�p�Dٰ�D���D�0�D�p�Dڰ�D���D�0�D�p�D۰�D���D�0�D�p�Dܰ�D���D�0�D�p�Dݰ�D���D�0�D�p�Dް�D���D�0�D�p�D߰�D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D���D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���Aå�A�p�A�9XA�%A¼jA�VA�bA�A���A���A���A�K�A��`A��A�I�A���A���A�^5A��A�^5A�l�A���A���A���A�9XA���A�l�A��A���A�E�A�{A�
=A��7A�A�A���A�C�A��A�C�A��^A�\)A��A�ȴA��9A���A�/A��jA��A��A���A��TA�ȴA��FA�~�A�M�A�VA���A�O�A���A���A��DA�E�A��A���A�bNA��RA�
=A��RA���A�\)A��A��A�Q�A��A�XA��uA���A��#A�G�A�l�A�S�A�K�A��A��A��/A���A�G�A��A��^A��A�1'A�C�A�n�A���A�M�A��FA�=qA�"�A���A��A�A��A��+A�x�A��RA���A�JA�ȴA��A�bA��uA��A�1'A�JA�VA�VA��A�A�A���A��A}�;AzĜAy��AxM�Au��Ar�`Aq`BAo33An-Amt�AlbNAkp�Aj�AgO�Ac�-Ab�\Aa��A`��A^  AZ�+AX�uAV�AU/AS�AS"�AR��AQ��AQ�AO`BAM�TAL��AK��AJ�HAHVADM�AB��A@$�A?�A>��A=��A="�A<I�A;��A;t�A:ȴA9
=A6r�A5��A4$�A3&�A2ȴA1A0��A0$�A.bNA.bA-7LA+C�A*�HA*ȴA)�
A)x�A(��A( �A'K�A%�#A$�HA$  A"v�A!`BA�AA�AȴA��A�wA�Ar�A��A`BA�RA��AĜAA�`AI�A��A�A�!AjAM�A��A�AM�A�A�A��A��A�A�A
^5A
�A�9A�#AVA��AAK�A��A1A/A��Az�A�
A ��A ��A V@��
@�V@�$�@��^@��D@��@��j@���@�j@�ȴ@���@�j@�@�Z@�K�@�5?@�Q�@�E�@�z�@��y@�hs@�9@���@�"�@��@�ff@��/@�"�@�@�Q�@ۍP@�A�@��@�V@ӶF@�+@�$�@�p�@�&�@���@�S�@�n�@���@���@�"�@ɩ�@� �@�t�@�
=@�@ċD@��m@�+@���@�-@�X@��D@� �@�b@�S�@�K�@�~�@��@�A�@�o@���@�-@�A�@�K�@�5?@�x�@�bN@���@�{@�~�@�E�@�`B@��`@�1'@���@���@��D@�r�@��@�Ĝ@��@��H@��\@�hs@��`@�Z@�|�@��7@�9X@��\@�@�p�@��u@�ƨ@�\)@� �@�r�@�b@��D@���@�1'@�Q�@�(�@��@���@�5?@�p�@�G�@�(�@�|�@���@��@��
@�V@���@�O�@��/@�1'@���@�l�@�
=@��H@���@�=q@���@�`B@�?}@��@��`@��9@��u@�I�@�1@��@�j@��!@�9X@�@���@��@�%@��@��j@���@�9X@� �@�ƨ@���@���@��P@�\)@�+@��y@�ff@�J@�@��@�1@��F@���@�dZ@��@�v�@�=q@���@��-@�hs@�bN@�  @��P@�t�@�|�@�t�@�;d@�dZ@�dZ@�o@��@�@�X@���@���@���@��/@�bN@���@���@�=q@��#@�hs@�7L@��@��j@�j@��@�j@�1'@��H@�ff@�V@�-@�M�@���@��@��@�z�@�A�@��
@��@��@�z�@�/@�z�@�Ĝ@���@���@��@���@���@���@�z�@�A�@�1'@��
@��y@���@���@���@���@���@���@�9X@\)@� �@�V@���@�7L@�x�@���@�`B@�G�@�`B@�p�@�O�@�&�@��@�%@��@�(�@�w@~�@~$�@}��@}�@}?}@}O�@}�T@}��@}�@|z�@|9X@{�m@{C�@z�H@z��@z��@{@z�H@z�\@zJ@y�@x�9@xbN@xb@w��@wK�@w�@v�@v�R@v��@vv�@v�+@v{@up�@u�@t�@t��@t9X@tz�@t�@t�D@t9X@sS�@sC�@sC�@so@r�!@q�^@p�`@pQ�@p1'@p1'@o�@n�y@m��@mp�@mV@l�@l��@l�D@j��@j�!@j��@j^5@jM�@j=q@jM�@j��@k33@ko@j�@j��@j�\@jM�@i��@i&�@hĜ@h�@g�;@g�@f5?@e�h@e?}@d�D@c�m@b��@a�^@a�^@b�@a�^@a�@`��@`��@`bN@`A�@`1'@_�;@_|�@_l�@_�@^��@^�+@^ff@^@]�@]@]@]��@]p�@]�@]`B@]�@]�T@^��@^ȴ@_�@^�@^�+@^�+@^�+@^v�@^V@^E�@^5?@]�@]p�@]�@\z�@\(�@\9X@\9X@\(�@[��@[�F@[�@[t�@Z�!@X  @W
=@V�R@V�R@V�R@V�y@W�@W|�@W��@W��@W�@W�P@V{@T��@T9X@S��@S�@S@R=q@R�@R-@R-@RJ@R�@Rn�@R�\@R��@R��@R��@R��@Rn�@R�@Q��@Q��@Q7L@P��@PQ�@P �@O�;@O��@OK�@O;d@O
=@N��@NV@M�@L1@J��@I��@I�@I��@IG�@HĜ@H�u@H1'@Hb@H  @G�;@G|�@F��@F��@F$�@F@F5?@F5?@FE�@Fff@GK�@G+@F�@F5?@E�h@D�/@DI�@C�
@C�@CC�@Co@B�@B�@B�H@A�^@@Q�@?�;@?��@?l�@>��@>�y@>��@?+@>5?@=/@=O�@=p�@=@>$�@>{@=�@=p�@=O�@=O�@=`B@<��@;ƨ@;o@:~�@9�@8  @7\)@7;d@6�@6��@6�+@6E�@6{@6@5�T@5��@5�@5p�@5`B@5?}@4�/@4��@4j@4I�@4�@3��@3ƨ@3�@3dZ@3C�@3o@2�!@2-@2�@1�@1X@1&�@0�`@0��@0r�@0bN@0A�@01'@0  @/�w@/�w@/�w@/l�@/\)@/K�@/K�@/+@.�@.v�@.V@.E�@-�h@,��@,9X@+�m@+t�@*�@*��@*~�@*^5@)�#@)x�@)hs@)hs@)hs@)G�@)7L@)7L@)7L@)�@(��@(Ĝ@(��@(bN@(1'@( �@(  @'�@'�;@'��@'�w@'��@'|�@'K�@'+@&��@&�y@&�@&ȴ@&��@&v�@&V@%��@%�h@%�@%?}@%V@$��@$j@$9X@$�@$1@#�
@#dZ@#@"��@"��@"��@"~�@"n�@"=q@!��@!�^@!��@!�7@!X@!&�@ Ĝ@ �u@  �@   @�@�w@��@��@�P@;d@
=@�y@v�@�T@�@?}@��@�@1@��@ƨ@33@@�@�@�H@�!@~�@^5@-@�#@�7@x�@hs@x�@X@7L@&�@%@�u@bN@A�@1'@ �@��@|�@\)@
=@�@�R@�R@��@E�@@�@�@�T@��@��@@�h@O�@�@��@��@I�@(�@�@��@�
@t�@�@��@�!@n�@M�@-@��@�@�^@�7@hs@��@�`@��@��@Ĝ@�9@��@1'@�P@;d@��@�y@�R@$�@�T@�-@��@p�@/@�/@�j@�j@�j@�@�D@Z@I�@9X@(�@�@��@�
@�F@��@S�@33@33@"�@@
�H@
��@
��@
�\@
^5@
-@
�@
J@	��@	��@	hs@	G�@	7L@	G�@	7L@�`@�9@��@�u@�u@bN@A�@b@��@|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A���A���A���Aå�A�p�A�9XA�%A¼jA�VA�bA�A���A���A���A�K�A��`A��A�I�A���A���A�^5A��A�^5A�l�A���A���A���A�9XA���A�l�A��A���A�E�A�{A�
=A��7A�A�A���A�C�A��A�C�A��^A�\)A��A�ȴA��9A���A�/A��jA��A��A���A��TA�ȴA��FA�~�A�M�A�VA���A�O�A���A���A��DA�E�A��A���A�bNA��RA�
=A��RA���A�\)A��A��A�Q�A��A�XA��uA���A��#A�G�A�l�A�S�A�K�A��A��A��/A���A�G�A��A��^A��A�1'A�C�A�n�A���A�M�A��FA�=qA�"�A���A��A�A��A��+A�x�A��RA���A�JA�ȴA��A�bA��uA��A�1'A�JA�VA�VA��A�A�A���A��A}�;AzĜAy��AxM�Au��Ar�`Aq`BAo33An-Amt�AlbNAkp�Aj�AgO�Ac�-Ab�\Aa��A`��A^  AZ�+AX�uAV�AU/AS�AS"�AR��AQ��AQ�AO`BAM�TAL��AK��AJ�HAHVADM�AB��A@$�A?�A>��A=��A="�A<I�A;��A;t�A:ȴA9
=A6r�A5��A4$�A3&�A2ȴA1A0��A0$�A.bNA.bA-7LA+C�A*�HA*ȴA)�
A)x�A(��A( �A'K�A%�#A$�HA$  A"v�A!`BA�AA�AȴA��A�wA�Ar�A��A`BA�RA��AĜAA�`AI�A��A�A�!AjAM�A��A�AM�A�A�A��A��A�A�A
^5A
�A�9A�#AVA��AAK�A��A1A/A��Az�A�
A ��A ��A V@��
@�V@�$�@��^@��D@��@��j@���@�j@�ȴ@���@�j@�@�Z@�K�@�5?@�Q�@�E�@�z�@��y@�hs@�9@���@�"�@��@�ff@��/@�"�@�@�Q�@ۍP@�A�@��@�V@ӶF@�+@�$�@�p�@�&�@���@�S�@�n�@���@���@�"�@ɩ�@� �@�t�@�
=@�@ċD@��m@�+@���@�-@�X@��D@� �@�b@�S�@�K�@�~�@��@�A�@�o@���@�-@�A�@�K�@�5?@�x�@�bN@���@�{@�~�@�E�@�`B@��`@�1'@���@���@��D@�r�@��@�Ĝ@��@��H@��\@�hs@��`@�Z@�|�@��7@�9X@��\@�@�p�@��u@�ƨ@�\)@� �@�r�@�b@��D@���@�1'@�Q�@�(�@��@���@�5?@�p�@�G�@�(�@�|�@���@��@��
@�V@���@�O�@��/@�1'@���@�l�@�
=@��H@���@�=q@���@�`B@�?}@��@��`@��9@��u@�I�@�1@��@�j@��!@�9X@�@���@��@�%@��@��j@���@�9X@� �@�ƨ@���@���@��P@�\)@�+@��y@�ff@�J@�@��@�1@��F@���@�dZ@��@�v�@�=q@���@��-@�hs@�bN@�  @��P@�t�@�|�@�t�@�;d@�dZ@�dZ@�o@��@�@�X@���@���@���@��/@�bN@���@���@�=q@��#@�hs@�7L@��@��j@�j@��@�j@�1'@��H@�ff@�V@�-@�M�@���@��@��@�z�@�A�@��
@��@��@�z�@�/@�z�@�Ĝ@���@���@��@���@���@���@�z�@�A�@�1'@��
@��y@���@���@���@���@���@���@�9X@\)@� �@�V@���@�7L@�x�@���@�`B@�G�@�`B@�p�@�O�@�&�@��@�%@��@�(�@�w@~�@~$�@}��@}�@}?}@}O�@}�T@}��@}�@|z�@|9X@{�m@{C�@z�H@z��@z��@{@z�H@z�\@zJ@y�@x�9@xbN@xb@w��@wK�@w�@v�@v�R@v��@vv�@v�+@v{@up�@u�@t�@t��@t9X@tz�@t�@t�D@t9X@sS�@sC�@sC�@so@r�!@q�^@p�`@pQ�@p1'@p1'@o�@n�y@m��@mp�@mV@l�@l��@l�D@j��@j�!@j��@j^5@jM�@j=q@jM�@j��@k33@ko@j�@j��@j�\@jM�@i��@i&�@hĜ@h�@g�;@g�@f5?@e�h@e?}@d�D@c�m@b��@a�^@a�^@b�@a�^@a�@`��@`��@`bN@`A�@`1'@_�;@_|�@_l�@_�@^��@^�+@^ff@^@]�@]@]@]��@]p�@]�@]`B@]�@]�T@^��@^ȴ@_�@^�@^�+@^�+@^�+@^v�@^V@^E�@^5?@]�@]p�@]�@\z�@\(�@\9X@\9X@\(�@[��@[�F@[�@[t�@Z�!@X  @W
=@V�R@V�R@V�R@V�y@W�@W|�@W��@W��@W�@W�P@V{@T��@T9X@S��@S�@S@R=q@R�@R-@R-@RJ@R�@Rn�@R�\@R��@R��@R��@R��@Rn�@R�@Q��@Q��@Q7L@P��@PQ�@P �@O�;@O��@OK�@O;d@O
=@N��@NV@M�@L1@J��@I��@I�@I��@IG�@HĜ@H�u@H1'@Hb@H  @G�;@G|�@F��@F��@F$�@F@F5?@F5?@FE�@Fff@GK�@G+@F�@F5?@E�h@D�/@DI�@C�
@C�@CC�@Co@B�@B�@B�H@A�^@@Q�@?�;@?��@?l�@>��@>�y@>��@?+@>5?@=/@=O�@=p�@=@>$�@>{@=�@=p�@=O�@=O�@=`B@<��@;ƨ@;o@:~�@9�@8  @7\)@7;d@6�@6��@6�+@6E�@6{@6@5�T@5��@5�@5p�@5`B@5?}@4�/@4��@4j@4I�@4�@3��@3ƨ@3�@3dZ@3C�@3o@2�!@2-@2�@1�@1X@1&�@0�`@0��@0r�@0bN@0A�@01'@0  @/�w@/�w@/�w@/l�@/\)@/K�@/K�@/+@.�@.v�@.V@.E�@-�h@,��@,9X@+�m@+t�@*�@*��@*~�@*^5@)�#@)x�@)hs@)hs@)hs@)G�@)7L@)7L@)7L@)�@(��@(Ĝ@(��@(bN@(1'@( �@(  @'�@'�;@'��@'�w@'��@'|�@'K�@'+@&��@&�y@&�@&ȴ@&��@&v�@&V@%��@%�h@%�@%?}@%V@$��@$j@$9X@$�@$1@#�
@#dZ@#@"��@"��@"��@"~�@"n�@"=q@!��@!�^@!��@!�7@!X@!&�@ Ĝ@ �u@  �@   @�@�w@��@��@�P@;d@
=@�y@v�@�T@�@?}@��@�@1@��@ƨ@33@@�@�@�H@�!@~�@^5@-@�#@�7@x�@hs@x�@X@7L@&�@%@�u@bN@A�@1'@ �@��@|�@\)@
=@�@�R@�R@��@E�@@�@�@�T@��@��@@�h@O�@�@��@��@I�@(�@�@��@�
@t�@�@��@�!@n�@M�@-@��@�@�^@�7@hs@��@�`@��@��@Ĝ@�9@��@1'@�P@;d@��@�y@�R@$�@�T@�-@��@p�@/@�/@�j@�j@�j@�@�D@Z@I�@9X@(�@�@��@�
@�F@��@S�@33@33@"�@@
�H@
��@
��@
�\@
^5@
-@
�@
J@	��@	��@	hs@	G�@	7L@	G�@	7L@�`@�9@��@�u@�u@bN@A�@b@��@|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B��B�B�B�B�!B�!B�B��B��B��B��B��B��B��B��B��B��B�-B�jB��B��B�3B�B�B��B��B��B��B��B�B�RB�wB��BȴBȴB��B��BɺB��B�NB�yB�B��B��B��BB+B	7B\BbBhBoBuB�B�B�B&�B/B5?B9XB=qBC�BI�BJ�BP�BYBdZBhsBiyBl�Bu�Bz�B}�B� B�B~�Br�BbNBYBQ�BD�B6FB/B$�B�B\B1BB�B�B�5B��BƨB�wB�^B�?B�B��B��B��Bz�BffB]/BP�BA�B-BB
�B
�qB
�3B
�B
��B
�{B
�+B
w�B
t�B
gmB
]/B
R�B
J�B
>wB
+B
"�B
�B
B	�B	�BB	��B	ȴB	��B	�jB	�9B	�B	��B	�DB	�B	~�B	x�B	l�B	\)B	N�B	C�B	<jB	6FB	0!B	/B	.B	)�B	#�B	�B	{B	\B	JB��B�
B��B�^B�LB�3B�'B�B�B�B��B��B��B��B��B�oB�\B�PB�JB�%B�%B}�B{�Bz�Bt�Br�Bq�Br�Bp�Bp�Bn�Bm�Bl�BjBiyBgmBe`BaHB_;BaHB`BB_;B^5B]/B\)B[#B[#B[#B[#BZBZBXBXBXBXBXBXBXBW
BVBVBT�BS�BVBT�BS�BT�BS�BT�BS�BT�BT�BW
BW
BVBW
BVBW
BVBW
BXBYBYBZBXBXBXBXBVBT�BR�BQ�BO�BP�BT�BXB]/B^5B\)B\)BW
BS�BN�BI�BI�BK�BL�BN�BO�BR�BP�BM�BG�BC�BB�B<jB:^B7LB7LB7LB9XB;dB<jB<jB<jB=qB=qB<jB<jB=qB=qB>wB?}BC�BD�BD�BE�BI�BM�BL�BP�BW
BYB_;B_;BbNBcTBaHBaHBbNB`BBbNBdZBcTBbNBaHBaHBffBjBjBk�Bn�Bo�Bo�Br�Bt�B}�B�B�\B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�FB�XB�^B�}BBÖBǮBɺBɺBƨBǮBƨBɺBɺB��B��BɺBǮBŢBƨBƨBȴB��B��B��B��B��B��B�
B�)B�;B�BB�NB�ZB�mB�yB�B��B��B	B	hB	�B	-B	1'B	49B	7LB	7LB	7LB	8RB	:^B	;dB	@�B	A�B	B�B	B�B	D�B	E�B	E�B	H�B	J�B	K�B	L�B	O�B	O�B	O�B	P�B	Q�B	S�B	S�B	T�B	T�B	VB	T�B	VB	W
B	YB	YB	ZB	]/B	`BB	bNB	cTB	cTB	cTB	aHB	bNB	dZB	iyB	k�B	m�B	n�B	n�B	o�B	p�B	s�B	v�B	w�B	w�B	y�B	x�B	}�B	�B	~�B	|�B	~�B	�B	�B	�B	�+B	�7B	�DB	�JB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�-B	�3B	�?B	�FB	�LB	�RB	�LB	�FB	�-B	�-B	�?B	�qB	�}B	��B	ÖB	ǮB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�)B	�/B	�5B	�5B	�5B	�;B	�;B	�;B	�HB	�`B	�fB	�fB	�mB	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B
  B
  B
B
B
B
B
B
%B
%B
%B
%B
B
%B
%B
%B
%B
%B
%B
%B
+B
+B
+B
1B
1B
+B
+B
	7B

=B

=B

=B

=B
DB
JB
PB
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
bB
hB
oB
uB
{B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
!�B
�B
�B
�B
�B
�B
�B
!�B
#�B
$�B
%�B
%�B
%�B
%�B
"�B
!�B
!�B
 �B
 �B
�B
�B
 �B
"�B
#�B
$�B
'�B
(�B
)�B
+B
,B
-B
-B
.B
.B
.B
/B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
1'B
1'B
.B
.B
.B
.B
-B
-B
-B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
33B
8RB
8RB
8RB
9XB
9XB
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
<jB
;dB
:^B
:^B
9XB
9XB
:^B
;dB
;dB
:^B
;dB
;dB
=qB
?}B
@�B
?}B
?}B
@�B
A�B
B�B
C�B
C�B
C�B
B�B
A�B
?}B
>wB
?}B
?}B
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
T�B
T�B
VB
VB
VB
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
W
B
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
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
^5B
_;B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
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
iyB
iyB
jB
jB
jB
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
l�B
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
p�B
p�B
p�B
p�B
p�B
p�B
p�B
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
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
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
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B��B��B��B�B�B�B�!B�!B�B��B��B��B��B��B��B��B��B��B��B�-B�jB��B��B�3B�B�B��B��B��B��B��B�B�RB�wB��BȴBȴB��B��BɺB��B�NB�yB�B��B��B��BB+B	7B\BbBhBoBuB�B�B�B&�B/B5?B9XB=qBC�BI�BJ�BP�BYBdZBhsBiyBl�Bu�Bz�B}�B� B�B~�Br�BbNBYBQ�BD�B6FB/B$�B�B\B1BB�B�B�5B��BƨB�wB�^B�?B�B��B��B��Bz�BffB]/BP�BA�B-BB
�B
�qB
�3B
�B
��B
�{B
�+B
w�B
t�B
gmB
]/B
R�B
J�B
>wB
+B
"�B
�B
B	�B	�BB	��B	ȴB	��B	�jB	�9B	�B	��B	�DB	�B	~�B	x�B	l�B	\)B	N�B	C�B	<jB	6FB	0!B	/B	.B	)�B	#�B	�B	{B	\B	JB��B�
B��B�^B�LB�3B�'B�B�B�B��B��B��B��B��B�oB�\B�PB�JB�%B�%B}�B{�Bz�Bt�Br�Bq�Br�Bp�Bp�Bn�Bm�Bl�BjBiyBgmBe`BaHB_;BaHB`BB_;B^5B]/B\)B[#B[#B[#B[#BZBZBXBXBXBXBXBXBXBW
BVBVBT�BS�BVBT�BS�BT�BS�BT�BS�BT�BT�BW
BW
BVBW
BVBW
BVBW
BXBYBYBZBXBXBXBXBVBT�BR�BQ�BO�BP�BT�BXB]/B^5B\)B\)BW
BS�BN�BI�BI�BK�BL�BN�BO�BR�BP�BM�BG�BC�BB�B<jB:^B7LB7LB7LB9XB;dB<jB<jB<jB=qB=qB<jB<jB=qB=qB>wB?}BC�BD�BD�BE�BI�BM�BL�BP�BW
BYB_;B_;BbNBcTBaHBaHBbNB`BBbNBdZBcTBbNBaHBaHBffBjBjBk�Bn�Bo�Bo�Br�Bt�B}�B�B�\B��B��B��B��B��B��B��B��B��B��B��B��B��B�!B�FB�XB�^B�}BBÖBǮBɺBɺBƨBǮBƨBɺBɺB��B��BɺBǮBŢBƨBƨBȴB��B��B��B��B��B��B�
B�)B�;B�BB�NB�ZB�mB�yB�B��B��B	B	hB	�B	-B	1'B	49B	7LB	7LB	7LB	8RB	:^B	;dB	@�B	A�B	B�B	B�B	D�B	E�B	E�B	H�B	J�B	K�B	L�B	O�B	O�B	O�B	P�B	Q�B	S�B	S�B	T�B	T�B	VB	T�B	VB	W
B	YB	YB	ZB	]/B	`BB	bNB	cTB	cTB	cTB	aHB	bNB	dZB	iyB	k�B	m�B	n�B	n�B	o�B	p�B	s�B	v�B	w�B	w�B	y�B	x�B	}�B	�B	~�B	|�B	~�B	�B	�B	�B	�+B	�7B	�DB	�JB	�VB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�'B	�-B	�3B	�?B	�FB	�LB	�RB	�LB	�FB	�-B	�-B	�?B	�qB	�}B	��B	ÖB	ǮB	ƨB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�
B	�B	�)B	�/B	�5B	�5B	�5B	�;B	�;B	�;B	�HB	�`B	�fB	�fB	�mB	�fB	�fB	�fB	�fB	�fB	�mB	�mB	�sB	�sB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B	��B
  B
  B
B
B
B
B
B
%B
%B
%B
%B
B
%B
%B
%B
%B
%B
%B
%B
+B
+B
+B
1B
1B
+B
+B
	7B

=B

=B

=B

=B
DB
JB
PB
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
bB
hB
oB
uB
{B
�B
�B
�B
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
!�B
"�B
"�B
"�B
"�B
!�B
�B
�B
�B
�B
�B
�B
!�B
#�B
$�B
%�B
%�B
%�B
%�B
"�B
!�B
!�B
 �B
 �B
�B
�B
 �B
"�B
#�B
$�B
'�B
(�B
)�B
+B
,B
-B
-B
.B
.B
.B
/B
0!B
0!B
1'B
1'B
1'B
1'B
1'B
1'B
2-B
2-B
1'B
1'B
.B
.B
.B
.B
-B
-B
-B
.B
/B
/B
/B
0!B
0!B
0!B
0!B
1'B
1'B
2-B
2-B
33B
8RB
8RB
8RB
9XB
9XB
:^B
;dB
;dB
;dB
<jB
<jB
<jB
<jB
<jB
=qB
<jB
;dB
:^B
:^B
9XB
9XB
:^B
;dB
;dB
:^B
;dB
;dB
=qB
?}B
@�B
?}B
?}B
@�B
A�B
B�B
C�B
C�B
C�B
B�B
A�B
?}B
>wB
?}B
?}B
>wB
?}B
?}B
?}B
?}B
?}B
@�B
@�B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
B�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
E�B
F�B
F�B
F�B
G�B
G�B
H�B
H�B
H�B
H�B
I�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
L�B
M�B
M�B
M�B
M�B
M�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
P�B
Q�B
Q�B
R�B
R�B
R�B
R�B
R�B
R�B
R�B
T�B
T�B
VB
VB
VB
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
W
B
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
ZB
ZB
ZB
ZB
ZB
ZB
ZB
[#B
[#B
[#B
[#B
[#B
[#B
\)B
\)B
]/B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
^5B
_;B
^5B
_;B
^5B
^5B
_;B
_;B
_;B
_;B
`BB
`BB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
bNB
cTB
dZB
dZB
dZB
dZB
e`B
e`B
ffB
ffB
ffB
gmB
gmB
gmB
gmB
hsB
hsB
hsB
hsB
hsB
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
iyB
iyB
jB
jB
jB
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
l�B
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
p�B
p�B
p�B
p�B
p�B
p�B
p�B
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
w�B
w�B
w�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
x�B
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
{�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.47 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20200529100033                              AO  ARCAADJP                                                                    20200529100033    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200529100033  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200529100033  QCF$                G�O�G�O�G�O�0               