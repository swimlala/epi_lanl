CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2020-08-07T11:00:43Z creation      
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
resolution        =���   axis      Z        d  9p   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  H�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  L�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     d  _�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  oT   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ~�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  �8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  �x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     d  ˸   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �L   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �L   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �L   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �L   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �x   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �|   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200807110043  20200807110043  5906156 US ARGO PROJECT                                                 GREGORY C. JOHNSON                                              PRES            TEMP            PSAL               0A   AO  7912                            2B  A   NAVIS_A                         1020                            170425                          863 @�.��`11   @�.�W:�@7��$�/�cHr� Ĝ1   GPS     Primary sampling: averaged                                                                                                                                                                                                                                         0A   A   A   @�33@���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD�CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DMfDM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dw  Dw� Dx  Dx� Dy  Dy� Dz  Dz� D{  D{� D|  D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�@ D D�� D�  D�@ DÀ D�� D�  D�@ DĀ D�� D�  D�@ Dŀ D�� D�  D�@ Dƀ D�� D�  D�@ Dǀ D�� D�  D�@ DȀ D�� D�  D�@ Dɀ D�� D�  D�@ Dʀ D�� D�  D�@ Dˀ D�� D�  D�@ D̀ D�� D�  D�@ D̀ D�� D�  D�@ D΀ D�� D�  D�@ Dπ D�� D�  D�@ DЀ D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�@ DԀ D�� D�  D�@ DՀ D�� D�  D�@ Dր D�� D�  D�@ D׀ D�� D�  D�@ D؀ D�� D�  D�@ Dـ D�� D�  D�@ Dڀ D�� D�  D�@ Dۀ D�� D�  D�@ D܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�@ D߀ D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D� D�� D�  D�@ D�� D�� D�  D�L�D�` 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�(�@�@���Az�A8z�AXz�Axz�A�=qA�=qA�=qA�=qA�=qA�=qA�=qA�=qB�B�B�B�B&�B.�B6�B>�BF�BN�BV�B^�Bf�Bn�Bv�B~�B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\B�\C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%��C'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=��C?��CA��CC�HCE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C��
C��
C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C�ФC���C���C���C���C���C���C���C���C���C���C���C���C���C���C��
C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���CԷ
C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D a�D ��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��D	a�D	��D
a�D
��Da�D��Da�DۆDa�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��Da�D��D a�D ��D!a�D!��D"a�D"��D#a�D#��D$a�D$��D%a�D%��D&a�D&��D'a�D'��D(a�D(��D)a�D)��D*a�D*��D+a�D+��D,a�D,��D-a�D-��D.a�D.��D/a�D/��D0a�D0��D1a�D1��D2a�D2��D3a�D3��D4a�D4��D5a�D5��D6a�D6��D7a�D7��D8a�D8��D9a�D9��D:a�D:��D;a�D;��D<a�D<��D=a�D=��D>a�D>��D?a�D?��D@a�D@��DAa�DA��DBa�DB��DCa�DC��DDa�DD��DEa�DE��DFa�DF��DGa�DG��DHa�DH��DIa�DI��DJa�DJ��DKa�DK��DLa�DL�RDMa�DM��DNa�DN��DOa�DO��DPa�DP��DQa�DQ��DRa�DR��DSa�DS��DTa�DT��DUa�DU��DVa�DV��DWa�DW��DXa�DX��DYa�DY��DZa�DZ��D[a�D[��D\a�D\��D]a�D]��D^a�D^��D_a�D_��D`a�D`��Daa�Da��Dba�Db��Dca�Dc��Dda�Dd��Dea�De��Dfa�Df��Dga�Dg��Dha�Dh��Dia�Di��Dja�Dj��Dka�Dk��Dla�Dl��Dma�Dm��Dna�Dn��Doa�Do��Dpa�Dp��Dqa�Dq��Dra�Dr��Dsa�Ds��Dta�Dt��Dua�Du��Dva�Dv��Dwa�Dw��Dxa�Dx��Dya�Dy��Dza�Dz��D{a�D{��D|a�D|��D}a�D}��D~a�D~��Da�D��D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D���D���D�0�D�p�D°�D���D�0�D�p�Dð�D���D�0�D�p�Dİ�D���D�0�D�p�DŰ�D���D�0�D�p�Dư�D���D�0�D�p�Dǰ�D���D�0�D�p�DȰ�D���D�0�D�p�Dɰ�D���D�0�D�p�Dʰ�D���D�0�D�p�D˰�D���D�0�D�p�D̰�D���D�0�D�p�DͰ�D���D�0�D�p�Dΰ�D���D�0�D�p�Dϰ�D���D�0�D�p�Dа�D���D�0�D�p�DѰ�D���D�0�D�p�DҰ�D���D�0�D�p�DӰ�D���D�0�D�p�D԰�D���D�0�D�p�Dհ�D���D�0�D�p�Dְ�D���D�0�D�p�Dװ�D���D�0�D�p�Dذ�D���D�0�D�p�Dٰ�D���D�0�D�p�Dڰ�D���D�0�D�p�D۰�D���D�0�D�p�Dܰ�D���D�0�D�p�Dݰ�D���D�0�D�p�Dް�D���D�0�D�p�D߰�D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D���D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D��D���D�0�D�p�D���D���D�0�D�p�D���D���D�=�D�P�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�AиRA�ĜA���A��/A���A���A��/A��HA��TA��TA��`A��mA��mA��yA��yA��A��A��A��A��A��A���A���A��A��;AС�A���A�A�l�Aʡ�A���AîA��A�;dA�G�A���A�VA�|�A�A�A�7LA��A��yA�ffA�1'A��DA�E�A�x�A�{A�O�A���A�-A�K�A���A�x�A�bA�ZA�9XA�%A�~�A���A�XA�"�A��A��A�(�A��;A�;dA�O�A��\A���A��RA�=qA���A� �A�(�A�&�A���A�5?A�A�v�A�z�A�t�A�r�A���A��yA���A�dZA��+A��A�bA��7A�jA���A�XA�I�A��\A��mA�{A��A���A���A�M�A��FA��^A�|�A��9A�z�A���A��\A�l�A��PA���A��yA�1'A�bNA+A}�Azz�AvȴAt�Arv�Ap�Amt�Aj1'Af^5AbȴAaA^�HA]VAZ�yAX�AU�AQ��AP��AOdZAM�wAL1'AK"�AJ�AJZAIXAH�AGVAEhsADȴAD�\AD(�AC�AB�+AA��A?�#A<��A<M�A;�7A:�yA9��A8JA6bNA5G�A3t�A2ĜA1�A0jA/�wA.�+A-hsA,�A,A+dZA*��A*bA)G�A(�9A'��A%�A%S�A$�RA#l�A"ZA!�-A �A�AI�AA�A1'A�A5?A
=A�-A`BA��A/A�uAbA��A"�A�yA�DA��Ax�A��AjA�mA-AA�AAS�A
�yA
�A
�\A
1'A	��A��AK�A��AS�AC�A�A��A^5AA�9A�FA�A��An�A�wA7L@�E�@�|�@��T@��@�33@�5?@�J@�@�7L@陚@�7L@�(�@�"�@��;@��/@�b@���@���@��y@���@އ+@�`B@��@܃@��
@�l�@ڇ+@٩�@؛�@�l�@��H@և+@�G�@�"�@Ѻ^@�V@�Z@��m@Ώ\@��@�z�@�=q@�O�@��@ț�@�1@�ƨ@���@�J@�O�@ě�@�"�@��@�?}@���@�9X@��@�E�@��-@��@��j@�t�@��R@�=q@��T@� �@���@���@�Ĝ@���@�K�@�-@���@�@��^@���@��@��D@�j@�9X@�(�@��j@��`@�j@�ƨ@�C�@��R@�M�@��@�7L@��j@��u@�j@� �@�S�@���@��@�|�@���@��\@�J@�J@�=q@�ȴ@�E�@���@�G�@��`@��j@���@��@�z�@�l�@�v�@��#@�@���@�K�@�1'@��D@���@���@�Q�@��@��@�x�@���@�dZ@�5?@��#@���@��h@��D@�j@�9X@�b@��`@�J@�n�@��+@�ȴ@�r�@��h@�E�@�l�@��y@�+@��@��H@���@��7@�?}@��j@��u@��j@��@���@��@��!@�G�@�ƨ@��
@�(�@�p�@���@�I�@�1@��m@��F@�t�@��@�
=@��H@�5?@�{@��@��^@�`B@��/@��9@�1'@��m@�ƨ@��F@��@���@��P@�t�@�dZ@�K�@�33@��R@��+@�~�@�M�@���@��h@�%@���@���@�Z@��@��F@��@��P@��@�l�@�+@��y@���@�~�@�~�@�v�@�~�@���@�M�@���@�@��#@���@���@��T@�=q@�ff@�M�@�@��^@��@��`@���@�%@��@��@��/@���@�Ĝ@���@���@�I�@� �@�;@�w@�P@~V@}��@}�T@}�T@}�h@}�T@~V@}�@|�@}?}@|�@|��@|1@{�
@{��@z�@z�!@z�@x��@x�`@x��@x�u@xQ�@xA�@xA�@xQ�@x�9@x�`@xr�@w�;@w��@w�w@w�w@w�w@w�w@w�w@w�P@v��@vȴ@v�R@vv�@v{@u�h@t��@t�/@t�@tZ@s��@s�@s"�@r��@rn�@q��@q�7@q��@q&�@p��@p�9@p�u@o�@o�@o�@n�R@nff@m��@m`B@mV@m�@mV@l�j@l�@kƨ@k��@k�@kdZ@k@j~�@j=q@i��@i%@h��@h�`@h�`@h��@h �@g��@g\)@g�@fȴ@fff@eO�@ep�@d�@d��@dI�@d9X@dz�@d(�@ct�@c"�@b~�@a��@a��@a7L@`��@_�;@_��@_\)@]�T@\�@\��@]?}@]��@]��@]�T@]�T@]��@]O�@\��@[�m@[�@[�@[t�@[C�@Z��@Zn�@Z^5@Z=q@Z-@ZJ@Y�^@YG�@Y�@X��@Xb@X  @X  @W�@W�w@W�@WK�@V�y@V�@V�R@V�+@V5?@U�@U?}@T��@T�@TZ@T�@Sƨ@S��@SS�@R��@R�\@RJ@Q��@Qx�@Q7L@PĜ@PA�@P  @O�;@O|�@O+@Nȴ@Nv�@N{@M��@M�-@Mp�@MO�@M/@L�@L�j@L�D@LZ@K��@K�
@Kƨ@K�@K33@K@J��@J�@I�#@I�7@IG�@I&�@I&�@I&�@I�@I�@H�`@H�9@Hr�@H �@H  @G�@G�@G\)@G+@G�@F��@F$�@E�@E��@E/@D�@D�D@DZ@D1@C�
@C��@C33@B�H@B�\@Bn�@BJ@A�#@A�7@AX@A�@@��@@r�@@b@?�@?|�@?;d@?+@>��@>�R@>�+@>V@>5?@>{@>{@=��@=�@=�@<�j@<z�@<9X@<(�@;��@;�F@;�@;S�@;@:n�@9��@9��@9�#@9��@9x�@9G�@9�@8�@8 �@7��@7l�@7l�@7+@6�@6��@6��@6V@6$�@6@5��@5�-@5p�@5V@4�@4I�@3�
@3S�@2��@2�\@2n�@2=q@2-@2-@2�@1�@1�^@1hs@1X@1G�@17L@17L@1%@0�u@0bN@01'@01'@0b@/�@/�@/�P@/|�@/;d@/�@/
=@.��@.ff@.V@.E�@.{@.@-@-p�@,��@,�j@,��@,Z@,9X@+�m@+��@+��@+�@+dZ@+o@*�H@*n�@*=q@*J@)�@)x�@)7L@)�@)%@(��@(��@(��@(��@(b@'�@'�@'\)@'+@&�@&ff@&E�@&@%�-@%�@%O�@%/@$�j@$�D@$Z@$1@#�
@#t�@#@"��@"^5@!�@!��@!�@ Ĝ@ ��@ �u@ r�@�@��@��@\)@;d@��@
=@�@ȴ@�+@v�@E�@@��@p�@�/@�D@I�@(�@�@�m@��@dZ@33@�H@~�@M�@-@J@�#@�7@X@7L@�@��@�9@  @��@K�@+@�@
=@�y@ȴ@�+@E�@@��@��@�@`B@V@�@9X@(�@�@�@�
@��@�@dZ@S�@C�@@��@~�@-@��@��@�7@��@�`@��@�9@bN@  @�@|�@\)@
=@�@��@��@��@��@v�@@��@�-@`B@�@�/@�@j@9X@�@1@�m@ƨ@ƨ@�F@��@t�@dZ@33@33@@
��@
��@
~�@
=q@
=q@
-@
-@
-@
-@	��@	��@	X@	&�@�`@��@Ĝ@��@�u@�@bN@1'@b@�@��@�w@�@\)@;d@+@�@�y@�@v�@V@E�@$�@@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   AиRA�ĜA���A��/A���A���A��/A��HA��TA��TA��`A��mA��mA��yA��yA��A��A��A��A��A��A���A���A��A��;AС�A���A�A�l�Aʡ�A���AîA��A�;dA�G�A���A�VA�|�A�A�A�7LA��A��yA�ffA�1'A��DA�E�A�x�A�{A�O�A���A�-A�K�A���A�x�A�bA�ZA�9XA�%A�~�A���A�XA�"�A��A��A�(�A��;A�;dA�O�A��\A���A��RA�=qA���A� �A�(�A�&�A���A�5?A�A�v�A�z�A�t�A�r�A���A��yA���A�dZA��+A��A�bA��7A�jA���A�XA�I�A��\A��mA�{A��A���A���A�M�A��FA��^A�|�A��9A�z�A���A��\A�l�A��PA���A��yA�1'A�bNA+A}�Azz�AvȴAt�Arv�Ap�Amt�Aj1'Af^5AbȴAaA^�HA]VAZ�yAX�AU�AQ��AP��AOdZAM�wAL1'AK"�AJ�AJZAIXAH�AGVAEhsADȴAD�\AD(�AC�AB�+AA��A?�#A<��A<M�A;�7A:�yA9��A8JA6bNA5G�A3t�A2ĜA1�A0jA/�wA.�+A-hsA,�A,A+dZA*��A*bA)G�A(�9A'��A%�A%S�A$�RA#l�A"ZA!�-A �A�AI�AA�A1'A�A5?A
=A�-A`BA��A/A�uAbA��A"�A�yA�DA��Ax�A��AjA�mA-AA�AAS�A
�yA
�A
�\A
1'A	��A��AK�A��AS�AC�A�A��A^5AA�9A�FA�A��An�A�wA7L@�E�@�|�@��T@��@�33@�5?@�J@�@�7L@陚@�7L@�(�@�"�@��;@��/@�b@���@���@��y@���@އ+@�`B@��@܃@��
@�l�@ڇ+@٩�@؛�@�l�@��H@և+@�G�@�"�@Ѻ^@�V@�Z@��m@Ώ\@��@�z�@�=q@�O�@��@ț�@�1@�ƨ@���@�J@�O�@ě�@�"�@��@�?}@���@�9X@��@�E�@��-@��@��j@�t�@��R@�=q@��T@� �@���@���@�Ĝ@���@�K�@�-@���@�@��^@���@��@��D@�j@�9X@�(�@��j@��`@�j@�ƨ@�C�@��R@�M�@��@�7L@��j@��u@�j@� �@�S�@���@��@�|�@���@��\@�J@�J@�=q@�ȴ@�E�@���@�G�@��`@��j@���@��@�z�@�l�@�v�@��#@�@���@�K�@�1'@��D@���@���@�Q�@��@��@�x�@���@�dZ@�5?@��#@���@��h@��D@�j@�9X@�b@��`@�J@�n�@��+@�ȴ@�r�@��h@�E�@�l�@��y@�+@��@��H@���@��7@�?}@��j@��u@��j@��@���@��@��!@�G�@�ƨ@��
@�(�@�p�@���@�I�@�1@��m@��F@�t�@��@�
=@��H@�5?@�{@��@��^@�`B@��/@��9@�1'@��m@�ƨ@��F@��@���@��P@�t�@�dZ@�K�@�33@��R@��+@�~�@�M�@���@��h@�%@���@���@�Z@��@��F@��@��P@��@�l�@�+@��y@���@�~�@�~�@�v�@�~�@���@�M�@���@�@��#@���@���@��T@�=q@�ff@�M�@�@��^@��@��`@���@�%@��@��@��/@���@�Ĝ@���@���@�I�@� �@�;@�w@�P@~V@}��@}�T@}�T@}�h@}�T@~V@}�@|�@}?}@|�@|��@|1@{�
@{��@z�@z�!@z�@x��@x�`@x��@x�u@xQ�@xA�@xA�@xQ�@x�9@x�`@xr�@w�;@w��@w�w@w�w@w�w@w�w@w�w@w�P@v��@vȴ@v�R@vv�@v{@u�h@t��@t�/@t�@tZ@s��@s�@s"�@r��@rn�@q��@q�7@q��@q&�@p��@p�9@p�u@o�@o�@o�@n�R@nff@m��@m`B@mV@m�@mV@l�j@l�@kƨ@k��@k�@kdZ@k@j~�@j=q@i��@i%@h��@h�`@h�`@h��@h �@g��@g\)@g�@fȴ@fff@eO�@ep�@d�@d��@dI�@d9X@dz�@d(�@ct�@c"�@b~�@a��@a��@a7L@`��@_�;@_��@_\)@]�T@\�@\��@]?}@]��@]��@]�T@]�T@]��@]O�@\��@[�m@[�@[�@[t�@[C�@Z��@Zn�@Z^5@Z=q@Z-@ZJ@Y�^@YG�@Y�@X��@Xb@X  @X  @W�@W�w@W�@WK�@V�y@V�@V�R@V�+@V5?@U�@U?}@T��@T�@TZ@T�@Sƨ@S��@SS�@R��@R�\@RJ@Q��@Qx�@Q7L@PĜ@PA�@P  @O�;@O|�@O+@Nȴ@Nv�@N{@M��@M�-@Mp�@MO�@M/@L�@L�j@L�D@LZ@K��@K�
@Kƨ@K�@K33@K@J��@J�@I�#@I�7@IG�@I&�@I&�@I&�@I�@I�@H�`@H�9@Hr�@H �@H  @G�@G�@G\)@G+@G�@F��@F$�@E�@E��@E/@D�@D�D@DZ@D1@C�
@C��@C33@B�H@B�\@Bn�@BJ@A�#@A�7@AX@A�@@��@@r�@@b@?�@?|�@?;d@?+@>��@>�R@>�+@>V@>5?@>{@>{@=��@=�@=�@<�j@<z�@<9X@<(�@;��@;�F@;�@;S�@;@:n�@9��@9��@9�#@9��@9x�@9G�@9�@8�@8 �@7��@7l�@7l�@7+@6�@6��@6��@6V@6$�@6@5��@5�-@5p�@5V@4�@4I�@3�
@3S�@2��@2�\@2n�@2=q@2-@2-@2�@1�@1�^@1hs@1X@1G�@17L@17L@1%@0�u@0bN@01'@01'@0b@/�@/�@/�P@/|�@/;d@/�@/
=@.��@.ff@.V@.E�@.{@.@-@-p�@,��@,�j@,��@,Z@,9X@+�m@+��@+��@+�@+dZ@+o@*�H@*n�@*=q@*J@)�@)x�@)7L@)�@)%@(��@(��@(��@(��@(b@'�@'�@'\)@'+@&�@&ff@&E�@&@%�-@%�@%O�@%/@$�j@$�D@$Z@$1@#�
@#t�@#@"��@"^5@!�@!��@!�@ Ĝ@ ��@ �u@ r�@�@��@��@\)@;d@��@
=@�@ȴ@�+@v�@E�@@��@p�@�/@�D@I�@(�@�@�m@��@dZ@33@�H@~�@M�@-@J@�#@�7@X@7L@�@��@�9@  @��@K�@+@�@
=@�y@ȴ@�+@E�@@��@��@�@`B@V@�@9X@(�@�@�@�
@��@�@dZ@S�@C�@@��@~�@-@��@��@�7@��@�`@��@�9@bN@  @�@|�@\)@
=@�@��@��@��@��@v�@@��@�-@`B@�@�/@�@j@9X@�@1@�m@ƨ@ƨ@�F@��@t�@dZ@33@33@@
��@
��@
~�@
=q@
=q@
-@
-@
-@
-@	��@	��@	X@	&�@�`@��@Ĝ@��@�u@�@bN@1'@b@�@��@�w@�@\)@;d@+@�@�y@�@v�@V@E�@$�@@��@��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B-B-B,B,B-B-B,B,B,B,B-B-B-B-B-B-B-B-B-B-B-B-B-B,B,B,B,B%�B�B�BbB�B0!B=qBT�BcTBm�B�PB�B�LB�dBǮB��B��B�B�#B�mB�B��BB%BJBuBhB�B�B�B�B"�B.B-B2-B7LB<jBE�BH�BL�BO�BXBZB[#B\)BZBYBVBN�BJ�BG�B@�B/B#�B�BJBB��B��B��B�B�B�5B��B�dB�B��B�+Bm�BcTBW
B@�B1'B �BDB
��B
�B
�sB
�5B
��B
�qB
�B
��B
�JB
}�B
p�B
dZB
W
B
K�B
:^B
&�B
+B	��B	�sB	�#B	ŢB	�!B	��B	~�B	r�B	e`B	YB	J�B	>wB	)�B	�B	JB	%B��B��B�B�B�B�ZB�/B�B��B��BȴBƨBƨB�wB�dB�?B��B��B��B��B��B��B�bB�\B�7B�B�B�B�B�B�+B�7B�7B�1B�%B�B�B}�Bz�Bs�Bn�Bl�BjBffBffBe`BgmBhsBk�Bp�Bo�Bl�Be`B_;BW
BN�BL�BJ�BJ�BH�BG�BF�BF�BD�BC�BD�BE�BF�BC�B@�B@�B@�BA�B@�B@�B@�B?}B?}B>wB=qB:^B8RB8RB7LB8RB7LB@�BQ�Bo�Bo�Br�Bq�Br�Bw�Bq�Bl�Bo�BgmB`BB_;BffBS�BQ�BbNBdZBaHBjB]/BS�B[#BaHBjBo�Bq�Bt�By�By�By�Bz�Bz�B{�Bz�Bz�B~�B~�B�B�B�B�+B�+B�DB�=B�=B�7B�+B�+B�%B�%B�+B�+B�+B�7B�DB�DB�PB�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�3B�9B�9B�-B�!B�B�B�B�B�B�!B�'B�3B�XB�jB�wBĜBƨBƨBƨBƨB��B�B�)B�/B�BB�NB�BB�;B�;B�;B�BB�NB�TB�ZB�B�B�TB�/B�)B�)B�/B�/B�5B�TB�`B�fB�sB�B��B��B	B��B��B��B	B	B	\B	�B	oB	\B	VB	\B	\B	PB	PB	VB	bB	�B	 �B	$�B	%�B	'�B	33B	=qB	B�B	F�B	F�B	>wB	6FB	/B	+B	(�B	+B	.B	1'B	49B	7LB	D�B	R�B	`BB	v�B	p�B	aHB	O�B	E�B	A�B	B�B	D�B	E�B	H�B	N�B	T�B	ZB	]/B	bNB	hsB	k�B	l�B	m�B	n�B	p�B	v�B	v�B	w�B	x�B	y�B	{�B	}�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�+B	�1B	�=B	�=B	�DB	�DB	�JB	�VB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�?B	�LB	�LB	�XB	�dB	�dB	�jB	�jB	�wB	��B	��B	��B	��B	ÖB	ĜB	ƨB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�5B	�BB	�ZB	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
%B
+B
+B
1B
1B
	7B
	7B
	7B

=B
DB
DB
JB
VB
VB
VB
VB
VB
\B
bB
hB
hB
oB
oB
hB
oB
hB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
%�B
%�B
%�B
%�B
%�B
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
(�B
)�B
)�B
)�B
+B
+B
,B
,B
-B
-B
-B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
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
[#B
ZB
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
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
jB
jB
jB
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
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
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
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
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
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B-B-B,B,B-B-B,B,B,B,B-B-B-B-B-B-B-B-B-B-B-B-B-B,B,B,B,B%�B�B�BbB�B0!B=qBT�BcTBm�B�PB�B�LB�dBǮB��B��B�B�#B�mB�B��BB%BJBuBhB�B�B�B�B"�B.B-B2-B7LB<jBE�BH�BL�BO�BXBZB[#B\)BZBYBVBN�BJ�BG�B@�B/B#�B�BJBB��B��B��B�B�B�5B��B�dB�B��B�+Bm�BcTBW
B@�B1'B �BDB
��B
�B
�sB
�5B
��B
�qB
�B
��B
�JB
}�B
p�B
dZB
W
B
K�B
:^B
&�B
+B	��B	�sB	�#B	ŢB	�!B	��B	~�B	r�B	e`B	YB	J�B	>wB	)�B	�B	JB	%B��B��B�B�B�B�ZB�/B�B��B��BȴBƨBƨB�wB�dB�?B��B��B��B��B��B��B�bB�\B�7B�B�B�B�B�B�+B�7B�7B�1B�%B�B�B}�Bz�Bs�Bn�Bl�BjBffBffBe`BgmBhsBk�Bp�Bo�Bl�Be`B_;BW
BN�BL�BJ�BJ�BH�BG�BF�BF�BD�BC�BD�BE�BF�BC�B@�B@�B@�BA�B@�B@�B@�B?}B?}B>wB=qB:^B8RB8RB7LB8RB7LB@�BQ�Bo�Bo�Br�Bq�Br�Bw�Bq�Bl�Bo�BgmB`BB_;BffBS�BQ�BbNBdZBaHBjB]/BS�B[#BaHBjBo�Bq�Bt�By�By�By�Bz�Bz�B{�Bz�Bz�B~�B~�B�B�B�B�+B�+B�DB�=B�=B�7B�+B�+B�%B�%B�+B�+B�+B�7B�DB�DB�PB�hB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�'B�3B�9B�9B�-B�!B�B�B�B�B�B�!B�'B�3B�XB�jB�wBĜBƨBƨBƨBƨB��B�B�)B�/B�BB�NB�BB�;B�;B�;B�BB�NB�TB�ZB�B�B�TB�/B�)B�)B�/B�/B�5B�TB�`B�fB�sB�B��B��B	B��B��B��B	B	B	\B	�B	oB	\B	VB	\B	\B	PB	PB	VB	bB	�B	 �B	$�B	%�B	'�B	33B	=qB	B�B	F�B	F�B	>wB	6FB	/B	+B	(�B	+B	.B	1'B	49B	7LB	D�B	R�B	`BB	v�B	p�B	aHB	O�B	E�B	A�B	B�B	D�B	E�B	H�B	N�B	T�B	ZB	]/B	bNB	hsB	k�B	l�B	m�B	n�B	p�B	v�B	v�B	w�B	x�B	y�B	{�B	}�B	}�B	~�B	� B	�B	�B	�B	�B	�B	�+B	�1B	�=B	�=B	�DB	�DB	�JB	�VB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�'B	�?B	�LB	�LB	�XB	�dB	�dB	�jB	�jB	�wB	��B	��B	��B	��B	ÖB	ĜB	ƨB	ǮB	ǮB	ǮB	ȴB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�)B	�/B	�/B	�5B	�BB	�ZB	�`B	�fB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
  B
B
B
%B
+B
+B
1B
1B
	7B
	7B
	7B

=B
DB
DB
JB
VB
VB
VB
VB
VB
\B
bB
hB
hB
oB
oB
hB
oB
hB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
!�B
!�B
"�B
"�B
"�B
"�B
#�B
#�B
#�B
%�B
%�B
%�B
%�B
%�B
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
(�B
)�B
)�B
)�B
+B
+B
,B
,B
-B
-B
-B
.B
.B
/B
/B
/B
/B
0!B
0!B
0!B
1'B
1'B
1'B
2-B
2-B
2-B
2-B
2-B
2-B
33B
33B
33B
33B
49B
49B
49B
5?B
5?B
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
6FB
7LB
7LB
7LB
7LB
8RB
9XB
9XB
9XB
9XB
:^B
:^B
;dB
<jB
<jB
<jB
=qB
=qB
=qB
>wB
>wB
?}B
?}B
?}B
@�B
@�B
@�B
A�B
A�B
A�B
A�B
B�B
B�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
E�B
E�B
E�B
E�B
F�B
F�B
F�B
F�B
G�B
G�B
G�B
H�B
H�B
H�B
H�B
H�B
H�B
H�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
L�B
M�B
M�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
O�B
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
T�B
T�B
T�B
VB
VB
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
[#B
ZB
[#B
[#B
[#B
\)B
\)B
\)B
]/B
]/B
]/B
]/B
]/B
^5B
^5B
^5B
^5B
_;B
_;B
_;B
`BB
`BB
_;B
`BB
`BB
`BB
`BB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
aHB
bNB
bNB
bNB
cTB
cTB
cTB
cTB
cTB
cTB
cTB
dZB
dZB
dZB
e`B
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
jB
jB
jB
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
m�B
n�B
n�B
o�B
o�B
o�B
o�B
o�B
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
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
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
|�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - surface_pressure                                                                                                                                                                                                                         none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            surface_pressure=0.47 dbar                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      Pressure adjusted at real time based on most recent valid surface pressure                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      20200807110043                              AO  ARCAADJP                                                                    20200807110043    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20200807110043  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20200807110043  QCF$                G�O�G�O�G�O�0               