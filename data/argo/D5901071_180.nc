CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       D2011-11-30 AOML 2.2 creation; 2015-12-13T22:40:41Z UW 3.1 conversion   
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  78   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7x   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  K|   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    
resolution        =���   C_format      %7.1f      FORTRAN_format        F7.1f      
_FillValue        G�O�     �  Mx   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  Uh   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ]X   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  _T   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  gD   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  i@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  q0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  y    PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  {   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    
resolution        :�o   C_format      %9.3f      FORTRAN_format        F9.3f      
_FillValue        G�O�     �  �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �X   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �\   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �`   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �d   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   units         decibar    
_FillValue        G�O�        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    units         decibar    
_FillValue        G�O�        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  5901071 US ARGO PROJECT                                                 Stephen Riser                                                   PRES            TEMP            PSAL               �A   AO  20111130143220  20190522121827  1727_5046_180                   2C  D   APEX                            2143                            040306                          846 @��P���1   @��Q-��
@4��G�{�c��l�C�1   GPS     Primary sampling: mixed [deeper than nominal 980dbar: discrete; nominal 980dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   A   A   @�  @�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  BxffB�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C�C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8�C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CU�fCX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� DfD� D��D� D  D�fDfD� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*fD*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DR��DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dy��D�6fD�` D���D�� D�6fD�s3D���D�� D�)�D�Y�D�� D��3D�  D�VfDڳ3D�ٚD�  D�VfD��D���1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@�ff@�ffA33A!��AC33Ac33A���A���A���A���A���Aљ�AᙚA�B ��B��B��B��B ��B(��B0��B8��B@��BH��BP��BX��B`��Bh��Bp��By33B�ffB�33B�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffB�ffC 33CL�CL�C33C33C
33C33C33C33C33C33C33C33C33C33C33C 33C"33C$33C&33C(33C*33C,33C.33C033C233C433C633C8L�C:33C<33C>33C@33CB33CD33CF33CH33CJ33CL33CN33CP33CR33CT33CV�CX33CZ33C\33C^33C`33Cb33Cd33Cf33Ch33Cj33Cl33Cn33Cp33Cr33Ct33Cv33Cx33Cz33C|33C~33C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C�&fC��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��C��D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D3D��DfD��D�D�3D3D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*3D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DSfDS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dy��D�<�D�ffD��3D��fD�<�D�y�D�� D��fD�0 D�` D��fD���D�&fD�\�Dڹ�D�� D�&fD�\�D�3D��31111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��TA��mA��yA��A��yA��A��A��A��A��A��A��mA��mA��A��A��A��A��A���A��A��A��A��A���A���A���AլAնFA՗�A�r�A�C�A�XA�  A��/Aͣ�A�33A̼jA˛�A�z�A�\)A�A�A�ȴA��yA�E�A��HA�;dA��A�A�x�A¼jA�ȴA��A�$�A���A�I�A�7LA�O�A�"�A���A��
A��jA�?}A�l�A�jA�ffA�1A��9A��DA�A�ȴA�hsA���A�$�A���A���A��#A�A�bA�+A��A��#A�ȴA���A�t�A�A�+A��#A��DA��TA�dZA��A��jA�{A�&�A���A�=qA�G�A��A�ĜA�|�A��FA�l�A�p�A�t�A�=qA�p�A�FA}��Az1Ax��AvZAuK�At-As��Ar(�Ap�9Ao�FAnE�Ak�FAidZAg�AeO�AbE�A]�#A[C�AX�AV�9AUp�AR�AQ�AN(�AKt�AI��AI�AIO�AFACXACVACoAC
=ABQ�A@�A>{A=G�A<bNA;��A;C�A9�-A8A�A8A7XA6M�A4JA2 �A1l�A0�A0�A/��A-�A,{A+A*I�A)/A'�
A'�FA'�FA'�-A't�A&1'A%�A%�;A%A"�yA"v�A!��A bNAp�A��A��A-A~�A-A�A$�Ax�A�RAn�AE�A�
A�A�AoA�A��AbA|�AdZAG�A&�AoA�`AjA?}A	C�AC�A��AE�A�A�uAM�AO�A ��A ��A z�A A�A 5?@��@���@�X@��/@���@�r�@�K�@�hs@��@���@��@���@���@��y@�"�@�V@�|�@�ff@��@�hs@��T@�l�@��@�1'@�C�@�hs@�j@�t�@�ff@ԓu@�t�@���@�v�@�=q@�x�@��
@Ο�@��@�9X@�ƨ@˝�@�l�@���@��@ȓu@Ǖ�@�o@Ƈ+@�5?@�@ũ�@�7L@�z�@å�@�K�@§�@�~�@�=q@�hs@��@��@�+@���@�-@�G�@�1'@��@�v�@��#@���@�9X@�o@�\)@���@�dZ@���@�=q@���@���@� �@�1@��
@�+@�G�@��@��
@��P@�dZ@�dZ@�K�@�33@��@�@��R@�=q@�G�@���@�j@�|�@���@���@�%@��@�j@�b@���@�K�@�v�@��+@��R@��@���@�ff@��7@��w@�=q@��H@��w@�I�@��@���@���@��/@��j@��D@�b@��@���@�ƨ@�dZ@��H@���@���@�/@�7L@�G�@�G�@��`@�A�@��P@��+@���@��#@��#@���@���@��@�X@�7L@�j@��@�33@���@�$�@��@�{@�O�@���@�r�@�Q�@�9X@� �@��@�  @�dZ@���@�|�@�K�@���@�~�@��@���@���@��h@�?}@�V@�A�@�K�@�+@�C�@��@��\@��@���@��h@�hs@�G�@��@�Ĝ@��@��@�Z@�1@��@��@�V@��@�A�@�  @���@�33@���@��R@���@��@��T@�Ĝ@��D@��@�z�@�r�@�bN@�Q�@�Q�@�1'@��m@��w@��@�;d@��@��@�ȴ@���@���@�^5@�$�@��T@�@���@�x�@�?}@��u@�A�@�(�@��m@��@��@��@���@�~�@�{@��#@���@���@���@�O�@��@���@�Q�@�1'@��@�|�@�t�@��H@�ȴ@���@�~�@�V@�=q@�X@��D@�|�@�o@��@�|�@���@�E�@��#@��^@�n�@�5?@��@{ƨ@t��@j��@c�F@\(�@T��@K�F@B��@<Z@7�P@2n�@,9X@&�+@ A�@��@�@t�@1'@�D@�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��TA��mA��yA��A��yA��A��A��A��A��A��A��mA��mA��A��A��A��A��A���A��A��A��A��A���A���A���AլAնFA՗�A�r�A�C�A�XA�  A��/Aͣ�A�33A̼jA˛�A�z�A�\)A�A�A�ȴA��yA�E�A��HA�;dA��A�A�x�A¼jA�ȴA��A�$�A���A�I�A�7LA�O�A�"�A���A��
A��jA�?}A�l�A�jA�ffA�1A��9A��DA�A�ȴA�hsA���A�$�A���A���A��#A�A�bA�+A��A��#A�ȴA���A�t�A�A�+A��#A��DA��TA�dZA��A��jA�{A�&�A���A�=qA�G�A��A�ĜA�|�A��FA�l�A�p�A�t�A�=qA�p�A�FA}��Az1Ax��AvZAuK�At-As��Ar(�Ap�9Ao�FAnE�Ak�FAidZAg�AeO�AbE�A]�#A[C�AX�AV�9AUp�AR�AQ�AN(�AKt�AI��AI�AIO�AFACXACVACoAC
=ABQ�A@�A>{A=G�A<bNA;��A;C�A9�-A8A�A8A7XA6M�A4JA2 �A1l�A0�A0�A/��A-�A,{A+A*I�A)/A'�
A'�FA'�FA'�-A't�A&1'A%�A%�;A%A"�yA"v�A!��A bNAp�A��A��A-A~�A-A�A$�Ax�A�RAn�AE�A�
A�A�AoA�A��AbA|�AdZAG�A&�AoA�`AjA?}A	C�AC�A��AE�A�A�uAM�AO�A ��A ��A z�A A�A 5?@��@���@�X@��/@���@�r�@�K�@�hs@��@���@��@���@���@��y@�"�@�V@�|�@�ff@��@�hs@��T@�l�@��@�1'@�C�@�hs@�j@�t�@�ff@ԓu@�t�@���@�v�@�=q@�x�@��
@Ο�@��@�9X@�ƨ@˝�@�l�@���@��@ȓu@Ǖ�@�o@Ƈ+@�5?@�@ũ�@�7L@�z�@å�@�K�@§�@�~�@�=q@�hs@��@��@�+@���@�-@�G�@�1'@��@�v�@��#@���@�9X@�o@�\)@���@�dZ@���@�=q@���@���@� �@�1@��
@�+@�G�@��@��
@��P@�dZ@�dZ@�K�@�33@��@�@��R@�=q@�G�@���@�j@�|�@���@���@�%@��@�j@�b@���@�K�@�v�@��+@��R@��@���@�ff@��7@��w@�=q@��H@��w@�I�@��@���@���@��/@��j@��D@�b@��@���@�ƨ@�dZ@��H@���@���@�/@�7L@�G�@�G�@��`@�A�@��P@��+@���@��#@��#@���@���@��@�X@�7L@�j@��@�33@���@�$�@��@�{@�O�@���@�r�@�Q�@�9X@� �@��@�  @�dZ@���@�|�@�K�@���@�~�@��@���@���@��h@�?}@�V@�A�@�K�@�+@�C�@��@��\@��@���@��h@�hs@�G�@��@�Ĝ@��@��@�Z@�1@��@��@�V@��@�A�@�  @���@�33@���@��R@���@��@��T@�Ĝ@��D@��@�z�@�r�@�bN@�Q�@�Q�@�1'@��m@��w@��@�;d@��@��@�ȴ@���@���@�^5@�$�@��T@�@���@�x�@�?}@��u@�A�@�(�@��m@��@��@��@���@�~�@�{@��#@���@���@���@�O�@��@���@�Q�@�1'@��@�|�@�t�@��H@�ȴ@���@�~�@�V@�=q@�X@��D@�|�@�o@��@�|�@���@�E�@��#@��^@�n�@�5?@��@{ƨ@t��@j��@c�F@\(�@T��@K�F@B��@<Z@7�P@2n�@,9X@&�+@ A�@��@�@t�@1'@�D@�`1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB5?B5?B49B49B49B49B49B49B49B33B49B33B33B33B33B33B33B33B49B33B33B33B2-B1'B1'B0!B.B.B+B(�B#�BoB��B1B-BI�Bp�B��B��B��B�B��B��B��B��B��B��B��B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�?B�FB�FB�XB�3B�!B�!B�B��B��B�uB�bB�JB�%B}�Bo�Be`B`BB\)BT�B(�B�RB�+B]/BM�B@�B!�BbB
��B
��BB
�yB
�B
ÖB
�!B
��B
��B
�7B
y�B
dZB
T�B
E�B
5?B
�B
B	��B	�sB	�ZB	�B	�B	�B	�B	�HB	�#B	��B	��B	�XB	��B	��B	��B	x�B	XB	B�B	/B	"�B	�B	bB	1B��B��B�B	+B	
=B	B��B��B��B��B��B�B�mB�TB�HB�;B�5B�B��B��B��B��BB�qB�dB�XB�LB�9B�-B�B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�oB�bB�bB�JB�7B�B�B� B� B~�B~�B~�B}�B}�B{�B|�B|�B|�B{�Bz�Bz�Bz�Bz�Bz�By�Bw�Bv�Bs�Bv�Bz�B{�B}�B{�Bx�Bv�Bu�Bt�Bt�Br�Br�Bq�Bp�Bp�Bu�Bw�Bw�By�B}�B}�B� B~�B�DB�\B�oB�=B{�By�Bp�BgmBe`Bx�B|�Bq�B_;B`BB`BBaHBbNBcTBe`BgmBhsBiyBhsBgmBgmBgmBhsBl�Bm�Bn�Bn�Bm�Bp�Bq�Bq�Bs�Bs�Bt�Bt�Bu�Bv�Bw�Bw�Bx�Bx�By�Bz�Bz�B|�B}�B�B�B�%B�+B�7B�DB�VB�{B��B��B��B��B��B��B��B��B��B�B�!B�'B�3B�9B�9B�FB�XB�^B�jB�wBBĜBȴBɺBɺBɺBɺB��B��B��B�B�B�;B�TB�`B�fB�sB�B�B�B��B	+B	1B	
=B	VB	\B	$�B	49B	9XB	>wB	B�B	C�B	I�B	L�B	O�B	P�B	P�B	P�B	R�B	W
B	[#B	\)B	[#B	ZB	YB	ZB	^5B	_;B	aHB	e`B	e`B	gmB	ffB	l�B	p�B	r�B	s�B	t�B	t�B	u�B	u�B	u�B	v�B	y�B	|�B	~�B	~�B	�B	�=B	�=B	�=B	�=B	�=B	�DB	�bB	�bB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�!B	�B	�B	�'B	�-B	�-B	�-B	�9B	�9B	�?B	�FB	�?B	�?B	�9B	�9B	�qB	��B	ÖB	ĜB	ŢB	ŢB	ŢB	ŢB	ÖB	ƨB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�#B	�)B	�5B	�;B	�BB	�HB	�HB	�HB	�TB	�`B	�`B	�`B	�`B	�mB	�sB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�sB	�yB	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B
1B
VB
{B
�B
)�B
2-B
<jB
C�B
H�B
M�B
S�B
ZB
`BB
ffB
jB
n�B
r�B
w�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111B5?B5?B49B49B49B49B49B49B49B33B49B33B33B33B33B33B33B33B49B33B33B33B2-B1'B1'B0!B.B/B,B(�B$�B�B��BJB/BK�Bu�B��B��B�B�B�B��B��B��B��B��B��B�B��B��B��B��B�B��B��B��B��B��B��B��B��B�?B�FB�RB�wB�LB�-B�'B�-B��B��B�{B�uB�bB�VB�Bs�BffB`BB]/B`BBH�BB�hBe`B\)BK�B)�B�BB
��B1B
�B
�5B
��B
�?B
�B
��B
�hB
� B
iyB
ZB
L�B
?}B
&�B
1B
  B	�B	�B	�B	��B	�B	�B	�fB	�5B	�B	��B	�wB	�B	��B	��B	�B	^5B	I�B	2-B	%�B	�B	uB	bB	B��B�B		7B	uB	DB��B��B��B��B��B��B�yB�`B�TB�BB�NB�)B�B��B��B��BȴB�}B�qB�dB�XB�^B�LB�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B��B�{B�{B�uB�PB�PB�VB�B�B�B�B� B� B~�B�B� B}�B}�B}�B}�B|�Bz�Bz�B{�Bz�Bz�By�Bz�Bz�B}�B|�B}�B� B�B� By�Bv�Bu�Bu�Bs�Br�Br�Br�Br�Bv�Bw�Bx�B{�B�B� B�B|�B�JB�oB��B�VB|�B|�Bv�BjBcTBx�B� Bx�BaHBaHBbNBbNBcTBe`BgmBiyBiyBiyBhsBhsBjBiyBk�Bm�Bn�Bn�Bo�Bp�Bq�Br�Bs�Bt�Bt�Bu�Bt�Bv�Bw�Bx�By�By�By�By�B{�B|�B}�B� B�B�%B�%B�7B�7B�PB�\B�{B��B��B��B��B��B��B��B��B��B�B�'B�'B�3B�?B�LB�RB�XB�^B�jB�wBBĜBȴBɺBɺB��B��B��B��B��B�
B�)B�BB�ZB�`B�mB�sB�B�B�B��B	1B	1B		7B	\B	DB	 �B	33B	8RB	=qB	B�B	B�B	I�B	L�B	O�B	P�B	Q�B	P�B	R�B	W
B	[#B	]/B	[#B	[#B	ZB	ZB	^5B	_;B	bNB	ffB	ffB	hsB	gmB	l�B	p�B	r�B	s�B	t�B	t�B	u�B	v�B	w�B	v�B	z�B	}�B	~�B	~�B	�B	�DB	�DB	�=B	�=B	�=B	�=B	�hB	�hB	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�!B	�'B	�!B	�B	�'B	�-B	�-B	�3B	�9B	�9B	�?B	�FB	�?B	�FB	�?B	�-B	�jB	��B	ĜB	ĜB	ŢB	ŢB	ŢB	ƨB	ÖB	ȴB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	�B	�B	�#B	�#B	�#B	�)B	�5B	�;B	�BB	�HB	�HB	�NB	�TB	�`B	�`B	�fB	�fB	�mB	�sB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�sB	�B	�B	�B	�B	�B	�B	��B	�B	�B	��B
1B
PB
{B
�B
)�B
2-B
<jB
C�B
H�B
M�B
S�B
ZB
`BB
ffB
jB
n�B
r�B
w�B
z�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<49X<��<#�
<#�
<#�
<e`B<49X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED ) + CTM                                                                                                                                                                                   dP =-0.2 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            CTM: alpha=0.141, tau=6.68s                                                                                                                                                                                                                                     Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. Conductivity cell thermal mass correction (CTM) applied to measurements taken in Cp mode. The quoted error is max[0.01, sqrt(OWerr^2+CTMerr^2)] in PSS-78.        201201031447372012010314473720120103144737  AO  ARGQ                                                                        20111130143220  QCP$                G�O�G�O�G�O�FFBFE           AO  ARGQ                                                                        20111130143220  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20120103144737  IP                  G�O�G�O�G�O�                