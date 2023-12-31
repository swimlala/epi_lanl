CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-02-26T22:12:39Z AOML 3.0 creation; 2016-08-07T21:17:29Z UW 3.1 conversion     
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7    PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7X   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    7�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    7�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     7�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8<   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8\   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~       axis      T           8`   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8h   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >Ey��0�:   
_FillValue        A.�~            8l   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8t   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8|   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    8�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    8�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    8�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        9�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    9�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    9�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ax   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  Cp   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  KH   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ^�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  f�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  h�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  p�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  xh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  z`   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �0   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �8   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �8   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �8   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �8   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �h   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �l   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �p   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20150226221239  20160807141729  5904460 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               A   AO  5285_8895_004                   2C  D   APEX                            6487                            072314                          846 @�KS�1   @�K�@�@-�1&�y�c�����1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    A   B   B   @9��@�  @�  A   A   A>ffA`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB.ffB7��B@  BH  BP  BX  B`  BhffBpffBx  B�  B�  B���B���B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP�CQ�fCT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy��D��fD�I�D�� D���D�3D�I�D��3D��fD���D�33D�@ 1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @U�@�@�A�HA&�HAEG�Af�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB	�RB�RB�RB!�RB*�B0�B9Q�BA�RBI�RBQ�RBY�RBa�RBj�Br�By�RB��)B��)B���B���B��)B��)B�\B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��B��)B��)B��)B��)B��)B��)C nC��CnCnCnC
nCnCnCnCnCnCnCnCnCnCnC nC"nC$nC&nC(nC*nC,nC.nC0nC2nC4nC6nC8nC:nC<nC>nC@nCBnCDnCFnCHnCJnCLnCNnCP��CRTzCTnCVnCXnCZnC\nC^nC`nCbnCdnCfnChnCjnClnCnnCpnCrnCtnCvnCxnCznC|nC~nC�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�C�C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
C�7
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt��Dy�RD�)D�W]D���D�ڐD��D�W]D���D��)D�]D�@�D�M�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�A�A�A�A�A�A�9XA��A���A۬A�l�A��A���A��yA��#A���A���A���A���A�ȴAڴ9AڍPA�t�A�?}AٸRA� �A��#Aպ^A�ȴA�  A�-A�G�A̩�A̍PA� �A�ƨAÅA��A��mA���A���A�bA�`BA��PA���A�n�A��A�"�A�bA�K�A�ƨA��FA�jA�jA�|�A��A��;A��+A��A��!A�1'A��7A�ZA�~�A�-A��A���A�/A�O�A��FA�;dA���A���A���A�+A�;dA���A�C�A�jA��A�9XA���A��+A��A��A�1'A��A���A�
=A��jA��yA�A�A��FA��\A��yA�|�A�I�A�Q�A|^5AtjAql�Am��Af=qA^��AZ  ASt�AQVAO�AF��AC�;AA�A?"�A=�7A;��A:��A7O�A5G�A4��A4��A4��A2�RA1G�A0��A0��A0^5A.��A-t�A,�jA,(�A+�hA+�A+
=A*jA* �A*~�A*�A++A(�A'�^A%x�A"�9A!33A A�A�#A�AbAA�uAjA�#A?}A�yA%A��A�hA�A5?A�A"�AZA�;A�mA  A/Al�AI�At�A?}A��A7LA  A7LA��A�TA�AjA{A�A��A��A �A��AA�9At�A�A�hA��A1A��AQ�A��A�-AdZA
ĜA
�A
A	�A	�^A	O�AM�AS�A&�AbNAZA�A�A��A��A��A�hA�A|�A�A�uAffAI�A �A�A��A�mA  A�;A�A�A��A�HA��A^5A�FAl�A\)A?}AoA ��@��m@��@��@��!@�V@�l�@�=q@�A�@�1@�1@��!@�hs@�D@�ƨ@�P@�;d@�t�@�33@���@�+@�-@�J@��@�x�@�j@�(�@�+@�E�@�/@�Z@�  @�=q@���@�D@�9X@���@�x�@��@�(�@��@�V@��@���@�j@�w@�ȴ@�M�@�=q@�@�x�@���@�j@�1'@��
@�@�V@ݩ�@�O�@ܴ9@��@۾w@�C�@�n�@ٲ-@��@�(�@��@�"�@��@ԓu@Ӯ@�{@���@�p�@У�@ЋD@�r�@϶F@�@�E�@��@�X@̃@�A�@�b@˕�@ʸR@�V@ɲ-@�Z@�|�@��@�v�@őh@�&�@ļj@�9X@Ý�@�l�@��y@°!@��@���@�p�@�7L@��@�%@���@��D@�bN@�ƨ@��@��P@��@�S�@��@�@��R@�E�@��@��-@�x�@�7L@��@���@��/@���@���@�bN@�A�@�b@�ƨ@���@�|�@�33@���@���@�v�@�$�@���@���@���@�hs@��@��D@�1'@���@��@�C�@���@��!@�ff@��@���@��@�&�@���@�9X@��w@���@���@��@��@�O�@�/@��@�V@���@��u@�I�@��@���@��y@�n�@���@���@�p�@�p�@�X@�&�@��/@��@�A�@�ƨ@�|�@�"�@��y@�ȴ@�E�@��^@�%@��u@�I�@���@�K�@���@�n�@�=q@���@��@���@�  @��P@��@�~�@�-@���@��-@���@�hs@�%@��@���@��F@�t�@�"�@�o@�ȴ@�=q@��-@�?}@��9@�1'@�  @��@��w@�t�@�"�@���@�5?@��@�J@��-@�&�@��u@�A�@��@�ƨ@��P@�"�@��y@�^5@��#@��h@�?}@��@��`@��j@�r�@� �@�1@��;@���@��@���@��@�dZ@�@�9X@�;d@��w@�
=@|�/@u?}@lz�@e�@\�@Q��@I��@Bn�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111  A�A�A�A�A�A�A�9XA��A���A۬A�l�A��A���A��yA��#A���A���A���A���A�ȴAڴ9AڍPA�t�A�?}AٸRA� �A��#Aպ^A�ȴA�  A�-A�G�A̩�A̍PA� �A�ƨAÅA��A��mA���A���A�bA�`BA��PA���A�n�A��A�"�A�bA�K�A�ƨA��FA�jA�jA�|�A��A��;A��+A��A��!A�1'A��7A�ZA�~�A�-A��A���A�/A�O�A��FA�;dA���A���A���A�+A�;dA���A�C�A�jA��A�9XA���A��+A��A��A�1'A��A���A�
=A��jA��yA�A�A��FA��\A��yA�|�A�I�A�Q�A|^5AtjAql�Am��Af=qA^��AZ  ASt�AQVAO�AF��AC�;AA�A?"�A=�7A;��A:��A7O�A5G�A4��A4��A4��A2�RA1G�A0��A0��A0^5A.��A-t�A,�jA,(�A+�hA+�A+
=A*jA* �A*~�A*�A++A(�A'�^A%x�A"�9A!33A A�A�#A�AbAA�uAjA�#A?}A�yA%A��A�hA�A5?A�A"�AZA�;A�mA  A/Al�AI�At�A?}A��A7LA  A7LA��A�TA�AjA{A�A��A��A �A��AA�9At�A�A�hA��A1A��AQ�A��A�-AdZA
ĜA
�A
A	�A	�^A	O�AM�AS�A&�AbNAZA�A�A��A��A��A�hA�A|�A�A�uAffAI�A �A�A��A�mA  A�;A�A�A��A�HA��A^5A�FAl�A\)A?}AoA ��@��m@��@��@��!@�V@�l�@�=q@�A�@�1@�1@��!@�hs@�D@�ƨ@�P@�;d@�t�@�33@���@�+@�-@�J@��@�x�@�j@�(�@�+@�E�@�/@�Z@�  @�=q@���@�D@�9X@���@�x�@��@�(�@��@�V@��@���@�j@�w@�ȴ@�M�@�=q@�@�x�@���@�j@�1'@��
@�@�V@ݩ�@�O�@ܴ9@��@۾w@�C�@�n�@ٲ-@��@�(�@��@�"�@��@ԓu@Ӯ@�{@���@�p�@У�@ЋD@�r�@϶F@�@�E�@��@�X@̃@�A�@�b@˕�@ʸR@�V@ɲ-@�Z@�|�@��@�v�@őh@�&�@ļj@�9X@Ý�@�l�@��y@°!@��@���@�p�@�7L@��@�%@���@��D@�bN@�ƨ@��@��P@��@�S�@��@�@��R@�E�@��@��-@�x�@�7L@��@���@��/@���@���@�bN@�A�@�b@�ƨ@���@�|�@�33@���@���@�v�@�$�@���@���@���@�hs@��@��D@�1'@���@��@�C�@���@��!@�ff@��@���@��@�&�@���@�9X@��w@���@���@��@��@�O�@�/@��@�V@���@��u@�I�@��@���@��y@�n�@���@���@�p�@�p�@�X@�&�@��/@��@�A�@�ƨ@�|�@�"�@��y@�ȴ@�E�@��^@�%@��u@�I�@���@�K�@���@�n�@�=q@���@��@���@�  @��P@��@�~�@�-@���@��-@���@�hs@�%@��@���@��F@�t�@�"�@�o@�ȴ@�=q@��-@�?}@��9@�1'@�  @��@��w@�t�@�"�@���@�5?@��@�J@��-@�&�@��u@�A�@��@�ƨ@��P@�"�@��y@�^5@��#@��h@�?}@��@��`@��j@�r�@� �@�1@��;@���@��@���@��@�dZG�O�@�9X@�;d@��w@�
=@|�/@u?}@lz�@e�@\�@Q��@I��@Bn�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;oBPBVBbB�B#�B>wBt�B�=B�bB�\B�\B�VB�VB�\B�\B�bB�bB�bB�hB�uB��B�!B�B�1B]/B2-B%�B'�B-B1'B0!BL�B��B��B�B:^BD�BP�B\)BffBv�B�B�=B��B��B��B��B��B�B�3B�^B�jB�XB�9B�FB�?B�'B�B��B�{B�=By�Bl�BgmB_;BVBF�B6FB.B�B�BoB+B��B��B�B��B�dB��B�Bz�BdZBXBQ�BA�B�BB
�B
��B
��B
�DB
|�B
p�B
N�B	��B	�?B	|�B	ffB	M�B	0!B	�B	JB	B��B�B��B��BǮBB�}B�wB�^B�FB�9B�?B�jB�wB��BȴBƨBŢBŢB��B�
B�)B�NB�B�B��B	%B	PB	�B	+B	1'B	+B	!�B	�B	oB	PB	\B	�B	�B	�B	�B	 �B	�B	�B	"�B	$�B	'�B	+B	-B	-B	7LB	7LB	49B	33B	6FB	=qB	F�B	M�B	N�B	O�B	M�B	M�B	R�B	]/B	dZB	gmB	|�B	�+B	�PB	�\B	��B	��B	��B	��B	��B	�B	�RB	�qB	�FB	�RB	�qB	ĜB	��B	��B	�)B	�yB	�ZB	�HB	�/B	�)B	�`B	�mB	�mB	�sB	�TB	�)B	�B	�B	�B	�;B	�HB	�`B	�B	�B	�B	��B	��B	�B	�B	�B	�B	�B	�B	��B	��B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B	��B	��B	�B	�B	�B	��B	�B	�B	�B	�sB	�fB	�`B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�fB	�ZB	�NB	�HB	�NB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�mB	�TB	�NB	�HB	�NB	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
+B
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
JB
JB
PB
PB
PB
VB
VB
VB
\B
\B
\B
\B
\B
\B
\B
bB
bB
bB
hB
hB
hB
hB
oB
oB
uB
uB
{B
{B
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
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
!�B
�B
#�B
'�B
-B
33B
8RB
>wB
C�B
G�B
N�B
S�B
W
1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111  B1B7BBB`B#�B>UBt�B�B�@B�8B�9B�5B�5B�9B�;B�BB�AB�AB�GB�SB�`B�B��B�B]B2B%�B'�B,�B1B/�BL�B��BʜB�|B:6BDuBP�B\BfEBv�B��B�B�]B�tB�~B�aB��B��B�B�=B�IB�4B�B� B�B�B��B��B�TB�By�BleBgGB_BU�BF}B6 B-�B~BfBIBB��B��B�pB��B�>B�_B��Bz�Bd0BW�BQ�BA`B�B�B
�B
мB
��B
�B
|�B
p�B
N�B	��B	�B	|�B	fEB	M�B	0B	�B	-B	�B��B�B��B͵BǑB�uB�bB�ZB�BB�*B�B�"B�LB�XBʥBȗBƈBŃBŅB��B��B�B�0B��B�B��B	B	/B	�B	*�B	1B	*�B	!�B	�B	LB	/B	8B	|B	�B	�B	�B	 �B	�B	�B	"�B	$�B	'�B	*�B	,�B	,�B	7'B	7%B	4B	3B	6"B	=LB	F�B	M�B	N�B	O�B	M�B	M�B	R�B	]B	d2B	gDB	|�B	�B	�'B	�2B	�dB	�|B	�kB	�uB	��B	��B	�(B	�FB	�B	�'B	�FB	�pB	̢B	��B	��B	�LB	�-B	�B	�B	��B	�6B	�BB	�@B	�IB	�(B	��B	��B	��B	��B	�B	�B	�3B	�QB	�`B	�pB	�B	��B	�B	�B	�{B	�vB	�xB	�B	��B	��B
 �B
 �B
�B
�B
�B
�B
�B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
 �B
�B	��B	��B	�wB	�iB	�wB	��B	�{B	�fB	�QB	�EB	�8B	�3B	�\B	�zB	�B	�B	�B	�B	�{B	�~B	�vB	�pB	�cB	�RB	�KB	�>B	�:B	�+B	�B	�B	�B	�EB	�bB	�wB	�vB	�|B	�tB	�uB	�oB	�kB	�cB	�^B	�\B	�]B	�]B	�]B	�_B	�cB	�[B	�\B	�dB	�^B	�XB	�OB	�TB	�]B	�\B	�^B	�VB	�QB	�WB	�SB	�cB	�bB	�\B	�KB	�=B	�&B	� B	�B	�!B	�=B	�EB	�WB	�VB	�\B	�\B	�bB	�iB	�jB	�iB	�jB	�iB	�dB	�aB	�mB	�tB	�vB	�pB	�oB	�fB	�iB	�iB	�hB	�nB	�sB	�nB	�vB	�vB	�uB	�yB	�{B	�|B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
B
�B
 B
	B
	B

B

B

B

B
B
B
B
B
B
B
B
B
%B
"B
$B
*B
'B
+B
*B
*B
,B
+B
3B
2B
1B
5B
6B
4B
4B
<B
<B
EB
DB
IB
FB
GB
FB
GB
PB
WB
UB
TB
WB
YB
YB
aB
\B
[B
[B
[B
^B
`B
gB
nB
nB
sB
tB
sB
tB
yB
xB
yB
zB
yB
|B
{B
yG�O�B
�B
#�B
'�B
,�B
3B
8B
>FB
CbB
GB
N�B
S�B
V�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.43 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071417292016080714172920160807141729  AO  ARCAADJP                                                                    20150226221239    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150226221239  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150226221239  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807141729  IP                  G�O�G�O�G�O�                