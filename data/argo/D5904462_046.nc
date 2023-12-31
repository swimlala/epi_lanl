CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2015-04-13T19:18:59Z AOML 3.0 creation; 2016-08-07T21:51:16Z UW 3.1 conversion     
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
_FillValue                    A�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  C�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    K�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  M�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  U|   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]p   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _p   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gd   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  id   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qX   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yL   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {L   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �4   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    �d   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    �d   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    �d   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  �d   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20150413191859  20160807145116  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               .A   AO  5287_9017_046                   2C  D   APEX                            6529                            072314                          846 @�H��˯�1   @�Hň���@1}�E���d�333331   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    .A   B   B   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&�fD'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DAfDA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  DtffDy` D�fD�FfD��fD�� D�fD�L�D�vfD�ɚD� D�FfD��3D�ٚD� D�33D�vfD��fD�3D�L�D�|�D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�@�A�HA&�HAF�HAf�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB	�RB�RB�RB!�RB)�RB1�RB9�RBA�RBI�RBQ�RBY�RBa�RBi�RBq�RBy�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C nCnCnCnCnC
nCnCnCnCnCnCnCnCnCnCnC nC"nC$nC&nC(nC*nC,nC.nC0nC2nC4nC6nC8nC:nC<nC>nC@nCBnCDnCFnCHnCJnCLnCNnCPnCRnCTnCVnCXnCZnC\nC^nC`nCbnCdnCfnChnCjnClnCnnCpnCrnCtnCvnCxnCznC|nC~nC�7
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
C�*=C�*=C�7
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
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA!�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy{�D�$)D�T)D��)D���D�)D�Z�D��)D��]D��D�T)D���D��]D��D�@�Dڄ)D��)D��D�Z�D�D��]11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�33A�+A�"�A�bA�ȴȦ+A�M�A�;dA�/A�+A�&�A�$�A�"�A��A�oA���A˧�A�S�A�A��mA�hsA�/Aȟ�AǇ+AƸRA�bNA��A� �A�ZA��Aã�A�Q�A���APA�n�A�S�A��A��TA��yA�\)A��A���A�33A���A��#A���A��!A�`BA��A��FA�ZA� �A��A��/A��yA�ȴA��!A�hsA�$�A���A���A��+A�`BA�&�A�ȴA��A�r�A�JA�r�A�A�jA��A��A��A���A���A�/A���A��A���A��9A�ZA��A��A�l�A�bA���A�E�A���A���A���A��PA�r�A�E�A�n�A��hA��jA��A��A�\)A�v�A�`BA�{A���A���A�~�A�n�A�ĜA�/A���A��mA�A���A��hA�
=A�A�E�A��9A�;dA��A��^A��uA���A�I�A�JA�A�v�A��yA��AVA}A{�FAy��At�yAm�TAg��Ac7LA` �A]�AY�-AWG�AT�+AS��AR�uAQK�AN�/AJ�uAHE�AFn�AEVADffAC��AB�HAB$�A@�A=��A<{A:M�A933A8VA7t�A6�A5�A1�A0��A.bNA+�
A*�!A*1A(�+A'��A&9XA%A$�A$  A"ĜA!�A�A�^A�RA��A��Ax�AVAC�A
=A{Al�Ar�AE�A�A33A��Ap�A�A��A^5A�^AoA
=A��A �AO�A{A\)A�\A��A�/A��A�#A-AoA
  A	|�A	O�A�A��A~�A�A\)A~�A��A�uA%A%A�A�A�TAdZA�A�RA9XAl�A��AK�A ��A n�@�+@��H@���@���@��@�|�@�o@���@��y@�o@�|�@�t�@�-@�j@��@�j@�R@��y@�"�@�w@��;@�dZ@�hs@�w@���@�o@�-@��@�9X@�1'@���@�|�@�R@�$�@�O�@�V@�-@�@��@��@��@�+@�M�@�J@�-@���@�z�@�  @ޟ�@�Z@�"�@�G�@҇+@�Q�@�  @ϝ�@Ο�@Ͳ-@�x�@�7L@�&�@���@�?}@�z�@���@�V@Ǯ@ư!@��@�/@���@��H@�1@�9X@���@��`@�@�@���@�%@��@�1'@���@�l�@�K�@�+@�ȴ@��\@�-@��@��-@��7@�p�@���@��@��
@��@�C�@�;d@�t�@�|�@�"�@���@�J@���@�O�@��@��@�K�@��@�
=@���@��+@�V@�$�@�@�&�@�(�@��
@��F@���@�|�@�K�@�;d@�
=@�V@�$�@�{@��@��h@�hs@��@���@���@�r�@�I�@� �@��@�ƨ@��@���@�dZ@�
=@���@�5?@���@�x�@�p�@�X@�/@��@���@�Ĝ@���@�r�@�Z@�Z@�Z@��@��m@��;@�ƨ@��@�C�@���@�ff@�@���@�V@�z�@��m@�S�@��@�33@�C�@�S�@�l�@�K�@���@�ff@�p�@��`@� �@��P@���@�{@���@��-@���@��7@�V@�bN@�I�@��@��@��@���@�dZ@���@��+@�5?@��T@��h@�O�@��`@���@�9X@�  @���@��P@�l�@�33@���@��@�V@�@��-@��7@�&�@��@��;@��P@�;d@�
=@���@�M�@��@��^@�`B@��/@�bN@���@�ƨ@���@��P@�S�@��@���@�v�@�^5@�{@���@��-@���@��h@��7@��@�X@���@��/@��@�
=@���@��@�Z@z��@qx�@e�-@^��@V��@P  @HA�@@A�@97L@0�u@*��@%�-@ �`@1@;d@hs@�h11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   A�33A�+A�"�A�bA�ȴȦ+A�M�A�;dA�/A�+A�&�A�$�A�"�A��A�oA���A˧�A�S�A�A��mA�hsA�/Aȟ�AǇ+AƸRA�bNA��A� �A�ZA��Aã�A�Q�A���APA�n�A�S�A��A��TA��yA�\)A��A���A�33A���A��#A���A��!A�`BA��A��FA�ZA� �A��A��/A��yA�ȴA��!A�hsA�$�A���A���A��+A�`BA�&�A�ȴA��A�r�A�JA�r�A�A�jA��A��A��A���A���A�/A���A��A���A��9A�ZA��A��A�l�A�bA���A�E�A���A���A���A��PA�r�A�E�A�n�A��hA��jA��A��A�\)A�v�A�`BA�{A���A���A�~�A�n�A�ĜA�/A���A��mA�A���A��hA�
=A�A�E�A��9A�;dA��A��^A��uA���A�I�A�JA�A�v�A��yA��AVA}A{�FAy��At�yAm�TAg��Ac7LA` �A]�AY�-AWG�AT�+AS��AR�uAQK�AN�/AJ�uAHE�AFn�AEVADffAC��AB�HAB$�A@�A=��A<{A:M�A933A8VA7t�A6�A5�A1�A0��A.bNA+�
A*�!A*1A(�+A'��A&9XA%A$�A$  A"ĜA!�A�A�^A�RA��A��Ax�AVAC�A
=A{Al�Ar�AE�A�A33A��Ap�A�A��A^5A�^AoA
=A��A �AO�A{A\)A�\A��A�/A��A�#A-AoA
  A	|�A	O�A�A��A~�A�A\)A~�A��A�uA%A%A�A�A�TAdZA�A�RA9XAl�A��AK�A ��A n�@�+@��H@���@���@��@�|�@�o@���@��y@�o@�|�@�t�@�-@�j@��@�j@�R@��y@�"�@�w@��;@�dZ@�hs@�w@���@�o@�-@��@�9X@�1'@���@�|�@�R@�$�@�O�@�V@�-@�@��@��@��@�+@�M�@�J@�-@���@�z�@�  @ޟ�@�Z@�"�@�G�@҇+@�Q�@�  @ϝ�@Ο�@Ͳ-@�x�@�7L@�&�@���@�?}@�z�@���@�V@Ǯ@ư!@��@�/@���@��H@�1@�9X@���@��`@�@�@���@�%@��@�1'@���@�l�@�K�@�+@�ȴ@��\@�-@��@��-@��7@�p�@���@��@��
@��@�C�@�;d@�t�@�|�@�"�@���@�J@���@�O�@��@��@�K�@��@�
=@���@��+@�V@�$�@�@�&�@�(�@��
@��F@���@�|�@�K�@�;d@�
=@�V@�$�@�{@��@��h@�hs@��@���@���@�r�@�I�@� �@��@�ƨ@��@���@�dZ@�
=@���@�5?@���@�x�@�p�@�X@�/@��@���@�Ĝ@���@�r�@�Z@�Z@�Z@��@��m@��;@�ƨ@��@�C�@���@�ff@�@���@�V@�z�@��m@�S�@��@�33@�C�@�S�@�l�@�K�@���@�ff@�p�@��`@� �@��P@���@�{@���@��-@���@��7@�V@�bN@�I�@��@��@��@���@�dZ@���@��+@�5?@��T@��h@�O�@��`@���@�9X@�  @���@��P@�l�@�33@���@��@�V@�@��-@��7@�&�@��@��;@��P@�;d@�
=@���@�M�@��@��^@�`B@��/@�bN@���@�ƨ@���@��P@�S�@��@���@�v�@�^5@�{@���@��-@���@��h@��7@��@�X@���@��/G�O�@�
=@���@��@�Z@z��@qx�@e�-@^��@V��@P  @HA�@@A�@97L@0�u@*��@%�-@ �`@1@;d@hs@�h11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oBŢBƨBƨBƨB��B��B�
B�B�/B�5B�BB�NB�TB�fB�B��B%BJB�B�B1'BR�BT�Bu�B��B��B�LB�qBȴB��B�NB�mB��B  BB+BDB{B-B?}BJ�BYBVBVBW
BW
BZBaHBe`Bo�Bt�Bs�Bt�Bt�B~�B~�B|�B|�B� B�B�B�%B�+B�=B�VB�\B�VB�\B�{B��B�+B�oB�hB��B�{B�=B�B� Br�BYBK�B0!B�B  B�sB�B��BB�FB�B�B�B��B��B�bBq�BffB\)BM�B@�B%�B�BB��B��B��B��B�B�9B��B�%BjBZBQ�BH�B8RB,B!�B�B�BJB
��B
�;B
��B
ǮB
��B
�RB
�PB
aHB
R�B
<jB
-B
�B	��B	��B	�bB	gmB	S�B	C�B	2-B	'�B	�B	�B	{B	VB	B��B��B��B��B	  B��B��B��B��B��B�B�B��B��B��B��B��B	B��B	B	+B	
=B	{B	oB	�B	�B	�B	�B	�B	bB	B�B�yB�`B�mB�yB�B�B�B��B��B��B��B��B�B�B�B��B��B��B��B��B	B	DB	�B	2-B	.B	!�B	!�B	C�B	N�B	R�B	VB	T�B	N�B	L�B	F�B	N�B	S�B	T�B	VB	ZB	\)B	^5B	^5B	aHB	t�B	|�B	}�B	� B	�B	�1B	�+B	�7B	�7B	�%B	�B	|�B	�B	�PB	�VB	�7B	�7B	�JB	�\B	�JB	�=B	�oB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	�{B	�DB	�%B	�B	{�B	s�B	��B	�{B	�\B	�PB	�DB	�DB	��B	��B	��B	�hB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�oB	�\B	�%B	m�B	cTB	bNB	bNB	ffB	jB	l�B	o�B	s�B	y�B	�B	�7B	�1B	~�B	w�B	u�B	x�B	y�B	�%B	�\B	��B	��B	��B	�oB	�JB	�PB	�bB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�3B	�?B	�FB	�LB	�RB	�RB	�LB	�XB	�dB	�qB	�qB	�wB	�wB	�qB	�qB	�wB	�wB	�}B	��B	B	B	B	B	ÖB	ĜB	ƨB	ȴB	ǮB	ǮB	ƨB	ȴB	ɺB	ɺB	ȴB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�
B	�B	�B	�B	�B	�#B	�)B	�)B	�/B	�5B	�;B	�BB	�HB	�NB	�NB	�TB	�ZB	�`B	�mB	�sB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�yB	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
+B
\B
�B
�B
%�B
)�B
5?B
:^B
<jB
B�B
I�B
Q�B
YB
^5B
cTB
ffB
k�B
o�B
r�B
v�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   B�}BƄBƃBƄBʞBϸB��B��B�B�B�B�*B�1B�AB�iB��BB#BpB�B1BR�BT�Bu�B�dB��B�)B�MBȔBʠB�+B�JB��B��B�BB$BYB,�B?ZBJ�BX�BU�BU�BV�BV�BY�Ba'BeCBoBt�Bs�Bt�Bt�B~�B~�B|�B|�B�B��B��B�B�B�B�5B�<B�6B�;B�ZB�cB�
B�OB�HB�mB�ZB�B��B�Br�BX�BK�B/�B_B��B�OB��BʝB�lB�#B��B��B��B��B��B�;Bq�Bf@B\BM�B@YB%�B^B�B��B��B��B��B�[B�B�\B��BjZBY�BQ�BH�B8+B+�B!�BlBVB$B
��B
�B
̦B
ǅB
�bB
�*B
�,B
a"B
R�B
<DB
,�B
uB	��B	�bB	�BB	gLB	S�B	CwB	2B	'�B	�B	uB	^B	;B	�B��B��B��B��B��B��B��B��B��B��B��B�zB��B��B��B��B��B	 �B��B	 �B	
B	
B	[B	KB	�B	�B	�B	�B	}B	@B	�B�B�YB�>B�MB�YB�B��B��B��B��B��B��B��B�B�~B�{B��B��B��B��B��B	 �B	B	�B	2B	-�B	!�B	!�B	CnB	N�B	R�B	U�B	T�B	N�B	L�B	F�B	N�B	S�B	T�B	U�B	Y�B	\B	^B	^B	aB	t�B	|�B	}�B	�B	��B	�B	�B	�B	�B	��B	��B	|�B	��B	�'B	�+B	�B	�B	�B	�2B	�B	�B	�DB	�\B	�oB	��B	��B	��B	��B	��B	��B	��B	�tB	��B	��B	��B	��B	��B	��B	��B	�QB	�B	��B	��B	{�B	s�B	�hB	�RB	�/B	�'B	�B	�B	�\B	�tB	�[B	�;B	�fB	��B	��B	��B	��B	��B	��B	�rB	�\B	�CB	�/B	��B	meB	c*B	b#B	b#B	f;B	jVB	l_B	osB	s�B	y�B	��B	�B	�B	~�B	w�B	u�B	x�B	y�B	��B	�-B	�dB	��B	�rB	�CB	�B	�$B	�8B	�OB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�$B	�$B	�B	�)B	�7B	�CB	�DB	�JB	�KB	�AB	�BB	�IB	�JB	�OB	�YB	�_B	�`B	�`B	�aB	�cB	�kB	�xB	ȅB	ǀB	ǀB	�zB	ȄB	ɋB	ɊB	ȇB	ɌB	ʖB	ʑB	˘B	˗B	̜B	̝B	̜B	ͥB	ΫB	ϰB	жB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	� B	�B	�%B	�+B	�1B	�;B	�CB	�MB	�TB	�[B	�_B	�mB	�nB	�nB	�_B	�\B	�UB	�SB	�OB	�GB	�NB	�ZB	�rB	�nB	�hB	�eB	�sB	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�G�O�B
�B
+B
OB
�B
%�B
)�B
5
B
:,B
<8B
B\B
I�B
Q�B
X�B
^B
c B
f3B
kQB
ojB
r}B
v�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.43 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451162016080714511620160807145116  AO  ARCAADJP                                                                    20150413191859    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20150413191859  QCP$                G�O�G�O�G�O�DFB5E           AO  ARGQQCPL                                                                    20150413191859  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145116  IP                  G�O�G�O�G�O�                