CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2016-03-10T10:16:59Z AOML 3.0 creation; 2016-08-07T21:51:27Z UW 3.1 conversion     
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
resolution        :�o     �  U�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    ]�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  _�   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    gx   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ix   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  qp   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    yh   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {h   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �`   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �(   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �,   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �4Argo profile    3.1 1.2 19500101000000  20160310101659  20160807145127  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               mA   AO  5287_9017_109                   2C  D   APEX                            6529                            072314                          846 @כ��G��1   @כӌSy�@.'-�d�-V1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    mA   B   B   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(ffB0  B8  B@  BHffBP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C;�fC>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\�C^�C_�fCb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� DtٚDy� D� D�L�D��3D�� D� D�S3D�� D�� D�	�D�9�D��fD��3D� D�C3Dڌ�D���D�  D�)�D�s3D�ɚ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @���@�A�HA&�HAF�HAf�HA�p�A�p�A�p�A�p�A�p�A�p�A�p�A�p�B�RB	�RB�RB�RB!�RB*�B1�RB9�RBA�RBJ�BQ�RBY�RBa�RBi�RBq�RBy�RB��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)B��)C nCnCnCnCnC
nCnCnCnCnCnCnCnCnCnCnC nC"nC$nC&nC(nC*nC,nC.nC0nC2nC4nC6nC8nC:nC<TzC>nC@nCBnCDnCFnCHnCJnCLnCNnCPnCRnCTnCVnCXnCZnC\��C^��C`TzCbnCdnCfnChnCjnClnCnnCpnCrnCtnCvnCxnCznC|nC~nC�7
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
C�*=C�7
C�7
C�7
C�7
C�7
C�7
C�*=C�7
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
D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�Dy��D��D�Z�D���D���D��D�`�D���D���D�]D�G]D��)D���D��D�P�Dښ�D�ڐD��D�7]D��D��]111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�v�A�t�A�r�A�t�A�t�A�t�A�r�A�x�A�v�A�v�A�z�A˅Aˉ7AˍPA˓uA˕�A˕�A˗�A˕�Aˏ\AˍPA˓uAˍPA˝�A˛�Aˡ�Aˣ�A˥�A˧�A˩�AˮAˮAˣ�AˍPA�jA��TAʅA�^5A�(�A�VA���A�O�A���AȼjAȬA�bNAǼjA���AƮA�p�A��A�oA�oA�oA�bA�JA�/A��A�ĜA�1A�~�A�I�A���A��yA���A��^A��\A��HA���A�$�A�1A���A��+A��7A���A�XA���A�oA��A��A�JA��A��A�1A�~�A��yA�hsA�O�A���A��A�Q�A��/A�hsA��hA�A��A�
=A��HA�p�A�l�A��yA�~�A���A��/A��-A���A��yA��A�%A~�A{K�An�Al��Ak�Ag+Ae%Ab�A_K�A\ffAY�wAX�+AUC�AQ�AN�/AL��AJĜAJ(�AI&�AHQ�AF��AD�/AC�AC&�ABr�AA�A?"�A=\)A:1'A8JA5�A4��A3�
A3"�A2-A1�A0bA.��A,ĜA,1A*�jA)�A)�PA)�7A(��A(-A&��A$�A$�!A$^5A#��A"��A!�TA �A��AȴA�`AȴAĜA��Ar�AA�AG�AĜAbNA�-AS�A^5A��AjA{A�-A�A��A~�A�A?}A��A�wAdZA�AK�A�#A�jA��A��A��A�A^5A~�A�wA�FA
�`A+A��A
��A�A	%A	/A	��A�9A��A"�AoA��A�A�A7LA��AĜA��A-A7LA ~�@�$�@�v�@�Ĝ@���@�ff@�dZ@�+@��@���@�E�@�V@��@�p�@�r�@���@�x�@�
=@�@�I�@��
@땁@�ȴ@�X@� �@��@�C�@���@�E�@�9X@��@��D@��@�{@�G�@�r�@��y@�{@ٙ�@�@��#@���@�x�@�C�@Ӿw@�n�@��@��@��@�7L@�/@��@��@�p�@�dZ@ΰ!@�p�@�1'@��m@�K�@ʟ�@��@ɲ-@�V@�r�@��@ǍP@�;d@�o@���@�M�@�X@��@��/@�I�@�9X@�9X@� �@�ƨ@�o@���@°!@�n�@�E�@��@��@���@�p�@���@��/@��D@��@���@�l�@��@��R@��@�hs@�/@�/@��@���@��9@���@���@��u@�j@� �@���@�;d@�n�@���@�X@�G�@�G�@�&�@���@�j@� �@�  @���@�+@��!@�~�@�E�@��-@��7@�`B@�%@���@���@�bN@��
@�S�@�@���@�E�@�E�@�-@�-@�J@��@��#@��^@�x�@�G�@���@�@���@��!@�J@���@�X@���@�(�@��F@�\)@�C�@�o@��!@�V@�=q@���@��@��T@��T@��#@��^@�hs@�?}@�%@��9@�I�@��
@�l�@�33@�+@�"�@�@��R@��+@�V@�@��@�O�@�7L@�V@��@�Ĝ@���@�9X@�1@�ƨ@���@�\)@���@��y@��R@�V@���@��u@��@�ƨ@���@��
@��@�v�@�E�@�=q@�-@��T@�hs@��9@�j@�Q�@�(�@�  @���@���@���@���@�x�@�O�@�7L@��@���@���@�A�@��w@���@�|�@���@�ff@��@��T@���@�@��@�z�@�Q�@� �@��P@�S�@�o@��H@�ȴ@��!@��+@�=q@���@��-@��h@��@��@���@�Z@�1'@��;@���@��@�l�@�K�@�o@���@��!@���@�I�@�S�@�Q�@r�H@g�@b^5@[�m@P �@Fv�@>v�@:J@3��@*�\@#�F@�@�@{@C�@�-@
^5@1'111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�v�A�t�A�r�A�t�A�t�A�t�A�r�A�x�A�v�A�v�A�z�A˅Aˉ7AˍPA˓uA˕�A˕�A˗�A˕�Aˏ\AˍPA˓uAˍPA˝�A˛�Aˡ�Aˣ�A˥�A˧�A˩�AˮAˮAˣ�AˍPA�jA��TAʅA�^5A�(�A�VA���A�O�A���AȼjAȬA�bNAǼjA���AƮA�p�A��A�oA�oA�oA�bA�JA�/A��A�ĜA�1A�~�A�I�A���A��yA���A��^A��\A��HA���A�$�A�1A���A��+A��7A���A�XA���A�oA��A��A�JA��A��A�1A�~�A��yA�hsA�O�A���A��A�Q�A��/A�hsA��hA�A��A�
=A��HA�p�A�l�A��yA�~�A���A��/A��-A���A��yA��A�%A~�A{K�An�Al��Ak�Ag+Ae%Ab�A_K�A\ffAY�wAX�+AUC�AQ�AN�/AL��AJĜAJ(�AI&�AHQ�AF��AD�/AC�AC&�ABr�AA�A?"�A=\)A:1'A8JA5�A4��A3�
A3"�A2-A1�A0bA.��A,ĜA,1A*�jA)�A)�PA)�7A(��A(-A&��A$�A$�!A$^5A#��A"��A!�TA �A��AȴA�`AȴAĜA��Ar�AA�AG�AĜAbNA�-AS�A^5A��AjA{A�-A�A��A~�A�A?}A��A�wAdZA�AK�A�#A�jA��A��A��A�A^5A~�A�wA�FA
�`A+A��A
��A�A	%A	/A	��A�9A��A"�AoA��A�A�A7LA��AĜA��A-A7LA ~�@�$�@�v�@�Ĝ@���@�ff@�dZ@�+@��@���@�E�@�V@��@�p�@�r�@���@�x�@�
=@�@�I�@��
@땁@�ȴ@�X@� �@��@�C�@���@�E�@�9X@��@��D@��@�{@�G�@�r�@��y@�{@ٙ�@�@��#@���@�x�@�C�@Ӿw@�n�@��@��@��@�7L@�/@��@��@�p�@�dZ@ΰ!@�p�@�1'@��m@�K�@ʟ�@��@ɲ-@�V@�r�@��@ǍP@�;d@�o@���@�M�@�X@��@��/@�I�@�9X@�9X@� �@�ƨ@�o@���@°!@�n�@�E�@��@��@���@�p�@���@��/@��D@��@���@�l�@��@��R@��@�hs@�/@�/@��@���@��9@���@���@��u@�j@� �@���@�;d@�n�@���@�X@�G�@�G�@�&�@���@�j@� �@�  @���@�+@��!@�~�@�E�@��-@��7@�`B@�%@���@���@�bN@��
@�S�@�@���@�E�@�E�@�-@�-@�J@��@��#@��^@�x�@�G�@���@�@���@��!@�J@���@�X@���@�(�@��F@�\)@�C�@�o@��!@�V@�=q@���@��@��T@��T@��#@��^@�hs@�?}@�%@��9@�I�@��
@�l�@�33@�+@�"�@�@��R@��+@�V@�@��@�O�@�7L@�V@��@�Ĝ@���@�9X@�1@�ƨ@���@�\)@���@��y@��R@�V@���@��u@��@�ƨ@���@��
@��@�v�@�E�@�=q@�-@��T@�hs@��9@�j@�Q�@�(�@�  @���@���@���@���@�x�@�O�@�7L@��@���@���@�A�@��w@���@�|�@���@�ff@��@��T@���@�@��@�z�@�Q�@� �@��P@�S�@�o@��H@�ȴ@��!@��+@�=q@���@��-@��h@��@��@���@�Z@�1'@��;@���@��@�l�@�K�@�o@���@��!G�O�@�I�@�S�@�Q�@r�H@g�@b^5@[�m@P �@Fv�@>v�@:J@3��@*�\@#�F@�@�@{@C�@�-@
^5@1'111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB  B  BB  B  B  BBBBBBBBBBBBBBBBBBBB%BDB
=B
=B	7BJBPBuB�B33B�1B�jB��B��B	7B�B;dBK�B[#B_;Bq�B��B��B�mB��BVB\B\B\B\BVB �B5?B1'B1'B/B'�B�B{BuBuBbBB�B�fB��B�B�{B�Bt�BiyBbNBQ�B_;Bt�Bt�BiyBP�BD�B49B�`B��B��B�1Bw�Bm�BcTBYB6FBbB  B
��B
�yB
�NB
ɺB
��B
�XB
�B
��B
~�B
o�B
^5B
M�B
�B
%B	�sB	��B	�bB	�%B	r�B	hsB	YB	H�B	:^B	.B	%�B	�B		7B	B��B��B�B�B�B�fB�NB�BB�/B�#B�B��B��B��B��B��B��B��BɺBŢBB��B�}BŢB�;B�mB�B��B		7B		7B	B	�B	+B	5?B	5?B	2-B	.B	+B	(�B	,B	:^B	O�B	Q�B	T�B	T�B	T�B	VB	[#B	[#B	\)B	\)B	]/B	n�B	�PB	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	��B	��B	��B	��B	�\B	�+B	�DB	�DB	�%B	�B	�1B	�\B	�DB	�{B	��B	��B	�!B	�-B	��B	�B	�!B	�FB	�FB	��B	�-B	�FB	�LB	�9B	�'B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�9B	�FB	�?B	�LB	B	ɺB	��B	ƨB	B	ÖB	ƨB	ŢB	��B	�}B	�wB	�qB	�dB	�XB	�-B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�JB	�1B	�+B	�1B	�=B	�\B	�bB	�bB	�bB	�{B	�bB	�JB	�7B	�+B	�1B	�7B	�=B	�=B	�DB	�VB	�VB	�\B	�\B	�bB	�bB	�bB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�'B	�?B	�XB	�RB	�RB	�XB	�^B	�dB	�jB	�jB	�wB	�}B	��B	��B	��B	ÖB	ƨB	ɺB	ɺB	��B	ɺB	ɺB	ɺB	ɺB	ɺB	ɺB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�
B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�5B	�BB	�ZB	�ZB	�ZB	�ZB	�`B	�`B	�`B	�fB	�fB	�fB	�mB	�sB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B
B
B
B
B
B
B
B
B
B
B
B
B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
B
B
B
B
B
B
B
B
B
B
%B
%B
%B
+B
+B
1B
	7B

=B
	7B

=B
DB
DB
DB
JB
JB
\B
hB
hB
oB
oB
uB
uB
{B
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 �B
#�B
,B
5?B
8RB
?}B
I�B
O�B
T�B
YB
_;B
ffB
k�B
p�B
t�B
x�B
{�B
�B
�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B
��B
��B �B
��B
��B
��B �B �B �B �B �B �B �B�B�B�B�B�B�B�B�B�B�B�B�BB#B
B
B	B*B-BSB�B3B�B�FB��B��B	B�B;BBK�B[B_Bq�B�xB��B�NB��B3B:B<B<B:B5B �B5B1B1	B.�B'�BBWBSBRBAB�B�B�BBʛB��B�UB��Bt�BiRBb(BQ�B_Bt�Bt�BiOBP�BDrB4B�7B�XB��B�
Bw�BmkBc+BX�B6B=B
��B
��B
�QB
�%B
ɕB
�ZB
�.B
��B
��B
~�B
otB
^B
M�B
�B
 B	�NB	��B	�BB	�B	r�B	hSB	X�B	H�B	:AB	-�B	%�B	vB		B	 �B��B��B�B�}B�cB�JB�3B�(B�B�B��B��B��B��BʹBʹBͷB˫BɚBńB�qB�fB�]BńB�B�NB�B��B		B		B	�B	^B	*�B	5B	5B	2B	-�B	*�B	(�B	+�B	:;B	O�B	Q�B	T�B	T�B	T�B	U�B	Z�B	Z�B	\B	\B	]B	nrB	�(B	�eB	�gB	�qB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�hB	�1B	�B	�B	�B	��B	��B	�B	�3B	�B	�QB	��B	��B	��B	�B	��B	��B	��B	�B	�B	��B	�B	�B	�!B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�qB	��B	��B	��B	��B	�nB	�xB	�oB	��B	��B	�B	�B	�B	�B	�dB	ɏB	ʕB	�zB	�bB	�iB	�}B	�uB	�[B	�OB	�JB	�BB	�8B	�,B	��B	��B	��B	��B	��B	��B	�tB	�_B	�kB	�sB	��B	��B	��B	��B	�aB	�B	�B	��B	�B	�B	�/B	�6B	�7B	�8B	�MB	�7B	�B	�B	��B	�B	�	B	�B	�B	�B	�)B	�)B	�0B	�/B	�7B	�5B	�8B	�HB	�eB	�kB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�)B	�%B	�"B	�*B	�0B	�7B	�;B	�<B	�KB	�RB	�TB	�[B	�ZB	�gB	�yB	ɉB	ɋB	ʓB	ɎB	ɌB	ɌB	ɍB	ɍB	ɌB	ʒB	ʑB	˘B	̝B	ͥB	ΩB	ϭB	ϮB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�+B	�,B	�+B	�*B	�2B	�2B	�/B	�5B	�7B	�6B	�?B	�BB	�GB	�LB	�TB	�VB	�TB	�TB	�UB	�`B	�aB	�oB	�tB	�}B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
 �B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
 �B
 �B
 �B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
	B

B
	B


B
B
B
B
B
B
)B
7B
8B
<B
>B
BB
CB
GB
GB
HB
OB
MB
NB
TB
UB
UB
WB
VB
MB
NB
OB
PB
PB
LB
NB
MB
TB
TG�O�B
bB
 �B
#�B
+�B
5B
8B
?IB
I�B
O�B
T�B
X�B
_B
f2B
kQB
pqB
t�B
x�B
{�B
��B
��B
��111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.43 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201608071451272016080714512720160807145127  AO  ARCAADJP                                                                    20160310101659    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20160310101659  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20160310101659  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160807145127  IP                  G�O�G�O�G�O�                