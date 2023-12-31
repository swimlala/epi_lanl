CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       N2014-07-21T23:08:34Z AOML 3.0 creation; 2016-06-01T00:08:14Z UW 3.1 conversion     
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20140721230834  20160531170814  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               8A   AO  4055_7112_056                   2C  D   APEX                            5374                            041511                          846 @ֺF �/�1   @ֺF��@9�O�;dZ�c�z�G�1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    8A   A   A   @�33@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bg��Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D�fD  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dyy�D�� D�S3D�y�D��fD�	�D�I�D�|�D���D� D�6fD�l�D�� D��fD�FfDڙ�D��fD��3D�6fD�\�D��f11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�z�@�G�A��A$��AD��Ad��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B	(�B(�B(�B!(�B)(�B1(�B9(�BA(�BI(�BQ(�BY(�Ba(�BhBq(�By(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BĔ{BȔ{B̔{BД{BԔ{Bؔ{Bܔ{B��{B�{B�{B�{B�{B��{B��{B��{C J=CJ=CJ=CJ=CJ=C
J=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=C J=C"J=C$J=C&J=C(J=C*J=C,J=C.J=C0J=C2J=C4J=C6J=C8J=C:J=C<J=C>J=C@J=CBJ=CDJ=CFc�CHJ=CJJ=CLJ=CNJ=CPJ=CRJ=CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=CbJ=CdJ=CfJ=ChJ=CjJ=ClJ=CnJ=CpJ=CrJ=CtJ=CvJ=CxJ=CzJ=C|J=C~J=C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dy�)D��HD�\{D���D��D��D�R�D��D��D�HD�?�D�vD��HD���D�O�Dڢ�D�ϮD��{D�?�D�fD��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�p�A�;dA�bA׋DA��A�Aֲ-A֝�A֑hAօA�x�A�r�A�n�A�jA�ffA�dZA�`BA�\)A�VA�M�A��A��
A�bNA�ȴA��yA�bNA��A���A���A�=qA�`BA��
A�ZA��A�;dA�A���A�oA�A�-A�9XA�VA��A��A��-A�K�A��-A��A��^A���A��A�O�A��\A�A��FA�dZA��A�{A�VA��A���A���A��RA��A�;dA���A�I�A�
=A��DA��A�bNA��HA��RA���A��A��A�^5A�|�A�O�A���A�VA���A�ƨA���A��A�A��DA�jA��wA�ƨA�oA�G�A�/A��#A���A��7A��A{�Av��AsAq�7An�HAmXAk��Aj1Ai�Agt�Af-Ad1Aa33A`�A\ZAY`BAV�9AU�;AU%ATĜATv�AT(�ASoAP�AO�AN�yAN�AM�PAL�ALffAJ�`AI�wAIoAG�AD��ACXAB~�A@�!A=�^A;�A:jA9�A8�\A8(�A8�A77LA4��A4VA4Q�A4A�A3��A2��A1��A0�yA/�A/;dA/VA.��A.�A.ffA.{A-x�A,��A*ZA)K�A(ĜA(1'A'O�A'�A&�/A&�uA&Q�A%A%%A$^5A"��A"A �\AA5?A�-A
=A��A(�Ax�A��An�A�A��A�uA^5A��A��AjA �A�TA�jA$�A�wAE�A�;A�hAA-A�FA`BA/AĜA  AA$�A
�A	�mA	A{AĜA�wA��A��AƨAoAJAC�A ��A �A �A 5?@�5?@�O�@�ȴ@��@�(�@�;d@���@��@�C�@��`@��D@�K�@�^5@��#@�x�@�%@���@�r�@���@陚@�h@�bN@�  @�dZ@�@�-@߾w@�V@�O�@ܓu@�=q@ؼj@�ff@��/@�  @��@��
@��T@͉7@�V@�  @�K�@�-@ɲ-@���@Ǖ�@ŉ7@Ĵ9@�  @Ý�@�
=@�V@��/@�(�@��@��@�7L@�  @�@��9@��w@��R@�J@�G�@���@��`@��@�1'@�"�@�~�@�5?@�G�@��;@���@���@��@��9@��@�bN@�Q�@�  @�1@��@�|�@��@�@�G�@�X@��@��\@�ff@��h@�x�@��-@��@�?}@�Ĝ@�ƨ@�@���@�?}@�r�@�A�@�1'@��P@���@�J@�J@���@���@���@�E�@�v�@��@���@���@��w@��@�t�@�l�@��y@���@��@��H@��\@�E�@��@�$�@�-@���@�&�@���@��j@���@�Q�@���@���@��@�^5@��@��@�V@��9@���@���@���@��u@�1@�t�@�ȴ@��@���@�`B@�G�@��@���@�  @�;d@��!@�ff@���@�X@���@��@�S�@���@�^5@�$�@��7@�?}@�7L@��9@�1'@��m@���@�C�@��H@���@�$�@��-@�x�@�`B@��@�Ĝ@��9@���@��D@�r�@�I�@�b@��P@�;d@�
=@��!@�v�@�M�@�-@�{@���@��@���@��-@���@���@��@�p�@�hs@�?}@�V@��j@��@� �@K�@;d@+@~��@~5?@}�T@}��@}O�@}V@|�@|j@|9X@{��@{ƨ@{t�@{33@z�H@zn�@yx�@x�`@xĜ@xĜ@x�9@x��@x�u@x�@xb@wl�@w�@vȴ@v�+@vv�@v5?@v{@u��@t�@s��@r��@r~�@r=q@rJ@rJ@q�@q�#@q��@q��@q��@q�7@q7L@q�@p��@p��@mV@f�@]@Tz�@L1@Dj@=p�@9��@5�@0��@*�@&{@"�\@o@�`@ƨ@�+@t�@
J@�`@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�p�A�;dA�bA׋DA��A�Aֲ-A֝�A֑hAօA�x�A�r�A�n�A�jA�ffA�dZA�`BA�\)A�VA�M�A��A��
A�bNA�ȴA��yA�bNA��A���A���A�=qA�`BA��
A�ZA��A�;dA�A���A�oA�A�-A�9XA�VA��A��A��-A�K�A��-A��A��^A���A��A�O�A��\A�A��FA�dZA��A�{A�VA��A���A���A��RA��A�;dA���A�I�A�
=A��DA��A�bNA��HA��RA���A��A��A�^5A�|�A�O�A���A�VA���A�ƨA���A��A�A��DA�jA��wA�ƨA�oA�G�A�/A��#A���A��7A��A{�Av��AsAq�7An�HAmXAk��Aj1Ai�Agt�Af-Ad1Aa33A`�A\ZAY`BAV�9AU�;AU%ATĜATv�AT(�ASoAP�AO�AN�yAN�AM�PAL�ALffAJ�`AI�wAIoAG�AD��ACXAB~�A@�!A=�^A;�A:jA9�A8�\A8(�A8�A77LA4��A4VA4Q�A4A�A3��A2��A1��A0�yA/�A/;dA/VA.��A.�A.ffA.{A-x�A,��A*ZA)K�A(ĜA(1'A'O�A'�A&�/A&�uA&Q�A%A%%A$^5A"��A"A �\AA5?A�-A
=A��A(�Ax�A��An�A�A��A�uA^5A��A��AjA �A�TA�jA$�A�wAE�A�;A�hAA-A�FA`BA/AĜA  AA$�A
�A	�mA	A{AĜA�wA��A��AƨAoAJAC�A ��A �A �A 5?@�5?@�O�@�ȴ@��@�(�@�;d@���@��@�C�@��`@��D@�K�@�^5@��#@�x�@�%@���@�r�@���@陚@�h@�bN@�  @�dZ@�@�-@߾w@�V@�O�@ܓu@�=q@ؼj@�ff@��/@�  @��@��
@��T@͉7@�V@�  @�K�@�-@ɲ-@���@Ǖ�@ŉ7@Ĵ9@�  @Ý�@�
=@�V@��/@�(�@��@��@�7L@�  @�@��9@��w@��R@�J@�G�@���@��`@��@�1'@�"�@�~�@�5?@�G�@��;@���@���@��@��9@��@�bN@�Q�@�  @�1@��@�|�@��@�@�G�@�X@��@��\@�ff@��h@�x�@��-@��@�?}@�Ĝ@�ƨ@�@���@�?}@�r�@�A�@�1'@��P@���@�J@�J@���@���@���@�E�@�v�@��@���@���@��w@��@�t�@�l�@��y@���@��@��H@��\@�E�@��@�$�@�-@���@�&�@���@��j@���@�Q�@���@���@��@�^5@��@��@�V@��9@���@���@���@��u@�1@�t�@�ȴ@��@���@�`B@�G�@��@���@�  @�;d@��!@�ff@���@�X@���@��@�S�@���@�^5@�$�@��7@�?}@�7L@��9@�1'@��m@���@�C�@��H@���@�$�@��-@�x�@�`B@��@�Ĝ@��9@���@��D@�r�@�I�@�b@��P@�;d@�
=@��!@�v�@�M�@�-@�{@���@��@���@��-@���@���@��@�p�@�hs@�?}@�V@��j@��@� �@K�@;d@+@~��@~5?@}�T@}��@}O�@}V@|�@|j@|9X@{��@{ƨ@{t�@{33@z�H@zn�@yx�@x�`@xĜ@xĜ@x�9@x��@x�u@x�@xb@wl�@w�@vȴ@v�+@vv�@v5?@v{@u��@t�@s��@r��@r~�@r=q@rJ@rJ@q�@q�#@q��@q��@q��@q�7@q7L@q�@p��@p��@mV@f�@]@Tz�@L1@Dj@=p�@9��@5�@0��@*�@&{@"�\@o@�`@ƨ@�+@t�@
J@�`@�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�+B�%B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B~�BgmBL�B33B�B�B��B2-B/B0!B0!B.B49B:^B=qB=qB@�BE�BE�B@�BC�BC�BB�BA�BA�BA�B@�B8RB2-B/B1'B$�B�B�BB��B��B�B�5B��BȴB�}B�B��B�1Bk�BL�B1'B{B
=B��B�B�B�sB�`B�
BȴB�XB��B��B�PB}�Bl�B\)BI�B@�B:^B5?B"�B
�B
��B
��B
�B
��B
�JB
s�B
N�B
{B	�B	��B	ŢB	�9B	��B	��B	�bB	�1B	}�B	s�B	dZB	Q�B	F�B	.B	�B	JB	+B	B	B	B	+B	B�B�B�yB�mB�TB�5B�)B�#B�HB�)B��B�}B�LB�-B�B�B�B��B��B��B��B��B��B��B��B��B��B��B�uB�oB�hB�bB�hB�hB�hB�hB�bB�hB�bB�=B�1B�%B�B�B�B� B� B� B� B� B|�Bz�Bv�Br�Bn�BjBffBe`BcTBaHB_;B]/B\)B[#BXBVBT�BS�BR�BP�BN�BL�BK�BI�BG�BE�BB�B@�B?}B=qB<jB;dB:^B9XB7LB49B2-B/B,B(�B%�B#�B"�B"�B �B�B�B�B�B�B�B�B�B�BuBoBhBbB\BVBPBDB
=BDB
=B
=B
=B
=B	7B	7B	7B1B%BB+B+B+B+B%BBB%BBBBBBBBBB+B+B+B	7B	7B
=B
=B
=B
=BPBVBVB\B\B\BhBoB{B{B�B�B�B�B�B�B�B�B �B �B �B �B!�B$�B%�B%�B%�B%�B'�B,B0!B2-B33B49B7LB8RB9XB:^B?}BB�BA�BB�BB�BA�BD�BF�BJ�BN�BO�BQ�BS�BS�BP�BP�BS�BXB[#B[#B[#B]/BbNBffBl�Bs�Br�Bt�Bv�Bt�By�B|�B}�B�B�B�+B�DB�PB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�!B�9B�FB�RB�dB�qB�wB�wB�wB�qB��BĜBǮB��B��B��B��B��B�B�B�BB�ZB�`B�sB�B��B��B��B��B	B	B	
=B	JB	DB	\B	uB	�B	�B	�B	�B	 �B	%�B	)�B	,B	-B	/B	2-B	33B	33B	49B	5?B	6FB	8RB	<jB	@�B	A�B	E�B	G�B	H�B	J�B	L�B	N�B	O�B	P�B	R�B	R�B	R�B	S�B	T�B	T�B	VB	VB	ZB	^5B	cTB	iyB	k�B	m�B	q�B	s�B	u�B	v�B	{�B	|�B	}�B	� B	�B	�B	�B	�B	�B	�%B	�1B	�VB	�hB	�hB	�hB	�oB	�oB	�oB	�uB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�!B	�!B	�!B	�!B	�'B	�-B	�-B	�3B	�9B	B	�B	�B
B
bB
�B
(�B
0!B
7LB
>wB
G�B
L�B
Q�B
[#B
\)B
cTB
jB
n�B
p�B
q�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B�B� B�B�B��B�B�B�B�B�	B�B�
B�B�B�B�B�B�B�B�B~�BgcBL�B3!B�B�B�oB2B/B0
B0B-�B4!B:EB=YB=[B@oBE�BE�B@iBC�BC�BBxBAsBAqBAsB@kB8=B2B/B1B$�B�BmB�B��B��B�pB�B��BțB�aB��B��B�BkkBL�B1	BcB
 B��B��B�iB�ZB�BB��BșB�<B��B�kB�3B}�BloB\BI�B@jB:CB5$B"�B
�B
��B
�kB
��B
��B
�1B
s�B
N�B
cB	�qB	��B	ōB	�%B	��B	��B	�MB	�B	}�B	s�B	dIB	Q�B	F�B	.B	�B	=B	B	B	B	B	B	�B�B�rB�mB�_B�HB�)B�B�B�;B�B��B�rB�>B� B�	B��B��B��B��B��B��B��B��B��B��B��B��B��B�iB�eB�]B�UB�\B�^B�_B�]B�VB�_B�VB�3B�%B�B�B�B��B�B�B�B�B�B|�Bz�Bv�Br�Bn�BjsBf]BeVBcIBa?B_2B]#B\B[BXBU�BT�BS�BR�BP�BN�BL�BK�BI�BG�BE�BB�B@yB?XB=NB<`B;ZB:TB9PB7CB4B2%B/B+�B(�B%�B#�B"�B"�B �B�B�B�B}BwB�B�BhB\BQBfBEBXBSB3B,B B
B B
B
B
B
B	B	B	-BBB�B B BBB B�B�B B�B�B�BB�B�BBB�B!BB#B	B	B
B
B
B
2BDB/B1BRBRBPB^BdBqBpBwBaB�B�B�B�B�B�B �B �B �B �B!�B$�B%�B%�B%�B%�B'�B+�B0B2 B3&B4*B7>B8GB9KB:OB?pBB�BAzBB�BBBAyBD�BF�BJ�BN�BO�BQ�BS�BS�BP�BP�BS�BX B[B[B[B]Bb>BfWBlvBs�Br�Bt�Bv�Bt�By�B|�B}�B��B�	B�B�3B�<B�cB�nB��B��B��B��B��B��B��B��B��B��B��B��B��B�B�&B�2B�=B�MB�]B�cB�cB�cB�\B�nBąBǗB��B��B��B��B��B��B�B�,B�CB�IB�^B�B��B��B��B��B	B	B	
$B	1B	-B	EB	]B	oB	�B	�B	�B	 �B	%�B	)�B	+�B	,�B	.�B	2B	3B	3B	4!B	5%B	6+B	89B	<OB	@hB	AoB	E�B	G�B	H�B	J�B	L�B	N�B	O�B	P�B	R�B	R�B	R�B	S�B	T�B	T�B	U�B	U�B	ZB	^B	c9B	i\B	kkB	msB	q�B	s�B	u�B	v�B	{�B	|�B	}�B	�B	��B	��B	��B	��B	�B	�B	�B	�9B	�MB	�IB	�JB	�RB	�RB	�SB	�[B	�cB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�pB	��B	�rB
�B
BB
�B
(�B
0B
7,B
>UB
G�B
L�B
Q�B
[ B
\B
c1B
j\B
nuB
p�B
q�B
t�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.29 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. Salinity adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOW uncertainty] in PSS-78.                                                                                                        201605311708142016053117081420160531170814  AO  ARCAADJP                                                                    20140721230834    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20140721230834  QCP$                G�O�G�O�G�O�DFB7E           AO  ARGQQCPL                                                                    20140721230834  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20160531170814  IP                  G�O�G�O�G�O�                