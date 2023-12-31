CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2018-11-21T12:59:54Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7,   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  74   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7t   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        7�   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    7�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    7�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     7�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     88   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8X   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8x   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8|   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    8�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            8�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           8�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           8�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
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
_FillValue                    g�   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  i�   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  q�   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    y�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  {�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                    �|   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �|   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  �t   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �    HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �0   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �4   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �D   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         �H   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        �L   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20181121125954  20190405100759  5904462 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  5287                            2C  D   APEX                            6529                            072314                          846 @��%*�D1   @�퐻��@0!�7Kƨ�d��
=q1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@���@���A   A@  A^ffA�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C�C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(�fD(�3D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�3Dy�3D�fD�P D�|�D�� D��D�C3D�p D�ٚD�fD�33D�y�D��fD�fD�FfDږfD๚D�3D�<�D� D��3111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�{A
>A$��AD��Ac
=A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B	(�B(�B(�B!(�B)(�B1(�B9(�BA(�BI(�BQ(�BY(�Ba(�Bi(�Bq(�By(�B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BĔ{BȔ{B̔{BД{BԔ{Bؔ{Bܔ{B��{B�{B�{B�{B�{B��{B��{B��{C J=Cc�CJ=CJ=CJ=C
J=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=C J=C"J=C$J=C&J=C(J=C*J=C,J=C.J=C0J=C2J=C4J=C6J=C8J=C:J=C<J=C>J=C@J=CBJ=CDJ=CFJ=CHJ=CJJ=CLJ=CNJ=CPJ=CRJ=CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=CbJ=CdJ=CfJ=ChJ=CjJ=ClJ=CnJ=CpJ=CrJ=CtJ=CvJ=CxJ=CzJ=C|J=C~J=C�%C�%C�%C�%C�%C�%C�1�C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt��Dy��D��D�YHD��D��HD�"�D�L{D�yHD���D��D�<{D���D�ϮD��D�O�Dڟ�D���D�{D�FD�HD��{111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�%A��A��A��A��A�{A�{A��A� �A�"�A�"�A�$�A�&�A�$�A�&�A�&�A�&�A�(�A�(�A�+A�1'A�/A�$�A�&�A�1'A�;dA�?}A�O�A�ZA�hsA�t�A�x�AˍPA˶FA��
A��/A��HA��mA�  A�dZA��;A��AΓuAΓuA�n�A�S�A��A�A���A͑hA�^5A�9XA���A̓uA�K�A��A��HA�hsA���A�\)A�XA�~�A� �AìA�A��TA�ZA���A��A���A��hA��A�ffA�VA��wA�I�A��A���A��A��A�^5A�Q�A�$�A��;A���A�9XA��9A���A�M�A�hsA�K�A��^A�~�A��!A�XA���A���A�;dA��
A��7A�JA���A��Ay�mAv��AtZAp�+An9XAkx�AiO�Ag�Af��AcO�A`bNA^�yA^v�A]�AZ��AW��AU�^ATJARn�AQG�APQ�AMK�AJ �AH=qAE�
AD  AA��A@A�A>{A<M�A;�A;�A:ZA8��A7�A6ȴA69XA5x�A3�A2^5A0-A0  A.��A/�hA1&�A1?}A17LA0n�A/�A-�-A,��A*��A(��A&��A%�A&��A'7LA'G�A'��A'�A'VA&I�A$ĜA"��A"ĜA"JA �HA �A bNA�AI�A�A��AS�A��A�jA��AbA��A
=AI�A��AVA��A��A+A�
A��A�9A�hA��A"�AbA�uA=qA  A��A�7A
��AȴA�Ax�A�!A�DAn�A�A��Ar�A1'A�AA�;A�FA ��A �@�l�@�;d@���@�%@���@���@�E�@���@�V@���@��/@�1@�A�@�@��m@��m@�u@���@�u@�l�@�@���@���@�R@�`B@�V@�G�@��#@�@���@���@��@��#@�Ĝ@�Q�@�F@��@��@�@��@�9@�F@�"�@��@�v�@��T@�^@��@�?}@��@��@��@�j@�j@��;@��@�@���@�1@�b@��D@�S�@�x�@�;d@���@ۥ�@۾w@�1'@�Z@۝�@�
=@ڇ+@�5?@ٺ^@ٙ�@�G�@�V@���@؛�@�|�@և+@�J@��#@ԛ�@�\)@��@�ȴ@җ�@�M�@�@�O�@�&�@��@��@θR@���@́@�7L@̣�@��m@���@�ȴ@ʧ�@�5?@ɲ-@�V@ȓu@��m@ǶF@�t�@�;d@�V@��@Ł@���@Ĭ@�9X@Ý�@Å@�t�@�
=@\@�E�@��@�x�@�V@�9X@��m@�+@���@���@��+@�M�@��#@�X@��`@�bN@��;@�33@�ff@��h@��@��@��/@��j@��@�j@�Q�@�9X@�b@��@��@�l�@��H@�$�@�hs@�O�@��/@� �@�  @���@��@�S�@�+@��@�@���@��h@�/@�Ĝ@��j@�I�@�(�@��@��P@�K�@��@��+@�=q@��#@���@��h@�X@��`@��D@�Q�@�(�@�  @��P@�\)@�K�@�+@�
=@���@��!@���@���@�/@��9@�I�@��@��w@��@�t�@�K�@��@�^5@�E�@��@��@��@���@���@��9@���@��D@�bN@�(�@��;@���@�|�@�S�@��@�
=@��y@��+@�-@�@�p�@�&�@�Ĝ@�r�@�1'@��
@�|�@�l�@�dZ@�C�@���@���@�$�@�X@��@���@�Ĝ@��j@��9@���@��@�1@���@�"�@��@���@���@�`B@�?}@�%@���@���@��m@�K�@���@���@�n�@�E�@�{@�@�@�G�@�Ĝ@��-@��@�t�@��+@|z�@p�`@g|�@_�w@W\)@P1'@HA�@@ �@6�@.5?@(�@#@��@��@{@x�@O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A�%A��A��A��A��A�{A�{A��A� �A�"�A�"�A�$�A�&�A�$�A�&�A�&�A�&�A�(�A�(�A�+A�1'A�/A�$�A�&�A�1'A�;dA�?}A�O�A�ZA�hsA�t�A�x�AˍPA˶FA��
A��/A��HA��mA�  A�dZA��;A��AΓuAΓuA�n�A�S�A��A�A���A͑hA�^5A�9XA���A̓uA�K�A��A��HA�hsA���A�\)A�XA�~�A� �AìA�A��TA�ZA���A��A���A��hA��A�ffA�VA��wA�I�A��A���A��A��A�^5A�Q�A�$�A��;A���A�9XA��9A���A�M�A�hsA�K�A��^A�~�A��!A�XA���A���A�;dA��
A��7A�JA���A��Ay�mAv��AtZAp�+An9XAkx�AiO�Ag�Af��AcO�A`bNA^�yA^v�A]�AZ��AW��AU�^ATJARn�AQG�APQ�AMK�AJ �AH=qAE�
AD  AA��A@A�A>{A<M�A;�A;�A:ZA8��A7�A6ȴA69XA5x�A3�A2^5A0-A0  A.��A/�hA1&�A1?}A17LA0n�A/�A-�-A,��A*��A(��A&��A%�A&��A'7LA'G�A'��A'�A'VA&I�A$ĜA"��A"ĜA"JA �HA �A bNA�AI�A�A��AS�A��A�jA��AbA��A
=AI�A��AVA��A��A+A�
A��A�9A�hA��A"�AbA�uA=qA  A��A�7A
��AȴA�Ax�A�!A�DAn�A�A��Ar�A1'A�AA�;A�FA ��A �@�l�@�;d@���@�%@���@���@�E�@���@�V@���@��/@�1@�A�@�@��m@��m@�u@���@�u@�l�@�@���@���@�R@�`B@�V@�G�@��#@�@���@���@��@��#@�Ĝ@�Q�@�F@��@��@�@��@�9@�F@�"�@��@�v�@��T@�^@��@�?}@��@��@��@�j@�j@��;@��@�@���@�1@�b@��D@�S�@�x�@�;d@���@ۥ�@۾w@�1'@�Z@۝�@�
=@ڇ+@�5?@ٺ^@ٙ�@�G�@�V@���@؛�@�|�@և+@�J@��#@ԛ�@�\)@��@�ȴ@җ�@�M�@�@�O�@�&�@��@��@θR@���@́@�7L@̣�@��m@���@�ȴ@ʧ�@�5?@ɲ-@�V@ȓu@��m@ǶF@�t�@�;d@�V@��@Ł@���@Ĭ@�9X@Ý�@Å@�t�@�
=@\@�E�@��@�x�@�V@�9X@��m@�+@���@���@��+@�M�@��#@�X@��`@�bN@��;@�33@�ff@��h@��@��@��/@��j@��@�j@�Q�@�9X@�b@��@��@�l�@��H@�$�@�hs@�O�@��/@� �@�  @���@��@�S�@�+@��@�@���@��h@�/@�Ĝ@��j@�I�@�(�@��@��P@�K�@��@��+@�=q@��#@���@��h@�X@��`@��D@�Q�@�(�@�  @��P@�\)@�K�@�+@�
=@���@��!@���@���@�/@��9@�I�@��@��w@��@�t�@�K�@��@�^5@�E�@��@��@��@���@���@��9@���@��D@�bN@�(�@��;@���@�|�@�S�@��@�
=@��y@��+@�-@�@�p�@�&�@�Ĝ@�r�@�1'@��
@�|�@�l�@�dZ@�C�@���@���@�$�@�X@��@���@�Ĝ@��j@��9@���@��@�1@���@�"�@��@���@���@�`B@�?}@�%@���@���@��m@�K�@���@���@�n�@�E�@�{@�@�@�G�G�O�@��-@��@�t�@��+@|z�@p�`@g|�@_�w@W\)@P1'@HA�@@ �@6�@.5?@(�@#@��@��@{@x�@O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	bB	\B	\B	\B	\B	\B	\B	bB	\B	\B	\B	hB	oB	\B	\B	\B	bB	bB	\B	bB	uB	uB	VB	VB	uB	�B	�B	"�B	,B	5?B	=qB	?}B	M�B	jB	� B	�B	�+B	�JB	��B	��B	�B
bNB
�-B
��B
ĜB
ĜB
ƨB
��B
�B
�NB
�mB
�B
�B
�B
�B
��B-BgmBt�B�B��B��B��B��B�;B1BuB/B<jBI�BF�B9XB7LBE�BM�BR�BS�BF�BA�BF�BH�BG�B9XB�B�B�wB��B�BhsBF�B�B
�B
ĜB
��B
v�B
`BB
VB
R�B
M�B
=qB
�B	��B	�`B	��B	�B	��B	�%B	{�B	l�B	cTB	W
B	O�B	C�B	:^B	5?B	2-B	.B	%�B	�B	�B	�B	{B	\B	JB	B��B��B�B�mB�ZB�HB�yB��B��B	  B��B�B��B	1B	\B	�B	\B	B	+B	�B	�B	,B	cTB	�=B	��B	��B	�{B	�7B	�B	x�B	m�B	bNB	ffB	�B	�bB	��B	�}B	��B	��B	��B	�}B	�-B	�'B	�B	��B	�B	��B	��B	��B	��B	��B	�hB	�1B	�B	z�B	x�B	�B	�1B	�%B	�B	|�B	y�B	s�B	r�B	o�B	k�B	l�B	�B	��B	�oB	�VB	�7B	�=B	�PB	�PB	�=B	�B	r�B	aHB	[#B	[#B	\)B	\)B	`BB	\)B	]/B	]/B	^5B	bNB	e`B	gmB	bNB	^5B	`BB	aHB	bNB	hsB	m�B	k�B	jB	gmB	p�B	u�B	v�B	z�B	~�B	x�B	u�B	~�B	�+B	�JB	�DB	�+B	�%B	�7B	�\B	�uB	�{B	��B	��B	��B	��B	�B	�3B	�XB	�jB	�wB	�}B	��B	ÖB	ÖB	ÖB	ÖB	ÖB	ĜB	ŢB	ŢB	ƨB	ǮB	ɺB	��B	��B	��B	��B	��B	�B	�B	�B	�
B	��B	��B	��B	��B	�
B	��B	ȴB	�wB	�}B	��B	��B	��B	�
B	�B	�B	�#B	�#B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�#B	�#B	�#B	�#B	�#B	�#B	�#B	�B	�B	�#B	�/B	�/B	�5B	�/B	�5B	�NB	�NB	�HB	�HB	�ZB	�yB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
  B	��B
  B
  B
  B
  B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
B
%B
%B
+B
+B
1B
+B
1B
1B
1B
1B
1B
+B
+B
%B
B
%B
%B
%B
+B
+B
+B
1B
1B
	7B
	7B
	7B

=B

=B

=B

=B
	7B
	7B

=B

=B
DB
DB
DB
DB
JB
JB
JB
JB
PB
PB
VB
\B
bB
bB
bB
hB
bB
bB
bB
bB
oB
{B
{B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
"�B
�B
 �B
&�B
.B
6FB
<jB
@�B
G�B
I�B
M�B
R�B
YB
^5B
bNB
gmB
k�B
o�B
s�B
w�B
z�B
� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B	GB	EB	EB	CB	EB	EB	EB	IB	GB	EB	EB	QB	WB	EB	EB	GB	JB	IB	DB	LB	_B	^B	;B	>B	^B	{B	�B	"�B	+�B	5(B	=[B	?fB	M�B	jgB	�B	�B	�B	�3B	��B	�lB	�B
b4B
�B
�sB
ĆB
ĆB
ƐB
ͼB
�B
�5B
�VB
�zB
�eB
�nB
��B
��B,�BgUBt�B�B��B�iBͽB��B�$BB[B/B<QBI�BF�B9>B74BE�BM�BR�BS�BF�BAnBF�BH�BG�B9;B�B�~B�ZB�gB��BhWBF�BlB
�|B
�B
�kB
v�B
`(B
U�B
R�B
M�B
=SB
�B	��B	�AB	�mB	��B	�|B	�B	{�B	lnB	c7B	V�B	O�B	CyB	:@B	5 B	2B	-�B	%�B	�B	hB	iB	[B	=B	,B	 �B��B��B�kB�NB�9B�'B�XB��B��B��B��B�B��B	B	:B	`B	:B	 �B	B	nB	_B	+�B	c3B	�B	�`B	�xB	�YB	�B	��B	x�B	moB	b0B	fDB	��B	�@B	��B	�[B	ˤB	ͲB	ʟB	�[B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�DB	�B	��B	z�B	x�B	��B	�B	�B	��B	|�B	y�B	s�B	r�B	o}B	kdB	lhB	��B	�eB	�MB	�5B	�B	�B	�.B	�,B	�B	��B	r�B	a&B	Z�B	Z�B	\B	\B	`!B	\B	]
B	]B	^B	b*B	e=B	gJB	b,B	^B	`B	a&B	b-B	hOB	mmB	kcB	j\B	gHB	p�B	u�B	v�B	z�B	~�B	x�B	u�B	~�B	�B	�%B	� B	�B	�B	�B	�8B	�QB	�WB	�cB	�~B	��B	��B	��B	�B	�4B	�EB	�SB	�YB	�eB	�sB	�uB	�rB	�pB	�oB	�wB	�~B	�~B	ƃB	ǉB	ɔB	ˤB	ͱB	εB	��B	��B	��B	��B	��B	��B	��B	εB	ͮB	��B	��B	пB	ȑB	�TB	�ZB	˥B	εB	��B	��B	��B	��B	� B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�	B	�B	�B	�B	�)B	�,B	�"B	�&B	�7B	�RB	�UB	�ZB	�ZB	�`B	�aB	�`B	�aB	�fB	�hB	�dB	�mB	�fB	�gB	�hB	�kB	�eB	�fB	�eB	�iB	�gB	�lB	�yB	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
 B
�B
B
B
B
B
B
B
B
	B
B
B
B
�B
�B
 B
�B
 B
B
B
B
B
	B
	B
	B
	B

B

B

B

B
	B
	B

B

B
B
B
B
B
%B
&B
&B
!B
)B
,B
0B
7B
=B
;B
>B
AB
=B
;B
;B
=B
IB
WB
SB
\B
bB
cB
`B
aB
_B
aB
gB
gB
hB
mB
lB
nB
lB
lB
mB
lG�O�B
{B
 �B
&�B
-�B
6B
<CB
@^B
G�B
I�B
M�B
R�B
X�B
^B
b*B
gGB
k_B
oxB
s�B
w�B
z�B
�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.29 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201904051007592019040510075920190405100759  AO  ARCAADJP                                                                    20181121125954    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20181121125954  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20181121125954  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190405100759  IP                  G�O�G�O�G�O�                