CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-10-16T07:01:06Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.1    Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         (Matthew Alkire, University of Washington      @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20171016070106  20190604095308  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055                            2C  D   APEX                            5374                            041511                          846 @�+T	1   @�+T��Q@9�~��"��c3I�^1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�ff@�  A   AffA@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BPffBXffB_��Bg��Bp  Bx  B�  B�33B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C�fC   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(y�D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt�fDy�fD�
D�AHD��
D���D�3D�J�D��qD�њD��D�MqD��\Dǫ�D�3D�W�Dڃ�D���D��D�HRD�q�D�l�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@�G�A��A#
=AD��Ad��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�B(�B	(�B(�B(�B!(�B)(�B1(�B9(�BA(�BI(�BQ�\BY�\B`BhBq(�By(�B��{B�ǮB�aHB��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{B��{BĔ{BȔ{B̔{BД{BԔ{Bؔ{Bܔ{B��{B�{B�{B�{B�{B��{B��{B��{C J=CJ=CJ=CJ=CJ=C
J=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=CJ=C0�C J=C"J=C$J=C&J=C(J=C*J=C,J=C.J=C0J=C2J=C4J=C6J=C8J=C:J=C<J=C>J=C@J=CBJ=CDJ=CFJ=CHJ=CJJ=CLJ=CNJ=CPJ=CRJ=CTJ=CVJ=CXJ=CZJ=C\J=C^J=C`J=CbJ=CdJ=CfJ=ChJ=CjJ=ClJ=CnJ=CpJ=CrJ=CtJ=CvJ=CxJ=CzJ=C|J=C~J=C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�1�C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%C�%D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(�)D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt��Dy��D� RD�J�D��RD���D�{D�S�D���D���D�D�V�D���DǵD�{D�`�DڍD��D�#�D�Q�D�z�D�v111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��Aڰ!A�ƨA���A���A���A��;A��;A��;A��A��
A��#A��#A��A��
A��
A��yA��`A��A�?}Aۗ�A�v�A�bNA܏\A܁A�5?A���A�Q�AٶFA׏\AՇ+AӓuA��
A�bNA�-Aƛ�A���A��
A��A�x�A�$�A��A��/A�-A���A��hA��+A�?}A�bNA�^5A�5?A�z�A�1A���A��;A�~�A��+A�33A��A��!A��A���A��A���A�A�A��+A�7LA��FA�K�A�jA��mA�hsA���A���A���A�VA���A� �A��jA��!A��FA���A�XA�  A���A���A��+A�ffA���A��#A�/A�l�A��9A��A�9XA���A��A��^A�v�A�5?A�^5A�S�A���A���A�1'A��\A��A}��A|{A{��Az1'Ax �Av~�AuG�ArAp��AoS�AnI�Al�!Aj��Ai
=Ag��Ag7LAf�uAeAe
=AdQ�Ab�HAaA`VA_�A^n�A\z�A[�;A[�AZffAXffAW�#AWS�AVv�AU�^AU�ATv�ATbAS�PARAQ"�AP��AO�TAN�!AMl�ALĜAL5?AK�AKC�AI�PAHv�AG��AG/AE�TADM�ACx�AB�DAA�wA@ĜA?�A=A;�A;t�A:�+A8�`A7p�A65?A5\)A4^5A2��A1�mA1�A0�DA0-A/K�A-hsA+��A*��A*�A*A�A)�wA(��A'��A%O�A%?}A%t�A$�A#�A#`BA"�\A"1A!��A!��A!�hA ��A v�A+Az�A�AdZA�uAE�A�mA�^A�7AC�A�;A^5A��A�A
=AbNA�A`BA�`A��AjA�HA�A
��A	��AĜAjAJA�;A��A33A�A�A�#AXA��AAC�A �RA -@��;@��y@���@�I�@��@���@�r�@�33@�G�@�bN@�+@�@�@���@��@웦@��H@陚@�?}@���@�  @��@�5?@�x�@�ȴ@��@�`B@�dZ@݁@ܼj@���@��@�-@�&�@ם�@�$�@��@ѩ�@���@�r�@�  @�o@�x�@̣�@�Z@��@�-@��/@�  @�33@�v�@ř�@�Q�@�|�@��@�V@�Ĝ@�Z@�ƨ@�S�@���@��7@��m@��y@�M�@��@�(�@��P@�V@���@�?}@�l�@���@�$�@�ff@��+@�@�/@�z�@�1@��@�K�@��!@���@�%@�A�@�ƨ@�C�@��R@�J@��9@�  @�
=@��!@�M�@��@��@�o@���@�E�@��T@�Ĝ@�bN@�A�@��@�1'@���@��H@���@���@�M�@���@�7L@��`@��D@��;@��P@�|�@���@���@���@�~�@��T@��@��@���@�bN@�  @���@�
=@���@��+@�E�@��-@�/@�Ĝ@��@���@���@�n�@�{@�p�@���@�9X@��@�9X@�1@��P@�C�@��H@���@�n�@��T@��-@��h@���@��D@�1'@��@�|�@�C�@���@�5?@�^5@�^5@��@�O�@�V@��@�&�@�V@��`@��j@�I�@��;@��F@���@�\)@��+@�M�@��@��^@�x�@��@���@�Z@�b@��;@�ƨ@��F@��@���@�S�@��y@���@�n�@��@���@�Ĝ@�1'@��@�P@\)@~�R@~E�@}�T@}p�@{��@{C�@z��@z=q@y��@y��@y��@yhs@y�@x��@xbN@x �@xb@x  @w��@w|�@wl�@w;d@v�@v��@vv�@vE�@u�@u��@up�@u?}@t��@s��@s�@sdZ@sC�@r�H@r�\@r~�@r~�@r~�@rn�@r^5@rn�@rn�@rM�@p9X@kC@c�V@^J�@TɆ@Nd�@F.�@?W?@9��@4<�@/��@+y�@&@�@ e�@e@=q@�H@m]@	�H@��@W?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  Aڰ!A�ƨA���A���A���A��;A��;A��;A��A��
A��#A��#A��A��
A��
A��yA��`A��A�?}Aۗ�A�v�A�bNA܏\A܁A�5?A���A�Q�AٶFA׏\AՇ+AӓuA��
A�bNA�-Aƛ�A���A��
A��A�x�A�$�A��A��/A�-A���A��hA��+A�?}A�bNA�^5A�5?A�z�A�1A���A��;A�~�A��+A�33A��A��!A��A���A��A���A�A�A��+A�7LA��FA�K�A�jA��mA�hsA���A���A���A�VA���A� �A��jA��!A��FA���A�XA�  A���A���A��+A�ffA���A��#A�/A�l�A��9A��A�9XA���A��A��^A�v�A�5?A�^5A�S�A���A���A�1'A��\A��A}��A|{A{��Az1'Ax �Av~�AuG�ArAp��AoS�AnI�Al�!Aj��Ai
=Ag��Ag7LAf�uAeAe
=AdQ�Ab�HAaA`VA_�A^n�A\z�A[�;A[�AZffAXffAW�#AWS�AVv�AU�^AU�ATv�ATbAS�PARAQ"�AP��AO�TAN�!AMl�ALĜAL5?AK�AKC�AI�PAHv�AG��AG/AE�TADM�ACx�AB�DAA�wA@ĜA?�A=A;�A;t�A:�+A8�`A7p�A65?A5\)A4^5A2��A1�mA1�A0�DA0-A/K�A-hsA+��A*��A*�A*A�A)�wA(��A'��A%O�A%?}A%t�A$�A#�A#`BA"�\A"1A!��A!��A!�hA ��A v�A+Az�A�AdZA�uAE�A�mA�^A�7AC�A�;A^5A��A�A
=AbNA�A`BA�`A��AjA�HA�A
��A	��AĜAjAJA�;A��A33A�A�A�#AXA��AAC�A �RA -@��;@��y@���@�I�@��@���@�r�@�33@�G�@�bN@�+@�@�@���@��@웦@��H@陚@�?}@���@�  @��@�5?@�x�@�ȴ@��@�`B@�dZ@݁@ܼj@���@��@�-@�&�@ם�@�$�@��@ѩ�@���@�r�@�  @�o@�x�@̣�@�Z@��@�-@��/@�  @�33@�v�@ř�@�Q�@�|�@��@�V@�Ĝ@�Z@�ƨ@�S�@���@��7@��m@��y@�M�@��@�(�@��P@�V@���@�?}@�l�@���@�$�@�ff@��+@�@�/@�z�@�1@��@�K�@��!@���@�%@�A�@�ƨ@�C�@��R@�J@��9@�  @�
=@��!@�M�@��@��@�o@���@�E�@��T@�Ĝ@�bN@�A�@��@�1'@���@��H@���@���@�M�@���@�7L@��`@��D@��;@��P@�|�@���@���@���@�~�@��T@��@��@���@�bN@�  @���@�
=@���@��+@�E�@��-@�/@�Ĝ@��@���@���@�n�@�{@�p�@���@�9X@��@�9X@�1@��P@�C�@��H@���@�n�@��T@��-@��h@���@��D@�1'@��@�|�@�C�@���@�5?@�^5@�^5@��@�O�@�V@��@�&�@�V@��`@��j@�I�@��;@��F@���@�\)@��+@�M�@��@��^@�x�@��@���@�Z@�b@��;@�ƨ@��F@��@���@�S�@��y@���@�n�@��@���@�Ĝ@�1'@��@�P@\)@~�R@~E�@}�T@}p�@{��@{C�@z��@z=q@y��@y��@y��@yhs@y�@x��@xbN@x �@xb@x  @w��@w|�@wl�@w;d@v�@v��@vv�@vE�@u�@u��@up�@u?}@t��@s��@s�@sdZ@sC�@r�H@r�\@r~�@r~�@r~�@rn�@r^5@rn�@rn�G�O�@p9X@kC@c�V@^J�@TɆ@Nd�@F.�@?W?@9��@4<�@/��@+y�@&@�@ e�@e@=q@�H@m]@	�H@��@W?111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB�RB�LB�LB�LB�LB�RB�RB�LB�FB�LB�LB�FB�LB�LB�LB�jB�^B�}B�sB�B�B��B�
B�B�B��B��B��B��BȴBÖB�qB��B��B��B�B�HB�B��BɺBƨBȴBȴB�jB�!B��B��B�1By�Bl�BdZBZBJ�BH�BM�BH�B<jB1'B(�B"�B�BoB	7B��B�B��B��B�;B��BɺBB�qB�^B�B��B��B��B��B��B�bB�B~�Bx�Bq�Bx�Bx�Bv�Bp�BhsB^5BR�BF�B<jB0!B&�BuBB
�B
�B
�TB
�B
��B
ɺB
�^B
�B
��B
��B
�1B
�B
v�B
hsB
_;B
T�B
=qB
2-B
'�B
�B
{B
	7B	��B	��B	�B	�B	�fB	�NB	�/B	��B	��B	��B	�dB	�3B	��B	��B	��B	�oB	�%B	�B	~�B	z�B	u�B	s�B	o�B	l�B	hsB	_;B	ZB	W
B	Q�B	I�B	A�B	>wB	;dB	9XB	49B	.B	)�B	%�B	"�B	�B	�B	bB	DB	B��B�yB�#B��B��BƨB�LB�B��B��B��B�VB�DB�7B�7B�+B�By�Bv�Bv�Bu�Bs�Br�Bo�BgmBZBhsBz�Bt�Bo�BjBcTBbNBffBiyBt�Br�Bp�Bo�Bp�Bq�Bt�Bx�By�By�Bx�Bw�Bt�Bo�BiyBcTBYBP�BM�BK�BH�BE�B@�B;dB7LB33B1'B0!B/B.B.B-B,B+B(�B&�B&�B%�B#�B"�B �B�B�B�B�B�B�B�B�B�B�B�B�B�B�B{B�B�B�B{B�B�B{BuBoBoBbBhBhBbBbBbBbBbBbBbBbBhBhBhB{B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B�B!�B#�B#�B$�B$�B$�B%�B&�B,B0!B0!B2-B1'B0!B/B/B-B(�B'�B'�B,B0!B1'B33B49B5?B5?B5?B49B49B5?B7LB7LB8RB:^B;dB>wB?}BD�BF�BH�BK�BM�BN�BO�BP�BP�BQ�BS�BT�B[#BcTBgmBk�Bl�Bl�Bm�Bp�Br�Bt�Bu�Bx�B|�B|�B� B�B�B�B�B�7B�=B�JB�VB�hB�{B��B��B��B��B��B��B��B�B�3B�9B�9B�FB�RB�^B�qB��BƨBȴB��B��B��B��B��B��B�B�#B�5B�BB�HB�TB�ZB�fB�fB�yB�B�B�B��B��B��B��B	B	B	B	1B	DB	DB	DB	JB	\B	hB	oB	oB	{B	�B	�B	�B	 �B	!�B	"�B	"�B	"�B	#�B	$�B	&�B	(�B	)�B	,B	/B	6FB	;dB	?}B	?}B	?}B	B�B	C�B	D�B	E�B	I�B	L�B	N�B	Q�B	S�B	T�B	VB	W
B	XB	[#B	\)B	]/B	^5B	_;B	`BB	bNB	bNB	cTB	e`B	gmB	gmB	iyB	jB	l�B	m�B	o�B	q�B	s�B	t�B	t�B	t�B	u�B	w�B	y�B	|�B	}�B	� B	�%B	�PB	�VB	�hB	�\B	�%B	ބB	�kB
 OB
hB
#�B
-CB
5ZB
?HB
E�B
MjB
S�B
W�B
_�B
d�B
kB
p�B
u�B
y�B
}�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B�<B�6B�4B�7B�6B�:B�7B�4B�0B�6B�6B�0B�7B�6B�4B�TB�EB�fB�ZB�B�B�jB��B��B�B��BͼB��B��BțB�}B�YB�jB��BͻB�B�,B��B��BɢBƐBȜBȜB�QB�B��B�B�By�BlqBdABZBJ�BH�BM�BH�B<RB1
B(�B"�B�BSB	B��B�B��B��B� B��BɞB�tB�VB�BB��B��B��B�yB��B��B�CB�B~�Bx�Bq�Bx�Bx�Bv�Bp�BhWB^BR�BF�B<MB0B&�BWB�B
�B
�eB
�6B
��B
��B
ɝB
�BB
��B
��B
�jB
�B
��B
v�B
hWB
_B
T�B
=QB
2B
'�B
�B
\B
	B	��B	��B	�B	�lB	�KB	�1B	�B	��B	ʢB	�lB	�FB	�B	��B	��B	��B	�QB	�B	��B	~�B	z�B	u�B	s�B	oB	lkB	hTB	_B	Y�B	V�B	Q�B	I�B	AjB	>YB	;EB	9:B	4B	-�B	)�B	%�B	"�B	�B	bB	BB	$B	�B��B�YB�BϿBͳBƇB�-B��B��B��B�`B�5B�$B�B�B�	B��By�Bv�Bv�Bu�Bs�Br�Bo~BgJBY�BhRBz�Bt�Bo|Bj_Bc3Bb-BfDBiXBt�Br�Bp�Bo{Bp�Bq�Bt�Bx�By�By�Bx�Bw�Bt�Bo|BiXBc2BX�BP�BM�BK�BH�BEB@cB;@B7(B3B1B/�B.�B-�B-�B,�B+�B*�B(�B&�B&�B%�B#�B"�B �B�B�B�B~B|BuBoBvBoBpBjBfB^B_BVBnBbB]BXB\B]BVBSBMBJB=BCBDB@B=B<B=B=B;B<B@BEBCBCBUB\BdBcBcBnBuBmBhBoB�B�B�B�B�B�B�B!�B#�B#�B$�B$�B$�B%�B&�B+�B/�B/�B2B1B/�B.�B.�B,�B(�B'�B'�B+�B/�B1B3B4B5B5B5B4B4B5B7&B7(B8-B:9B;@B>QB?VBDvBF�BH�BK�BM�BN�BO�BP�BP�BQ�BS�BT�BZ�Bc.BgHBk`BldBldBmkBp}Br�Bt�Bu�Bx�B|�B|�B�B��B��B��B��B�B�B�%B�0B�@B�WB�kB�yB��B��B��B��B��B��B�B�B�B�B�.B�7B�KB�eBƂBȎBʛB̧BβBнB��B��B��B��B�B�B�#B�.B�2B�?B�@B�SB�dB�yB�B��B��B��B��B	�B	�B	�B	B	B	 B	B	%B	5B	@B	IB	HB	RB	lB	�B	�B	 �B	!�B	"�B	"�B	"�B	#�B	$�B	&�B	(�B	)�B	+�B	.�B	6B	;=B	?UB	?WB	?WB	BiB	CoB	DuB	ExB	I�B	L�B	N�B	Q�B	S�B	T�B	U�B	V�B	W�B	Z�B	\B	]B	^B	_B	`B	b&B	b*B	c+B	e8B	gEB	gFB	iQB	jXB	leB	mmB	owB	q�B	s�B	t�B	t�B	t�B	u�B	w�B	y�B	|�B	}�B	�B	��B	�*B	�-G�O�B	�7B	��B	�^B	�EB
 )B
CB
#�B
-B
52B
?$B
E�B
MFB
S�B
W�B
_�B
d�B
j�B
p�B
u�B
y�B
}}111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.29 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201906040953082019060409530820190604095308  AO  ARCAADJP                                                                    20171016070106    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171016070106  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171016070106  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604095308  IP                  G�O�G�O�G�O�                