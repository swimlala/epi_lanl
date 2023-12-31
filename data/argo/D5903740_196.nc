CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-08-25T07:01:11Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20170825070111  20190604095307  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055                            2C  D   APEX                            5374                            041511                          846 @� ���X1   @� ��㫓@:m�hr�!�cU�E��1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @�  @���A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  BffB��B��B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C�C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF�CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dt� Dt��Dy� D��
D�R=D�u�D�l{D�{D�J�D��{D�ƸD�qD�>�D�Q�D��3D��D�>fD�uqD��D��D�9�D�C�D���111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @�Q�@��A(�A$(�AD(�Ad(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B	
=Bp�B�
B��B)
=B1
=B9
=BA
=BI
=BQ
=BY
=Ba
=Bi
=Bq
=By
=B��B��B��B��B��B��B��B��B��B��B��B��B��RB��B��B��B��BąBȅB̅BЅBԅB؅B܅B��B�B�B�B��B�B��B��C B�CB�CB�CB�CB�C
B�CB�CB�CB�C\)CB�CB�CB�CB�CB�CB�C B�C"B�C$B�C&B�C(B�C*B�C,B�C.B�C0B�C2B�C4B�C6B�C8B�C:B�C<B�C>B�C@B�CBB�CDB�CF\)CHB�CJB�CLB�CNB�CPB�CRB�CTB�CVB�CXB�CZB�C\B�C^B�C`B�CbB�CdB�CfB�ChB�CjB�ClB�CnB�CpB�CrB�CtB�CvB�CxB�CzB�C|B�C~B�C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�.C�.C�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HC�!HD �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D�>D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,�D,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt��Dt�qDy��D��\D�Z�D�~D�t�D��D�S3D���D��
D��D�G
D�Z>D��D�)D�F�D�}�D�D��qD�A�D�K�D�� 111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A׏\A׋DA׃A�x�A�|�A�`BA�ZA�ZA�VA�S�A�O�A�K�A�I�A�I�A�I�A�9XA�-A� �AֶFA���A�ZAɸRA��A�M�AƑhA�$�Aò-A�A�A��FA�%A�E�A�33A�x�A�bNA�v�A�ĜA��yA��A�E�A�5?A���A�/A���A��hA���A��RA���A��A��uA�5?A�n�A���A��yA��!A���A��PA�jA�VA���A��A�t�A�A�z�A���A�9XA�A�  A�JA�{A�ffA�1'A��A�~�A�{A�A��/A�A�A�S�A�ƨA���A��A�|�A�{A��\A�;dA�E�A��A�9XA�Q�A��A��TA���A��A��wA���A�p�A~(�A{\)AyK�Axv�Aw/AtjAq�;Ao��AoK�As+Ar��ArM�Ap�!An^5Ak+Aj{Ad�yAc�mAd��Aet�Ag�AgAf��AfA�Ad�`Ad�Act�AbVAa
=A^�A]"�A[�
AZA�AY
=AX  AV��AU/ASC�AR1ARJAQ33AN�9AL�AJ�yAJ�RAI�FAI�hAIp�AHĜAG��AF��AF5?AEƨAE7LADbNAC�#AChsAB�ABz�AB�AA��AA\)A@�A@ZA?��A?�A>��A>Q�A=�A=33A<-A;�A;%A:�uA9��A9dZA9
=A8ȴA8v�A7oA6�jA61'A5�PA4�9A4�A3�FA37LA2��A25?A2A1A17LA0VA/�A/hsA.r�A-��A,��A,1A+XA*��A)dZA)/A(�/A(�A(�A'A%A%��A%\)A$�+A#�#A"�uA!��A ��A (�A"�AbNA�FAȴA$�A7LA~�A��A�/A��AZA��AK�A��A{A�wA|�A��A�uA�#Ap�AVAA�AK�A1'A�A��A�A"�A
 �A	�A	p�A�`A�A�TA�!AA�A�AdZA�DA�A
=A v�A I�A 9X@��;@�M�@�bN@�@�@��u@���@�7L@�Q�@�dZ@��@��^@��@���@�@��@�Ĝ@��@�@�t�@�=q@���@� �@ߕ�@��@�J@ݡ�@�%@��`@ܴ9@�;d@�hs@��`@�b@�5?@��@�K�@�ff@Л�@�E�@�J@ͺ^@�/@˥�@�E�@Ɂ@ǝ�@Ə\@ļj@�$�@�Ĝ@��m@���@�p�@���@�z�@�Q�@���@�r�@�b@��u@��9@�I�@�j@�bN@��@�~�@�/@�(�@�@�=q@��7@�(�@��F@�dZ@��R@�-@��-@�x�@�V@��@�z�@�Z@�(�@��
@���@��\@���@��^@�p�@��@��D@�9X@��@�o@��!@�v�@��@�O�@�r�@�ƨ@�|�@�ȴ@�M�@���@�&�@�(�@�b@��
@�
=@�ȴ@�V@���@��T@�p�@�X@��@�1'@��;@���@�33@��+@�p�@��u@��@��@�K�@���@�$�@��7@��9@�Z@��m@���@���@�|�@��@��R@��!@���@���@� �@��P@���@���@�-@�hs@�G�@�V@��j@�z�@�A�@��F@�t�@�C�@��@�M�@��@�J@���@��T@��-@�`B@�7L@���@���@�b@�ƨ@�|�@�S�@�ȴ@�=q@�hs@��@���@���@�Z@�1@��m@���@��w@���@���@���@�\)@��y@��+@�=q@��@��T@��h@�O�@��`@���@��9@��@\)@~��@~ȴ@~E�@}�T@}p�@}/@|��@|��@|��@|��@|Z@{�F@z�@z~�@z�@yx�@y%@x�9@x�@xQ�@xA�@x  @w�@wK�@v�@vE�@u�@uO�@t�@t��@t9X@s�F@st�@sdZ@r{�@i��@a�>@W��@Q�@K��@E��@>�@9��@4��@.�1@)0�@%`B@ S�@�@@PH@2�@*�@�A@��@s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A׏\A׋DA׃A�x�A�|�A�`BA�ZA�ZA�VA�S�A�O�A�K�A�I�A�I�A�I�A�9XA�-A� �AֶFA���A�ZAɸRA��A�M�AƑhA�$�Aò-A�A�A��FA�%A�E�A�33A�x�A�bNA�v�A�ĜA��yA��A�E�A�5?A���A�/A���A��hA���A��RA���A��A��uA�5?A�n�A���A��yA��!A���A��PA�jA�VA���A��A�t�A�A�z�A���A�9XA�A�  A�JA�{A�ffA�1'A��A�~�A�{A�A��/A�A�A�S�A�ƨA���A��A�|�A�{A��\A�;dA�E�A��A�9XA�Q�A��A��TA���A��A��wA���A�p�A~(�A{\)AyK�Axv�Aw/AtjAq�;Ao��AoK�As+Ar��ArM�Ap�!An^5Ak+Aj{Ad�yAc�mAd��Aet�Ag�AgAf��AfA�Ad�`Ad�Act�AbVAa
=A^�A]"�A[�
AZA�AY
=AX  AV��AU/ASC�AR1ARJAQ33AN�9AL�AJ�yAJ�RAI�FAI�hAIp�AHĜAG��AF��AF5?AEƨAE7LADbNAC�#AChsAB�ABz�AB�AA��AA\)A@�A@ZA?��A?�A>��A>Q�A=�A=33A<-A;�A;%A:�uA9��A9dZA9
=A8ȴA8v�A7oA6�jA61'A5�PA4�9A4�A3�FA37LA2��A25?A2A1A17LA0VA/�A/hsA.r�A-��A,��A,1A+XA*��A)dZA)/A(�/A(�A(�A'A%A%��A%\)A$�+A#�#A"�uA!��A ��A (�A"�AbNA�FAȴA$�A7LA~�A��A�/A��AZA��AK�A��A{A�wA|�A��A�uA�#Ap�AVAA�AK�A1'A�A��A�A"�A
 �A	�A	p�A�`A�A�TA�!AA�A�AdZA�DA�A
=A v�A I�A 9X@��;@�M�@�bN@�@�@��u@���@�7L@�Q�@�dZ@��@��^@��@���@�@��@�Ĝ@��@�@�t�@�=q@���@� �@ߕ�@��@�J@ݡ�@�%@��`@ܴ9@�;d@�hs@��`@�b@�5?@��@�K�@�ff@Л�@�E�@�J@ͺ^@�/@˥�@�E�@Ɂ@ǝ�@Ə\@ļj@�$�@�Ĝ@��m@���@�p�@���@�z�@�Q�@���@�r�@�b@��u@��9@�I�@�j@�bN@��@�~�@�/@�(�@�@�=q@��7@�(�@��F@�dZ@��R@�-@��-@�x�@�V@��@�z�@�Z@�(�@��
@���@��\@���@��^@�p�@��@��D@�9X@��@�o@��!@�v�@��@�O�@�r�@�ƨ@�|�@�ȴ@�M�@���@�&�@�(�@�b@��
@�
=@�ȴ@�V@���@��T@�p�@�X@��@�1'@��;@���@�33@��+@�p�@��u@��@��@�K�@���@�$�@��7@��9@�Z@��m@���@���@�|�@��@��R@��!@���@���@� �@��P@���@���@�-@�hs@�G�@�V@��j@�z�@�A�@��F@�t�@�C�@��@�M�@��@�J@���@��T@��-@�`B@�7L@���@���@�b@�ƨ@�|�@�S�@�ȴ@�=q@�hs@��@���@���@�Z@�1@��m@���@��w@���@���@���@�\)@��y@��+@�=q@��@��T@��h@�O�@��`@���@��9@��@\)@~��@~ȴ@~E�@}�T@}p�@}/@|��@|��@|��@|��@|Z@{�F@z�@z~�@z�@yx�@y%@x�9@x�@xQ�@xA�@x  @w�@wK�@v�@vE�@u�@uO�@t�@t��@t9X@s�F@st�G�O�@r{�@i��@a�>@W��@Q�@K��@E��@>�@9��@4��@.�1@)0�@%`B@ S�@�@@PH@2�@*�@�A@��@s�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��B  B  B  B  BBBBBBBBBBBBBB��B��B�}BȴB��B��B�fB��BBVB�B�B!�B(�B2-B-B5?B5?B;dBO�B�oB��B�hB��B�bB�oB��B��B�B��B��B��B��B�+B{�Bo�BXBK�BE�BA�BG�BB�BF�BJ�BS�BS�BL�BG�B�BBoB�B6FB@�B+B��B�;B��B��B�bB� Bs�B`BBE�BA�BZB8RBbB
=B�B
=B
�
B
�bB
p�B
F�B
&�B
hB

=B
�B	��B	�TB	��B	�NB	�TB	�jB	��B	�\B	��B	��B
uB
uB	��B	�B	�RB	��B	gmB	e`B	�B	��B	��B	�5B	�)B	�/B	�B	��B	��B	B	�FB	��B	�=B	|�B	n�B	dZB	\)B	W
B	O�B	E�B	=qB	G�B	D�B	1'B	�B	�B	�B	�B	�B	 �B	 �B	�B	�B	{B	uB	bB	JB	
=B	1B	%B	B	B	B	B	B	  B��B��B��B��B��B�B�B�B�sB�`B�NB�BB�5B�)B�B��B��B��B��BɺBƨBŢBB��B�}B�qB�jB�^B�LB�?B�-B�B�B��B��B��B��B��B��B��B��B��B�uB�oB�oB�oB�bB�PB�7B�+B�%B�B�B� B{�Bw�Bu�Br�Bp�Bm�BjBe`B_;B]/B[#BZBXBW
BVBT�BR�BQ�BO�BM�BK�BI�BF�BE�BC�BA�B>wB=qB;dB9XB7LB33B/B-B,B+B(�B'�B&�B$�B#�B"�B"�B!�B �B�B�B�B�B�B�B{BoBVBPBVBVBVBbB\BVBPB
=B+BBBBBBDBPBPBPB\BbBbB\B\B\BbBbBhBoBhBbBVBJBVBhB�B�B�B�B�B�B�B�B�B�B�B�B�B�B$�B+B.B/B/B0!B1'B49B7LB:^B;dB<jB?}B@�B@�BD�BG�BI�BK�BK�BL�BL�BL�BM�BN�BR�BS�BW
BXBXBZB\)B\)B_;BaHBbNBbNBcTBffBk�Bk�Bk�Bk�Bp�Bu�Bu�By�B|�B}�B�B�B�%B�1B�1B�=B�=B�DB�\B�hB�uB��B��B��B��B��B��B��B�B�B�B�'B�9B�RB�dB�dB�wB��BǮBǮBƨBŢBŢBƨBǮBȴB��B��B��B��B�B�
B�B�5B�HB�TB�fB�B�B�B�B�B�B�B��B��B��B��B��B	B	B	B	B	B	%B	1B	
=B	JB	\B	bB	hB	hB	oB	uB	uB	�B	�B	 �B	#�B	%�B	'�B	.B	33B	7LB	:^B	=qB	D�B	F�B	H�B	I�B	L�B	N�B	P�B	Q�B	R�B	T�B	VB	VB	W
B	ZB	]/B	_;B	_;B	`BB	aHB	aHB	cTB	cTB	dZB	dZB	ffB	hsB	iyB	jB	jB	jB	k�B	jB	k�B	l�B	n�B	jB	�SB	��B	��B	�8B	��B
NB
!B
-CB
:^B
@B
E�B
I�B
N�B
T�B
[qB
`�B
g8B
k�B
q[B
u�B
zD111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B��B��B��B��B��B �B �B�B�B�B�B�B�B�B�B�B�B�B��B��B�dBȟBʫB˱B�QB��B�BAB�B�B!�B(�B2B,�B5(B5(B;MBO�B�XB�pB�QB��B�IB�\B��B��B��B��B��B��B�jB�B{�Bo�BW�BK�BE�BAsBG�BBzBF�BJ�BS�BS�BL�BG�B�BBXB�B6.B@mB*�B��B�$B�oB��B�KB�Bs�B`*BE�BAsBZB8:BGB
$B�B
"B
��B
�FB
p�B
F�B
&�B
MB

"B
yB	��B	�;B	��B	�5B	�9B	�PB	��B	�?B	��B	��B
ZB
ZB	��B	�B	�6B	��B	gRB	eFB	��B	��B	͹B	�B	�B	�B	��B	��B	̰B	�uB	�+B	��B	�"B	|�B	n|B	d=B	\B	V�B	O�B	E�B	=TB	G�B	D�B	1
B	�B	oB	�B	�B	�B	 �B	 �B	�B	kB	`B	VB	EB	/B	
!B	B		B	�B	�B	 �B	 �B	 �B��B��B��B��B��B��B�B�{B�hB�VB�EB�/B�$B�B�B��B��B��BλB̱BɝBƊBŃB�tB�gB�_B�TB�MB�@B�2B�#B�B��B��B��B��B��B��B��B��B��B�|B�qB�WB�OB�QB�QB�CB�2B�B�B�B��B��B�B{�Bw�Bu�Br�Bp�BmsBj_BeAB_B]B[BZ BW�BV�BU�BT�BR�BQ�BO�BM�BK�BI�BF�BE�BCxBAlB>WB=SB;EB98B7,B3B.�B,�B+�B*�B(�B'�B&�B$�B#�B"�B"�B!�B �B�B�B�ByBfBbB\BQB7B2B5B5B7BBB;B6B/B
BB�B�B�B�B�B#B.B1B/B<BBBAB=B=B<BABDBEBMBEBAB5B(B4BEBcB`BgBxBzBxBxB�B�B�B�B�B�B�B$�B*�B-�B.�B.�B0B1B4B7/B:<B;BB<GB?\B@aB@aBD|BG�BI�BK�BK�BL�BL�BL�BM�BN�BR�BS�BV�BW�BW�BY�B\B\	B_Ba$Bb.Bb+Bc3BfDBkaBkeBkeBkcBp�Bu�Bu�By�B|�B}�B��B��B�B�B�B�B�B�B�9B�FB�TB�_B�mB�{B��B��B��B��B��B��B��B�B�B�0B�AB�AB�TB�fBǊBǌBƈBŀBŁBƆBǎBȒB˦B��B��B��B��B��B��B�B�%B�4B�FB�^B�dB�kB�kB�oB�}B�B��B��B��B��B��B	 �B	 �B	�B	�B	�B	B	B	
B	(B	;B	@B	EB	IB	LB	TB	RB	kB	�B	 �B	#�B	%�B	'�B	-�B	3B	7,B	:;B	=OB	DyB	F�B	H�B	I�B	L�B	N�B	P�B	Q�B	R�B	T�B	U�B	U�B	V�B	Y�B	]B	_B	_B	` B	a(B	a&B	c1B	c3B	d8B	d9B	fBB	hRB	iUB	jZB	jZB	j\B	kaB	j]B	kbB	liB	nwG�O�B	�/B	��B	ˤB	�B	��B
+B
 �B
-B
:<B
?�B
EdB
I�B
N�B
T�B
[NB
`�B
gB
k�B
q7B
u�B
z!111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.26 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201906040953072019060409530720190604095307  AO  ARCAADJP                                                                    20170825070111    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20170825070111  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20170825070111  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604095307  IP                  G�O�G�O�G�O�                