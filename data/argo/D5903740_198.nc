CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2017-10-05T17:06:45Z creation      
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
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20171005170645  20190604095308  5903740 US ARGO PROJECT                                                 STEPHEN RISER                                                   PRES            TEMP            PSAL               �A   AO  4055                            2C  D   APEX                            5374                            041511                          846 @�&�h�1   @�&�7l�@9b�\(���cM?|�h1   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    �A   B   B   @   @�  @�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A���A�  B   B  B  B  B ffB(  B/��B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C7�fC9�fC<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��3C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,fD,� D-  D-� D.  D.� D/  D/� D0  D0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� DW  DW� DX  DX� DY  DY� DZ  DZ� D[  D[� D\  D\� D]  D]� D^  D^� D_  D_� D`  D`� Da  Da� Db  Db� Dc  Dc� Dd  Dd� De  De� Df  Df� Dg  Dg� Dh  Dh� Di  Di� Dj  Dj� Dk  Dk� Dl  Dl� Dm  Dm� Dn  Dn� Do  Do� Dp  Dp� Dq  Dq� Dr  Dr� Ds  Ds� Dt  Dts3Dy��D�\D�L{D��qD��D��
D�L{D��HD�ӅD��qD�G�D��\D��=D��RD�/�Dښ=D��
D�{D�T{D�D�Ƹ111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @:�H@�p�@�p�A�RA&�RAF�RAf�RA�\)A�\)A�\)A�\)A�\)A�\)A�(�A�\)B�B	�B�B�B"zB)�B1G�B9�BA�BI�BQ�BY�Ba�Bi�Bq�By�B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
B��
C k�Ck�Ck�Ck�Ck�C
k�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�Ck�C k�C"k�C$k�C&k�C(k�C*k�C,k�C.k�C0k�C2k�C4k�C6k�C8Q�C:Q�C<k�C>k�C@k�CBk�CDk�CFk�CHk�CJk�CLk�CNk�CPk�CRk�CTk�CVk�CXk�CZk�C\k�C^k�C`k�Cbk�Cdk�Cfk�Chk�Cjk�Clk�Cnk�Cpk�Crk�Ctk�Cvk�Cxk�Czk�C|k�C~k�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�(�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�C�5�D �D ��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D	�D	��D
�D
��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D�D��D �D ��D!�D!��D"�D"��D#�D#��D$�D$��D%�D%��D&�D&��D'�D'��D(�D(��D)�D)��D*�D*��D+�D+��D,!GD,��D-�D-��D.�D.��D/�D/��D0�D0��D1�D1��D2�D2��D3�D3��D4�D4��D5�D5��D6�D6��D7�D7��D8�D8��D9�D9��D:�D:��D;�D;��D<�D<��D=�D=��D>�D>��D?�D?��D@�D@��DA�DA��DB�DB��DC�DC��DD�DD��DE�DE��DF�DF��DG�DG��DH�DH��DI�DI��DJ�DJ��DK�DK��DL�DL��DM�DM��DN�DN��DO�DO��DP�DP��DQ�DQ��DR�DR��DS�DS��DT�DT��DU�DU��DV�DV��DW�DW��DX�DX��DY�DY��DZ�DZ��D[�D[��D\�D\��D]�D]��D^�D^��D_�D_��D`�D`��Da�Da��Db�Db��Dc�Dc��Dd�Dd��De�De��Df�Df��Dg�Dg��Dh�Dh��Di�Di��Dj�Dj��Dk�Dk��Dl�Dl��Dm�Dm��Dn�Dn��Do�Do��Dp�Dp��Dq�Dq��Dr�Dr��Ds�Ds��Dt�Dt�Dy��D��D�Y�D���D��D�{D�Y�D���D���D�
�D�UD���D�߮D��D�=Dڧ�D��{D�!�D�a�D�D��)111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A� �Aى7Aٛ�Aٙ�Aٗ�Aٗ�Aُ\AٍPAمA�l�A�G�A�1A���AخA؏\A�G�A��Aם�A��A�jA��AƧ�A��A���A���A�-A�z�A��A�dZA��A�A�7LA�ȴA���A�v�A�Q�A�1'A��A�M�A��RA�t�A�E�A��jA�\)A�hsA�n�A�v�A��jA�|�A�dZA�1A�?}A�ĜA�ȴA��-A���A���A�33A�VA��A��9A�ȴA��#A���A�{A�?}A�(�A�bNA�bNA�+A�E�A�$�A�`BA�`BA�9XA�S�A���A�7LA�$�A��jA��A��
A�/A�%A�
=A�dZA�$�A��A~��A}K�Az~�Ax�Av�yAt�AtffAtQ�As��AsK�Ar�HArE�AqXAn�9Amt�Al�Ak��Ai��Ae?}Acl�Ab��Ab�uAa�A`=qA_�hA^v�A]
=A\jA[�^AZ�uAY�AWt�AV�yAUASAR�+AQ��AQ�AP1ANI�AM|�AL �AKt�AJ��AIhsAH1AF�/AEC�AD�ADz�AC��ABn�AB5?AB{ABAA�TA@��A@VA?�7A>��A=�mA<�HA<r�A;��A;
=A:ȴA:��A:jA:A9�TA9A8�A7�A7\)A7A6�RA6ZA5��A4�9A4(�A3l�A3�A2�A1�PA1`BA1
=A0n�A/��A/�7A/33A.��A.E�A-p�A,�DA+�A*��A*JA);dA(n�A'��A';dA& �A%�hA%&�A$�9A$1A#O�A"��A"�jA"M�A!��A!C�A ��A�mA�PA��A�#AdZA�+A��A��A�PA
=Az�A�A"�A��A��AS�A  AO�AI�A�A�FA�jAM�A�wAl�A�uA�Ap�A
�A
�+A
{A	&�A�\AXA �A��AXAA��AZA�AXA&�AQ�AK�A ��@�-@�x�@�ƨ@�ȴ@�ƨ@�|�@���@��
@�`B@�1@�{@�O�@�  @�@�O�@���@���@�&�@�(�@ى7@��@ش9@؃@�Q�@�t�@�j@���@ёh@Л�@�b@��@�V@�A�@˅@�S�@�K�@�C�@�K�@�K�@�t�@�bN@���@ͺ^@��/@��
@�;d@�E�@�z�@�$�@þw@��
@��@�S�@���@�&�@�Q�@��@��j@�S�@�
=@�~�@�\)@�p�@���@�j@���@�M�@��@��@�E�@�@�x�@�7L@��`@�(�@�ƨ@�
=@��@�G�@���@�9X@�dZ@��\@�{@�x�@�&�@���@���@� �@��@�\)@��@�-@���@��@�z�@���@��m@��@�l�@�;d@��@���@�ff@�J@��#@���@�%@�bN@���@�ƨ@��@�ȴ@�hs@��@�I�@��
@�ƨ@��@��@�{@���@�O�@�V@�z�@�I�@�9X@�1@��;@��w@�l�@�;d@�+@�
=@�ȴ@�ȴ@�ff@�J@��#@�@���@�/@��@��@�A�@�  @���@��@�\)@���@�n�@�@���@���@��@��`@�I�@��
@�33@��H@���@��\@�V@��#@���@��@�O�@��`@���@�Q�@�1'@�1'@�(�@��@��@��F@��@�;d@��@�v�@���@��@��-@��@�/@���@��j@���@�9X@��@�  @��@��@�P@��@�P@K�@
=@+@
=@~ȴ@}�@|��@|�D@|j@|9X@{�m@{t�@{33@{@z��@z=q@y�^@y�7@yx�@yX@y7L@xA�@w��@v��@v��@vV@v@v@u�@u`B@tI�@s��@s��@r�@qX@p�u@pr�@pQ�@o�P@nff@n$�@n@m@m`B@l��@k��@j	@`$@\�P@U��@M4@H�@B!�@<�$@7��@1�-@.��@(��@#˒@�@��@+k@�#@�@	+�@�@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  A� �Aى7Aٛ�Aٙ�Aٗ�Aٗ�Aُ\AٍPAمA�l�A�G�A�1A���AخA؏\A�G�A��Aם�A��A�jA��AƧ�A��A���A���A�-A�z�A��A�dZA��A�A�7LA�ȴA���A�v�A�Q�A�1'A��A�M�A��RA�t�A�E�A��jA�\)A�hsA�n�A�v�A��jA�|�A�dZA�1A�?}A�ĜA�ȴA��-A���A���A�33A�VA��A��9A�ȴA��#A���A�{A�?}A�(�A�bNA�bNA�+A�E�A�$�A�`BA�`BA�9XA�S�A���A�7LA�$�A��jA��A��
A�/A�%A�
=A�dZA�$�A��A~��A}K�Az~�Ax�Av�yAt�AtffAtQ�As��AsK�Ar�HArE�AqXAn�9Amt�Al�Ak��Ai��Ae?}Acl�Ab��Ab�uAa�A`=qA_�hA^v�A]
=A\jA[�^AZ�uAY�AWt�AV�yAUASAR�+AQ��AQ�AP1ANI�AM|�AL �AKt�AJ��AIhsAH1AF�/AEC�AD�ADz�AC��ABn�AB5?AB{ABAA�TA@��A@VA?�7A>��A=�mA<�HA<r�A;��A;
=A:ȴA:��A:jA:A9�TA9A8�A7�A7\)A7A6�RA6ZA5��A4�9A4(�A3l�A3�A2�A1�PA1`BA1
=A0n�A/��A/�7A/33A.��A.E�A-p�A,�DA+�A*��A*JA);dA(n�A'��A';dA& �A%�hA%&�A$�9A$1A#O�A"��A"�jA"M�A!��A!C�A ��A�mA�PA��A�#AdZA�+A��A��A�PA
=Az�A�A"�A��A��AS�A  AO�AI�A�A�FA�jAM�A�wAl�A�uA�Ap�A
�A
�+A
{A	&�A�\AXA �A��AXAA��AZA�AXA&�AQ�AK�A ��@�-@�x�@�ƨ@�ȴ@�ƨ@�|�@���@��
@�`B@�1@�{@�O�@�  @�@�O�@���@���@�&�@�(�@ى7@��@ش9@؃@�Q�@�t�@�j@���@ёh@Л�@�b@��@�V@�A�@˅@�S�@�K�@�C�@�K�@�K�@�t�@�bN@���@ͺ^@��/@��
@�;d@�E�@�z�@�$�@þw@��
@��@�S�@���@�&�@�Q�@��@��j@�S�@�
=@�~�@�\)@�p�@���@�j@���@�M�@��@��@�E�@�@�x�@�7L@��`@�(�@�ƨ@�
=@��@�G�@���@�9X@�dZ@��\@�{@�x�@�&�@���@���@� �@��@�\)@��@�-@���@��@�z�@���@��m@��@�l�@�;d@��@���@�ff@�J@��#@���@�%@�bN@���@�ƨ@��@�ȴ@�hs@��@�I�@��
@�ƨ@��@��@�{@���@�O�@�V@�z�@�I�@�9X@�1@��;@��w@�l�@�;d@�+@�
=@�ȴ@�ȴ@�ff@�J@��#@�@���@�/@��@��@�A�@�  @���@��@�\)@���@�n�@�@���@���@��@��`@�I�@��
@�33@��H@���@��\@�V@��#@���@��@�O�@��`@���@�Q�@�1'@�1'@�(�@��@��@��F@��@�;d@��@�v�@���@��@��-@��@�/@���@��j@���@�9X@��@�  @��@��@�P@��@�P@K�@
=@+@
=@~ȴ@}�@|��@|�D@|j@|9X@{�m@{t�@{33@{@z��@z=q@y�^@y�7@yx�@yX@y7L@xA�@w��@v��@v��@vV@v@v@u�@u`B@tI�@s��@s��@r�@qX@p�u@pr�@pQ�@o�P@nff@n$�@n@m@m`B@l��G�O�@j	@`$@\�P@U��@M4@H�@B!�@<�$@7��@1�-@.��@(��@#˒@�@��@+k@�#@�@	+�@�@111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oG�O�;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB��BB�B�B�B�B�B�B�B�BuBJB
=B	7B	7B1B+B	7B	7BB��B�TB�/B�
B�
B��B��B��B��B��B�B�
B��B�FB��B�Br�Bz�By�B~�B|�Bs�B^5BP�BF�B=qB6FB-B�B�B{B
=B��B�B�TB�HB�BB�#B��BÖB�9B��B��B��B�7B}�Bz�Bx�BhsBJ�B7LB �BuB33B�B\BPB
=BB
��B
�yB
�B
��B
ƨB
�XB
�!B
�B
��B
�\B
}�B
`BB
K�B
=qB
1'B
-B
-B
+B
&�B
$�B
 �B
�B
1B	��B	��B	�B	�B	�3B	��B	�B	�LB	�B	��B	�B	��B	��B	��B	��B	�bB	�1B	� B	z�B	p�B	cTB	YB	R�B	J�B	B�B	9XB	5?B	-B	+B	)�B	!�B	�B	hB	DB		7B	1B	+B	+B	+B	%B	B	B	  B��B��B��B�B�B�B�mB�ZB�NB�NB�HB�BB�HB�HB�B��B��B��B��B��B��B��BǮBÖB��B�jB�^B�XB�LB�?B�3B�-B�'B�!B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�uB�uB�oB�bB�VB�JB�DB�=B�1B�%B�B}�B{�Bv�Bo�Bl�BiyBhsBffBcTB`BB\)BVBL�BF�BB�BA�B@�B>wB<jB<jB=qB<jB:^B<jBA�BD�BD�BD�BE�BC�BB�BD�BD�BD�BD�BC�BD�BE�BF�BF�BF�BF�BE�BE�BC�B@�B<jB2-B�B�BuBoBhBbB\BVBVBVBPBVBPBJBVBVBVBPBJBDB
=B
=BDBDBDBDBPBPB\BhBhBuB{B�B#�B2-BB�BL�BO�BQ�BQ�BO�BN�BK�BE�B9XB2-B0!B/B.B.B33B6FB;dB:^B7LB8RB:^B<jB=qB?}BA�BH�BN�B]/B_;B`BBaHBbNBdZBe`BgmBjBl�Bn�Bo�Br�Bu�Bv�Bx�By�Bz�B{�B}�B� B� B�B�%B�1B�DB�\B�uB�uB�{B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�!B�'B�-B�-B�-B�3B�FB�RB�^B�dB�wB�}B�}B��BÖBŢB��B��B��B��B��B��B��B��B��B��B��B�B�
B�B�B�#B�/B�/B�5B�NB�TB�fB�sB�sB�sB�B�B��B��B��B	B	B	+B	JB	VB	\B	bB	{B	�B	�B	�B	�B	�B	�B	�B	�B	�B	!�B	"�B	'�B	,B	,B	.B	0!B	1'B	49B	49B	5?B	7LB	8RB	9XB	;dB	<jB	=qB	=qB	=qB	>wB	@�B	B�B	E�B	G�B	I�B	N�B	P�B	P�B	Q�B	R�B	VB	W
B	YB	\)B	_;B	cTB	dZB	dZB	e`B	e`B	iyB	l�B	n�B	s�B	u�B	w�B	w�B	x�B	{�B	|�B	~�B	� B	�B	�B	�B	�B	�%B	�7B	�PB	�PB	�PB	�VB	�\B	�bB	�uB	�`B	�\B	��B	��B
&B
B
'�B
2|B
7�B
@B
E�B
N�B
T�B
X�B
^�B
e�B
kB
o�B
u�B
{�B
}111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  B��B�B�B�B�B�B�B�B�BsBRB%B
B	B	BB	B	B	B�B��B�/B�B��B��B��B��B��B��B��B��B��BˣB� B��B��Br�Bz�By�B~�B|�Bs�B^BP�BF~B=KB6B,�B�BxBUB
B��B�rB�.B�B�B��BΰB�mB�B��B��B�WB�B}�Bz�Bx�BhJBJ�B7#B �BJB3	BzB1B%B
B �B
��B
�NB
��B
ʖB
�~B
�/B
��B
��B
��B
�0B
}�B
`B
K�B
=FB
0�B
,�B
,�B
*�B
&�B
$�B
 �B
tB
B	��B	��B	�ZB	��B	�B	��B	��B	�B	��B	��B	��B	��B	��B	�~B	�aB	�7B	�B	�B	z�B	pwB	c'B	X�B	R�B	J�B	BbB	9+B	5B	,�B	*�B	)�B	!�B	dB	9B	B		
B	B	�B	�B	�B	�B	�B	�B��B��B��B�B�{B�cB�VB�AB�,B�B�B�B�B�B�B��B��BжBϯBϱB��BϯBʒBǀB�fB�ZB�:B�/B�*B�B�B�B��B��B��B��B��B��B��B��B��B��B��B��B�zB�kB�^B�ZB�NB�EB�DB�@B�4B�(B�B�B�B� B��B��B}�B{�Bv�BonBlYBiHBhCBf5Bc"B`B[�BU�BL�BFwBB]BAYB@SB>FB<8B<7B=?B<9B:,B<7BAVBDlBDkBDmBEoBCfBB\BDlBDkBDkBDgBCfBDjBEpBFvBFvBFuBFsBEpBElBCbB@PB<5B1�ByBNB?B9B3B.B&B"BB!BB!BBB!B!B"BBBB
B
BBBBBBB*B4B2B?BFBXB#�B1�BBXBL�BO�BQ�BQ�BO�BN�BK�BElB9$B1�B/�B.�B-�B-�B2�B6B;.B:'B7B8B:)B<3B=;B?GBARBH~BN�B\�B_B`BaBbBd!Be)Bg8BjHBlRBnbBogBrxBu�Bv�Bx�By�Bz�B{�B}�B�B�B��B��B��B�B�'B�>B�>B�BB�OB�PB�TB�\B�gB�wB�zB�~B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�(B�,B�=B�FB�FB�RB�^B�jBʊB̕B̕B̔BΟBΠBѵBҺB��B��B��B��B��B��B��B��B��B��B��B�B�B�.B�:B�;B�<B�\B�yB��B��B��B	�B	�B	�B	B	!B	"B	+B	DB	UB	bB	nB	oB	mB	uB	�B	�B	�B	!�B	"�B	'�B	+�B	+�B	-�B	/�B	0�B	4 B	3�B	5B	7B	8B	9B	;-B	<3B	=;B	=9B	=9B	>@B	@IB	BWB	EkB	GvB	I�B	N�B	P�B	P�B	Q�B	R�B	U�B	V�B	X�B	[�B	_B	cB	d$B	d#B	e)B	e)B	i@B	lSB	naB	s|B	u�B	w�B	w�B	x�B	{�B	|�B	~�B	�B	��B	��B	��B	��B	��B	� B	�B	�B	�B	�B	�"B	�+G�O�B	�'B	�$B	�B	��B
�B
�B
'�B
2DB
7zB
?�B
EQB
NnB
T\B
X�B
^�B
e�B
j�B
o�B
u�B
{�B
I111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = sw_salt( sw_cndr(PSAL,TEMP,PRES), TEMP, PRES_ADJUSTED )                                                                                                                                                                                         dP =-0.42 dbar.                                                                                                                                                                                                                                                 none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   No significant salinity drift detected. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                                                                                                             201906040953082019060409530820190604095308  AO  ARCAADJP                                                                    20171005170645    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20171005170645  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20171005170645  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20190604095308  IP                  G�O�G�O�G�O�                