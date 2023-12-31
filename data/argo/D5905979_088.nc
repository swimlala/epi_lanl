CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-03-18T22:01:16Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile      comment_dmqc_operator         ZPRIMARY | https://orcid.org/0000-0001-7324-3159 | Matthew Alkire, University of Washington        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7(   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7,   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    70   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7@   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7P   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    7`   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  @  7h   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  @  7�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  0  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8    DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                     8$   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8D   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8H   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                     8L   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                     8l   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                     8�   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    8�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           8�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
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
resolution        =���   axis      Z          9�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  @�   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       B�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  I�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���       K�   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       R�   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Y�   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       [L   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  bP   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       d   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       k   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  r   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       s�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  z�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       |�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  0  ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                    ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                    ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                    ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  ,  ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                    �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                    �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                    �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                    �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  @  �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                    �X   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                    �h   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �l   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar         �|   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar         ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�        ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210318220116  20220204114419  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               XA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @��4�.1   @��4�.@7U?|�h�c��j~��8   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    XA   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B)��B/33B7��B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B���B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�33B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C%�fC(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>�C@�CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D��D� D  D� D��Dy�D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'� D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.  D.� D/  D/�fD0fD0� D1  D1� D2  D2� D3  D3� D4  D4� D5  D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DR  DR� DS  DS� DT  DT� DU  DU� DV  DV� Dy�HD�&�D�a�D���D��3D�&�D�Z=D���D��D��D�S3D���D��D�'\D�[3Dژ D���D�qD�W
D�D��q11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @�
>@�=q@�=qA�A=�A]�A}�A��\A��\A��\A��\AΏ\Aޏ\A�\A��\BG�BG�BG�BG�B(�HB.z�B6�HB?G�BGG�BOG�BWG�B_G�BgG�BoG�BwG�BG�B���B���B���B���B�p�B���B���B���B���B���B���B���B���B���B���B���Bã�Bǣ�Bˣ�Bϣ�Bӣ�B��
Bۣ�Bߣ�B��B��B��B��B��B���B���B���C��C��C��C��C	��C��C��C��C��C��C��C��C��C��C��C��C!��C#��C%�RC'��C)��C+��C-��C/��C1��C3��C5��C7��C9��C;��C=�C?�CA��CC��CE��CG��CI��CK��CM��CO��CQ��CS��CU��CW��CY��C[��C]��C_��Ca��Cc��Ce��Cg��Ci��Ck��Cm��Co��Cq��Cs��Cu��Cw��Cy��C{��C}��C��C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���C���D t{D �{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D	t{D	�{D
t{D
�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�Dt{D�{Dt{D�DnD�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{Dt{D�{D t{D �{D!t{D!�{D"t{D"�{D#t{D#�{D$t{D$�{D%t{D%�{D&t{D&�{D't{D'�{D(t{D(�{D)t{D)�{D*t{D*�{D+t{D+�{D,t{D,�{D-t{D-�{D.t{D.�{D/z�D/��D0t{D0�{D1t{D1�{D2t{D2�{D3t{D3�{D4t{D4�{D5t{D5�{D6t{D6�{D7t{D7�{D8t{D8�{D9t{D9�{D:t{D:�{D;t{D;�{D<t{D<�{D=t{D=�{D>t{D>�{D?t{D?�{D@t{D@�{DAt{DA�{DBt{DB�{DCt{DC�{DDt{DD�{DEt{DE�{DFt{DF�{DGt{DG�{DHt{DH�{DIt{DI�{DJt{DJ�{DKt{DK�{DLt{DL�{DMt{DM�{DNt{DN�{DOt{DO�{DPt{DP�{DQt{DQ�{DRt{DR�{DSt{DS�{DTt{DT�{DUt{DU�{DVt{Dy��D� �D�\)D���D��pD� �D�TzD���D��QD��D�MpD��)D���D�!�D�UpDڒ=D��D��D�QGD��D���11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A�{A��A� �A�"�A�"�A� �A�$�A�$�A�$�A�&�A�$�A�(�A�+A�/A�+A�(�A�$�A��Aհ!A���A���A�\)A��TA�n�A�dZA��;A��`A�I�A��A�%A�I�Aħ�AāA�t�A�n�A�9XA�~�A�^5A���A���A�jA�O�A�=qA�  A�^5A�5?A��A�JA�A�A�A� �A��-A��A�x�A��-A� �A��HA�A�dZA�VA�\)A�l�A�ĜA�A��PA�`BA�1A��RA�\)A��A�Q�A�+A�A�A�
=A��jA��jA��TA�jA��A���A���A�?}A���A���A���A�ZA��A��A���A�ĜA�\)A�z�A���A���A�A�M�A�ĜA�A��\A��mA��PA��yA���A��hA��A�33A���A�
=A�^5A�JA�r�A�\)A��A�A�A�Q�A�ȴA�{A�C�A��A|�+Az��Ay�Aw�TAvbArz�AoS�Am�Ak�^Aj^5Ai�^AgK�AdVAb��A^ĜA\9XA[t�A[�AY�AXn�AQ��AL��AJjAI��AHv�AF��AF$�AEK�ACAA��A@ffA>��A=�A<VA:�A8�+A8�A7�
A7�^A7`BA7�A7�A6 �A4~�A2�\A1t�A0bNA/�A-��A+�TA*��A)�wA)K�A)%A(I�A'�A%�;A%`BA$��A#�TA"v�A"1A!�FA!S�A Q�AE�AJA�A��AdZA��AbAS�A��A��AK�A~�AVA�AXA��A �AK�AVA��A�!A?}A�DA{A�HA��A�PA&�AjA
�A	t�A��AJA�-A��A�PA��A��A�-A ��@�"�@��/@��@�Q�@���@��+@��T@�R@�X@�b@�dZ@�^5@���@�r�@�33@���@�bN@�{@�1'@�R@�$�@��@��@�X@��`@��u@��@���@��@�33@�E�@ى7@�z�@ׅ@�@��/@Ӆ@Ұ!@���@��@�z�@�@�^5@��@�  @�
=@�o@�@ʰ!@��#@�p�@�r�@�l�@�S�@ȃ@�{@�b@�dZ@�;d@�K�@�"�@�@�M�@ř�@ŉ7@ŉ7@Ł@Ł@őh@�r�@�S�@+@���@��7@�7L@��@�ƨ@��R@�$�@��`@���@��j@�b@��w@��@�l�@�@��@��F@�~�@�M�@�n�@�C�@�ff@�1'@���@���@�r�@�(�@��m@�;d@��@�{@�@��@���@��@�S�@���@�V@��-@��@��9@��u@��@���@��j@��@�j@�z�@�%@��@���@��`@�bN@��H@�ff@�$�@��-@�hs@�p�@�p�@�?}@�7L@�?}@�p�@��@�-@��T@�J@���@�v�@��+@�~�@�n�@���@���@��@��D@�Q�@�(�@�1'@�I�@�1'@���@�;d@�33@�"�@��H@���@�=q@�@��^@�hs@�/@�%@��/@���@��D@�b@�ƨ@��@���@���@��@��F@���@�dZ@�
=@���@�v�@�E�@�@��T@�?}@���@�b@��
@���@�dZ@�;d@��@���@���@�~�@�-@���@�p�@��h@��^@�O�@�z�@���@�}V@}f�@w�}@p(�@f�@]��@U�7@L(�@G1�@@~(@=Y�@5;@0Ft@+b�@%Y�@"?@�@�@\�@
�@�t11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A�{A��A� �A�"�A�"�A� �A�$�A�$�A�$�A�&�A�$�A�(�A�+A�/A�+A�(�A�$�A��Aհ!A���A���A�\)A��TA�n�A�dZA��;A��`A�I�A��A�%A�I�Aħ�AāA�t�A�n�A�9XA�~�A�^5A���A���A�jA�O�A�=qA�  A�^5A�5?A��A�JA�A�A�A� �A��-A��A�x�A��-A� �A��HA�A�dZA�VA�\)A�l�A�ĜA�A��PA�`BA�1A��RA�\)A��A�Q�A�+A�A�A�
=A��jA��jA��TA�jA��A���A���A�?}A���A���A���A�ZA��A��A���A�ĜA�\)A�z�A���A���A�A�M�A�ĜA�A��\A��mA��PA��yA���A��hA��A�33A���A�
=A�^5A�JA�r�A�\)A��A�A�A�Q�A�ȴA�{A�C�A��A|�+Az��Ay�Aw�TAvbArz�AoS�Am�Ak�^Aj^5Ai�^AgK�AdVAb��A^ĜA\9XA[t�A[�AY�AXn�AQ��AL��AJjAI��AHv�AF��AF$�AEK�ACAA��A@ffA>��A=�A<VA:�A8�+A8�A7�
A7�^A7`BA7�A7�A6 �A4~�A2�\A1t�A0bNA/�A-��A+�TA*��A)�wA)K�A)%A(I�A'�A%�;A%`BA$��A#�TA"v�A"1A!�FA!S�A Q�AE�AJA�A��AdZA��AbAS�A��A��AK�A~�AVA�AXA��A �AK�AVA��A�!A?}A�DA{A�HA��A�PA&�AjA
�A	t�A��AJA�-A��A�PA��A��A�-A ��@�"�@��/@��@�Q�@���@��+@��T@�R@�X@�b@�dZ@�^5@���@�r�@�33@���@�bN@�{@�1'@�R@�$�@��@��@�X@��`@��u@��@���@��@�33@�E�@ى7@�z�@ׅ@�@��/@Ӆ@Ұ!@���@��@�z�@�@�^5@��@�  @�
=@�o@�@ʰ!@��#@�p�@�r�@�l�@�S�@ȃ@�{@�b@�dZ@�;d@�K�@�"�@�@�M�@ř�@ŉ7@ŉ7@Ł@Ł@őh@�r�@�S�@+@���@��7@�7L@��@�ƨ@��R@�$�@��`@���@��j@�b@��w@��@�l�@�@��@��F@�~�@�M�@�n�@�C�@�ff@�1'@���@���@�r�@�(�@��m@�;d@��@�{@�@��@���@��@�S�@���@�V@��-@��@��9@��u@��@���@��j@��@�j@�z�@�%@��@���@��`@�bN@��H@�ff@�$�@��-@�hs@�p�@�p�@�?}@�7L@�?}@�p�@��@�-@��T@�J@���@�v�@��+@�~�@�n�@���@���@��@��D@�Q�@�(�@�1'@�I�@�1'@���@�;d@�33@�"�@��H@���@�=q@�@��^@�hs@�/@�%@��/@���@��D@�b@�ƨ@��@���@���@��@��F@���@�dZ@�
=@���@�v�@�E�@�@��T@�?}@���@�b@��
@���@�dZ@�;d@��@���@���@�~�@�-@���@�p�@��h@��^@�O�@�z�@���@�}V@}f�@w�}@p(�@f�@]��@U�7@L(�@G1�@@~(@=Y�@5;@0Ft@+b�@%Y�@"?@�@�@\�@
�@�t11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB	�wB	�wB	�wB	�wB	�wB	�wB	�wB	�wB	�wB	�wB	�wB	�wB	�wB	�}B	��B	��B	ŢB	ǮB	��B	��B	��B	��B	��B
\B
PB
\B
-B
L�B
O�B
Q�B
`BB
e`B
gmB
hsB
hsB
k�B
z�B
��B
ÖB
�ZB
��B
��B
��BB1B
��B{BJBhB#�BJ�Bn�Bz�B�B��B�B�'B�B��B��B�wB�B��B��B��B��B��B��B��B��B��B��B�qBBȴBB��B��B�3BbNBH�BVB+B��BBB�B�XB�3B�B"�B/B8RBC�BC�B:^B7LB'�B�BuB\B%B�B�BÖB��B� Bs�B[#B6FBJB
��B
�B
�HB
��B
�oB
�VB
�B
v�B
bNB
A�B
(�B
DB	��B	�#B	�9B	��B	�VB	� B	r�B	iyB	\)B	=qB	:^B	#�B	{B	bB	\B	�B	49B	%B�TB�
B��B��BȴBĜB��B�}B�XB�RB�!B�B��B��B��B��B��B��B��B��B��B��B�bB�DB�+B�B�B|�Bw�Bw�Bw�Bt�Bt�Br�Bt�Bu�Bt�Bt�Bs�Bq�Bm�Bl�Bk�BjBdZBdZBgmBgmBe`BdZBcTBbNB`BB_;B^5B[#BZBYBXBVBS�BR�BQ�BN�BM�BK�BH�BG�BG�BD�BD�BC�BB�BB�B>wB>wB<jB;dB;dB:^B8RB7LB5?B49B2-B1'B1'B0!B1'B/B/B0!B/B0!B0!B1'B1'B1'B2-B0!B33B33B33B49B33B33B33B33B33B33B33B33B6FB7LB9XB9XB:^B<jB?}B?}BA�BA�BB�BB�BB�BE�BD�BD�BG�BE�BE�BD�BF�BJ�BM�BR�BT�BVB`BBs�Bm�Bm�Bp�Bu�Bz�B�B�=B�PB�hB�oB�uB��B��B��B��B��B��B��B��B�B�B�B�B�B�!B�!B�B�!B�!B�-B�9B�3B�9B�LB�RB�jBŢBƨBĜB��B��B��B��BBŢB��B��B��B��B�B�B�)B�5B�;B�TB�yB�B�B�B�B��B��B��B��B	%B		7B	VB	{B	�B	�B	�B	�B	�B	�B	 �B	!�B	#�B	$�B	%�B	(�B	0!B	5?B	6FB	7LB	=qB	B�B	F�B	I�B	J�B	K�B	K�B	L�B	O�B	S�B	YB	ZB	[#B	aHB	ffB	hsB	iyB	iyB	k�B	m�B	p�B	q�B	s�B	s�B	t�B	v�B	v�B	w�B	w�B	x�B	{�B	{�B	{�B	|�B	~�B	�B	�B	�B	�B	�+B	�+B	�+B	�1B	�VB	�\B	�VB	�hB	�{B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B
DB
�B
"�B
#:B
+�B
6B
=�B
C�B
FB
E�B
T�B
Y�B
_!B
f�B
jKB
n�B
t�B
v�B
y�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
tB
hB
tB
%%B
D�B
G�B
JB
XWB
]tB
_�B
`�B
`�B
c�B
r�B
��B
��B
�hB
��B
��B
� B
�+B =B
��B�BVB	tB�BB�Bf�Br�B}%B��B�B�+B�B��B��B�zB�B��B��B� B��B��B��B��B��B��B��B�vB��B��B��B��B��B�:BZZB@�BNB�-B��B�B�!B�B�_B�;B�B�B'B0QB;�B;�B2^B/LB�B�BwB_B�(B�B�B��B��BxBk�BS3B.XB_B
��B
�B
�`B
��B
��B
�tB
{1B
n�B
ZoB
9�B
!B
kB	��B	�NB	�fB	�B	��B	x1B	j�B	a�B	T]B	5�B	2�B	B	�B	�B	�B	�B	,qB�`BےB�IB�+B�B��B��B��B��B��B��B�dB�WB�9B�B��B��B��B��B��B�B�B��B��B��BrB|`BzSBu6BpBpBpBmBmBj�BmBnBmBmBl Bi�Be�Bd�Bc�Bb�B\�B\�B_�B_�B]�B\�B[�BZ�BX�BW�BV�BSpBRjBQdBP]BNQBLEBK?BJ:BG'BF!BDBAB?�B?�B<�B<�B;�B:�B:�B6�B6�B4�B3�B3�B2�B0�B/�B-�B,�B*B)yB)zB(tB)zB'nB'nB(tB'nB(tB(tB)zB){B){B*�B(uB+�B+�B+�B,�B+�B+�B+�B+�B+�B+�B+�B+�B.�B/�B1�B1�B2�B4�B7�B7�B9�B9�B:�B:�B:�B=�B<�B<�B@B=�B=�B<�B>�BCBF'BKFBMRBNXBX�BlBe�Be�Bh�BnBs3B|jB��B��B��B��B��B��B��B�B�!B�!B�-B�-B�@B�QB�]B�XB�RB�^B�qB�qB�dB�qB�qB�}B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�;B�;B�AB�SB�fB�wBփB׉BۢB��B��B��B��B��B�	B�.B�(B�.B�qB	�B	�B	�B	�B	�B	�B	�B	�B	B	B	B	"B	(B	-B	!@B	(kB	-�B	.�B	/�B	5�B	:�B	>�B	BB	C	B	DB	DB	EB	H'B	L@B	Q_B	RdB	SjB	Y�B	^�B	`�B	a�B	a�B	c�B	e�B	h�B	i�B	k�B	k�B	mB	oB	oB	pB	pB	qB	t-B	t-B	t-B	u4B	w@B	zQB	zQB	|^B	}dB	pB	pB	pB	�vB	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�	B	�!B	�-B	�'B	�-B	��B	��B
�B
;B
B
yB
$B
.OB
5�B
;�B
>IB
>.B
M!B
RB
W^B
^�B
b�B
f�B
mB
oB
r11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.18 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.005) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144192022020411441920220204114419  AO  ARCAADJP                                                                    20210318220116    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210318220116  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210318220116  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114419  IP                  G�O�G�O�G�O�                