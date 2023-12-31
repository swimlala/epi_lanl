CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             
   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2021-03-18T22:01:15Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210318220115  20220204114417  5905979 UW, Argo                                                        STEPHEN RISER                                                   PRES            TEMP            PSAL               DA   AO  7662                            2C  D   APEX                            8312                            080318                          846 @س��ۦ1   @س��ۦ@6]/��w�d�Q�8   GPS     Primary sampling: mixed [deeper than nominal 985dbar: discrete; nominal 985dbar to surface: 2dbar-bin averaged]                                                                                                                                                    DA   A   A   @���@�  A   A   A@  A`  A�  A�  A�  A�  A�  A�  A�  A�  B   B  B  B  B   B(  B0  B8  B@  BH  BP  BX  B`  Bh  Bp  Bx  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  B�  C   C  C  C  C  C
  C  C  C  C  C  C  C  C  C  C  C   C"  C$  C&  C(  C*  C,  C.  C0  C2  C4  C6  C8  C:  C<  C>  C@  CB  CD  CF  CH  CJ  CL  CN  CP  CR  CT  CV  CX  CZ  C\  C^  C`  Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cx  Cz  C|  C~  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C��C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  D   D � D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D	  D	� D
  D
� D
��D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D  D� D   D � D!  D!� D"  D"� D#  D#� D$  D$� D%  D%� D&  D&� D'  D'y�D(  D(� D)  D)� D*  D*� D+  D+� D,  D,� D-  D-� D.fD.� D/  D/y�D0  D0� D1  D1� D2  D2� D3  D3� D4  D4y�D4��D5� D6  D6� D7  D7� D8  D8� D9  D9� D:  D:� D;  D;� D<  D<� D=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DB  DB� DC  DC� DD  DD� DE  DE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM  DM� DN  DN� DO  DO� DP  DP� DQ  DQ� DRfDR� DR��DS� DT  DT� DU  DU�fDV  DV� Dy�)D�+�D�X D�� D��fD�{D�g
D���D���D��D�Z=D���D�ÅD�$�D�X�D�ND��D��D�O
D�D�Ǯ11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@�Q�@�Q�A(�A<(�A\(�A|(�A�{A�{A�{A�{A�{A�{A�{A�{B
=B
=B
=B
=B'
=B/
=B7
=B?
=BG
=BO
=BW
=B_
=Bg
=Bo
=Bw
=B
=B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B��BÅBǅB˅BυBӅBׅBۅB߅B�B�B�B�B�B��B��B��CCCCC	CCCCCCCCCCCC!C#C%C'C)C+C-C/C1C3C5C7C9C;C=C?CACCCECGCICKCMCOCQCSCUCWCYC[C]C_CaCcCeCgCiCkCmCoCqCsCuCwCyC{C}CC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��C��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HC��HD p�D �Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D	p�D	�D
p�D
�>Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�Dp�D�D p�D �D!p�D!�D"p�D"�D#p�D#�D$p�D$�D%p�D%�D&p�D&�D'j>D'�D(p�D(�D)p�D)�D*p�D*�D+p�D+�D,p�D,�D-p�D-�
D.p�D.�D/j>D/�D0p�D0�D1p�D1�D2p�D2�D3p�D3�D4j>D4�>D5p�D5�D6p�D6�D7p�D7�D8p�D8�D9p�D9�D:p�D:�D;p�D;�D<p�D<�D=p�D=�D>p�D>�D?p�D?�D@p�D@�DAp�DA�DBp�DB�DCp�DC�DDp�DD�DEp�DE�DFp�DF�DGp�DG�DHp�DH�DIp�DI�DJp�DJ�DKp�DK�DLp�DL�DMp�DM�DNp�DN�DOp�DO�DPp�DP�DQp�DQ�
DRp�DR�>DSp�DS�DTp�DT�DUw
DU�DVp�Dy��D�$)D�PRD��RD�θD��D�_\D�� D��D�
D�R�D��Dǻ�D�D�QHD�FfD��qD� D�G\D�x�D�� 11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��A��yA��A��A��A��A��A��yA��#A���A��\A�I�A�bA��mA��
A��^A�r�A��wA��DA��A�ȴA�A�A���A�n�A��A��#A��A��A�E�A�33A�oA�  A���A���A�ȴA�{A��9A�v�A�&�A��A�(�A�?}A�S�A���A���A��PA���A�7LA�l�A��;A�`BA�5?A��A�A���A��#A�ƨA��7A�E�A��A��DA�;dA��FA��+A��A�bA��`A��A��A�A���A��A�9XA���A� �A�\)A�  A��7A�K�A���A�&�A��#A�E�A���A��`A���A�
=A���A�S�A���A���A��A�ffA�
=A�ĜA�jA�VA��A���A�E�A���A�A�v�A�z�A�t�A�VA}�hA{33Ay�Av�+At�/Ar  AqAo�wAo"�Am�Ak�Af�RAe7LAd-AbZA]�TA[�7AX��AV��AU��AT��AQ�AN�AJ�uAIx�AGdZAEƨAB�/A>1'A<�A;�A9`BA8=qA8A6�A3�PA2��A1�-A0�yA0v�A/��A/�A.r�A-�A-\)A,�HA+C�A)S�A($�A&��A%ƨA%&�A$^5A#�A#/A!C�AG�A �AA�A�^A�`A�jA|�AffA��AA��A �A\)Az�A��A�+AffAM�A�
A�RAz�An�AE�A�-A�A�DA��A
ȴA
  A	�FA	O�A�A9XAJA�A�9A��A(�A��AC�A��A��A�A�A?}A �9@��y@��@�x�@��P@���@�ff@�O�@�M�@� �@�o@�@�@�7@��/@��@�V@��/@��@�@�Q�@��@�v�@�^5@�^@��@���@��@��
@�o@���@�S�@�%@׾w@�+@�v�@��#@Ӿw@�/@�%@���@��/@Гu@���@�
=@�5?@�hs@�  @���@��T@�Ĝ@�(�@� �@��@ǍP@�+@�ȴ@�=q@\@��@�A�@� �@��@��\@��T@��@���@��`@��/@��9@���@�=q@��@��9@��@�A�@��F@��P@���@�M�@�E�@�{@�%@���@�ff@�E�@�$�@��@���@�?}@��@���@���@��@���@���@��@��@�j@��
@��\@���@��@�7L@�&�@��/@��@��@�33@��@�^5@�-@��@���@�j@��
@�l�@��@��!@�^5@���@�7L@��/@��j@��u@�r�@�bN@�1'@��w@�|�@�K�@�;d@��H@�v�@�$�@�{@�@��@��/@�j@�ƨ@��@�\)@�+@��H@�v�@�^5@�V@��@���@�p�@�/@���@���@��9@�A�@��@���@�t�@�dZ@�\)@�33@�
=@��@��@��R@�v�@�^5@�@���@���@��@�/@��@�b@�\)@�@���@��@�+@��@��+@��@�{@���@�%@�Q�@��@��@��@�1@�Z@�I�@�Q�@��m@�ƨ@���@�K�@��@��F@���@�;d@��m@��
@��
@���@�K�@�ȴ@�n�@�-@�$�@�Ĝ@��`@�O�@�O�@�Ĝ@�Q�@�(�@��;@�;d@��y@��R@y�@oqv@iVm@ac@[o�@T4n@L(�@Dm�@>:*@6��@0y>@,<�@#�+@��@x@($@��@
�@�@��@ ��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   A��yA��A��A��A��A��A��yA��#A���A��\A�I�A�bA��mA��
A��^A�r�A��wA��DA��A�ȴA�A�A���A�n�A��A��#A��A��A�E�A�33A�oA�  A���A���A�ȴA�{A��9A�v�A�&�A��A�(�A�?}A�S�A���A���A��PA���A�7LA�l�A��;A�`BA�5?A��A�A���A��#A�ƨA��7A�E�A��A��DA�;dA��FA��+A��A�bA��`A��A��A�A���A��A�9XA���A� �A�\)A�  A��7A�K�A���A�&�A��#A�E�A���A��`A���A�
=A���A�S�A���A���A��A�ffA�
=A�ĜA�jA�VA��A���A�E�A���A�A�v�A�z�A�t�A�VA}�hA{33Ay�Av�+At�/Ar  AqAo�wAo"�Am�Ak�Af�RAe7LAd-AbZA]�TA[�7AX��AV��AU��AT��AQ�AN�AJ�uAIx�AGdZAEƨAB�/A>1'A<�A;�A9`BA8=qA8A6�A3�PA2��A1�-A0�yA0v�A/��A/�A.r�A-�A-\)A,�HA+C�A)S�A($�A&��A%ƨA%&�A$^5A#�A#/A!C�AG�A �AA�A�^A�`A�jA|�AffA��AA��A �A\)Az�A��A�+AffAM�A�
A�RAz�An�AE�A�-A�A�DA��A
ȴA
  A	�FA	O�A�A9XAJA�A�9A��A(�A��AC�A��A��A�A�A?}A �9@��y@��@�x�@��P@���@�ff@�O�@�M�@� �@�o@�@�@�7@��/@��@�V@��/@��@�@�Q�@��@�v�@�^5@�^@��@���@��@��
@�o@���@�S�@�%@׾w@�+@�v�@��#@Ӿw@�/@�%@���@��/@Гu@���@�
=@�5?@�hs@�  @���@��T@�Ĝ@�(�@� �@��@ǍP@�+@�ȴ@�=q@\@��@�A�@� �@��@��\@��T@��@���@��`@��/@��9@���@�=q@��@��9@��@�A�@��F@��P@���@�M�@�E�@�{@�%@���@�ff@�E�@�$�@��@���@�?}@��@���@���@��@���@���@��@��@�j@��
@��\@���@��@�7L@�&�@��/@��@��@�33@��@�^5@�-@��@���@�j@��
@�l�@��@��!@�^5@���@�7L@��/@��j@��u@�r�@�bN@�1'@��w@�|�@�K�@�;d@��H@�v�@�$�@�{@�@��@��/@�j@�ƨ@��@�\)@�+@��H@�v�@�^5@�V@��@���@�p�@�/@���@���@��9@�A�@��@���@�t�@�dZ@�\)@�33@�
=@��@��@��R@�v�@�^5@�@���@���@��@�/@��@�b@�\)@�@���@��@�+@��@��+@��@�{@���@�%@�Q�@��@��@��@�1@�Z@�I�@�Q�@��m@�ƨ@���@�K�@��@��F@���@�;d@��m@��
@��
@���@�K�@�ȴ@�n�@�-@�$�@�Ĝ@��`@�O�@�O�@�Ĝ@�Q�@�(�@��;@�;d@��y@��R@y�@oqv@iVm@ac@[o�@T4n@L(�@Dm�@>:*@6��@0y>@,<�@#�+@��@x@($@��@
�@�@��@ ��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   ;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;o;oB
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��BB!�BZBl�B�B�\B��B�B�'B�XB��BŢB��B��B�
B�
B�B�B�B�HB�fB�sB�yB�B�B�B�B��B��B��B��BJB+BoB;dBJ�BL�BN�BN�BM�BO�BN�BM�BI�BE�B@�B=qB8RB49B-B�B�B�B�B�BJBB��B�B�B�;B�B��B��B�!B��B��B��B�JBv�Br�Bk�BR�B8RB2-B%�BB
�B
�TB
�;B
��B
�dB
�FB
�3B
�'B
�B
��B
�bB
~�B
q�B
O�B
.B
�B
bB	��B	�B	�B	��B	ȴB	��B	�FB	��B	�+B	n�B	aHB	S�B	9XB	#�B	�B	B��B��B�B�
B��B�FB�B��B��B�DB�B� B{�Bu�Br�Bq�Bm�BgmBhsBhsBgmBhsBhsBgmBffBe`Be`BcTBcTB`BB_;B_;B^5B_;B^5B^5B_;BW
BT�BQ�BQ�BVBW
B\)B\)B[#B[#B[#BZBXBXBVBVBT�BT�BT�BR�BQ�BQ�BP�BP�BN�BN�BL�BK�BK�BJ�BJ�BI�BF�BD�BE�BD�BF�BE�BG�BI�BN�BO�BP�BQ�BS�BT�BT�BS�BVBW
BW
BVBW
BYBW
BVBW
BYB[#BZB[#B\)B]/B^5B]/B]/B]/B]/B]/B^5B^5B^5B^5B^5B_;BbNBdZBe`BffBgmBhsBhsBn�Bp�Bo�Bo�Bn�Bn�Bn�Bq�Br�Bs�Bw�B{�B}�B�B�B�B�B�B�B�B�B�bB�uB��B��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B�B�B�B�!B�!B�!B�3B�^B�wB�}B��BÖBŢBȴB��B��B��B��B�#B�HB�NB�TB�TB�fB�B�B��B��B��B��B��B��B	B	B	1B		7B	
=B	DB	uB	�B	�B	�B	 �B	"�B	'�B	-B	/B	0!B	1'B	2-B	33B	49B	7LB	9XB	:^B	:^B	=qB	@�B	C�B	C�B	E�B	K�B	M�B	P�B	T�B	XB	YB	[#B	]/B	`BB	`BB	`BB	cTB	e`B	gmB	iyB	k�B	l�B	n�B	r�B	u�B	x�B	y�B	y�B	y�B	{�B	|�B	}�B	� B	�B	�B	�B	�+B	�1B	�1B	�7B	�DB	�JB	�JB	�PB	�VB	�VB	�\B	�hB	�uB	�{B	�{B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�B	�B	�!B	�B	�'B	�-B	�3B	�3B	�XB	�qB	��B	��B	��B	��B	��B	��B	ĜB	ĜB	ƨB	ɺB	��B	��B	ɺB	ȴB	ɺB	��B	��B	��B
 OB
�B
�B
B
$�B
-]B
5%B
<�B
A�B
IlB
N�B
R�B
Z�B
`B
eFB
i_B
n�B
xB
{B
}"B
��11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   B
�0B
�*B
�0B
�*B
�*B
�*B
�#B
�B
�B
�B
��B
��B
��B
�B
�MBBRSBd�Bz@B��B�B�FB�YB��B��B��B��B�.B�:B�:B�@B�@B�@B�wBޕB�B�B�B�B�B��B�
B�B��B��BxB�ZB
�B3�BB�BD�BGBGBE�BH
BGBE�BA�B=�B8�B5�B0B,gB%<B�B�B�B�B�B{B�8B�B��B�B�pB�LB�"B��B�YB�B��B��B��BoBj�Bc�BK1B0�B*nB%B
�QB
��B
ۛB
ׂB
�'B
��B
��B
�~B
�rB
�fB
�)B
��B
wIB
i�B
H1B
&iB
�B
�B	�9B	�B	�xB	�GB	�B	��B	��B	�<B	�B	f�B	Y�B	L\B	1�B	?B	�B��B�eB�5B��B�xB��B��B�zB�CB��B��Bz�BxvBt]Bn:Bk'Bj!Bf	B_�B`�B`�B_�B`�B`�B_�B^�B]�B]�B[�B[�BX�BW�BW�BV�BW�BV�BV�BW�BO�BMzBJiBJiBN�BO�BT�BT�BS�BS�BS�BR�BP�BP�BN�BN�BM|BM|BM|BKpBJjBJjBIdBIdBGXBGXBELBDFBDFBCABCABB:B?(B=B>"B=B?(B>#B@/BB;BGYBH_BIeBJlBLxBM~BM~BLyBN�BO�BO�BN�BO�BQ�BO�BN�BO�BQ�BS�BR�BS�BT�BU�BV�BU�BU�BU�BU�BU�BV�BV�BV�BV�BV�BW�BZ�B\�B]�B^�B_�B`�B`�BgBi&Bh Bh BgBgBgBj,Bk2Bl8BpQBtiBvvBy�Bz�Bz�Bz�Bz�B{�B{�B|�B��B��B�B�B�B�'B�3B�9B�?B�?B�?B�?B�LB�dB�wB�}B�}B��B��B��B��B��B��B��B��B��B��B��B�	B�B�"B�4B�RB�^B�kB�wBӢB��B��B��B��B��B�B�4B�GB�LB�LB�LB�RB�qB��B��B	 �B	�B	�B	�B	�B	B	"B	.B	AB	MB	 kB	%�B	'�B	(�B	)�B	*�B	+�B	,�B	/�B	1�B	2�B	2�B	5�B	8�B	<B	<B	>B	DAB	FLB	I^B	MwB	P�B	Q�B	S�B	U�B	X�B	X�B	X�B	[�B	]�B	_�B	a�B	c�B	eB	gB	k(B	n:B	qLB	rRB	rRB	rRB	t^B	ueB	vkB	xwB	{�B	|�B	}�B	�B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�	B	�B	�	B	�B	�B	�"B	�@B	�XB	�wB	�}B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	�B	�B	�-B	�4B	�4B	�-B	�'B	�-B	�FB	�LB	�LB	��B
0B
,B
qB
gB
%�B
-�B
5(B
:GB
A�B
F�B
KFB
SBB
X{B
]�B
a�B
gTB
p�B
s�B
u�B
z�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111   <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
PRES            TEMP            PSAL            PRES_ADJUSTED = PRES - dP                                                                                                                                                                                                                                       none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = PSAL + dS, where dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment factor r.                                                                                                                             dP =0.24 dbar.                                                                                                                                                                                                                                                  none                                                                                                                                                                                                                                                            r =0.9998(+/-0.0001), vertically averaged dS =-0.008(+/-0.004) in PSS-78.                                                                                                                                                                                       Pressures adjusted by using pressure offset at the sea surface. The quoted error is manufacturer specified accuracy in dbar.                                                                                                                                    The quoted error is manufacturer specified accuracy with respect to ITS-90 at time of laboratory calibration.                                                                                                                                                   Significant salinity drift detected. OWC least squares fit adopted. Salinity also adjusted for effects of pressure adjustment. The quoted error is max[0.01, 1xOWC uncertainty] in PSS-78.                                                                      202202041144172022020411441720220204114417  AO  ARCAADJP                                                                    20210318220115    IP                G�O�G�O�G�O�                AO  ARGQQCPL                                                                    20210318220115  QCP$                G�O�G�O�G�O�5F03E           AO  ARGQQCPL                                                                    20210318220115  QCF$                G�O�G�O�G�O�0               UW  ARSQUWQC    WOD & nearby Argo as visual check                               20220204114417  IP                  G�O�G�O�G�O�                