CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-09-22T11:56:37Z creation; 2023-04-26T19:14:30Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.5   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    7�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    7�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    7�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    7�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    7�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    8   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  8   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  8�   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  9   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        9x   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    9�   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    9�   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  9�   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    9�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    9�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  9�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  :   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  :T   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    :�   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   axis      T      
resolution        >�E�r�_K   
_FillValue        A.�~            :�   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    :�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        ?F�k"kmj   
_FillValue        A.�~            :�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   	valid_min         �V�        	valid_max         @V�        axis      Y      
_FillValue        @�i�            :�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    	valid_min         �f�        	valid_max         @f�        axis      X      
_FillValue        @�i�            :�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    :�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    :�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    :�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    :�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    ;    CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        =    PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     x  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     x  d`   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     x  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  �0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ʨ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ҈   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `(   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x h   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �4   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �<   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �L   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �T   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �    HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �(   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20200922115637  20230426191430  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               c   cAA  AOAO7316_008644_099                 7316_008644_099                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�:���u@�:���u11  @�:�.H�@�:�.H�@'(5���@'(5����c�_Z�jj�c�_Z�jj11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�\@@  @�G�@�G�@\@�\A ��A\)A ��A,(�A@  AaG�A�Q�A�  A�  A��A��A�  A�  A�  A��B  B(�B�
B�
B((�B0Q�B8(�B?�
BH  BP  BX  B`  Bh  Bo�
Bx(�B�(�B�  B��B�  B�{B�  B�  B�(�B�(�B�(�B�{B�{B�{B�{B�  B��B�  B�{B�  B�{B�{B�(�B��B��
B��B��
B��B�  B�  B�{B�  B��
B��
C��C�C�C  C

=C
=C
=C��C  C  C  C  C  C  C  C   C!��C$  C&
=C(  C)��C+�C-��C0
=C2
=C4
=C6
=C8  C9��C<  C>
=C@{CB
=CD  CF
=CH
=CJ
=CL
=CN  CP
=CR
=CS��CU��CX  CZ  C[��C^
=C`
=Cb  Cd  Cf
=Ch{Cj
=Cl  Cn  Co�Cq�Cs��Cv
=Cx
=Cz  C|  C~
=C�C���C���C���C���C���C�  C���C���C�C�C�  C���C�  C�  C�C�C�  C�C�
=C�C�  C���C�C�  C�  C�C�C�  C�  C�  C�  C�  C�  C�  C�C���C�  C�C�  C�  C�C�C�
=C�C�  C�C�C�C���C���C���C���C�C�C�
=C�C���C�  C�C�  C�  C�C�  C���C�  C�  C�  C���C���C�
=C�
=C�  C�  C�C���C���C�  C�  C�  C���C���C�  C�C�  C���C�  C�C���C�  C�C�  C���C�C�\C�C�  C���C�  C�C�C�  C�  C�  C�C�  C�  C���C���C���C���C�  C�
=C�
=C�C�C�C�
=C�
=C�  C�  C�  C���C���C���C���C���C�  D   D z�D  D��D�D��D�D��D�D��D�D��D�D��D�D� D�D� D	  D	� D
  D
}qD
�qD}qD�qD}qD  D� D�D��D�D}qD�RDz�D�qD� D  D}qD�D� D�qD}qD  D��D�D}qD  D� D�qDz�D  D�D  D� D  D}qD  D��D�D}qD�qD� D�qD}qD   D ��D!�D!� D"  D"� D#  D#� D$  D$� D$�qD%}qD&  D&� D'  D'� D(  D(}qD(�qD)}qD)�qD*��D+D+� D,  D,� D-  D-� D.  D.}qD.�qD/� D0  D0� D0�qD1� D1�qD2}qD3  D3}qD3�qD4� D5�D5��D6  D6��D7�D7��D8�D8� D9  D9� D9�qD:}qD:�qD;� D<  D<��D=  D=� D>D>��D?  D?� D@  D@��DA�DA��DB�DB� DC  DC� DD  DD� DE  DE� DE�qDF� DG  DG}qDH  DH� DH�qDI}qDJ  DJ� DK  DK� DL  DL� DM  DM� DN�DN}qDN�qDO}qDO�qDP}qDP�qDQ��DR�DR� DS  DS��DTDT��DU  DU� DV�DV� DV�qDW� DX�DX��DX�qDYz�DY��DZ}qD[  D[� D\  D\� D]  D]��D^D^��D_�D_��D`�D`� Da�Da� Da�qDb� Dc  Dc}qDd  Dd}qDd�qDe� Df  Df� Dg  Dg� Dh  Dh}qDh�qDiz�Dj  Dj��Dk  Dk� DlDl��Dm�Dm� Dm�qDn� Do  Do}qDp  Dp� Dp�qDq� Dr  Dr}qDs  Ds��Dt�Dt� Du�Du��Du�qDv� Dw  Dwz�Dw�qDx� Dy�Dy��Dz�Dz� D{  D{� D|  D|��D}  D}z�D}�qD~� D�D��D�HD�@ D��HD�D��D�AHD��HD�� D���D�@ D��HD��HD�  D�@ D�� D�� D���D�>�D�~�D�� D�  D�>�D�� D�� D���D�=qD�� D��HD�  D�>�D��HD�� D���D�@ D��HD�� D�  D�AHD��HD��HD�HD�>�D�~�D��HD��D�@ D�� D��HD���D�@ D��HD�� D���D�=qD�~�D��HD�  D�@ D�~�D�� D���D�>�D�� D�� D���D�>�D��HD�� D���D�>�D�� D�� D�HD�AHD�� D���D���D�@ D��HD��HD�  D�>�D��HD�� D���D�AHD�� D���D���D�>�D�~�D���D���D�>�D�� D�� D�HD�>�D�~�D�� D��D�AHD��HD��HD�  D�@ D�~�D��qD�  D�AHD�� D���D�HD�B�D�� D��qD���D�@ D�� D��HD�  D�>�D�~�D��HD��D�@ D�~�D�� D���D�=qD�~�D��HD�  D�>�D�}qD��qD���D�@ D��HD�� D���D�AHD��HD�� D�HD�@ D�� D�� D�  D�AHD�~�D�� D�  D�@ D��HD���D���D�@ D�~�D�� D��D�@ D��HD��HD�  D�>�D�� D�D�HD�@ D�~�D��qD��)D�>�D��HD���D�  D�AHD�� D���D��)D�>�D�� D�� D�  D�>�D�~�D��qD�  D�AHD��HD�� D�  D�>�D�~�D���D���D�>�D�~�D���D���D�=qD�}qD��qD��qD�=qD�}qD��qD���D�>�D�~�D�� D�HD�AHD��HD��HD�HD�@ D�� D�� D�HD�AHD���D���D�  D�AHD�~�D���D�  D�AHD��HD��HD��D�@ D�}qD���D���D�>�D�� D��HD��D�@ D�|)D���D�HD�AHD�� D���D���D�>�D�~�D�� D���D�>�D�� D�� D�  D�AHD�� D��HD�HD�@ D�� D��HD�  D�@ D�~�D¾�D��qD�>�DÁHD�� D���D�@ DĀ D�� D�  D�>�D�~�D�� D��qD�@ DƂ�D�D�HD�@ Dǀ D�� D�HD�@ DȁHD��HD�  D�@ Dɀ Dɾ�D���D�>�Dʀ Dʾ�D���D�@ D�~�D˾�D�HD�AHD�~�D�� D�  D�@ D́HD��HD�HD�@ D΁HD�� D���D�AHDπ DϾ�D�  D�AHDЁHD��HD���D�>�Dр D��HD�HD�AHDҀ D�� D��D�AHDӀ DӾ�D���D�@ DԀ D�� D�HD�@ DՀ D��HD�  D�@ Dր D��HD�HD�@ DׁHD�� D�  D�>�D؀ D��HD�  D�@ D�~�Dپ�D�  D�AHDڀ Dھ�D�  D�B�Dۂ�D��HD�  D�AHD܁HD��HD�  D�@ D�~�Dݾ�D�  D�@ Dހ D�� D�  D�AHD�~�D�� D�  D�>�D�� D��HD�HD�AHD� D�� D�  D�@ D�~�D�� D�  D�>�D�~�D㾸D�  D�@ D� D侸D�  D�@ D�~�D�� D�  D�AHD�HD��HD�  D�>�D�~�D羸D���D�@ D� D辸D�HD�@ D� D�� D�  D�@ D�}qD꾸D�  D�>�D� D��HD�  D�@ D�~�D�qD���D�AHD� D�� D�  D�>�D�HD��HD�  D�@ D�HD�� D�  D�AHD�~�D�� D�  D�>�D�~�D�� D�  D�=qD�~�D��HD�  D�@ D�HD�D�HD�>�D� D��HD�  D�@ D��HD��HD���D�>�D�}qD�� D�  D�@ D��HD�� D��qD�@ D���D��HD���D�AHD��HD��HD���D�&f>u>�\)?8Q�?��?�{?�@�@(��@:�H@\(�@xQ�@��@�33@�G�@�\)@�(�@Ǯ@�@��
@�\)@�p�AffA(�A�\A��A ��A'�A-p�A4z�A:�HA@  AFffAL��AQG�AVffA[�A`��Adz�Ah��Amp�Ar�\AuAy��A}p�A���A��\A�z�A��RA���A�=qA�(�A�ffA�Q�A��A��
A�{A�  A�G�A��A�A�
=A�G�A��
A�p�A�
=A���A��A�p�A�
=A�G�A��A��A�
=A���A��A��A�
=A���AÅA��A�
=A�G�A��
A�p�A�\)Aљ�A��
A�{A�\)A��A�(�A�{A�  A�=qA�z�A�A�  A�=qA�(�A�{A�A��A�(�A�p�A�\)A���A��A��A�
=B z�BG�B�B
=B  B��B��B=qB\)BQ�B	�B	�B
�\B�Bz�B�B�B�HB�
Bz�B�B�B�RB�Bz�B�BB�\B�B��Bp�B=qB
=B(�BG�BffB\)B Q�B!p�B"ffB#�B$��B&=qB'\)B(Q�B)p�B*�RB,  B-p�B.�RB0(�B1G�B2�\B3�
B5G�B6�HB8(�B9��B:�RB<  B=��B?
=B@��BB{BC�BD��BFffBG�
BIG�BJ�HBLQ�BM�BO�BQ�BR�RBTQ�BUBW\)BX��BZ�\B[�
B]G�B^�HB`Q�Ba�Bc\)Bd��BfffBh  Bip�Bk
=Blz�Bn{Bo�Bq�Br�RBtQ�Bu��Bw33Bx��BzffB|  B}B33B��\B�\)B�=qB�
=B�  B��HB��B���B��B�ffB�G�B�(�B���B��
B���B���B�z�B�G�B�(�B�
=B��
B��RB���B�ffB�G�B�{B���B��
B���B�p�B�=qB��B��
B��RB���B�ffB�G�B�{B��RB��B�=qB��HB��B�(�B��RB�G�B�B�Q�B���B�G�B�B�(�B��\B���B�p�B��
B�=qB���B��B��B��B�Q�B���B��B�p�B��B�(�B���B���B�\)B��B�{B�ffB��HB�33B���B�  B�Q�B��RB��B��B��B�Q�B���B��B��B��B�Q�B��RB��B��B��B�=qB���B���B�\)B��B�  B�Q�B��RB�
=B�p�B�B�(�B��\B��HB�G�B���B�  B�ffB��RB�33B���B�  B�ffB��RB��B��B�  B�Q�B���B�33BÙ�B�  B�ffB���B�G�BŮB�  B�z�B���B�G�BǮB�{B�z�B��HB�\)B�B�=qBʸRB�
=B˅B��B�ffB��HB�G�B�B�(�BΣ�B��BυB�  B�ffB��HB�\)B��
B�Q�B���B�G�BӮB�(�Bԏ\B�
=B�p�B��B�ffB��HB�G�B�B�=qBأ�B��BٮB�(�Bڣ�B��Bۙ�B�{Bܣ�B��Bݙ�B�{B�z�B���B�p�B�  B�z�B�
=B�B�{B�\B���B�p�B��B�ffB���B�p�B�  B�z�B���B�p�B��B�Q�B��HB�\)B�B�ffB��HB�p�B��B�ffB��HB�\)B��
B�ffB��HB�p�B�  B��\B��B�B�(�B��B�33B�B�=qB���B�\)B�  B��\B��B���B�(�B���B��B��B�=qB��RB�\)B��
B�ffB���B�p�B�  B�z�B�
=B��C 
=C G�C ��C �C33C�CC
=CG�C�\C�
C(�Cp�CC
=CQ�C�\C�
C�CffC�RC
=CQ�C��C�HC�CffC�C  CG�C��C�HC	(�C	p�C	�C	�C
33C
z�C
��C{CffC��C�C(�CffC�C  CG�C�\C�
C{C\)C��C�HC33Cz�CC
=CQ�C��C�
C(�Cz�C��C{C\)C��C�C=qC�\C�C33Cz�CC
=CffC�RC
=CQ�C��C�C33C�C�HC(�Cp�C�RC
=CffC�RC  CG�C��C�HC33C�C�
C{C\)C�C�C=qC�\C�HC33Cz�CC
=C\)C�C   C Q�C ��C �HC!33C!�C!�
C"(�C"z�C"�RC#
=C#G�C#��C#��C$G�C$�\C$�
C%�C%ffC%�RC&  C&\)C&�C&��C'=qC'�C'�
C(�C(p�C(C){C)ffC)��C)��C*=qC*�C*�HC+33C+�C+C,{C,\)C,�C-  C-Q�C-�\C-�HC.(�C.z�C.�
C/�C/p�C/�RC0  C0Q�C0��C1  C1G�C1�\C1�HC2(�C2z�C2��C3(�C3p�C3�RC4  C4\)C4�C5  C5G�C5�\C5�
C633C6�\C6�
C733C7z�C7C8{C8\)C8�RC9
=C9Q�C9��C9�C:G�C:��C:�C;33C;z�C;��C<(�C<z�C<��C={C=\)C=�C>
=C>\)C>�C>��C?G�C?�\C?�HC@=qC@�\C@�HCA�CAp�CACB{CBp�CBCC{CC\)CC�CC��CDG�CD��CD��CE=qCE�CE�HCF33CF�\CF��CG{CGffCG�RCH{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                    ?��@�\@@  @�G�@�G�@\@�\A ��A\)A ��A,(�A@  AaG�A�Q�A�  A�  A��A��A�  A�  A�  A��B  B(�B�
B�
B((�B0Q�B8(�B?�
BH  BP  BX  B`  Bh  Bo�
Bx(�B�(�B�  B��B�  B�{B�  B�  B�(�B�(�B�(�B�{B�{B�{B�{B�  B��B�  B�{B�  B�{B�{B�(�B��B��
B��B��
B��B�  B�  B�{B�  B��
B��
C��C�C�C  C

=C
=C
=C��C  C  C  C  C  C  C  C   C!��C$  C&
=C(  C)��C+�C-��C0
=C2
=C4
=C6
=C8  C9��C<  C>
=C@{CB
=CD  CF
=CH
=CJ
=CL
=CN  CP
=CR
=CS��CU��CX  CZ  C[��C^
=C`
=Cb  Cd  Cf
=Ch{Cj
=Cl  Cn  Co�Cq�Cs��Cv
=Cx
=Cz  C|  C~
=C�C���C���C���C���C���C�  C���C���C�C�C�  C���C�  C�  C�C�C�  C�C�
=C�C�  C���C�C�  C�  C�C�C�  C�  C�  C�  C�  C�  C�  C�C���C�  C�C�  C�  C�C�C�
=C�C�  C�C�C�C���C���C���C���C�C�C�
=C�C���C�  C�C�  C�  C�C�  C���C�  C�  C�  C���C���C�
=C�
=C�  C�  C�C���C���C�  C�  C�  C���C���C�  C�C�  C���C�  C�C���C�  C�C�  C���C�C�\C�C�  C���C�  C�C�C�  C�  C�  C�C�  C�  C���C���C���C���C�  C�
=C�
=C�C�C�C�
=C�
=C�  C�  C�  C���C���C���C���C���C�  D   D z�D  D��D�D��D�D��D�D��D�D��D�D��D�D� D�D� D	  D	� D
  D
}qD
�qD}qD�qD}qD  D� D�D��D�D}qD�RDz�D�qD� D  D}qD�D� D�qD}qD  D��D�D}qD  D� D�qDz�D  D�D  D� D  D}qD  D��D�D}qD�qD� D�qD}qD   D ��D!�D!� D"  D"� D#  D#� D$  D$� D$�qD%}qD&  D&� D'  D'� D(  D(}qD(�qD)}qD)�qD*��D+D+� D,  D,� D-  D-� D.  D.}qD.�qD/� D0  D0� D0�qD1� D1�qD2}qD3  D3}qD3�qD4� D5�D5��D6  D6��D7�D7��D8�D8� D9  D9� D9�qD:}qD:�qD;� D<  D<��D=  D=� D>D>��D?  D?� D@  D@��DA�DA��DB�DB� DC  DC� DD  DD� DE  DE� DE�qDF� DG  DG}qDH  DH� DH�qDI}qDJ  DJ� DK  DK� DL  DL� DM  DM� DN�DN}qDN�qDO}qDO�qDP}qDP�qDQ��DR�DR� DS  DS��DTDT��DU  DU� DV�DV� DV�qDW� DX�DX��DX�qDYz�DY��DZ}qD[  D[� D\  D\� D]  D]��D^D^��D_�D_��D`�D`� Da�Da� Da�qDb� Dc  Dc}qDd  Dd}qDd�qDe� Df  Df� Dg  Dg� Dh  Dh}qDh�qDiz�Dj  Dj��Dk  Dk� DlDl��Dm�Dm� Dm�qDn� Do  Do}qDp  Dp� Dp�qDq� Dr  Dr}qDs  Ds��Dt�Dt� Du�Du��Du�qDv� Dw  Dwz�Dw�qDx� Dy�Dy��Dz�Dz� D{  D{� D|  D|��D}  D}z�D}�qD~� D�D��D�HD�@ D��HD�D��D�AHD��HD�� D���D�@ D��HD��HD�  D�@ D�� D�� D���D�>�D�~�D�� D�  D�>�D�� D�� D���D�=qD�� D��HD�  D�>�D��HD�� D���D�@ D��HD�� D�  D�AHD��HD��HD�HD�>�D�~�D��HD��D�@ D�� D��HD���D�@ D��HD�� D���D�=qD�~�D��HD�  D�@ D�~�D�� D���D�>�D�� D�� D���D�>�D��HD�� D���D�>�D�� D�� D�HD�AHD�� D���D���D�@ D��HD��HD�  D�>�D��HD�� D���D�AHD�� D���D���D�>�D�~�D���D���D�>�D�� D�� D�HD�>�D�~�D�� D��D�AHD��HD��HD�  D�@ D�~�D��qD�  D�AHD�� D���D�HD�B�D�� D��qD���D�@ D�� D��HD�  D�>�D�~�D��HD��D�@ D�~�D�� D���D�=qD�~�D��HD�  D�>�D�}qD��qD���D�@ D��HD�� D���D�AHD��HD�� D�HD�@ D�� D�� D�  D�AHD�~�D�� D�  D�@ D��HD���D���D�@ D�~�D�� D��D�@ D��HD��HD�  D�>�D�� D�D�HD�@ D�~�D��qD��)D�>�D��HD���D�  D�AHD�� D���D��)D�>�D�� D�� D�  D�>�D�~�D��qD�  D�AHD��HD�� D�  D�>�D�~�D���D���D�>�D�~�D���D���D�=qD�}qD��qD��qD�=qD�}qD��qD���D�>�D�~�D�� D�HD�AHD��HD��HD�HD�@ D�� D�� D�HD�AHD���D���D�  D�AHD�~�D���D�  D�AHD��HD��HD��D�@ D�}qD���D���D�>�D�� D��HD��D�@ D�|)D���D�HD�AHD�� D���D���D�>�D�~�D�� D���D�>�D�� D�� D�  D�AHD�� D��HD�HD�@ D�� D��HD�  D�@ D�~�D¾�D��qD�>�DÁHD�� D���D�@ DĀ D�� D�  D�>�D�~�D�� D��qD�@ DƂ�D�D�HD�@ Dǀ D�� D�HD�@ DȁHD��HD�  D�@ Dɀ Dɾ�D���D�>�Dʀ Dʾ�D���D�@ D�~�D˾�D�HD�AHD�~�D�� D�  D�@ D́HD��HD�HD�@ D΁HD�� D���D�AHDπ DϾ�D�  D�AHDЁHD��HD���D�>�Dр D��HD�HD�AHDҀ D�� D��D�AHDӀ DӾ�D���D�@ DԀ D�� D�HD�@ DՀ D��HD�  D�@ Dր D��HD�HD�@ DׁHD�� D�  D�>�D؀ D��HD�  D�@ D�~�Dپ�D�  D�AHDڀ Dھ�D�  D�B�Dۂ�D��HD�  D�AHD܁HD��HD�  D�@ D�~�Dݾ�D�  D�@ Dހ D�� D�  D�AHD�~�D�� D�  D�>�D�� D��HD�HD�AHD� D�� D�  D�@ D�~�D�� D�  D�>�D�~�D㾸D�  D�@ D� D侸D�  D�@ D�~�D�� D�  D�AHD�HD��HD�  D�>�D�~�D羸D���D�@ D� D辸D�HD�@ D� D�� D�  D�@ D�}qD꾸D�  D�>�D� D��HD�  D�@ D�~�D�qD���D�AHD� D�� D�  D�>�D�HD��HD�  D�@ D�HD�� D�  D�AHD�~�D�� D�  D�>�D�~�D�� D�  D�=qD�~�D��HD�  D�@ D�HD�D�HD�>�D� D��HD�  D�@ D��HD��HD���D�>�D�}qD�� D�  D�@ D��HD�� D��qD�@ D���D��HD���D�AHD��HD��HD���G�O�>u>�\)?8Q�?��?�{?�@�@(��@:�H@\(�@xQ�@��@�33@�G�@�\)@�(�@Ǯ@�@��
@�\)@�p�AffA(�A�\A��A ��A'�A-p�A4z�A:�HA@  AFffAL��AQG�AVffA[�A`��Adz�Ah��Amp�Ar�\AuAy��A}p�A���A��\A�z�A��RA���A�=qA�(�A�ffA�Q�A��A��
A�{A�  A�G�A��A�A�
=A�G�A��
A�p�A�
=A���A��A�p�A�
=A�G�A��A��A�
=A���A��A��A�
=A���AÅA��A�
=A�G�A��
A�p�A�\)Aљ�A��
A�{A�\)A��A�(�A�{A�  A�=qA�z�A�A�  A�=qA�(�A�{A�A��A�(�A�p�A�\)A���A��A��A�
=B z�BG�B�B
=B  B��B��B=qB\)BQ�B	�B	�B
�\B�Bz�B�B�B�HB�
Bz�B�B�B�RB�Bz�B�BB�\B�B��Bp�B=qB
=B(�BG�BffB\)B Q�B!p�B"ffB#�B$��B&=qB'\)B(Q�B)p�B*�RB,  B-p�B.�RB0(�B1G�B2�\B3�
B5G�B6�HB8(�B9��B:�RB<  B=��B?
=B@��BB{BC�BD��BFffBG�
BIG�BJ�HBLQ�BM�BO�BQ�BR�RBTQ�BUBW\)BX��BZ�\B[�
B]G�B^�HB`Q�Ba�Bc\)Bd��BfffBh  Bip�Bk
=Blz�Bn{Bo�Bq�Br�RBtQ�Bu��Bw33Bx��BzffB|  B}B33B��\B�\)B�=qB�
=B�  B��HB��B���B��B�ffB�G�B�(�B���B��
B���B���B�z�B�G�B�(�B�
=B��
B��RB���B�ffB�G�B�{B���B��
B���B�p�B�=qB��B��
B��RB���B�ffB�G�B�{B��RB��B�=qB��HB��B�(�B��RB�G�B�B�Q�B���B�G�B�B�(�B��\B���B�p�B��
B�=qB���B��B��B��B�Q�B���B��B�p�B��B�(�B���B���B�\)B��B�{B�ffB��HB�33B���B�  B�Q�B��RB��B��B��B�Q�B���B��B��B��B�Q�B��RB��B��B��B�=qB���B���B�\)B��B�  B�Q�B��RB�
=B�p�B�B�(�B��\B��HB�G�B���B�  B�ffB��RB�33B���B�  B�ffB��RB��B��B�  B�Q�B���B�33BÙ�B�  B�ffB���B�G�BŮB�  B�z�B���B�G�BǮB�{B�z�B��HB�\)B�B�=qBʸRB�
=B˅B��B�ffB��HB�G�B�B�(�BΣ�B��BυB�  B�ffB��HB�\)B��
B�Q�B���B�G�BӮB�(�Bԏ\B�
=B�p�B��B�ffB��HB�G�B�B�=qBأ�B��BٮB�(�Bڣ�B��Bۙ�B�{Bܣ�B��Bݙ�B�{B�z�B���B�p�B�  B�z�B�
=B�B�{B�\B���B�p�B��B�ffB���B�p�B�  B�z�B���B�p�B��B�Q�B��HB�\)B�B�ffB��HB�p�B��B�ffB��HB�\)B��
B�ffB��HB�p�B�  B��\B��B�B�(�B��B�33B�B�=qB���B�\)B�  B��\B��B���B�(�B���B��B��B�=qB��RB�\)B��
B�ffB���B�p�B�  B�z�B�
=B��C 
=C G�C ��C �C33C�CC
=CG�C�\C�
C(�Cp�CC
=CQ�C�\C�
C�CffC�RC
=CQ�C��C�HC�CffC�C  CG�C��C�HC	(�C	p�C	�C	�C
33C
z�C
��C{CffC��C�C(�CffC�C  CG�C�\C�
C{C\)C��C�HC33Cz�CC
=CQ�C��C�
C(�Cz�C��C{C\)C��C�C=qC�\C�C33Cz�CC
=CffC�RC
=CQ�C��C�C33C�C�HC(�Cp�C�RC
=CffC�RC  CG�C��C�HC33C�C�
C{C\)C�C�C=qC�\C�HC33Cz�CC
=C\)C�C   C Q�C ��C �HC!33C!�C!�
C"(�C"z�C"�RC#
=C#G�C#��C#��C$G�C$�\C$�
C%�C%ffC%�RC&  C&\)C&�C&��C'=qC'�C'�
C(�C(p�C(C){C)ffC)��C)��C*=qC*�C*�HC+33C+�C+C,{C,\)C,�C-  C-Q�C-�\C-�HC.(�C.z�C.�
C/�C/p�C/�RC0  C0Q�C0��C1  C1G�C1�\C1�HC2(�C2z�C2��C3(�C3p�C3�RC4  C4\)C4�C5  C5G�C5�\C5�
C633C6�\C6�
C733C7z�C7C8{C8\)C8�RC9
=C9Q�C9��C9�C:G�C:��C:�C;33C;z�C;��C<(�C<z�C<��C={C=\)C=�C>
=C>\)C>�C>��C?G�C?�\C?�HC@=qC@�\C@�HCA�CAp�CACB{CBp�CBCC{CC\)CC�CC��CDG�CD��CD��CE=qCE�CE�HCF33CF�\CF��CG{CGffCG�RCH{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                    @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�wA�FA�FA�wA�RA�jA�wA�^A�wA�A�ĜA�ĜA�9A���A���A���A��DA�r�A�jA�`BA�ZA�S�A��`A�l�A��A̓A£�A�A���A�A�A�?}A��7A��;A�ZA��
A���A�dZA�1A�hsA�I�A��-A�bA�;dA���A�G�A�33A�n�A�/A��A���A���A��hA�;dA��PA��AxffAn�Ak�mAe/Aa�A_+A\z�AZ1AX  AU�AN�uAJI�AGdZAFbAD�A@��A>�/A<9XA9�TA9ƨA97LA7��A69XA3x�A0��A/ƨA.��A. �A-��A,��A+�A*M�A)ƨA(��A((�A'�A'��A'G�A&�A&�A&Q�A%�A%�PA%S�A%�A$�HA${A#�A"�`A"I�A!�A!��A ^5A�wAS�AbNA�AG�A�`A�!A�uAVA1A�PA�A1A�;A��At�A`BA�A��A��Ap�AK�A��AA�
AO�A��A-A��A`BA;dA�A��A^5AA�A1'A�A�A��A�AVA�9A��AffA(�A��A�A`BA?}AVA��A��AbNA$�A�A�A7LA�AĜA�A�uAn�A��A��A�^A�PAl�AdZA\)A�A
�\A	�
A	XA�A$�AbA��A��A�`A�A��Ax�A;dA�A��A�9Az�A=qAJA�mA��AVA�+An�A^5AI�AQ�A �A�
A�-A��A�7A?}A"�AA �HA �A J@�\)@���@��@��@���@���@�v�@�{@��@�z�@���@��@��R@���@�ȴ@�ff@��@���@��@�(�@��\@�G�@��@��
@�@��@�\@�=q@�V@��
@��@���@���@띲@�!@�=q@�@��@�9X@��m@���@�ƨ@睲@��@�$�@�&�@�Ĝ@�9X@�b@�@�;d@�~�@��@���@�h@��@�j@�A�@߾w@�C�@ޗ�@��@��@ܣ�@� �@�  @���@�o@�V@���@���@�|�@�@�^5@��T@���@�@ա�@�`B@�%@�b@�t�@��@�~�@�=q@��@Ѻ^@�V@ϕ�@�~�@��@�@�Ĝ@�Z@��@˕�@�33@��y@��@ə�@Ɂ@�hs@�&�@�I�@ǥ�@�33@���@�ff@őh@Ł@�O�@�bN@�C�@��y@�v�@���@�X@�bN@���@���@�-@�X@�j@��P@��@��!@��@��@�7L@��@�z�@��;@�S�@�M�@��#@�hs@���@�9X@��F@�l�@�\)@�
=@��!@�~�@�@��T@���@��-@�x�@��@���@�9X@��@�K�@�
=@�ff@�{@��@�@��7@�?}@��`@���@� �@���@�dZ@�S�@�;d@�"�@�@�ȴ@�^5@�@��-@�/@�Q�@���@�"�@���@�$�@���@�`B@��/@���@�bN@��@�ƨ@�|�@�"�@��R@���@�=q@���@��7@�?}@��@�bN@��@��F@�t�@��H@�$�@�&�@���@��u@��;@��w@��F@��@���@�S�@�+@���@���@���@�v�@�J@�hs@���@���@���@��@�bN@�9X@�b@�  @���@��m@��;@�ƨ@��P@��H@�~�@�E�@��h@�/@���@��u@�(�@��
@��@�t�@�|�@�C�@��@��\@�$�@��#@�/@���@�Ĝ@��j@��u@�z�@�I�@�1'@��m@��P@�|�@�\)@�o@���@���@��!@���@�=q@�hs@�V@��@�Q�@� �@�1@��w@�|�@���@�ȴ@�E�@��^@�hs@���@��j@��9@���@�j@�(�@��m@��w@���@���@��@�;d@�o@��y@���@���@��!@�n�@��-@�`B@�O�@�7L@�V@���@��u@�Q�@�9X@� �@�1@���@���@�K�@�+@��@��H@���@��\@�E�@��#@��@�%@��@�Ĝ@�z�@� �@�;@�@~ff@}�T@}�h@}�@|�@|�@{dZ@z�\@z^5@y��@x�9@x1'@x  @w�P@w;d@v��@vE�@u�@u��@u?}@t�/@t�@s�@sS�@r��@r^5@rJ@q�^@q&�@pQ�@o��@o+@n�+@m�@m�-@mO�@l�/@l��@l9X@l1@kƨ@ko@j-@i�7@i7L@h��@h��@g�@g�w@g��@g+@f��@e�@e�@d�j@d�D@d1@c�@cS�@b�@b=q@a��@ax�@a7L@a�@`��@`�@`1'@`b@_�@_�w@_l�@_
=@^�R@^�+@^E�@^@]�T@]p�@\�j@\Z@[ƨ@[S�@Z��@Z-@Y��@Y�#@Y�7@YX@Y7L@Y�@X��@XĜ@X�u@X  @W��@WK�@V��@V{@U�@U/@T��@T��@T��@T�D@Tz�@TZ@S��@R�\@RM�@R�@R�@R�@Q��@Q�@Qhs@Q%@P�9@PQ�@O��@N�y@N��@Nff@N$�@M�@M�@L��@Lj@LI�@K��@K�F@Kt�@KS�@K"�@J�H@J�!@J~�@Jn�@J^5@I�@Ix�@IG�@I�@H��@H��@HQ�@Hb@G��@G�@G|�@Gl�@G;d@G+@G�@F�y@F�+@F{@E��@E`B@EO�@D��@D�/@D��@C�m@C��@CS�@Co@B�H@BM�@A��@A��@AG�@A%@@��@@b@?�;@?��@?��@?l�@>�@>ff@>$�@>{@=�T@=�h@=p�@=p�@=�@=p�@=/@<��@<��@<z�@<(�@;�F@;33@:��@:^5@:=q@:�@:J@9��@9�#@9��@9G�@9&�@8Ĝ@8 �@7�w@7\)@6v�@5�@5p�@4�j@4�D@4j@4I�@4�@3�
@3��@3C�@2�@2~�@2M�@2-@1�@1��@1��@1x�@1%@0r�@/|�@/K�@/�@.��@.�y@.ȴ@.�R@.��@.v�@.E�@.E�@.5?@-�@-`B@-V@,�@+��@+�
@+ƨ@+��@+t�@+t�@+dZ@+dZ@+C�@*�H@*�\@*^5@*-@)�#@)�^@)��@)X@(�`@(1'@'�;@'�w@'\)@'K�@'K�@'K�@'+@&�R@&V@&$�@%�@%@%��@%�h@%�@%p�@%`B@%O�@%?}@%�@$�/@$�D@$(�@#�m@#�m@#ƨ@#��@#t�@#33@#o@"�!@"M�@"J@!�#@!��@!X@!&�@!&�@ ��@ A�@�@\)@+@v�@@�T@�@V@�@��@�@z�@j@j@Z@Z@I�@�@ƨ@�F@��@��@��@��@S�@�H@�!@n�@=q@�@�@�^@��@G�@%@��@�`@�`@��@�@��@|�@K�@�@�@ȴ@ȴ@�R@��@��@��@��@V@@@`B@O�@?}@/@V@�j@Z@�@1@�
@��@t�@dZ@dZ@S�@C�@"�@�\@^5@M�@J@�@��@�^@��@��@x�@X@7L@%@�`@��@�9@r�@bN@Q�@A�@b@  @�;@�@�P@|�@l�@;d@�@�y@�@�R@��@ff@5?@{@�-@`B@��@�@�j@j@I�@9X@9X@��@�F@��@t�@dZ@S�@S�@C�@"�@
�H@
�H@
�H@
��@
��@
�\@
n�@
=q@
�@	��@	�#@	�^@	��@	X@	�@	�@��@��@Ĝ@��@bN@A�@ �@�;@�w@��@�P@l�@l�@l�@l�@l�@|�A�wA�wA�ƨA���A�9A�RA�9A�-A�FA�RA�jA���A�jA�RA�FA�RA�wA���A�wA�^A�^A�jA�^A�wA�A�ƨA�ĜA�A�A�ĜA�A�ĜA�ĜA���A�A���A�RA�A�!A��A���A��uA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A�~�A�|�A�z�A�z�A�~�A�x�A�r�A�p�A�r�A�l�A�hsA�jA�n�A�l�A�hsA�hsA�l�A�l�A�jA�ffA�hsA�bNA�\)A�^5A�`BA�\)A�ZA�^5A�^5A�ZA�XA�ZA�\)A�VA�S�A�XA�ZA�S�A�S�A�XA�VA�Q�A�O�A�S�A�Q�A�I�A�C�A�7LA�+A�1A���AߑhA�ZA�C�A�Aް!A�=qA�ƨAݡ�A�t�A�ZA�ĜA�M�A��TAۥ�A�n�A��A�Aڝ�A٥�A��TA�=qA���A�1A��A��
A�1A��A�^5A�XAɛ�AǃA��/A�JA��A���A�A�A���A��A�^5A���A�v�A�l�A�bNA�M�A�33A���A�S�A�XA�O�A��A��A�VA���A���A�?}A���A�-A��!A�\)A��A�ȴA��A�\)A�1'A�"�A�
=A��A���A��FA���A��PA�~�A�hsA�7LA��
A��+A�5?A���A��A�$�A�bA���A��A�n�A��A�dZA�ZA���A�A�A��A�A�A���A�l�A��A�33A�bA���A�z�A�v�A�O�A�A���A�r�A�O�A�|�A��A��#A�z�A�1A���A�;dA���A��#A���A�n�A�5?A��A��A�bA�VA�  A��A��A�ȴA��FA��hA�-A��7A�A�A���A�p�A���A��jA���A�~�A�r�A�jA�ffA�S�A�C�A�(�A��yA�bNA��A��A��#A���A���A��RA���A��\A�jA�(�A�bA���A��A��`A��HA��/A�ĜA���A�v�A�7LA��A��A��RA�x�A�(�A�ƨA�^5A���A�p�A�A���A�ȴA���A�x�A�`BA�-A���A��HA��TA��HA��/A��FA��A�jA�S�A�?}A�1A��A��mA��A���A���A�Q�A�-A��A��+A�XA�jA�dZA��A��A���A�5?A���A�+A��A�
=A���A�x�A�33A��;A�x�A�"�A�wA\)A~��A~5?A|��A{K�Az�DAzAy�TAy�wAx��Av�Av~�Av1'Au�As�FAqG�ApbAo"�An5?Am�Am�wAmS�AmVAl�Alr�Al �AlAk�;Ak�wAk�PAk�Ak\)Ak�Ai�TAg�FAe`BAc��Ac�Abv�Ab-Aa�TAa�FAa��Aa��Aa�Aa|�Aap�Aa\)AaA`�A`$�A_��A_oA^�RA^r�A^^5A^ �A]�hA]p�A\��A\�+A\�A[��A[��A[�TA[t�AZ��AZ~�AZ-AY�TAY�PAYK�AY+AX��AX�9AXQ�AW��AWAW�^AW�AW��AW`BAW33AVz�AU��AU�AT��ATAS�7AR��AQhsAP��AO��AN�DAM/ALjAL  AK��AK�7AKS�AJĜAJbAIS�AH�yAH��AHI�AH  AG��AG�AF��AFr�AFI�AFA�AF=qAF9XAF5?AF�AE�;AE��AEp�AE;dAD��AD�jAD�\AD�AC��AC\)AB�\AAhsA@E�A@JA?�A?�^A?�7A??}A?%A>��A>�A>�!A>�uA>bNA>{A=�wA<I�A;O�A;?}A;"�A:ȴA:M�A:JA9�TA9��A9��A9��A9�-A9�FA9��A9��A9�
A9A9�^A9�A9�hA9�PA9t�A9l�A9\)A9S�A8n�A8ffA8=qA7�A7��A7��A7?}A7"�A6��A6�HA6�9A6~�A6bNA61A5�hA5%A4v�A4�A3|�A2��A3oA2��A2�uA1�A1oA0�A0�uA0ffA0I�A0�A0JA/�A/��A/�FA/hsA/XA/C�A/
=A.�A.�HA.��A.�jA.�DA.Q�A.(�A.bA.JA-�A-�mA-�mA-�A-�A-�mA-�
A-��A-p�A-
=A,�HA,�/A,��A,ĜA,��A,  A+�A+oA*�A*�yA*��A*�A*z�A*bNA*ZA*M�A*-A*(�A*$�A* �A*�A)�A)��A)x�A);dA(��A(��A(�9A(��A(�A(n�A(ZA(Q�A(A�A(1'A(�A(bA(1A(  A'��A'��A'��A'�TA'��A'��A'�wA'�^A'�-A'��A'��A'�hA'|�A'l�A'l�A'S�A'G�A'�A&��A&��A&��A&�HA&�A&��A&��A&�!A&�!A&�\A&~�A&v�A&n�A&jA&bNA&^5A&^5A&VA&I�A&A�A&=qA&-A& �A&{A%�A%�
A%��A%��A%�hA%�PA%�PA%�7A%�A%�7A%p�A%XA%C�A%C�A%?}A%7LA%/A%"�A%�A%VA%VA%%A$��A$��A$�A$�`A$�HA$��A$�jA$�A$�uA$M�A#�A#��A#l�A#\)A#?}A#&�A#oA#A"��A"��A"��A"�A"�yA"�`A"�HA"��A"z�A"^5A"M�A"A�A"5?A"$�A"bA"1A"A!�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                    A�wA�FA�FA�wA�RA�jA�wA�^A�wA�A�ĜA�ĜA�9A���A���A���A��DA�r�A�jA�`BA�ZA�S�A��`A�l�A��A̓A£�A�A���A�A�A�?}A��7A��;A�ZA��
A���A�dZA�1A�hsA�I�A��-A�bA�;dA���A�G�A�33A�n�A�/A��A���A���A��hA�;dA��PA��AxffAn�Ak�mAe/Aa�A_+A\z�AZ1AX  AU�AN�uAJI�AGdZAFbAD�A@��A>�/A<9XA9�TA9ƨA97LA7��A69XA3x�A0��A/ƨA.��A. �A-��A,��A+�A*M�A)ƨA(��A((�A'�A'��A'G�A&�A&�A&Q�A%�A%�PA%S�A%�A$�HA${A#�A"�`A"I�A!�A!��A ^5A�wAS�AbNA�AG�A�`A�!A�uAVA1A�PA�A1A�;A��At�A`BA�A��A��Ap�AK�A��AA�
AO�A��A-A��A`BA;dA�A��A^5AA�A1'A�A�A��A�AVA�9A��AffA(�A��A�A`BA?}AVA��A��AbNA$�A�A�A7LA�AĜA�A�uAn�A��A��A�^A�PAl�AdZA\)A�A
�\A	�
A	XA�A$�AbA��A��A�`A�A��Ax�A;dA�A��A�9Az�A=qAJA�mA��AVA�+An�A^5AI�AQ�A �A�
A�-A��A�7A?}A"�AA �HA �A J@�\)@���@��@��@���@���@�v�@�{@��@�z�@���@��@��R@���@�ȴ@�ff@��@���@��@�(�@��\@�G�@��@��
@�@��@�\@�=q@�V@��
@��@���@���@띲@�!@�=q@�@��@�9X@��m@���@�ƨ@睲@��@�$�@�&�@�Ĝ@�9X@�b@�@�;d@�~�@��@���@�h@��@�j@�A�@߾w@�C�@ޗ�@��@��@ܣ�@� �@�  @���@�o@�V@���@���@�|�@�@�^5@��T@���@�@ա�@�`B@�%@�b@�t�@��@�~�@�=q@��@Ѻ^@�V@ϕ�@�~�@��@�@�Ĝ@�Z@��@˕�@�33@��y@��@ə�@Ɂ@�hs@�&�@�I�@ǥ�@�33@���@�ff@őh@Ł@�O�@�bN@�C�@��y@�v�@���@�X@�bN@���@���@�-@�X@�j@��P@��@��!@��@��@�7L@��@�z�@��;@�S�@�M�@��#@�hs@���@�9X@��F@�l�@�\)@�
=@��!@�~�@�@��T@���@��-@�x�@��@���@�9X@��@�K�@�
=@�ff@�{@��@�@��7@�?}@��`@���@� �@���@�dZ@�S�@�;d@�"�@�@�ȴ@�^5@�@��-@�/@�Q�@���@�"�@���@�$�@���@�`B@��/@���@�bN@��@�ƨ@�|�@�"�@��R@���@�=q@���@��7@�?}@��@�bN@��@��F@�t�@��H@�$�@�&�@���@��u@��;@��w@��F@��@���@�S�@�+@���@���@���@�v�@�J@�hs@���@���@���@��@�bN@�9X@�b@�  @���@��m@��;@�ƨ@��P@��H@�~�@�E�@��h@�/@���@��u@�(�@��
@��@�t�@�|�@�C�@��@��\@�$�@��#@�/@���@�Ĝ@��j@��u@�z�@�I�@�1'@��m@��P@�|�@�\)@�o@���@���@��!@���@�=q@�hs@�V@��@�Q�@� �@�1@��w@�|�@���@�ȴ@�E�@��^@�hs@���@��j@��9@���@�j@�(�@��m@��w@���@���@��@�;d@�o@��y@���@���@��!@�n�@��-@�`B@�O�@�7L@�V@���@��u@�Q�@�9X@� �@�1@���@���@�K�@�+@��@��H@���@��\@�E�@��#@��@�%@��@�Ĝ@�z�@� �@�;@�@~ff@}�T@}�h@}�@|�@|�@{dZ@z�\@z^5@y��@x�9@x1'@x  @w�P@w;d@v��@vE�@u�@u��@u?}@t�/@t�@s�@sS�@r��@r^5@rJ@q�^@q&�@pQ�@o��@o+@n�+@m�@m�-@mO�@l�/@l��@l9X@l1@kƨ@ko@j-@i�7@i7L@h��@h��@g�@g�w@g��@g+@f��@e�@e�@d�j@d�D@d1@c�@cS�@b�@b=q@a��@ax�@a7L@a�@`��@`�@`1'@`b@_�@_�w@_l�@_
=@^�R@^�+@^E�@^@]�T@]p�@\�j@\Z@[ƨ@[S�@Z��@Z-@Y��@Y�#@Y�7@YX@Y7L@Y�@X��@XĜ@X�u@X  @W��@WK�@V��@V{@U�@U/@T��@T��@T��@T�D@Tz�@TZ@S��@R�\@RM�@R�@R�@R�@Q��@Q�@Qhs@Q%@P�9@PQ�@O��@N�y@N��@Nff@N$�@M�@M�@L��@Lj@LI�@K��@K�F@Kt�@KS�@K"�@J�H@J�!@J~�@Jn�@J^5@I�@Ix�@IG�@I�@H��@H��@HQ�@Hb@G��@G�@G|�@Gl�@G;d@G+@G�@F�y@F�+@F{@E��@E`B@EO�@D��@D�/@D��@C�m@C��@CS�@Co@B�H@BM�@A��@A��@AG�@A%@@��@@b@?�;@?��@?��@?l�@>�@>ff@>$�@>{@=�T@=�h@=p�@=p�@=�@=p�@=/@<��@<��@<z�@<(�@;�F@;33@:��@:^5@:=q@:�@:J@9��@9�#@9��@9G�@9&�@8Ĝ@8 �@7�w@7\)@6v�@5�@5p�@4�j@4�D@4j@4I�@4�@3�
@3��@3C�@2�@2~�@2M�@2-@1�@1��@1��@1x�@1%@0r�@/|�@/K�@/�@.��@.�y@.ȴ@.�R@.��@.v�@.E�@.E�@.5?@-�@-`B@-V@,�@+��@+�
@+ƨ@+��@+t�@+t�@+dZ@+dZ@+C�@*�H@*�\@*^5@*-@)�#@)�^@)��@)X@(�`@(1'@'�;@'�w@'\)@'K�@'K�@'K�@'+@&�R@&V@&$�@%�@%@%��@%�h@%�@%p�@%`B@%O�@%?}@%�@$�/@$�D@$(�@#�m@#�m@#ƨ@#��@#t�@#33@#o@"�!@"M�@"J@!�#@!��@!X@!&�@!&�@ ��@ A�@�@\)@+@v�@@�T@�@V@�@��@�@z�@j@j@Z@Z@I�@�@ƨ@�F@��@��@��@��@S�@�H@�!@n�@=q@�@�@�^@��@G�@%@��@�`@�`@��@�@��@|�@K�@�@�@ȴ@ȴ@�R@��@��@��@��@V@@@`B@O�@?}@/@V@�j@Z@�@1@�
@��@t�@dZ@dZ@S�@C�@"�@�\@^5@M�@J@�@��@�^@��@��@x�@X@7L@%@�`@��@�9@r�@bN@Q�@A�@b@  @�;@�@�P@|�@l�@;d@�@�y@�@�R@��@ff@5?@{@�-@`B@��@�@�j@j@I�@9X@9X@��@�F@��@t�@dZ@S�@S�@C�@"�@
�H@
�H@
�H@
��@
��@
�\@
n�@
=q@
�@	��@	�#@	�^@	��@	X@	�@	�@��@��@Ĝ@��@bN@A�@ �@�;@�w@��@�P@l�@l�@l�@l�@l�G�O�A�wA�wA�ƨA���A�9A�RA�9A�-A�FA�RA�jA���A�jA�RA�FA�RA�wA���A�wA�^A�^A�jA�^A�wA�A�ƨA�ĜA�A�A�ĜA�A�ĜA�ĜA���A�A���A�RA�A�!A��A���A��uA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A�~�A�|�A�z�A�z�A�~�A�x�A�r�A�p�A�r�A�l�A�hsA�jA�n�A�l�A�hsA�hsA�l�A�l�A�jA�ffA�hsA�bNA�\)A�^5A�`BA�\)A�ZA�^5A�^5A�ZA�XA�ZA�\)A�VA�S�A�XA�ZA�S�A�S�A�XA�VA�Q�A�O�A�S�A�Q�A�I�A�C�A�7LA�+A�1A���AߑhA�ZA�C�A�Aް!A�=qA�ƨAݡ�A�t�A�ZA�ĜA�M�A��TAۥ�A�n�A��A�Aڝ�A٥�A��TA�=qA���A�1A��A��
A�1A��A�^5A�XAɛ�AǃA��/A�JA��A���A�A�A���A��A�^5A���A�v�A�l�A�bNA�M�A�33A���A�S�A�XA�O�A��A��A�VA���A���A�?}A���A�-A��!A�\)A��A�ȴA��A�\)A�1'A�"�A�
=A��A���A��FA���A��PA�~�A�hsA�7LA��
A��+A�5?A���A��A�$�A�bA���A��A�n�A��A�dZA�ZA���A�A�A��A�A�A���A�l�A��A�33A�bA���A�z�A�v�A�O�A�A���A�r�A�O�A�|�A��A��#A�z�A�1A���A�;dA���A��#A���A�n�A�5?A��A��A�bA�VA�  A��A��A�ȴA��FA��hA�-A��7A�A�A���A�p�A���A��jA���A�~�A�r�A�jA�ffA�S�A�C�A�(�A��yA�bNA��A��A��#A���A���A��RA���A��\A�jA�(�A�bA���A��A��`A��HA��/A�ĜA���A�v�A�7LA��A��A��RA�x�A�(�A�ƨA�^5A���A�p�A�A���A�ȴA���A�x�A�`BA�-A���A��HA��TA��HA��/A��FA��A�jA�S�A�?}A�1A��A��mA��A���A���A�Q�A�-A��A��+A�XA�jA�dZA��A��A���A�5?A���A�+A��A�
=A���A�x�A�33A��;A�x�A�"�A�wA\)A~��A~5?A|��A{K�Az�DAzAy�TAy�wAx��Av�Av~�Av1'Au�As�FAqG�ApbAo"�An5?Am�Am�wAmS�AmVAl�Alr�Al �AlAk�;Ak�wAk�PAk�Ak\)Ak�Ai�TAg�FAe`BAc��Ac�Abv�Ab-Aa�TAa�FAa��Aa��Aa�Aa|�Aap�Aa\)AaA`�A`$�A_��A_oA^�RA^r�A^^5A^ �A]�hA]p�A\��A\�+A\�A[��A[��A[�TA[t�AZ��AZ~�AZ-AY�TAY�PAYK�AY+AX��AX�9AXQ�AW��AWAW�^AW�AW��AW`BAW33AVz�AU��AU�AT��ATAS�7AR��AQhsAP��AO��AN�DAM/ALjAL  AK��AK�7AKS�AJĜAJbAIS�AH�yAH��AHI�AH  AG��AG�AF��AFr�AFI�AFA�AF=qAF9XAF5?AF�AE�;AE��AEp�AE;dAD��AD�jAD�\AD�AC��AC\)AB�\AAhsA@E�A@JA?�A?�^A?�7A??}A?%A>��A>�A>�!A>�uA>bNA>{A=�wA<I�A;O�A;?}A;"�A:ȴA:M�A:JA9�TA9��A9��A9��A9�-A9�FA9��A9��A9�
A9A9�^A9�A9�hA9�PA9t�A9l�A9\)A9S�A8n�A8ffA8=qA7�A7��A7��A7?}A7"�A6��A6�HA6�9A6~�A6bNA61A5�hA5%A4v�A4�A3|�A2��A3oA2��A2�uA1�A1oA0�A0�uA0ffA0I�A0�A0JA/�A/��A/�FA/hsA/XA/C�A/
=A.�A.�HA.��A.�jA.�DA.Q�A.(�A.bA.JA-�A-�mA-�mA-�A-�A-�mA-�
A-��A-p�A-
=A,�HA,�/A,��A,ĜA,��A,  A+�A+oA*�A*�yA*��A*�A*z�A*bNA*ZA*M�A*-A*(�A*$�A* �A*�A)�A)��A)x�A);dA(��A(��A(�9A(��A(�A(n�A(ZA(Q�A(A�A(1'A(�A(bA(1A(  A'��A'��A'��A'�TA'��A'��A'�wA'�^A'�-A'��A'��A'�hA'|�A'l�A'l�A'S�A'G�A'�A&��A&��A&��A&�HA&�A&��A&��A&�!A&�!A&�\A&~�A&v�A&n�A&jA&bNA&^5A&^5A&VA&I�A&A�A&=qA&-A& �A&{A%�A%�
A%��A%��A%�hA%�PA%�PA%�7A%�A%�7A%p�A%XA%C�A%C�A%?}A%7LA%/A%"�A%�A%VA%VA%%A$��A$��A$�A$�`A$�HA$��A$�jA$�A$�uA$M�A#�A#��A#l�A#\)A#?}A#&�A#oA#A"��A"��A"��A"�A"�yA"�`A"�HA"��A"z�A"^5A"M�A"A�A"5?A"$�A"bA"1A"A!�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                    ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B��B�lB��B�	B��B�	B�lB��B��B��B�B�B%�B,=B-CB3hB9�B:*B:�B9�B8�BQ�B}�B�[B	=qB	�aB
�B
hB
L�B
rB
v�B
t�B
tTB
uZB
}"B
��B
��B
��B
��B
�B
��B
��B
��B
�xB
�{B
NpB
C�B
2�B
%B
�B	�B	�B	�B	��B	��B	�GB	`�B	^�B	C�B	B'B	7LB	1�B	&�B	#�B	&�B	!bB	%B	"�B	&LB	6B	1�B	:^B	9�B	<6B	AUB	C�B	JXB	Z�B	y�B	��B	~]B	}�B	{�B	.B	��B	�eB	�tB	�B	�0B	�jB	��B	� B	�gB	�#B	�5B	��B	��B	�WB	�B	�B	�8B	��B	�cB
�B
%B
fB
+B
�B
VB
%B
(XB
(�B
+�B
.IB
1[B
4�B
7�B
8�B
?}B
@�B
@B
A B
A B
@�B
A�B
B�B
E9B
EB
D�B
I�B
G�B
GzB
I�B
HKB
M�B
MjB
M�B
M�B
NB
OvB
P�B
Q�B
Q�B
Q�B
R�B
R�B
R�B
U�B
R�B
R B
S[B
R�B
RTB
S�B
R�B
Q�B
P}B
PB
R B
S�B
R�B
Q�B
QB
N�B
OB
O�B
OvB
OvB
PB
QNB
Q�B
TaB
T�B
T�B
S�B
S[B
R�B
NpB
K�B
L�B
H�B
FtB
C�B
C�B
B�B
B'B
?HB
<B
<6B
;dB
:^B
:*B
9�B
8�B
8B
7�B
7B
6zB
:^B
6B
6zB
7B
8B
9�B
9XB
7�B
6B
5�B
5�B
5�B
5?B
4�B
49B
4B
5B
4B
4�B
5B
5?B
6B
6zB
7B
8�B
7�B
4�B
3hB
33B
4B
4nB
5?B
6�B
5B
5B
6�B
2�B
49B
/�B
.}B
-B
+�B
*eB
(XB
&�B
&�B
#�B
#�B
"�B
!�B
#nB
 �B
!�B
 �B
!�B
 �B
�B
!B
�B
�B
�B
!-B
 \B
�B
!B
OB
OB
�B
!B
�B
�B
OB
�B
B
IB
B
�B
�B
B
~B
=B
�B
kB
7B
qB
B
eB
kB
=B
+B
7B
_B
�B
�B
�B
YB
�B
�B
�B
�B
SB
SB
SB
B
$B
YB
SB
�B
�B
�B
MB
MB
B
B
�B
�B
�B
B
�B
�B
�B
�B
B
�B
�B
�B
MB
�B
�B
MB
�B
MB
B
�B
MB
�B
FB
FB
B
�B
FB
�B
�B
�B
�B
{B
B
�B
�B
FB
@B
�B
:B
uB
@B
B
�B
�B
�B
FB
FB
B
�B
{B
{B
FB
{B
FB
MB
�B
�B
+B
�B
1B
�B
�B
�B
	B
=B
	B
�B
B
�B
�B
B
�B
�B
�B
CB
B
xB
xB
�B
B
OB
�B
�B
�B
�B
�B
�B
�B
�B
!B
!B
�B
�B
 'B
 �B
 'B
 'B
 �B
 �B
!-B
!-B
!bB
 �B
"4B
!�B
!�B
 \B
!�B
#�B
$�B
%B
%B
%zB
&LB
&LB
&�B
&LB
&LB
&B
&�B
%�B
%�B
&B
&�B
'B
'RB
'RB
&�B
&�B
&�B
'B
&�B
&�B
'�B
*eB
)�B
)�B
*�B
+B
+6B
+6B
+B
+�B
+�B
+�B
,�B
-wB
.B
.�B
/OB
/B
.�B
-�B
-�B
-�B
.�B
/B
/�B
/�B
0UB
0UB
0!B
0UB
1'B
1�B
1�B
1�B
1[B
2-B
3�B
3�B
3�B
5B
4�B
4nB
5?B
5B
5�B
5B
6B
6FB
7B
7�B
7LB
7LB
7�B
7�B
8B
8B
8�B
8�B
8RB
8�B
9$B
8�B
9XB
9$B
8�B
8�B
9$B
:�B
9�B
9�B
9�B
9�B
:*B
:^B
:�B
:^B
:�B
:�B
:�B
;0B
;�B
;dB
;dB
;�B
;�B
<B
<�B
>BB
>wB
>wB
>wB
>�B
?B
?HB
?HB
@�B
@�B
AUB
A B
AUB
A�B
A�B
B[B
C-B
B�B
C-B
D�B
DgB
DgB
D�B
D�B
EmB
EmB
E�B
FB
FB
F?B
GEB
GB
GEB
G�B
G�B
G�B
HB
HKB
H�B
IB
IB
I�B
I�B
I�B
J#B
J�B
JXB
J�B
J�B
J�B
K^B
K�B
L0B
K�B
L0B
LdB
MB
L�B
L�B
L�B
M6B
M�B
N<B
N<B
N<B
N�B
OBB
OB
O�B
O�B
PB
P�B
P}B
P}B
P}B
QNB
QNB
QB
QNB
Q�B
Q�B
Q�B
R B
R B
RTB
R B
R B
R�B
S&B
S[B
S�B
S�B
TaB
T�B
T�B
T�B
U2B
UgB
UgB
U2B
U�B
UgB
U�B
VmB
VmB
V�B
W�B
WsB
W�B
X�B
XyB
XyB
XyB
X�B
XyB
XEB
Y�B
ZB
ZQB
Z�B
ZQB
ZB
Z�B
ZB
Z�B
[WB
[#B
[WB
\)B
\�B
\�B
\�B
\�B
]�B
]�B
^5B
^jB
^5B
^�B
^�B
_;B
_B
_;B
_pB
_�B
_�B
_pB
_pB
`BB
`vB
`vB
`vB
`�B
`�B
a|B
aB
a|B
a�B
a�B
a�B
a�B
a|B
a�B
a�B
bNB
b�B
b�B
b�B
b�B
cTB
b�B
c B
d&B
c�B
d&B
dZB
d&B
e,B
d�B
e`B
e`B
e�B
f2B
f�B
f�B
f2B
f�B
f�B
g�B
g�B
h
B
g�B
h
B
h�B
h>B
h>B
h>B
h
B
h�B
hsB
h�B
iB
iDB
iyB
i�B
j�B
j�B
jB
j�B
j�B
jB
j�B
j�B
kB
kB
kQB
k�B
k�B
l"B
m]B
l�B
m�B
ncB
m�B
n/B
n/B
n�B
n�B
n�B
o5B
o5B
o�B
o�B
o�B
pB
o�B
pB
pB
p;B
p�B
q�B
qvB
q�B
q�B
q�B
rB
rB
rB
rB
rGB
rGB
rB
r|B
r�B
sB
s�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
uZB
u�B
u�B
u�B
v+B
v+B
v+B
v+B
v�B
wfB
wfB
w�B
w�B
w�B
w�B
w�B
w�B
x8B
x�B
xlB
x�B
y	B
y	B
y	B
y	B
y	B
y	B
y	B
y	B
y	B
y>B
yrB
zB
zB
y�B
zDB
zDB
zxB
zxB
z�B
{B
{JB
{�B
{�B
|B
|PB
|PB
|B
|B
}"B
}�B
}�B
}�B
~�B
~�B
~�B
�B
� B
� B
�4B
�iB
�iB
��B
��B
��B
��B
��B
��B
�;B
�;B
�;B
�;B
�;B
�;B
�oB
�B
�AB
�uB
�uB
�uB
��B
��B
��B
�{B
�{B
�{B
�{B
�GB
�{B
��B
��B
��B
��B
�B
�SB
��B
�SB
��B
�SB
��B
�SB
�SB
��B
��B
�%B
��B
��B
��B
��B
��B
�_B
��B
��B
��B
��B
�fB
�1B
��B
�1B
�fB
�fB
��B
�lB
�lB
�lB
��B
��B
�	B
�	B
�	B
�	B
�rB
�=B
��B
��B
��B
�B
��B
�DB
�DB
�DB
�xB
��B
�xB
��B
��B
�B
�B
�B
�JB
�~B
�~B
��B
��B
��B
�PB
�B
�B
��B
��B
��B
��B
�VB
�\B
�(B
�\B
��B
��B
��B
��B
�.B
��B
��B
��B
�.B
�.B
�4B
��B
�bB
��B
��B
�bB
� B
��B
�hB
��B
�B
�:B
�B
��B
�B
�oB
�:B
�B
��B
�oB
�uB
�B
�B
��B
�FB
�B
��B
��B
��B
��B
�{B
��B
�@B�|B�B�>B�xB��B�PB��B�	B�B�xB��B��B�B�fB��B��B�>B�lB��B��B�B�2B�	B�DB�DB��B��B��B�(B��B�B��B�B��B��B��B�BB�B�B�B"�B$B$tB&�B($B'�B(�B+kB,B,B,B,�B-�B+�B+�B-CB,�B+�B-B.IB,�B-B.IB/B,�B-wB/OB6FB6�B7LB8B8RB7B8�B:�B:*B9XB:*B;dB:*B9$B9�B;0B:�B8�B9�B:*B;0B9�B:�B<B:�B9�B:�B;dB:*B9�B:^B:�B9�B8�B:^B:�B9$B7�B9�B9XB7LB7B8�B9�B7�B7�B:�B=<B>wBAUBL�B^�Bf2BqABs�B~(B~(B�;B��By�B{Bu%B��BncBxlBu�BzBzxBu�B�B�_B	�B��B		B	�B	'�B	-�B	4�B	EB	B'B	GzB	�"B	�B	�B	�B	��B	�WB	�cB	�dB	�B
�B
)�B
�B
�B
1B
B
�B
�B
�B
�B

rB
�B
�B
w�B
��B
a�B
m�B
k�B
u�B
}VB
u%B
xB
y	B
u%B
uZB
v+B
r�B
r�B
t�B
u�B
s�B
tTB
s�B
q�B
r|B
x8B
{B
x�B
q�B
o�B
ncB
t�B
��B
�bB
kB
yrB
z�B
�4B
��B
�dB
��B
�	B
��B
~�B
��B
��B
��B
��B
�(B
��B
��B
�%B
�B
��B
ӏB!�B
�vB
��B
��B
�pB
ǮB
�<B
�<B
�!B
�!B
�UB
��B
�kB
�B
��B
�4B
��B
�B
�!B
�!B
��B
�eB
��B
��B
��B
��B
|PB
ʌB
��B
S[B
R�B
S�B
NpB
L�B
K�B
M�B
L0B
I�B
UgB
O�B
C�B
@B
:�B
9�B
:�B
7�B
9$B
9�B
9�B
@OB
1�B
1�B
-�B
.}B
,�B
+kB
.�B
,qB
)�B
#:B
!�B
(XB
#�B
VB
&LB
�B
7B
qB
�B
B
AB	��B
�B
�B	�"B
�B	��B	�B	��B	��B	�B	�+B	�ZB	�lB	��B	�QB	�+B	��B	�8B	�DB	�mB	�QB	�DB	�vB	��B	��B	��B	�<B	�B	�B	��B	ݘB	רB	՛B	��B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	�\B	�3B	�wB	�VB	�eB	��B	�oB	��B	�VB	�B	��B	�VB	��B	�7B	��B	�_B	�_B	t�B	o�B	o B	l�B	l"B	iyB	f�B	bNB	aB	`�B	^�B	ZQB	W�B	W�B	u�B	��B	~�B	QB	S�B	K)B	IB	H�B	DgB	B�B	C�B	A�B	B�B	B�B	D3B	L�B	FtB	H�B	E�B	NpB	B�B	G�B	9�B	B�B	O�B	+kB	B�B	6�B	D3B	0�B	2-B	2aB	3�B	3�B	3�B	0�B	/�B	0�B	-wB	+�B	*0B	,�B	,�B	(�B	&LB	!�B	!-B	!�B	#:B	"4B	/B	&�B	$tB	 �B	#nB	�B	&�B	'�B	'�B	 �B	,�B	+6B	$B	!�B	B	+B	B	#B	*�B	(�B	$B	"4B	!�B	$B	VB	$�B	(�B	*eB	"hB	"�B	!�B	!�B	 �B	 \B	&�B	#�B	 �B	%�B	$tB	#:B	#�B	(�B	(�B	,=B	5B	?}B	?}B	/�B	0�B	2aB	3hB	5tB	3�B	0!B	2-B	0�B	/�B	3�B	4nB	2�B	k�B	5�B	33B	49B	:�B	?�B	9�B	7�B	:�B	;�B	7�B	7�B	8�B	8�B	8RB	<�B	>�B	>wB	?�B	>�B	>BB	?}B	=�B	?B	>B	R B	@OB	C�B	@B	?HB	FB	D�B	CaB	B�B	D3B	H�B	GzB	E�B	J�B	Q�B	O�B	S&B	U�B	W?B	ZB	XyB	h>B	uZB	��B	r|B	t�B	~(B	y�B	{JB	�B	�;B	�MB	�uB	��B	��B	�B	�iB	��B	|�B	|�B	|�B	}�B	�B	}�B	}�B	}�B	{�B	{�B	|PB	{�B	{JB	z�B	y�B	{�B	|�B	}"B	��B	{�B	|B	|�B	}�B	�B	��B	�bB	�B	�	B	�	B	��B	��B	��B	��B	�*B	��B	��B	��B	��B	��B	��B	��B	�RB	�qB	�OB	��B	�tB	��B	��B	�#B	��B	��B	��B	˒B	��B	͟B	�^B	�B	̘B	�dB	��B	�dB	��B	ϫB	��B	�}B	�pB	͟B	��B	�B	�BB	��B	ҽB	��B	� B	уB	�gB	��B	��B	҉B	�9B	�aB	�9B	�
B	�B	רB	��B	یB	��B	��B	��B	�B	�]B	��B	��B	ߤB	��B	�;B	��B	��B	�NB	��B	�>B	�yB	��B	��B	�sB	��B	�yB	�B	�B	�"B	�B	�)B	��B	�B	�]B	�B	�cB	� B	��B	�/B	�B	�cB	� B	�B	�B	�B	�B	��B	�oB	�B	�8B	�PB	��B	�PB	�JB	�(B	��B	��B	��B	�]B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
B
SB
B
�B
�B
�B
+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                    B�;B��B�AB��B��B��B�B�IB��B��B��B��BTB%RB,&B-�B3�B9�B:_B:�B:dB<B`�B��B�B	v�B	��B
mB
-!B
m�B
|9B
{B
x�B
~XB
�aB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�SB
�&B
V�B
KB
<[B
6�B
�B	��B	�B	�B	��B	�KB	��B	vB	kqB	LNB	J�B	?�B	8�B	1�B	8�B	4�B	+OB	*B	(�B	2�B	<�B	;B	A�B	:�B	>�B	F�B	I]B	S�B	c�B	}eB	�iB	�AB	~�B	hB	�@B	��B	�mB	�UB	ɟB	�B	�kB	�>B	ӜB	֢B	��B	ߖB	�(B	�B	�6B	�B	�B	��B	��B
|B
�B
�B
�B
TB
�B
"�B
( B
);B
)�B
,bB
.�B
2<B
5�B
9aB
;�B
A�B
A'B
@�B
A�B
A�B
A�B
CoB
ExB
FoB
E�B
GB
K�B
H�B
I_B
K3B
K%B
O�B
NNB
N,B
NYB
OB
QB
QB
Q�B
RCB
R�B
STB
SUB
T�B
V�B
SB
SB
T>B
S�B
SB
T@B
SpB
RvB
QoB
P�B
R�B
T�B
S�B
R�B
R4B
O�B
O�B
P<B
O�B
P!B
Q�B
Q�B
Q�B
T�B
UqB
U"B
T0B
TcB
T�B
P�B
M�B
N5B
KpB
F�B
D=B
D�B
E�B
ErB
@aB
<�B
=B
;�B
:�B
;9B
:gB
9�B
8�B
8HB
7�B
9 B
<B
6�B
6�B
7^B
8B
:TB
:YB
8>B
6TB
6ZB
6�B
6B
5�B
5.B
5$B
66B
6YB
4�B
4�B
5B
5sB
6JB
6�B
7�B
:LB
9B
5�B
4�B
3�B
3�B
4yB
5�B
7�B
5rB
6qB
8~B
5�B
6jB
0�B
/�B
-�B
,�B
+hB
) B
(�B
(�B
%�B
&9B
$�B
$1B
%B
!�B
"�B
"�B
#B
!dB
�B
>B
�B
 2B
 <B
"�B
!2B
 �B
�B
B
;B
!B
�B
�B
B
0B
�B
B
CB
B
,B
B
�B
gB
4B
"B
�B
�B
�B
B
"B
�B
2B
jB
	B
�B
!B
B
DB
B
GB
�B
B
�B
�B
�B
�B
�B
�B
aB
&B
IB
FB
�B
B
�B
�B
�B
qB
WB
�B
UB
GB
GB
�B
�B
�B
�B
`B
�B
�B
{B
�B
B
aB
EB
dB
}B
�B
?B
�B
�B
�B
B
1B
�B
B

B
sB
�B
IB
B
�B
B
+B
�B
�B
LB
=B
�B
B
KB
�B
�B
$B
]B
�B
�B
�B
�B
&B
aB
ZB
�B
 B
XB
gB
|B
B
B
7B
�B
�B
B
�B
qB
B
B
DB
!B
&B
�B
�B
�B
�B
 B
B
�B
B
/B
zB
IB
�B
/B
B
B
�B
�B
�B
 MB
 ;B
 �B
!�B
 vB
 �B
"B
!�B
!�B
!�B
!�B
"%B
#�B
#�B
"�B
 �B
"�B
#�B
$�B
%(B
%6B
&B
&�B
'B
'*B
&iB
&�B
&�B
'�B
&�B
&B
&kB
&�B
'dB
'�B
'�B
'B
' B
'B
'5B
&�B
'@B
)3B
+B
*RB
+!B
+[B
+�B
+�B
,	B
+�B
,9B
+�B
+�B
-B
.FB
.�B
/�B
/�B
0YB
/�B
.B
-�B
.4B
.�B
/}B
/�B
0�B
0�B
0�B
0oB
0�B
1�B
1�B
1�B
1�B
26B
3�B
4~B
3�B
4�B
5mB
4�B
5B
5�B
6B
6B
6B
7B
6�B
8B
7�B
7gB
7�B
7�B
8>B
8�B
8pB
8�B
8�B
8�B
9B
9xB
9DB
9�B
9JB
9!B
9ZB
:}B
;+B
:!B
9�B
:B
:zB
:�B
:�B
:�B
:�B
:�B
;	B
;5B
;�B
;�B
;�B
;�B
<B
<B
<�B
=�B
?�B
>�B
>�B
>�B
?DB
?�B
?�B
@B
A6B
A8B
A�B
A�B
A�B
BB
B�B
C'B
CgB
CB
DcB
EB
D�B
D�B
E+B
EbB
E�B
E�B
E�B
FnB
F~B
G B
G�B
GXB
G�B
H)B
HB
HAB
H�B
IB
InB
I�B
I�B
J}B
JB
JWB
J�B
J�B
J�B
J�B
J�B
K@B
L@B
LkB
L�B
LBB
L�B
MB
M6B
L�B
MB
MeB
M�B
NgB
N�B
N{B
N�B
OTB
O~B
O~B
P�B
O�B
P�B
P�B
P�B
P�B
P�B
Q�B
QsB
QAB
Q�B
Q�B
RB
R?B
RVB
RcB
R�B
ROB
R�B
SmB
S�B
S�B
T=B
T|B
T�B
U4B
U'B
UB
UeB
U�B
U�B
UZB
U�B
U�B
V*B
V�B
V�B
WEB
X0B
W�B
XhB
X�B
X�B
X�B
X�B
X�B
X�B
Y	B
Z�B
ZdB
Z�B
Z�B
ZXB
ZBB
Z�B
Z�B
[OB
[�B
[�B
\
B
\�B
]B
\�B
\�B
]dB
^ B
^"B
^�B
^�B
^�B
_B
_B
_`B
_?B
_~B
_�B
_�B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
a B
a6B
a�B
aYB
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bPB
b�B
b�B
c,B
cB
b�B
c|B
c;B
c�B
dwB
d8B
dlB
d�B
d�B
e�B
eOB
e�B
e�B
e�B
f�B
gB
f�B
fiB
f�B
g0B
hCB
g�B
h!B
hB
h[B
h�B
hBB
h6B
hUB
hOB
h�B
h�B
h�B
ihB
i�B
i�B
jYB
kCB
j�B
j�B
j�B
j�B
j�B
k,B
k9B
kHB
k�B
k�B
lYB
ldB
l�B
m�B
mB
nnB
n�B
n!B
nWB
niB
n�B
oB
n�B
o�B
o�B
o�B
o�B
o�B
pFB
o�B
p3B
p~B
p�B
q�B
q�B
q�B
r B
q�B
rB
r(B
r7B
r9B
rDB
rNB
r`B
raB
s	B
s:B
s�B
taB
t�B
t�B
t�B
t�B
t�B
t�B
t�B
t�B
uB
u�B
u�B
u�B
vB
vOB
vFB
v�B
v�B
w?B
w�B
w�B
w�B
w�B
w�B
w�B
w�B
xAB
x�B
x�B
x�B
x�B
y,B
yB
yB
yB
yB
yB
yB
y/B
yPB
y�B
y�B
zNB
zB
zB
zuB
zmB
z�B
z�B
{B
{xB
{�B
{�B
{�B
|^B
|�B
|ZB
|\B
|�B
}�B
}�B
~B
~tB
iB
%B
_B
�8B
�&B
�%B
�ZB
��B
�}B
��B
��B
��B
��B
��B
��B
�PB
�\B
�@B
�?B
�DB
��B
��B
�CB
��B
��B
��B
��B
�B
�B
�0B
��B
��B
��B
��B
��B
�*B
�B
��B
��B
�!B
�_B
�fB
��B
�hB
��B
�YB
��B
�_B
��B
�B
�9B
��B
��B
��B
��B
��B
�B
��B
�	B
��B
��B
�?B
��B
�DB
��B
�EB
�{B
��B
�[B
��B
��B
��B
��B
��B
�B
�B
�B
�/B
��B
�cB
��B
��B
��B
�4B
�B
�WB
�XB
�[B
��B
��B
��B
��B
�B
�)B
�,B
�GB
�oB
��B
��B
��B
��B
��B
��B
�GB
��B
�CB
��B
��B
��B
��B
�|B
�=B
�cB
�B
�B
��B
��B
�BB
��B
��B
�B
�SB
�rB
�4B
��B
��B
��B
��B
��B
�4B
��B
��B
��B
�)B
�RB
�YB
��B
�B
��B
�_B
�B
��B
��B
��B
�5B
�RB
� B
�eB
�'B
�B
��B
��B
��B
�|B
��G�O�B�|B�B�>B�xB��B�PB��B�	B�B�xB��B��B�B�fB��B��B�>B�lB��B��B�B�2B�	B�DB�DB��B��B��B�(B��B�B��B�B��B��B��B�BB�B�B�B"�B$B$tB&�B($B'�B(�B+kB,B,B,B,�B-�B+�B+�B-CB,�B+�B-B.IB,�B-B.IB/B,�B-wB/OB6FB6�B7LB8B8RB7B8�B:�B:*B9XB:*B;dB:*B9$B9�B;0B:�B8�B9�B:*B;0B9�B:�B<B:�B9�B:�B;dB:*B9�B:^B:�B9�B8�B:^B:�B9$B7�B9�B9XB7LB7B8�B9�B7�B7�B:�B=<B>wBAUBL�B^�Bf2BqABs�B~(B~(B�;B��By�B{Bu%B��BncBxlBu�BzBzxBu�B�B�_B	�B��B		B	�B	'�B	-�B	4�B	EB	B'B	GzB	�"B	�B	�B	�B	��B	�WB	�cB	�dB	�B
�B
)�B
�B
�B
1B
B
�B
�B
�B
�B

rB
�B
�B
w�B
��B
a�B
m�B
k�B
u�B
}VB
u%B
xB
y	B
u%B
uZB
v+B
r�B
r�B
t�B
u�B
s�B
tTB
s�B
q�B
r|B
x8B
{B
x�B
q�B
o�B
ncB
t�B
��B
�bB
kB
yrB
z�B
�4B
��B
�dB
��B
�	B
��B
~�B
��B
��B
��B
��B
�(B
��B
��B
�%B
�B
��B
ӏB!�B
�vB
��B
��B
�pB
ǮB
�<B
�<B
�!B
�!B
�UB
��B
�kB
�B
��B
�4B
��B
�B
�!B
�!B
��B
�eB
��B
��B
��B
��B
|PB
ʌB
��B
S[B
R�B
S�B
NpB
L�B
K�B
M�B
L0B
I�B
UgB
O�B
C�B
@B
:�B
9�B
:�B
7�B
9$B
9�B
9�B
@OB
1�B
1�B
-�B
.}B
,�B
+kB
.�B
,qB
)�B
#:B
!�B
(XB
#�B
VB
&LB
�B
7B
qB
�B
B
AB	��B
�B
�B	�"B
�B	��B	�B	��B	��B	�B	�+B	�ZB	�lB	��B	�QB	�+B	��B	�8B	�DB	�mB	�QB	�DB	�vB	��B	��B	��B	�<B	�B	�B	��B	ݘB	רB	՛B	��B	�B	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	�\B	�3B	�wB	�VB	�eB	��B	�oB	��B	�VB	�B	��B	�VB	��B	�7B	��B	�_B	�_B	t�B	o�B	o B	l�B	l"B	iyB	f�B	bNB	aB	`�B	^�B	ZQB	W�B	W�B	u�B	��B	~�B	QB	S�B	K)B	IB	H�B	DgB	B�B	C�B	A�B	B�B	B�B	D3B	L�B	FtB	H�B	E�B	NpB	B�B	G�B	9�B	B�B	O�B	+kB	B�B	6�B	D3B	0�B	2-B	2aB	3�B	3�B	3�B	0�B	/�B	0�B	-wB	+�B	*0B	,�B	,�B	(�B	&LB	!�B	!-B	!�B	#:B	"4B	/B	&�B	$tB	 �B	#nB	�B	&�B	'�B	'�B	 �B	,�B	+6B	$B	!�B	B	+B	B	#B	*�B	(�B	$B	"4B	!�B	$B	VB	$�B	(�B	*eB	"hB	"�B	!�B	!�B	 �B	 \B	&�B	#�B	 �B	%�B	$tB	#:B	#�B	(�B	(�B	,=B	5B	?}B	?}B	/�B	0�B	2aB	3hB	5tB	3�B	0!B	2-B	0�B	/�B	3�B	4nB	2�B	k�B	5�B	33B	49B	:�B	?�B	9�B	7�B	:�B	;�B	7�B	7�B	8�B	8�B	8RB	<�B	>�B	>wB	?�B	>�B	>BB	?}B	=�B	?B	>B	R B	@OB	C�B	@B	?HB	FB	D�B	CaB	B�B	D3B	H�B	GzB	E�B	J�B	Q�B	O�B	S&B	U�B	W?B	ZB	XyB	h>B	uZB	��B	r|B	t�B	~(B	y�B	{JB	�B	�;B	�MB	�uB	��B	��B	�B	�iB	��B	|�B	|�B	|�B	}�B	�B	}�B	}�B	}�B	{�B	{�B	|PB	{�B	{JB	z�B	y�B	{�B	|�B	}"B	��B	{�B	|B	|�B	}�B	�B	��B	�bB	�B	�	B	�	B	��B	��B	��B	��B	�*B	��B	��B	��B	��B	��B	��B	��B	�RB	�qB	�OB	��B	�tB	��B	��B	�#B	��B	��B	��B	˒B	��B	͟B	�^B	�B	̘B	�dB	��B	�dB	��B	ϫB	��B	�}B	�pB	͟B	��B	�B	�BB	��B	ҽB	��B	� B	уB	�gB	��B	��B	҉B	�9B	�aB	�9B	�
B	�B	רB	��B	یB	��B	��B	��B	�B	�]B	��B	��B	ߤB	��B	�;B	��B	��B	�NB	��B	�>B	�yB	��B	��B	�sB	��B	�yB	�B	�B	�"B	�B	�)B	��B	�B	�]B	�B	�cB	� B	��B	�/B	�B	�cB	� B	�B	�B	�B	�B	��B	�oB	�B	�8B	�PB	��B	�PB	�JB	�(B	��B	��B	��B	�]B	��B	��B	��B	��B	��B	��B
�B
�B
�B
�B
B
SB
B
�B
�B
�B
+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                    <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<y�=�=l�=f��=��<���<�}�=��<3)H<#�
<#�
<1<�<��<��<�xr<?mZ<o�<{�r<.h�<#�
<#�
<#�
<��(<���<#�
<#�
<'��<��
<#�
<,~�<�l&<��<�]�<�*�<HbK<��	<W�I<#�
<#�
<#�
<#�
<;ڴ<�_<oe�</�m<#�
<#�
<Vq*<#�
<$
<#�
<#�
<#�
<#�
<#�
<(��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2020092211563720200922115637IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022400410320210224004103QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022400410320210224004103QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2021042714020020210427140200IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142220230426191422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142220230426191422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142220230426191422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                