CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-07-27T19:35:23Z creation; 2022-02-04T23:30:02Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.2   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue        G�O�     x  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     x  c    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     x  �8   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     x  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 0�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x 8p   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � V�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     x ^�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` }    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   }`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �p   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �x   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210727193523  20220204223515  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_177                 6810_008521_177                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @ه-&��j@ه-&��j11  @ه-TɅ�@ه-TɅ�@2�ҝ�%�@2�ҝ�%��d���#�d���#11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@   @:�H@z�H@��R@�  @�\AG�A  A�RA,��A@  A`  A�Q�A��A�  A�  A�Q�A�  A߮A�  A��B  B  B�
B�
B'�B0(�B8  B?�
BG�
BO�BW�B`  Bg�
Bo�
Bx  B�B��
B�{B�  B��B��
B�  B�(�B�  B�  B�{B�{B��
B��B�{B�(�B�(�B�  B�  B�  B�  B�  B�{B�  B��B��
B��
B��B��B��B�{B�  C   C  C  C
=C  C	�C  C��C��C
=C  C�C��C  C
=C
=C   C"  C$
=C&  C'�C)��C,
=C.
=C0  C2  C4
=C6  C7��C:  C<
=C=��C?��CB  CD  CE�CG�CI��CL  CN
=CP  CR  CT  CV
=CX
=CZ  C[��C^  C`{Ca��Cc��Ce��Ch
=Cj  Ck��Cm�Cp
=Cr
=Cs��Cv  Cx
=Cz  C|  C~
=C�C�C���C�C���C�  C�  C�  C�
=C�
=C�
=C�  C�  C�C���C���C�C�C�C�  C�  C�  C�  C�  C�C�  C�  C�C�  C�  C�  C�C�  C�  C�C�C�  C���C�  C�C�  C�  C���C�C���C�C�C�  C�C���C���C�  C�C���C���C���C���C�C�C���C�  C���C�  C���C�  C�  C�C�  C�  C�C�C�  C���C�  C�C�C�  C�  C�  C�
=C���C���C�  C�  C�  C�C�  C�  C���C���C�C���C�C�C�  C�  C�
=C�  C�C�  C�  C�C���C�  C�
=C�C�  C���C���C�  C���C�C�  C�C���C�C�C�
=C�C���C�  C�  C�  C�  C�C�  C�C���C���D �D �qD�D  D��D�qD� D  D��D  D�D�qD}qD  D��D  D� D�qD	z�D	��D
� DD}qD�qD��D  Dz�D�RD}qD  D}qD  Dz�D��D� DD��D��D}qD�D��D�D��D  D}qD�qD}qD�D��DD��DD�D�D��D  D��D�D� D  Dz�D��D}qD   D z�D ��D!}qD"  D"}qD"�qD#}qD#��D$}qD%  D%� D&  D&��D'D'�D(�D(}qD)  D)� D*�D*��D+  D+}qD,  D,}qD,��D-� D.�D.}qD.�RD/� D0  D0z�D0��D1� D2�D2� D2�qD3z�D3�qD4� D4�qD5}qD5�qD6}qD6�qD7� D7�qD8z�D9�D9��D:  D:}qD:�qD;��D<D<��D=  D=��D=�qD>}qD?  D?��D@  D@��DADA}qDA�qDB� DC  DC��DD�DD}qDD�qDE��DF�DF��DGDG��DH�DH��DI�DI��DJ  DJ��DK�DK� DK�qDLz�DM  DM}qDN  DN� DO  DO� DP  DP}qDQ�DQ�DR�DR��DSDS� DS�qDT��DU�DU��DV�DV� DW�DW� DW�qDX� DYDY��DY�qDZ}qD[�D[��D\�D\� D\��D]� D^  D^}qD_  D_� D_�qD`� DaDa��Db  Db� Db�qDc� Dc�qDd� Dd�qDe}qDe�qDf� Dg  Dg}qDg�qDh}qDh�qDi}qDi��Djz�Dj�qDkz�Dl  Dl�Dm�Dm}qDm��Dn}qDn�qDo}qDo�qDp}qDq  Dq� Dq�qDr� Ds�Ds��Dt�Dt��Du  Du}qDu�qDv� Dw�Dw��Dx�Dx� Dx��Dy� Dz  Dz� Dz�qD{xRD{�qD|��D}�D}��D}�qD~}qD~��D}qD�HD�AHD�~�D���D�  D�@ D�� D�� D�HD�@ D�~�D���D���D�>�D�~�D���D�HD�@ D�� D�� D���D�@ D�~�D��qD���D�>�D��HD�D�  D�@ D�� D�� D�  D�@ D��HD���D���D�@ D�� D���D�  D�AHD�� D�� D�  D�>�D�}qD��)D���D�AHD�� D��HD��D�B�D�� D���D���D�@ D��HD��HD�  D�@ D�� D�� D�  D�AHD��HD��qD���D�@ D�� D���D���D�AHD���D���D��D�@ D�� D��HD�HD�@ D�~�D���D�  D�AHD�}qD��HD��D�AHD�� D��HD�  D�>�D�� D��HD�HD�B�D��HD�� D�HD�@ D�� D�D�HD�@ D�� D��HD�  D�>�D�~�D�� D�  D�@ D�� D�� D�  D�=qD��HD�D�  D�@ D�� D��HD��D�@ D�}qD���D�  D�AHD�� D��HD�HD�>�D�~�D��qD���D�AHD��HD�� D�HD�AHD��HD�� D�  D�B�D��HD���D���D�>�D�~�D�� D�  D�>�D�}qD���D�HD�B�D�� D�D�HD�>�D�~�D�� D�  D�=qD�}qD��qD��qD�AHD��HD��HD�HD�>�D�}qD���D�HD�B�D�� D��qD��qD�@ D���D�� D���D�@ D�~�D��qD���D�AHD���D�� D�  D�@ D�� D�� D���D�>�D�~�D���D�  D�>�D�~�D��HD�HD�@ D�� D�D�  D�=qD�� D�� D�HD�AHD�� D��HD��D�>�D�~�D���D���D�AHD�� D�� D���D�>�D��HD�D��D�AHD��HD��HD���D�@ D�� D�� D���D�AHD���D���D�  D�AHD��HD��HD��D�@ D�� D�� D��qD�=qD�~�D���D�  D�AHD���D�� D���D�AHD��HD���D�  D�AHD��HD��HD���D�=qD D���D�HD�>�DÀ D�� D�  D�AHDĀ D��HD���D�@ Dŀ Dž�D�  D�>�D�~�D�� D�HD�@ D�~�DǾ�D��qD�=qD�}qDȽqD��qD�@ DɁHDɾ�D���D�@ DʁHD��HD��D�@ D�|)D˾�D���D�=qD�~�D̽qD���D�@ D�}qD�� D��D�AHD΀ Dξ�D��qD�@ DρHD��HD�  D�@ DЁHD�D�  D�>�Dр D�D��D�>�D�~�DҾ�D��qD�@ D�~�D�� D���D�>�DԁHD�� D���D�@ DՁHD��HD���D�>�D�~�D�� D�  D�=qD�~�D�� D�HD�AHD؀ Dؾ�D���D�@ Dـ Dپ�D�HD�AHDځHD�� D���D�>�Dۀ D۾�D���D�@ D�~�Dܾ�D���D�@ D݁HDݾ�D���D�AHDށHD޽qD���D�AHD߀ D��HD�HD�@ D�~�D�qD��qD�=qD� D��HD�  D�>�D� D⾸D�  D�@ D� D�D�HD�>�D�~�D��HD���D�=qD�~�D徸D�  D�AHD悏D�D���D�=qD�}qD�� D��D�B�D�HD�� D�HD�AHD� D�qD���D�@ D� D��HD�HD�@ D낏D��HD�  D�@ D� D��HD�HD�@ D� D��HD�HD�>�D�~�D�� D�  D�>�D�~�D�� D�  D�>�D�~�D�� D���D�@ D�HD�� D�HD�8R>�G�?��?k�?��R?�Q�?�
=?��H@z�@!G�@0��@@  @Y��@h��@u@��@��@���@�G�@�{@�Q�@��R@���@�@�G�@���@�z�A ��A�A	��A  AffA�HA\)A%A,(�A0  A4z�A:�HAAG�AEAJ=qAP  AVffAZ=qA`  AfffAj�HAo\)Atz�Az=qA~�RA�G�A��A��RA���A��HA�p�A�  A��HA�z�A�ffA��A��
A�{A���A��HA���A�  A��\A�(�A��RA��A��
A�A�Q�A�33A�p�A�\)A��A���AƸRA���A�(�AθRAУ�A��HA�{A�  A�=qA�A�  A��A���A�A陚A��
A�
=A�A�33A�A���A��HA���B   B�B=qB�B�B{B33B��B	�B
=B��B�B�HBz�B��B�RB  B��B�\B�
BG�BffB�B�BB
=B ��B!B"�\B$(�B%G�B%�B'\)B(��B)��B*�RB,  B-p�B.ffB/\)B0��B2{B3
=B4  B5��B6�RB7�B8��B:{B;�B<��B=��B?
=B@(�B@��BB�\BC�BDz�BEBG33BH(�BH��BJ=qBK�BLz�BMG�BN�\BP  BP��BQBS\)BT��BU��BV�\BX  BYp�BZffB[\)B\��B]�B^�HB`Q�BaBb�HBc�Bd��Bf�\Bg�Bh��BiBk\)Bl��Bmp�Bn�RBpQ�Bq��Br=qBs�Bu�BvffBw33Bxz�Bz{B{33B|(�B}��B
=B�  B�z�B�G�B�  B�z�B���B�B�z�B��HB�p�B�(�B���B�G�B��B��\B�33B��B�(�B��HB���B�  B��\B��B��
B�ffB���B�p�B�{B���B�33B�B�ffB�
=B���B�{B��RB�p�B�{B���B��B��
B��\B���B��B�Q�B��HB�\)B��B���B�33B��B�=qB���B��B�{B��RB��B�{B���B��B��B��\B��B��B�{B���B�33B��B�(�B��RB��B�p�B��
B�z�B���B���B�p�B��B�{B�ffB��HB��B�\)B���B�{B�Q�B�ffB���B��B�p�B��B��
B�=qB��RB��HB��B�p�B��
B�{B�Q�B���B��B�G�B�p�B�  B�=qB�z�B���B�33B���B�B�  B�ffB��HB�
=B�G�B��B�{B�ffB��\B���B�G�B��B��
B�(�B���B���B��B�p�B��
B�=qB�ffB���B��B��B�B��B�ffB���B���B�G�B�B�(�B�Q�B���B��B��B��B�  B�z�B��HB�G�B�p�BîB�=qBģ�B���B��BŮB�{B�Q�BƏ\B���B�p�B��
B�  B�Q�B���B�33B�\)B�B�=qB�ffBʸRB�33B˙�B��B�(�Ḅ�B�
=B�G�B͙�B�  B�z�BθRB���B�p�B��
B�(�B�ffBиRB�G�BѮB��B�(�Bҏ\B�
=B�p�Bә�B��B�ffB���B��B�G�B�B�=qB֣�B���B��Bי�B�{B�Q�B؏\B��Bٙ�B�B�{Bڏ\B���B�\)Bۙ�B��B�Q�B���B���B�\)B��
B�=qB�z�B���B�33B߮B�  B�(�B�RB�33B�p�B�B�=qB�\B���B��B�B�{B�Q�B�\B��HB�p�B��
B�(�B�ffB���B�\)B癚B��
B�=qB���B��B�\)B�B�=qB�\B�RB�33B�B�  B�(�B�RB��B�G�B�B�(�B�Q�B���B�G�BB��
B�=qB�RB��HB�G�B��
B�(�B�ffB���B�G�B�B��
B�=qB���B�33B�p�B�B�(�B��RB���B�33B�B�(�B�ffB���B�G�B���B��
B�ffB���B�
=B�\)B��B�Q�B�z�B���B�p�B�B�  B�ffB��HB�G�B�p�B�C �C \)C z�C ��C �
C{CG�CffC�\CC  C(�CQ�Cz�CC�HC  C=qCz�C��C�RC�C(�C\)C�C��C�HC�CQ�Cp�C��C�HC
=C(�Cp�C�C�
C��C�CffC��CC�C(�C\)C�C��C�HC	�C	G�C	p�C	�C	�C
{C
=qC
p�C
�C
��C{C=qCp�CC�C{C=qC�CC�HC
=CG�C�C�C�
C
=CQ�Cz�C��C�HC�CG�Cp�C��C�C{C33Cp�C�C�HC  C33Cz�C�C�
C��C=qCz�C��C��C  CG�Cz�C��C��C
=CQ�Cz�C��C�
C�CQ�Cp�CC��C�CG�C�\C�
C  C(�CffC�C�C
=CG�C�C��C�C(�Cp�C�C�HC{CG�C�\C�HC{C=qCz�C��C
=C33Cp�C�RC�HC�CffC��C�
C
=CQ�C��C��C  C=qC�\CC��C =qC �\C ��C ��C!(�C!z�C!C!��C"(�C"ffC"��C"��C#=qC#ffC#��C#��C$33C$ffC$��C$�
C%�C%ffC%�C%�HC&
=C&G�C&�C&��C'
=C'Q�C'�\C'C'��C((�C(ffC(�RC)  C)=qC)ffC)��C)�C*33C*ffC*�\C*�
C+(�C+\)C+z�C+�C,  C,=qC,p�C,��C,��C-{C-\)C-�\C-C-��C.(�C.z�C.C.�HC/{C/Q�C/��C/�
C0
=C033C0p�C0�C0�C133C1ffC1�\C1��C2
=C2Q�C2�\C2�RC2�C333C3z�C3�RC3�HC4
=C4G�C4�\C4��C5  C5(�C5\)C5��C5�C6{C6=qC6p�C6�C6�C7(�C7\)C7�\C7�RC7�HC8{C8Q�C8��C8�
C9  C9(�C9\)C9��C9�
C:{C:G�C:p�C:��C:��C;  C;=qC;p�C;�C;�C<�C<G�C<p�C<�C<�C=(�C=ffC=��C=��C=�C>�C>\)C>��C>�
C?
=C?33C?\)C?��C?��C@
=C@G�C@�C@�RC@�CA{CA=qCAp�CA��CA�HCB(�CBffCB��CBCB�CC�CC\)CC��CCCC�CD�CD\)CD��CD�
CE
=CEG�CEz�CE�CE�
CF
=CF=qCFz�CFCG
=CG=qCGffCG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                              ?��@   @:�H@z�H@��R@�  @�\AG�A  A�RA,��A@  A`  A�Q�A��A�  A�  A�Q�A�  A߮A�  A��B  B  B�
B�
B'�B0(�B8  B?�
BG�
BO�BW�B`  Bg�
Bo�
Bx  B�B��
B�{B�  B��B��
B�  B�(�B�  B�  B�{B�{B��
B��B�{B�(�B�(�B�  B�  B�  B�  B�  B�{B�  B��B��
B��
B��B��B��B�{B�  C   C  C  C
=C  C	�C  C��C��C
=C  C�C��C  C
=C
=C   C"  C$
=C&  C'�C)��C,
=C.
=C0  C2  C4
=C6  C7��C:  C<
=C=��C?��CB  CD  CE�CG�CI��CL  CN
=CP  CR  CT  CV
=CX
=CZ  C[��C^  C`{Ca��Cc��Ce��Ch
=Cj  Ck��Cm�Cp
=Cr
=Cs��Cv  Cx
=Cz  C|  C~
=C�C�C���C�C���C�  C�  C�  C�
=C�
=C�
=C�  C�  C�C���C���C�C�C�C�  C�  C�  C�  C�  C�C�  C�  C�C�  C�  C�  C�C�  C�  C�C�C�  C���C�  C�C�  C�  C���C�C���C�C�C�  C�C���C���C�  C�C���C���C���C���C�C�C���C�  C���C�  C���C�  C�  C�C�  C�  C�C�C�  C���C�  C�C�C�  C�  C�  C�
=C���C���C�  C�  C�  C�C�  C�  C���C���C�C���C�C�C�  C�  C�
=C�  C�C�  C�  C�C���C�  C�
=C�C�  C���C���C�  C���C�C�  C�C���C�C�C�
=C�C���C�  C�  C�  C�  C�C�  C�C���C���D �D �qD�D  D��D�qD� D  D��D  D�D�qD}qD  D��D  D� D�qD	z�D	��D
� DD}qD�qD��D  Dz�D�RD}qD  D}qD  Dz�D��D� DD��D��D}qD�D��D�D��D  D}qD�qD}qD�D��DD��DD�D�D��D  D��D�D� D  Dz�D��D}qD   D z�D ��D!}qD"  D"}qD"�qD#}qD#��D$}qD%  D%� D&  D&��D'D'�D(�D(}qD)  D)� D*�D*��D+  D+}qD,  D,}qD,��D-� D.�D.}qD.�RD/� D0  D0z�D0��D1� D2�D2� D2�qD3z�D3�qD4� D4�qD5}qD5�qD6}qD6�qD7� D7�qD8z�D9�D9��D:  D:}qD:�qD;��D<D<��D=  D=��D=�qD>}qD?  D?��D@  D@��DADA}qDA�qDB� DC  DC��DD�DD}qDD�qDE��DF�DF��DGDG��DH�DH��DI�DI��DJ  DJ��DK�DK� DK�qDLz�DM  DM}qDN  DN� DO  DO� DP  DP}qDQ�DQ�DR�DR��DSDS� DS�qDT��DU�DU��DV�DV� DW�DW� DW�qDX� DYDY��DY�qDZ}qD[�D[��D\�D\� D\��D]� D^  D^}qD_  D_� D_�qD`� DaDa��Db  Db� Db�qDc� Dc�qDd� Dd�qDe}qDe�qDf� Dg  Dg}qDg�qDh}qDh�qDi}qDi��Djz�Dj�qDkz�Dl  Dl�Dm�Dm}qDm��Dn}qDn�qDo}qDo�qDp}qDq  Dq� Dq�qDr� Ds�Ds��Dt�Dt��Du  Du}qDu�qDv� Dw�Dw��Dx�Dx� Dx��Dy� Dz  Dz� Dz�qD{xRD{�qD|��D}�D}��D}�qD~}qD~��D}qD�HD�AHD�~�D���D�  D�@ D�� D�� D�HD�@ D�~�D���D���D�>�D�~�D���D�HD�@ D�� D�� D���D�@ D�~�D��qD���D�>�D��HD�D�  D�@ D�� D�� D�  D�@ D��HD���D���D�@ D�� D���D�  D�AHD�� D�� D�  D�>�D�}qD��)D���D�AHD�� D��HD��D�B�D�� D���D���D�@ D��HD��HD�  D�@ D�� D�� D�  D�AHD��HD��qD���D�@ D�� D���D���D�AHD���D���D��D�@ D�� D��HD�HD�@ D�~�D���D�  D�AHD�}qD��HD��D�AHD�� D��HD�  D�>�D�� D��HD�HD�B�D��HD�� D�HD�@ D�� D�D�HD�@ D�� D��HD�  D�>�D�~�D�� D�  D�@ D�� D�� D�  D�=qD��HD�D�  D�@ D�� D��HD��D�@ D�}qD���D�  D�AHD�� D��HD�HD�>�D�~�D��qD���D�AHD��HD�� D�HD�AHD��HD�� D�  D�B�D��HD���D���D�>�D�~�D�� D�  D�>�D�}qD���D�HD�B�D�� D�D�HD�>�D�~�D�� D�  D�=qD�}qD��qD��qD�AHD��HD��HD�HD�>�D�}qD���D�HD�B�D�� D��qD��qD�@ D���D�� D���D�@ D�~�D��qD���D�AHD���D�� D�  D�@ D�� D�� D���D�>�D�~�D���D�  D�>�D�~�D��HD�HD�@ D�� D�D�  D�=qD�� D�� D�HD�AHD�� D��HD��D�>�D�~�D���D���D�AHD�� D�� D���D�>�D��HD�D��D�AHD��HD��HD���D�@ D�� D�� D���D�AHD���D���D�  D�AHD��HD��HD��D�@ D�� D�� D��qD�=qD�~�D���D�  D�AHD���D�� D���D�AHD��HD���D�  D�AHD��HD��HD���D�=qD D���D�HD�>�DÀ D�� D�  D�AHDĀ D��HD���D�@ Dŀ Dž�D�  D�>�D�~�D�� D�HD�@ D�~�DǾ�D��qD�=qD�}qDȽqD��qD�@ DɁHDɾ�D���D�@ DʁHD��HD��D�@ D�|)D˾�D���D�=qD�~�D̽qD���D�@ D�}qD�� D��D�AHD΀ Dξ�D��qD�@ DρHD��HD�  D�@ DЁHD�D�  D�>�Dр D�D��D�>�D�~�DҾ�D��qD�@ D�~�D�� D���D�>�DԁHD�� D���D�@ DՁHD��HD���D�>�D�~�D�� D�  D�=qD�~�D�� D�HD�AHD؀ Dؾ�D���D�@ Dـ Dپ�D�HD�AHDځHD�� D���D�>�Dۀ D۾�D���D�@ D�~�Dܾ�D���D�@ D݁HDݾ�D���D�AHDށHD޽qD���D�AHD߀ D��HD�HD�@ D�~�D�qD��qD�=qD� D��HD�  D�>�D� D⾸D�  D�@ D� D�D�HD�>�D�~�D��HD���D�=qD�~�D徸D�  D�AHD悏D�D���D�=qD�}qD�� D��D�B�D�HD�� D�HD�AHD� D�qD���D�@ D� D��HD�HD�@ D낏D��HD�  D�@ D� D��HD�HD�@ D� D��HD�HD�>�D�~�D�� D�  D�>�D�~�D�� D�  D�>�D�~�D�� D���D�@ D�HD�� D�HG�O�>�G�?��?k�?��R?�Q�?�
=?��H@z�@!G�@0��@@  @Y��@h��@u@��@��@���@�G�@�{@�Q�@��R@���@�@�G�@���@�z�A ��A�A	��A  AffA�HA\)A%A,(�A0  A4z�A:�HAAG�AEAJ=qAP  AVffAZ=qA`  AfffAj�HAo\)Atz�Az=qA~�RA�G�A��A��RA���A��HA�p�A�  A��HA�z�A�ffA��A��
A�{A���A��HA���A�  A��\A�(�A��RA��A��
A�A�Q�A�33A�p�A�\)A��A���AƸRA���A�(�AθRAУ�A��HA�{A�  A�=qA�A�  A��A���A�A陚A��
A�
=A�A�33A�A���A��HA���B   B�B=qB�B�B{B33B��B	�B
=B��B�B�HBz�B��B�RB  B��B�\B�
BG�BffB�B�BB
=B ��B!B"�\B$(�B%G�B%�B'\)B(��B)��B*�RB,  B-p�B.ffB/\)B0��B2{B3
=B4  B5��B6�RB7�B8��B:{B;�B<��B=��B?
=B@(�B@��BB�\BC�BDz�BEBG33BH(�BH��BJ=qBK�BLz�BMG�BN�\BP  BP��BQBS\)BT��BU��BV�\BX  BYp�BZffB[\)B\��B]�B^�HB`Q�BaBb�HBc�Bd��Bf�\Bg�Bh��BiBk\)Bl��Bmp�Bn�RBpQ�Bq��Br=qBs�Bu�BvffBw33Bxz�Bz{B{33B|(�B}��B
=B�  B�z�B�G�B�  B�z�B���B�B�z�B��HB�p�B�(�B���B�G�B��B��\B�33B��B�(�B��HB���B�  B��\B��B��
B�ffB���B�p�B�{B���B�33B�B�ffB�
=B���B�{B��RB�p�B�{B���B��B��
B��\B���B��B�Q�B��HB�\)B��B���B�33B��B�=qB���B��B�{B��RB��B�{B���B��B��B��\B��B��B�{B���B�33B��B�(�B��RB��B�p�B��
B�z�B���B���B�p�B��B�{B�ffB��HB��B�\)B���B�{B�Q�B�ffB���B��B�p�B��B��
B�=qB��RB��HB��B�p�B��
B�{B�Q�B���B��B�G�B�p�B�  B�=qB�z�B���B�33B���B�B�  B�ffB��HB�
=B�G�B��B�{B�ffB��\B���B�G�B��B��
B�(�B���B���B��B�p�B��
B�=qB�ffB���B��B��B�B��B�ffB���B���B�G�B�B�(�B�Q�B���B��B��B��B�  B�z�B��HB�G�B�p�BîB�=qBģ�B���B��BŮB�{B�Q�BƏ\B���B�p�B��
B�  B�Q�B���B�33B�\)B�B�=qB�ffBʸRB�33B˙�B��B�(�Ḅ�B�
=B�G�B͙�B�  B�z�BθRB���B�p�B��
B�(�B�ffBиRB�G�BѮB��B�(�Bҏ\B�
=B�p�Bә�B��B�ffB���B��B�G�B�B�=qB֣�B���B��Bי�B�{B�Q�B؏\B��Bٙ�B�B�{Bڏ\B���B�\)Bۙ�B��B�Q�B���B���B�\)B��
B�=qB�z�B���B�33B߮B�  B�(�B�RB�33B�p�B�B�=qB�\B���B��B�B�{B�Q�B�\B��HB�p�B��
B�(�B�ffB���B�\)B癚B��
B�=qB���B��B�\)B�B�=qB�\B�RB�33B�B�  B�(�B�RB��B�G�B�B�(�B�Q�B���B�G�BB��
B�=qB�RB��HB�G�B��
B�(�B�ffB���B�G�B�B��
B�=qB���B�33B�p�B�B�(�B��RB���B�33B�B�(�B�ffB���B�G�B���B��
B�ffB���B�
=B�\)B��B�Q�B�z�B���B�p�B�B�  B�ffB��HB�G�B�p�B�C �C \)C z�C ��C �
C{CG�CffC�\CC  C(�CQ�Cz�CC�HC  C=qCz�C��C�RC�C(�C\)C�C��C�HC�CQ�Cp�C��C�HC
=C(�Cp�C�C�
C��C�CffC��CC�C(�C\)C�C��C�HC	�C	G�C	p�C	�C	�C
{C
=qC
p�C
�C
��C{C=qCp�CC�C{C=qC�CC�HC
=CG�C�C�C�
C
=CQ�Cz�C��C�HC�CG�Cp�C��C�C{C33Cp�C�C�HC  C33Cz�C�C�
C��C=qCz�C��C��C  CG�Cz�C��C��C
=CQ�Cz�C��C�
C�CQ�Cp�CC��C�CG�C�\C�
C  C(�CffC�C�C
=CG�C�C��C�C(�Cp�C�C�HC{CG�C�\C�HC{C=qCz�C��C
=C33Cp�C�RC�HC�CffC��C�
C
=CQ�C��C��C  C=qC�\CC��C =qC �\C ��C ��C!(�C!z�C!C!��C"(�C"ffC"��C"��C#=qC#ffC#��C#��C$33C$ffC$��C$�
C%�C%ffC%�C%�HC&
=C&G�C&�C&��C'
=C'Q�C'�\C'C'��C((�C(ffC(�RC)  C)=qC)ffC)��C)�C*33C*ffC*�\C*�
C+(�C+\)C+z�C+�C,  C,=qC,p�C,��C,��C-{C-\)C-�\C-C-��C.(�C.z�C.C.�HC/{C/Q�C/��C/�
C0
=C033C0p�C0�C0�C133C1ffC1�\C1��C2
=C2Q�C2�\C2�RC2�C333C3z�C3�RC3�HC4
=C4G�C4�\C4��C5  C5(�C5\)C5��C5�C6{C6=qC6p�C6�C6�C7(�C7\)C7�\C7�RC7�HC8{C8Q�C8��C8�
C9  C9(�C9\)C9��C9�
C:{C:G�C:p�C:��C:��C;  C;=qC;p�C;�C;�C<�C<G�C<p�C<�C<�C=(�C=ffC=��C=��C=�C>�C>\)C>��C>�
C?
=C?33C?\)C?��C?��C@
=C@G�C@�C@�RC@�CA{CA=qCAp�CA��CA�HCB(�CBffCB��CBCB�CC�CC\)CC��CCCC�CD�CD\)CD��CD�
CE
=CEG�CEz�CE�CE�
CF
=CF=qCFz�CFCG
=CG=qCGffCG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                              @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�Q�A�K�A�M�A�K�A�M�A�Q�A�bNA�bNA�bNA�dZA�ffA�bNA�bNA�ffA�dZA�dZA�ffA�ffA�ffA�hsA�ffA�hsA�hsA�jA�hsA�ffA�ffA�ffA�dZA�`BA�C�A�XA٣�A���A�5?A�ffA��A���A�ȴAӁA�t�A�E�A���Aͧ�A�ȴA�A�/A�%Aɲ-A�~�AċDAò-A��A7A�~�A���A�bNA�&�A�+A�1'A���A�1'A���A�1A�ȴA���A�ĜA�A���A���A�r�A���A��!A�=qA��/A�9XA� �A�x�A��+A��A�\)A��A���A��9A�  A��A��A�%A��A�v�A�$�A���A��7A�E�A�&�A��-A��A�{A�`BA���A��A�Q�A��HA�33A�33A��RA��A��HA�~�A���A�$�A��7A��A~VAz��AyVAwG�Au`BAsC�Ap �Ann�Am�-Alz�AkhsAi��Af�Ae�AcoA`�A_��A_��A^�A[hsAZ1'AX�uAT��AS?}AQVAN��AM%ALE�AK��AJ�AF�+AC&�ABz�AAoA@bA>�`A>��A>{A=C�A<��A;��A9hsA85?A7%A5A4  A2�+A1S�A.n�A-&�A+�-A)|�A'A%�7A#�A"�A ��AhsA1A�AVA�wA^5A�FAhsA��A��AȴAbA&�Ax�AȴA1'A��Ax�A�jA1A��A?}Av�AffA��A�A�jA�mA33A
ffA
E�A	p�A	S�A	K�A	C�A��Al�A�-AJA=qAx�A�AAAI�AjAVA�#A�`AI�A�A �A��A&�AJA�@��A �A �RA �yAhsA ��A jA  �@��@�{@��D@�A�@�
=@�b@��\@�hs@���@��u@��m@�l�@�@�1'@�A�@��m@�dZ@�l�@��@��@�V@�@�G�@���@�@�1@�w@��@@�l�@�K�@�"�@���@��@�-@�@홚@�G�@��@��`@웦@�z�@��
@�@�-@�@��`@�\)@�M�@��@�/@�w@�!@�7@�  @�o@�V@٩�@��@��H@�n�@��#@�x�@���@�7L@�`B@�hs@Ձ@�O�@�Z@��;@��@�  @��
@�ȴ@�$�@���@ёh@�hs@�7L@�Z@϶F@��H@�~�@�-@�$�@�J@��@�hs@̓u@���@�5?@�@�O�@�?}@�&�@�%@���@� �@�l�@ƸR@�M�@��T@őh@�x�@�`B@�`B@��@Ĵ9@�j@�l�@��H@¸R@�-@��@�7L@��@��9@�Q�@��@��@�K�@�5?@�j@��@�V@�C�@�1'@�t�@��`@�Ĝ@��`@�V@�Ĝ@��m@��@��;@��w@���@��P@��@��@��@���@�?}@�%@��@�1'@��F@�l�@�o@�ȴ@��!@���@��+@���@��@���@�z�@�bN@�(�@���@�o@��!@�=q@��@�@�&�@��9@��D@�I�@��m@��F@���@�\)@�33@��@��H@���@���@���@���@�n�@�M�@�5?@�{@��#@�?}@��@�Z@��@�K�@��y@�ff@�-@��T@�x�@��@���@��j@���@�I�@���@���@���@�K�@��@���@�=q@���@�@�p�@�/@���@�r�@�(�@�  @�ƨ@��F@���@��@�t�@�;d@�ȴ@�-@���@�O�@���@��@���@��9@��@��u@�I�@��F@��P@�t�@�S�@��y@���@��@�p�@�%@���@��D@�j@�b@��m@��F@�K�@��@���@���@�E�@��^@��@��/@�bN@�  @��w@�l�@�
=@�5?@���@��7@�x�@�p�@��@���@�Z@���@��m@��w@���@��P@��@�t�@�dZ@�C�@�33@���@���@��T@���@�@�X@�Ĝ@��u@��D@�j@�1'@� �@��w@�dZ@�K�@�"�@��H@��R@��R@��!@���@���@���@�~�@�5?@��#@��-@��7@�X@�?}@��@�Ĝ@�I�@��m@��P@�33@���@��@��\@�5?@��@�X@�7L@�%@��`@���@�Ĝ@�Ĝ@��@�j@� �@���@��@�@���@�n�@�ff@�^5@�M�@�$�@��@��-@��@�hs@�X@�O�@�G�@�7L@�&�@���@��D@� �@�b@��@��@�@~�y@~E�@}�-@}?}@|��@|�D@|9X@{C�@z~�@zM�@z-@y��@y�7@x��@x  @w�@w|�@wl�@w+@v�+@vV@u��@u�@up�@u/@t�@t(�@s�@s"�@r�\@r^5@rM�@rM�@r�@q�@q��@qhs@qX@q7L@p��@pb@o�@o;d@n�y@n��@nV@m/@l1@k��@k�
@k@j�@i��@i7L@h��@h��@h��@hĜ@h��@hbN@g��@g\)@g\)@gK�@g;d@f�y@f�+@fff@fV@fV@f@e�T@e�-@e�h@e�@eO�@d��@dj@c��@b�H@b~�@bM�@b�@a��@a�@`��@`�`@`�`@`Ĝ@`�@`1'@`b@_�@_�w@_|�@_�@^��@^�R@^v�@^V@]�@]��@]p�@\�@\�j@\9X@[ƨ@[��@[t�@[S�@["�@Z��@ZJ@Y��@YX@Y%@X�9@X�u@XQ�@W�;@W�P@WK�@V$�@U�-@UV@T(�@SS�@S"�@So@S@R�!@Rn�@RJ@Q�7@Q7L@Q%@PĜ@P�9@O�@O\)@OK�@O�@N��@N�@N5?@M�-@MO�@L��@L��@LZ@Kƨ@J��@I�7@H�`@Hr�@H �@G�w@G\)@G;d@F��@F��@F�+@E��@D9X@C��@C"�@C@B�@B�H@B��@B~�@B-@A�@A��@A�7@Ahs@@��@@ �@?�;@?�w@?�P@?K�@>�R@>@=�-@=p�@=p�@=`B@=O�@=/@<�j@<�@;�m@;��@:��@9��@9G�@9%@8�u@8bN@8  @7|�@7
=@6��@6�+@5�@5�@5O�@5?}@5�@4�@4j@4�@3�
@3t�@3"�@2�@2�\@2-@1�#@1��@1�^@1��@1�^@1�^@1�7@17L@0�9@0�@0bN@0  @/�P@/|�@/\)@/\)@/\)@/\)@/K�@/;d@/
=@.�R@.�+@.V@.{@-��@-�@-�@,�@,�@,�D@+�m@+��@+dZ@+33@+o@*�H@*^5@)��@)hs@)X@)7L@)&�@)�@(��@(��@(��@(Ĝ@(��@(r�@(1'@( �@(  @(  @'��@'�@&�R@&v�@&V@&5?@%�T@%�-@%�-@%�@%/@%/@%/@%/@%/@%V@$�/@$�j@$j@$9X@#�F@#t�@#"�@#@"��@"��@"M�@"-@!��@!�#@!X@!7L@!&�@ ��@ ��@ 1'@�P@ȴ@�+@ff@E�@5?@@��@��@p�@��@z�@Z@I�@9X@(�@�@1@��@��@�m@��@�@33@�@�!@�\@^5@J@��@��@��@��@hs@hs@&�@�`@Ĝ@�@A�@1'@ �@b@  @�@��@|�@;d@��@��@�y@�R@��@v�@V@$�@��@�@O�@/@�j@j@1@�m@ƨ@�F@��@dZ@33@�@��@~�@M�@-@�@�A�VA�Q�A�XA�O�A�S�A�K�A�E�A�Q�A�S�A�K�A�I�A�M�A�G�A�M�A�O�A�M�A�I�A�VA�Q�A�S�A�O�A�I�A�dZA�dZA�^5A�^5A�ffA�bNA�`BA�bNA�dZA�dZA�bNA�dZA�hsA�`BA�`BA�bNA�ffA�dZA�`BA�dZA�ffA�`BA�bNA�dZA�bNA�`BA�dZA�hsA�ffA�bNA�dZA�hsA�ffA�bNA�bNA�ffA�ffA�bNA�`BA�ffA�`BA�bNA�ffA�bNA�bNA�ffA�ffA�bNA�ffA�jA�hsA�dZA�dZA�jA�hsA�dZA�ffA�hsA�dZA�bNA�ffA�hsA�dZA�dZA�jA�ffA�dZA�jA�hsA�dZA�hsA�jA�ffA�`BA�hsA�ffA�bNA�ffA�jA�jA�ffA�jA�ffA�dZA�hsA�jA�ffA�dZA�jA�ffA�ffA�jA�hsA�ffA�l�A�jA�hsA�hsA�l�A�hsA�dZA�hsA�dZA�dZA�hsA�bNA�dZA�hsA�ffA�dZA�hsA�ffA�bNA�ffA�ffA�dZA�bNA�dZA�ffA�ffA�bNA�ffA�ffA�dZA�hsA�l�A�jA�`BA�^5A�`BA�bNA�`BA�`BA�dZA�bNA�^5A�`BA�XA�Q�A�M�A�S�A�I�A�;dA�;dA�oA���A��
Aܴ9A�S�A�{A۸RA�ZA�  Aڛ�A�l�AفAؼjA�~�A���Aש�A�t�A�G�A�  A֬A�O�A�bAմ9A�C�A��A�1A���A��`A�ĜA�x�A�\)A�O�A�C�A�;dA�5?A�-A�
=A��yA��#A��#A��
A���A���A��
A���A���A��
A��A���A���A���A�ȴAӼjAӲ-AӴ9AӬAӗ�AӋDA�|�A�hsA�C�A�/A�&�A�Aҟ�A�^5Aї�A���A���A�t�A�M�A�&�A�
=A���A��HA�ƨAϗ�A��A��yA�v�A��A���A���Aʹ9AͶFAʹ9A͇+A�S�A�/A�JA���A��A�ĜA̓uA�ZA�E�A�7LA��A�%A��A˸RA˝�A�r�A�9XA��A���A�A�A�bA�oA�JA�%A�1A�1A�A���A��A��`A��
Aʧ�A�&�A�A��A���A�n�A��A�hsA��#AǗ�A�n�A�`BA�ZA�5?A���A�5?A�VA�A���A��AžwAŬAş�AōPA�ZA�/Aĥ�A�t�A�hsA�hsA�XA�7LA�-A�&�A��A�A��A��`A��/A���A�ƨA�AþwAð!Aç�AÝ�AÑhA�|�A�hsA�Q�A�E�A�5?A�$�A��A��A�oA�
=A�  A�  A���A��A��A��A��yA��;A��
A¸RA�A�~�A�dZA�Q�A�/A�  A��`A��
A�ƨA���A��DA��A�z�A�n�A�VA�A�A�/A�"�A���A��mA��/A��#A���A��wA��FA��9A���A���A���A���A���A��DA��A�t�A�^5A�O�A�I�A�E�A�=qA�&�A���A���A���A��hA��A�dZA�7LA��A���A���A�Q�A�A���A���A��hA�p�A�bNA�M�A�1'A�
=A��A�|�A�^5A�S�A�"�A��yA���A�bNA�G�A���A��A���A�t�A�M�A�7LA��A�VA��A���A��^A��9A��-A���A���A��DA��A�n�A�ffA�\)A�$�A���A���A�1A�
=A�1A�VA��A��A��A��RA���A���A��DA�p�A�bNA�=qA�VA���A�Q�A�A�A���A�A��A���A�  A�A�JA��A��A��A� �A�$�A��A��/A���A�Q�A�/A�{A���A��A��`A��A��A�r�A�r�A�p�A�ffA�ZA�XA�I�A�C�A�9XA�33A�bA��yA��A��A�`BA�1A�A��hA��A�ZA�C�A�+A�VA��TA��^A��A��A�dZA�M�A�?}A�7LA��A��A�ĜA��+A�^5A�^5A�`BA�O�A�A�A�7LA��A�
=A���A��A��/A�ĜA��!A���A�r�A�^5A�-A���A��7A�p�A�?}A� �A�VA��^A��A�hsA�K�A�7LA��A���A��#A��9A��A��A��wA���A�S�A���A��-A���A��\A��DA��hA���A���A���A��\A��A��A�|�A�v�A�ffA�I�A�"�A��TA�ȴA��-A���A��7A�VA��A�A�z�A�jA�\)A�^5A�I�A�/A�1'A�5?A�;dA�;dA�;dA�;dA�=qA�;dA�9XA�7LA�33A�9XA�7LA�&�A��A��A�JA�
=A���A���A���A��jA���A���A��A�dZA�(�A��A��A��mA��HA���A�A��A���A��A�z�A� �A�oA��A��;A���A���A�ƨA��jA��A��uA�\)A�A�A�JA�  A��A��HA��9A�ZA���A��7A��A��`A���A��9A���A���A��7A�^5A�;dA�&�A���A��wA�jA��A��`A��\A�5?A�(�A� �A�%A��DA�JA��TA���A���A�(�A��A���A���A��-A�`BA�+A���A���A��A�XA�Q�A�A�A��A�{A���A��/A��jA���A�v�A�K�A�+A�VA���A��TA���A��uA�r�A�I�A�A�VA��/A��A��PA�ffA�(�A�oA�JA�  A��A��HA��A���A���A�ȴA���A��9A��A���A�r�A�{A��;A���A�r�A�A�A�-A��A�%A���A��A��mA��;A���A�A��FA���A���A��\A��A�|�A�v�A�p�A�bNA�G�A��;A�r�A���A�n�A�XA�XA�XA�XA�XA�VA�G�A�A�A��A�oA��A��#A���A�VA�-A�A��A�7LA�A��mA��#A���A�ȴA��wA���A��+A�x�A�l�A�\)A�K�A�E�A�C�A�;dA�{A���A���A���A�r�A�I�A�(�A��A�JA�A���A��mA�ƨA�~�A�7LA��wA��7A�p�A�=qA��A��A���A���A��7A�7LA�JA���A��`A���A��RA��7A�ffA�VA�C�A�1'A�A��;A��hA�;dA��A��/A���A�9XA���A��jA�p�A�O�A�;dA�"�A��A��RA��uA��A�dZA�K�A�
=A���A���A��DA�t�A�hsA�dZA�bNA�`BA�\)A�O�A�?}A�/A��A�1A��yA�ƨA���A�t�A�ffA�XA�+A�%A���A��+A�5?A�JA��A���A��jG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                              A�Q�A�K�A�M�A�K�A�M�A�Q�A�bNA�bNA�bNA�dZA�ffA�bNA�bNA�ffA�dZA�dZA�ffA�ffA�ffA�hsA�ffA�hsA�hsA�jA�hsA�ffA�ffA�ffA�dZA�`BA�C�A�XA٣�A���A�5?A�ffA��A���A�ȴAӁA�t�A�E�A���Aͧ�A�ȴA�A�/A�%Aɲ-A�~�AċDAò-A��A7A�~�A���A�bNA�&�A�+A�1'A���A�1'A���A�1A�ȴA���A�ĜA�A���A���A�r�A���A��!A�=qA��/A�9XA� �A�x�A��+A��A�\)A��A���A��9A�  A��A��A�%A��A�v�A�$�A���A��7A�E�A�&�A��-A��A�{A�`BA���A��A�Q�A��HA�33A�33A��RA��A��HA�~�A���A�$�A��7A��A~VAz��AyVAwG�Au`BAsC�Ap �Ann�Am�-Alz�AkhsAi��Af�Ae�AcoA`�A_��A_��A^�A[hsAZ1'AX�uAT��AS?}AQVAN��AM%ALE�AK��AJ�AF�+AC&�ABz�AAoA@bA>�`A>��A>{A=C�A<��A;��A9hsA85?A7%A5A4  A2�+A1S�A.n�A-&�A+�-A)|�A'A%�7A#�A"�A ��AhsA1A�AVA�wA^5A�FAhsA��A��AȴAbA&�Ax�AȴA1'A��Ax�A�jA1A��A?}Av�AffA��A�A�jA�mA33A
ffA
E�A	p�A	S�A	K�A	C�A��Al�A�-AJA=qAx�A�AAAI�AjAVA�#A�`AI�A�A �A��A&�AJA�@��A �A �RA �yAhsA ��A jA  �@��@�{@��D@�A�@�
=@�b@��\@�hs@���@��u@��m@�l�@�@�1'@�A�@��m@�dZ@�l�@��@��@�V@�@�G�@���@�@�1@�w@��@@�l�@�K�@�"�@���@��@�-@�@홚@�G�@��@��`@웦@�z�@��
@�@�-@�@��`@�\)@�M�@��@�/@�w@�!@�7@�  @�o@�V@٩�@��@��H@�n�@��#@�x�@���@�7L@�`B@�hs@Ձ@�O�@�Z@��;@��@�  @��
@�ȴ@�$�@���@ёh@�hs@�7L@�Z@϶F@��H@�~�@�-@�$�@�J@��@�hs@̓u@���@�5?@�@�O�@�?}@�&�@�%@���@� �@�l�@ƸR@�M�@��T@őh@�x�@�`B@�`B@��@Ĵ9@�j@�l�@��H@¸R@�-@��@�7L@��@��9@�Q�@��@��@�K�@�5?@�j@��@�V@�C�@�1'@�t�@��`@�Ĝ@��`@�V@�Ĝ@��m@��@��;@��w@���@��P@��@��@��@���@�?}@�%@��@�1'@��F@�l�@�o@�ȴ@��!@���@��+@���@��@���@�z�@�bN@�(�@���@�o@��!@�=q@��@�@�&�@��9@��D@�I�@��m@��F@���@�\)@�33@��@��H@���@���@���@���@�n�@�M�@�5?@�{@��#@�?}@��@�Z@��@�K�@��y@�ff@�-@��T@�x�@��@���@��j@���@�I�@���@���@���@�K�@��@���@�=q@���@�@�p�@�/@���@�r�@�(�@�  @�ƨ@��F@���@��@�t�@�;d@�ȴ@�-@���@�O�@���@��@���@��9@��@��u@�I�@��F@��P@�t�@�S�@��y@���@��@�p�@�%@���@��D@�j@�b@��m@��F@�K�@��@���@���@�E�@��^@��@��/@�bN@�  @��w@�l�@�
=@�5?@���@��7@�x�@�p�@��@���@�Z@���@��m@��w@���@��P@��@�t�@�dZ@�C�@�33@���@���@��T@���@�@�X@�Ĝ@��u@��D@�j@�1'@� �@��w@�dZ@�K�@�"�@��H@��R@��R@��!@���@���@���@�~�@�5?@��#@��-@��7@�X@�?}@��@�Ĝ@�I�@��m@��P@�33@���@��@��\@�5?@��@�X@�7L@�%@��`@���@�Ĝ@�Ĝ@��@�j@� �@���@��@�@���@�n�@�ff@�^5@�M�@�$�@��@��-@��@�hs@�X@�O�@�G�@�7L@�&�@���@��D@� �@�b@��@��@�@~�y@~E�@}�-@}?}@|��@|�D@|9X@{C�@z~�@zM�@z-@y��@y�7@x��@x  @w�@w|�@wl�@w+@v�+@vV@u��@u�@up�@u/@t�@t(�@s�@s"�@r�\@r^5@rM�@rM�@r�@q�@q��@qhs@qX@q7L@p��@pb@o�@o;d@n�y@n��@nV@m/@l1@k��@k�
@k@j�@i��@i7L@h��@h��@h��@hĜ@h��@hbN@g��@g\)@g\)@gK�@g;d@f�y@f�+@fff@fV@fV@f@e�T@e�-@e�h@e�@eO�@d��@dj@c��@b�H@b~�@bM�@b�@a��@a�@`��@`�`@`�`@`Ĝ@`�@`1'@`b@_�@_�w@_|�@_�@^��@^�R@^v�@^V@]�@]��@]p�@\�@\�j@\9X@[ƨ@[��@[t�@[S�@["�@Z��@ZJ@Y��@YX@Y%@X�9@X�u@XQ�@W�;@W�P@WK�@V$�@U�-@UV@T(�@SS�@S"�@So@S@R�!@Rn�@RJ@Q�7@Q7L@Q%@PĜ@P�9@O�@O\)@OK�@O�@N��@N�@N5?@M�-@MO�@L��@L��@LZ@Kƨ@J��@I�7@H�`@Hr�@H �@G�w@G\)@G;d@F��@F��@F�+@E��@D9X@C��@C"�@C@B�@B�H@B��@B~�@B-@A�@A��@A�7@Ahs@@��@@ �@?�;@?�w@?�P@?K�@>�R@>@=�-@=p�@=p�@=`B@=O�@=/@<�j@<�@;�m@;��@:��@9��@9G�@9%@8�u@8bN@8  @7|�@7
=@6��@6�+@5�@5�@5O�@5?}@5�@4�@4j@4�@3�
@3t�@3"�@2�@2�\@2-@1�#@1��@1�^@1��@1�^@1�^@1�7@17L@0�9@0�@0bN@0  @/�P@/|�@/\)@/\)@/\)@/\)@/K�@/;d@/
=@.�R@.�+@.V@.{@-��@-�@-�@,�@,�@,�D@+�m@+��@+dZ@+33@+o@*�H@*^5@)��@)hs@)X@)7L@)&�@)�@(��@(��@(��@(Ĝ@(��@(r�@(1'@( �@(  @(  @'��@'�@&�R@&v�@&V@&5?@%�T@%�-@%�-@%�@%/@%/@%/@%/@%/@%V@$�/@$�j@$j@$9X@#�F@#t�@#"�@#@"��@"��@"M�@"-@!��@!�#@!X@!7L@!&�@ ��@ ��@ 1'@�P@ȴ@�+@ff@E�@5?@@��@��@p�@��@z�@Z@I�@9X@(�@�@1@��@��@�m@��@�@33@�@�!@�\@^5@J@��@��@��@��@hs@hs@&�@�`@Ĝ@�@A�@1'@ �@b@  @�@��@|�@;d@��@��@�y@�R@��@v�@V@$�@��@�@O�@/@�j@j@1@�m@ƨ@�F@��@dZ@33@�@��@~�@M�@-@�G�O�A�VA�Q�A�XA�O�A�S�A�K�A�E�A�Q�A�S�A�K�A�I�A�M�A�G�A�M�A�O�A�M�A�I�A�VA�Q�A�S�A�O�A�I�A�dZA�dZA�^5A�^5A�ffA�bNA�`BA�bNA�dZA�dZA�bNA�dZA�hsA�`BA�`BA�bNA�ffA�dZA�`BA�dZA�ffA�`BA�bNA�dZA�bNA�`BA�dZA�hsA�ffA�bNA�dZA�hsA�ffA�bNA�bNA�ffA�ffA�bNA�`BA�ffA�`BA�bNA�ffA�bNA�bNA�ffA�ffA�bNA�ffA�jA�hsA�dZA�dZA�jA�hsA�dZA�ffA�hsA�dZA�bNA�ffA�hsA�dZA�dZA�jA�ffA�dZA�jA�hsA�dZA�hsA�jA�ffA�`BA�hsA�ffA�bNA�ffA�jA�jA�ffA�jA�ffA�dZA�hsA�jA�ffA�dZA�jA�ffA�ffA�jA�hsA�ffA�l�A�jA�hsA�hsA�l�A�hsA�dZA�hsA�dZA�dZA�hsA�bNA�dZA�hsA�ffA�dZA�hsA�ffA�bNA�ffA�ffA�dZA�bNA�dZA�ffA�ffA�bNA�ffA�ffA�dZA�hsA�l�A�jA�`BA�^5A�`BA�bNA�`BA�`BA�dZA�bNA�^5A�`BA�XA�Q�A�M�A�S�A�I�A�;dA�;dA�oA���A��
Aܴ9A�S�A�{A۸RA�ZA�  Aڛ�A�l�AفAؼjA�~�A���Aש�A�t�A�G�A�  A֬A�O�A�bAմ9A�C�A��A�1A���A��`A�ĜA�x�A�\)A�O�A�C�A�;dA�5?A�-A�
=A��yA��#A��#A��
A���A���A��
A���A���A��
A��A���A���A���A�ȴAӼjAӲ-AӴ9AӬAӗ�AӋDA�|�A�hsA�C�A�/A�&�A�Aҟ�A�^5Aї�A���A���A�t�A�M�A�&�A�
=A���A��HA�ƨAϗ�A��A��yA�v�A��A���A���Aʹ9AͶFAʹ9A͇+A�S�A�/A�JA���A��A�ĜA̓uA�ZA�E�A�7LA��A�%A��A˸RA˝�A�r�A�9XA��A���A�A�A�bA�oA�JA�%A�1A�1A�A���A��A��`A��
Aʧ�A�&�A�A��A���A�n�A��A�hsA��#AǗ�A�n�A�`BA�ZA�5?A���A�5?A�VA�A���A��AžwAŬAş�AōPA�ZA�/Aĥ�A�t�A�hsA�hsA�XA�7LA�-A�&�A��A�A��A��`A��/A���A�ƨA�AþwAð!Aç�AÝ�AÑhA�|�A�hsA�Q�A�E�A�5?A�$�A��A��A�oA�
=A�  A�  A���A��A��A��A��yA��;A��
A¸RA�A�~�A�dZA�Q�A�/A�  A��`A��
A�ƨA���A��DA��A�z�A�n�A�VA�A�A�/A�"�A���A��mA��/A��#A���A��wA��FA��9A���A���A���A���A���A��DA��A�t�A�^5A�O�A�I�A�E�A�=qA�&�A���A���A���A��hA��A�dZA�7LA��A���A���A�Q�A�A���A���A��hA�p�A�bNA�M�A�1'A�
=A��A�|�A�^5A�S�A�"�A��yA���A�bNA�G�A���A��A���A�t�A�M�A�7LA��A�VA��A���A��^A��9A��-A���A���A��DA��A�n�A�ffA�\)A�$�A���A���A�1A�
=A�1A�VA��A��A��A��RA���A���A��DA�p�A�bNA�=qA�VA���A�Q�A�A�A���A�A��A���A�  A�A�JA��A��A��A� �A�$�A��A��/A���A�Q�A�/A�{A���A��A��`A��A��A�r�A�r�A�p�A�ffA�ZA�XA�I�A�C�A�9XA�33A�bA��yA��A��A�`BA�1A�A��hA��A�ZA�C�A�+A�VA��TA��^A��A��A�dZA�M�A�?}A�7LA��A��A�ĜA��+A�^5A�^5A�`BA�O�A�A�A�7LA��A�
=A���A��A��/A�ĜA��!A���A�r�A�^5A�-A���A��7A�p�A�?}A� �A�VA��^A��A�hsA�K�A�7LA��A���A��#A��9A��A��A��wA���A�S�A���A��-A���A��\A��DA��hA���A���A���A��\A��A��A�|�A�v�A�ffA�I�A�"�A��TA�ȴA��-A���A��7A�VA��A�A�z�A�jA�\)A�^5A�I�A�/A�1'A�5?A�;dA�;dA�;dA�;dA�=qA�;dA�9XA�7LA�33A�9XA�7LA�&�A��A��A�JA�
=A���A���A���A��jA���A���A��A�dZA�(�A��A��A��mA��HA���A�A��A���A��A�z�A� �A�oA��A��;A���A���A�ƨA��jA��A��uA�\)A�A�A�JA�  A��A��HA��9A�ZA���A��7A��A��`A���A��9A���A���A��7A�^5A�;dA�&�A���A��wA�jA��A��`A��\A�5?A�(�A� �A�%A��DA�JA��TA���A���A�(�A��A���A���A��-A�`BA�+A���A���A��A�XA�Q�A�A�A��A�{A���A��/A��jA���A�v�A�K�A�+A�VA���A��TA���A��uA�r�A�I�A�A�VA��/A��A��PA�ffA�(�A�oA�JA�  A��A��HA��A���A���A�ȴA���A��9A��A���A�r�A�{A��;A���A�r�A�A�A�-A��A�%A���A��A��mA��;A���A�A��FA���A���A��\A��A�|�A�v�A�p�A�bNA�G�A��;A�r�A���A�n�A�XA�XA�XA�XA�XA�VA�G�A�A�A��A�oA��A��#A���A�VA�-A�A��A�7LA�A��mA��#A���A�ȴA��wA���A��+A�x�A�l�A�\)A�K�A�E�A�C�A�;dA�{A���A���A���A�r�A�I�A�(�A��A�JA�A���A��mA�ƨA�~�A�7LA��wA��7A�p�A�=qA��A��A���A���A��7A�7LA�JA���A��`A���A��RA��7A�ffA�VA�C�A�1'A�A��;A��hA�;dA��A��/A���A�9XA���A��jA�p�A�O�A�;dA�"�A��A��RA��uA��A�dZA�K�A�
=A���A���A��DA�t�A�hsA�dZA�bNA�`BA�\)A�O�A�?}A�/A��A�1A��yA�ƨA���A�t�A�ffA�XA�+A�%A���A��+A�5?A�JA��A���A��jG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                              ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�B B4B�B B�B�B.BbB�B.B�B.B�B.B�BbB�B�B�B�B�B�B�B�B\BVB"BJB�B7BQNB�{B�tB�'B�B�B�]B�>B�BuB�BL0Bn�B�oB�	B�B�3B�UB�EB�mB��B��B�gB�gB��B�6Bh
B2�BB4�BwfBt�B�B�_B��B��B��B��B�~Bq�BffBe�BM�BJ�BM6BAUB2aB%�B'�B�B��B��B��B�jBB��B�FB��B�MB�=B�ABl�BgBT,BNB@�B/�B�BVB
��B
�lB
�5B
�HB
��B
�B
� B
�6B
��B
��B
�B
��B
��B
v�B
m]B
e�B
U2B
R�B
@OB
5?B
.�B
($B
"4B
�B
DB	��B	�B	�mB	�B	ںB	�B	�B	�<B	��B	�0B	��B	�hB	��B	�B	}�B	xB	v�B	h�B	T�B	PHB	N<B	IRB	E�B	AUB	?}B	<B	8�B	4nB	.�B	'�B	$@B	~B	�B	{B	@B	�B	GB��B��B��B��B�B�B�B��B�]B�,B�B�TB�
B�2B��B�B�QB�B��B��B�oB�/B�B�B�B��B�B��B�AB�AB�iB��B	 �B	�B	YB	�B	YB	$B	FB	�B	�B	�B	uB		7B	uB	#�B	!�B	=B	#:B	,=B	33B	:*B	<6B	@�B	?}B	=qB	<jB	@�B	LdB	NB	H�B	B�B	<6B	2�B	;dB	G�B	N�B	YB	e`B	d�B	e�B	o5B	q�B	qB	v�B	z�B	u�B	rGB	r�B	sMB	xB	xlB	~�B	�B	��B	�B	�B	��B	�:B	��B	��B	��B	�0B	��B	�IB	��B	��B	��B	��B	�-B	�3B	��B	�9B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�^B	�B	�HB	��B	��B	�[B	�aB	��B	�-B	��B	�UB	�OB	ƨB	�dB	�^B	�0B	�mB	��B	ĜB	ŢB	�B	��B	�KB	ΥB	��B	�9B	��B	��B	�/B	�WB	�/B	�B	�B	�fB	��B	�B	�B	��B	�B	�QB	�B	��B	��B	�B	�B	�B	�cB	��B	�)B	�B	��B	�B	��B	�)B	�iB	�B	��B	��B	�vB	�B	��B	�ZB	�`B	�2B	�fB	�fB	��B	��B	��B	��B	��B	�cB
 �B	�]B	��B	�(B	��B	��B	�cB
�B
MB
�B	��B	��B	��B	��B
�B
�B
+B
SB
�B
4B
�B
MB
�B
eB
�B
�B
�B
B
1B
�B
	B
B
~B
�B
�B
�B
 �B
!bB
!�B
!�B
!bB
!�B
$@B
$B
#nB
$�B
$@B
%B
&LB
&�B
'�B
($B
($B
(�B
*0B
)�B
*0B
*�B
+kB
+B
+�B
,=B
,=B
,qB
-B
,�B
,�B
,�B
,qB
.�B
-�B
.B
.�B
.}B
0!B
/�B
0!B
1�B
0�B
2aB
3hB
3hB
4B
4�B
5?B
5?B
5�B
5�B
7B
7LB
7�B
7�B
8RB
9�B
9$B
:*B
:^B
:�B
;�B
;�B
<�B
=<B
=�B
>B
>wB
>�B
?B
>�B
>�B
?�B
@�B
A�B
A�B
D3B
C�B
C�B
D3B
C�B
C�B
C�B
EB
E9B
E9B
EB
D�B
FtB
E�B
GB
HB
H�B
H�B
HB
HKB
IRB
H�B
IRB
I�B
I�B
I�B
IRB
I�B
I�B
J�B
J�B
K�B
K�B
K�B
L0B
L�B
M�B
N�B
N<B
NpB
M�B
N�B
OBB
PB
PHB
PHB
PHB
P�B
P�B
P�B
P�B
P�B
P�B
P�B
R�B
R�B
RTB
R�B
R�B
S�B
T,B
T,B
T,B
T�B
T�B
T�B
U�B
U�B
U�B
U�B
VmB
VmB
VmB
V9B
VmB
V9B
VB
V9B
V�B
W?B
W
B
W?B
W�B
W�B
W�B
XyB
YB
ZQB
Z�B
[#B
[#B
[WB
[�B
[WB
[#B
Z�B
ZQB
Z�B
Z�B
Z�B
[#B
\)B
\�B
\�B
\�B
\]B
\)B
\�B
\]B
\)B
\]B
\)B
\)B
\�B
\�B
\�B
^B
_B
_pB
_;B
_pB
_pB
_pB
`B
`�B
`�B
`vB
`B
_�B
_�B
_�B
`vB
`vB
`�B
aB
a|B
a|B
b�B
c B
cTB
c B
c�B
c�B
d�B
e�B
f2B
e�B
f2B
f2B
ffB
ffB
g8B
gB
f�B
g8B
g�B
h
B
h�B
iB
iyB
i�B
iyB
i�B
i�B
i�B
jB
jKB
jB
jB
jB
kB
kB
k�B
k�B
k�B
k�B
l�B
l�B
m)B
l�B
m�B
n�B
o5B
o�B
o�B
o�B
oiB
o�B
o�B
pB
poB
p�B
p�B
p�B
p�B
qB
qAB
qvB
qvB
qAB
q�B
q�B
q�B
q�B
q�B
rB
rGB
r�B
sMB
tB
tTB
tTB
tTB
t�B
u%B
uZB
uZB
uZB
uZB
uZB
u�B
uZB
uZB
u�B
v`B
v`B
v`B
v�B
v�B
v�B
w2B
v�B
v�B
w�B
wfB
xB
x8B
xlB
xlB
xlB
x�B
x�B
y�B
y�B
zB
zB
zxB
zDB
z�B
z�B
z�B
{B
|PB
|�B
}"B
}�B
~�B
~]B
~(B
~]B
~�B
~�B
~�B
�B
�B
� B
� B
�B
�;B
�B
�;B
�;B
�;B
�B
�AB
�AB
�uB
��B
��B
��B
�GB
��B
�B
��B
�%B
�YB
��B
�+B
��B
��B
�B
�	B
�B
��B
��B
��B
��B
��B
��B
�lB
�=B
�	B
��B
�rB
�rB
�=B
�DB
�xB
�DB
�xB
��B
��B
�B
��B
�B
�B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�.B
�.B
��B
��B
�hB
��B
�B
�:B
�:B
�B
�@B
�@B
�@B
�uB
�uB
�B
��B
�{B
�{B
��B
��B
�MB
��B
�B
��B
�B
��B
�B
��B
�B
��B
�$B
�$B
�$B
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
�_B
�_B
��B
��B
�eB
��B
��B
��B
��B
��B
�7B
�kB
�7B
�7B
�B
�B
��B
�	B
��B
��B
��B
��B
��B
�B
�CB
�B
�CB
�CB
�CB
��B
��B
��B
�xB
�IB
�~B
��B
�OB
�B
�B
��B
��B
��B
�!B
��B
�!B
�!B
�!B
��B
�VB
�VB
�VB
��B
��B
��B
��B
��B
��B
�-B
�bB
��B
��B
��B
��B
��B
��B
��B
�B
�B
�nB
�B
�zB
�zB
�zB
��B
��B
��B
��B
�B
�B
��B
�B
��B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�RB
��B
��B
��B
��B
��B
��B
��B
�$B
��B
�XB
��B
��B
��B
��B
�*B
�*B
�*B
�*B
�*B
�_B
��B
�0B
�eB
�0B
�0B
��B
�eB
��B
��B
�B
�6B
��B
�kB
��B
�=B
�=B
�B
��B
��B
��B
�B
�CB
�wB
��B
��B
�B
�IB
�IB
�IB
��B(B�B.B�B�BoB�B�B
�BoB�B:BbB:BbB�B4B�B(B�B:B@B�B(B�B B�B.B B�B\B�B�BbB�BbB�B.B�B�B4B(B�B B�B\B�B B B�B�B4B BbB�BB B�B�B�BhB(B4B�B(B�B B�B�B�B�B�B�BhB4B(B�BhBbB�B BhB.B�B�B�B�B�B B�B\B B�BVB�B�B�B�B\BbB�B�B�BVB�B�B�B�B.B�B�B.B B\B\B B�B�B B�BVBbB�B(B�B�B�BbB�B�B\B�B�B(B.B(B�B�BbB�BB�B�BVB�BVBVB�B�B�B�B�B�B�B�B�BB�B
�B�BB�B	�B�B
�B	�B�B�B�BB"hB�B&�B+kB/OB;�B7Bg�B_Bf�B~�Bp�BzDB{B�_B��B��B��B��B��B��B��B��B�^B��B��B��BÖB��B�mB�EB�B�}BٴB�B�B�NB�B�B��B�B��B�fB�B�B��B��B�)B�B�oB��B��B�rB�B��B��BB�.B�B�B	�B��BSB �B��B�B�BSB{B�BYB�B4B�BCB%zB,�B(XB2�B5�BIRBMBe�Bq�BtTBpoBm�Bp�Bh
Bu�Bl�Bn�Bp�ByrB�B�	B�kB��B��B�OB��B��B��B�!B��B�4B�:B�B�hB��B��B��B�B�@B��B�B��B�wB��B�[B��B�B�dB�pB�jB�}B�RB��B�jB�BB�KBÖB�'B�}B��BǮB�-B�[B�'B�BȀB՛B�EBŢBÖBƨB��BĜB�gB��BŢB�tB�9B��B�B�mBŢB��BŢB�B�B�9B�mB�B��B��B�gB�BĜBÖB��B�-B�3B��B��B�aB�aB��B� B��B��B��B��B�mB�B�B�zB��BƨB��B�3B�?B��B�gB�'B��B�?B�gBĜBB��B�B��B��B�mB�B�gB�aB��B�gB��B��B�3B��B��B�9B�3B�aB� B�B��B�9B��B�HB�FB��B�UB��B��B��B��B��B�B��B�4Bw2BpBtBg8Bk�Bl�Bb�Bl�BXBF�BGBI�B@BC�B3�B.�B49B'�B#:B!B!B�B=B�BB=B�B�B�B�B_B7B$BCB�B=B$@B*�B>BBJXBYB[�B[�B]/Bp�B��B�AB~(B}�Bw�Bx�Bu�Bt�Br|Bw�BgBj�BbNBbBo5B}�B��B��B�7B��B�B�B�PB��B��B�hB�hB�B��B��B�qB��B��B�_B�qB�B��B��B��B��B�YB�+B�'B�B�LB��B�_B��B�hB��B��B��B�4B��B�B�+B��B�uB��B�_B��B�!B�B��B�qB��B��B�!B��B��B��B�"B��B��B�PB��B��B�~B��B�\B��B�\B��B�4B��B�B�B��B��B��B�DB�PB�SB�1B��B�B�B|�Bx�Bt�Bw�Bs�Bm�Bj�Bs�BdZB`BBffBkBg�Bb�Bb�B`�BgmBbBh
Bg�BiBi�BgmBe,Bf�Bi�Bj�Be�BdZB^�B\�BZ�BY�BX�BT�BYBFB@�BA B@OBF�BEmBGBI�BI�BI�BJ�BN<BMBMjBM�BOBOBBLdBL�BPHBK^BJ#BO�BIBPBGBQ�BG�BC�BA�B>B@OBL�B4�B.IB5�B6B5B2�B5?B0�B.IB/�B(�B*eB+�B*0B#�B$tB#�B%FB"�B%FB(�B#�B)�B#nB 'B!�B!-B)_B5B49B$�B�B�B+B\B�B@BBbB�B~B�B.B�B�(B��B��B�mB�sB�fB  B�yBޞB��B�B�BߤB�#B�B�B�BٴB�B��B՛B��BϫB�B�}B�dB�aB�BбBɺBǮBȀB�mB�OB�B��B�-B�B�B�dB�B�UB��B��B��B��B�CB��B�tB��B�LB��B�B�:B��B�B��B�-B�!B�!B��B��B�B��B�\B�+B�SB��B�.B�(B�.B��B�VB��B�~B�~B�xB��B��B�fB�YB�MB�B�B��B�VB�DB��Br�BsMBncBm]Bm)BkQBk�Bm�Bl"BiDBi�Bd&B^5Be`BkBb�Bc BkBj�B^jBV�BT�BR�BS�BS�BYBS�BP}BN�BR�BPHBN<BI�BK�BJ�BK�BP�BHBIRBGBA�B?HB@�B>BB:�B<jB9XB;�B@�BE�B-�B-�B/�B(�B$@B&LB*eB0UB%�BxB7B�B$BBoB+B�B�BxB�B�B�B�B	B�B�B\BB�BB
��B
��B
��B
��B
��B
��B
�2B
�B
��B  B
��B
��B
��B
�2B
��B
�B
�B
�;B
�)B
� B
�TB
�B
�)B
��B
�vB
��B
�B
�NB
�NB
��B
�ZB
�QB
�NB
��B
�B
�vB
ΥB
˒B
ɺG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                              G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                              G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2021072719352320210727193523IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021080700005820210807000058QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021080700005820210807000058QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365120220126093651IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295320220204232953IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295320220204232953IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295320220204232953IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                