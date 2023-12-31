CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-09-12T13:30:14Z creation; 2023-04-26T19:14:30Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  dh   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �H   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  Ҩ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � hH   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20200912133014  20230426191430  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               b   bAA  AOAO7316_008644_098                 7316_008644_098                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�7�w�r�@�7�w�r�11  @�7����@�7����@'IrGE8�@'IrGE8��c�q6� ��c�q6� �11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?aG�?��H@@  @�G�@�  @��R@�  AG�A��A\)A,(�AAG�Aa�A���A�  A�  A�  A�  A�Q�A�  A�  B (�B(�B(�B�
B   B'�
B0  B8(�B@(�BH(�BP  BX(�B`(�Bh  Bp  Bw�
B�B�  B�(�B�(�B�=qB�  B�  B��B�  B�{B�{B�  B��B��B�  B�{B�{B�  B�  B��B��B��B�  B�  B�  B��B�  B�  B��B��B�  B�  C   C  C  C  C��C	��C  C��C  C
=C  C  C  C��C��C��C   C"  C$  C&
=C'��C)��C,  C.  C0  C2{C4
=C5��C8  C9��C<  C>  C?��CA��CD  CF
=CH
=CI��CK��CN  CO��CQ�CT  CV  CW��CZ  C\  C^  C_��Cb  Cd
=Cf  Cg��Cj  Cl  Cm��Co��Cq��Cs�Cu�Cw��Cz
=C|  C~  C�C�
=C�  C���C�  C�  C�  C�  C�  C�  C���C�  C�  C�  C�C�  C�
=C�  C�  C�C�
=C�
=C�C�  C���C�C�
=C�
=C�C�  C���C�C�C���C�  C�C�  C�C�
=C�  C�  C�  C���C�  C�
=C�C�  C�  C�C�C�C�C���C���C���C���C�C�
=C�C�  C�C�  C���C�  C�  C���C���C�  C�
=C�C�  C�  C�  C�  C�  C�  C�C�
=C�  C���C�  C�  C���C�  C�C���C�  C�  C�C�C�  C���C�C�
=C�C�C�
=C�
=C�C�C�  C�C�C�  C�  C�  C�C���C���C���C���C���C�  C�  C���C���C�  C�  C�C���C�  C�  C���C�  C�C�  C���C�  D D ��D  D� D  D� D�qD}qD�qD}qD�qDz�D��D� D�D� D�D��D�qD	}qD	�qD
}qD  D��DD� D��D}qD  D��DD� D�qD� D  D��DD��D�qD� D  D�D  D}qD  D��D  D}qD�qDz�D  D}qD�qD}qD  D}qD�qD��D�Dz�D�qD}qD  D� D   D � D!  D!� D"  D"}qD"�qD#}qD$  D$� D%  D%� D%�qD&��D'�D'}qD'�qD(��D)�D)}qD)�qD*z�D*��D+��D,�D,��D-�D-��D.D.}qD/  D/�D0�D0��D1�D1}qD1��D2� D2�qD3}qD4�D4� D5  D5� D5�qD6}qD6�qD7��D8  D8� D9D9��D:  D:� D:�qD;��D;�qD<}qD=  D=� D=�qD>��D?  D?� D@�D@��DA  DA}qDB  DB��DC  DC}qDD  DD}qDD�qDE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM�DM��DN�DN� DN�qDO}qDO�qDP}qDQ�DQ��DR  DR}qDS  DS� DT  DT� DU  DU� DV  DV� DW  DW��DX  DX� DY  DY� DY�qDZ� D[  D[� D\  D\��D]  D]� D^�D^� D_  D_� D_�qD`}qD`�qDa}qDb  Db�Dc�Dc� Dd  Dd}qDd�qDe��Df�Df� Dg�Dg� Dh  Dh� Dh�qDi� Dj  Dj}qDj�qDk}qDk�qDl� Dm  Dm� Dn  Dn� Do�Do� Do�qDp��Dq�Dq}qDq�qDr}qDs  Ds��Dt  Dt��Du�Du}qDu�qDv}qDw  Dw��Dx�Dx� Dx�qDy� Dz  Dz}qD{�D{� D|  D|� D}  D}��D~D~�D  D��D�  D�>�D�� D��HD��D�AHD�� D���D�  D�AHD��HD���D��)D�<)D�|)D��)D�  D�@ D�~�D���D���D�@ D�� D��HD���D�=qD�~�D��HD���D�>�D�� D��HD��D�AHD�� D���D��qD�>�D�� D�� D�HD�B�D��HD�� D�  D�>�D��HD��HD�  D�@ D�~�D��HD�  D�@ D��HD�� D�  D�>�D�~�D�� D��D�AHD��HD��HD�  D�AHD�� D�� D��D�AHD�� D�D�HD�AHD�� D���D�HD�B�D��HD�� D���D�>�D�� D��HD��D�@ D�~�D�� D�HD�@ D�~�D���D�  D�AHD��HD�� D���D�@ D�� D�� D�  D�AHD��HD�� D���D�@ D�� D��HD�HD�AHD���D�� D���D�@ D�� D��HD���D�<)D�� D��HD�HD�AHD��HD�D��D�C�D��HD���D�  D�@ D�� D�� D���D�>�D�� D��HD�HD�B�D��HD���D�  D�@ D��HD��HD�  D�@ D�~�D���D���D�@ D��HD�� D�  D�@ D�� D���D�  D�@ D�� D��HD�HD�@ D�� D��HD�  D�@ D��HD���D���D�@ D�~�D�� D�HD�@ D��HD��HD�HD�>�D�~�D��qD���D�@ D�}qD���D�  D�>�D��HD��HD��qD�=qD��HD��HD��qD�=qD�}qD��qD��qD�=qD�~�D���D�  D�@ D�� D�� D�  D�@ D��HD���D��qD�=qD�~�D��HD���D�>�D�~�D�� D�HD�AHD�� D��qD�  D�@ D��HD�D�  D�>�D�� D�� D�HD�@ D�~�D��HD�  D�>�D�~�D���D�  D�B�D��HD���D���D�AHD��HD�� D�  D�@ D�� D���D�  D�AHD��HD��HD�HD�AHD�� D�� D�HD�@ D�� D��HD�HD�@ D�� D��HD��D�AHDHD�� D�  D�@ D�~�D�� D�  D�>�DāHD�� D���D�@ Dŀ Dž�D���D�>�DƁHD��HD�HD�AHDǀ DǾ�D���D�>�DȀ D��HD�HD�@ Dɀ D��HD�  D�>�D�~�Dʾ�D���D�>�Dˀ D�� D�  D�@ D�~�D�� D�  D�@ D́HD��HD�  D�@ D΁HDξ�D�  D�AHDρHD��HD�  D�@ DЀ D�� D�  D�@ DсHD��HD�  D�>�DҀ D�� D�  D�AHDӁHD�D�HD�@ DԀ D�� D�  D�@ DՀ Dվ�D���D�>�DցHD�� D���D�@ D�~�D�� D�HD�@ D�~�D��HD�HD�AHDفHD��HD�HD�@ D�~�D�� D�  D�@ D�~�D۾�D���D�>�D܀ D�� D�HD�AHD݀ Dݾ�D�  D�AHDހ D޾�D�HD�@ D߀ D��HD�HD�AHD��HD�� D�  D�@ D� D�� D���D�>�D�~�D�qD���D�>�D�~�D�� D�  D�@ D�HD�D�HD�@ D� D徸D��qD�=qD�~�D��HD�  D�>�D�HD��HD���D�>�D�~�D��HD�HD�>�D�~�D龸D�  D�@ D� D꾸D��qD�=qD�~�D뾸D�  D�AHD�~�D쾸D�  D�@ D�~�D�� D�HD�AHD� DD��qD�>�D�HD�� D�  D�AHD��HD�� D���D�@ D� D�� D�  D�@ D� D�qD���D�AHD� D�� D���D�>�D� D�� D���D�>�D�~�D��HD�HD�AHD�~�D�� D��D�@ D�~�D�� D�HD�B�D�~�D���D���D�@ D��HD�� D�HD�(�D�e?�?#�
?�  ?��
?�
=@�@z�@0��@G�@^�R@u@��@�z�@��R@��@�Q�@�ff@��@޸R@�{@�(�Az�A
�HA�A��A\)A%�A+�A2�\A8��A?\)AEAL��AS33AXQ�A^�RAe�Aj�HAp  AuAz�HA\)A�=qA���A��RA���A��\A��A�
=A���A�33A�p�A�\)A���A�33A�p�A�\)A�G�A��A�A��A���A��A�{A��A���A��
A�ffA�Q�A�=qA�z�A��RA���A\A��A�
=A�Q�Aʏ\A��AθRAУ�A��HA��AָRAأ�Aڏ\A���A޸RA�Q�A�=qA�z�A�ffA�  A�=qA�z�A�{A�  A��A�(�A�p�A�\)A���A��A��A��RB z�BG�B{B�HB�
B��Bp�BffB33B  B��B	B
�\B33B  B��B��B{B�HB�
B��B�BB�\B�B(�B��BG�B{B�HB\)B  B��B�B�\B33B�
B��BBffB33B (�B!G�B"{B"�HB#�
B$��B%�B&�HB'�B(��B*{B+33B,Q�B-G�B.=qB/\)B0��B1�B2�HB3�
B4��B6=qB7�B8��B9�B;
=B<(�B=��B?
=B@(�BAp�BB�RBD  BE�BFffBG�
BIG�BJffBK�BL��BN=qBO�BQ�BR�\BS�BU�BV=qBW�BY�BZ�\B\  B]p�B^�RB`(�BaG�Bb�RBd(�Be��Bg
=Bhz�BiBk33Blz�BmBo33Bp��Br=qBs�
BuG�Bv�HBxQ�By�B{�B|��B~ffB�
B��RB���B�ffB�G�B�{B��HB���B�ffB��B��B���B�p�B�(�B���B��B�ffB�
=B��B�=qB���B�33B��B�(�B��\B�
=B�\)B�B�(�B�z�B���B��B�\)B��B��B�=qB��\B��HB�33B��B��B�=qB��\B��HB�33B��B��
B�{B�ffB���B���B�G�B���B��B�=qB���B�
=B�\)B�B�  B�Q�B��RB�
=B�\)B�B�{B�z�B���B�33B��B��
B�(�B�z�B���B�33B�p�B�B�{B�ffB��RB���B�G�B���B��B�(�B��\B���B�33B�p�B�B�{B�ffB��RB�
=B�\)B��B�  B�=qB��\B��HB�33B�p�B��B�  B�Q�B��\B��HB�33B��B��
B�(�B�z�B���B�33B��B��
B�=qB��\B���B�G�B���B��B�Q�B���B�
=B�p�B�B�(�B��\B��HB�G�B��B�  B�z�B��HB�33B��B��B�=qB���B�
=B�p�B��
B�=qB���B���B�\)B�B�=qB���B�
=B�p�B��
B�=qB��RB��B��B��B�Q�B���B�33B���B�{B�z�B��HB�G�B�B�(�B£�B��BÅB��B�ffB���B�\)B�B�=qBƸRB�33BǮB�=qBȣ�B��BɮB�(�Bʣ�B��B˙�B�(�Ḅ�B��B͙�B�(�BΣ�B��Bϙ�B�(�BЏ\B��Bљ�B�{Bҏ\B�
=BӅB�  Bԏ\B�
=BՅB�  B֏\B��Bי�B�(�Bأ�B�33B�B�=qB���B�\)B��
B�Q�B��HB�\)B��
B�ffB��HB�\)B��
B�Q�B���B�G�B��
B�ffB��HB�p�B��B�z�B���B�B�  B�z�B�
=B�B�  B�z�B���B�p�B�  B�z�B���B�p�B��B�z�B���B�p�B�  B�\B��BB�{B��\B��B�B�{B�\B���B�B��B�z�B��HB�\)B��
B�=qB���B�G�B�B�Q�B��RB�G�B�B�=qB��RB��B���B�{B���B��B���B�(�B���B�33B��C {C Q�C �\C ��C
=CQ�C��C�HC(�CffC��C�C33Cp�C�RC  CG�C�\C�
C(�CffC�RC�C33Cz�CC{C\)C��C�C33Cp�C�RC	  C	Q�C	��C	�C
=qC
�C
��C{C\)C�C��CQ�C��C�C33C�C��C{CffCC{CffC�RC  CG�C��C�HC=qC�\C�HC33C�C�
C�Cp�C�RC{CffC�RC
=CffC�RC  CG�C��C��CQ�C��C��C=qC�\C�HC33C�C�HC33C�\C�HC33C�\C�HC(�Cz�C�HC=qC��C�C=qC�\C�HC33C�\C�C =qC ��C �C!=qC!�\C!�C"=qC"��C"��C#G�C#��C#�C$=qC$�\C$�C%G�C%��C%��C&Q�C&��C&�C'G�C'��C'��C(\)C(�C)
=C)\)C)��C)��C*Q�C*�RC+
=C+p�C+��C,{C,p�C,C-{C-p�C-��C.33C.�\C.�HC/=qC/�\C/�HC0=qC0��C0��C1Q�C1�C2  C2G�C2��C3  C3\)C3�RC4{C4ffC4�RC5
=C5\)C5�RC6  C6ffC6�RC7�C7p�C7C8
=C8Q�C8�C9  C9ffC9�RC:{C:ffC:�C;
=C;Q�C;��C;��C<Q�C<��C=  C=Q�C=��C=�C>33C>�\C>�C?=qC?�\C?�HC@33C@z�C@��CA�CAz�CA��CB(�CBp�CB�RCC
=CC\)CC��CD  CDQ�CD��CD��CEG�CE��CE�HCF33CFz�CF�
CG33CG�CG�
CH(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                            ?aG�?��H@@  @�G�@�  @��R@�  AG�A��A\)A,(�AAG�Aa�A���A�  A�  A�  A�  A�Q�A�  A�  B (�B(�B(�B�
B   B'�
B0  B8(�B@(�BH(�BP  BX(�B`(�Bh  Bp  Bw�
B�B�  B�(�B�(�B�=qB�  B�  B��B�  B�{B�{B�  B��B��B�  B�{B�{B�  B�  B��B��B��B�  B�  B�  B��B�  B�  B��B��B�  B�  C   C  C  C  C��C	��C  C��C  C
=C  C  C  C��C��C��C   C"  C$  C&
=C'��C)��C,  C.  C0  C2{C4
=C5��C8  C9��C<  C>  C?��CA��CD  CF
=CH
=CI��CK��CN  CO��CQ�CT  CV  CW��CZ  C\  C^  C_��Cb  Cd
=Cf  Cg��Cj  Cl  Cm��Co��Cq��Cs�Cu�Cw��Cz
=C|  C~  C�C�
=C�  C���C�  C�  C�  C�  C�  C�  C���C�  C�  C�  C�C�  C�
=C�  C�  C�C�
=C�
=C�C�  C���C�C�
=C�
=C�C�  C���C�C�C���C�  C�C�  C�C�
=C�  C�  C�  C���C�  C�
=C�C�  C�  C�C�C�C�C���C���C���C���C�C�
=C�C�  C�C�  C���C�  C�  C���C���C�  C�
=C�C�  C�  C�  C�  C�  C�  C�C�
=C�  C���C�  C�  C���C�  C�C���C�  C�  C�C�C�  C���C�C�
=C�C�C�
=C�
=C�C�C�  C�C�C�  C�  C�  C�C���C���C���C���C���C�  C�  C���C���C�  C�  C�C���C�  C�  C���C�  C�C�  C���C�  D D ��D  D� D  D� D�qD}qD�qD}qD�qDz�D��D� D�D� D�D��D�qD	}qD	�qD
}qD  D��DD� D��D}qD  D��DD� D�qD� D  D��DD��D�qD� D  D�D  D}qD  D��D  D}qD�qDz�D  D}qD�qD}qD  D}qD�qD��D�Dz�D�qD}qD  D� D   D � D!  D!� D"  D"}qD"�qD#}qD$  D$� D%  D%� D%�qD&��D'�D'}qD'�qD(��D)�D)}qD)�qD*z�D*��D+��D,�D,��D-�D-��D.D.}qD/  D/�D0�D0��D1�D1}qD1��D2� D2�qD3}qD4�D4� D5  D5� D5�qD6}qD6�qD7��D8  D8� D9D9��D:  D:� D:�qD;��D;�qD<}qD=  D=� D=�qD>��D?  D?� D@�D@��DA  DA}qDB  DB��DC  DC}qDD  DD}qDD�qDE� DF  DF� DG  DG� DH  DH� DI  DI� DJ  DJ� DK  DK� DL  DL� DM�DM��DN�DN� DN�qDO}qDO�qDP}qDQ�DQ��DR  DR}qDS  DS� DT  DT� DU  DU� DV  DV� DW  DW��DX  DX� DY  DY� DY�qDZ� D[  D[� D\  D\��D]  D]� D^�D^� D_  D_� D_�qD`}qD`�qDa}qDb  Db�Dc�Dc� Dd  Dd}qDd�qDe��Df�Df� Dg�Dg� Dh  Dh� Dh�qDi� Dj  Dj}qDj�qDk}qDk�qDl� Dm  Dm� Dn  Dn� Do�Do� Do�qDp��Dq�Dq}qDq�qDr}qDs  Ds��Dt  Dt��Du�Du}qDu�qDv}qDw  Dw��Dx�Dx� Dx�qDy� Dz  Dz}qD{�D{� D|  D|� D}  D}��D~D~�D  D��D�  D�>�D�� D��HD��D�AHD�� D���D�  D�AHD��HD���D��)D�<)D�|)D��)D�  D�@ D�~�D���D���D�@ D�� D��HD���D�=qD�~�D��HD���D�>�D�� D��HD��D�AHD�� D���D��qD�>�D�� D�� D�HD�B�D��HD�� D�  D�>�D��HD��HD�  D�@ D�~�D��HD�  D�@ D��HD�� D�  D�>�D�~�D�� D��D�AHD��HD��HD�  D�AHD�� D�� D��D�AHD�� D�D�HD�AHD�� D���D�HD�B�D��HD�� D���D�>�D�� D��HD��D�@ D�~�D�� D�HD�@ D�~�D���D�  D�AHD��HD�� D���D�@ D�� D�� D�  D�AHD��HD�� D���D�@ D�� D��HD�HD�AHD���D�� D���D�@ D�� D��HD���D�<)D�� D��HD�HD�AHD��HD�D��D�C�D��HD���D�  D�@ D�� D�� D���D�>�D�� D��HD�HD�B�D��HD���D�  D�@ D��HD��HD�  D�@ D�~�D���D���D�@ D��HD�� D�  D�@ D�� D���D�  D�@ D�� D��HD�HD�@ D�� D��HD�  D�@ D��HD���D���D�@ D�~�D�� D�HD�@ D��HD��HD�HD�>�D�~�D��qD���D�@ D�}qD���D�  D�>�D��HD��HD��qD�=qD��HD��HD��qD�=qD�}qD��qD��qD�=qD�~�D���D�  D�@ D�� D�� D�  D�@ D��HD���D��qD�=qD�~�D��HD���D�>�D�~�D�� D�HD�AHD�� D��qD�  D�@ D��HD�D�  D�>�D�� D�� D�HD�@ D�~�D��HD�  D�>�D�~�D���D�  D�B�D��HD���D���D�AHD��HD�� D�  D�@ D�� D���D�  D�AHD��HD��HD�HD�AHD�� D�� D�HD�@ D�� D��HD�HD�@ D�� D��HD��D�AHDHD�� D�  D�@ D�~�D�� D�  D�>�DāHD�� D���D�@ Dŀ Dž�D���D�>�DƁHD��HD�HD�AHDǀ DǾ�D���D�>�DȀ D��HD�HD�@ Dɀ D��HD�  D�>�D�~�Dʾ�D���D�>�Dˀ D�� D�  D�@ D�~�D�� D�  D�@ D́HD��HD�  D�@ D΁HDξ�D�  D�AHDρHD��HD�  D�@ DЀ D�� D�  D�@ DсHD��HD�  D�>�DҀ D�� D�  D�AHDӁHD�D�HD�@ DԀ D�� D�  D�@ DՀ Dվ�D���D�>�DցHD�� D���D�@ D�~�D�� D�HD�@ D�~�D��HD�HD�AHDفHD��HD�HD�@ D�~�D�� D�  D�@ D�~�D۾�D���D�>�D܀ D�� D�HD�AHD݀ Dݾ�D�  D�AHDހ D޾�D�HD�@ D߀ D��HD�HD�AHD��HD�� D�  D�@ D� D�� D���D�>�D�~�D�qD���D�>�D�~�D�� D�  D�@ D�HD�D�HD�@ D� D徸D��qD�=qD�~�D��HD�  D�>�D�HD��HD���D�>�D�~�D��HD�HD�>�D�~�D龸D�  D�@ D� D꾸D��qD�=qD�~�D뾸D�  D�AHD�~�D쾸D�  D�@ D�~�D�� D�HD�AHD� DD��qD�>�D�HD�� D�  D�AHD��HD�� D���D�@ D� D�� D�  D�@ D� D�qD���D�AHD� D�� D���D�>�D� D�� D���D�>�D�~�D��HD�HD�AHD�~�D�� D��D�@ D�~�D�� D�HD�B�D�~�D���D���D�@ D��HD�� D�HD�(�G�O�?�?#�
?�  ?��
?�
=@�@z�@0��@G�@^�R@u@��@�z�@��R@��@�Q�@�ff@��@޸R@�{@�(�Az�A
�HA�A��A\)A%�A+�A2�\A8��A?\)AEAL��AS33AXQ�A^�RAe�Aj�HAp  AuAz�HA\)A�=qA���A��RA���A��\A��A�
=A���A�33A�p�A�\)A���A�33A�p�A�\)A�G�A��A�A��A���A��A�{A��A���A��
A�ffA�Q�A�=qA�z�A��RA���A\A��A�
=A�Q�Aʏ\A��AθRAУ�A��HA��AָRAأ�Aڏ\A���A޸RA�Q�A�=qA�z�A�ffA�  A�=qA�z�A�{A�  A��A�(�A�p�A�\)A���A��A��A��RB z�BG�B{B�HB�
B��Bp�BffB33B  B��B	B
�\B33B  B��B��B{B�HB�
B��B�BB�\B�B(�B��BG�B{B�HB\)B  B��B�B�\B33B�
B��BBffB33B (�B!G�B"{B"�HB#�
B$��B%�B&�HB'�B(��B*{B+33B,Q�B-G�B.=qB/\)B0��B1�B2�HB3�
B4��B6=qB7�B8��B9�B;
=B<(�B=��B?
=B@(�BAp�BB�RBD  BE�BFffBG�
BIG�BJffBK�BL��BN=qBO�BQ�BR�\BS�BU�BV=qBW�BY�BZ�\B\  B]p�B^�RB`(�BaG�Bb�RBd(�Be��Bg
=Bhz�BiBk33Blz�BmBo33Bp��Br=qBs�
BuG�Bv�HBxQ�By�B{�B|��B~ffB�
B��RB���B�ffB�G�B�{B��HB���B�ffB��B��B���B�p�B�(�B���B��B�ffB�
=B��B�=qB���B�33B��B�(�B��\B�
=B�\)B�B�(�B�z�B���B��B�\)B��B��B�=qB��\B��HB�33B��B��B�=qB��\B��HB�33B��B��
B�{B�ffB���B���B�G�B���B��B�=qB���B�
=B�\)B�B�  B�Q�B��RB�
=B�\)B�B�{B�z�B���B�33B��B��
B�(�B�z�B���B�33B�p�B�B�{B�ffB��RB���B�G�B���B��B�(�B��\B���B�33B�p�B�B�{B�ffB��RB�
=B�\)B��B�  B�=qB��\B��HB�33B�p�B��B�  B�Q�B��\B��HB�33B��B��
B�(�B�z�B���B�33B��B��
B�=qB��\B���B�G�B���B��B�Q�B���B�
=B�p�B�B�(�B��\B��HB�G�B��B�  B�z�B��HB�33B��B��B�=qB���B�
=B�p�B��
B�=qB���B���B�\)B�B�=qB���B�
=B�p�B��
B�=qB��RB��B��B��B�Q�B���B�33B���B�{B�z�B��HB�G�B�B�(�B£�B��BÅB��B�ffB���B�\)B�B�=qBƸRB�33BǮB�=qBȣ�B��BɮB�(�Bʣ�B��B˙�B�(�Ḅ�B��B͙�B�(�BΣ�B��Bϙ�B�(�BЏ\B��Bљ�B�{Bҏ\B�
=BӅB�  Bԏ\B�
=BՅB�  B֏\B��Bי�B�(�Bأ�B�33B�B�=qB���B�\)B��
B�Q�B��HB�\)B��
B�ffB��HB�\)B��
B�Q�B���B�G�B��
B�ffB��HB�p�B��B�z�B���B�B�  B�z�B�
=B�B�  B�z�B���B�p�B�  B�z�B���B�p�B��B�z�B���B�p�B�  B�\B��BB�{B��\B��B�B�{B�\B���B�B��B�z�B��HB�\)B��
B�=qB���B�G�B�B�Q�B��RB�G�B�B�=qB��RB��B���B�{B���B��B���B�(�B���B�33B��C {C Q�C �\C ��C
=CQ�C��C�HC(�CffC��C�C33Cp�C�RC  CG�C�\C�
C(�CffC�RC�C33Cz�CC{C\)C��C�C33Cp�C�RC	  C	Q�C	��C	�C
=qC
�C
��C{C\)C�C��CQ�C��C�C33C�C��C{CffCC{CffC�RC  CG�C��C�HC=qC�\C�HC33C�C�
C�Cp�C�RC{CffC�RC
=CffC�RC  CG�C��C��CQ�C��C��C=qC�\C�HC33C�C�HC33C�\C�HC33C�\C�HC(�Cz�C�HC=qC��C�C=qC�\C�HC33C�\C�C =qC ��C �C!=qC!�\C!�C"=qC"��C"��C#G�C#��C#�C$=qC$�\C$�C%G�C%��C%��C&Q�C&��C&�C'G�C'��C'��C(\)C(�C)
=C)\)C)��C)��C*Q�C*�RC+
=C+p�C+��C,{C,p�C,C-{C-p�C-��C.33C.�\C.�HC/=qC/�\C/�HC0=qC0��C0��C1Q�C1�C2  C2G�C2��C3  C3\)C3�RC4{C4ffC4�RC5
=C5\)C5�RC6  C6ffC6�RC7�C7p�C7C8
=C8Q�C8�C9  C9ffC9�RC:{C:ffC:�C;
=C;Q�C;��C;��C<Q�C<��C=  C=Q�C=��C=�C>33C>�\C>�C?=qC?�\C?�HC@33C@z�C@��CA�CAz�CA��CB(�CBp�CB�RCC
=CC\)CC��CD  CDQ�CD��CD��CEG�CE��CE�HCF33CFz�CF�
CG33CG�CG�
CH(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                            @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�2@�NG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A���A���A���A���A��A��A��A��yA��A��TA��#A�ȴA߾wAߴ9Aߝ�A߉7A�z�A�r�A�bNA�A�A���AݮA���A�|�A�$�A���A�"�A�  A�A�A��A��FA�A���A��A�r�A���A��A��A�?}A�O�A�l�A��^A�K�A�9XA�t�A��^A�K�A��A�XA{�Av  Ao�7Ae��AaƨA]�A[p�AXr�AR��AP�AK�7AF�AC�PAB��AA�mAA33A>  A<�!A:��A:�A933A7��A6�A6M�A6 �A6bA5�A5��A4��A37LA2�\A2bA1C�A0�9A05?A/�mA.��A.(�A-�mA-��A-
=A,VA,bA+��A+K�A+%A*��A*Q�A(�+A'oA$ffA#;dA"��A!C�A!O�A!\)A!�TA!��A!oA ��A"bA!�A!��A ȴA�mA�mA�A�wAXAĜAVA��AK�A��A1A�A�DA��AC�A�\A��Ap�A�AQ�AZA  A�A��A��A5?A��A��A��Ax�A`BAO�A��AQ�A��A��A�A33A��A^5A �A  AAhsA\)AO�A
=A�uA(�A�^AK�A�!AQ�A�-Ap�A;dA
��A
ȴA
~�A
-A	��A	�FA	p�A	
=A��Av�A�^Ax�AO�AoAA�yA�AAA\)A7LA�HA�uAbNAJA��A;dA�A�9A��Az�AA�A{A��A�A��A�A �RA ��A ZA VA M�A -A b@��F@��H@��\@��T@��@���@�Ĝ@�Q�@�b@���@��@���@��j@�(�@���@�@���@�{@���@���@�x�@��@��j@�\)@�~�@�$�@���@�X@��@�dZ@��@�-@�7@��@��@�z�@�9X@�P@��H@��@�/@�V@���@�bN@��@��
@�;d@��@�v�@�@䛦@�P@⟾@��#@߾w@ޏ\@�/@��
@�"�@�ff@�J@�O�@�j@�1'@�Q�@���@ְ!@��@���@�G�@Լj@��@�"�@�$�@�%@�j@Ο�@͉7@̃@˕�@���@�~�@�@ɲ-@�V@�bN@��@�|�@�o@�~�@�J@Ų-@�hs@�7L@ģ�@�Z@ÍP@¸R@�E�@���@���@���@�x�@�`B@��@�z�@�1@�1@�b@��F@���@�K�@���@��T@��-@��@��9@��F@�@���@�^5@�{@��h@�?}@�?}@�?}@�&�@��u@�Z@��;@�"�@��@���@�~�@���@�X@��@��/@���@���@��D@�j@���@�+@���@��\@�^5@�-@�$�@�V@�M�@��T@���@�r�@�Q�@��@�
=@�-@���@�hs@��@�Ĝ@���@�S�@���@�-@�@�`B@��u@��;@��@�@�=q@�@���@��@�1'@���@��@�K�@���@��R@���@�v�@��@��#@���@�hs@�X@��@��@���@��9@�r�@��m@��@���@���@��h@���@�A�@���@���@��;@��w@�|�@�
=@��!@���@��+@�v�@�n�@���@��@�1@�S�@��@��y@���@�v�@�E�@�{@��@�{@�{@��/@���@�z�@�b@��@�ƨ@��F@���@�|�@�S�@���@�V@�5?@��@�J@��#@��@�V@���@��@��u@�bN@�  @�ƨ@��@���@���@�|�@�\)@�C�@�+@��R@��+@�=q@���@��@�@�O�@��@�V@���@���@�j@��w@�l�@�
=@��R@�v�@���@�p�@�/@���@��u@�Q�@��
@��@�S�@�
=@���@�~�@�J@��#@���@���@��@��/@�Z@�b@�ƨ@��F@���@�|�@�l�@�l�@�dZ@�dZ@�S�@�33@�@��!@�~�@�-@��T@��^@�hs@���@�Ĝ@�Q�@�@�P@~�y@~v�@}�T@}O�@}/@|�D@|Z@|9X@{�m@{"�@z�!@zM�@z=q@z-@y��@yx�@y&�@y�@y%@x�`@x1'@w\)@w�@v�@v�R@vv�@v5?@u�-@uO�@t��@s�m@st�@s@r�@r��@r�\@r~�@r=q@q�@q�^@q��@p��@pr�@pb@o;d@o�@n��@n��@nE�@n5?@m��@mp�@m`B@mV@lj@l(�@k��@k��@kC�@j��@jM�@i��@i7L@h�9@hr�@h1'@g��@g�@g\)@f��@f�y@f�@f�@fȴ@f��@fv�@e�@eV@d�@d9X@c��@c�
@ct�@b�!@bn�@b-@a��@a��@aX@`��@`A�@_��@_|�@_;d@_
=@^�y@^�R@^v�@^@]��@]O�@\Z@[�
@Z�@Y��@Y�@X�9@XQ�@WK�@V��@Vȴ@V�+@VV@U�@U@T��@S�m@S�m@S�
@S�m@S�
@R�H@R-@RJ@Q�@QX@P�`@P�u@PbN@Ol�@N�+@N5?@M�T@M�@MO�@M?}@L�@L�j@L�j@L�j@L��@L�j@L�@L9X@K�
@KdZ@J=q@Ix�@IX@IG�@I%@G��@Gl�@GK�@G�@Fȴ@Fff@FE�@E�@E�-@EO�@E�@D��@D(�@C�
@CS�@CC�@C33@Co@B��@B��@Bn�@B-@A��@A�7@@��@@Q�@?�P@?+@?�@?
=@>�y@>�R@>��@>��@>ff@=�@=��@=O�@=/@<��@<�@;ƨ@:��@:^5@9��@9��@9��@9G�@9%@8��@8A�@8 �@7�@7�w@7+@6��@6�@6��@6v�@6V@65?@6{@5�T@5�@5O�@5?}@5�@4�@4��@4��@4�D@4j@4(�@3��@3ƨ@3��@3dZ@3C�@3C�@333@333@333@3@2�H@1�@1X@1%@0��@0Ĝ@0bN@/�@/K�@/�@.�y@.�R@.v�@.ff@.$�@.@-�@-@-�@-V@,�@,��@,��@,j@,(�@+��@+C�@*��@*n�@*=q@)�@)�7@)G�@)�@(��@(r�@(  @'��@'|�@'K�@'
=@&�+@&ff@&$�@%O�@$��@$j@$j@$Z@$I�@$(�@#�
@#��@#t�@#C�@#@"��@"��@"=q@"J@!�@!�^@!��@!x�@!&�@!%@ �`@ ��@ �9@ �u@ Q�@ b@��@�P@+@�@ff@�T@�h@O�@�@V@�@�D@Z@I�@(�@��@ƨ@��@t�@C�@@��@^5@=q@�@�#@��@x�@X@G�@�@�@ �@|�@;d@��@��@�+@V@$�@��@��@�@`B@V@�/@�/@��@��@Z@(�@�F@�@t�@dZ@dZ@dZ@S�@C�@C�@33@o@o@�H@�!@~�@^5@��@��@hs@G�@7L@�@Ĝ@��@�u@�u@�@r�@Q�@A�@A�@ �@�;@�P@\)@K�@K�@K�@K�@K�@+@ȴ@��@E�@5?@{@@�@�T@��@@@�-@��@p�@��@�/@�j@�@z�@Z@I�@I�@9X@(�@(�@1@�m@ƨ@�@t�@dZ@
�@
��@
��@
��@
�\@
~�@
M�@
�@	��@	�^@	��@	x�@	X@	G�@�`@��@r�@1'@A�@b@  @�;@�@��@��@|�@|�@|�@l�@+@
=@�@�+@v�@@�@�T@�T@��@@@@@@@�h@�h@�hA��A���A��A��A���A���A��A���A���A��A���A���A���A���A���A���A���A��A��A���A���A��A��A���A���A��A��yA��A��A��A��A��A��A��`A��/A��TA��`A��HA��HA��/A��TA��TA��TA��
A���A�ƨA���A���A�ȴA���A�ȴA�ȴA�A���A�A���A߼jA߾wA���A߾wA߸RA߸RA߼jAߺ^A߰!A߲-A߲-A߮A߬Aߧ�A߬Aߟ�Aߕ�Aߗ�Aߗ�AߑhAߓuAߑhAߓuAߏ\A߉7A߉7A߃A�~�A�|�A�~�A�|�A�z�A�x�A�z�A�z�A�v�A�z�A�|�A�x�A�n�A�r�A�t�A�p�A�n�A�l�A�dZA�`BA�`BA�dZA�dZA�bNA�`BA�bNA�dZA�^5A�O�A�S�A�G�A�C�A�;dA�5?A�/A�+A�$�A��A�VA�A�A���A���A��A��`A��;A���A޾wAާ�Aޝ�Aއ+A�l�A�dZA�\)A�/Aݲ-Aܴ9A�%A�`BA�%AؾwA��A�x�A��A�
=A�A��A�O�A�hsA���A�v�A�-AЍPA�-A�n�A��Aǣ�A�ĜAƮA�x�A�
=Aã�A�M�A�-A��A�1A��;A�p�A�?}A�$�A�A��9A�(�A��7A�C�A��A�oA�bA�
=A�A��A�ƨA�\)A�oA���A�1A��yA��A�A���A�r�A�I�A�1'A�$�A���A���A���A��PA�~�A�v�A�XA���A��mA���A���A��\A�ffA���A��/A�G�A���A�^5A�E�A�|�A��RA�S�A���A�A�A��mA�\)A��+A�ZA���A��+A�\)A�M�A��A��uA��A�p�A�^5A� �A���A�S�A���A�M�A�1A��A�ƨA�~�A�I�A�+A��A�A��yA���A�A���A��PA�z�A�bNA�S�A�G�A�7LA�(�A��A��A�
=A���A��A��mA���A�A��-A�n�A�l�A�?}A� �A�  A���A���A��yA��TA��;A���A�x�A��A��A�r�A���A��A��7A� �A�
=A��A��/A���A��jA���A�~�A�E�A��yA��hA�G�A��A��A�A��`A��FA��A�`BA�XA�Q�A�33A��A��FA�\)A��A���A���A��\A�^5A�1'A�oA��A��9A�hsA�1A���A�A��+A�VA��^A�1A�XA��A�dZA�1'A��A��PA���A�G�A�z�A�z�A���A�E�A�VA��HA���A�=qA���A�l�A�Q�A��A�bA���A���A��wA��A�Q�A�1A���A��!A��A�M�A��A���A��A��HA��RA��PA�l�A�A�A��A���A�XA� �A��FA�  A}7LAy��AxȴAx�AxA�Aw�Aw�Awt�Aw+Av�Av�+Av-Au��Au33At�\At5?As�As|�Ar�9Aq��Aqt�Ap�Ak\)AidZAh��AhI�Ag�Af1Ae;dAeAd�Ad��Ad��AdE�Ac/AbI�Aa�AaƨAaG�A`�9A`  A_�A_&�A^Q�A]�mA]A]C�A]/A]%A\�`A\�A\Q�A[p�A[+AZ�HAZ�DAZI�AZAYAY�AYS�AX��AW��AWG�AU��AU;dAS��AR�ARE�AR1'AR{AQ��AQ�TAQ��AQ��AQ�AP�\AOAN��ANQ�AM��AMVAL�uALA�AK��AK
=AJffAI�wAIC�AH�uAHbAG�AG"�AFZAEp�AD�/AD��ADJAC�AC��ACt�AC`BACS�AC/AC�AB��ABȴAB�DAB�\AB��ABVAB(�ABJAA��AA�AA�AA�AA�AA�#AA��AA�wAA��AAt�AAdZAAO�AAK�AA33AAVA@r�A?�FA>��A>�A=�#A=��A=��A=hsA=S�A=?}A=/A=�A<��A<ȴA<�\A<=qA;�
A;�-A;&�A:��A:�!A:�A:�A:��A:�A:r�A:jA:ZA:9XA9��A9�wA9�7A9`BA9S�A9O�A9K�A9G�A97LA8��A8I�A7��A7�wA7��A7�A7p�A7\)A7C�A7&�A7oA7
=A6�A6�RA6�DA6jA6Q�A6M�A6I�A6E�A6A�A69XA61'A6-A6(�A6�A6�A6{A6bA6bA6{A6{A6{A6{A6bA6bA6JA5��A5�;A5��A5�wA5�^A5��A5��A5��A5�
A5�^A5��A5dZA5/A4�yA4�DA41'A4  A3�-A3K�A333A3�A2�A2��A2�!A2��A2�uA2�+A2v�A2jA2ffA2bNA2VA1�A1��A1|�A1dZA1XA1G�A17LA1&�A1oA0��A0�/A0��A0�uA0z�A0r�A0^5A0M�A09XA0(�A0JA01A01A0  A/��A/�
A/ƨA/�-A/p�A/�A.��A.�uA.ffA.VA.E�A.=qA.�A.�A.bA-��A-�A-�A-�mA-�TA-��A-��A-�wA-�-A-��A-��A-�A-`BA-&�A-%A-%A,�yA,�RA,n�A,ZA,Q�A,M�A,E�A,A�A,9XA,-A,{A,  A+�A+�
A+��A+�^A+��A+��A+�7A+p�A+dZA+S�A+G�A+;dA++A+�A+�A+�A+VA*��A*�HA*��A*�yA*�HA*�A*��A*�jA*�!A*��A*ffA*bNA*I�A*JA)�FA)S�A(��A(v�A((�A'��A'�
A'��A'C�A'VA&�yA&��A&r�A%�PA$�/A$v�A$A#�FA#�7A#x�A#l�A#;dA#&�A#�A#�A#
=A"��A"�A"�/A"�RA"r�A"�A!��A!��A!7LA �HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                            A���A���A���A���A���A���A���A��A��A��A��yA��A��TA��#A�ȴA߾wAߴ9Aߝ�A߉7A�z�A�r�A�bNA�A�A���AݮA���A�|�A�$�A���A�"�A�  A�A�A��A��FA�A���A��A�r�A���A��A��A�?}A�O�A�l�A��^A�K�A�9XA�t�A��^A�K�A��A�XA{�Av  Ao�7Ae��AaƨA]�A[p�AXr�AR��AP�AK�7AF�AC�PAB��AA�mAA33A>  A<�!A:��A:�A933A7��A6�A6M�A6 �A6bA5�A5��A4��A37LA2�\A2bA1C�A0�9A05?A/�mA.��A.(�A-�mA-��A-
=A,VA,bA+��A+K�A+%A*��A*Q�A(�+A'oA$ffA#;dA"��A!C�A!O�A!\)A!�TA!��A!oA ��A"bA!�A!��A ȴA�mA�mA�A�wAXAĜAVA��AK�A��A1A�A�DA��AC�A�\A��Ap�A�AQ�AZA  A�A��A��A5?A��A��A��Ax�A`BAO�A��AQ�A��A��A�A33A��A^5A �A  AAhsA\)AO�A
=A�uA(�A�^AK�A�!AQ�A�-Ap�A;dA
��A
ȴA
~�A
-A	��A	�FA	p�A	
=A��Av�A�^Ax�AO�AoAA�yA�AAA\)A7LA�HA�uAbNAJA��A;dA�A�9A��Az�AA�A{A��A�A��A�A �RA ��A ZA VA M�A -A b@��F@��H@��\@��T@��@���@�Ĝ@�Q�@�b@���@��@���@��j@�(�@���@�@���@�{@���@���@�x�@��@��j@�\)@�~�@�$�@���@�X@��@�dZ@��@�-@�7@��@��@�z�@�9X@�P@��H@��@�/@�V@���@�bN@��@��
@�;d@��@�v�@�@䛦@�P@⟾@��#@߾w@ޏ\@�/@��
@�"�@�ff@�J@�O�@�j@�1'@�Q�@���@ְ!@��@���@�G�@Լj@��@�"�@�$�@�%@�j@Ο�@͉7@̃@˕�@���@�~�@�@ɲ-@�V@�bN@��@�|�@�o@�~�@�J@Ų-@�hs@�7L@ģ�@�Z@ÍP@¸R@�E�@���@���@���@�x�@�`B@��@�z�@�1@�1@�b@��F@���@�K�@���@��T@��-@��@��9@��F@�@���@�^5@�{@��h@�?}@�?}@�?}@�&�@��u@�Z@��;@�"�@��@���@�~�@���@�X@��@��/@���@���@��D@�j@���@�+@���@��\@�^5@�-@�$�@�V@�M�@��T@���@�r�@�Q�@��@�
=@�-@���@�hs@��@�Ĝ@���@�S�@���@�-@�@�`B@��u@��;@��@�@�=q@�@���@��@�1'@���@��@�K�@���@��R@���@�v�@��@��#@���@�hs@�X@��@��@���@��9@�r�@��m@��@���@���@��h@���@�A�@���@���@��;@��w@�|�@�
=@��!@���@��+@�v�@�n�@���@��@�1@�S�@��@��y@���@�v�@�E�@�{@��@�{@�{@��/@���@�z�@�b@��@�ƨ@��F@���@�|�@�S�@���@�V@�5?@��@�J@��#@��@�V@���@��@��u@�bN@�  @�ƨ@��@���@���@�|�@�\)@�C�@�+@��R@��+@�=q@���@��@�@�O�@��@�V@���@���@�j@��w@�l�@�
=@��R@�v�@���@�p�@�/@���@��u@�Q�@��
@��@�S�@�
=@���@�~�@�J@��#@���@���@��@��/@�Z@�b@�ƨ@��F@���@�|�@�l�@�l�@�dZ@�dZ@�S�@�33@�@��!@�~�@�-@��T@��^@�hs@���@�Ĝ@�Q�@�@�P@~�y@~v�@}�T@}O�@}/@|�D@|Z@|9X@{�m@{"�@z�!@zM�@z=q@z-@y��@yx�@y&�@y�@y%@x�`@x1'@w\)@w�@v�@v�R@vv�@v5?@u�-@uO�@t��@s�m@st�@s@r�@r��@r�\@r~�@r=q@q�@q�^@q��@p��@pr�@pb@o;d@o�@n��@n��@nE�@n5?@m��@mp�@m`B@mV@lj@l(�@k��@k��@kC�@j��@jM�@i��@i7L@h�9@hr�@h1'@g��@g�@g\)@f��@f�y@f�@f�@fȴ@f��@fv�@e�@eV@d�@d9X@c��@c�
@ct�@b�!@bn�@b-@a��@a��@aX@`��@`A�@_��@_|�@_;d@_
=@^�y@^�R@^v�@^@]��@]O�@\Z@[�
@Z�@Y��@Y�@X�9@XQ�@WK�@V��@Vȴ@V�+@VV@U�@U@T��@S�m@S�m@S�
@S�m@S�
@R�H@R-@RJ@Q�@QX@P�`@P�u@PbN@Ol�@N�+@N5?@M�T@M�@MO�@M?}@L�@L�j@L�j@L�j@L��@L�j@L�@L9X@K�
@KdZ@J=q@Ix�@IX@IG�@I%@G��@Gl�@GK�@G�@Fȴ@Fff@FE�@E�@E�-@EO�@E�@D��@D(�@C�
@CS�@CC�@C33@Co@B��@B��@Bn�@B-@A��@A�7@@��@@Q�@?�P@?+@?�@?
=@>�y@>�R@>��@>��@>ff@=�@=��@=O�@=/@<��@<�@;ƨ@:��@:^5@9��@9��@9��@9G�@9%@8��@8A�@8 �@7�@7�w@7+@6��@6�@6��@6v�@6V@65?@6{@5�T@5�@5O�@5?}@5�@4�@4��@4��@4�D@4j@4(�@3��@3ƨ@3��@3dZ@3C�@3C�@333@333@333@3@2�H@1�@1X@1%@0��@0Ĝ@0bN@/�@/K�@/�@.�y@.�R@.v�@.ff@.$�@.@-�@-@-�@-V@,�@,��@,��@,j@,(�@+��@+C�@*��@*n�@*=q@)�@)�7@)G�@)�@(��@(r�@(  @'��@'|�@'K�@'
=@&�+@&ff@&$�@%O�@$��@$j@$j@$Z@$I�@$(�@#�
@#��@#t�@#C�@#@"��@"��@"=q@"J@!�@!�^@!��@!x�@!&�@!%@ �`@ ��@ �9@ �u@ Q�@ b@��@�P@+@�@ff@�T@�h@O�@�@V@�@�D@Z@I�@(�@��@ƨ@��@t�@C�@@��@^5@=q@�@�#@��@x�@X@G�@�@�@ �@|�@;d@��@��@�+@V@$�@��@��@�@`B@V@�/@�/@��@��@Z@(�@�F@�@t�@dZ@dZ@dZ@S�@C�@C�@33@o@o@�H@�!@~�@^5@��@��@hs@G�@7L@�@Ĝ@��@�u@�u@�@r�@Q�@A�@A�@ �@�;@�P@\)@K�@K�@K�@K�@K�@+@ȴ@��@E�@5?@{@@�@�T@��@@@�-@��@p�@��@�/@�j@�@z�@Z@I�@I�@9X@(�@(�@1@�m@ƨ@�@t�@dZ@
�@
��@
��@
��@
�\@
~�@
M�@
�@	��@	�^@	��@	x�@	X@	G�@�`@��@r�@1'@A�@b@  @�;@�@��@��@|�@|�@|�@l�@+@
=@�@�+@v�@@�@�T@�T@��@@@@@@@�h@�hG�O�A��A���A��A��A���A���A��A���A���A��A���A���A���A���A���A���A���A��A��A���A���A��A��A���A���A��A��yA��A��A��A��A��A��A��`A��/A��TA��`A��HA��HA��/A��TA��TA��TA��
A���A�ƨA���A���A�ȴA���A�ȴA�ȴA�A���A�A���A߼jA߾wA���A߾wA߸RA߸RA߼jAߺ^A߰!A߲-A߲-A߮A߬Aߧ�A߬Aߟ�Aߕ�Aߗ�Aߗ�AߑhAߓuAߑhAߓuAߏ\A߉7A߉7A߃A�~�A�|�A�~�A�|�A�z�A�x�A�z�A�z�A�v�A�z�A�|�A�x�A�n�A�r�A�t�A�p�A�n�A�l�A�dZA�`BA�`BA�dZA�dZA�bNA�`BA�bNA�dZA�^5A�O�A�S�A�G�A�C�A�;dA�5?A�/A�+A�$�A��A�VA�A�A���A���A��A��`A��;A���A޾wAާ�Aޝ�Aއ+A�l�A�dZA�\)A�/Aݲ-Aܴ9A�%A�`BA�%AؾwA��A�x�A��A�
=A�A��A�O�A�hsA���A�v�A�-AЍPA�-A�n�A��Aǣ�A�ĜAƮA�x�A�
=Aã�A�M�A�-A��A�1A��;A�p�A�?}A�$�A�A��9A�(�A��7A�C�A��A�oA�bA�
=A�A��A�ƨA�\)A�oA���A�1A��yA��A�A���A�r�A�I�A�1'A�$�A���A���A���A��PA�~�A�v�A�XA���A��mA���A���A��\A�ffA���A��/A�G�A���A�^5A�E�A�|�A��RA�S�A���A�A�A��mA�\)A��+A�ZA���A��+A�\)A�M�A��A��uA��A�p�A�^5A� �A���A�S�A���A�M�A�1A��A�ƨA�~�A�I�A�+A��A�A��yA���A�A���A��PA�z�A�bNA�S�A�G�A�7LA�(�A��A��A�
=A���A��A��mA���A�A��-A�n�A�l�A�?}A� �A�  A���A���A��yA��TA��;A���A�x�A��A��A�r�A���A��A��7A� �A�
=A��A��/A���A��jA���A�~�A�E�A��yA��hA�G�A��A��A�A��`A��FA��A�`BA�XA�Q�A�33A��A��FA�\)A��A���A���A��\A�^5A�1'A�oA��A��9A�hsA�1A���A�A��+A�VA��^A�1A�XA��A�dZA�1'A��A��PA���A�G�A�z�A�z�A���A�E�A�VA��HA���A�=qA���A�l�A�Q�A��A�bA���A���A��wA��A�Q�A�1A���A��!A��A�M�A��A���A��A��HA��RA��PA�l�A�A�A��A���A�XA� �A��FA�  A}7LAy��AxȴAx�AxA�Aw�Aw�Awt�Aw+Av�Av�+Av-Au��Au33At�\At5?As�As|�Ar�9Aq��Aqt�Ap�Ak\)AidZAh��AhI�Ag�Af1Ae;dAeAd�Ad��Ad��AdE�Ac/AbI�Aa�AaƨAaG�A`�9A`  A_�A_&�A^Q�A]�mA]A]C�A]/A]%A\�`A\�A\Q�A[p�A[+AZ�HAZ�DAZI�AZAYAY�AYS�AX��AW��AWG�AU��AU;dAS��AR�ARE�AR1'AR{AQ��AQ�TAQ��AQ��AQ�AP�\AOAN��ANQ�AM��AMVAL�uALA�AK��AK
=AJffAI�wAIC�AH�uAHbAG�AG"�AFZAEp�AD�/AD��ADJAC�AC��ACt�AC`BACS�AC/AC�AB��ABȴAB�DAB�\AB��ABVAB(�ABJAA��AA�AA�AA�AA�AA�#AA��AA�wAA��AAt�AAdZAAO�AAK�AA33AAVA@r�A?�FA>��A>�A=�#A=��A=��A=hsA=S�A=?}A=/A=�A<��A<ȴA<�\A<=qA;�
A;�-A;&�A:��A:�!A:�A:�A:��A:�A:r�A:jA:ZA:9XA9��A9�wA9�7A9`BA9S�A9O�A9K�A9G�A97LA8��A8I�A7��A7�wA7��A7�A7p�A7\)A7C�A7&�A7oA7
=A6�A6�RA6�DA6jA6Q�A6M�A6I�A6E�A6A�A69XA61'A6-A6(�A6�A6�A6{A6bA6bA6{A6{A6{A6{A6bA6bA6JA5��A5�;A5��A5�wA5�^A5��A5��A5��A5�
A5�^A5��A5dZA5/A4�yA4�DA41'A4  A3�-A3K�A333A3�A2�A2��A2�!A2��A2�uA2�+A2v�A2jA2ffA2bNA2VA1�A1��A1|�A1dZA1XA1G�A17LA1&�A1oA0��A0�/A0��A0�uA0z�A0r�A0^5A0M�A09XA0(�A0JA01A01A0  A/��A/�
A/ƨA/�-A/p�A/�A.��A.�uA.ffA.VA.E�A.=qA.�A.�A.bA-��A-�A-�A-�mA-�TA-��A-��A-�wA-�-A-��A-��A-�A-`BA-&�A-%A-%A,�yA,�RA,n�A,ZA,Q�A,M�A,E�A,A�A,9XA,-A,{A,  A+�A+�
A+��A+�^A+��A+��A+�7A+p�A+dZA+S�A+G�A+;dA++A+�A+�A+�A+VA*��A*�HA*��A*�yA*�HA*�A*��A*�jA*�!A*��A*ffA*bNA*I�A*JA)�FA)S�A(��A(v�A((�A'��A'�
A'��A'C�A'VA&�yA&��A&r�A%�PA$�/A$v�A$A#�FA#�7A#x�A#l�A#;dA#&�A#�A#�A#
=A"��A"�A"�/A"�RA"r�A"�A!��A!��A!7LA �HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                            ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�aB��B��B��B�}B�UB�[B�tB�EBɆB��B�0B� B�?B�fB�B��B��B�B�B�QB�QB�B�B�B��B�JB	_�B	��B	��B	�B
�B
^�B
jKB
qvB
�aB
�NB
�WB
�mB
��B
��B
��B
��B
��B
x8B
*�B
CB
�B	��B	ܒB	͟B	ƨB	��B	�kB	��B	dZB	TaB	>�B	2-B	&�B	MB	�B	:B	�B	B	DB	�B	7B	/OB	9XB	b�B	ncB	wfB	��B	��B	�?B	��B	�6B	�-B	��B	ٴB	�B	�DB	�B	��B	��B	��B	�]B	��B	�.B
oB
�B
:B
�B
!bB
!�B
�B
�B
~B
�B
�B
B
1B	��B	�B	��B	�PB
GB
�B
xB
�B
+B
*0B
,�B
+6B
*�B
"�B
"4B
'�B
*�B
,�B
/�B
-�B
+�B
'�B
'�B
$tB
"4B
$@B
$tB
!�B
$�B
$B
"4B
 �B
 'B
$@B
%�B
'�B
)*B
(�B
(�B
(�B
(�B
(�B
)_B
(�B
*0B
)�B
,qB
,�B
+�B
+�B
,�B
+kB
-CB
.�B
.}B
-wB
0�B
2�B
5tB
4nB
3�B
0�B
3�B
8RB
7�B
7�B
7LB
6�B
6�B
7�B
8�B
9�B
9�B
9$B
:*B
9�B
8�B
7�B
7�B
5�B
4B
49B
6zB
7�B
:^B
9�B
8RB
9XB
8�B
7�B
8B
5�B
5tB
4nB
6�B
5B
2-B
2�B
49B
6zB
6�B
5?B
4�B
6�B
7�B
6FB
2aB
3hB
2-B
1�B
1�B
1�B
1�B
0�B
0�B
/OB
0!B
,�B
+�B
,�B
-�B
,=B
,�B
-CB
-CB
+6B
+kB
+kB
,B
*eB
*0B
)�B
)*B
(�B
(�B
($B
)�B
($B
&LB
&B
%zB
$B
&�B
"�B
#B
"�B
!�B
!�B
 \B
 \B
 �B
�B
!B
B
�B
�B
�B
�B
CB
qB
�B
1B
�B
MB
B
�B
�B
B
�B
	7B
%B
_B
�B
�B
�B
_B
�B
	lB

�B
	�B
	B
�B
�B
JB
~B

�B
�B
	7B
+B
_B
 �B
 �B	�(B	��B	��B	��B	��B	�.B
  B
 iB
oB
�B
AB
{B
GB
uB
�B
B
�B
�B
B
�B
B
oB
oB
oB
B
�B
GB
AB
B
�B
�B
�B
	7B
	�B
�B
+B
YB
�B
_B
�B
YB
%B
%B
�B
YB
%B
�B
�B
�B
�B
+B
�B
+B
�B
�B
�B
�B
�B
�B
_B
1B
�B
fB
�B
�B
	�B
�B
	lB
	7B

	B

=B
B
�B
�B
�B
�B
.B
�B
:B
�B
hB
hB
4B
�B
 B
�B
hB
4B
�B
�B
B
hB
B
�B
oB
�B
�B
oB
oB
B
�B
FB
�B
MB
�B
SB
�B
�B
YB
�B
�B
_B
�B
�B
	B
qB
�B
�B
B
IB
~B
xB
�B
IB
�B
�B
 'B
 �B
 'B
!bB
!�B
"4B
"4B
#B
#B
!�B
!bB
 �B
 �B
!-B
!-B
!-B
!�B
!�B
#�B
'�B
'RB
&�B
)�B
(�B
)�B
(�B
(�B
(�B
(�B
(�B
*�B
+�B
,qB
,�B
,�B
,�B
.B
.�B
.�B
.�B
/�B
/�B
0�B
0UB
0UB
0�B
0�B
1'B
1'B
1'B
1�B
1�B
2aB
2aB
1�B
1�B
2aB
3�B
3hB
3hB
4B
3�B
49B
5B
4�B
5tB
5?B
5tB
6�B
6FB
6�B
7�B
7B
7�B
8�B
8�B
8�B
8�B
9XB
9�B
:^B
:*B
9�B
9�B
:*B
;�B
;0B
<B
<B
<B
<jB
<6B
<6B
<B
<6B
<B
;�B
<B
<�B
<�B
<�B
=�B
=�B
=�B
>BB
>�B
>wB
?�B
?}B
?�B
@�B
@�B
A B
A�B
AUB
B'B
A�B
A�B
B'B
B�B
C-B
CaB
C-B
C-B
C-B
D3B
C�B
C�B
C�B
C�B
D�B
D�B
E9B
E9B
E9B
E9B
EmB
FB
E�B
GB
F�B
GzB
G�B
GzB
G�B
G�B
G�B
G�B
HB
HB
G�B
H�B
H�B
H�B
I�B
IB
IRB
I�B
I�B
IRB
J#B
I�B
I�B
JXB
JXB
J�B
J�B
J�B
J�B
K)B
K�B
K�B
L0B
LdB
LdB
L�B
L�B
L�B
MB
M6B
MB
M6B
MB
MB
L�B
M6B
M�B
N�B
NB
N�B
NpB
NpB
N�B
O�B
O�B
O�B
O�B
O�B
PHB
P�B
P�B
QNB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
RTB
Q�B
R�B
S&B
S&B
TaB
U2B
UgB
UgB
VB
V�B
V�B
V�B
W?B
V�B
W?B
W
B
YB
YKB
X�B
X�B
XyB
X�B
Y�B
ZB
ZB
ZB
Z�B
[#B
Z�B
Z�B
\�B
\�B
\�B
]�B
]�B
]�B
]�B
^B
^B
]�B
^B
]�B
]�B
]�B
^jB
^5B
^�B
`B
_�B
_�B
_pB
`B
aHB
a|B
a|B
a|B
a�B
a�B
bB
bNB
bNB
b�B
b�B
cTB
c�B
c�B
dZB
d&B
c�B
d&B
dZB
d&B
d�B
dZB
d�B
d�B
e�B
e�B
gB
f�B
f�B
f�B
gB
gB
g8B
f�B
gmB
g�B
g�B
h
B
g�B
g�B
h>B
iDB
jB
jKB
jB
jB
j�B
kB
kB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m)B
m]B
m�B
m�B
m�B
m�B
m�B
n/B
n/B
n/B
n/B
ncB
n�B
n�B
n�B
n�B
o B
o B
o B
n�B
n�B
o B
n�B
p�B
p�B
poB
p�B
poB
p�B
qAB
q�B
q�B
q�B
q�B
rGB
rB
r|B
r�B
r|B
r�B
r�B
sMB
s�B
s�B
s�B
s�B
tB
t�B
t�B
uZB
uZB
u�B
u�B
v+B
v`B
v+B
v�B
v�B
wfB
wfB
wfB
w�B
xB
x8B
x8B
xlB
yrB
zB
y�B
y�B
y�B
y�B
zB
zxB
zxB
zxB
zxB
z�B
{B
{JB
{B
{B
{�B
{�B
{�B
{�B
|PB
|PB
|PB
|PB
|�B
|�B
|�B
|�B
}"B
}�B
}�B
~(B
~�B
.B
cB
�B
�B
�B
� B
�iB
�iB
�iB
��B
��B
��B
�B
�B
�;B
�oB
��B
�AB
�B
�AB
��B
��B
��B
��B
��B
��B
�{B
�B
��B
��B
�B
�SB
�SB
��B
��B
�%B
�YB
�YB
�YB
��B
��B
��B
��B
��B
�_B
�_B
�1B
��B
�1B
�1B
�1B
�1B
�fB
�1B
�fB
�fB
�fB
��B
��B
��B
�B
�B
�lB
��B
�	B
�	B
�	B
�=B
��B
�B
��B
��B
��B
�B
�B
�B
�B
�DB
��B
�B
�B
�B
�B
�B
�B
�B
�JB
��B
��B
�PB
�PB
�PB
��B
��B
��B
��B
��B
��B
��B
��B
�"B
�VB
��B
��B
��B
��B
��B
�(B
��B
�(B
�(B
�(B
�\B
�\B
�\B
��B
��B
��B
��B
��B
��B
��B
� B
��B
� B
�4B
��B
��B
��B
�B
��B
�B
��B
�oB
�B
�@B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�FB
�FB
��B
��B
��B
�MB
��B
�B
��B
��B
��B
��B
��B
��B
��B
�B
�B
��B�3B��B��BĜB��BÖB��B�UB��B��B��B��B�}B� B�}B�HB��B��B��B�OB�gB��B��BŢB�?B�#B�jB�6B�jB�^B�)BɺB�<B��B��B҉BӏB��BҽB�B�2B��BѷB��B� B�B�B��B�,B�&B�B�mB�B�B�B�B�KB�B��B�B��B�KB�B�DB�B�B�yB�DB�B��B�DB�WB��B�B�B��B�B�KB�KB��B��B��B��B��B�)B�B�B�WB�"B�QB�B�WB��B�KB�QB�B��B�B�B�B�B�B�"B��B�B�B�B��B�B��B�KB�/B�B�;B��B��B�AB�|B�B�|B�|B��B��B�B�B�B�B��B�B��B��B��B�+B�	B�	B��B�+B�PB
=B7�BL�BS�Be`BzB{JBr|B�PB�:B��B�vB��B��B�`B��B�B�B	�B	.�B	�~B	T�B	;�B	2�B	8�B	��B	~(B	x8B	y�B	w�B	z�B	� B	�@B	��B	��B	��B	�qB	� B	�&B	�jB	�&B	�NB	�B	��B	�NB	��B	��B
�B
  B
"B
3�B
I�B
QB
XEB
_;B
aB
a�B
bB
`B
e�B
k�B
l�B
m]B
lWB
jB
o B
y	B
r�B
qB
n�B
t�B
�\B
ƨB
��B
��B
�-B
�cB
��B
�B
��B
�B
�BB
ޞB
��B�B
��B
�pB
�B
��B
�B
��B
�^B
�aB
�B
�kB
�B
�B
��B
�qB
��B
�wB
��B
�\B
��B
�B
��B
�qB
�kB
�	B
�kB
��B
��B
��B
��B
�$B
�+B
�SB
�MB
��B
�uB
�oB
��B
��B
��B
��B
��B
�DB
�rB
�JB
�+B
��B
��B
��B
�B
�iB
}�B
}�B
x8B
v+B
qB
� B
�eB
�B
{B
{�B
�{B
RTB
1�B
+kB
-CB
)*B
(�B
)*B
)_B
)_B
,�B
5B
&�B
+kB
�B
�B
"�B
!B
%FB
�B
�B
$B
�B
eB
xB
7B
�B
�B
�B
JB
�B
�B
�B
fB
fB

�B
�B
	B
4B
�B
�B	��B
DB
 B
GB
�B	�B	��B	��B	�	B	�iB

�B	�B	�DB	�+B	�B	�B	�sB	�HB	� B	�sB	�<B	�gB	��B	��B	��B	̘B	�B	��B	͟B	�
B	��B	�XB	̘B	�B	�
B	��B	� B	�[B	�tB	�B	� B	��B	��B	�<B	�0B	��B	ںB	�2B
�B	��B	�qB	�B	�B	��B	��B	��B	�tB	��B	��B	�FB	�B	�zB	��B	��B	�~B	�	B	�=B	��B	��B	�B	ߤB	��B	zB	lWB	s�B	w2B	f2B	[#B	ZQB	[�B	UgB	aB	e`B	YB	PB	H�B	UgB	K^B	L�B	@�B	K�B	NpB	:^B	9$B	B'B	5tB	4�B	5�B	/�B	6�B	<6B	-�B	/B	/B	+�B	*�B	'B	#:B	%B	,B	#�B	-wB	$�B	$tB	6B	�B	~B	
�B	xB		�B	�B	�B		7B	4B	
	B	:B	�B	xB	�B	�B	�B	�B	�B	hB	B	�B	�B	�B	oB	�B	oB	�B	�B	4B	�B	�B	VB	xB	1B	1B	�B	
�B	_B	�B	�B	�B��B	�B	�B	�B		B	qB		B	�B	�B	�B	�B	eB	B	B	�B	�B	�B	SB	+B	YB	#�B	-�B	:�B	0�B	.�B	)�B	.}B	,�B	-�B	,�B	-�B	.�B	.}B	7�B	=�B	G�B	K)B	N<B	^�B	d�B	g�B	e`B	d�B	f�B	ffB	iB	h�B	k�B	m�B	t�B	qB	u�B	u%B	tTB	s�B	sMB	s�B	tTB	~]B	��B	��B	��B	��B	�PB	�"B	��B	�B	�MB	��B	�B	�4B	�$B	��B	��B	�B	�tB	�FB	��B	�zB	�LB	��B	�B	��B	�*B	�XB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�HB	�[B	�B	ǮB	�mB	�mB	��B	ʌB	��B	�^B	��B	�B	�}B	҉B	��B	��B	��B	�NB	�fB	�B	�B	��B	�DB	�B	�
B	��B	��B	��B	��B	�yB	�DB	�yB	�KB	��B	�cB	��B	��B	�JB	��B	��B	��B	��B	�VB
B
 iB	��B	��B	��B	��B
  B
 �B
;B
 �B
  B	�]B	�]B	�]B	��B	��B	�VB
�B
 �B	��B	��B	�PB	��B	�(B	��B
B	�.B
 4B
 �B
  B
 �B
oB
oB
�B
�B
B
�B
�B
B
+B
DB
�B
�B
oB
�B
�B
 'B
B
OB
�B
!B
!B
VB
 �B
!-B
!�B
"�B
"4B
"�B
"4B
"4B
!�B
 'B
 �B
�B
 \B
VB
!bB
 'B
VB
B
~B
B
�B
�B
B
CB
�B
IB
IB
=B
qB
CB
!�B
eB
"hB
#�B
%�B
$B
!bB
"4B
xB
�B
SB
YB
�B
(B
\B
�B
4B
CB
xB
xB
�B	��B	�xB	�	B	��B	��B	��B	�B	�B	�B	��B	�+B	�lB	��B	��B	�xB	��B	��B	�+B	�2G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                            B�[B½B��B��B��B�bB�B�kB�lB��BͥB̂B�PB��B�vB��B�\B�QB��B��B�B�,B�`B�dB3ZB��B	0LB	r B	�zB	�~B	�B
.�B
cdB
pB
��B
��B
�gB�B
ͶB
��B
�UB
�qB
�yB
��B
�sB
9�B
/�B
1B
"B	��B	ږB	��B	�JB	�B	��B	r�B	`�B	G�B	<�B	9IB	$B	�B	!�B	 B	�B	,B	�B	$�B	4B	?�B	e"B	q�B	|�B	�B	��B	��B	�9B	��B	��B	͎B	�B	�PB	�DB	�nB	�cB
 �B
 B
�B
HB
 5B
~B
�B
�B
�B
"�B
#"B
 �B
dB
�B
%�B
$|B
�B
JB	�#B	��B	��B	��B
�B
�B
�B
�B
B
+,B
-B
.?B
-�B
#B
"HB
(AB
,YB
.�B
1zB
0B
-@B
)�B
*pB
(DB
#�B
&VB
&�B
$�B
&�B
&B
$�B
"B
 LB
%}B
'�B
*IB
*!B
*B
)�B
)$B
)LB
)�B
)�B
)"B
+�B
,B
.6B
-jB
,!B
,�B
-�B
-B
.%B
/9B
/\B
.�B
0�B
3B
6�B
6%B
5HB
2_B
5�B
:]B
9[B
9�B
8CB
7xB
7�B
8�B
9�B
:�B
:�B
:&B
;9B
;6B
9rB
9B
:B
6�B
4�B
5B
6�B
84B
;�B
;sB
9RB
:�B
9�B
8�B
9=B
6�B
6�B
5tB
8�B
6oB
2�B
3:B
4�B
7QB
7�B
5�B
5B
7�B
9DB
7�B
2�B
4FB
2NB
1�B
2uB
2tB
2aB
1�B
1�B
0�B
1{B
-`B
,
B
-�B
.1B
-B
.9B
/[B
/6B
,DB
,.B
,�B
,�B
+YB
*�B
*B
)�B
)tB
)�B
*�B
+�B
(�B
&�B
&�B
&4B
&�B
'�B
#�B
$/B
#�B
"wB
!�B
 �B
!�B
!�B
 �B
�B
fB
*B
�B
:B
iB
^B
5B
�B
�B
B
WB
�B
�B
~B
iB
OB
�B
zB
�B
�B
&B
�B
�B
�B

tB
�B
5B
	jB
�B
B
�B
B
�B

�B

�B

�B
	oB
�B
�B	�]B
 �B	��B
 NB	��B
 qB
 �B
SB
LB
�B
!B
-B
�B
�B
�B
�B
:B
�B
�B
iB
qB
�B
�B
�B
�B
B
B
ZB
B
(B
B
iB

�B

�B
�B
@B
AB
	�B
�B
_B
�B
�B
B
(B
bB
1B
B
B
HB
�B
B
�B
�B
�B
�B
	+B
B
3B
�B
�B
YB
�B
	�B
	�B
KB

B
�B
	�B
	KB
	�B

iB
�B
|B
?B
�B
�B
fB
�B
FB
B
B
%B
�B
�B
HB
�B
>B
	B
%B
#B
�B
�B
xB
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
�B
kB
�B
SB
B
�B
4B
�B
�B
�B
*B
B
MB
XB
B
�B
�B
�B
B
-B
BB
�B
 }B
!B
!�B
 PB
!�B
"%B
"dB
#vB
$tB
$�B
"�B
!�B
!/B
!HB
!�B
!�B
!�B
"B
!�B
#�B
)�B
'WB
'�B
*�B
)EB
)�B
)B
),B
)B
(�B
)�B
+|B
+�B
,�B
,�B
-B
-\B
.�B
/B
/B
/�B
0&B
0BB
1&B
0�B
0mB
0�B
1)B
1jB
1^B
1nB
2aB
2fB
2�B
2�B
1�B
2B
34B
3�B
3�B
3�B
4xB
4JB
5�B
5�B
5bB
6B
5�B
6�B
7lB
6�B
7]B
8B
7�B
8�B
9`B
8�B
9LB
9iB
9�B
:fB
:�B
:UB
:<B
:`B
;lB
<�B
;�B
<�B
<,B
<9B
<�B
<XB
<=B
<B
<=B
<+B
<B
<kB
=9B
=	B
=zB
>8B
>B
>LB
?B
?RB
?aB
@�B
?�B
@SB
@�B
AB
A�B
A�B
A�B
BZB
A�B
BB
B�B
CpB
C�B
CvB
CFB
ClB
C�B
D~B
C�B
C�B
C�B
DRB
E�B
EB
EzB
EaB
E~B
E�B
E�B
FxB
F�B
G�B
GWB
G�B
G�B
G�B
G�B
G�B
G�B
H7B
HLB
HFB
HTB
IiB
H�B
I�B
I�B
IHB
I�B
JB
I�B
I�B
J}B
JB
JB
J�B
J�B
J�B
J�B
KNB
KmB
K�B
LB
L�B
L�B
L�B
L�B
M-B
L�B
L�B
M]B
MKB
MB
M;B
MB
M'B
MB
M�B
N�B
O	B
N{B
OB
N�B
N�B
O�B
O�B
O�B
O�B
O�B
P#B
P�B
QEB
QXB
Q�B
Q�B
Q�B
Q�B
Q�B
RB
R^B
R�B
RIB
S�B
S�B
TB
UHB
V	B
U�B
U�B
V�B
W(B
WB
WB
WxB
W=B
W�B
X)B
Z!B
YUB
X�B
X�B
X�B
Y�B
Z�B
ZCB
ZMB
Z�B
[,B
[vB
[5B
[�B
]pB
]SB
]"B
]�B
]�B
]�B
]�B
^2B
^B
]�B
]�B
]�B
]�B
^B
^�B
^�B
_�B
`�B
`B
_�B
_�B
a.B
a�B
a�B
a�B
a�B
bB
bB
bkB
b�B
b�B
b�B
cB
c�B
c�B
dB
dnB
d<B
dB
dhB
d�B
d_B
d�B
d�B
eB
e�B
f�B
f�B
ggB
f�B
f�B
f�B
g5B
gB
gOB
gB
g�B
g�B
h&B
h1B
hB
h8B
i"B
j9B
j�B
j�B
j�B
j�B
k
B
kcB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
l�B
l�B
m'B
mB
mB
mB
maB
m�B
m�B
m�B
m�B
m�B
n B
n`B
nFB
nUB
nqB
n�B
n�B
n�B
n�B
n�B
oB
oB
oB
n�B
oB
o4B
o�B
q4B
p�B
p�B
p�B
p�B
qQB
q�B
q�B
q�B
rB
rB
r`B
rVB
r�B
r�B
r�B
r�B
sVB
ssB
s�B
s�B
s�B
tB
t�B
uB
t�B
u�B
u�B
u�B
vZB
vpB
v�B
vsB
w0B
w>B
w�B
w�B
w�B
w�B
x~B
xbB
x�B
yAB
z B
zBB
y�B
y�B
y�B
zB
zcB
z�B
z�B
z�B
z�B
{B
{OB
{�B
{�B
{�B
{�B
{�B
|B
|:B
|tB
|sB
|fB
|tB
|�B
|�B
|�B
}5B
}kB
}�B
~MB
~�B
FB
�B
�B
�B
�B
�B
�`B
��B
�B
��B
��B
�B
�B
�,B
�<B
�B
��B
�B
�fB
�5B
��B
��B
��B
��B
��B
��B
�mB
��B
��B
��B
��B
�pB
�yB
��B
��B
�B
�YB
�~B
��B
��B
�&B
��B
�B
��B
�;B
��B
��B
�aB
�B
�CB
�5B
�6B
�EB
�xB
�8B
�yB
��B
�oB
��B
��B
�B
�-B
�jB
��B
�	B
�,B
�B
�2B
��B
��B
� B
��B
��B
��B
�0B
�"B
�B
�5B
��B
��B
�GB
�(B
�B
�B
�B
�B
�=B
��B
��B
�4B
�eB
�tB
�eB
��B
��B
��B
��B
��B
��B
��B
��B
��B
�}B
��B
��B
��B
�B
�B
�,B
�B
�:B
�/B
�LB
��B
��B
��B
�B
��B
�jB
��B
��B
��B
��B
�B
��B
�7B
��B
��B
��B
��B
�(B
��B
�iB
�B
��B
�IB
�8B
��B
��B
�6B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
��B
��B
�!B
��B
�bB
��B
�,B
��B
��B
��B
��B
��B
��B
��B
�!B
�!G�O�B�3B��B��BĜB��BÖB��B�UB��B��B��B��B�}B� B�}B�HB��B��B��B�OB�gB��B��BŢB�?B�#B�jB�6B�jB�^B�)BɺB�<B��B��B҉BӏB��BҽB�B�2B��BѷB��B� B�B�B��B�,B�&B�B�mB�B�B�B�B�KB�B��B�B��B�KB�B�DB�B�B�yB�DB�B��B�DB�WB��B�B�B��B�B�KB�KB��B��B��B��B��B�)B�B�B�WB�"B�QB�B�WB��B�KB�QB�B��B�B�B�B�B�B�"B��B�B�B�B��B�B��B�KB�/B�B�;B��B��B�AB�|B�B�|B�|B��B��B�B�B�B�B��B�B��B��B��B�+B�	B�	B��B�+B�PB
=B7�BL�BS�Be`BzB{JBr|B�PB�:B��B�vB��B��B�`B��B�B�B	�B	.�B	�~B	T�B	;�B	2�B	8�B	��B	~(B	x8B	y�B	w�B	z�B	� B	�@B	��B	��B	��B	�qB	� B	�&B	�jB	�&B	�NB	�B	��B	�NB	��B	��B
�B
  B
"B
3�B
I�B
QB
XEB
_;B
aB
a�B
bB
`B
e�B
k�B
l�B
m]B
lWB
jB
o B
y	B
r�B
qB
n�B
t�B
�\B
ƨB
��B
��B
�-B
�cB
��B
�B
��B
�B
�BB
ޞB
��B�B
��B
�pB
�B
��B
�B
��B
�^B
�aB
�B
�kB
�B
�B
��B
�qB
��B
�wB
��B
�\B
��B
�B
��B
�qB
�kB
�	B
�kB
��B
��B
��B
��B
�$B
�+B
�SB
�MB
��B
�uB
�oB
��B
��B
��B
��B
��B
�DB
�rB
�JB
�+B
��B
��B
��B
�B
�iB
}�B
}�B
x8B
v+B
qB
� B
�eB
�B
{B
{�B
�{B
RTB
1�B
+kB
-CB
)*B
(�B
)*B
)_B
)_B
,�B
5B
&�B
+kB
�B
�B
"�B
!B
%FB
�B
�B
$B
�B
eB
xB
7B
�B
�B
�B
JB
�B
�B
�B
fB
fB

�B
�B
	B
4B
�B
�B	��B
DB
 B
GB
�B	�B	��B	��B	�	B	�iB

�B	�B	�DB	�+B	�B	�B	�sB	�HB	� B	�sB	�<B	�gB	��B	��B	��B	̘B	�B	��B	͟B	�
B	��B	�XB	̘B	�B	�
B	��B	� B	�[B	�tB	�B	� B	��B	��B	�<B	�0B	��B	ںB	�2B
�B	��B	�qB	�B	�B	��B	��B	��B	�tB	��B	��B	�FB	�B	�zB	��B	��B	�~B	�	B	�=B	��B	��B	�B	ߤB	��B	zB	lWB	s�B	w2B	f2B	[#B	ZQB	[�B	UgB	aB	e`B	YB	PB	H�B	UgB	K^B	L�B	@�B	K�B	NpB	:^B	9$B	B'B	5tB	4�B	5�B	/�B	6�B	<6B	-�B	/B	/B	+�B	*�B	'B	#:B	%B	,B	#�B	-wB	$�B	$tB	6B	�B	~B	
�B	xB		�B	�B	�B		7B	4B	
	B	:B	�B	xB	�B	�B	�B	�B	�B	hB	B	�B	�B	�B	oB	�B	oB	�B	�B	4B	�B	�B	VB	xB	1B	1B	�B	
�B	_B	�B	�B	�B��B	�B	�B	�B		B	qB		B	�B	�B	�B	�B	eB	B	B	�B	�B	�B	SB	+B	YB	#�B	-�B	:�B	0�B	.�B	)�B	.}B	,�B	-�B	,�B	-�B	.�B	.}B	7�B	=�B	G�B	K)B	N<B	^�B	d�B	g�B	e`B	d�B	f�B	ffB	iB	h�B	k�B	m�B	t�B	qB	u�B	u%B	tTB	s�B	sMB	s�B	tTB	~]B	��B	��B	��B	��B	�PB	�"B	��B	�B	�MB	��B	�B	�4B	�$B	��B	��B	�B	�tB	�FB	��B	�zB	�LB	��B	�B	��B	�*B	�XB	��B	��B	��B	��B	��B	��B	��B	�B	�B	�HB	�[B	�B	ǮB	�mB	�mB	��B	ʌB	��B	�^B	��B	�B	�}B	҉B	��B	��B	��B	�NB	�fB	�B	�B	��B	�DB	�B	�
B	��B	��B	��B	��B	�yB	�DB	�yB	�KB	��B	�cB	��B	��B	�JB	��B	��B	��B	��B	�VB
B
 iB	��B	��B	��B	��B
  B
 �B
;B
 �B
  B	�]B	�]B	�]B	��B	��B	�VB
�B
 �B	��B	��B	�PB	��B	�(B	��B
B	�.B
 4B
 �B
  B
 �B
oB
oB
�B
�B
B
�B
�B
B
+B
DB
�B
�B
oB
�B
�B
 'B
B
OB
�B
!B
!B
VB
 �B
!-B
!�B
"�B
"4B
"�B
"4B
"4B
!�B
 'B
 �B
�B
 \B
VB
!bB
 'B
VB
B
~B
B
�B
�B
B
CB
�B
IB
IB
=B
qB
CB
!�B
eB
"hB
#�B
%�B
$B
!bB
"4B
xB
�B
SB
YB
�B
(B
\B
�B
4B
CB
xB
xB
�B	��B	�xB	�	B	��B	��B	��B	�B	�B	�B	��B	�+B	�lB	��B	��B	�xB	��B	��B	�+B	�2G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                            <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<3�=&=�=U*<���<*K<#�
<=7A<v�x<#�
<#�
<�@<��H<�6}<ψ�<#�
<.�<*�<#�
<#�
<�=&D<}p<���=,�=�\<�@�<\�<��d<�&n<���<�%<v4�<U-L<#�
<:��<���<-��<�$&<���<8�%<#�
<#�
<#�
<87�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2020091213301420200912133014IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022400410220210224004102QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022400410220210224004102QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2021042714020020210427140200IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142220230426191422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142220230426191422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142220230426191422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                