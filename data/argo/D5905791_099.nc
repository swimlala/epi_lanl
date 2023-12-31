CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-11-05T08:32:28Z creation; 2022-09-06T18:25:46Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.6   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20211105083228  20220907192126  5905791 5905791 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               c   cAA  AOAO7825_008765_099                 7825_008765_099                 2C  2C  DD  SOLO_II                         SOLO_II                         8765                            8765                            V2.6; SBE602 06Mar19            V2.6; SBE602 06Mar19            853 853 @٠I�_p@٠I�_p11  @٠I�.H�@٠I�.H�@4�q�p�@4�q�p��e,&A�)�e,&A�)11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @�\@B�\@�  @��\@�G�@�  @��RA�RA�RA+�A@��AaG�A�Q�A�Q�A�  A�  A�Q�AϮA߮A�  B   B  B(�B  B   B'�
B/�
B8  B?�
BH(�BP  BW�
B`  Bh(�Bp(�Bx  B�
B�  B�{B��B��B�  B�  B�{B�  B��
B�  B�{B�{B�  B�  B�{B�  B�{B�(�B�{B�{B�{B�{B�{B�=qB�(�B�(�B�(�B�=qB�{B��B��B��
C�C  C  C��C
  C  C
=C
=C��C�C  C{C{C��C�C��C"  C$  C&
=C(  C*  C,  C.  C0{C2  C4  C6{C8  C9�C;��C=��C?��CB{CD  CF  CH
=CJ  CK�CM��CP
=CR  CT
=CV
=CX
=CZ
=C[��C]�C`  Cb
=Cd{Cf{Ch
=Cj
=Cl  Cm��Co��Cr  Ct
=Cv
=Cw��Cz  C|  C}��C�  C�C�C�C���C���C���C�  C�  C�  C�  C�C�C�C�  C�  C���C���C���C���C�C�
=C�  C�  C�  C���C�C�C�  C���C���C�C�
=C�C���C���C�C�  C���C�  C���C���C���C�  C���C���C���C�  C���C���C���C�C�C���C���C�C�C���C���C���C�  C���C���C�  C�  C�C�C���C���C���C���C�C�C�  C�C�  C�  C���C���C���C�C�  C���C�  C���C���C�  C�  C���C�C�
=C�
=C�C���C�  C�
=C�
=C�C�  C�C�C���C���C���C���C�  C���C���C�C�  C�  C�C�
=C�
=C�  C���C���C���C���C�  C���C���C���C�  C�  C�  C�C�
=D �D ��D  D}qD�RD}qD�qD}qD  D��D  D}qD�D��D�D��D�D��D	�D	�D
D
�D�D��D�D�D�D��D�D��D��Dz�D��Dz�D��Dz�D�qD��D  D��D�D�D�D� D��Dz�D  D��D  D}qD  D��D�qD}qD�D��D�qDz�D  D}qD��D� D�qD}qD   D ��D �qD!z�D!�qD"��D#  D#��D$D$� D%�D%��D%�qD&}qD'D'�D(  D(� D)  D)��D*�D*��D*�qD+�D,�D,z�D,�qD-}qD-�qD.� D/  D/� D0  D0� D1  D1��D2  D2}qD3�D3�D4�D4� D5  D5}qD6  D6��D7�D7��D8  D8��D9�D9��D9�qD:}qD:�qD;� D<�D<� D=  D=� D>�D>�D?D?� D@  D@��DA�DA��DB�DB��DC  DC��DD�DD� DD�qDE� DF  DF� DG  DG� DG�qDHz�DH�RDI}qDJ�DJ� DK  DK� DL�DL� DL�qDM}qDM�qDN}qDN��DO� DPDP�DQ�DQ}qDR  DR� DS�DS��DT  DT}qDU  DU��DV  DV� DW�DW� DW�qDX� DY�DY}qDY�qDZ��D[�D[� D\�D\� D\�qD]� D^  D^� D^�qD_}qD_�qD`� DaDa��Da�qDb}qDc�Dc}qDc�qDd� De  De� Df  Df� Dg  Dg� Dh  Dh� Dh��Di� Dj�Dj� Dj��Dk}qDk�qDlz�Dl��Dm� Dn  Dn}qDn�qDoz�Do�qDp��Dq�Dq��Dr�Dr�Ds�Ds��Ds�qDt}qDu  Du}qDu�qDv��Dw�Dw}qDw�qDx� Dx�qDy}qDz  Dz�D{D{�D|D|}qD|��D}}qD~�D~� D~�qD� D��D�AHD�� D�� D�  D�AHD��HD��HD�HD�@ D�� D�� D�  D�@ D�~�D���D�  D�>�D�� D�� D�HD�B�D��HD��HD�  D�=qD�}qD���D�  D�@ D�� D��HD��D�@ D�~�D���D�HD�AHD��HD�� D��qD�@ D�� D�� D�  D�@ D�� D���D�  D�B�D���D�D�  D�>�D�~�D��qD�  D�AHD��HD��HD���D�@ D���D�D��D�@ D�� D�� D�HD�AHD���D���D�HD�B�D���D��HD�  D�>�D�}qD���D�HD�@ D�~�D��HD��D�AHD�� D�� D���D�>�D�}qD�� D�  D�AHD��HD���D�HD�B�D�� D���D�HD�@ D�~�D��HD��D�AHD�� D��HD�  D�>�D�}qD�� D���D�>�D�� D�� D�  D�@ D��HD��HD�  D�>�D�}qD��qD�  D�AHD�~�D���D���D�>�D�� D�� D�HD�>�D�}qD���D���D�>�D�~�D���D�  D�AHD���D���D��D�@ D��HD��HD�HD�B�D���D��HD�  D�>�D�� D��HD�  D�>�D�}qD�� D�HD�B�D��HD��HD�  D�>�D�� D�D��D�>�D�}qD���D�HD�@ D�~�D��qD��qD�>�D�~�D���D�HD�@ D���D�D�HD�AHD�� D���D���D�AHD�� D���D���D�AHD���D�D���D�@ D�� D��qD�  D�AHD�� D�� D�HD�@ D�� D��HD�  D�>�D�� D���D���D�=qD�� D�� D���D�>�D��HD��HD���D�@ D��HD��HD�HD�AHD���D�D��D�>�D�� D�� D��qD�=qD�~�D���D��qD�@ D��HD���D�  D�AHD�~�D��)D��)D�@ D���D��HD���D�>�D�� D�� D�  D�>�D�~�D���D���D�@ D��HD���D�  D�C�D���D��qD��)D�=qD�~�D�� D�HD�B�DÁHD�� D��D�AHDāHD�D�HD�@ DŁHD��HD�  D�B�Dƀ D�� D�HD�AHDǀ DǾ�D�HD�AHD�}qDȽqD���D�>�DɁHD��HD���D�>�D�~�Dʾ�D���D�@ DˁHD�� D�  D�@ D�~�D�� D��D�AHD�~�DͽqD���D�@ D΀ Dξ�D�  D�AHDπ D�� D�HD�>�D�~�Dо�D���D�@ Dр D�� D�  D�@ DҀ D��HD��D�AHDӀ DӾ�D�  D�AHDԁHD�� D�HD�AHDՀ D�� D���D�@ Dր D�� D�  D�@ D׀ D�� D���D�@ D؁HD�� D�  D�>�D�|)D�� D��D�B�Dڂ�D�D�  D�>�Dۀ D��HD�HD�>�D܁HD�D�HD�@ D݂�D�D�  D�@ Dހ D޾�D���D�@ D�}qD�� D�HD�AHD�~�D�� D�  D�>�D� D�� D�  D�@ D� D�� D�  D�AHD�HD㾸D���D�B�D�HD�� D��D�@ D� D��HD�  D�>�D�HD��HD�  D�AHD� D羸D�  D�>�D� D�� D���D�=qD�}qD�� D�  D�=qD�~�D�qD�HD�B�D� D��HD�  D�>�D� D��HD���D�@ D킏D�D�  D�>�D� D�D�HD�AHD� D�� D�  D�@ D�}qD�qD��qD�>�D� D�D��qD�AHD�D�� D�HD�@ D�~�D�D�HD�B�D�HD�� D�HD�@ D�~�D��HD�HD�@ D��HD�� D��qD�=qD�|)D��qD�  D�@ D��HD�D�  D�@ D��HD��HD��D�8R?��?8Q�?aG�?��R?�33?�G�?��@z�@�R@0��@E�@O\)@fff@p��@��@���@�z�@��H@��@���@�z�@�  @�ff@�33@�Q�@��@�@�
=A ��A�A
=qA{Az�AQ�A�RA!�A(��A,(�A2�\A7
=A;�AAG�AE�AK�AN�RATz�AW�A^{Aa�Ag
=Al(�Ao\)Au�AxQ�A}p�A�  A��A���A��A�G�A�(�A�A���A��A�p�A��RA���A�33A�{A��A��\A�z�A�ffA���A��HA�{A��A��\A�(�A�
=A���A��A��A�Q�A��A���A�{A���A��HA�A�\)A�=qA�z�A�ffA���Aڏ\A�p�A�\)A�=qA��
A�RA�  A�A���A�  A�G�A�z�A�A���A��\A�p�A�
=B ��BB
=B(�B�BffB\)B��B	G�B
�HB�B�BB33B�
Bp�B{B�B(�B��BffB�B��Bp�B�HB�B��B��B33B�
B!G�B!�B#\)B$(�B%��B&=qB'�
B(��B)�B*�RB,  B-�B.{B/\)B0z�B1p�B2�RB3�
B5�B5B7\)B8  B9��B:=qB;�B<z�B=�B>�\B@  B@��BBffBC
=BDz�BE�BF�RBG\)BH��BIp�BK
=BK�BM�BM�BO\)BP  BQp�BRffBS�
BT��BU�BV�RBX  BX��BZ{BZ�HB\z�B]�B^�\B_\)B`��Bap�Bb�HBc�Bd��Be��Bg
=Bg�Bi�BiBk33Bk�
BmG�Bm�Bo\)Bp  Bqp�Br{Bs�Bt(�Bu��Bv=qBw�Bxz�ByBz�\B|  B|��B~{B~�RB�  B�ffB��B�p�B�(�B�z�B�33B���B�Q�B��RB�p�B�B��\B���B���B�(�B��RB�p�B�B��\B���B�B�(�B��HB�\)B�  B�z�B�33B��B�Q�B��HB��B�(�B��RB�p�B��B���B��B��B�Q�B�33B��B�ffB��HB��B�(�B���B��B�=qB���B��B�ffB��HB�B�=qB��B���B�z�B���B��B�Q�B�
=B���B�Q�B�
=B���B�ffB��HB��B�(�B��HB���B�(�B�
=B��B�ffB��HB��
B�Q�B�33B��B��\B�\)B��B���B�\)B�(�B���B��B�=qB���B���B�(�B���B��B�(�B���B�p�B�(�B��HB�p�B�=qB��RB���B�{B���B�p�B�ffB��HB�B�ffB��B�  Bȏ\Bə�B�{B�
=B˙�B�z�B�33B��B���B�p�B�ffB���B��
Bҏ\B�G�B�=qBԸRBծB�ffB��B�  B؏\BمB�{B�
=BۮB�z�B�\)B��B���Bߙ�B�z�B�G�B�  B���B�p�B�z�B��B��B�RB�\)B�Q�B���B�B�(�B�
=B뙚B�(�B��HB�G�B�  B�Q�B���B��B�B�{B�z�B���B��B�B��
B�Q�B�z�B�
=B�G�B�B�{B�Q�B���B�
=B���B�B�Q�B�z�B���B�G�B��B�{B�Q�B���B��B���B�B�Q�B��\B�
=B�p�B��B�=qB�ffB��HB�33B���B�  B�=qB��HB���B���B�C (�C Q�C z�C C �
C�C=qCp�C�C��C{C(�Cp�C��CC  C�C\)Cz�C�C��C  CQ�Cp�C��C�HC�CG�C\)C�\C�
C�C=qCQ�C��CC  C=qC\)C��CC  C=qC\)C�CC	{C	33C	z�C	��C	��C
{C
(�C
z�C
��C
�HC{C=qC�C��C�
C�C=qC�C��C�HC{C=qC�\C�C�HC(�C=qC�\C�RC��C�CQ�C��C�RC
=C(�CffC��CC
=C=qC\)C�C�
C{CQ�Cp�C�C�HC
=CQ�Cp�C�RC�HC
=CQ�Cp�C�C�HC
=CQ�Cp�C�C�C
=CQ�Cz�C�C��C{CQ�C�C�C��C�CQ�C��C�C��C(�CQ�C��C�RC��C33CQ�C��C��C�C=qC\)C�C�
C�C33CffC�C��C  C�Cp�C�\CC
=C(�CffC��CC{C33C\)C�C��C 
=C G�C ffC �C ��C!{C!=qC!p�C!�RC!�
C"�C"G�C"�C"��C"�C#33C#ffC#�C#�
C$
=C$33C$z�C$��C$�
C%{C%=qC%�C%�C%�HC&(�C&G�C&��C&��C&�C'=qC'z�C'��C'�C({C(\)C(��C(C)
=C)G�C)ffC)C)�C*�C*p�C*��C*�
C+�C+=qC+�C+C+�C,(�C,ffC,�C,�
C-
=C-33C-�C-��C-�HC.33C.G�C.�\C.�
C.��C/33C/�C/�C/�HC0(�C0Q�C0�\C0�HC1  C1G�C1�C1�C2  C233C2ffC2�RC2�
C3�C3ffC3�C3�
C4{C4=qC4�C4C4�C5=qC5z�C5��C5�C6(�C6G�C6��C6�
C7  C7Q�C7�C7�C7��C8=qC8\)C8��C8�C9
=C9\)C9�\C9C:
=C:Q�C:p�C:C;
=C;33C;p�C;C;�HC<33C<z�C<��C<�HC=33C=\)C=��C=��C>{C>\)C>�C>��C?{C?ffC?�C?C@{C@=qC@z�C@��CA  CA(�CA�CA�RCA�HCB33CBz�CB��CB��CC33CC\)CC�CC�CD�CDffCD�CD��CE(�CEffCE�\CE�HCF�CFQ�CF��CF�HCG{CG\)CG�CG��CH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                        ?�  @�\@B�\@�  @��\@�G�@�  @��RA�RA�RA+�A@��AaG�A�Q�A�Q�A�  A�  A�Q�AϮA߮A�  B   B  B(�B  B   B'�
B/�
B8  B?�
BH(�BP  BW�
B`  Bh(�Bp(�Bx  B�
B�  B�{B��B��B�  B�  B�{B�  B��
B�  B�{B�{B�  B�  B�{B�  B�{B�(�B�{B�{B�{B�{B�{B�=qB�(�B�(�B�(�B�=qB�{B��B��B��
C�C  C  C��C
  C  C
=C
=C��C�C  C{C{C��C�C��C"  C$  C&
=C(  C*  C,  C.  C0{C2  C4  C6{C8  C9�C;��C=��C?��CB{CD  CF  CH
=CJ  CK�CM��CP
=CR  CT
=CV
=CX
=CZ
=C[��C]�C`  Cb
=Cd{Cf{Ch
=Cj
=Cl  Cm��Co��Cr  Ct
=Cv
=Cw��Cz  C|  C}��C�  C�C�C�C���C���C���C�  C�  C�  C�  C�C�C�C�  C�  C���C���C���C���C�C�
=C�  C�  C�  C���C�C�C�  C���C���C�C�
=C�C���C���C�C�  C���C�  C���C���C���C�  C���C���C���C�  C���C���C���C�C�C���C���C�C�C���C���C���C�  C���C���C�  C�  C�C�C���C���C���C���C�C�C�  C�C�  C�  C���C���C���C�C�  C���C�  C���C���C�  C�  C���C�C�
=C�
=C�C���C�  C�
=C�
=C�C�  C�C�C���C���C���C���C�  C���C���C�C�  C�  C�C�
=C�
=C�  C���C���C���C���C�  C���C���C���C�  C�  C�  C�C�
=D �D ��D  D}qD�RD}qD�qD}qD  D��D  D}qD�D��D�D��D�D��D	�D	�D
D
�D�D��D�D�D�D��D�D��D��Dz�D��Dz�D��Dz�D�qD��D  D��D�D�D�D� D��Dz�D  D��D  D}qD  D��D�qD}qD�D��D�qDz�D  D}qD��D� D�qD}qD   D ��D �qD!z�D!�qD"��D#  D#��D$D$� D%�D%��D%�qD&}qD'D'�D(  D(� D)  D)��D*�D*��D*�qD+�D,�D,z�D,�qD-}qD-�qD.� D/  D/� D0  D0� D1  D1��D2  D2}qD3�D3�D4�D4� D5  D5}qD6  D6��D7�D7��D8  D8��D9�D9��D9�qD:}qD:�qD;� D<�D<� D=  D=� D>�D>�D?D?� D@  D@��DA�DA��DB�DB��DC  DC��DD�DD� DD�qDE� DF  DF� DG  DG� DG�qDHz�DH�RDI}qDJ�DJ� DK  DK� DL�DL� DL�qDM}qDM�qDN}qDN��DO� DPDP�DQ�DQ}qDR  DR� DS�DS��DT  DT}qDU  DU��DV  DV� DW�DW� DW�qDX� DY�DY}qDY�qDZ��D[�D[� D\�D\� D\�qD]� D^  D^� D^�qD_}qD_�qD`� DaDa��Da�qDb}qDc�Dc}qDc�qDd� De  De� Df  Df� Dg  Dg� Dh  Dh� Dh��Di� Dj�Dj� Dj��Dk}qDk�qDlz�Dl��Dm� Dn  Dn}qDn�qDoz�Do�qDp��Dq�Dq��Dr�Dr�Ds�Ds��Ds�qDt}qDu  Du}qDu�qDv��Dw�Dw}qDw�qDx� Dx�qDy}qDz  Dz�D{D{�D|D|}qD|��D}}qD~�D~� D~�qD� D��D�AHD�� D�� D�  D�AHD��HD��HD�HD�@ D�� D�� D�  D�@ D�~�D���D�  D�>�D�� D�� D�HD�B�D��HD��HD�  D�=qD�}qD���D�  D�@ D�� D��HD��D�@ D�~�D���D�HD�AHD��HD�� D��qD�@ D�� D�� D�  D�@ D�� D���D�  D�B�D���D�D�  D�>�D�~�D��qD�  D�AHD��HD��HD���D�@ D���D�D��D�@ D�� D�� D�HD�AHD���D���D�HD�B�D���D��HD�  D�>�D�}qD���D�HD�@ D�~�D��HD��D�AHD�� D�� D���D�>�D�}qD�� D�  D�AHD��HD���D�HD�B�D�� D���D�HD�@ D�~�D��HD��D�AHD�� D��HD�  D�>�D�}qD�� D���D�>�D�� D�� D�  D�@ D��HD��HD�  D�>�D�}qD��qD�  D�AHD�~�D���D���D�>�D�� D�� D�HD�>�D�}qD���D���D�>�D�~�D���D�  D�AHD���D���D��D�@ D��HD��HD�HD�B�D���D��HD�  D�>�D�� D��HD�  D�>�D�}qD�� D�HD�B�D��HD��HD�  D�>�D�� D�D��D�>�D�}qD���D�HD�@ D�~�D��qD��qD�>�D�~�D���D�HD�@ D���D�D�HD�AHD�� D���D���D�AHD�� D���D���D�AHD���D�D���D�@ D�� D��qD�  D�AHD�� D�� D�HD�@ D�� D��HD�  D�>�D�� D���D���D�=qD�� D�� D���D�>�D��HD��HD���D�@ D��HD��HD�HD�AHD���D�D��D�>�D�� D�� D��qD�=qD�~�D���D��qD�@ D��HD���D�  D�AHD�~�D��)D��)D�@ D���D��HD���D�>�D�� D�� D�  D�>�D�~�D���D���D�@ D��HD���D�  D�C�D���D��qD��)D�=qD�~�D�� D�HD�B�DÁHD�� D��D�AHDāHD�D�HD�@ DŁHD��HD�  D�B�Dƀ D�� D�HD�AHDǀ DǾ�D�HD�AHD�}qDȽqD���D�>�DɁHD��HD���D�>�D�~�Dʾ�D���D�@ DˁHD�� D�  D�@ D�~�D�� D��D�AHD�~�DͽqD���D�@ D΀ Dξ�D�  D�AHDπ D�� D�HD�>�D�~�Dо�D���D�@ Dр D�� D�  D�@ DҀ D��HD��D�AHDӀ DӾ�D�  D�AHDԁHD�� D�HD�AHDՀ D�� D���D�@ Dր D�� D�  D�@ D׀ D�� D���D�@ D؁HD�� D�  D�>�D�|)D�� D��D�B�Dڂ�D�D�  D�>�Dۀ D��HD�HD�>�D܁HD�D�HD�@ D݂�D�D�  D�@ Dހ D޾�D���D�@ D�}qD�� D�HD�AHD�~�D�� D�  D�>�D� D�� D�  D�@ D� D�� D�  D�AHD�HD㾸D���D�B�D�HD�� D��D�@ D� D��HD�  D�>�D�HD��HD�  D�AHD� D羸D�  D�>�D� D�� D���D�=qD�}qD�� D�  D�=qD�~�D�qD�HD�B�D� D��HD�  D�>�D� D��HD���D�@ D킏D�D�  D�>�D� D�D�HD�AHD� D�� D�  D�@ D�}qD�qD��qD�>�D� D�D��qD�AHD�D�� D�HD�@ D�~�D�D�HD�B�D�HD�� D�HD�@ D�~�D��HD�HD�@ D��HD�� D��qD�=qD�|)D��qD�  D�@ D��HD�D�  D�@ D��HD��HD��G�O�?��?8Q�?aG�?��R?�33?�G�?��@z�@�R@0��@E�@O\)@fff@p��@��@���@�z�@��H@��@���@�z�@�  @�ff@�33@�Q�@��@�@�
=A ��A�A
=qA{Az�AQ�A�RA!�A(��A,(�A2�\A7
=A;�AAG�AE�AK�AN�RATz�AW�A^{Aa�Ag
=Al(�Ao\)Au�AxQ�A}p�A�  A��A���A��A�G�A�(�A�A���A��A�p�A��RA���A�33A�{A��A��\A�z�A�ffA���A��HA�{A��A��\A�(�A�
=A���A��A��A�Q�A��A���A�{A���A��HA�A�\)A�=qA�z�A�ffA���Aڏ\A�p�A�\)A�=qA��
A�RA�  A�A���A�  A�G�A�z�A�A���A��\A�p�A�
=B ��BB
=B(�B�BffB\)B��B	G�B
�HB�B�BB33B�
Bp�B{B�B(�B��BffB�B��Bp�B�HB�B��B��B33B�
B!G�B!�B#\)B$(�B%��B&=qB'�
B(��B)�B*�RB,  B-�B.{B/\)B0z�B1p�B2�RB3�
B5�B5B7\)B8  B9��B:=qB;�B<z�B=�B>�\B@  B@��BBffBC
=BDz�BE�BF�RBG\)BH��BIp�BK
=BK�BM�BM�BO\)BP  BQp�BRffBS�
BT��BU�BV�RBX  BX��BZ{BZ�HB\z�B]�B^�\B_\)B`��Bap�Bb�HBc�Bd��Be��Bg
=Bg�Bi�BiBk33Bk�
BmG�Bm�Bo\)Bp  Bqp�Br{Bs�Bt(�Bu��Bv=qBw�Bxz�ByBz�\B|  B|��B~{B~�RB�  B�ffB��B�p�B�(�B�z�B�33B���B�Q�B��RB�p�B�B��\B���B���B�(�B��RB�p�B�B��\B���B�B�(�B��HB�\)B�  B�z�B�33B��B�Q�B��HB��B�(�B��RB�p�B��B���B��B��B�Q�B�33B��B�ffB��HB��B�(�B���B��B�=qB���B��B�ffB��HB�B�=qB��B���B�z�B���B��B�Q�B�
=B���B�Q�B�
=B���B�ffB��HB��B�(�B��HB���B�(�B�
=B��B�ffB��HB��
B�Q�B�33B��B��\B�\)B��B���B�\)B�(�B���B��B�=qB���B���B�(�B���B��B�(�B���B�p�B�(�B��HB�p�B�=qB��RB���B�{B���B�p�B�ffB��HB�B�ffB��B�  Bȏ\Bə�B�{B�
=B˙�B�z�B�33B��B���B�p�B�ffB���B��
Bҏ\B�G�B�=qBԸRBծB�ffB��B�  B؏\BمB�{B�
=BۮB�z�B�\)B��B���Bߙ�B�z�B�G�B�  B���B�p�B�z�B��B��B�RB�\)B�Q�B���B�B�(�B�
=B뙚B�(�B��HB�G�B�  B�Q�B���B��B�B�{B�z�B���B��B�B��
B�Q�B�z�B�
=B�G�B�B�{B�Q�B���B�
=B���B�B�Q�B�z�B���B�G�B��B�{B�Q�B���B��B���B�B�Q�B��\B�
=B�p�B��B�=qB�ffB��HB�33B���B�  B�=qB��HB���B���B�C (�C Q�C z�C C �
C�C=qCp�C�C��C{C(�Cp�C��CC  C�C\)Cz�C�C��C  CQ�Cp�C��C�HC�CG�C\)C�\C�
C�C=qCQ�C��CC  C=qC\)C��CC  C=qC\)C�CC	{C	33C	z�C	��C	��C
{C
(�C
z�C
��C
�HC{C=qC�C��C�
C�C=qC�C��C�HC{C=qC�\C�C�HC(�C=qC�\C�RC��C�CQ�C��C�RC
=C(�CffC��CC
=C=qC\)C�C�
C{CQ�Cp�C�C�HC
=CQ�Cp�C�RC�HC
=CQ�Cp�C�C�HC
=CQ�Cp�C�C�C
=CQ�Cz�C�C��C{CQ�C�C�C��C�CQ�C��C�C��C(�CQ�C��C�RC��C33CQ�C��C��C�C=qC\)C�C�
C�C33CffC�C��C  C�Cp�C�\CC
=C(�CffC��CC{C33C\)C�C��C 
=C G�C ffC �C ��C!{C!=qC!p�C!�RC!�
C"�C"G�C"�C"��C"�C#33C#ffC#�C#�
C$
=C$33C$z�C$��C$�
C%{C%=qC%�C%�C%�HC&(�C&G�C&��C&��C&�C'=qC'z�C'��C'�C({C(\)C(��C(C)
=C)G�C)ffC)C)�C*�C*p�C*��C*�
C+�C+=qC+�C+C+�C,(�C,ffC,�C,�
C-
=C-33C-�C-��C-�HC.33C.G�C.�\C.�
C.��C/33C/�C/�C/�HC0(�C0Q�C0�\C0�HC1  C1G�C1�C1�C2  C233C2ffC2�RC2�
C3�C3ffC3�C3�
C4{C4=qC4�C4C4�C5=qC5z�C5��C5�C6(�C6G�C6��C6�
C7  C7Q�C7�C7�C7��C8=qC8\)C8��C8�C9
=C9\)C9�\C9C:
=C:Q�C:p�C:C;
=C;33C;p�C;C;�HC<33C<z�C<��C<�HC=33C=\)C=��C=��C>{C>\)C>�C>��C?{C?ffC?�C?C@{C@=qC@z�C@��CA  CA(�CA�CA�RCA�HCB33CBz�CB��CB��CC33CC\)CC�CC�CD�CDffCD�CD��CE(�CEffCE�\CE�HCF�CFQ�CF��CF�HCG{CG\)CG�CG��CH�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                        @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�5?A�7LA�5?A�7LA�33A�5?A�33A�33A�-A�(�A�1'A�-A� �A� �A��A�JA���A��A��A��yA��TA��HA��;A��
A٧�AكA��Aز-A�
=A���A�r�A�%A��yA�p�A�
=AՇ+A�{A��AхA��HA�t�A�S�A�t�A���A�7LA��9A�
=A��A�A��A��+A�O�A� �A���A��!A�C�A�  A��!A�VA���A��DA�ƨA���A�oA��\A�1A�hsA���A�bA�-A���A�C�A��A��A�?}A��-A�/A�l�A�ȴA�33A��`A��RA��A��!A��A��hA�ffA��A��PA���A�jA�&�A��HA���A���A��+A�dZA�/A�hsA�C�A��A�JA��A��A�~�A�G�A��;A���A���A�oA�ƨA�~�A���A��wA�bA��A|��A{�PAz��Ax�AtbAq|�Ao�FAmoAkhsAkS�Aj��Aix�Ag�TAgdZAgt�Ag\)Ag+Af�Af�jAf=qAdAb�+Ab(�Aa��A`�RA`=qA^^5A[;dAYXAV��AU�hAT�ASO�AQl�APffAN�jAMhsAM�AK�-AF��ACXAB1AAp�A@(�A=|�A;�hA:�A8JA6bA5�A3ƨA2E�A1�A.^5A,��A,(�A+��A*��A( �A%�
A$��A$v�A$5?A$1'A$ �A#��A#�PA"�A"�A!hsA ȴA�9A\)A�mAp�A�Av�A��A�A7LA�HA��A�A=qA�A��A�A�#AM�An�A��A
=A�AƨA�7A�TA��A�A�RA�A�A�`AZA
v�A�/A�;A��A�@�33@�5?@�j@��@���@���@�=q@��@���@�@��^@�G�@�%@웦@�F@�33@��y@��@�r�@�I�@�t�@��@��`@��@�@��@�^@�M�@�?}@ާ�@܃@�dZ@�+@���@��@�
=@���@�~�@�$�@��@ղ-@��`@ӕ�@�C�@��y@�@�O�@�/@�1'@�|�@Χ�@��@��`@�z�@�  @�@�G�@���@�hs@�p�@�G�@���@ȴ9@ț�@�A�@�t�@ƸR@�V@�`B@���@å�@�dZ@�@�5?@��@�  @��w@���@�dZ@�o@�{@�p�@�/@��@��@�t�@��y@��!@�~�@�ff@�$�@���@�x�@�`B@�/@���@��9@�Q�@�(�@� �@���@�+@��@�~�@���@��-@���@�G�@���@��!@���@�9X@�33@��@��@�^5@�~�@�M�@�I�@�1'@���@�ƨ@�C�@�M�@�G�@�7L@�G�@���@��@�A�@���@�E�@�|�@�^5@�E�@�5?@�V@�^5@�^5@�V@�M�@�-@�J@�~�@��T@�hs@���@�j@�+@�@��H@���@�E�@�-@��@���@���@�7L@��@���@�Q�@�(�@�1@���@��m@��F@�33@��R@��h@�V@��u@�(�@��@��F@�;d@�@�ff@�=q@�5?@�-@�{@���@��-@���@�O�@��`@�Ĝ@��u@�z�@�  @��F@�l�@�"�@��y@��R@���@���@�v�@�5?@��@���@�p�@�?}@�7L@��@��@���@��D@�  @��w@�S�@���@�~�@�ff@�^5@�E�@�{@���@��T@�x�@�/@���@��@�Q�@��
@�\)@�o@��@��!@��@�hs@��@��`@���@��9@��u@��D@��D@��@�I�@�K�@���@��\@�-@��@��7@�V@���@��j@���@�r�@�Q�@�b@��w@��@���@�|�@�K�@�+@�o@���@���@�~�@�ff@�E�@�-@�{@���@���@�x�@���@�r�@�Z@�Q�@�I�@�Q�@�Q�@�Z@�Z@�bN@�Z@�I�@�1'@�b@�  @���@���@�33@���@��H@�ȴ@��!@��+@�@���@���@���@��h@�p�@�O�@�7L@��u@�(�@���@�\)@�33@��@��H@���@��+@�n�@�ff@�^5@�5?@��@��@��h@�G�@��@��/@�Ĝ@��u@��@�z�@�r�@�bN@�bN@�Q�@��@��@~ȴ@~5?@}��@|�@{C�@z�@y&�@x��@x1'@w|�@v��@v�y@v�R@v$�@u��@u�-@u�h@uO�@t��@t�j@tI�@s�m@sdZ@s"�@r�\@q��@qX@p��@pb@o�w@o�P@nȴ@nv�@nV@n5?@m�T@m�h@l��@lj@l1@k�F@k��@kdZ@k33@k@j�!@jJ@ihs@h��@hQ�@h  @g�;@g�w@f�@f��@fv�@fff@f@e�T@e�@d��@d�D@cƨ@ct�@cS�@c33@c@c@b�H@b��@b��@b��@b��@b~�@b~�@bM�@a�@`�`@`1'@_�@_+@^ȴ@^V@^5?@]�-@]�@\��@\��@\j@\9X@\1@[�
@[t�@Z�H@Z�\@Z=q@Z�@Y��@Y�#@Y��@Yhs@YX@YX@X�`@XbN@W��@W�@V��@VV@U@U�@U�@TI�@S�F@St�@So@R^5@Q�@QX@P�@O�;@O�P@Ol�@O;d@O;d@O;d@O�@O
=@N��@N��@N�@N�R@N�+@Nv�@Nv�@NV@NE�@NE�@NE�@N@M�@L��@LZ@L�@K�F@K�@K33@Ko@J�@J�!@I��@Ix�@I7L@H��@Hr�@H  @G|�@F�y@FE�@E��@E�@C�m@C33@B�@B�H@B�!@A��@A�^@A��@A7L@@bN@@1'@@b@?|�@>��@>�R@>��@>�+@>v�@>V@=��@=V@<��@<��@<j@<9X@;ƨ@;C�@;@:��@:~�@:M�@:M�@:-@9�#@9�^@9G�@8��@8Ĝ@8�u@8A�@7��@7�@7��@7|�@7K�@7�@6ȴ@5�@5V@4�@4�j@4�@4��@4��@4�D@4I�@3�F@3"�@2-@1��@1�7@1G�@0��@0Ĝ@0r�@/�@/��@/\)@/K�@/;d@/�@/
=@.�@.ȴ@.�R@.��@.V@.{@-`B@-�@,z�@,I�@+��@+�@+dZ@+C�@+33@+@*�@*�@*n�@)��@)X@)%@(r�@(bN@(Q�@(Q�@(Q�@(A�@( �@'�w@'l�@&��@%�@%p�@%?}@%V@$�@$�/@$�j@$��@$��@$�D@$�D@$z�@$j@$I�@#��@#ƨ@#S�@"��@"��@"�!@"�\@"^5@!�@!X@!&�@ �`@ Q�@ A�@  �@ b@�;@K�@
=@�y@�@�@�@�@ȴ@�R@�R@�+@E�@{@�T@��@p�@��@(�@��@��@�@S�@o@��@n�@=q@J@�#@�#@�#@��@G�@7L@7L@��@��@r�@b@�@��@��@�P@|�@
=@ff@V@E�@$�@�@z�@Z@I�@I�@I�@9X@9X@9X@9X@�@ƨ@C�@�!@��@n�@n�@M�@M�@M�@=q@=q@-@��@�@�^@�7@hs@X@G�@7L@�@��@Q�@�@��@+@�R@��@��@ff@��@�@O�@?}@/@V@��@��@z�@Z@Z@Z@Z@Z@Z@Z@Z@I�@(�@�
@��@S�@@
�!@
��@
�\@
~�@
^5@
�@	��@	��@	hs@	X@	&�@	�@��@��@��@Q�@b@�@�@|�@K�@+@�@��@��@�y@ȴ@��@�+@�+@v�@{@�@�@�T@�T@��@�@p�@`B@`B@`B@O�@?}@?}A�/A�7LA�33A�9XA�7LA�7LA�;dA�33A�;dA�1'A�5?A�5?A�7LA�7LA�33A�9XA�33A�9XA�-A�7LA�33A�5?A�5?A�33A�5?A�/A�5?A�/A�7LA�/A�1'A�-A�+A�(�A�&�A�/A�-A�5?A�/A�5?A�(�A�-A�+A�+A�$�A��A�"�A��A�"�A��A�"�A��A� �A� �A��A�"�A��A�$�A��A�$�A� �A� �A��A� �A��A��A��A��A��A��A�  A���A���A���A���A���A���A���A���A��A��A��A��A��yA��A��yA��A��yA��A��yA��A��mA��A��yA��A��mA��yA��mA��yA��yA��`A��`A��TA��`A��;A��TA��;A��TA��;A��`A��;A��`A��;A��`A��;A��`A��;A��HA��/A��;A��;A��/A��HA��#A��HA��#A��/A���A���A�ĜA���Aٰ!Aٲ-A٥�A٣�Aٝ�Aٝ�Aٙ�Aٕ�AّhAى7AكA�|�A�v�A�l�A�bNA�K�A�$�A��A�  A���A��`A��;A���A���AؼjAغ^AجAؑhA�ZA�+A�JA�%A�A���A���A���A���A��A��A��
A���A׶FAי�A׏\A׍PA�|�A�z�A�l�A�hsA�^5A�O�A�(�A��A�%A���A��A���A���A���A��A���A��A��A��HA���AָRA֩�A֗�A�|�A�O�A�M�A�?}A�5?A�$�A� �A�{A�oA�A���A��A��;A���Aա�AՕ�AՇ+A�r�A�dZA�Q�A�I�A�E�A�1'A� �A�oA���A��mA���Aԟ�A�=qA�$�A��A�A��yAӝ�A�VAң�A��A�+A��
A�ȴA�Aд9A��A�n�A�?}A�{AθRA͋DA�33A���A�M�Aˏ\A�C�AʬA�(�A��yA���Aɧ�A�A�A�%A���A��Aȇ+A�K�A�A��A��;A�ĜAơ�A�n�A�7LA�AžwA�|�A��AĸRA�|�A��`AA�=qA� �A���A���A��RA���A��uA�~�A�r�A�S�A�=qA�1A���A�?}A���A�ƨA�ĜA���A�`BA�ƨA��A��A�VA�A��A��A���A�|�A�/A��A�"�A���A��A��7A�z�A�v�A�r�A�n�A�hsA�hsA�^5A�A�A��A��TA���A�hsA�E�A�ĜA�O�A��A���A���A�;dA��FA�jA�1'A��;A��A���A��PA�x�A�hsA�bNA�S�A�1'A��A��A��A�bA��A��A��TA���A���A��!A��\A��A�n�A�jA�Q�A�K�A�1'A�"�A��A�1A��A���A�ȴA���A���A��DA�XA�$�A�%A�A��TA�~�A�(�A��HA���A��7A�jA�7LA� �A�$�A��A�VA��A���A��jA���A��\A�x�A�n�A�E�A�;dA�-A�$�A�{A�JA�1A���A��A��`A��A���A��FA���A���A��hA�hsA�A�A�/A�$�A�bA�oA�VA�VA�%A��A�ȴA���A��A���A���A���A��A�`BA�O�A�A�A�-A� �A��A��A��mA���A���A���A���A��A�E�A�$�A��A�oA�%A��A���A�ƨA���A�{A�
=A���A�  A���A���A��A��#A�t�A�n�A�hsA�^5A�^5A�`BA�XA�ZA�ZA�XA�I�A�oA�ZA�I�A�;dA��/A��A��/A��jA��+A�E�A��A�1A���A���A��A��A���A��A��A��#A��A��jA���A��7A�l�A�M�A�/A�{A�  A��A��TA���A���A���A�ĜA�ĜA��RA��!A���A��PA��7A�x�A�r�A�bNA�M�A�G�A�5?A�"�A�A��A��;A��wA��-A���A��uA��+A�n�A�G�A��A�
=A�  A��A��;A���A�ȴA�ȴA���A��RA���A��PA�dZA�I�A�?}A�;dA�33A�1'A�$�A��A�{A�A���A�jA�5?A��A�VA���A��mA�bNA�oA�  A���A��A��HA��;A�ȴA��9A��!A���A��A�hsA�S�A�K�A�C�A�+A��A��A��wA���A��7A�\)A�K�A�?}A�1'A�bA��hA�VA�9XA�-A��A�1A�1A���A��A���A���A��9A���A�I�A��A�ȴA���A��A��jA��RA��-A��uA��A�Q�A�?}A�9XA��A�bA��A�ȴA�p�A�(�A���A�ffA�VA���A��;A���A�`BA���A�z�A�S�A��A��
A��RA��FA��!A��A��A���A��DA�t�A�I�A��A��A�;dA�1A��yA��wA���A�\)A�A���A���A��7A�p�A�`BA�ZA�S�A�M�A�A�A�1'A�9XA�?}A�7LA�33A�$�A��A�1A��A��A��A��/A��TA��`A��;A��TA��TA��HA��;A��HA���A���A���A���A���A���A���A��FA��RA��jA�ƨA���A��hA���A��hA��hA���A��DA��uA��hA�|�A��A�~�A�z�A�x�A�v�A�l�A�l�A�jA�`BA�`BA�ZA�S�A�O�A�I�A�?}A�=qA�7LA�(�A��A��A�
=A���A��A��/A��jA��PA�E�A��A�A�A�XA��A�E�A�"�A��A�VA���A���A���A��HA���A�ĜA��!A���A��\A��A�n�A�`BA�Q�A�G�A�-A�A��HA���A��jA���A��+A�5?A��A��A�r�A��A��RA��+A�M�A��A��RA���A���A��PA�r�A�K�A�$�A�bA�  A�ƨA�hsA�bA��#A��FA���A�z�A�^5A�E�A�A�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                        A�5?A�7LA�5?A�7LA�33A�5?A�33A�33A�-A�(�A�1'A�-A� �A� �A��A�JA���A��A��A��yA��TA��HA��;A��
A٧�AكA��Aز-A�
=A���A�r�A�%A��yA�p�A�
=AՇ+A�{A��AхA��HA�t�A�S�A�t�A���A�7LA��9A�
=A��A�A��A��+A�O�A� �A���A��!A�C�A�  A��!A�VA���A��DA�ƨA���A�oA��\A�1A�hsA���A�bA�-A���A�C�A��A��A�?}A��-A�/A�l�A�ȴA�33A��`A��RA��A��!A��A��hA�ffA��A��PA���A�jA�&�A��HA���A���A��+A�dZA�/A�hsA�C�A��A�JA��A��A�~�A�G�A��;A���A���A�oA�ƨA�~�A���A��wA�bA��A|��A{�PAz��Ax�AtbAq|�Ao�FAmoAkhsAkS�Aj��Aix�Ag�TAgdZAgt�Ag\)Ag+Af�Af�jAf=qAdAb�+Ab(�Aa��A`�RA`=qA^^5A[;dAYXAV��AU�hAT�ASO�AQl�APffAN�jAMhsAM�AK�-AF��ACXAB1AAp�A@(�A=|�A;�hA:�A8JA6bA5�A3ƨA2E�A1�A.^5A,��A,(�A+��A*��A( �A%�
A$��A$v�A$5?A$1'A$ �A#��A#�PA"�A"�A!hsA ȴA�9A\)A�mAp�A�Av�A��A�A7LA�HA��A�A=qA�A��A�A�#AM�An�A��A
=A�AƨA�7A�TA��A�A�RA�A�A�`AZA
v�A�/A�;A��A�@�33@�5?@�j@��@���@���@�=q@��@���@�@��^@�G�@�%@웦@�F@�33@��y@��@�r�@�I�@�t�@��@��`@��@�@��@�^@�M�@�?}@ާ�@܃@�dZ@�+@���@��@�
=@���@�~�@�$�@��@ղ-@��`@ӕ�@�C�@��y@�@�O�@�/@�1'@�|�@Χ�@��@��`@�z�@�  @�@�G�@���@�hs@�p�@�G�@���@ȴ9@ț�@�A�@�t�@ƸR@�V@�`B@���@å�@�dZ@�@�5?@��@�  @��w@���@�dZ@�o@�{@�p�@�/@��@��@�t�@��y@��!@�~�@�ff@�$�@���@�x�@�`B@�/@���@��9@�Q�@�(�@� �@���@�+@��@�~�@���@��-@���@�G�@���@��!@���@�9X@�33@��@��@�^5@�~�@�M�@�I�@�1'@���@�ƨ@�C�@�M�@�G�@�7L@�G�@���@��@�A�@���@�E�@�|�@�^5@�E�@�5?@�V@�^5@�^5@�V@�M�@�-@�J@�~�@��T@�hs@���@�j@�+@�@��H@���@�E�@�-@��@���@���@�7L@��@���@�Q�@�(�@�1@���@��m@��F@�33@��R@��h@�V@��u@�(�@��@��F@�;d@�@�ff@�=q@�5?@�-@�{@���@��-@���@�O�@��`@�Ĝ@��u@�z�@�  @��F@�l�@�"�@��y@��R@���@���@�v�@�5?@��@���@�p�@�?}@�7L@��@��@���@��D@�  @��w@�S�@���@�~�@�ff@�^5@�E�@�{@���@��T@�x�@�/@���@��@�Q�@��
@�\)@�o@��@��!@��@�hs@��@��`@���@��9@��u@��D@��D@��@�I�@�K�@���@��\@�-@��@��7@�V@���@��j@���@�r�@�Q�@�b@��w@��@���@�|�@�K�@�+@�o@���@���@�~�@�ff@�E�@�-@�{@���@���@�x�@���@�r�@�Z@�Q�@�I�@�Q�@�Q�@�Z@�Z@�bN@�Z@�I�@�1'@�b@�  @���@���@�33@���@��H@�ȴ@��!@��+@�@���@���@���@��h@�p�@�O�@�7L@��u@�(�@���@�\)@�33@��@��H@���@��+@�n�@�ff@�^5@�5?@��@��@��h@�G�@��@��/@�Ĝ@��u@��@�z�@�r�@�bN@�bN@�Q�@��@��@~ȴ@~5?@}��@|�@{C�@z�@y&�@x��@x1'@w|�@v��@v�y@v�R@v$�@u��@u�-@u�h@uO�@t��@t�j@tI�@s�m@sdZ@s"�@r�\@q��@qX@p��@pb@o�w@o�P@nȴ@nv�@nV@n5?@m�T@m�h@l��@lj@l1@k�F@k��@kdZ@k33@k@j�!@jJ@ihs@h��@hQ�@h  @g�;@g�w@f�@f��@fv�@fff@f@e�T@e�@d��@d�D@cƨ@ct�@cS�@c33@c@c@b�H@b��@b��@b��@b��@b~�@b~�@bM�@a�@`�`@`1'@_�@_+@^ȴ@^V@^5?@]�-@]�@\��@\��@\j@\9X@\1@[�
@[t�@Z�H@Z�\@Z=q@Z�@Y��@Y�#@Y��@Yhs@YX@YX@X�`@XbN@W��@W�@V��@VV@U@U�@U�@TI�@S�F@St�@So@R^5@Q�@QX@P�@O�;@O�P@Ol�@O;d@O;d@O;d@O�@O
=@N��@N��@N�@N�R@N�+@Nv�@Nv�@NV@NE�@NE�@NE�@N@M�@L��@LZ@L�@K�F@K�@K33@Ko@J�@J�!@I��@Ix�@I7L@H��@Hr�@H  @G|�@F�y@FE�@E��@E�@C�m@C33@B�@B�H@B�!@A��@A�^@A��@A7L@@bN@@1'@@b@?|�@>��@>�R@>��@>�+@>v�@>V@=��@=V@<��@<��@<j@<9X@;ƨ@;C�@;@:��@:~�@:M�@:M�@:-@9�#@9�^@9G�@8��@8Ĝ@8�u@8A�@7��@7�@7��@7|�@7K�@7�@6ȴ@5�@5V@4�@4�j@4�@4��@4��@4�D@4I�@3�F@3"�@2-@1��@1�7@1G�@0��@0Ĝ@0r�@/�@/��@/\)@/K�@/;d@/�@/
=@.�@.ȴ@.�R@.��@.V@.{@-`B@-�@,z�@,I�@+��@+�@+dZ@+C�@+33@+@*�@*�@*n�@)��@)X@)%@(r�@(bN@(Q�@(Q�@(Q�@(A�@( �@'�w@'l�@&��@%�@%p�@%?}@%V@$�@$�/@$�j@$��@$��@$�D@$�D@$z�@$j@$I�@#��@#ƨ@#S�@"��@"��@"�!@"�\@"^5@!�@!X@!&�@ �`@ Q�@ A�@  �@ b@�;@K�@
=@�y@�@�@�@�@ȴ@�R@�R@�+@E�@{@�T@��@p�@��@(�@��@��@�@S�@o@��@n�@=q@J@�#@�#@�#@��@G�@7L@7L@��@��@r�@b@�@��@��@�P@|�@
=@ff@V@E�@$�@�@z�@Z@I�@I�@I�@9X@9X@9X@9X@�@ƨ@C�@�!@��@n�@n�@M�@M�@M�@=q@=q@-@��@�@�^@�7@hs@X@G�@7L@�@��@Q�@�@��@+@�R@��@��@ff@��@�@O�@?}@/@V@��@��@z�@Z@Z@Z@Z@Z@Z@Z@Z@I�@(�@�
@��@S�@@
�!@
��@
�\@
~�@
^5@
�@	��@	��@	hs@	X@	&�@	�@��@��@��@Q�@b@�@�@|�@K�@+@�@��@��@�y@ȴ@��@�+@�+@v�@{@�@�@�T@�T@��@�@p�@`B@`B@`B@O�@?}G�O�A�/A�7LA�33A�9XA�7LA�7LA�;dA�33A�;dA�1'A�5?A�5?A�7LA�7LA�33A�9XA�33A�9XA�-A�7LA�33A�5?A�5?A�33A�5?A�/A�5?A�/A�7LA�/A�1'A�-A�+A�(�A�&�A�/A�-A�5?A�/A�5?A�(�A�-A�+A�+A�$�A��A�"�A��A�"�A��A�"�A��A� �A� �A��A�"�A��A�$�A��A�$�A� �A� �A��A� �A��A��A��A��A��A��A�  A���A���A���A���A���A���A���A���A��A��A��A��A��yA��A��yA��A��yA��A��yA��A��mA��A��yA��A��mA��yA��mA��yA��yA��`A��`A��TA��`A��;A��TA��;A��TA��;A��`A��;A��`A��;A��`A��;A��`A��;A��HA��/A��;A��;A��/A��HA��#A��HA��#A��/A���A���A�ĜA���Aٰ!Aٲ-A٥�A٣�Aٝ�Aٝ�Aٙ�Aٕ�AّhAى7AكA�|�A�v�A�l�A�bNA�K�A�$�A��A�  A���A��`A��;A���A���AؼjAغ^AجAؑhA�ZA�+A�JA�%A�A���A���A���A���A��A��A��
A���A׶FAי�A׏\A׍PA�|�A�z�A�l�A�hsA�^5A�O�A�(�A��A�%A���A��A���A���A���A��A���A��A��A��HA���AָRA֩�A֗�A�|�A�O�A�M�A�?}A�5?A�$�A� �A�{A�oA�A���A��A��;A���Aա�AՕ�AՇ+A�r�A�dZA�Q�A�I�A�E�A�1'A� �A�oA���A��mA���Aԟ�A�=qA�$�A��A�A��yAӝ�A�VAң�A��A�+A��
A�ȴA�Aд9A��A�n�A�?}A�{AθRA͋DA�33A���A�M�Aˏ\A�C�AʬA�(�A��yA���Aɧ�A�A�A�%A���A��Aȇ+A�K�A�A��A��;A�ĜAơ�A�n�A�7LA�AžwA�|�A��AĸRA�|�A��`AA�=qA� �A���A���A��RA���A��uA�~�A�r�A�S�A�=qA�1A���A�?}A���A�ƨA�ĜA���A�`BA�ƨA��A��A�VA�A��A��A���A�|�A�/A��A�"�A���A��A��7A�z�A�v�A�r�A�n�A�hsA�hsA�^5A�A�A��A��TA���A�hsA�E�A�ĜA�O�A��A���A���A�;dA��FA�jA�1'A��;A��A���A��PA�x�A�hsA�bNA�S�A�1'A��A��A��A�bA��A��A��TA���A���A��!A��\A��A�n�A�jA�Q�A�K�A�1'A�"�A��A�1A��A���A�ȴA���A���A��DA�XA�$�A�%A�A��TA�~�A�(�A��HA���A��7A�jA�7LA� �A�$�A��A�VA��A���A��jA���A��\A�x�A�n�A�E�A�;dA�-A�$�A�{A�JA�1A���A��A��`A��A���A��FA���A���A��hA�hsA�A�A�/A�$�A�bA�oA�VA�VA�%A��A�ȴA���A��A���A���A���A��A�`BA�O�A�A�A�-A� �A��A��A��mA���A���A���A���A��A�E�A�$�A��A�oA�%A��A���A�ƨA���A�{A�
=A���A�  A���A���A��A��#A�t�A�n�A�hsA�^5A�^5A�`BA�XA�ZA�ZA�XA�I�A�oA�ZA�I�A�;dA��/A��A��/A��jA��+A�E�A��A�1A���A���A��A��A���A��A��A��#A��A��jA���A��7A�l�A�M�A�/A�{A�  A��A��TA���A���A���A�ĜA�ĜA��RA��!A���A��PA��7A�x�A�r�A�bNA�M�A�G�A�5?A�"�A�A��A��;A��wA��-A���A��uA��+A�n�A�G�A��A�
=A�  A��A��;A���A�ȴA�ȴA���A��RA���A��PA�dZA�I�A�?}A�;dA�33A�1'A�$�A��A�{A�A���A�jA�5?A��A�VA���A��mA�bNA�oA�  A���A��A��HA��;A�ȴA��9A��!A���A��A�hsA�S�A�K�A�C�A�+A��A��A��wA���A��7A�\)A�K�A�?}A�1'A�bA��hA�VA�9XA�-A��A�1A�1A���A��A���A���A��9A���A�I�A��A�ȴA���A��A��jA��RA��-A��uA��A�Q�A�?}A�9XA��A�bA��A�ȴA�p�A�(�A���A�ffA�VA���A��;A���A�`BA���A�z�A�S�A��A��
A��RA��FA��!A��A��A���A��DA�t�A�I�A��A��A�;dA�1A��yA��wA���A�\)A�A���A���A��7A�p�A�`BA�ZA�S�A�M�A�A�A�1'A�9XA�?}A�7LA�33A�$�A��A�1A��A��A��A��/A��TA��`A��;A��TA��TA��HA��;A��HA���A���A���A���A���A���A���A��FA��RA��jA�ƨA���A��hA���A��hA��hA���A��DA��uA��hA�|�A��A�~�A�z�A�x�A�v�A�l�A�l�A�jA�`BA�`BA�ZA�S�A�O�A�I�A�?}A�=qA�7LA�(�A��A��A�
=A���A��A��/A��jA��PA�E�A��A�A�A�XA��A�E�A�"�A��A�VA���A���A���A��HA���A�ĜA��!A���A��\A��A�n�A�`BA�Q�A�G�A�-A�A��HA���A��jA���A��+A�5?A��A��A�r�A��A��RA��+A�M�A��A��RA���A���A��PA�r�A�K�A�$�A�bA�  A�ƨA�hsA�bA��#A��FA���A�z�A�^5A�E�A�A�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                        ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Bv�Bv`Bv�Bv`Bv`Bv�Bv`Bv`Bv�Bv�Bv`Bw2Bv�Bw2Bw2BwfBwfBv�Bw2Bw2BwfBw2Bv�BxB}�B��B�oB�eB��B��B�B��B��B��B�bB�tB�@B��B�<B�B�B�B
�B�B(XB&�B'�B5tB2aBE�B>BB<BC-BFBC�BG�BH�BJ#BK�BL�BMjBMBFtB>B:�B4nB/�B,�B)�B$�B �BB�B�B�B��B�PB��B�oB�B��B��BуB��B�dB�qB��B�rBs�Bg�BP}BL0BI�BHKBFBC�B@�B<B6�BB�B�B��B��B�9B�aB�LB��B�+B~�By�Bt�Bp�B^�BUgBI�B:�B,�B(XB�B{B
�B
��B
�B
ɆB
�B
�tB
�}B
��B
��B
�OB
�}B
�IB
��B
��B
�6B
�tB
�eB
��B
�@B
��B
��B
�iB
iDB
ZQB
C�B
9�B
-�B
'B
B
�B
�B
�B
�B	�JB	�B	רB	��B	�0B	��B	B	�XB	��B	��B	��B	��B	�B	�'B	�7B	��B	��B	��B	�fB	��B	�%B	yrB	u�B	t�B	sB	r�B	rB	q�B	p;B	o B	l�B	iDB	h�B	g�B	c�B	a�B	a�B	YKB	\�B	e�B	g�B	f�B	f2B	i�B	]�B	X�B	YB	Y�B	]�B	^jB	g8B	j�B	kB	d�B	b�B	w�B	v�B	��B	�B	�~B	�oB	��B	��B	��B	�DB	�uB	u�B	iDB	`BB	OBB	PB	O�B	PHB	IB	G�B	B�B	E�B	J#B	K)B	L�B	N<B	M�B	M�B	QB	R�B	R�B	QB	T�B	T�B	S�B	T�B	XyB	Y�B	ZB	[�B	`�B	^B	c�B	gmB	d�B	d�B	aB	_�B	c�B	i�B	f2B	e�B	e�B	e�B	e�B	e�B	g�B	i�B	iyB	jKB	o5B	n�B	ncB	r�B	x�B	{JB	{B	wfB	u�B	v+B	|PB	{B	~(B	�YB	�fB	�rB	�B	�xB	��B	��B	�4B	��B	�:B	��B	��B	�	B	�qB	��B	�B	��B	��B	��B	��B	��B	��B	�CB	��B	�[B	��B	�B	��B	�B	��B	�HB	�B	��B	�3B	ŢB	ƨB	��B	�zB	��B	�^B	�dB	�B	ҽB	�gB	خB	��B	�|B	�TB	��B	�B	�AB	�B

�B
"B
�B
(B
�B
7B
�B
(�B
&�B
%�B
'RB
&�B
%�B
#B
!�B
&�B
,�B
/�B
/�B
/B
.IB
.IB
'�B
"�B
$�B
)�B
-wB
0UB
1�B
1�B
1�B
2-B
4nB
B'B
K^B
M�B
R�B
VmB
W�B
W�B
W�B
XyB
\)B
\�B
]�B
`BB
aB
d�B
g�B
j�B
m�B
poB
u�B
xlB
x�B
y>B
x�B
zxB
z�B
|PB
}�B
~�B
�B
�;B
�B
��B
�rB
�xB
��B
��B
�~B
��B
�\B
��B
��B
�B
��B
��B
��B
�_B
�B
�qB
��B
�OB
�VB
��B
��B
��B
�tB
��B
��B
��B
��B
��B
�0B
��B
�B
��B
��B
�IB
�OB
�'B
��B
��B
��B
�[B
�-B
�3B
��B
��B
�B
�B
��B
��B
��B
��B
��B
�dB
�6B
��B
��B
�OB
�[B
�3B
ƨB
�B
�EB
�B
��B
ǮB
ʌB
��B
˒B
�^B
��B
͟B
�pB
��B
�B
�BB
��B
�B
��B
��B
� B
�TB
�&B
��B
��B
�,B
ԕB
�2B
�B
�B
�mB
�9B
֡B
�EB
�EB
�yB
��B
�B
چB
��B
�#B
�#B
�WB
یB
یB
یB
��B
��B
�)B
�]B
�)B
�)B
��B
ޞB
�jB
ޞB
ޞB
��B
��B
�B
�B
��B
�B
�|B
�HB
�B
��B
�B
�B
�B
�B
�B
�B
�B
��B
�,B
��B
�,B
��B
�2B
�B
�mB
�>B
�
B
��B
�yB
�B
�B
�B
�yB
�B
�B
�yB
�yB
�yB
�yB
�B
�B
�B
��B
��B
� B
�B
��B
�B
�B
�B
��B
�GB
�B
��B
��B
�MB
�B
�B
��B
�B
��B
��B
��B
��B
�+B
��B
��B
�B
�8B
�lB
�>B
�rB
�>B
��B
��B
�B
��B
�B
�B
�B
�B
��B
��B
��B
�PB
��B
�VB
��B
�]B
��B
��B
�.B �B 4B 4B iBBB�B�B�B�B�B�BB�B�B�B�BBBSB�B%BYB�B_B_B�B_BfBfBfB�B
rB	�B	�B
	B
=B
rB
�B
�BBJB�B~B�B�B~BJBJB~BPB�B�B�B�B�B\B�B(B�B�BhB�B�B4BB@B@BuBuB�B�B�BBBBFBFB{B�B�B�BBMB�B�BSB�B+B+B�B�B�B1B1BeBeBkB�BkB�B=B�BqBCBxB�BBB�B�B�B�BOB�B�B�B�BVBVB \B �B!-B!�B!�B!�B!�B#B#nB#nB#�B#�B#�B#�B$B#�B#�B#�B$B#�B$B$tB$tB%B%B%B%FB%�B&LB&B%�B&LB&�B&LB&�B(�B(�B(�B(�B(�B(�B(�B(�B(�B)*B*eB*�B*�B*�B*�B+B*�B+6B+kB,B,B,B,=B,=B,=B,qB,=B,=B,B,=B+�B,�B,qB,�B,�B-B-CB-CB-CB-CB-wB-CB-B-�B.�B.}B.�B/�B/�B/�B/�B/�B/�B/�B/�B0!B0UB1�B1�B1�B2aB2aB2aB2�B2�B2�B2�B2�B2�B2�B2�B33B2�B3�B3�B4B4B4B4nB5B5tB5tB5�B6FB6FB6FB6B6FB6�B6�B7B7B7B7B7B7B7B6�B7B7LB7�B7�B7LB7�B8�B9XB9XB9�B9�B9�B:^B:�B;0B;dB;�B;�B;�B;�B<6B<�B<�B<jB=B<�B=�B=�B=�B=�B>BB>B>B?B?�B?}B?}B?}B@�BA�BA�BA�BA�BA�BA�BA�BA�BA�BA�BB'BB�BCaBCaBC�BC�BC�BC�BC�BC�BC�BC�BD3BD3BDgBD�BD�BD�BD�BD�BD�BE9BE�BF?BFBGzBG�BGzBGEBG�BH�BH�BIBIRBIBI�BIRBI�BJ�BJ�BJXBJ�BJ�BJ�BJ�BJ�BJXBJXBJ�BJ�BK)BK^BK�BLdBLdBLdBL�BLdBL�BMBMjBM�BM�BM�BNBN<BN<BNpBOBOBOBBO�BO�BPHBPBPHBP}BPHBPHBP�BQBQBP�BQBQ�BQ�BQ�BQ�BQ�BR�BR�BR�BR�BR�BS&BS[BS�BS�BxlBv�Bv�Bv`Bv`Bv+Bu�BxlBt�Bx�Bt�BwfBu�Bv�Bw�Bu�Bw�Bu�Bv�Bt�Bv�Bv+Bv�Bw2Bu�Bw�Bu%Bw�Br�Bw�Bu%BxBv�Bv`Bu�Bv`BwfBt�Bw�Bv+BxBu�Bv�BxBwfBw�Bu�Bx8Bu�BxBu�Bw�Bv�Bv�Bw�Bv`Bw�Bv+BxBv`Bx8Bv`By>Bv+BxlBv+Bw�Bv�BtTBr�B~(Bu�Bx8BwfBw�Bw2Bv�Bv�BxBx�Bv�Bv�Bv�BxlBu�BxBu�BxBu�Bx8Bu�BxlBv+Bw�Bv`BxBv�Bw�Bv�Bv�BxBv�Bw�Bv�BxlBv�Bx8Bv`BxBu�Bx8Bv+Bx�Bu�Bx8Bu�BxlBv`Bw�Bv�BwfBw�Bv`Bw�Bv+Bw�Bv+Bx8By>B{BzDB~�B{B~�B}�B~�B~(B~�B�B� B�oB��B��B�B��B��B�B��B�B��B�B��B�MB��B�YB��B��B�1B��B��B��B�-B��B��B�'B�VB��B�VB�\B��B�bB�-B�B��B��B��B��B�OB�VB�qB�CB�~B��B�MB�SB�uB��B�hB�B�B��B��B�$B��B�B��B��B��B�B��B��B��B�-B��B��B��B��B��B�hB�\B��B�bB��B��B�nB�B�tB�LB�@B�B��B�tB�tB�B��B�hB��B��B�!B��B��B�*B�kB��B�'B��B��B��B�BΥB͟B��B�B�B�B�B�B�>B�B�B1B�/B��BB��B�B��B�VB�B�cB�8B��B�B��B
rB0UB	�BfB�BfB�B�B1B"B�B�B�B7�BD�B*�B$�B)�B)*B'RB&�B$@B$�B!bB&LB!bB'RB,�B7B/�B/B*0B.�B0UBQ�B:�B1[B2�B0�B1�B.}B4�B:�B6�B[WBd�BCaB=<BA�B<�B<�B<�B;�B<�B9�B;�B=<B;�BC�BF�B=<B:*BQ�BF�B=<B8RB=qBB'BIRBa�B=<BI�BB[BB�BA�BF?BE�BE�BEBM�BO�BD3BG�BCaBJ�BGzBG�BI�BIRBIBNBL0BM6BI�BOBBJXBM�BK�BJ#BMjBL�BK�BK^BJ#BN�BL�BQBMjBHBGBM�BY�BR�BIRBGzBAUBEmBF�B>B<B>�B@B?HB>�B8�B;�B7LB<�B7LB:�B8RB6�B3�B4nB4B1�B33B2�B5�B1�B1[B/B1'B2�B-�B2�B7�B/�B.�B,qB+�B-wB,=B,=B/�B4�B,B)�B*�B(�B+�B-wB1[B(�B,=B)�B(�B'RB'�B0�B+kB$�B&LB#nB)*B)�B$�B#:B!�B#nB%�B#�B!bB$@B*�BB~BBCBB�B \B�B�B�B!B�B�BBSB�BMB+BIB�B:B	�B)�BxBYB�BB#B.B
�B
	B�B�B	B%B�B�B
	B%B�B1B�B	7BYB+BSBB�B�B�B�cB 4BoB��B 4B  B��B �B�PB  B��B�JB��B��B��B��B�VB�B��B�JB�2B��B�%B�TB��B��B��B��B��B��B�B�vB�B��B��B�5B�5B�B�B��B�yB��B��B�B�yB��B��B�B�B�)B�B��B�B��B�B�2B�)B�WB�KB��BרB�
BچB֡B՛B�9B�
B�KB��B��BѷB�B�BB�sB�pB�B�HB�NBɆB��B�zB�0B�sB��B��B��B�B��B�6B��B�B��B��B��B�nB�pB�UB�!B��B�XB�CB�wB��B�B�B��B��B��B�_B��B��B��B�B�B�_B�:B�7B�B�B�~B�4B��B}�B��B��B��Bw�Bt�Bt�Bt�Br|Bv�Bu�BoiBn/BqvBu�B~]Be�BaHB_;B_�BaHBb�BQ�BR�BRTBS�BPHBN�BMBOvBO�BL�BMBK^BM�BNpBJ�BJ�BO�BDgBK�BK^BMjBGEBK^BJ�BH�BG�BIRBH�BF?BK�BI�BGzBGEBG�BF�BGzBK^BA�BG�BD�BR BEmBC�BD3BD�BC�BEBA�BC�BH�BA�BA�BC�BB�BA�BB[B@�B?�BA�B@�B@B@�B>BB=�B>wB<jB;dB<jB:�B9�B9XB6�B5tB5tB7B2-B7B>BB@OB:�B2-B�B
=B
�BB	7B�BfB�B�B�B�BGB iB �BB�VB��B��B�(B��B�B�`B��B��B�B��B�5B�5B��B��B��BخB�TBܒB�gB�B�B�<B�BBΥB�RB��BŢB�XB��B�mB�B��B�XB�?B��B��B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                        G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                        G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202209061825322022090618253220220906182532202209061825322022090618253220220906182532SI  SI  ARFMARFM                                                                                                                                                2021110508322820211105083228IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021111505010220211115050102QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021111505010220211115050102QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022090607225720220906072257IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022090618253620220906182536IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022090618253620220906182536IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022090618253620220906182536IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                