CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-05-23T10:29:04Z creation; 2023-04-26T19:14:26Z DMQC;      
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
_FillValue        G�O�     �  dt   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � A4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � h�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20180523102904  20230426191426  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7316_008644_013                 7316_008644_013                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�d�P��|@�d�P��|11  @�dӃ{J#@�dӃ{J#@*h{��0@*h{��0�d1>� ���d1>� ��11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�\)?��H@=p�@�  @�  @��R@�  A   A  A\)A,(�A@��A`  A�  A�  A��A��A��A�  A߮A�B   B  B  B  B (�B(  B/�
B8  B@(�BH(�BP  BW�
B`  Bh  Bp  Bx(�B�
B�  B�  B��\B�B�{B�  B��B��
B��B�  B�{B�{B�{B�  B��B�  B��B��B�  B�{B�  B�  B�  B�  B�  B�  B�  B��B�  B�  B��B��C��C��C  C
=C
  C��C�C��C  C��C  C  C��C  C  C   C"
=C$
=C&{C(
=C)��C+��C-��C/��C1��C3�C5�C7�C9�C;�C=�C?�CA�CD  CF
=CH  CJ
=CL{CN
=CO��CR  CT
=CV{CX
=CZ  C[��C]��C_��Cb  Cd{Cf
=Ch
=Cj  Cl  Cn  Co��Cq��Ct
=Cv
=Cx
=Cy��C{�C~  C�  C���C���C���C�  C�  C�  C�C�  C���C�  C�  C�C�  C���C�  C�  C�  C�C�C�
=C�C�  C�C�  C���C���C�  C�  C�C�  C���C�  C�C�  C�  C�  C���C���C���C�  C�  C�  C�  C�C���C�  C�  C�  C���C���C�  C�  C�  C�C�C���C�  C�C�C�  C���C�  C�C�C�C�  C�  C�  C���C���C�  C�  C���C���C�  C�  C���C�  C�C�  C�C�C�  C���C���C���C���C�C�  C�C�  C���C���C�  C�  C���C�  C�  C�  C�C���C���C���C�  C���C�  C�C�  C�  C���C���C���C���C�  C�
=C�
=C�C�C�C�  C�  C�  C�C�
=C�
=C�  C���C���D � D  Dz�D�qD� D�D�D�D� D�qDz�D�qD}qD�qD}qD�qD}qD�qD	� D
�D
� D
�qD� D�qDz�D  D� D�D� D�qD� D�D��D�D��D�D��D�D��D�D��D�D��D�D�D  Dz�D  D�D�D��D�D��D�D��D  D}qD��D}qD  D� D�qD��D   D }qD ��D!� D"�D"}qD"��D#� D$  D$z�D%  D%��D%�qD&� D&�qD'}qD'�qD(}qD(��D)� D*D*�D*�qD+}qD,�D,��D-  D-��D-�qD.� D/  D/� D0�D0}qD0��D1}qD1�qD2� D3�D3� D4  D4� D4�qD5}qD5��D6}qD7�D7��D8D8� D8��D9}qD:  D:��D;  D;}qD<  D<��D=  D=}qD>  D>� D>�qD?}qD?��D@}qDA�DA��DB  DB��DCDC��DD  DD� DE�DE��DF  DF}qDF�qDG� DH�DH��DH�qDI}qDJ�DJ��DK�DK��DL  DL� DM�DM��DN�DN��DN�qDOz�DP  DP�DQ  DQ}qDR�DR��DR�qDS� DT�DT��DU  DU}qDU�qDV� DW  DW� DXDX��DY�DY� DZ  DZ}qDZ�qD[}qD\  D\��D]�D]��D^�D^��D_�D_��D`  D`z�Da  Da��Db  Db}qDb�qDc� Dc�qDd� De�De� De�qDf� Dg�Dg� Dg�qDh� Di  Di� Dj  Dj}qDk  Dk� Dk�qDl� Dm  Dm� Dn�Dn� Do  Do��Dp�Dp}qDp�qDq� Dr�Dr� Ds  Ds� Dt  Dt��Du�Du��Dv�Dv�DwDw��Dx�Dx}qDx�qDy� Dz�Dz}qDz�qD{}qD|  D|��D|�qD}� D~  D~��D�D� D�  D�@ D�~�D���D�  D�@ D�}qD���D�  D�AHD��HD�D�  D�AHD��HD�� D���D�>�D�}qD���D�  D�AHD��HD��qD�  D�AHD�~�D�� D���D�>�D�� D��HD�  D�>�D�~�D��qD�  D�AHD�� D���D���D�>�D�� D���D���D�@ D��HD�� D�  D�@ D�� D�� D�HD�AHD�~�D�� D�HD�@ D�� D��HD�  D�AHD���D��HD�  D�>�D�}qD���D�  D�>�D�}qD���D�HD�@ D�� D��HD�  D�>�D�~�D�� D��D�>�D�~�D��HD�HD�@ D�� D���D���D�@ D��HD�� D�  D�AHD�~�D��qD��qD�>�D�� D��HD���D�>�D�~�D���D���D�@ D�� D��HD�HD�@ D�~�D���D�  D�AHD�� D���D�  D�@ D��HD�D�HD�@ D��HD���D���D�AHD�� D���D�  D�AHD��HD�� D�  D�>�D�� D��HD�HD�AHD�� D��HD�HD�AHD�� D���D�HD�@ D�� D�� D��D�B�D�� D�� D�  D�@ D�� D�� D�  D�>�D�� D�� D�HD�AHD�� D��qD���D�@ D�~�D���D�HD�AHD�~�D���D���D�AHD��HD�� D�HD�B�D�� D�� D���D�>�D�� D��HD�  D�AHD�� D�� D�HD�AHD�� D�� D�  D�>�D�~�D�� D���D�=qD�}qD���D�  D�>�D�~�D�� D�HD�B�D�� D���D���D�>�D�� D���D��qD�>�D�� D���D���D�>�D�~�D�� D�  D�@ D�� D�� D���D�>�D��HD�� D���D�>�D�~�D��HD�  D�=qD�~�D�� D���D�>�D�~�D���D�  D�@ D�� D��HD��D�AHD�~�D���D�  D�>�D�~�D���D���D�@ D�~�D���D�  D�@ D�� D���D�  D�@ D�� D�� D���D�>�D D��HD�HD�@ D�~�Dþ�D���D�>�DĀ D�� D���D�>�Dŀ D�� D�  D�@ D�~�Dƾ�D�  D�@ D�~�DǾ�D�  D�@ D�~�DȾ�D���D�>�D�~�D�� D�HD�@ Dʀ D�� D�HD�AHDˁHD��HD�  D�@ D́HD��HD�  D�>�D�~�D;�D���D�>�D�~�Dξ�D�  D�@ DρHD�� D���D�@ DЀ Dо�D���D�>�DсHD�� D�  D�AHD�~�DҾ�D�  D�@ D�~�DӾ�D�  D�AHDԁHD�� D���D�>�DՁHD�� D���D�>�D�~�D־�D�  D�@ Dׂ�D�� D���D�>�D؀ D�� D��D�AHD�~�Dپ�D���D�>�DځHDھ�D���D�=qDۀ D�� D��qD�=qD܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D��HD�HD�>�D�~�D߾�D���D�@ D�� D�� D�  D�@ D� D�� D�  D�AHD�~�D⾸D�  D�@ D� D��HD�  D�=qD�~�D侸D���D�>�D�~�D徸D�  D�>�D� D�� D���D�>�D�HD��HD�  D�>�D� D�� D���D�@ D�HD�� D�HD�AHD�~�D꾸D�HD�@ D�HD�� D�  D�AHD�~�D쾸D�  D�@ D� D�� D�HD�AHD� D�� D���D�AHD� D�� D�  D�@ D�� D�D�  D�@ D� D�D��D�B�D� D�D���D�>�D�~�D��HD�  D�@ D�HD�D�  D�AHD�� D�� D�HD�AHD�� D�� D�  D�B�D��HD��HD��D�@ D�~�D���D�HD�AHD��HD��HD���D�@ D�~�D���?L��?u?�=q?��R?���?\?��?�@�@\)@��@(��@:�H@L��@W
=@fff@s33@��\@��@�@��H@��\@���@�@�p�@��@���@�@�  @�=q@�@��HAG�AffA(�A  A�
AQ�Ap�A!�A%�A)��A.{A2�\A7�A;�A?\)AC33AHQ�AL��AP��AU�AXQ�A\��AaG�AeAi��Amp�Ap��Atz�Ay��A}p�A�Q�A�=qA�(�A��RA���A��A��A�A��A�G�A��HA�z�A�ffA���A��\A�(�A�A�\)A���A��A�p�A�
=A���A��\A��A�
=A�Q�A�=qA�(�A�ffA�Q�A�=qA��
A�A�  A�=qA�(�A�{A�  A��A�z�AθRAУ�Aҏ\A�(�A�{A�Q�Aڏ\A�z�A�{A߮A��A�(�A�{A�Q�A��A��
A�{A�Q�A�33A��A�\)A�G�A��
A�ffB Q�Bp�BffB�B��B=qB�B��B	B
�HBQ�B��B�HB  B�BffB�B��B{B
=B(�B��B�RB�
B��B{B33B ��B!B"�RB#�B$��B&=qB'�B(z�B)p�B*�RB,  B-p�B.�\B/�B0��B1�B333B4Q�B5p�B6ffB7�B8��B:{B;33B<Q�B=G�B>�\B?�B@��BA�BB�HBC�
BE�BF=qBG33BH  BH��BJ{BK33BLQ�BMG�BN=qBO33BPQ�BQ��BR�RBS�BT��BU�BW
=BX(�BYG�BZffB[33B\z�B]B^�HB`  B`��BaBc
=Bd  Be�BfffBg\)Bhz�BiG�Bj=qBk\)Blz�Bm��Bn�\Bo�Bpz�Bqp�Br�\Bs�Bt��Bu��BvffBw33BxQ�Byp�Bz=qB{33B|(�B|��B}B~�\B�B�=qB���B�G�B��B�(�B��\B���B�G�B��
B�Q�B���B�G�B�B�(�B���B��B��B�{B���B�33B��B�{B�z�B���B�
=B�\)B���B�B�  B�(�B�(�B�=qB�=qB�Q�B�Q�B�Q�B�ffB�z�B��\B���B���B��RB���B��RB���B���B�
=B�
=B��B��B�33B�G�B�\)B��B���B��B�B�B��
B��B�{B�(�B�Q�B�z�B��\B���B��RB��HB��HB�
=B��B�\)B�p�B��B��
B�  B�{B�(�B�Q�B�ffB���B��HB�
=B�G�B�p�B��B�B�  B�(�B�Q�B�z�B��RB��HB�33B�\)B���B��
B��B�{B�Q�B�z�B���B���B�
=B�33B�\)B���B�B�  B�=qB�ffB���B���B���B�33B�\)B��B��B��
B�  B�=qB�ffB���B��HB��B�G�B��B��B��
B�{B�Q�B�z�B��RB��HB�
=B�33B�p�B��B��B�(�B�Q�B��\B���B��B�G�B��B�B�  B�=qB��\B���B�
=B�\)B���B��
B�(�B�z�B��RB�
=B�\)B��B�  B�Q�B���B�
=B�G�B��B�  B�ffB��RB�
=B�p�B�B�{B�ffB���B��B��B�B�(�B�z�B��HB�G�B���B�  B�ffB��RB�33B��B��B�=qB���B�
=B�p�B��
B�(�B���B�
=B�\)B��
B�=qB���B��B��B��B�Q�B��RB��B��B��
B�=qB���B���B�p�B�B�(�B��\B���B�\)B��B�{B�z�B��HB�\)B�B�=qB���B��B���B�{B��\B�
=B�p�B��B�Q�B���B�33B���B�{B��\B�
=B��B��B�Q�B¸RB��BÅB��B�z�B���B�p�B��B�Q�B���B�33BǙ�B�(�Bȣ�B�33BɮB�{Bʏ\B���B�p�B�  B̏\B�
=BͅB��B�z�B�
=Bϙ�B�(�BУ�B��Bљ�B�=qB���B�\)B��
B�Q�B��HBՅB�{B֣�B��B׮B�=qB��HB�p�B��B�z�B���BۅB�(�BܸRB�G�B�B�Q�B���Bߙ�B�(�B�RB�G�B��
B�ffB��B�B�Q�B��HB�B�(�B���B�p�B�{B��B�33B��
B�z�B��B�B�Q�B��HB�p�B�{B��B�\)B�  B��\B��B�B�z�B�33B�B�Q�B��HB���B�=qB��HB�p�B�(�B��HB��B�(�B���B�\)B�  B��RB�p�B�{B���B�G�B��C Q�C ��C �C=qC�C�HC=qC�\C��C�C�C�
C{CffCC{CffC�C  CQ�C�C  CG�C��C��CQ�C��C�HC	=qC	��C	��C
=qC
�\C
�HC=qC��C�HC33C�\C�C33Cz�C�
C33C�C��C�Cz�C�
C(�Cp�CC{Cp�CC{C\)C�RC{Cp�C�RC  C\)C�RC
=CQ�C��C��CQ�C��C��CG�C��C��CG�C�C�HCG�C��C�HC33C�\C�C=qC�\C�HC=qC��C�C=qC�\C�CG�C�\C�
C33C�\C�HC (�C z�C �
C!(�C!p�C!��C"(�C"z�C"C#{C#p�C#��C$(�C$p�C$C%(�C%�C%��C&�C&�C&�
C'(�C'z�C'�
C(33C(��C(�C)=qC)��C*  C*\)C*�C+
=C+p�C+C,�C,�C,�HC-33C-��C.  C.Q�C.��C/
=C/p�C/C0�C0�C0�HC133C1�\C1��C2Q�C2��C3  C3p�C3C4�C4�C4�
C5(�C5��C5��C6G�C6��C7
=C7\)C7�RC8�C8z�C8��C9(�C9��C9��C:=qC:��C;
=C;\)C;�C<{C<z�C<��C=(�C=�\C=�C>=qC>��C?
=C?ffC?�RC@(�C@z�C@��CA33CA��CA��CB=qCB��CC
=CCffCC�RCD{CDz�CD�HCE=qCE�\CE�CFQ�CF�CG
=CGffCG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                            ?�\)?��H@=p�@�  @�  @��R@�  A   A  A\)A,(�A@��A`  A�  A�  A��A��A��A�  A߮A�B   B  B  B  B (�B(  B/�
B8  B@(�BH(�BP  BW�
B`  Bh  Bp  Bx(�B�
B�  B�  B��\B�B�{B�  B��B��
B��B�  B�{B�{B�{B�  B��B�  B��B��B�  B�{B�  B�  B�  B�  B�  B�  B�  B��B�  B�  B��B��C��C��C  C
=C
  C��C�C��C  C��C  C  C��C  C  C   C"
=C$
=C&{C(
=C)��C+��C-��C/��C1��C3�C5�C7�C9�C;�C=�C?�CA�CD  CF
=CH  CJ
=CL{CN
=CO��CR  CT
=CV{CX
=CZ  C[��C]��C_��Cb  Cd{Cf
=Ch
=Cj  Cl  Cn  Co��Cq��Ct
=Cv
=Cx
=Cy��C{�C~  C�  C���C���C���C�  C�  C�  C�C�  C���C�  C�  C�C�  C���C�  C�  C�  C�C�C�
=C�C�  C�C�  C���C���C�  C�  C�C�  C���C�  C�C�  C�  C�  C���C���C���C�  C�  C�  C�  C�C���C�  C�  C�  C���C���C�  C�  C�  C�C�C���C�  C�C�C�  C���C�  C�C�C�C�  C�  C�  C���C���C�  C�  C���C���C�  C�  C���C�  C�C�  C�C�C�  C���C���C���C���C�C�  C�C�  C���C���C�  C�  C���C�  C�  C�  C�C���C���C���C�  C���C�  C�C�  C�  C���C���C���C���C�  C�
=C�
=C�C�C�C�  C�  C�  C�C�
=C�
=C�  C���C���D � D  Dz�D�qD� D�D�D�D� D�qDz�D�qD}qD�qD}qD�qD}qD�qD	� D
�D
� D
�qD� D�qDz�D  D� D�D� D�qD� D�D��D�D��D�D��D�D��D�D��D�D��D�D�D  Dz�D  D�D�D��D�D��D�D��D  D}qD��D}qD  D� D�qD��D   D }qD ��D!� D"�D"}qD"��D#� D$  D$z�D%  D%��D%�qD&� D&�qD'}qD'�qD(}qD(��D)� D*D*�D*�qD+}qD,�D,��D-  D-��D-�qD.� D/  D/� D0�D0}qD0��D1}qD1�qD2� D3�D3� D4  D4� D4�qD5}qD5��D6}qD7�D7��D8D8� D8��D9}qD:  D:��D;  D;}qD<  D<��D=  D=}qD>  D>� D>�qD?}qD?��D@}qDA�DA��DB  DB��DCDC��DD  DD� DE�DE��DF  DF}qDF�qDG� DH�DH��DH�qDI}qDJ�DJ��DK�DK��DL  DL� DM�DM��DN�DN��DN�qDOz�DP  DP�DQ  DQ}qDR�DR��DR�qDS� DT�DT��DU  DU}qDU�qDV� DW  DW� DXDX��DY�DY� DZ  DZ}qDZ�qD[}qD\  D\��D]�D]��D^�D^��D_�D_��D`  D`z�Da  Da��Db  Db}qDb�qDc� Dc�qDd� De�De� De�qDf� Dg�Dg� Dg�qDh� Di  Di� Dj  Dj}qDk  Dk� Dk�qDl� Dm  Dm� Dn�Dn� Do  Do��Dp�Dp}qDp�qDq� Dr�Dr� Ds  Ds� Dt  Dt��Du�Du��Dv�Dv�DwDw��Dx�Dx}qDx�qDy� Dz�Dz}qDz�qD{}qD|  D|��D|�qD}� D~  D~��D�D� D�  D�@ D�~�D���D�  D�@ D�}qD���D�  D�AHD��HD�D�  D�AHD��HD�� D���D�>�D�}qD���D�  D�AHD��HD��qD�  D�AHD�~�D�� D���D�>�D�� D��HD�  D�>�D�~�D��qD�  D�AHD�� D���D���D�>�D�� D���D���D�@ D��HD�� D�  D�@ D�� D�� D�HD�AHD�~�D�� D�HD�@ D�� D��HD�  D�AHD���D��HD�  D�>�D�}qD���D�  D�>�D�}qD���D�HD�@ D�� D��HD�  D�>�D�~�D�� D��D�>�D�~�D��HD�HD�@ D�� D���D���D�@ D��HD�� D�  D�AHD�~�D��qD��qD�>�D�� D��HD���D�>�D�~�D���D���D�@ D�� D��HD�HD�@ D�~�D���D�  D�AHD�� D���D�  D�@ D��HD�D�HD�@ D��HD���D���D�AHD�� D���D�  D�AHD��HD�� D�  D�>�D�� D��HD�HD�AHD�� D��HD�HD�AHD�� D���D�HD�@ D�� D�� D��D�B�D�� D�� D�  D�@ D�� D�� D�  D�>�D�� D�� D�HD�AHD�� D��qD���D�@ D�~�D���D�HD�AHD�~�D���D���D�AHD��HD�� D�HD�B�D�� D�� D���D�>�D�� D��HD�  D�AHD�� D�� D�HD�AHD�� D�� D�  D�>�D�~�D�� D���D�=qD�}qD���D�  D�>�D�~�D�� D�HD�B�D�� D���D���D�>�D�� D���D��qD�>�D�� D���D���D�>�D�~�D�� D�  D�@ D�� D�� D���D�>�D��HD�� D���D�>�D�~�D��HD�  D�=qD�~�D�� D���D�>�D�~�D���D�  D�@ D�� D��HD��D�AHD�~�D���D�  D�>�D�~�D���D���D�@ D�~�D���D�  D�@ D�� D���D�  D�@ D�� D�� D���D�>�D D��HD�HD�@ D�~�Dþ�D���D�>�DĀ D�� D���D�>�Dŀ D�� D�  D�@ D�~�Dƾ�D�  D�@ D�~�DǾ�D�  D�@ D�~�DȾ�D���D�>�D�~�D�� D�HD�@ Dʀ D�� D�HD�AHDˁHD��HD�  D�@ D́HD��HD�  D�>�D�~�D;�D���D�>�D�~�Dξ�D�  D�@ DρHD�� D���D�@ DЀ Dо�D���D�>�DсHD�� D�  D�AHD�~�DҾ�D�  D�@ D�~�DӾ�D�  D�AHDԁHD�� D���D�>�DՁHD�� D���D�>�D�~�D־�D�  D�@ Dׂ�D�� D���D�>�D؀ D�� D��D�AHD�~�Dپ�D���D�>�DځHDھ�D���D�=qDۀ D�� D��qD�=qD܀ D�� D�  D�@ D݀ D�� D�  D�@ Dހ D��HD�HD�>�D�~�D߾�D���D�@ D�� D�� D�  D�@ D� D�� D�  D�AHD�~�D⾸D�  D�@ D� D��HD�  D�=qD�~�D侸D���D�>�D�~�D徸D�  D�>�D� D�� D���D�>�D�HD��HD�  D�>�D� D�� D���D�@ D�HD�� D�HD�AHD�~�D꾸D�HD�@ D�HD�� D�  D�AHD�~�D쾸D�  D�@ D� D�� D�HD�AHD� D�� D���D�AHD� D�� D�  D�@ D�� D�D�  D�@ D� D�D��D�B�D� D�D���D�>�D�~�D��HD�  D�@ D�HD�D�  D�AHD�� D�� D�HD�AHD�� D�� D�  D�B�D��HD��HD��D�@ D�~�D���D�HD�AHD��HD��HD���D�@ D�~�G�O�?L��?u?�=q?��R?���?\?��?�@�@\)@��@(��@:�H@L��@W
=@fff@s33@��\@��@�@��H@��\@���@�@�p�@��@���@�@�  @�=q@�@��HAG�AffA(�A  A�
AQ�Ap�A!�A%�A)��A.{A2�\A7�A;�A?\)AC33AHQ�AL��AP��AU�AXQ�A\��AaG�AeAi��Amp�Ap��Atz�Ay��A}p�A�Q�A�=qA�(�A��RA���A��A��A�A��A�G�A��HA�z�A�ffA���A��\A�(�A�A�\)A���A��A�p�A�
=A���A��\A��A�
=A�Q�A�=qA�(�A�ffA�Q�A�=qA��
A�A�  A�=qA�(�A�{A�  A��A�z�AθRAУ�Aҏ\A�(�A�{A�Q�Aڏ\A�z�A�{A߮A��A�(�A�{A�Q�A��A��
A�{A�Q�A�33A��A�\)A�G�A��
A�ffB Q�Bp�BffB�B��B=qB�B��B	B
�HBQ�B��B�HB  B�BffB�B��B{B
=B(�B��B�RB�
B��B{B33B ��B!B"�RB#�B$��B&=qB'�B(z�B)p�B*�RB,  B-p�B.�\B/�B0��B1�B333B4Q�B5p�B6ffB7�B8��B:{B;33B<Q�B=G�B>�\B?�B@��BA�BB�HBC�
BE�BF=qBG33BH  BH��BJ{BK33BLQ�BMG�BN=qBO33BPQ�BQ��BR�RBS�BT��BU�BW
=BX(�BYG�BZffB[33B\z�B]B^�HB`  B`��BaBc
=Bd  Be�BfffBg\)Bhz�BiG�Bj=qBk\)Blz�Bm��Bn�\Bo�Bpz�Bqp�Br�\Bs�Bt��Bu��BvffBw33BxQ�Byp�Bz=qB{33B|(�B|��B}B~�\B�B�=qB���B�G�B��B�(�B��\B���B�G�B��
B�Q�B���B�G�B�B�(�B���B��B��B�{B���B�33B��B�{B�z�B���B�
=B�\)B���B�B�  B�(�B�(�B�=qB�=qB�Q�B�Q�B�Q�B�ffB�z�B��\B���B���B��RB���B��RB���B���B�
=B�
=B��B��B�33B�G�B�\)B��B���B��B�B�B��
B��B�{B�(�B�Q�B�z�B��\B���B��RB��HB��HB�
=B��B�\)B�p�B��B��
B�  B�{B�(�B�Q�B�ffB���B��HB�
=B�G�B�p�B��B�B�  B�(�B�Q�B�z�B��RB��HB�33B�\)B���B��
B��B�{B�Q�B�z�B���B���B�
=B�33B�\)B���B�B�  B�=qB�ffB���B���B���B�33B�\)B��B��B��
B�  B�=qB�ffB���B��HB��B�G�B��B��B��
B�{B�Q�B�z�B��RB��HB�
=B�33B�p�B��B��B�(�B�Q�B��\B���B��B�G�B��B�B�  B�=qB��\B���B�
=B�\)B���B��
B�(�B�z�B��RB�
=B�\)B��B�  B�Q�B���B�
=B�G�B��B�  B�ffB��RB�
=B�p�B�B�{B�ffB���B��B��B�B�(�B�z�B��HB�G�B���B�  B�ffB��RB�33B��B��B�=qB���B�
=B�p�B��
B�(�B���B�
=B�\)B��
B�=qB���B��B��B��B�Q�B��RB��B��B��
B�=qB���B���B�p�B�B�(�B��\B���B�\)B��B�{B�z�B��HB�\)B�B�=qB���B��B���B�{B��\B�
=B�p�B��B�Q�B���B�33B���B�{B��\B�
=B��B��B�Q�B¸RB��BÅB��B�z�B���B�p�B��B�Q�B���B�33BǙ�B�(�Bȣ�B�33BɮB�{Bʏ\B���B�p�B�  B̏\B�
=BͅB��B�z�B�
=Bϙ�B�(�BУ�B��Bљ�B�=qB���B�\)B��
B�Q�B��HBՅB�{B֣�B��B׮B�=qB��HB�p�B��B�z�B���BۅB�(�BܸRB�G�B�B�Q�B���Bߙ�B�(�B�RB�G�B��
B�ffB��B�B�Q�B��HB�B�(�B���B�p�B�{B��B�33B��
B�z�B��B�B�Q�B��HB�p�B�{B��B�\)B�  B��\B��B�B�z�B�33B�B�Q�B��HB���B�=qB��HB�p�B�(�B��HB��B�(�B���B�\)B�  B��RB�p�B�{B���B�G�B��C Q�C ��C �C=qC�C�HC=qC�\C��C�C�C�
C{CffCC{CffC�C  CQ�C�C  CG�C��C��CQ�C��C�HC	=qC	��C	��C
=qC
�\C
�HC=qC��C�HC33C�\C�C33Cz�C�
C33C�C��C�Cz�C�
C(�Cp�CC{Cp�CC{C\)C�RC{Cp�C�RC  C\)C�RC
=CQ�C��C��CQ�C��C��CG�C��C��CG�C�C�HCG�C��C�HC33C�\C�C=qC�\C�HC=qC��C�C=qC�\C�CG�C�\C�
C33C�\C�HC (�C z�C �
C!(�C!p�C!��C"(�C"z�C"C#{C#p�C#��C$(�C$p�C$C%(�C%�C%��C&�C&�C&�
C'(�C'z�C'�
C(33C(��C(�C)=qC)��C*  C*\)C*�C+
=C+p�C+C,�C,�C,�HC-33C-��C.  C.Q�C.��C/
=C/p�C/C0�C0�C0�HC133C1�\C1��C2Q�C2��C3  C3p�C3C4�C4�C4�
C5(�C5��C5��C6G�C6��C7
=C7\)C7�RC8�C8z�C8��C9(�C9��C9��C:=qC:��C;
=C;\)C;�C<{C<z�C<��C=(�C=�\C=�C>=qC>��C?
=C?ffC?�RC@(�C@z�C@��CA33CA��CA��CB=qCB��CC
=CCffCC�RCD{CDz�CD�HCE=qCE�\CE�CFQ�CF�CG
=CGffCG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                            @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�o@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�K�A�G�A�Q�A�XA�XA�VA�ZA�^5A�`BA�^5A�^5A�`BA�bNA�bNA�dZA�dZA�ffA�ffA�ffA�ffA�ffA�hsA�jA�jA�dZA�S�A�=qA�C�A�1'A�$�A�&�A�-A��A�1A�%A��A��TA���Aٴ9A�`BAэPAЍPA̍PA��A�%AÕ�A�K�A�9XA���A��9A�M�A�v�A�+A��+A�v�A���A�v�A��A�A��A�33A�;dA�/A���A� �A"�Axr�At�Ao��Ak��Ai`BAg&�Aa|�A\I�AYVAV�yAT��AS�AS��ARȴARJAO�
ANI�AK\)AI�AG�TAE\)AC�FABJAA/A>�jA=dZA<ĜA<bA:�yA:�A:�A8��A8ffA81A7�PA7
=A6�A5��A4A�A2�RA1�A/��A/S�A/%A.A�A-�-A,bNA,v�A.�A-��A-�
A,ȴA+��A+C�A+%A*E�A*{A)��A'A&�A&��A';dA'l�A';dA&��A&�uA&$�A%A%��A%�A%O�A%%A$��A$5?A#ƨA#\)A"�HA"ȴA"��A"Q�A!��A!oA ��A �RA -A   A�PA33A�jA�A�A�FA�9AQ�A�mA\)A��A�yA�A�jAr�A�
A`BA��A�A�Ax�AVA�`A��A�A/A��A��A�+AjA9XA��A�wA�AhsA%AĜAjA$�A��A��A�7AhsA?}A�A�A�AbNA(�A�A1A�A��AVAz�A(�AXAl�AO�AVAn�AAC�AoA
�A
��A
ffA	�TA	G�A	VAz�A��A��A7LA��AĜAȴAȴA�jAv�AbAp�A;dAoA�/A{A�A��AG�A%A�`A��A{A��AC�A
=A ��A ��A Z@��
@��y@��-@��@��w@�
=@�~�@�@�O�@�?}@��`@���@�b@���@�S�@�~�@�@��j@��;@���@�r�@�l�@��@�n�@�@��#@�D@�C�@�!@�v�@��@�@�x�@��@�C�@��T@�G�@��@�D@�A�@��@�(�@�D@�1'@�l�@���@�E�@���@���@��@�@�hs@��@���@�@߮@�33@��H@އ+@���@ݩ�@��/@��m@�dZ@�@�ff@�J@ٺ^@�X@�1'@ץ�@��H@�v�@ղ-@���@�9X@��
@�dZ@�33@���@�{@���@Ѳ-@��`@� �@��
@υ@��H@�@�O�@�%@�1'@���@�5?@�-@���@���@ə�@�p�@�&�@���@ȴ9@�A�@�  @ǝ�@�33@��H@ũ�@�7L@�V@Ĵ9@�Z@�b@þw@��@�v�@�E�@��#@��@�`B@�7L@���@�I�@�K�@��R@��@��#@�x�@��@��u@��@�|�@��y@�ȴ@���@�^5@�{@�@��T@��h@�V@��`@���@��j@��@���@���@�1@��@�\)@�"�@�E�@���@��-@�7L@�V@���@��@�z�@�A�@�  @��@�C�@�
=@��\@���@���@���@��7@��7@�p�@�/@��/@�z�@��m@�dZ@�+@��@��+@�v�@�ff@�$�@�p�@�O�@��@��u@�bN@�Z@�9X@�  @��
@�C�@��@���@��!@�^5@�J@���@�hs@�&�@���@��`@��9@�9X@��F@�\)@���@��+@�=q@���@��h@�7L@���@��/@���@�Q�@���@�;d@��@�ȴ@��R@��!@���@�5?@��-@�?}@��/@���@��9@�r�@�1'@�1@��@�;d@��y@���@���@�V@��@���@�x�@��@���@��D@�9X@�  @��F@�
=@��y@��R@�E�@�{@��@�x�@��/@��@�I�@�(�@��
@���@�t�@�33@�@��y@���@�^5@�J@��@���@�O�@�/@��@�I�@��@�ƨ@�C�@��@��\@�E�@�$�@�@���@��-@�O�@�?}@���@��9@���@��@�9X@��@�C�@���@�M�@�5?@�$�@��#@��^@���@�hs@�/@�Ĝ@�z�@��@�b@�  @�ƨ@�l�@�\)@�
=@��y@��y@���@�M�@��@���@�@��@�?}@���@��@��@��w@��@�dZ@�33@�v�@�V@�5?@���@��7@�hs@�7L@��`@���@��@�Z@�(�@��;@���@�|�@�dZ@��@���@�n�@�5?@�-@�J@��T@�p�@�V@��`@��j@��@�Q�@� �@�  @�w@~�y@~5?@}�-@}�@|�D@{�
@{o@z�@z�\@y��@yG�@x��@xQ�@w;d@v��@v{@u�h@u/@t�@t��@tI�@s�m@s��@so@rM�@q�@q��@q��@q�7@q7L@q&�@q�@p��@pA�@o�w@o;d@nȴ@n�+@nV@n5?@m�T@m�h@m/@l�@l�j@l��@l(�@k�m@kƨ@kS�@j�!@j�@ix�@i�@h��@hbN@h1'@g�;@g��@gK�@f�@fff@e�T@e��@eV@d1@c�F@ct�@cC�@c33@c@bn�@a��@a�@a�@a��@a7L@`r�@`A�@_�@_+@^{@]`B@\��@\j@[ƨ@[dZ@Z�@Z^5@ZJ@Yhs@Y�@X�u@X1'@W�;@W|�@W+@V�y@V�R@U��@T�@TI�@T9X@S��@R�\@R�\@RM�@Q��@Qx�@Q7L@Q%@P��@P��@P�u@Pr�@P �@O�@Ol�@Nȴ@N{@M��@MO�@M�@L�@K�m@KC�@J~�@I��@I��@I��@I�7@Ix�@IX@IG�@I7L@I�@I%@HĜ@HQ�@G�;@G�w@G;d@F�@Fff@E�T@E�h@EO�@D�j@DZ@C�m@C�F@C�@CC�@C@B�\@B=q@A��@A�#@A��@AG�@A%@@��@@Q�@@1'@?�P@>�y@>v�@>5?@>$�@>$�@>{@>@=�-@=�@<z�@<(�@<(�@<�@;�F@;33@:��@:n�@9��@9hs@9&�@9�@8�`@8��@8r�@8  @7�@7|�@7�@6�y@6��@6v�@6ff@65?@5��@5�h@5?}@4�@4�j@4�D@4�D@4j@4�@3��@3S�@3C�@3"�@2�H@2n�@2�@1�@1��@1�@0��@0bN@01'@0 �@0  @/��@/��@/;d@.�y@.��@.$�@.$�@.@.@.@-�T@-�@-��@-`B@-�@,��@,�@,9X@,�@,�@+��@+t�@+33@+"�@+@+@*�\@*J@*�@*�@)�^@)X@(Ĝ@(�u@(�@(b@'�;@'��@'�@'l�@'K�@'�@&�R@&�+@&v�@&V@&@%�-@%�@%p�@%/@$�/@$Z@$�@#�m@#�m@#ƨ@#�@#@"��@"�\@"M�@"-@!�#@!�^@!x�@!G�@!7L@!&�@ ��@ Ĝ@ �9@ �@  �@\)@ff@�@�-@�-@p�@?}@`B@p�@V@��@Z@9X@I�@Z@�@�@n�@M�@J@�7@��@x�@7L@%@��@�u@�@bN@1'@�@��@�@�@|�@K�@
=@��@{@��@p�@?}@V@�/@��@��@��@��@9X@��@��@�F@S�@S�@dZ@33@@�\@n�@^5@M�@^5@^5@M�@=q@M�@�@��@�@��@��@�7@hs@7L@&�@7L@&�@%@Ĝ@r�@A�@�@��@�w@|�@l�@;d@+@�@�y@ȴ@�R@��@v�@E�@$�@�T@@��@�@`B@O�@?}@?}A�G�A�M�A�O�A�K�A�G�A�I�A�M�A�G�A�E�A�G�A�E�A�I�A�XA�VA�S�A�S�A�XA�\)A�\)A�VA�VA�VA�XA�XA�VA�VA�VA�ZA�\)A�^5A�ZA�ZA�\)A�`BA�`BA�^5A�^5A�bNA�`BA�^5A�\)A�^5A�bNA�bNA�`BA�\)A�\)A�`BA�dZA�bNA�bNA�`BA�^5A�bNA�dZA�dZA�dZA�`BA�`BA�bNA�ffA�bNA�`BA�bNA�ffA�ffA�bNA�`BA�bNA�ffA�ffA�dZA�`BA�bNA�dZA�dZA�bNA�bNA�bNA�dZA�hsA�hsA�dZA�bNA�ffA�jA�hsA�hsA�dZA�dZA�hsA�jA�hsA�ffA�dZA�hsA�jA�hsA�dZA�bNA�dZA�ffA�hsA�ffA�ffA�dZA�ffA�ffA�hsA�jA�hsA�dZA�dZA�hsA�jA�hsA�dZA�bNA�dZA�hsA�hsA�jA�ffA�dZA�hsA�jA�l�A�jA�ffA�ffA�jA�l�A�l�A�jA�hsA�hsA�jA�l�A�l�A�jA�ffA�hsA�jA�n�A�l�A�jA�hsA�hsA�ffA�\)A�ZA�XA�VA�ZA�M�A�VA�M�A�M�A�O�A�;dA�;dA�7LA�5?A�;dA�?}A�?}A�?}A�=qA�I�A�K�A�K�A�I�A�&�A�-A�9XA�/A�-A�$�A��A�$�A�&�A�&�A�&�A� �A�"�A�"�A�"�A�"�A�"�A�"�A�-A�-A�1'A�1'A�&�A�+A�/A�1'A�(�A�+A�/A�$�A�$�A��A��A�bA�JA�VA�{A�{A�1A�  A�  A�%A�1A�%A�1A�bA�oA�1A���A���A���A���A���A��mA��`A��TA��`A��`A��mA��`A��TA��HA��TA��`A��mA��`A��TA��;A��#A���AھwAھwA���AھwAں^Aک�AڋDA�jA�JA���A�z�A�dZA�^5A�VA�=qA��TA�/A�G�A֋DAոRA�t�A�v�A�(�A�JA���A��
Aѩ�AэPAч+Aч+Aч+Aч+AхAуAуAуAуAхAч+Aщ7Aщ7Aч+AхAхAуAуAуAхAч+Aч+AхAуAуAхAхAхAуA�~�A�x�A�p�A�p�A�n�A�dZA�^5A�bNA�^5A�bNA�bNA�`BA�ZA�VA�O�A�I�A�7LA�+A�JA��AЕ�A�VA�"�A���A��HA�ƨAϲ-AρA�Q�A�+A�1A��`A�ȴAβ-A�p�A�A�A�oA��
A�x�A�9XA�VA��A�ĜÁA�33A��yAˬA�^5A�33A�?}A�A�A��A��/A���A���A���A�AʶFAʲ-Aʲ-AʮA�~�A��A�ƨAɉ7Aɏ\A�x�A�O�A�E�A�(�A�
=A��AȬAȁA�`BA�5?A�{A�bA�  AǬA�S�A�A��TAƾwAƏ\A�t�A�ZA�5?A��/Aţ�A�O�A�  A��yA���Aġ�Aĉ7AāA�dZA�9XA���AÉ7A¼jA�r�A�ZA�+A��A�ĜA�p�A�%A���A�VA�bA��jA�1'A�A�`BA���A��7A�ZA��A���A���A���A��A�hsA�{A���A�ZA�VA���A�hsA��FA��TA�7LA�A�l�A�S�A�I�A�;dA��A��A��/A�ƨA���A�9XA���A���A��A�1A��;A��jA���A���A���A��uA�l�A�+A��
A�C�A��DA��;A�M�A��-A�ffA�E�A��A��
A�\)A�33A���A��A�dZA�;dA�VA�\)A��A���A�bNA�1'A���A��RA���A�z�A�^5A�&�A�A��TA�ȴA��jA���A��+A��A�t�A�O�A� �A�ƨA���A��A��\A�"�A���A���A��A�A�A��A��A���A���A�z�A�VA��A�S�A�dZA�oA��#A��A���A��jA���A��uA�v�A�VA�1'A�+A�A���A���A���A���A���A�~�A�XA�XA�VA�9XA�;dA�33A�+A�=qA�+A��A�ȴA�C�A�E�A�E�A�=qA��/A��DA�C�A��TA�E�A�|�A���A��yA��A���A��mA��FA�jA�?}A��A��DA�`BA��A���A��TA���A���A�ƨA���A��!A~�uAz�`Ay��Ax��Ax��Ax��AxffAx{Aw�
AwhsAvJAuO�As��Ar��ArȴAr��Aq��ApAn�HAn�Am��Al�!Al(�Ak�Ak�FAkXAj�`Aj�!Aj5?Ai�wAiS�Ai�Ah��Ah�\Ah5?Ag�FAg�PAg7LAfbNAf�Ad��Ac�;AcG�Aa�;A_��A^��A^9XA]�FA\�`A\bNA[��A[O�AZv�AYhsAYVAX�`AX�HAX�AX��AXȴAW�wAV��AU��AU��AU�7AUdZAU&�AT�HAT�\ATM�AT-AT �AT{AS�AS�AS�#AS�wAS�FAS��AS��AS��AS�hASl�AS�AR�AR��AR��AR�!AR�uAR�+ARjARA�AR�AQ�#AQ��AQ|�AQ�AP �AO�hAOK�AO/AO�AO�AN�HANjANAM�PAM;dAL�/AL�DAKdZAJ~�AJ1AJ  AI��AI��AI��AI��AI�AI��AI"�AH��AH9XAG�;AGAF�DAF �AE�wAE|�AEC�AEAD�`AD�RADM�AC��AC�^AC"�AB�AB��ABI�AB�AA�mAA�
AAƨAA�AAx�AAS�AA�A@�HA@�uA@Q�A>�A>M�A> �A>  A=��A=��A=l�A=p�A=�A<��A<�`A<��A<ȴA<��A<��A<�A<bNA<(�A<A;A;�A;"�A:�yA:�`A:��A:��A:�9A:�9A:�!A:�A:��A:��A:�uA:jA: �A9�TA9�A9\)A9"�A8��A8�`A8��A8�9A8��A8�A8VA8A�A85?A81'A8$�A8�A8  A7�TA7��A7�FA7��A7�7A7t�A7hsA7S�A733A7�A6�A6�HA6ĜA6��A6�+A6z�A6v�A6n�A6ffA6�A5�TA5��A5G�A4�yA4�9A4�uA4ZA4(�A3��A3K�A2ȴA2�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                            A�K�A�G�A�Q�A�XA�XA�VA�ZA�^5A�`BA�^5A�^5A�`BA�bNA�bNA�dZA�dZA�ffA�ffA�ffA�ffA�ffA�hsA�jA�jA�dZA�S�A�=qA�C�A�1'A�$�A�&�A�-A��A�1A�%A��A��TA���Aٴ9A�`BAэPAЍPA̍PA��A�%AÕ�A�K�A�9XA���A��9A�M�A�v�A�+A��+A�v�A���A�v�A��A�A��A�33A�;dA�/A���A� �A"�Axr�At�Ao��Ak��Ai`BAg&�Aa|�A\I�AYVAV�yAT��AS�AS��ARȴARJAO�
ANI�AK\)AI�AG�TAE\)AC�FABJAA/A>�jA=dZA<ĜA<bA:�yA:�A:�A8��A8ffA81A7�PA7
=A6�A5��A4A�A2�RA1�A/��A/S�A/%A.A�A-�-A,bNA,v�A.�A-��A-�
A,ȴA+��A+C�A+%A*E�A*{A)��A'A&�A&��A';dA'l�A';dA&��A&�uA&$�A%A%��A%�A%O�A%%A$��A$5?A#ƨA#\)A"�HA"ȴA"��A"Q�A!��A!oA ��A �RA -A   A�PA33A�jA�A�A�FA�9AQ�A�mA\)A��A�yA�A�jAr�A�
A`BA��A�A�Ax�AVA�`A��A�A/A��A��A�+AjA9XA��A�wA�AhsA%AĜAjA$�A��A��A�7AhsA?}A�A�A�AbNA(�A�A1A�A��AVAz�A(�AXAl�AO�AVAn�AAC�AoA
�A
��A
ffA	�TA	G�A	VAz�A��A��A7LA��AĜAȴAȴA�jAv�AbAp�A;dAoA�/A{A�A��AG�A%A�`A��A{A��AC�A
=A ��A ��A Z@��
@��y@��-@��@��w@�
=@�~�@�@�O�@�?}@��`@���@�b@���@�S�@�~�@�@��j@��;@���@�r�@�l�@��@�n�@�@��#@�D@�C�@�!@�v�@��@�@�x�@��@�C�@��T@�G�@��@�D@�A�@��@�(�@�D@�1'@�l�@���@�E�@���@���@��@�@�hs@��@���@�@߮@�33@��H@އ+@���@ݩ�@��/@��m@�dZ@�@�ff@�J@ٺ^@�X@�1'@ץ�@��H@�v�@ղ-@���@�9X@��
@�dZ@�33@���@�{@���@Ѳ-@��`@� �@��
@υ@��H@�@�O�@�%@�1'@���@�5?@�-@���@���@ə�@�p�@�&�@���@ȴ9@�A�@�  @ǝ�@�33@��H@ũ�@�7L@�V@Ĵ9@�Z@�b@þw@��@�v�@�E�@��#@��@�`B@�7L@���@�I�@�K�@��R@��@��#@�x�@��@��u@��@�|�@��y@�ȴ@���@�^5@�{@�@��T@��h@�V@��`@���@��j@��@���@���@�1@��@�\)@�"�@�E�@���@��-@�7L@�V@���@��@�z�@�A�@�  @��@�C�@�
=@��\@���@���@���@��7@��7@�p�@�/@��/@�z�@��m@�dZ@�+@��@��+@�v�@�ff@�$�@�p�@�O�@��@��u@�bN@�Z@�9X@�  @��
@�C�@��@���@��!@�^5@�J@���@�hs@�&�@���@��`@��9@�9X@��F@�\)@���@��+@�=q@���@��h@�7L@���@��/@���@�Q�@���@�;d@��@�ȴ@��R@��!@���@�5?@��-@�?}@��/@���@��9@�r�@�1'@�1@��@�;d@��y@���@���@�V@��@���@�x�@��@���@��D@�9X@�  @��F@�
=@��y@��R@�E�@�{@��@�x�@��/@��@�I�@�(�@��
@���@�t�@�33@�@��y@���@�^5@�J@��@���@�O�@�/@��@�I�@��@�ƨ@�C�@��@��\@�E�@�$�@�@���@��-@�O�@�?}@���@��9@���@��@�9X@��@�C�@���@�M�@�5?@�$�@��#@��^@���@�hs@�/@�Ĝ@�z�@��@�b@�  @�ƨ@�l�@�\)@�
=@��y@��y@���@�M�@��@���@�@��@�?}@���@��@��@��w@��@�dZ@�33@�v�@�V@�5?@���@��7@�hs@�7L@��`@���@��@�Z@�(�@��;@���@�|�@�dZ@��@���@�n�@�5?@�-@�J@��T@�p�@�V@��`@��j@��@�Q�@� �@�  @�w@~�y@~5?@}�-@}�@|�D@{�
@{o@z�@z�\@y��@yG�@x��@xQ�@w;d@v��@v{@u�h@u/@t�@t��@tI�@s�m@s��@so@rM�@q�@q��@q��@q�7@q7L@q&�@q�@p��@pA�@o�w@o;d@nȴ@n�+@nV@n5?@m�T@m�h@m/@l�@l�j@l��@l(�@k�m@kƨ@kS�@j�!@j�@ix�@i�@h��@hbN@h1'@g�;@g��@gK�@f�@fff@e�T@e��@eV@d1@c�F@ct�@cC�@c33@c@bn�@a��@a�@a�@a��@a7L@`r�@`A�@_�@_+@^{@]`B@\��@\j@[ƨ@[dZ@Z�@Z^5@ZJ@Yhs@Y�@X�u@X1'@W�;@W|�@W+@V�y@V�R@U��@T�@TI�@T9X@S��@R�\@R�\@RM�@Q��@Qx�@Q7L@Q%@P��@P��@P�u@Pr�@P �@O�@Ol�@Nȴ@N{@M��@MO�@M�@L�@K�m@KC�@J~�@I��@I��@I��@I�7@Ix�@IX@IG�@I7L@I�@I%@HĜ@HQ�@G�;@G�w@G;d@F�@Fff@E�T@E�h@EO�@D�j@DZ@C�m@C�F@C�@CC�@C@B�\@B=q@A��@A�#@A��@AG�@A%@@��@@Q�@@1'@?�P@>�y@>v�@>5?@>$�@>$�@>{@>@=�-@=�@<z�@<(�@<(�@<�@;�F@;33@:��@:n�@9��@9hs@9&�@9�@8�`@8��@8r�@8  @7�@7|�@7�@6�y@6��@6v�@6ff@65?@5��@5�h@5?}@4�@4�j@4�D@4�D@4j@4�@3��@3S�@3C�@3"�@2�H@2n�@2�@1�@1��@1�@0��@0bN@01'@0 �@0  @/��@/��@/;d@.�y@.��@.$�@.$�@.@.@.@-�T@-�@-��@-`B@-�@,��@,�@,9X@,�@,�@+��@+t�@+33@+"�@+@+@*�\@*J@*�@*�@)�^@)X@(Ĝ@(�u@(�@(b@'�;@'��@'�@'l�@'K�@'�@&�R@&�+@&v�@&V@&@%�-@%�@%p�@%/@$�/@$Z@$�@#�m@#�m@#ƨ@#�@#@"��@"�\@"M�@"-@!�#@!�^@!x�@!G�@!7L@!&�@ ��@ Ĝ@ �9@ �@  �@\)@ff@�@�-@�-@p�@?}@`B@p�@V@��@Z@9X@I�@Z@�@�@n�@M�@J@�7@��@x�@7L@%@��@�u@�@bN@1'@�@��@�@�@|�@K�@
=@��@{@��@p�@?}@V@�/@��@��@��@��@9X@��@��@�F@S�@S�@dZ@33@@�\@n�@^5@M�@^5@^5@M�@=q@M�@�@��@�@��@��@�7@hs@7L@&�@7L@&�@%@Ĝ@r�@A�@�@��@�w@|�@l�@;d@+@�@�y@ȴ@�R@��@v�@E�@$�@�T@@��@�@`B@O�@?}G�O�A�G�A�M�A�O�A�K�A�G�A�I�A�M�A�G�A�E�A�G�A�E�A�I�A�XA�VA�S�A�S�A�XA�\)A�\)A�VA�VA�VA�XA�XA�VA�VA�VA�ZA�\)A�^5A�ZA�ZA�\)A�`BA�`BA�^5A�^5A�bNA�`BA�^5A�\)A�^5A�bNA�bNA�`BA�\)A�\)A�`BA�dZA�bNA�bNA�`BA�^5A�bNA�dZA�dZA�dZA�`BA�`BA�bNA�ffA�bNA�`BA�bNA�ffA�ffA�bNA�`BA�bNA�ffA�ffA�dZA�`BA�bNA�dZA�dZA�bNA�bNA�bNA�dZA�hsA�hsA�dZA�bNA�ffA�jA�hsA�hsA�dZA�dZA�hsA�jA�hsA�ffA�dZA�hsA�jA�hsA�dZA�bNA�dZA�ffA�hsA�ffA�ffA�dZA�ffA�ffA�hsA�jA�hsA�dZA�dZA�hsA�jA�hsA�dZA�bNA�dZA�hsA�hsA�jA�ffA�dZA�hsA�jA�l�A�jA�ffA�ffA�jA�l�A�l�A�jA�hsA�hsA�jA�l�A�l�A�jA�ffA�hsA�jA�n�A�l�A�jA�hsA�hsA�ffA�\)A�ZA�XA�VA�ZA�M�A�VA�M�A�M�A�O�A�;dA�;dA�7LA�5?A�;dA�?}A�?}A�?}A�=qA�I�A�K�A�K�A�I�A�&�A�-A�9XA�/A�-A�$�A��A�$�A�&�A�&�A�&�A� �A�"�A�"�A�"�A�"�A�"�A�"�A�-A�-A�1'A�1'A�&�A�+A�/A�1'A�(�A�+A�/A�$�A�$�A��A��A�bA�JA�VA�{A�{A�1A�  A�  A�%A�1A�%A�1A�bA�oA�1A���A���A���A���A���A��mA��`A��TA��`A��`A��mA��`A��TA��HA��TA��`A��mA��`A��TA��;A��#A���AھwAھwA���AھwAں^Aک�AڋDA�jA�JA���A�z�A�dZA�^5A�VA�=qA��TA�/A�G�A֋DAոRA�t�A�v�A�(�A�JA���A��
Aѩ�AэPAч+Aч+Aч+Aч+AхAуAуAуAуAхAч+Aщ7Aщ7Aч+AхAхAуAуAуAхAч+Aч+AхAуAуAхAхAхAуA�~�A�x�A�p�A�p�A�n�A�dZA�^5A�bNA�^5A�bNA�bNA�`BA�ZA�VA�O�A�I�A�7LA�+A�JA��AЕ�A�VA�"�A���A��HA�ƨAϲ-AρA�Q�A�+A�1A��`A�ȴAβ-A�p�A�A�A�oA��
A�x�A�9XA�VA��A�ĜÁA�33A��yAˬA�^5A�33A�?}A�A�A��A��/A���A���A���A�AʶFAʲ-Aʲ-AʮA�~�A��A�ƨAɉ7Aɏ\A�x�A�O�A�E�A�(�A�
=A��AȬAȁA�`BA�5?A�{A�bA�  AǬA�S�A�A��TAƾwAƏ\A�t�A�ZA�5?A��/Aţ�A�O�A�  A��yA���Aġ�Aĉ7AāA�dZA�9XA���AÉ7A¼jA�r�A�ZA�+A��A�ĜA�p�A�%A���A�VA�bA��jA�1'A�A�`BA���A��7A�ZA��A���A���A���A��A�hsA�{A���A�ZA�VA���A�hsA��FA��TA�7LA�A�l�A�S�A�I�A�;dA��A��A��/A�ƨA���A�9XA���A���A��A�1A��;A��jA���A���A���A��uA�l�A�+A��
A�C�A��DA��;A�M�A��-A�ffA�E�A��A��
A�\)A�33A���A��A�dZA�;dA�VA�\)A��A���A�bNA�1'A���A��RA���A�z�A�^5A�&�A�A��TA�ȴA��jA���A��+A��A�t�A�O�A� �A�ƨA���A��A��\A�"�A���A���A��A�A�A��A��A���A���A�z�A�VA��A�S�A�dZA�oA��#A��A���A��jA���A��uA�v�A�VA�1'A�+A�A���A���A���A���A���A�~�A�XA�XA�VA�9XA�;dA�33A�+A�=qA�+A��A�ȴA�C�A�E�A�E�A�=qA��/A��DA�C�A��TA�E�A�|�A���A��yA��A���A��mA��FA�jA�?}A��A��DA�`BA��A���A��TA���A���A�ƨA���A��!A~�uAz�`Ay��Ax��Ax��Ax��AxffAx{Aw�
AwhsAvJAuO�As��Ar��ArȴAr��Aq��ApAn�HAn�Am��Al�!Al(�Ak�Ak�FAkXAj�`Aj�!Aj5?Ai�wAiS�Ai�Ah��Ah�\Ah5?Ag�FAg�PAg7LAfbNAf�Ad��Ac�;AcG�Aa�;A_��A^��A^9XA]�FA\�`A\bNA[��A[O�AZv�AYhsAYVAX�`AX�HAX�AX��AXȴAW�wAV��AU��AU��AU�7AUdZAU&�AT�HAT�\ATM�AT-AT �AT{AS�AS�AS�#AS�wAS�FAS��AS��AS��AS�hASl�AS�AR�AR��AR��AR�!AR�uAR�+ARjARA�AR�AQ�#AQ��AQ|�AQ�AP �AO�hAOK�AO/AO�AO�AN�HANjANAM�PAM;dAL�/AL�DAKdZAJ~�AJ1AJ  AI��AI��AI��AI��AI�AI��AI"�AH��AH9XAG�;AGAF�DAF �AE�wAE|�AEC�AEAD�`AD�RADM�AC��AC�^AC"�AB�AB��ABI�AB�AA�mAA�
AAƨAA�AAx�AAS�AA�A@�HA@�uA@Q�A>�A>M�A> �A>  A=��A=��A=l�A=p�A=�A<��A<�`A<��A<ȴA<��A<��A<�A<bNA<(�A<A;A;�A;"�A:�yA:�`A:��A:��A:�9A:�9A:�!A:�A:��A:��A:�uA:jA: �A9�TA9�A9\)A9"�A8��A8�`A8��A8�9A8��A8�A8VA8A�A85?A81'A8$�A8�A8  A7�TA7��A7�FA7��A7�7A7t�A7hsA7S�A733A7�A6�A6�HA6ĜA6��A6�+A6z�A6v�A6n�A6ffA6�A5�TA5��A5G�A4�yA4�9A4�uA4ZA4(�A3��A3K�A2ȴA2�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                            ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BB[BB[BA�BB'BA�BB'BA�BB'BB'BB'BB[BB'BB'BB'BB'BB'BB'BB'BB[BB'BB[BB'BB[BB'BB�BC-BC�BB�BDgBDgBC�BCaBD�BD�BD�BF�BF?BLdBh�B�6B	,�B	AUB	��B	��B	�B
�B
O�B
VB
=�B
A B
33B
0�B
*eB
_B	�B	�JB	� B	�yB	��B	��B	�/B	��B	�B	�=B	�"B	��B	d�B	`�B	O�B	5�B	,qB	!�B	�B�VB�B�cB�`B�|B��B�;BߤB��B�`B�fB��B��B��B�B	�B	�B	�B	/�B	O�B	Z�B	c�B	d�B	ncB	}VB	��B	�_B	�zB	��B	��B	�aB	�RB	��B	��B	՛B	ѷB	�TB	֡B	רB	خB	�fB
�B
�B
-CB
,qB
!bB
�B
"4B
%�B
(�B
+6B
#�B
�B
(�B
4nB
>wB
A B
B�B
B'B
A B
C-B
D�B
G�B
I�B
K�B
N�B
O�B
P�B
RTB
S[B
R�B
S[B
TaB
W?B
U�B
U2B
V9B
XB
XyB
YKB
ZQB
Z�B
ZQB
ZB
Z�B
[�B
[WB
[�B
\�B
[�B
[�B
[�B
[�B
\]B
]/B
\�B
^5B
\)B
\�B
]dB
\�B
[�B
[�B
[�B
[�B
\)B
\�B
\]B
\�B
]�B
]�B
^B
^5B
^�B
]�B
\�B
[#B
\)B
^�B
^�B
^�B
_pB
`�B
`�B
aHB
aHB
_�B
_;B
_�B
`�B
`vB
`vB
a�B
^�B
]/B
X�B
YB
Y�B
XEB
UgB
R�B
NpB
MB
J�B
HB
F?B
GB
D�B
C�B
D3B
?�B
>�B
>BB
>wB
?B
@�B
B[B
B�B
B'B
A�B
?�B
=�B
<6B
;0B
:^B
8RB
:*B
7LB
5tB
5�B
5B
3�B
1'B
/�B
.�B
-CB
,�B
,B
,B
)�B
'�B
%zB
$B
#:B
#�B
#nB
#:B
$�B
$�B
$�B
$tB
#�B
#B
"�B
!�B
�B
�B
VB
�B
�B
�B
�B
�B
�B
{B
�B
(B
�B
�B
�B
�B
B
B
�B
_B
+B
�B
YB
�B
�B
�B
�B
�B
�B
MB
�B
MB
�B
B
�B
MB
{B
�B
�B
{B
�B
�B
�B
�B
B
�B
\B
�B
�B
JB
xB
�B
PB
�B
	�B
	B

rB

�B
�B
B
	�B
fB

�B
�B
�B
�B
�B
�B
+B
+B
�B
	�B
	7B
	B
xB
JB

rB

=B

	B

rB

�B

rB

�B

�B
B
xB

�B

�B

�B

�B
xB
	�B
	�B

rB

=B

	B
	�B

�B

�B

=B
�B
B

�B

�B

�B
�B
�B
�B

�B

rB
DB

=B

�B

rB

rB

rB
	�B
	�B

	B

	B
	�B
	�B

rB

�B
xB
B
B
JB
JB
�B
�B
�B
PB
�B
�B
�B
(B
�B
\B
�B
�B
�B
hB
�B
:B
�B
�B
oB
�B
oB
oB
oB
:B
:B
�B
oB
�B
uB
�B
uB
�B
�B
�B
uB
�B
�B
�B
�B
�B
�B
MB
�B
�B
�B
�B
�B
B
�B
B
�B
$B
$B
_B
�B
�B
�B
_B
_B
�B
�B
�B
$B
B
�B
�B
�B
MB
MB
�B
�B
B
SB
SB
SB
B
�B
$B
YB
�B
�B
+B
+B
_B
�B
�B
�B
1B
�B
�B
B
�B
eB
�B
�B
B
�B
	B
qB
�B
=B
�B
�B
B
CB
~B
!B
 �B
!�B
!bB
!�B
!�B
"4B
!�B
"4B
"�B
"hB
"�B
"�B
#nB
#nB
#nB
#�B
$B
#�B
$�B
$�B
$@B
$�B
%FB
%FB
%�B
%�B
%�B
%�B
&B
%�B
&�B
&�B
'�B
'�B
'�B
'�B
($B
(�B
)�B
*0B
*eB
*�B
*�B
+B
*�B
+6B
+�B
+�B
,�B
,�B
-�B
-CB
-CB
-wB
-�B
-�B
.�B
.}B
.IB
.�B
/OB
/OB
/B
/OB
/B
/B
0�B
0UB
0UB
0UB
0�B
0�B
1'B
1�B
1[B
1'B
1�B
1�B
1[B
2-B
2aB
2-B
2aB
2�B
2�B
2�B
2�B
2�B
33B
2�B
3hB
4nB
4nB
4nB
5B
5tB
6�B
6�B
6�B
6�B
7LB
7B
6�B
6zB
6FB
7LB
7B
7B
7�B
7�B
8�B
7�B
8B
8�B
8�B
8�B
8�B
9�B
:*B
9�B
:^B
:�B
:�B
:�B
:�B
;�B
;dB
;�B
<6B
<�B
=qB
>BB
>wB
?B
?�B
?}B
?}B
?�B
@OB
@�B
@�B
@�B
@�B
@�B
A B
AUB
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B[B
C-B
CaB
C�B
D3B
DgB
D�B
D�B
D�B
D�B
EB
EB
E9B
E�B
E�B
E�B
F?B
GEB
GB
GEB
GzB
GB
GzB
HKB
HB
G�B
G�B
G�B
HKB
H�B
H�B
H�B
IB
J�B
JXB
J�B
K^B
K�B
K�B
L0B
L�B
L�B
M�B
M6B
M�B
M�B
NB
NpB
NpB
N<B
N<B
OBB
O�B
O�B
O�B
P�B
QNB
P�B
QNB
Q�B
Q�B
RTB
RTB
RTB
RTB
RTB
RTB
R�B
R�B
R�B
T,B
T,B
S�B
T�B
T�B
U2B
VmB
V�B
V�B
W?B
W
B
W
B
W?B
W
B
W
B
W
B
W?B
W?B
W?B
WsB
W�B
XB
W�B
XEB
XyB
X�B
YB
YKB
YKB
Y�B
Y�B
Z�B
ZQB
Z�B
Z�B
Z�B
Z�B
[WB
[WB
[WB
[�B
[�B
\)B
\�B
]�B
]dB
^5B
]�B
^B
^B
]�B
]�B
]�B
]�B
]�B
^5B
^�B
^�B
^�B
^�B
_B
_;B
_pB
_�B
`vB
`vB
`�B
aB
aHB
aHB
aHB
a�B
aHB
a�B
a�B
a�B
bB
a�B
bB
a�B
bNB
b�B
b�B
b�B
cTB
c�B
cTB
c�B
c�B
dZB
d�B
d�B
e,B
e`B
e�B
e�B
e`B
e�B
f2B
ffB
ffB
f�B
f�B
f�B
g8B
gmB
hsB
hsB
h>B
g�B
h>B
h�B
iB
iyB
jKB
lWB
k�B
k�B
k�B
l�B
l�B
m)B
l�B
l�B
m�B
m]B
m�B
m]B
m)B
m)B
m�B
m�B
n/B
n/B
n�B
o5B
o�B
oiB
o�B
pB
o�B
pB
pB
p;B
p;B
p�B
qB
p�B
qvB
qB
rGB
rGB
rB
sMB
s�B
sB
sMB
sB
s�B
s�B
sMB
s�B
tTB
tB
tTB
t�B
u%B
u%B
uZB
u�B
u�B
u�B
u�B
uZB
u�B
u�B
u�B
u�B
u%B
s�B
s�B
s�B
tTB
t�B
t�B
u�B
w2B
x8B
xB
w2B
v�B
v�B
wfB
w�B
wfB
v�B
v�B
wfB
x�B
xlB
x�B
y	B
y>B
zB
zDB
zB
{B
{�B
{�B
|B
|�B
{�B
|�B
|�B
|�B
}"B
|�B
}"B
}�B
~�B
~(B
.B
.B
cB
cB
cB
�B
~�B
.B
cB
� B
� B
�4B
�4B
�B
�iB
�iB
�B
�B
�iB
�iB
��B
�;B
�;B
��B
�oB
�B
��B
�uB
��B
�{B
�B
�B
�GB
�{B
��B
�{B
�B
��B
��B
�B
�SB
��B
��B
��B
��B
��B
�YB
�YB
�YB
��B
��B
��B
�+B
�_B
�_B
��B
��B
�1B
��B
�1B
��BB�BA BA�BB�BC�BA�BA BB�BCaBB�BA�BA B=qBB�BC-BC-BA�B@�BA BB'BB[BB�BA�BA�BA�BB�BB�BB'B@�BAUBC-BC-BB[B@�BA�BC-BB[BAUBAUBB�BB�BB�BAUBAUBA�BC-BC-BB�BA BAUBA�BB�BCaBB[BAUB@�BA�BC-BB�BA�BAUBB'BB�BB�BA BAUBB�BC-BB[BA�BA BB[BCaBB�BA�BAUBB[BB�BB�BB[B@�BAUBB�BCaBB'BA BAUBB'BC-BC-BA�BAUBA�BC-BC-BB'BAUBAUBCaBCaBB�BB'BAUBB'BB[BB�BB�BA�BA�BA BA�BB�BB�BB'BAUBB'BB�BCaBC-BB'BA�BAUBB�BC-BB'BAUBA BA�BC-BC-BA�BAUBAUBB'BB�BCaBA�BAUB@�BB'BC�BB�BA�BAUBA�BB[BC�BB[BA�BB�BC�BC�BA�BB'BE9BA�BD�BFB?BC-BB�BD�BE9BB�BB�BB[BC�BDgBA�BA�BB[BCaBLdBC�BA�BGBC�BEmBEmBD�BC�BC-BDgBE�BDgBD�BCaBDgBC�BD�BC�BB�B@�BB[BC�BB�BC�BD�BC�BB�BB�BHKBB[BE�BB[BFBDgBDgBD3BC�BB�BD�BD�BFBC�BD�BA�BB�BC�BFtBGzBEBD�BD�BE9BIRBGzBH�BGEBF?BE�BFBGEBG�BGBFBEBD�BEmBGEBH�BP�BMBLdBK�BK^BK�BPBU2B[#Bn�BgmBxlBkQBjKBjBk�B� B��B��B�hB��B	YB	�B	B	"B	�B	�B	'�B	-�B	.�B	.B	.�B	/B	/OB	0!B	0!B	/�B	/�B	/B	.IB	,�B	-�B	.�B	/OB	/�B	/�B	/�B	/B	.B	-�B	.IB	.}B	/B	.�B	.IB	-wB	,�B	-CB	-B	.B	0�B	/�B	/OB	1[B	1�B	/�B	1�B	/B	.�B	/OB	/OB	0UB	0!B	/�B	/�B	1'B	1'B	5�B	B�B	HKB	M�B	P�B	OBB	P�B	Q�B	ZB	\�B	`�B	c�B	e,B	e`B	g8B	oiB	p�B	t�B	w�B	�B	��B	��B	�B	��B	��B	�	B	�VB	�zB	��B	��B	�B	��B	��B	�3B	��B	�aB	��B	�B	�'B	��B	��B	��B	��B	��B	B	�KB	��B	� B	��B	�3B	��B	��B	�B	�B	ΥB	��B	�gB	��B	רB	�KB	�]B	��B	�B	��B	��B	�`B	�B	��B	��B	��B
B
B
1B
YB
 �B
B
B
oB	�]B
  B
AB
%B
�B
)_B
>wB
&�B
.B
-�B
-wB
6�B
6zB
7�B
A�B
>BB
PB
T�B
��B
z�B
a�B
a�B
[�B
e,B
_;B
U2B
S�B
RTB
N�B
V9B
u�B
>BB
A�B
8B
?HB
O�B
EmB
A B
FtB
<�B
6�B
4�B
5tB
2�B
1�B
:�B
]dB
MjB
@�B
FtB
B'B
F�B
>�B
3�B
.}B
2�B
.�B
,�B
,B
.B
33B
2-B
=qB
C�B
49B
>wB
0!B
(�B
$�B
�B
%�B
#:B
W�B
:*B
�B
.IB
n�B
'B
'B
�B
bB
�B

=B
PB

	B
�B
�B
B
�B
�B
 �B	��B	��B	��B	��B	��B	�+B	�	B	�B	��B
�B	�lB	�B	�QB	��B
B	��B	��B
�B	�B	�ZB	ݘB	��B	֡B	��B	�HB	��B
+B	�B	֡B	�B	�HB	�B	�TB	��B	خB	��B
�B
B	�]B	�DB	�B	�B	�B	�+B	�2B	�B	�B	�iB	�B	��B	�B	�|B	�DB	��B	�TB
�B	��B	�B	��B	��B	��B	��B	�HB	�NB	�EB	�>B
	�B	ϫB	�IB	��B	��B	��B	��B	�FB	�CB	��B	�MB	��B	��B	��B	��B	��B	�B	|�B	y>B	�ZB	��B	qAB	r�B	c B	a|B	`�B	aHB	`vB	`�B	h
B	b�B	��B	S�B	L0B	L�B	i�B	T�B	T�B	D�B	AUB	DgB	6B	3hB	5�B	4�B	1�B	/OB	2�B	.}B	-CB	'�B	(�B	#nB	'RB	"�B	=B	"hB	�B	�B	#:B	YB	�B	-wB	�B		7B�(B�]B	MB��B�`B�2B	oB�B��B��B�2B�B��B��B	�B��B�B�B�B�B�B�B�fB�,B��B�HB��B��B�BB�TB�B�BB��BߤBޞB�BB�B�ZB��B�B��B�pBޞBܒBߤB�pB��B�|B�BܒB�B��B�mB�&B��B��B�|B�,B�8B�8B�B�B�B�B�PB�B��B�#B�WB�#B�B�B�yB�B�B�HB�BB�yB� B�"B�B�B�)B� B�B�cB�MB��B��B�PB	�B��B�.B	AB��B	�B	 �B	B	�B	%B	�B		B	B	~B	�B	($B	YB	SB	�B	�B	OB	#:B	4nB	>B	H�B	K�B	O�B	OvB	P�B	S�B	VB	V�B	Z�B	[#B	]�B	`BB	dZB	c B	bNB	c�B	d�B	e�B	d�B	d�B	e�B	e,B	d�B	e`B	iDB	o B	q�B	x8B	{B	�B	}�B	{B	z�B	�B	�GB	�B	��B	�=B	�\B	��B	��B	�SB	��B	��B	��B	�B	�B	�zB	��B	��B	��B	�qB	��B	��B	�B	�?B	�zB	�B	�3B	�FB	��B	��B	��B	�OB	��B	�zB	�^B	ĜB	��B	��B	ȴB	�pB	�&B	��B	ǮG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                            BB[BBBA�BB'BA�BA�BA�BBBBABB'BBABB'BB'BB'BB'BB'BB'BB'BB[BB'BB[BB'BB[BB[BB�BC�BC�BCaBD�BDgBC�BC�BEBD�BEBG+BG�BS�B�OB�;B	5�B	VB	�B	�(B
uB
0B
qB
poB
UB
VB
L�B
[	B
?�B
}B
�B
�B	��B	��B	�HB	��B	��B
 �B	�]B	�LB	��B	��B	r�B	p�B	\�B	=�B	4�B	3�B	(>B	KB�B�%B�>B�B�B�4B�B��B�B�B�NB� B�B��B	SB	.B	OB	2aB	R B	^jB	d�B	g8B	r-B	cB	�=B	�B	�RB	�|B	�	B	�B	��B	уB	��B	ּB	�B	��B	��B	��B	�EB	�bB
�B
 �B
0�B
0!B
"�B
 �B
$�B
&�B
*�B
1AB
&fB
�B
(>B
3�B
?.B
BB
C�B
C�B
BuB
C�B
EB
HfB
J�B
M6B
PbB
QhB
R:B
S�B
S�B
S&B
T�B
V�B
X�B
VB
V9B
XB
X�B
ZB
Z�B
\B
]B
[	B
[=B
^jB
]/B
\�B
]~B
]�B
[�B
\B
\B
\�B
^�B
^�B
_;B
`B
\�B
^�B
^�B
]dB
]B
]�B
^�B
\�B
]dB
]dB
\�B
]~B
^�B
^�B
^�B
^�B
_�B
^�B
^OB
\)B
]IB
_;B
_B
_VB
`B
aHB
`�B
b4B
b�B
`vB
_pB
_�B
aB
a�B
b�B
c�B
`B
_�B
X�B
Y�B
Z�B
Z�B
W�B
T�B
O(B
M�B
KxB
IB
H1B
IB
E�B
E�B
FYB
@iB
@�B
?�B
>�B
?B
@�B
B�B
C�B
C�B
DB
@�B
>]B
="B
=�B
:�B
9rB
;dB
88B
6B
6�B
6�B
5?B
2|B
0UB
/iB
-�B
-�B
-�B
-�B
,=B
)�B
'8B
%`B
$@B
%,B
$@B
#nB
%zB
%`B
%�B
%FB
$ZB
$tB
#�B
$@B
!�B
 BB
#nB
�B
QB
�B
�B
9B

B
�B
�B
�B
�B
�B
pB
�B
�B
�B
	�B
B
�B
EB
�B
�B
)B
[B
�B
�B
�B
�B
9B
�B
�B
MB
�B
�B
2B
�B
mB
2B
�B
�B
aB
aB
�B
�B
.B
�B
�B
B
dB
�B
VB
B

�B

rB
B
�B
dB
�B

=B
	RB
�B
	B
1B
	B

#B
�B
�B
�B

�B

�B
	�B

�B
B
6B

�B

�B

rB

�B

�B

�B
DB
)B
�B
�B
�B
�B
�B
�B
JB

=B

�B
B

�B

�B
B
�B
DB
B
JB
^B
)B
DB
�B
�B
�B
�B
^B
DB
JB
B
�B
^B
xB

�B
	�B

rB

�B

=B
	�B

=B
^B
)B
�B
0B
0B
dB
~B
B
�B
"B
�B
B
\B
�B
B
B
�B
HB
bB
4B
�B
�B
�B
TB
�B
�B
&B
�B
�B
oB
oB
�B
@B
@B
�B
aB
,B
B
{B
�B
�B
B
B
2B
2B
�B
B
�B
�B
�B
B
�B
�B
�B
gB
�B
�B
�B
�B
�B
�B
+B
_B
�B
KB
B
�B
_B
�B
�B
�B
mB
mB
�B
�B
�B
�B
�B
�B
�B
mB
mB
mB
�B
B
EB
yB
+B
_B
�B
�B
�B
KB
eB
�B
1B
B
�B
B
B
7B
QB
�B
=B
�B
�B
qB
xB
)B
B
�B
�B
�B
 B
!�B
"NB
!�B
!�B
"4B
"�B
"hB
"�B
#B
"�B
# B
#nB
$B
#�B
#�B
$tB
$ZB
$�B
%`B
%B
$�B
%�B
%�B
%�B
&LB
%�B
%�B
&B
&fB
&fB
'B
'B
(
B
'�B
'�B
(�B
(�B
)�B
+6B
*�B
*�B
*�B
+B
+QB
+B
+�B
,"B
,�B
-CB
-�B
-�B
-wB
-�B
.B
.B
.IB
.�B
.}B
.�B
/�B
/�B
/�B
/�B
/�B
/�B
0UB
1�B
0�B
0�B
0�B
1AB
1AB
2|B
2B
1�B
1�B
2|B
1�B
1�B
2�B
2�B
2�B
2�B
2�B
3B
3�B
3MB
2�B
3�B
3�B
4B
4�B
4�B
4�B
5tB
6FB
7�B
6�B
72B
7fB
7�B
7�B
6�B
6�B
7B
8B
7�B
7�B
8B
8lB
9>B
8B
8�B
9>B
9rB
8�B
9�B
:�B
:�B
:�B
:�B
;0B
;B
:�B
;B
<B
;�B
<B
<�B
=<B
=�B
>]B
>�B
?cB
?�B
?�B
?�B
@iB
@�B
A;B
AUB
A;B
A B
A B
AoB
A�B
A�B
BB
A�B
A�B
B'B
B�B
B�B
B�B
C�B
C�B
D3B
D�B
D�B
D�B
D�B
D�B
EB
ESB
E�B
E�B
F%B
F%B
F?B
G+B
G�B
G_B
GzB
G�B
GEB
HB
H�B
H1B
G�B
G�B
HKB
IB
H�B
H�B
I�B
J#B
KDB
J�B
K^B
K�B
L0B
LJB
L�B
MB
M6B
M�B
M�B
N<B
N"B
NpB
N�B
N�B
N�B
O(B
P.B
P}B
O�B
PB
Q�B
QhB
Q4B
Q�B
Q�B
R:B
R�B
R�B
R�B
RoB
R�B
R�B
R�B
S@B
S[B
T�B
T{B
T{B
U2B
UB
U�B
W
B
WYB
WYB
W�B
W
B
W$B
WYB
W$B
W$B
W$B
WYB
WYB
W�B
W�B
XB
XEB
X_B
X�B
X�B
YeB
YeB
Y�B
Y�B
ZQB
ZQB
Z�B
Z�B
Z�B
[	B
[=B
[=B
[�B
[�B
[�B
[�B
\B
\�B
\�B
]�B
^B
^�B
^5B
^OB
^B
]�B
]�B
]�B
^B
^5B
^�B
_!B
^�B
^�B
_B
_�B
_�B
_�B
`�B
`�B
`�B
`�B
aHB
a�B
a|B
a�B
a�B
a|B
bB
a�B
b4B
b4B
a�B
bNB
bNB
b�B
b�B
c:B
c B
c�B
c�B
c�B
c�B
d@B
d�B
eB
e,B
ezB
e�B
e�B
e�B
e�B
fLB
f�B
f�B
f�B
f�B
f�B
f�B
gmB
g�B
h�B
h�B
h�B
g�B
hXB
h�B
iB
i�B
jKB
l�B
l=B
lB
k�B
l�B
m]B
mCB
l�B
l�B
nB
m�B
m�B
mwB
mCB
m�B
nIB
m�B
nIB
n�B
o5B
o�B
o�B
o�B
pB
p;B
o�B
p;B
pUB
pUB
poB
q'B
qAB
p�B
q�B
q[B
r�B
r|B
r-B
s�B
s�B
s�B
s�B
sMB
s�B
s�B
s�B
t9B
t�B
tTB
t�B
t�B
utB
uZB
u�B
v+B
u�B
u�B
vFB
utB
u�B
v+B
v`B
v�B
vB
tnB
tB
s�B
t�B
t�B
t�B
u�B
w�B
x�B
x8B
wLB
v�B
v�B
w�B
x�B
w�B
v�B
wB
w�B
x�B
x�B
x�B
y>B
y�B
z*B
z^B
zDB
{JB
{�B
|B
|6B
|�B
|B
|�B
}B
}<B
}�B
}<B
}�B
}�B
~�B
~]B
cB
.B
cB
cB
�B
� B
~�B
}B
�B
� B
� B
�iB
�iB
�oB
��B
��B
� B
�B
�iB
��B
��B
�;B
�oB
��B
��B
�'B
��B
��B
��B
��B
�-B
��B
�aB
��B
�3B
��B
�MB
�B
�B
�9B
��B
��B
��B
�B
�B
�%B
�tB
�tB
�tB
��B
��B
��B
�_B
�zB
�zB
��B
�B
�KB
�B
�1G�O�BB�BA BA�BB�BC�BA�BA BB�BCaBB�BA�BA B=qBB�BC-BC-BA�B@�BA BB'BB[BB�BA�BA�BA�BB�BB�BB'B@�BAUBC-BC-BB[B@�BA�BC-BB[BAUBAUBB�BB�BB�BAUBAUBA�BC-BC-BB�BA BAUBA�BB�BCaBB[BAUB@�BA�BC-BB�BA�BAUBB'BB�BB�BA BAUBB�BC-BB[BA�BA BB[BCaBB�BA�BAUBB[BB�BB�BB[B@�BAUBB�BCaBB'BA BAUBB'BC-BC-BA�BAUBA�BC-BC-BB'BAUBAUBCaBCaBB�BB'BAUBB'BB[BB�BB�BA�BA�BA BA�BB�BB�BB'BAUBB'BB�BCaBC-BB'BA�BAUBB�BC-BB'BAUBA BA�BC-BC-BA�BAUBAUBB'BB�BCaBA�BAUB@�BB'BC�BB�BA�BAUBA�BB[BC�BB[BA�BB�BC�BC�BA�BB'BE9BA�BD�BFB?BC-BB�BD�BE9BB�BB�BB[BC�BDgBA�BA�BB[BCaBLdBC�BA�BGBC�BEmBEmBD�BC�BC-BDgBE�BDgBD�BCaBDgBC�BD�BC�BB�B@�BB[BC�BB�BC�BD�BC�BB�BB�BHKBB[BE�BB[BFBDgBDgBD3BC�BB�BD�BD�BFBC�BD�BA�BB�BC�BFtBGzBEBD�BD�BE9BIRBGzBH�BGEBF?BE�BFBGEBG�BGBFBEBD�BEmBGEBH�BP�BMBLdBK�BK^BK�BPBU2B[#Bn�BgmBxlBkQBjKBjBk�B� B��B��B�hB��B	YB	�B	B	"B	�B	�B	'�B	-�B	.�B	.B	.�B	/B	/OB	0!B	0!B	/�B	/�B	/B	.IB	,�B	-�B	.�B	/OB	/�B	/�B	/�B	/B	.B	-�B	.IB	.}B	/B	.�B	.IB	-wB	,�B	-CB	-B	.B	0�B	/�B	/OB	1[B	1�B	/�B	1�B	/B	.�B	/OB	/OB	0UB	0!B	/�B	/�B	1'B	1'B	5�B	B�B	HKB	M�B	P�B	OBB	P�B	Q�B	ZB	\�B	`�B	c�B	e,B	e`B	g8B	oiB	p�B	t�B	w�B	�B	��B	��B	�B	��B	��B	�	B	�VB	�zB	��B	��B	�B	��B	��B	�3B	��B	�aB	��B	�B	�'B	��B	��B	��B	��B	��B	B	�KB	��B	� B	��B	�3B	��B	��B	�B	�B	ΥB	��B	�gB	��B	רB	�KB	�]B	��B	�B	��B	��B	�`B	�B	��B	��B	��B
B
B
1B
YB
 �B
B
B
oB	�]B
  B
AB
%B
�B
)_B
>wB
&�B
.B
-�B
-wB
6�B
6zB
7�B
A�B
>BB
PB
T�B
��B
z�B
a�B
a�B
[�B
e,B
_;B
U2B
S�B
RTB
N�B
V9B
u�B
>BB
A�B
8B
?HB
O�B
EmB
A B
FtB
<�B
6�B
4�B
5tB
2�B
1�B
:�B
]dB
MjB
@�B
FtB
B'B
F�B
>�B
3�B
.}B
2�B
.�B
,�B
,B
.B
33B
2-B
=qB
C�B
49B
>wB
0!B
(�B
$�B
�B
%�B
#:B
W�B
:*B
�B
.IB
n�B
'B
'B
�B
bB
�B

=B
PB

	B
�B
�B
B
�B
�B
 �B	��B	��B	��B	��B	��B	�+B	�	B	�B	��B
�B	�lB	�B	�QB	��B
B	��B	��B
�B	�B	�ZB	ݘB	��B	֡B	��B	�HB	��B
+B	�B	֡B	�B	�HB	�B	�TB	��B	خB	��B
�B
B	�]B	�DB	�B	�B	�B	�+B	�2B	�B	�B	�iB	�B	��B	�B	�|B	�DB	��B	�TB
�B	��B	�B	��B	��B	��B	��B	�HB	�NB	�EB	�>B
	�B	ϫB	�IB	��B	��B	��B	��B	�FB	�CB	��B	�MB	��B	��B	��B	��B	��B	�B	|�B	y>B	�ZB	��B	qAB	r�B	c B	a|B	`�B	aHB	`vB	`�B	h
B	b�B	��B	S�B	L0B	L�B	i�B	T�B	T�B	D�B	AUB	DgB	6B	3hB	5�B	4�B	1�B	/OB	2�B	.}B	-CB	'�B	(�B	#nB	'RB	"�B	=B	"hB	�B	�B	#:B	YB	�B	-wB	�B		7B�(B�]B	MB��B�`B�2B	oB�B��B��B�2B�B��B��B	�B��B�B�B�B�B�B�B�fB�,B��B�HB��B��B�BB�TB�B�BB��BߤBޞB�BB�B�ZB��B�B��B�pBޞBܒBߤB�pB��B�|B�BܒB�B��B�mB�&B��B��B�|B�,B�8B�8B�B�B�B�B�PB�B��B�#B�WB�#B�B�B�yB�B�B�HB�BB�yB� B�"B�B�B�)B� B�B�cB�MB��B��B�PB	�B��B�.B	AB��B	�B	 �B	B	�B	%B	�B		B	B	~B	�B	($B	YB	SB	�B	�B	OB	#:B	4nB	>B	H�B	K�B	O�B	OvB	P�B	S�B	VB	V�B	Z�B	[#B	]�B	`BB	dZB	c B	bNB	c�B	d�B	e�B	d�B	d�B	e�B	e,B	d�B	e`B	iDB	o B	q�B	x8B	{B	�B	}�B	{B	z�B	�B	�GB	�B	��B	�=B	�\B	��B	��B	�SB	��B	��B	��B	�B	�B	�zB	��B	��B	��B	�qB	��B	��B	�B	�?B	�zB	�B	�3B	�FB	��B	��B	��B	�OB	��B	�zB	�^B	ĜB	��B	��B	ȴB	�pB	�&B	��B	ǮG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                            <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�<��<(�<�w'<���<��H<�A(<��t=�+<�h'<���<�A(<��E=+�:<�n�<)|><�g=�z<��<Z(�<#�
<#�
<#�
<���<�v�<E*�<L��<�S<o�[<�:�<]0�<#�
<#�
<��<�!�<C�*<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<,X<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2018052310290420180523102904IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018060217034220180602170342QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018060217034220180602170342QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107550820190521075508IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                