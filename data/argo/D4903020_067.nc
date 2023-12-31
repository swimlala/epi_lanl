CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-03-01T02:37:28Z creation; 2021-03-26T17:01:02Z DMQC;      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210301023728  20210326170212  4903020 4903020 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               C   CAA  AOAO7836_008777_067                 7836_008777_067                 2C  2C  DD  SOLO_II                         SOLO_II                         8777                            8777                            V2.6; SBE602 19Apr19            V2.6; SBE602 19Apr19            853 853 @�a����@�a����11  @�a�q�i�@�a�q�i�@;��UqK�@;��UqK��d�n�wpz�d�n�wpz11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u?�@@  @��\@�  @�p�@�  A ��A  A ��A,��A?\)A`  A�  A���A�Q�A�  A�  AϮA�  A�Q�B (�B  B�B�
B (�B((�B0(�B8(�B?�
BG�BO�BW�B_�
Bh(�Bp(�Bw�
B�{B�  B��B�{B�(�B�{B�{B�  B�  B�  B�{B�(�B�(�B�  B�  B�  B�{B�=qB�{B�  B�{B�{B��B��B�  B��
B��B��B�  B�  B��B�  B��C�C��C
=C  C	��C��C��C��C��C��C�C�C
=C
=C�C��C"  C$
=C&  C({C*
=C+��C-��C0
=C2
=C3��C5��C7��C:  C;�C=�C@  CB  CC��CF
=CH{CJ
=CK��CM��CP  CR  CS��CU��CX  CZ  C[��C]��C_��Ca��Cd  Ce��Ch  Cj  Cl  Cn
=Cp
=Cr  Cs��Cu��Cx  Cy��C{�C~
=C�C���C�  C�C�  C���C���C���C�  C�C�
=C�
=C���C�  C�C���C���C�C�  C�  C�C�C�  C�  C�  C�C�
=C�  C���C�C�  C�  C�C�C�C�  C�C���C���C�  C���C�C�  C�C���C�  C�C�  C�C�  C���C���C���C���C�  C�  C�  C���C�  C���C�  C���C�  C�C�  C�C���C���C�  C�  C�C�C�  C�C�C�  C���C�C�
=C�C�C�
=C�C�  C���C�  C�C�  C���C�C���C���C�  C�C�C�C���C���C�  C�  C�  C�C�C�C�  C�  C�C�  C�C�C���C�  C�  C���C���C���C���C���C���C���C���C���C�  C�
=C���C���C�  C�D �D ��D �qD}qD�qD� D�D� D�D� D�qD� D�D��D��D}qD��D}qD	  D	}qD	�qD
��D  D}qD�qD�D�D� D  D��D�D��D�D}qD�RD� D�D� D�qDz�D��D}qD  D��D�D}qD�qD}qD  D�D  Dz�D�qD� D  D��D  D}qD  D� D  D��D  D� D�qD }qD!  D!� D"�D"��D#  D#� D#��D$� D%  D%}qD&�D&��D'  D'� D(�D(��D)�D)� D*  D*�D+�D+��D,�D,��D-  D-z�D-��D.}qD.�qD/}qD0  D0� D0��D1z�D1�qD2}qD3  D3� D4  D4� D5  D5��D5�qD6}qD7  D7}qD7�qD8}qD9�D9��D9�qD:� D;  D;}qD;�qD<��D=D=��D>�D>� D>�qD?� D@�D@}qDA  DA� DB  DB}qDC  DC�DD�DD��DE�DE}qDF  DF}qDF�qDG� DG�qDH}qDI  DI� DI�qDJz�DJ�qDK��DL�DL��DM�DM��DN�DN� DO  DO��DPDP��DQ  DQ� DR  DR� DS  DS��DT  DT}qDU�DU��DVDV��DW  DWz�DX  DX� DX��DY� DZ  DZ� D[  D[� D\�D\� D\�qD]��D^�D^}qD^�qD_� D_�qD`}qD`�qDa� Da�qDb� Dc  Dc�Dd�Dd}qDe  De� De�qDf� Dg  Dg}qDg�qDh� Dh��DixRDj  Dj�Dk�Dk� Dk�qDl� Dm�Dm}qDm��Dnz�Dn�qDo� Do�qDpz�Dq�Dq��Dq�qDr}qDs  Ds� Dt�Dt��Du�Du��Du�qDv}qDw�Dw}qDw�qDx��Dx�qDyz�Dy�qDz� D{�D{}qD{��D|}qD|�qD}� D}�qD~� D  D��D�HD�@ D�� D�� D�HD�@ D�~�D�� D�  D�=qD�~�D���D�  D�@ D��HD�D��D�AHD�� D��HD�  D�>�D�� D��HD�  D�=qD�~�D�� D�  D�@ D�� D��qD��qD�@ D�~�D��)D���D�B�D�� D���D�  D�=qD�|)D��qD��qD�=qD�~�D���D�HD�AHD�~�D�� D�  D�AHD�� D�� D�HD�AHD��HD�D�HD�@ D�~�D�� D�  D�AHD���D�D�HD�@ D��HD��HD�HD�AHD�~�D���D���D�>�D�� D���D��qD�=qD�~�D��qD��qD�>�D��HD��HD��D�B�D��HD�� D���D�@ D�~�D���D�  D�@ D�~�D�� D�  D�=qD�~�D���D�  D�AHD�� D�� D���D�>�D�~�D���D�  D�AHD�~�D�� D��qD�>�D��HD�� D�  D�AHD�~�D���D�  D�AHD�~�D���D���D�<)D�}qD���D���D�@ D�� D�� D�HD�@ D�}qD���D��D�B�D�� D�� D�  D�AHD�� D���D�  D�>�D�~�D�� D�HD�B�D��HD���D�  D�>�D�~�D�� D�  D�>�D�� D��HD�HD�=qD�� D�� D�  D�B�D�� D��qD��qD�>�D�~�D�� D���D�=qD�� D��HD�HD�@ D�~�D�� D���D�>�D��HD�� D���D�AHD��HD�D�HD�>�D�~�D���D���D�@ D�� D��HD�  D�>�D�� D�D���D�=qD�� D��HD�HD�>�D�}qD�� D�  D�@ D��HD���D�  D�AHD���D�� D�  D�AHD���D��HD���D�AHD�� D���D��qD�=qD�~�D�� D�HD�AHD�}qD���D�HD�B�D�� D���D�  D�@ D�� D��HD�HD�B�D�� D��HD��D�AHD�� D��HD��D�@ D�}qD���D���D�@ D��HD�� D�HD�B�D��HD�� D���D�AHD D¾�D�HD�@ DÁHD�D�  D�>�DĀ D��HD�HD�>�D�~�D�� D�HD�AHDƀ D��HD�HD�AHDǁHDǾ�D���D�>�DȀ DȾ�D�HD�AHD�~�D�� D���D�=qD�~�Dʾ�D�  D�AHDˀ D�� D���D�>�D�~�D̽qD���D�>�D̀ D��HD���D�>�D΀ D�� D�  D�AHDπ DϾ�D�HD�@ DЀ D��HD�HD�@ D�~�D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�=qDԀ D��HD��qD�=qDՀ Dվ�D���D�@ DցHD�� D�  D�AHDׂ�D��HD�HD�>�D�}qDؽqD���D�>�D�~�DٽqD���D�@ D�~�D�� D�  D�@ Dۀ D۾�D���D�@ D�}qDܽqD�HD�@ D�~�D�� D�  D�>�D�~�D�� D�HD�@ D�~�D�� D��D�AHD�� DྸD���D�>�D�HD��HD�HD�AHD�HD��HD���D�>�D� D�� D�  D�@ D�}qD侸D�  D�AHD� D��HD��D�AHD� D澸D�  D�@ D� D羸D���D�>�D� D�� D���D�@ D�~�D�qD�  D�AHD�~�D�� D��D�@ D�~�D��HD��D�@ D�}qD�� D�HD�AHD� D��HD�  D�@ D�HD�� D�  D�@ D� DﾸD���D�AHD��HD�� D��qD�>�D� D�� D�  D�@ D� D�� D�HD�@ D�HD��HD�HD�B�D� D�� D�HD�B�D�� D�� D�HD�@ D��HD��HD�  D�=qD�� D�� D�  D�AHD�~�D���D��D�C�D��HD�� D���D�>�D�� D��q?�?\)?�  ?�=q?��
?\?��@�@�@!G�@5@G�@Q�@^�R@s33@�G�@��@�\)@���@��H@�  @���@���@�z�@���@\@˅@У�@�@�p�@��@���@��@�
=@��RA33A�A	��A�A  A�
A
=A��A(�A   A#�
A%�A'�A,(�A0  A2�\A4z�A8Q�A<��AAG�AE�AG
=AJ�HAMp�AS33AXQ�AZ�HA^�RAb�\Ah��An{Aq�AvffAy��A~�RA��A���A��RA���A��\A���A��A�=qA�z�A�A��A�=qA��A�ffA�Q�A��HA�A��A�G�A�33A�{A���A��\A�(�A�{A���A��
A�p�A�
=A���A�(�A�
=Aȣ�Aʏ\A��A�  A�G�A��HA�p�A�Q�Aڏ\A�(�A�{A߮A��HA�p�A�A陚A�33A��A�  A�\A��A�
=A���A��\A��B   BG�B=qB
=B  BG�B�RB  B��B	�B
�HB(�Bp�B�HB(�B�B=qB
=Bz�B{B�HB�
B��B=qB�Bz�Bp�BffB�
B!G�B"=qB#33B$  B%p�B&�\B'�
B(��B)��B*�\B+�B,��B.=qB/33B0(�B0��B2=qB3\)B4��B5�B6�RB7�
B8��B9B;
=B<Q�B=��B>�\B?�B@Q�BAp�BB�HBD(�BEG�BF{BG
=BH  BI�BJ{BK�BL��BN{BO
=BP  BP��BQ�BS
=BTQ�BU��BV�HBX(�BYp�BZ=qB[\)B\z�B]��B^�RB_�
Ba�BbffBd  Be�Bf=qBg\)Bhz�Bi��Bj�RBk�
Bm�BnffBo�Bq�Br=qBs�Bt��Bv{Bw�Bx��Bz{B{33B|Q�B}B~�RB�{B��RB�\)B��B��\B�G�B��B�ffB��B��B�=qB��HB�p�B�(�B���B�p�B�  B���B�\)B�  B���B�33B��B��\B�33B�B�=qB���B�\)B�  B��RB�G�B�  B���B�G�B�  B��\B�33B��
B��\B�33B�B�ffB�
=B��B�Q�B���B��B�(�B��RB�\)B��B��\B��B��
B���B�G�B�  B�z�B��B��B�Q�B���B��B�ffB�
=B��B�ffB�
=B���B�=qB��RB�p�B�  B���B�G�B��B��\B��B�B�ffB�
=B��B�Q�B���B���B�Q�B���B��B�=qB���B��B�ffB�
=B��B�=qB��HB�p�B�(�B��RB�G�B�  B���B�G�B��B£�B�G�B��Bď\B�\)B�  BƸRB�p�B�{BȸRB�G�B��
B�ffB�
=BˮB�ffB��B��
B�z�B�33B��BЏ\B�33B��B�z�B��B�B�z�B�
=Bՙ�B�Q�B�
=B��
B؏\B�33BٮB�Q�B��B��B܏\B�33BݮB�ffB��B��B��B�G�B��
B�z�B�
=B��
B�\B�\)B�{B��B�33B�B�ffB��B��
B�\B�\)B�{B���B�p�B�{B�RB�\)B�  B�RB�G�B�  B��B�33B��B��RB�p�B�(�B��RB�33B�{B���B���B�=qB���B�\)B�  B��RB�p�B�=qB���B��C 33C z�C C
=Cp�C�
C=qC�\C�
C�C�C�CQ�C��C�HC33C��C  C\)C��C�C\)C�C��CQ�CC	{C	\)C	��C

=C
p�C
C
=CffC��C(�Cp�C�RC(�C��C�HC�C�\C��C=qC�\C
=CG�C��C{CG�C�RC{C\)CC(�Cz�CC(�C�\C��C(�C��C�C33C��C
=C=qC��C
=CG�CC{C\)C��C
=C\)CC��CffC��C�HCG�Cp�C�RC
=C33C\)C��C�C
=CQ�C�\C�RC{C(�Cz�C��C�HC {C \)C �\C C!
=C!33C!z�C!��C!�C"{C"\)C"z�C"�
C"�C#=qC#ffC#�C#�HC$
=C$Q�C$�C$C$�HC%=qC%Q�C%��C%�RC&
=C&33C&z�C&��C&�C'{C'G�C'�\C'�RC(  C((�C(p�C(��C(�HC)  C)\)C)p�C)C)��C*33C*ffC*��C*�HC+{C+Q�C+��C+�RC,
=C,=qC,z�C,��C,�HC-{C-\)C-z�C-�
C-�C.=qC.\)C.�C.�
C/{C/Q�C/p�C/�
C/�C0=qC0p�C0��C0�C1
=C1ffC1z�C1�
C2
=C2G�C2z�C2�C2��C3(�C3p�C3�\C3�C4
=C4Q�C4�C4�RC5
=C5(�C5z�C5��C5�HC6�C6G�C6��C6C7
=C7G�C7ffC7�RC7�C8{C8p�C8�\C8C9{C9=qC9p�C9C9�C:�C:p�C:�\C:�
C;
=C;33C;�C;�RC;�HC<33C<ffC<�\C<�HC=
=C==qC=�\C=�RC=��C>=qC>\)C>��C>�C?
=C?Q�C?�\C?�RC@  C@=qC@ffC@�RC@�
CA�CA\)CAz�CA��CB
=CB(�CBz�CB�CB�
CC33CCQ�CC�\CC�HCD
=CD33CD�CD�CD�CE33CEQ�CE��CECF{CF=qCFz�CFCF�HCG{CGp�CG��CGG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                         ?u?�@@  @��\@�  @�p�@�  A ��A  A ��A,��A?\)A`  A�  A���A�Q�A�  A�  AϮA�  A�Q�B (�B  B�B�
B (�B((�B0(�B8(�B?�
BG�BO�BW�B_�
Bh(�Bp(�Bw�
B�{B�  B��B�{B�(�B�{B�{B�  B�  B�  B�{B�(�B�(�B�  B�  B�  B�{B�=qB�{B�  B�{B�{B��B��B�  B��
B��B��B�  B�  B��B�  B��C�C��C
=C  C	��C��C��C��C��C��C�C�C
=C
=C�C��C"  C$
=C&  C({C*
=C+��C-��C0
=C2
=C3��C5��C7��C:  C;�C=�C@  CB  CC��CF
=CH{CJ
=CK��CM��CP  CR  CS��CU��CX  CZ  C[��C]��C_��Ca��Cd  Ce��Ch  Cj  Cl  Cn
=Cp
=Cr  Cs��Cu��Cx  Cy��C{�C~
=C�C���C�  C�C�  C���C���C���C�  C�C�
=C�
=C���C�  C�C���C���C�C�  C�  C�C�C�  C�  C�  C�C�
=C�  C���C�C�  C�  C�C�C�C�  C�C���C���C�  C���C�C�  C�C���C�  C�C�  C�C�  C���C���C���C���C�  C�  C�  C���C�  C���C�  C���C�  C�C�  C�C���C���C�  C�  C�C�C�  C�C�C�  C���C�C�
=C�C�C�
=C�C�  C���C�  C�C�  C���C�C���C���C�  C�C�C�C���C���C�  C�  C�  C�C�C�C�  C�  C�C�  C�C�C���C�  C�  C���C���C���C���C���C���C���C���C���C�  C�
=C���C���C�  C�D �D ��D �qD}qD�qD� D�D� D�D� D�qD� D�D��D��D}qD��D}qD	  D	}qD	�qD
��D  D}qD�qD�D�D� D  D��D�D��D�D}qD�RD� D�D� D�qDz�D��D}qD  D��D�D}qD�qD}qD  D�D  Dz�D�qD� D  D��D  D}qD  D� D  D��D  D� D�qD }qD!  D!� D"�D"��D#  D#� D#��D$� D%  D%}qD&�D&��D'  D'� D(�D(��D)�D)� D*  D*�D+�D+��D,�D,��D-  D-z�D-��D.}qD.�qD/}qD0  D0� D0��D1z�D1�qD2}qD3  D3� D4  D4� D5  D5��D5�qD6}qD7  D7}qD7�qD8}qD9�D9��D9�qD:� D;  D;}qD;�qD<��D=D=��D>�D>� D>�qD?� D@�D@}qDA  DA� DB  DB}qDC  DC�DD�DD��DE�DE}qDF  DF}qDF�qDG� DG�qDH}qDI  DI� DI�qDJz�DJ�qDK��DL�DL��DM�DM��DN�DN� DO  DO��DPDP��DQ  DQ� DR  DR� DS  DS��DT  DT}qDU�DU��DVDV��DW  DWz�DX  DX� DX��DY� DZ  DZ� D[  D[� D\�D\� D\�qD]��D^�D^}qD^�qD_� D_�qD`}qD`�qDa� Da�qDb� Dc  Dc�Dd�Dd}qDe  De� De�qDf� Dg  Dg}qDg�qDh� Dh��DixRDj  Dj�Dk�Dk� Dk�qDl� Dm�Dm}qDm��Dnz�Dn�qDo� Do�qDpz�Dq�Dq��Dq�qDr}qDs  Ds� Dt�Dt��Du�Du��Du�qDv}qDw�Dw}qDw�qDx��Dx�qDyz�Dy�qDz� D{�D{}qD{��D|}qD|�qD}� D}�qD~� D  D��D�HD�@ D�� D�� D�HD�@ D�~�D�� D�  D�=qD�~�D���D�  D�@ D��HD�D��D�AHD�� D��HD�  D�>�D�� D��HD�  D�=qD�~�D�� D�  D�@ D�� D��qD��qD�@ D�~�D��)D���D�B�D�� D���D�  D�=qD�|)D��qD��qD�=qD�~�D���D�HD�AHD�~�D�� D�  D�AHD�� D�� D�HD�AHD��HD�D�HD�@ D�~�D�� D�  D�AHD���D�D�HD�@ D��HD��HD�HD�AHD�~�D���D���D�>�D�� D���D��qD�=qD�~�D��qD��qD�>�D��HD��HD��D�B�D��HD�� D���D�@ D�~�D���D�  D�@ D�~�D�� D�  D�=qD�~�D���D�  D�AHD�� D�� D���D�>�D�~�D���D�  D�AHD�~�D�� D��qD�>�D��HD�� D�  D�AHD�~�D���D�  D�AHD�~�D���D���D�<)D�}qD���D���D�@ D�� D�� D�HD�@ D�}qD���D��D�B�D�� D�� D�  D�AHD�� D���D�  D�>�D�~�D�� D�HD�B�D��HD���D�  D�>�D�~�D�� D�  D�>�D�� D��HD�HD�=qD�� D�� D�  D�B�D�� D��qD��qD�>�D�~�D�� D���D�=qD�� D��HD�HD�@ D�~�D�� D���D�>�D��HD�� D���D�AHD��HD�D�HD�>�D�~�D���D���D�@ D�� D��HD�  D�>�D�� D�D���D�=qD�� D��HD�HD�>�D�}qD�� D�  D�@ D��HD���D�  D�AHD���D�� D�  D�AHD���D��HD���D�AHD�� D���D��qD�=qD�~�D�� D�HD�AHD�}qD���D�HD�B�D�� D���D�  D�@ D�� D��HD�HD�B�D�� D��HD��D�AHD�� D��HD��D�@ D�}qD���D���D�@ D��HD�� D�HD�B�D��HD�� D���D�AHD D¾�D�HD�@ DÁHD�D�  D�>�DĀ D��HD�HD�>�D�~�D�� D�HD�AHDƀ D��HD�HD�AHDǁHDǾ�D���D�>�DȀ DȾ�D�HD�AHD�~�D�� D���D�=qD�~�Dʾ�D�  D�AHDˀ D�� D���D�>�D�~�D̽qD���D�>�D̀ D��HD���D�>�D΀ D�� D�  D�AHDπ DϾ�D�HD�@ DЀ D��HD�HD�@ D�~�D�� D�  D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�=qDԀ D��HD��qD�=qDՀ Dվ�D���D�@ DցHD�� D�  D�AHDׂ�D��HD�HD�>�D�}qDؽqD���D�>�D�~�DٽqD���D�@ D�~�D�� D�  D�@ Dۀ D۾�D���D�@ D�}qDܽqD�HD�@ D�~�D�� D�  D�>�D�~�D�� D�HD�@ D�~�D�� D��D�AHD�� DྸD���D�>�D�HD��HD�HD�AHD�HD��HD���D�>�D� D�� D�  D�@ D�}qD侸D�  D�AHD� D��HD��D�AHD� D澸D�  D�@ D� D羸D���D�>�D� D�� D���D�@ D�~�D�qD�  D�AHD�~�D�� D��D�@ D�~�D��HD��D�@ D�}qD�� D�HD�AHD� D��HD�  D�@ D�HD�� D�  D�@ D� DﾸD���D�AHD��HD�� D��qD�>�D� D�� D�  D�@ D� D�� D�HD�@ D�HD��HD�HD�B�D� D�� D�HD�B�D�� D�� D�HD�@ D��HD��HD�  D�=qD�� D�� D�  D�AHD�~�D���D��D�C�D��HD�� D���D�>�D�� G�O�?�?\)?�  ?�=q?��
?\?��@�@�@!G�@5@G�@Q�@^�R@s33@�G�@��@�\)@���@��H@�  @���@���@�z�@���@\@˅@У�@�@�p�@��@���@��@�
=@��RA33A�A	��A�A  A�
A
=A��A(�A   A#�
A%�A'�A,(�A0  A2�\A4z�A8Q�A<��AAG�AE�AG
=AJ�HAMp�AS33AXQ�AZ�HA^�RAb�\Ah��An{Aq�AvffAy��A~�RA��A���A��RA���A��\A���A��A�=qA�z�A�A��A�=qA��A�ffA�Q�A��HA�A��A�G�A�33A�{A���A��\A�(�A�{A���A��
A�p�A�
=A���A�(�A�
=Aȣ�Aʏ\A��A�  A�G�A��HA�p�A�Q�Aڏ\A�(�A�{A߮A��HA�p�A�A陚A�33A��A�  A�\A��A�
=A���A��\A��B   BG�B=qB
=B  BG�B�RB  B��B	�B
�HB(�Bp�B�HB(�B�B=qB
=Bz�B{B�HB�
B��B=qB�Bz�Bp�BffB�
B!G�B"=qB#33B$  B%p�B&�\B'�
B(��B)��B*�\B+�B,��B.=qB/33B0(�B0��B2=qB3\)B4��B5�B6�RB7�
B8��B9B;
=B<Q�B=��B>�\B?�B@Q�BAp�BB�HBD(�BEG�BF{BG
=BH  BI�BJ{BK�BL��BN{BO
=BP  BP��BQ�BS
=BTQ�BU��BV�HBX(�BYp�BZ=qB[\)B\z�B]��B^�RB_�
Ba�BbffBd  Be�Bf=qBg\)Bhz�Bi��Bj�RBk�
Bm�BnffBo�Bq�Br=qBs�Bt��Bv{Bw�Bx��Bz{B{33B|Q�B}B~�RB�{B��RB�\)B��B��\B�G�B��B�ffB��B��B�=qB��HB�p�B�(�B���B�p�B�  B���B�\)B�  B���B�33B��B��\B�33B�B�=qB���B�\)B�  B��RB�G�B�  B���B�G�B�  B��\B�33B��
B��\B�33B�B�ffB�
=B��B�Q�B���B��B�(�B��RB�\)B��B��\B��B��
B���B�G�B�  B�z�B��B��B�Q�B���B��B�ffB�
=B��B�ffB�
=B���B�=qB��RB�p�B�  B���B�G�B��B��\B��B�B�ffB�
=B��B�Q�B���B���B�Q�B���B��B�=qB���B��B�ffB�
=B��B�=qB��HB�p�B�(�B��RB�G�B�  B���B�G�B��B£�B�G�B��Bď\B�\)B�  BƸRB�p�B�{BȸRB�G�B��
B�ffB�
=BˮB�ffB��B��
B�z�B�33B��BЏ\B�33B��B�z�B��B�B�z�B�
=Bՙ�B�Q�B�
=B��
B؏\B�33BٮB�Q�B��B��B܏\B�33BݮB�ffB��B��B��B�G�B��
B�z�B�
=B��
B�\B�\)B�{B��B�33B�B�ffB��B��
B�\B�\)B�{B���B�p�B�{B�RB�\)B�  B�RB�G�B�  B��B�33B��B��RB�p�B�(�B��RB�33B�{B���B���B�=qB���B�\)B�  B��RB�p�B�=qB���B��C 33C z�C C
=Cp�C�
C=qC�\C�
C�C�C�CQ�C��C�HC33C��C  C\)C��C�C\)C�C��CQ�CC	{C	\)C	��C

=C
p�C
C
=CffC��C(�Cp�C�RC(�C��C�HC�C�\C��C=qC�\C
=CG�C��C{CG�C�RC{C\)CC(�Cz�CC(�C�\C��C(�C��C�C33C��C
=C=qC��C
=CG�CC{C\)C��C
=C\)CC��CffC��C�HCG�Cp�C�RC
=C33C\)C��C�C
=CQ�C�\C�RC{C(�Cz�C��C�HC {C \)C �\C C!
=C!33C!z�C!��C!�C"{C"\)C"z�C"�
C"�C#=qC#ffC#�C#�HC$
=C$Q�C$�C$C$�HC%=qC%Q�C%��C%�RC&
=C&33C&z�C&��C&�C'{C'G�C'�\C'�RC(  C((�C(p�C(��C(�HC)  C)\)C)p�C)C)��C*33C*ffC*��C*�HC+{C+Q�C+��C+�RC,
=C,=qC,z�C,��C,�HC-{C-\)C-z�C-�
C-�C.=qC.\)C.�C.�
C/{C/Q�C/p�C/�
C/�C0=qC0p�C0��C0�C1
=C1ffC1z�C1�
C2
=C2G�C2z�C2�C2��C3(�C3p�C3�\C3�C4
=C4Q�C4�C4�RC5
=C5(�C5z�C5��C5�HC6�C6G�C6��C6C7
=C7G�C7ffC7�RC7�C8{C8p�C8�\C8C9{C9=qC9p�C9C9�C:�C:p�C:�\C:�
C;
=C;33C;�C;�RC;�HC<33C<ffC<�\C<�HC=
=C==qC=�\C=�RC=��C>=qC>\)C>��C>�C?
=C?Q�C?�\C?�RC@  C@=qC@ffC@�RC@�
CA�CA\)CAz�CA��CB
=CB(�CBz�CB�CB�
CC33CCQ�CC�\CC�HCD
=CD33CD�CD�CD�CE33CEQ�CE��CECF{CF=qCFz�CFCF�HCG{CGp�CG��CGG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                         @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A��hA��hA��uA��\A��uA��hA��DA��+A��A��DA��hA��hA��\A��hA��PA��DA��DA�t�A�p�A�ffA�\)A�I�A�K�A�5?A� �A���A��A�$�A���A�1'A��A� �A���A���A��^A���A�S�A�/A�
=A��yA���A�A��-A���A���A��\A�p�A�G�A�;dA�/A��A�VA���A��mA�ȴA���A�"�A��yA���A��+A���A�t�A�1'A��TA�p�A�;dA�~�A��A�A�A�JA��A�;dA���A�Q�A�r�A�p�A��A��yA�JA�5?A��;A�JA�S�A��HA�E�A�A��;A��`A���A�n�A��
A�O�A�ƨA��A�E�A��A�A~A�A{�wAy�AxZAvA�Au&�At(�As��Aq�Ap�RAp$�Ao��Ao�AnZAml�Al�\Ak`BAiƨAh��Ag&�Ad�+Ac�Ab��Ab �Ab{Aa��A`�A`(�A_�A^�/A^A\��A\1'AZ�+AYC�AV��AU;dAT��AS?}AR�AQ�AQ%AP9XAO�wAOhsAO%AN�\AN^5AM&�ALA�AK��AI�^AHȴAH~�AHffAG�#AFffAE�-AD�AChsAB�RAB1'AA��AA;dA@�A?��A>�!A=�A<�RA;l�A:��A9oA7O�A6�jA6-A5��A5A3ƨA3`BA2��A2ĜA2�A2A�A2 �A1�A1��A1oA0ZA/&�A.VA-S�A,I�A+�wA+hsA+�A*��A*�uA*  A)/A'��A'S�A&ȴA%�FA$�A#x�A#
=A"�jA"A!C�A �A�
A��AE�A��A/A�#A&�A�!AffA�-A$�AS�A��A�A�+A��A�A��A��A�AhsAffA-A��Ap�AoA5?A��AS�AA
I�A	�A	An�A�^A
=AE�AO�A��A�wA&�A�Av�A  Ap�A�A ��A b@��F@�C�@���@��+@�@�ƨ@��!@�&�@��+@��j@�K�@�@�  @�+@��@�u@�@��H@ꟾ@�@�P@�-@�^@���@䛦@��
@�"�@��@��@���@�K�@އ+@���@�{@�%@�j@��
@���@Ӆ@�ff@�I�@̼j@�@�33@�J@�z�@�+@�-@�`B@�&�@��j@�bN@�\)@��+@�J@���@��D@��;@��!@��@��T@�@��-@���@��h@�x�@�%@�l�@��j@�Q�@��P@�E�@�/@��9@�1'@���@��T@��u@�33@��\@��^@��@��/@��@��F@�S�@���@�{@��^@�x�@�?}@���@�bN@��;@��F@���@��H@�ff@��@��@�x�@��@�  @��@�dZ@��H@�~�@��@���@��^@�x�@��@�Ĝ@�I�@��
@��@�ff@�V@�M�@�5?@���@��7@���@���@�I�@�1@�ƨ@��P@�|�@�|�@�t�@�\)@�
=@���@�=q@�J@�`B@�Ĝ@�I�@��m@��@�|�@�33@��@���@��#@��@��j@�1'@�  @�  @��@��@��;@��F@�t�@�C�@�n�@�%@�z�@��@�S�@��@��H@��R@���@�V@��-@�?}@��j@���@�r�@�A�@�1'@��;@�l�@���@���@�-@��@���@���@��@��@��@�j@�Q�@��@���@��F@���@�|�@�l�@�+@���@�J@���@���@�O�@�z�@�(�@�1@+@~�@~v�@~{@}�h@}/@}V@|�@|�D@|j@|�@{�m@{�F@{dZ@z�!@z-@z�@zJ@y�@y�#@y�^@yhs@xĜ@xQ�@wl�@v�@v��@vff@vV@vE�@v@u�T@u�-@u`B@t�j@tZ@s��@sC�@r-@q��@qX@q�@pr�@o�@o��@o�@n�R@n5?@n@m�@m?}@l1@k�F@k�@kdZ@k"�@j��@j=q@i��@h�9@hb@g�w@g�@fȴ@f$�@e��@ep�@e�@d�@d�D@d�D@dz�@dj@c��@c��@c�@c�@ct�@cS�@c33@b�H@b^5@a�7@`Ĝ@`Ĝ@`��@a%@a7L@`��@`bN@`b@_�P@_;d@^��@^�R@^��@^5?@^@]@]�@\��@\I�@\1@\�@\�@[�@[C�@Z�H@Z�!@Y��@Y�7@X��@X�u@X�@XbN@X1'@X1'@W�@W+@V�@VV@V5?@V{@U��@T��@T�@TZ@S�m@S�F@S��@S��@S�@SS�@S"�@So@R��@R-@Qx�@Q7L@P��@P��@P�9@P�@PQ�@Pb@O�@O�P@O\)@O+@N�y@N��@N5?@M��@M�h@M?}@L��@L�@L��@L9X@K�m@KC�@J�\@J~�@JM�@I��@Ihs@IX@I7L@H��@H�u@H �@G�@G�w@G+@F��@F��@F�+@F$�@E@E`B@D�@Dj@D1@C��@CC�@C@B�@B��@B=q@A��@AX@AG�@AG�@A7L@A7L@A7L@@��@@�u@@b@@  @@  @?�;@?�@?�P@?|�@?l�@?\)@?;d@>�R@>5?@=�T@=��@=�h@=�@=O�@<�j@<�D@<j@<�@;�m@;t�@;"�@:��@:~�@:n�@9�#@9x�@9%@8r�@8A�@8  @7�;@7�w@7��@7|�@7l�@7\)@7;d@7+@7�@6��@6�y@6�@6�+@6{@5�T@5@5�h@5�h@5�@5p�@5p�@5O�@5V@4I�@3�@3S�@3"�@3"�@3"�@3@2��@2�\@2=q@2J@1�#@1��@1X@1�@0�@0A�@0A�@0A�@01'@0 �@/�;@/K�@.��@.ff@.E�@.5?@.$�@-�@-��@-�-@-��@-�h@-�h@-�@-p�@-p�@-`B@-O�@-V@,��@,I�@,9X@,(�@,�@+��@+�m@+�m@+ƨ@+��@+33@*��@*�\@*n�@*^5@*=q@*=q@*-@*J@)��@)�^@)7L@)�@(��@(��@(Q�@(b@'�;@'\)@'+@'
=@&��@&ȴ@&��@&$�@%@%?}@$�/@$I�@$1@#��@#�F@#�@#t�@#S�@#C�@#o@#@"��@"n�@"^5@"=q@!��@!�^@!��@!�7@!�7@!hs@ ��@ Q�@  �@ b@�;@|�@l�@\)@
=@�@ȴ@�R@��@�+@ff@{@�@@�@V@�@�/@z�@�@�m@ƨ@�F@�@dZ@S�@o@��@��@��@��@��@��@n�@-@�@�#@�7@x�@hs@&�@%@��@��@Ĝ@��@�@Q�@Q�@Q�@A�@  @�w@\)@+@
=@�@ȴ@��@ff@E�@E�@5?@@�@��@@�@O�@/@V@�D@I�@(�@�
@��@��@�@dZ@C�@�@��@�!@��@��@~�@n�@n�@^5@^5@^5@=q@-@�#@�^@x�@7L@Ĝ@Q�@ �@�w@��@\)@\)@\)@\)@K�@;d@
=@�@�R@v�@5?@@@�@�T@�T@��@@�-@��@�@O�@O�@?}@/@�@V@V@�@��@�@I�@�
@dZ@S�@
�@
��@
��@
��@
��@
��@
�\@
n�@
M�@
M�@
=q@
=q@
-@	�@	��@	x�@	G�@	7L@	&�@��@�u@r�@bN@ �@  @�@��@�@|�@\)@;d@�@��@�@ȴ@ȴ@�R@��@V@E�@5?@$�@{@@@�@O�@O�@?}@/@�@V@�@�j@�j@�D@jA���A���A���A���A��uA���A���A���A���A���A��uA��hA��\A��PA��hA��uA��hA��\A��hA��uA���A��hA��\A��hA��\A��\A��PA��\A��uA��hA��uA���A���A��hA��PA��\A��\A��\A��DA��7A��DA��DA��7A��A��+A��7A��7A��A��A��7A��7A��DA��A��DA��PA��\A��hA��hA��PA��PA��\A��hA��uA��hA��hA���A���A���A��hA��PA��\A��hA��uA��hA��\A��PA��\A��hA��hA��hA��PA��\A��hA��uA��\A��\A��\A��\A��PA��PA��\A��hA��hA��+A��A��7A��7A��DA��PA��DA��DA��PA��DA��7A��+A��DA��PA��DA��7A��+A�r�A�t�A�t�A�r�A�p�A�t�A�x�A�~�A�r�A�n�A�n�A�l�A�n�A�l�A�ffA�dZA�dZA�bNA�ffA�n�A�hsA�dZA�\)A�`BA�bNA�^5A�\)A�I�A�G�A�G�A�I�A�K�A�K�A�M�A�K�A�M�A�O�A�O�A�M�A�I�A�K�A�K�A�A�A�?}A�33A�7LA�1'A�5?A�/A�(�A� �A� �A� �A�"�A�"�A��A� �A��A�
=A�A���A��HA��#A���A��FA���A���A���A��hA�v�A�33A�  A��A��PA�
=A���A��-A�t�A�t�A�`BA�1'A��A�A�A�5?A��A��jA��uA�XA�I�A��A�A��yA��HA���A���A��7A�v�A�^5A�ZA�M�A�7LA�1'A�$�A��A�bA�VA�VA�oA�bA�%A���A��A��mA��#A���A���A���A���A���A�ȴA�ĜA���A��jA��RA��9A��-A���A���A���A���A���A��hA��A�hsA�bNA�\)A�I�A�A�A�;dA�9XA�5?A�33A�-A�(�A�"�A��A�oA�
=A�1A�A�A�  A���A��A��A��yA��TA��;A��A���A���A���A���A���A�ȴA�ĜA�ĜA�A�A���A��jA��^A��RA��FA��9A��-A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A��hA��PA��7A��A�~�A�z�A�t�A�r�A�l�A�jA�bNA�S�A�I�A�E�A�C�A�?}A�?}A�?}A�=qA�;dA�9XA�9XA�7LA�5?A�33A�1'A�/A�-A�&�A� �A��A��A��A��A�{A�oA�oA�bA�VA�
=A�%A�A�A���A���A���A��A��A��A��A��A��`A��A���A���A��^A��jA�ƨA���A��
A��A��#A���A���A��-A���A�p�A�-A��A�{A�VA�A��A��A��A��yA��`A��HA��;A��#A���A��wA��^A��-A��!A���A��7A�x�A�n�A�XA�;dA�(�A�A���A��A���A��A�v�A�n�A�hsA�ffA�bNA�`BA�XA�5?A�oA���A��A��A��A��TA��/A��
A���A��FA��hA�dZA�O�A�5?A��A��FA�r�A��A���A��FA���A��hA��\A��DA��A�I�A�&�A�{A�A��A��
A��^A��A��A�A�A��A���A��wA�r�A�=qA���A���A���A���A��PA��+A��A��A�|�A�z�A�n�A�^5A�K�A�-A���A���A�=qA�
=A��HA��A���A��7A�~�A�x�A�ZA�$�A���A�~�A���A�"�A�
=A���A��/A���A���A�x�A�E�A��A�~�A�+A��hA�G�A��A���A��RA�ffA�1A��\A�n�A�ffA�VA�C�A��A��A���A�v�A�A�A�=qA�/A��A��A�JA���A��A��;A��
A�A���A�v�A�S�A�$�A���A��-A���A��A��A�1A��A���A�(�A��
A�VA�$�A�A�ȴA��A���A�z�A�hsA�dZA�`BA�`BA�\)A�\)A�Q�A�Q�A�K�A�I�A�E�A�=qA�+A�JA��A��/A���A���A���A��^A��FA���A��hA�hsA�XA�G�A�/A�{A��/A��+A�G�A���A���A��^A��+A�XA�"�A��FA�hsA�-A��A�1A�A���A��mA��-A�v�A�p�A�9XA�{A���A��-A��7A�^5A�K�A�I�A�?}A�9XA�/A� �A�VA�1A�A��;A���A���A��uA��7A�p�A�M�A�
=A���A��A��A��A�C�A�C�A�33A��A���A�ĜA���A��RA���A���A���A���A���A��\A��A�x�A�r�A�t�A�l�A�hsA�`BA�O�A�G�A�?}A�;dA�7LA�9XA�7LA�1'A�1'A�-A�(�A��A��A�bA�1A�mA��A��AƨA�wA��At�AC�A+A�A�A~��A~�RA~�uA~E�A}�A}�#A}��A}33A|��A|z�A|5?A{�;A{`BA{C�A{&�A{oA{Az�Az�\Az=qAy��Ay��Ay�Ay�7Ayp�AyO�Ay+Ax��Ax��Ax�DAxZAx1'Ax  Aw�Awp�AwC�Av��Avr�Av5?Av  Au�
AuAu�FAu��Au�Au��Au��Au��Au`BAt��At��At�DAtz�Atr�AtbNAtZAt=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                         A���A���A��hA��hA��uA��\A��uA��hA��DA��+A��A��DA��hA��hA��\A��hA��PA��DA��DA�t�A�p�A�ffA�\)A�I�A�K�A�5?A� �A���A��A�$�A���A�1'A��A� �A���A���A��^A���A�S�A�/A�
=A��yA���A�A��-A���A���A��\A�p�A�G�A�;dA�/A��A�VA���A��mA�ȴA���A�"�A��yA���A��+A���A�t�A�1'A��TA�p�A�;dA�~�A��A�A�A�JA��A�;dA���A�Q�A�r�A�p�A��A��yA�JA�5?A��;A�JA�S�A��HA�E�A�A��;A��`A���A�n�A��
A�O�A�ƨA��A�E�A��A�A~A�A{�wAy�AxZAvA�Au&�At(�As��Aq�Ap�RAp$�Ao��Ao�AnZAml�Al�\Ak`BAiƨAh��Ag&�Ad�+Ac�Ab��Ab �Ab{Aa��A`�A`(�A_�A^�/A^A\��A\1'AZ�+AYC�AV��AU;dAT��AS?}AR�AQ�AQ%AP9XAO�wAOhsAO%AN�\AN^5AM&�ALA�AK��AI�^AHȴAH~�AHffAG�#AFffAE�-AD�AChsAB�RAB1'AA��AA;dA@�A?��A>�!A=�A<�RA;l�A:��A9oA7O�A6�jA6-A5��A5A3ƨA3`BA2��A2ĜA2�A2A�A2 �A1�A1��A1oA0ZA/&�A.VA-S�A,I�A+�wA+hsA+�A*��A*�uA*  A)/A'��A'S�A&ȴA%�FA$�A#x�A#
=A"�jA"A!C�A �A�
A��AE�A��A/A�#A&�A�!AffA�-A$�AS�A��A�A�+A��A�A��A��A�AhsAffA-A��Ap�AoA5?A��AS�AA
I�A	�A	An�A�^A
=AE�AO�A��A�wA&�A�Av�A  Ap�A�A ��A b@��F@�C�@���@��+@�@�ƨ@��!@�&�@��+@��j@�K�@�@�  @�+@��@�u@�@��H@ꟾ@�@�P@�-@�^@���@䛦@��
@�"�@��@��@���@�K�@އ+@���@�{@�%@�j@��
@���@Ӆ@�ff@�I�@̼j@�@�33@�J@�z�@�+@�-@�`B@�&�@��j@�bN@�\)@��+@�J@���@��D@��;@��!@��@��T@�@��-@���@��h@�x�@�%@�l�@��j@�Q�@��P@�E�@�/@��9@�1'@���@��T@��u@�33@��\@��^@��@��/@��@��F@�S�@���@�{@��^@�x�@�?}@���@�bN@��;@��F@���@��H@�ff@��@��@�x�@��@�  @��@�dZ@��H@�~�@��@���@��^@�x�@��@�Ĝ@�I�@��
@��@�ff@�V@�M�@�5?@���@��7@���@���@�I�@�1@�ƨ@��P@�|�@�|�@�t�@�\)@�
=@���@�=q@�J@�`B@�Ĝ@�I�@��m@��@�|�@�33@��@���@��#@��@��j@�1'@�  @�  @��@��@��;@��F@�t�@�C�@�n�@�%@�z�@��@�S�@��@��H@��R@���@�V@��-@�?}@��j@���@�r�@�A�@�1'@��;@�l�@���@���@�-@��@���@���@��@��@��@�j@�Q�@��@���@��F@���@�|�@�l�@�+@���@�J@���@���@�O�@�z�@�(�@�1@+@~�@~v�@~{@}�h@}/@}V@|�@|�D@|j@|�@{�m@{�F@{dZ@z�!@z-@z�@zJ@y�@y�#@y�^@yhs@xĜ@xQ�@wl�@v�@v��@vff@vV@vE�@v@u�T@u�-@u`B@t�j@tZ@s��@sC�@r-@q��@qX@q�@pr�@o�@o��@o�@n�R@n5?@n@m�@m?}@l1@k�F@k�@kdZ@k"�@j��@j=q@i��@h�9@hb@g�w@g�@fȴ@f$�@e��@ep�@e�@d�@d�D@d�D@dz�@dj@c��@c��@c�@c�@ct�@cS�@c33@b�H@b^5@a�7@`Ĝ@`Ĝ@`��@a%@a7L@`��@`bN@`b@_�P@_;d@^��@^�R@^��@^5?@^@]@]�@\��@\I�@\1@\�@\�@[�@[C�@Z�H@Z�!@Y��@Y�7@X��@X�u@X�@XbN@X1'@X1'@W�@W+@V�@VV@V5?@V{@U��@T��@T�@TZ@S�m@S�F@S��@S��@S�@SS�@S"�@So@R��@R-@Qx�@Q7L@P��@P��@P�9@P�@PQ�@Pb@O�@O�P@O\)@O+@N�y@N��@N5?@M��@M�h@M?}@L��@L�@L��@L9X@K�m@KC�@J�\@J~�@JM�@I��@Ihs@IX@I7L@H��@H�u@H �@G�@G�w@G+@F��@F��@F�+@F$�@E@E`B@D�@Dj@D1@C��@CC�@C@B�@B��@B=q@A��@AX@AG�@AG�@A7L@A7L@A7L@@��@@�u@@b@@  @@  @?�;@?�@?�P@?|�@?l�@?\)@?;d@>�R@>5?@=�T@=��@=�h@=�@=O�@<�j@<�D@<j@<�@;�m@;t�@;"�@:��@:~�@:n�@9�#@9x�@9%@8r�@8A�@8  @7�;@7�w@7��@7|�@7l�@7\)@7;d@7+@7�@6��@6�y@6�@6�+@6{@5�T@5@5�h@5�h@5�@5p�@5p�@5O�@5V@4I�@3�@3S�@3"�@3"�@3"�@3@2��@2�\@2=q@2J@1�#@1��@1X@1�@0�@0A�@0A�@0A�@01'@0 �@/�;@/K�@.��@.ff@.E�@.5?@.$�@-�@-��@-�-@-��@-�h@-�h@-�@-p�@-p�@-`B@-O�@-V@,��@,I�@,9X@,(�@,�@+��@+�m@+�m@+ƨ@+��@+33@*��@*�\@*n�@*^5@*=q@*=q@*-@*J@)��@)�^@)7L@)�@(��@(��@(Q�@(b@'�;@'\)@'+@'
=@&��@&ȴ@&��@&$�@%@%?}@$�/@$I�@$1@#��@#�F@#�@#t�@#S�@#C�@#o@#@"��@"n�@"^5@"=q@!��@!�^@!��@!�7@!�7@!hs@ ��@ Q�@  �@ b@�;@|�@l�@\)@
=@�@ȴ@�R@��@�+@ff@{@�@@�@V@�@�/@z�@�@�m@ƨ@�F@�@dZ@S�@o@��@��@��@��@��@��@n�@-@�@�#@�7@x�@hs@&�@%@��@��@Ĝ@��@�@Q�@Q�@Q�@A�@  @�w@\)@+@
=@�@ȴ@��@ff@E�@E�@5?@@�@��@@�@O�@/@V@�D@I�@(�@�
@��@��@�@dZ@C�@�@��@�!@��@��@~�@n�@n�@^5@^5@^5@=q@-@�#@�^@x�@7L@Ĝ@Q�@ �@�w@��@\)@\)@\)@\)@K�@;d@
=@�@�R@v�@5?@@@�@�T@�T@��@@�-@��@�@O�@O�@?}@/@�@V@V@�@��@�@I�@�
@dZ@S�@
�@
��@
��@
��@
��@
��@
�\@
n�@
M�@
M�@
=q@
=q@
-@	�@	��@	x�@	G�@	7L@	&�@��@�u@r�@bN@ �@  @�@��@�@|�@\)@;d@�@��@�@ȴ@ȴ@�R@��@V@E�@5?@$�@{@@@�@O�@O�@?}@/@�@V@�@�j@�j@�DG�O�A���A���A���A���A��uA���A���A���A���A���A��uA��hA��\A��PA��hA��uA��hA��\A��hA��uA���A��hA��\A��hA��\A��\A��PA��\A��uA��hA��uA���A���A��hA��PA��\A��\A��\A��DA��7A��DA��DA��7A��A��+A��7A��7A��A��A��7A��7A��DA��A��DA��PA��\A��hA��hA��PA��PA��\A��hA��uA��hA��hA���A���A���A��hA��PA��\A��hA��uA��hA��\A��PA��\A��hA��hA��hA��PA��\A��hA��uA��\A��\A��\A��\A��PA��PA��\A��hA��hA��+A��A��7A��7A��DA��PA��DA��DA��PA��DA��7A��+A��DA��PA��DA��7A��+A�r�A�t�A�t�A�r�A�p�A�t�A�x�A�~�A�r�A�n�A�n�A�l�A�n�A�l�A�ffA�dZA�dZA�bNA�ffA�n�A�hsA�dZA�\)A�`BA�bNA�^5A�\)A�I�A�G�A�G�A�I�A�K�A�K�A�M�A�K�A�M�A�O�A�O�A�M�A�I�A�K�A�K�A�A�A�?}A�33A�7LA�1'A�5?A�/A�(�A� �A� �A� �A�"�A�"�A��A� �A��A�
=A�A���A��HA��#A���A��FA���A���A���A��hA�v�A�33A�  A��A��PA�
=A���A��-A�t�A�t�A�`BA�1'A��A�A�A�5?A��A��jA��uA�XA�I�A��A�A��yA��HA���A���A��7A�v�A�^5A�ZA�M�A�7LA�1'A�$�A��A�bA�VA�VA�oA�bA�%A���A��A��mA��#A���A���A���A���A���A�ȴA�ĜA���A��jA��RA��9A��-A���A���A���A���A���A��hA��A�hsA�bNA�\)A�I�A�A�A�;dA�9XA�5?A�33A�-A�(�A�"�A��A�oA�
=A�1A�A�A�  A���A��A��A��yA��TA��;A��A���A���A���A���A���A�ȴA�ĜA�ĜA�A�A���A��jA��^A��RA��FA��9A��-A��A��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A��hA��PA��7A��A�~�A�z�A�t�A�r�A�l�A�jA�bNA�S�A�I�A�E�A�C�A�?}A�?}A�?}A�=qA�;dA�9XA�9XA�7LA�5?A�33A�1'A�/A�-A�&�A� �A��A��A��A��A�{A�oA�oA�bA�VA�
=A�%A�A�A���A���A���A��A��A��A��A��A��`A��A���A���A��^A��jA�ƨA���A��
A��A��#A���A���A��-A���A�p�A�-A��A�{A�VA�A��A��A��A��yA��`A��HA��;A��#A���A��wA��^A��-A��!A���A��7A�x�A�n�A�XA�;dA�(�A�A���A��A���A��A�v�A�n�A�hsA�ffA�bNA�`BA�XA�5?A�oA���A��A��A��A��TA��/A��
A���A��FA��hA�dZA�O�A�5?A��A��FA�r�A��A���A��FA���A��hA��\A��DA��A�I�A�&�A�{A�A��A��
A��^A��A��A�A�A��A���A��wA�r�A�=qA���A���A���A���A��PA��+A��A��A�|�A�z�A�n�A�^5A�K�A�-A���A���A�=qA�
=A��HA��A���A��7A�~�A�x�A�ZA�$�A���A�~�A���A�"�A�
=A���A��/A���A���A�x�A�E�A��A�~�A�+A��hA�G�A��A���A��RA�ffA�1A��\A�n�A�ffA�VA�C�A��A��A���A�v�A�A�A�=qA�/A��A��A�JA���A��A��;A��
A�A���A�v�A�S�A�$�A���A��-A���A��A��A�1A��A���A�(�A��
A�VA�$�A�A�ȴA��A���A�z�A�hsA�dZA�`BA�`BA�\)A�\)A�Q�A�Q�A�K�A�I�A�E�A�=qA�+A�JA��A��/A���A���A���A��^A��FA���A��hA�hsA�XA�G�A�/A�{A��/A��+A�G�A���A���A��^A��+A�XA�"�A��FA�hsA�-A��A�1A�A���A��mA��-A�v�A�p�A�9XA�{A���A��-A��7A�^5A�K�A�I�A�?}A�9XA�/A� �A�VA�1A�A��;A���A���A��uA��7A�p�A�M�A�
=A���A��A��A��A�C�A�C�A�33A��A���A�ĜA���A��RA���A���A���A���A���A��\A��A�x�A�r�A�t�A�l�A�hsA�`BA�O�A�G�A�?}A�;dA�7LA�9XA�7LA�1'A�1'A�-A�(�A��A��A�bA�1A�mA��A��AƨA�wA��At�AC�A+A�A�A~��A~�RA~�uA~E�A}�A}�#A}��A}33A|��A|z�A|5?A{�;A{`BA{C�A{&�A{oA{Az�Az�\Az=qAy��Ay��Ay�Ay�7Ayp�AyO�Ay+Ax��Ax��Ax�DAxZAx1'Ax  Aw�Awp�AwC�Av��Avr�Av5?Av  Au�
AuAu�FAu��Au�Au��Au��Au��Au`BAt��At��At�DAtz�Atr�AtbNAtZAt=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                         ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�B�B	lB	B	B�B�B	B�B�B�BfB�B	B	B�B�B�B�B�B�B�B�BB�B�BBB_B�B��B�|B��B�+BBBDB�B
	B�B�B�BB�BBB{BGB;B�(B��B��B�8B��B��B�B��B�B�B�HB�5B��B�B��B�RB�qB��B��B��BcBv�B^�BI�BC�B+�B�B��B�BȴB��B��B�+B.BrGB]/B9XB*�B!�B�B�B�B�yB�0B�wB�LB��B�B�1B��B��Bw2Bj�BaHBT�BK�BD�B@OB9$B/�B+kB&�B"�BBB�B�B
��B
��B
�;B
�2B
�KB
��B
ӏB
�TB
�B
��B
��B
B
��B
�RB
�[B
�qB
��B
�xB
�MB
�fB
��B
�uB
y	B
v�B
tTB
o B
j�B
hsB
f2B
e,B
c B
`vB
XyB
S&B
MjB
D�B
B[B
@B
?�B
6FB
3�B
-�B
)�B
$B
!bB
�B
�B
B
�B
�B
B
�B	�.B	�JB	��B	��B	�B	�KB	��B	�B	�|B	ߤB	�5B	��B	��B	یB	�QB	ٴB	�EB	�9B	�,B	�}B	�#B	�EB	��B	�}B	�BB	��B	�*B	��B	��B	��B	��B	��B	��B	�'B	��B	�4B	�=B	��B	��B	��B	{�B	w�B	rGB	oiB	o B	l�B	i�B	bNB	`vB	^jB	Z�B	Y�B	T�B	R�B	OB	MB	H�B	IRB	EmB	A�B	>�B	@�B	:�B	9$B	8RB	7LB	5tB	5B	1�B	0!B	0!B	-B	-CB	*�B	(�B	($B	$tB	$@B	"�B	VB	 \B	xB	�B	=B	�B	�B	_B	�B	SB	�B	MB	B	oB	:B	4B	�B	VB	B	
=B	�B	_B	�B	�B	�B	{B	�B	�B	�B	�B	�B	 �B��B	 iB��B�cB��B	  B��B��B��B�B�(B�B��B�B�xB��B�PB�B��B��B�.B	;B	 iB	B	B	�B	B	�B	�B	�B	+B	1B	�B	�B		7B		7B	�B	�B	B	�B	B	�B	�B	B	B	"B	:B	�B	B	�B	$B	�B	+B	qB	B	VB	"4B	#B	%�B	&�B	&�B	)�B	*0B	+kB	-CB	/�B	0�B	2aB	2�B	3�B	5?B	7B	6�B	6�B	8�B	:*B	:^B	:^B	<B	?�B	AUB	B[B	C�B	EmB	GB	HKB	H�B	I�B	J�B	K�B	MjB	OvB	P�B	RTB	X�B	XEB	XyB	XyB	Y�B	\)B	a|B	a�B	b�B	d&B	e`B	f�B	f�B	f�B	f�B	g8B	h�B	j�B	lWB	m�B	p�B	rGB	t�B	wfB	xlB	y>B	zB	zB	|�B	�B	��B	�7B	��B	��B	��B	�(B	��B	��B	��B	��B	��B	�YB	�VB	��B	�XB	�eB	�B	�CB	�B	�}B	�!B	��B	�B	�dB	�B	�B	�B	�BB	��B	��B	ǮB	ɆB	�jB	�B	�B	бB	�gB	خB	�B	چB	یB	�dB	ߤB	�BB	�|B	��B	�B	�&B	�DB	�/B	�iB	�oB	�B	�JB	�"B	�(B
�B
GB
�B
YB
�B

rB

�B
DB
�B
"B
�B
�B
�B
�B
�B
_B
�B
�B
1B
�B
�B
�B
~B
!B
#:B
%B
&�B
($B
(�B
(�B
)�B
+6B
+�B
.}B
1'B
3hB
4�B
8RB
=�B
@OB
A B
A�B
DgB
F?B
G�B
J#B
LdB
N�B
OvB
O�B
S�B
ZQB
[�B
\]B
\�B
]dB
]�B
^B
]�B
^�B
_�B
`�B
d&B
e�B
h>B
iDB
jB
k�B
m�B
m�B
m�B
m�B
n/B
p;B
q�B
r|B
r�B
r�B
s�B
s�B
v�B
{B
}�B
~�B
~�B
cB
�B
�B
�uB
��B
��B
�_B
�1B
�B
��B
��B
�DB
�xB
�B
�~B
�VB
��B
��B
�.B
� B
��B
�B
�SB
��B
�kB
�	B
��B
�IB
�~B
��B
�OB
�OB
��B
�bB
��B
�B
�@B
�tB
��B
��B
�*B
��B
��B
�=B
�qB
��B
�B
�wB
�}B
�}B
�OB
�'B
��B
��B
�tB
�tB
��B
�LB
��B
��B
��B
�*B
��B
�dB
�6B
�B
�B
�qB
��B
�wB
�B
�B
�HB
��B
��B
�aB
�9B
�9B
ŢB
��B
ȀB
ȴB
�B
ɆB
�XB
�^B
˒B
�0B
��B
�B
�B
�B
�HB
�B
��B
҉B
��B
��B
��B
�mB
�
B
�
B
�sB
��B
�QB
��B
�#B
�#B
�#B
�#B
�#B
یB
ܒB
��B
�B
��B
�jB
�;B
ߤB
��B
�B
�B
�B
�|B
�B
�B
��B
��B
��B
�B
�,B
�`B
��B
�B
�8B
�
B
�B
�yB
�B
��B
��B
�B
�"B
�]B
�B
��B
��B
�cB
�B
�B
� B
� B
�5B
�5B
�iB
�B
�B
�B
�;B
��B
�AB
�AB
�B
�B
�B
��B
�B
��B
��B
��B
��B
�%B
��B
�ZB
�ZB
��B
��B
�`B
��B
�2B
��B
��B
��B
�	B
��B
��B
��B
��B
��B
��B
�B
�PB
��B
��B
��B
��B
�(B
��B
��B
��B
��B
��B
��B
��B
�.B
�.B
�.B
�.B
��B �BB;B;B;BoBoBoB�B�B�B{B�B�BBBBMB�B�BSB�B�B�B�B�B+B�B�B�B�B	B	�B
=BBxBJB�B�B"B�B�B�B(B\B\B�B�B.B�B�B B�BoB�BuB�B�BMBBBMB�B�B�BSB�B$BYB�B�B�B_B�BeBB	B�BqB�B�B�BCB�B�B�BBB�B�B�BOBOBOB�B�B!B!B�B�B�B�B�B�B�B�B�B�B \B �B �B �B �B!bB!bB!�B!�B"hB"�B#B#nB#�B$B$B$B$@B$tB$�B%zB&�B'RB'RB'RB(XB(XB(�B)*B)_B)�B)�B)�B)�B*eB*�B*�B*�B*�B*�B+B*�B*�B+B+B+B+B+�B+kB,B,=B,�B-B,�B-�B-�B.IB.IB.IB.IB.IB.}B.�B.�B/B/�B0!B0UB0UB0UB0�B0�B0�B0�B0�B0�B0�B1[B1'B1[B1�B1�B1�B1�B1�B1�B2-B2�B3�B3�B3�B4nB4�B4�B4�B4�B4�B5B5?B5?B5?B5tB5?B5tB5�B6B6FB6FB6B6B6zB7B6�B7LB7�B7�B7�B8B8�B8�B8�B8�B8�B9$B9XB9XB9XB9XB9�B9�B9�B:*B:^B:^B:^B:�B:�B:�B;0B;0B;0B;0B;0B;dB;dB;dB<B<jB�B
=B�B	lB
�B�B	B	BfB
	BDB�B1B	�B�B�B	�B	�B	�B1B1B
rB�B1B
=B
	B	B�B�B	�B�B�B�B
�B	7B
	B	BfB	B	lBfB1B�B	lB�B�B1B	B	7B�B�B	lB
=BfBfB+B�B1B	lB�B�B+BfB
=B	�B	B�B�B	�B	�B	�B1B�B�B	7B	�B	�B�B�B	B	�B	�B�B1B	lB	�B	BfB�B	B	�B�B�BfB�B�B+B�B�B1B�B�B�B�B�B�BYB�BfB%B�B%BYB�B_B%B�B�B�B�B+B�B�B�B�B%B�B�B�BB�B�B%B�B�B%B{B�BYBYB�B�BMB�B�B�BMB�BSB�BMB�BMB�BSBB�B�B �B%B�B�B�BBGBMB�BB1BGB�B�B�BSBfBSB+B�BAB	�B%B
=B�B(B)_B+B
rB�B�B  BxB~B��B��B�vB�B�B�|B�"B�B�B�B�B��B�|B�/B�GB��B�B� B�B�B�B��B��B�VB�.BB	�B�B�B~B�B"B�B�B~BB�B~B�BxB
�B
�B
	B	�B
�B
rBxB
=BxB
�B�BDB�B�B
�B�B_B�B�B�B�B�BfB�B�B�B%B�B�BSB�B�B�B�B�BB�B�B{B�B{B�B�B{BGBBuB�BuBuBBuBuBB�B�BuB�BB{B�BB�BBAB{B{B�B�BMB�B�B�B�B�BBoB��B;B��BB;B��B��B�"B��B��B��B�"B��B��B�B�B�B�JB�xB�B�	B��B�rB�>B��B�fB�8B�lB�lB�lB�B�2B��B��B��B�ZB�B�TB�MB��B�B��B�B�B��B�B��B�B� B�]B�cB�GB�GB�MB��B��B��B��B�B��B�AB�2B��B�,B�fB��B�B�B��B�vBߤBޞB�jB��BޞB�dB�#B�BںBרB��BԕB�?B��B��B�TB͟B��B�-B��B��B�B�qB��B��B��B�RB�6B��B��B�!B�B��B�IB�qB�*B�B�kB��B��B��B�hB�zB��B��B�CB� B�fB��B�YB��B�%B��B�PB�oB�iB�B~�B�4By�Bx�B.By>BqABqvBsMBkQBc�BV�BU2B_BR BL�BJ#BH�BIRBH�BGzBGzBC�BC�BC�BB'BB[B8B*�B(XB"�BVBCB�B�B�B B&�B~BVB��B��B�QB�B�`B��B�BܒB��B��B��B�RB�B��B�nB��B�B��B��B�=B��B��B��B��B�YB�B��B��B��B�+B�B�B�uB� B�iB�iB}"B{�B}"Bv�Bv�By	Bp;BqvBffBp�Bf�B_B\]Bd�BZ�BZ�BS[B=qBB[BHKBF?BL�B2�B/�B-�B,�B,B-B+�B,�B+B+kB)*B)�B'�B(�B+�B&�B%B#B!-B�B�B�B�B�BCB�B�B�B+B�B�BBBYB�BB��BoBB��B�|B�/B�]B��B�B�B�2B�ZB�B�B��B�B�dB��B��B՛B��B�,BбBѷB� B�HB��B�6B�vB͟B�XB�?B��B�B�B�B��B�OBB��B�}B��B��B�'B�LB��B�B��B�B��B��B��B��B��B��B��B��B��B��B�OB��B��B�IB�B�B��B�	B�qB��B��B��B�B��B�eB��B�YB�YB��B�B�B�B��B�MB�\B��B�~B��B�DB��B��B��B��B�oB��B��B�AB|�BxBz�By>BqBp;BpoBn/Bo5BqvBoiBl�BjKBiBh�BgBe�Be,Bd�Bb�Bd�B`�B_BaB^B[�BY�B[�BYBV9BP�BRTBPBNpBOBBL�BL0BL�BK^BQ�BT,BK�BGBFtBFtBF?BD3BE�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                         G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                         G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         202103261700362021032617003620210326170036202103261700362021032617003620210326170036SI  SI  ARFMARFM                                                                                                                                                2021030102372820210301023728IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021031101012320210311010123QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021031101012320210311010123QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021032510164620210325101646IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021032617005220210326170052IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0ARGO_for_DMQC Climatology Version 2020V03                       ARGO_for_DMQC Climatology Version 2020V03                       2021032617005220210326170052IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021032617005220210326170052IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                