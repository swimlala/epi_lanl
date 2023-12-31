CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-01-08T19:32:49Z creation; 2023-04-26T19:14:29Z DMQC;      
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20200108193249  20230426191429  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               I   IAA  AOAO7316_008644_073                 7316_008644_073                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @���s��@���s��11  @���PH�@���PH�@(��0j+@(��0j+�d���D��d���D�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q@   @@  @}p�@�  @�(�@�p�@��RA\)A�RA,(�A@��AaG�A�  A�  A�  A�  A�  A�  A߮A�  B (�B(�B(�B  B   B'�
B/�B7�
B@(�BH(�BP  BX  B`  Bg�
Bo�Bx  B�{B�(�B�Q�B��B�  B�  B�  B�  B�  B�  B�{B�  B�  B�(�B�  B��B��B�  B�  B�{B�{B��B��B�  B��B��B�  B�  B�  B��B��B�  C 
=C{C{C
=C
=C

=C  C  C  C  C  C  C  C  C
=C
=C��C!�C#�C%��C(  C*
=C,
=C-��C0
=C2  C4  C6  C8  C:
=C<  C=��C@  CB  CD  CF  CH  CI��CK�CM��CP
=CR  CS��CU�CW��CZ  C\  C]��C_��Cb  Cc��Ce�Cg��Cj  Cl
=Cn{Cp  Cq�Cs��Cv  Cx
=Cz  C|  C}��C�  C�  C�C�  C�  C�  C�  C�  C���C���C���C�  C���C���C�  C�  C�  C�  C�  C�C�  C���C���C���C���C�C�C�C�C�C�
=C�  C���C�  C�  C�C���C���C���C�  C�
=C�\C�C���C�C�
=C�C�  C�
=C�C�  C�C�
=C�\C�\C�C���C���C�  C�C���C���C���C���C���C���C���C���C���C�  C�
=C�C�  C���C��C���C���C���C�  C�  C�  C�  C�C�  C���C�C�  C���C�  C�  C�  C�  C�C�C�C�C���C��C���C���C�  C�C�  C���C���C���C���C���C�  C���C���C�  C�C�
=C�  C�C�
=C�C���C���C���C�  C�  C�C�  C�  C�  C�  D �D }qD  D� D�qD}qD  D� D�D� D�qD}qD�qD}qD�qD� D  D��D�qD	z�D
  D
��D�D��D�D� D  D}qD�D�D  D� D�D}qD��D� D�D� D�qD� D�D� D  D}qD��D}qD�D��DD�DD� D�qD� D�D� D�qD� D�D� D��D}qD  D��D �D }qD!  D!� D"  D"� D#  D#� D$  D$� D%�D%��D%�qD&� D'�D'� D'�qD(� D(�qD)}qD)�qD*}qD+  D+��D,�D,� D-�D-� D.  D.��D/�D/}qD/�qD0��D1  D1� D2  D2}qD3  D3� D4  D4� D4�qD5}qD6�D6��D7  D7��D8  D8}qD9  D9� D:  D:��D;�D;��D;�qD<}qD=  D=� D>  D>}qD>�qD?��D@�D@��DA  DA��DB�DB��DC�DC��DD�DD� DEDE��DF  DF��DG�DG��DH�DH}qDH��DI� DJ�DJ}qDJ�qDK� DL  DL� DM�DM� DN  DN� DN�qDO� DP  DP}qDP��DQ� DR  DR� DSDS�DT�DT� DU  DU� DU�qDV� DV�qDW� DX�DX��DY�DY� DZ�DZ�D[  D[}qD[�qD\��D]�D]� D]�qD^}qD_  D_� D`  D`� D`�qDa� Db  Db� Dc�Dc�Dd  Dd}qDe�De�Df�Df}qDg  Dg�Dh�Dh}qDi  Di�Dj�Dj��Dk  Dk}qDl  Dl��Dm  Dm}qDm��Dn}qDo  Do}qDp  Dp� Dq  Dq��Dq�qDr� Ds  Ds}qDt  Dt� Du  Du��Du�qDv}qDv�qDw}qDx�Dx� Dx�qDy��DzDz�D{  D{� D{�qD|}qD}�D}��D~  D~��DD��D�qD�@ D�~�D�� D�  D�AHD�� D���D�HD�AHD���D�� D�  D�AHD�~�D���D���D�>�D�~�D�� D�  D�@ D�� D���D�  D�@ D�� D�D�  D�=qD��HD��HD�  D�AHD��HD��HD�HD�@ D�� D�D�HD�@ D�~�D���D�  D�>�D�}qD��qD���D�@ D�� D���D��qD�@ D���D�D�  D�>�D�� D�� D�HD�AHD�~�D�� D�HD�@ D�� D�� D�HD�AHD��HD�� D�  D�@ D�� D�� D�  D�@ D�� D��HD�  D�>�D�}qD���D�  D�AHD���D�� D��qD�@ D��HD���D���D�@ D�� D�� D�  D�AHD�� D��qD�  D�@ D�� D���D�  D�AHD�� D�� D�HD�AHD�� D�� D�  D�>�D�� D�� D�  D�AHD�� D��qD���D�>�D�~�D�� D���D�@ D�� D���D�  D�AHD�� D��HD�HD�@ D�~�D��qD�  D�AHD��HD�� D�  D�@ D�~�D���D��qD�<)D�~�D�� D���D�=qD�� D�D��D�B�D��HD�� D�  D�>�D�~�D���D�HD�C�D���D�D��D�AHD��HD�� D�HD�AHD���D�D�HD�>�D�~�D���D�  D�@ D�� D��HD�HD�>�D�}qD��qD��qD�=qD�~�D���D�  D�AHD�� D��HD�  D�>�D�� D���D���D�>�D�� D�� D�  D�@ D��HD���D�HD�B�D��HD��qD���D�>�D�~�D���D���D�@ D�� D�� D�HD�AHD�~�D�� D�  D�>�D�� D�� D�  D�@ D��HD��HD��D�B�D��HD��HD�  D�@ D�~�D���D���D�>�D�~�D���D�  D�@ D�� D���D��qD�>�D�� D��HD�  D�>�D�~�D���D�  D�AHD��HD�� D���D�@ D��HD�� D���D�@ D�� D�� D���D�=qD D��HD�  D�@ DÀ D��HD�  D�@ DĀ D�� D�  D�@ D�~�D��HD�HD�@ Dƀ D��HD�HD�@ Dǀ D�� D���D�@ DȁHD�� D�HD�@ Dɀ D�� D���D�@ Dʀ D��HD�  D�>�D�}qD�� D�HD�>�D̀ D��HD�HD�AHD͂�D�� D���D�AHD�~�D�� D�  D�@ Dπ D�� D���D�AHDЀ D�� D�  D�>�D�~�D�� D���D�>�D�~�DҾ�D�  D�AHDӁHD��HD�HD�@ DԀ DԾ�D�  D�B�DՁHD��HD�  D�AHDր D�� D���D�=qD�~�D�� D�  D�@ D؀ Dؾ�D�  D�@ Dـ D��HD�  D�>�Dڀ D�� D�  D�>�D�~�D۾�D��qD�=qD�}qDܽqD���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D߀ D�� D���D�>�D�~�DྸD���D�@ D� D�� D�HD�B�D�HD�� D��D�AHD� D��HD��D�AHD� D侸D��qD�>�D�~�D��HD���D�=qD� D��HD�HD�@ D�HD��HD�HD�B�D�HD�� D�  D�@ D�HD��HD�  D�B�D�HD�� D���D�>�D�~�D�� D�  D�@ D� D�� D�  D�@ D�~�D���D���D�>�DD�D�  D�@ D�HD�� D�HD�B�D��HD�� D�  D�@ D�~�D�D�  D�AHD�~�D�D�  D�@ D�~�D�D���D�>�D�~�D���D���D�@ D��HD��HD�  D�AHD�� D�� D���D�=qD��HD�� D�  D�@ D�� D��HD�HD�@ D��HD�� D�  D�,�D�k�?L��?aG�?�=q?�{?Ǯ?�G�@�\@��@+�@8Q�@J=q@aG�@s33@��
@���@�z�@�G�@��@���@���@��
@У�@�Q�@�  @�@�z�@��RA�
AQ�A{A�
AQ�Ap�A"�\A(Q�A.{A1�A6ffA:�HA@��AEAJ=qAO\)ATz�AZ=qA_\)Ac�
AhQ�Amp�As33Aw�A|(�A���A�33A�A�Q�A��\A�p�A�  A��\A�z�A�\)A�=qA���A�
=A���A�z�A��A��A�(�A�
=A��A���A�
=A��A���A��A��A�z�A�
=A��A�(�AθRAљ�A�(�A�ffA���A�33A�{A��A��HA�p�A�  A��HA�p�A�A�=qA�p�A��A��A���A��B�B�\B�
B��B=qB�
B��B
=qB�B�BffB�B��B�\B�
B��B=qB�B��BffB�B��B{B�B ��B!B#33B$z�B%B'
=B((�B)p�B*�HB,  B-�B.=qB/�B1�B2=qB3\)B4Q�B5B7
=B8(�B9G�B:�\B;�
B=G�B>=qB?\)B@��BB{BC\)BDQ�BE��BF�HBH(�BIG�BJffBK�BL��BN=qBO\)BPz�BQBS33BTQ�BUp�BV�\BW�
BX��BY�B[
=B\(�B]p�B^�RB_�B`��BaBc
=Bd(�BeG�Bf{Bg
=Bg�
Bh��Bj=qBk33Bl(�Bm�Bn{Bo33Bpz�BqBs
=Bt  Bup�Bv�HBxQ�By�B{\)B|��B}�B\)B�ffB�33B��B��\B�33B��
B�z�B���B���B�(�B���B�
=B�\)B��B��B�(�B�ffB���B��HB��B�\)B��B�B��B�(�B�Q�B��\B���B�
=B�G�B��B�B�  B�=qB�ffB���B��HB�
=B�G�B�p�B��B�  B�=qB�z�B��RB���B�33B�\)B��B�B��B�(�B�Q�B��\B���B�
=B�G�B��B�B�  B�(�B�Q�B�z�B��RB���B�33B�p�B��B�  B�=qB�z�B��RB���B�33B�p�B��B��B�(�B�ffB���B���B�33B��B��
B�  B�Q�B��RB���B�G�B���B��B�=qB�z�B��RB�
=B�G�B���B��B�=qB��\B��HB�33B��B��B�(�B��\B���B�G�B��B�  B�Q�B���B�
=B�\)B��B�  B�Q�B���B���B�G�B��B�  B�ffB��RB��B�p�B��
B�=qB��\B��HB�G�B��B�  B�ffB���B�33B��B��B�=qB��\B���B�G�B���B�  B�Q�B���B�
=B�\)B�B�{B�ffB���B��B��B��
B�(�B�z�B��HB�G�B���B��B�=qB���B���B�\)B��B��B�Q�B���B���B�G�B���B��
B�(�B�z�B���B��B�\)B�B�  B�Q�B���B���B�33B��B��
B�(�B�ffB���B��B�p�B�B�{B�ffB��RB�
=B�\)B�B�  B�Q�B���B�
=B�\)B��B�  B�ffB��RB�
=B�p�B�B�(�B�z�B���B�33BÅB��B�Q�Bģ�B�
=B�p�B��
B�=qBƣ�B��BǅB��B�Q�BȸRB�33Bə�B�  B�ffBʸRB��B˅B��B�Q�B̸RB��BͅB�  B�ffB��HB�\)B�B�=qBУ�B��BхB�  B�ffB��HB�G�B�B�=qBԣ�B�33BծB�=qBָRB�33B�B�=qBظRB�G�B�B�=qBڸRB�33B�B�=qBܸRB�33BݮB�=qB���B�G�B��
B�ffB�
=B�B�{B��B�33B�B�Q�B���B�G�B��
B�ffB��HB�\)B��B�z�B���B�B�{B��B�33B��
B�ffB��HB�p�B��B�z�B���B�B�{B��B�33B�B�Q�B��HB�p�B�  B�z�B�
=B��B�{B��\B�
=B���B�=qB���B�G�B��
B�Q�B��HB�\)B��
B�Q�B���B�\)B��B�z�B�
=B��C   C =qC z�C �RC ��C33C�C��C
=CG�C�CC  C=qCz�CC
=CG�C�C��C  C=qCz�C�RC��CG�C�CC  C33Cp�C��C�C�CffC�C�C	(�C	ffC	��C	�
C

=C
G�C
�\C
��C
=CG�C�C�RC�C(�C\)C��C�
C�C\)C�\C��C��C33CffC�C�HC(�CffC��C�
C{CG�Cz�C�RC��C33Cp�C�C�HC{CG�C�C��C{CQ�C�CC  C=qC�C�
C�C\)C��C�
C�CffC�RC�C(�Cp�C�RC
=CG�C�CC
=CQ�C��C�C(�CffC��C  C=qC�C��C
=CG�C��C�HC(�Cz�C�RC��C33Cz�CC{C\)C��C�C (�C p�C �RC!
=C!\)C!��C!�
C"�C"ffC"�RC#  C#Q�C#�\C#�
C${C$ffC$�RC%
=C%Q�C%��C%�HC&33C&�C&�
C'{C'\)C'��C(  C(Q�C(��C(�HC)(�C)z�C)�
C*�C*p�C*�RC+  C+Q�C+�C,  C,G�C,�\C,�
C-(�C-�C-�
C.(�C.p�C.C/{C/ffC/C0
=C0\)C0�C0��C1G�C1��C2  C2G�C2�\C2�HC3(�C3z�C3�
C433C4�C4��C5{C5ffC5�RC6
=C6ffC6�RC7
=C7Q�C7��C8  C8Q�C8��C8��C9G�C9��C9��C:Q�C:��C:�C;33C;�\C;��C<G�C<��C<�HC=33C=�\C=�C>=qC>�\C>�HC?=qC?��C?�C@=qC@�\C@�HCA33CA�\CA�CBG�CB��CB�HCC33CC��CC��CDG�CD��CD�CE33CE��CE��CFG�CF�\CF�HCG=qCG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                 ?�=q@   @@  @}p�@�  @�(�@�p�@��RA\)A�RA,(�A@��AaG�A�  A�  A�  A�  A�  A�  A߮A�  B (�B(�B(�B  B   B'�
B/�B7�
B@(�BH(�BP  BX  B`  Bg�
Bo�Bx  B�{B�(�B�Q�B��B�  B�  B�  B�  B�  B�  B�{B�  B�  B�(�B�  B��B��B�  B�  B�{B�{B��B��B�  B��B��B�  B�  B�  B��B��B�  C 
=C{C{C
=C
=C

=C  C  C  C  C  C  C  C  C
=C
=C��C!�C#�C%��C(  C*
=C,
=C-��C0
=C2  C4  C6  C8  C:
=C<  C=��C@  CB  CD  CF  CH  CI��CK�CM��CP
=CR  CS��CU�CW��CZ  C\  C]��C_��Cb  Cc��Ce�Cg��Cj  Cl
=Cn{Cp  Cq�Cs��Cv  Cx
=Cz  C|  C}��C�  C�  C�C�  C�  C�  C�  C�  C���C���C���C�  C���C���C�  C�  C�  C�  C�  C�C�  C���C���C���C���C�C�C�C�C�C�
=C�  C���C�  C�  C�C���C���C���C�  C�
=C�\C�C���C�C�
=C�C�  C�
=C�C�  C�C�
=C�\C�\C�C���C���C�  C�C���C���C���C���C���C���C���C���C���C�  C�
=C�C�  C���C��C���C���C���C�  C�  C�  C�  C�C�  C���C�C�  C���C�  C�  C�  C�  C�C�C�C�C���C��C���C���C�  C�C�  C���C���C���C���C���C�  C���C���C�  C�C�
=C�  C�C�
=C�C���C���C���C�  C�  C�C�  C�  C�  C�  D �D }qD  D� D�qD}qD  D� D�D� D�qD}qD�qD}qD�qD� D  D��D�qD	z�D
  D
��D�D��D�D� D  D}qD�D�D  D� D�D}qD��D� D�D� D�qD� D�D� D  D}qD��D}qD�D��DD�DD� D�qD� D�D� D�qD� D�D� D��D}qD  D��D �D }qD!  D!� D"  D"� D#  D#� D$  D$� D%�D%��D%�qD&� D'�D'� D'�qD(� D(�qD)}qD)�qD*}qD+  D+��D,�D,� D-�D-� D.  D.��D/�D/}qD/�qD0��D1  D1� D2  D2}qD3  D3� D4  D4� D4�qD5}qD6�D6��D7  D7��D8  D8}qD9  D9� D:  D:��D;�D;��D;�qD<}qD=  D=� D>  D>}qD>�qD?��D@�D@��DA  DA��DB�DB��DC�DC��DD�DD� DEDE��DF  DF��DG�DG��DH�DH}qDH��DI� DJ�DJ}qDJ�qDK� DL  DL� DM�DM� DN  DN� DN�qDO� DP  DP}qDP��DQ� DR  DR� DSDS�DT�DT� DU  DU� DU�qDV� DV�qDW� DX�DX��DY�DY� DZ�DZ�D[  D[}qD[�qD\��D]�D]� D]�qD^}qD_  D_� D`  D`� D`�qDa� Db  Db� Dc�Dc�Dd  Dd}qDe�De�Df�Df}qDg  Dg�Dh�Dh}qDi  Di�Dj�Dj��Dk  Dk}qDl  Dl��Dm  Dm}qDm��Dn}qDo  Do}qDp  Dp� Dq  Dq��Dq�qDr� Ds  Ds}qDt  Dt� Du  Du��Du�qDv}qDv�qDw}qDx�Dx� Dx�qDy��DzDz�D{  D{� D{�qD|}qD}�D}��D~  D~��DD��D�qD�@ D�~�D�� D�  D�AHD�� D���D�HD�AHD���D�� D�  D�AHD�~�D���D���D�>�D�~�D�� D�  D�@ D�� D���D�  D�@ D�� D�D�  D�=qD��HD��HD�  D�AHD��HD��HD�HD�@ D�� D�D�HD�@ D�~�D���D�  D�>�D�}qD��qD���D�@ D�� D���D��qD�@ D���D�D�  D�>�D�� D�� D�HD�AHD�~�D�� D�HD�@ D�� D�� D�HD�AHD��HD�� D�  D�@ D�� D�� D�  D�@ D�� D��HD�  D�>�D�}qD���D�  D�AHD���D�� D��qD�@ D��HD���D���D�@ D�� D�� D�  D�AHD�� D��qD�  D�@ D�� D���D�  D�AHD�� D�� D�HD�AHD�� D�� D�  D�>�D�� D�� D�  D�AHD�� D��qD���D�>�D�~�D�� D���D�@ D�� D���D�  D�AHD�� D��HD�HD�@ D�~�D��qD�  D�AHD��HD�� D�  D�@ D�~�D���D��qD�<)D�~�D�� D���D�=qD�� D�D��D�B�D��HD�� D�  D�>�D�~�D���D�HD�C�D���D�D��D�AHD��HD�� D�HD�AHD���D�D�HD�>�D�~�D���D�  D�@ D�� D��HD�HD�>�D�}qD��qD��qD�=qD�~�D���D�  D�AHD�� D��HD�  D�>�D�� D���D���D�>�D�� D�� D�  D�@ D��HD���D�HD�B�D��HD��qD���D�>�D�~�D���D���D�@ D�� D�� D�HD�AHD�~�D�� D�  D�>�D�� D�� D�  D�@ D��HD��HD��D�B�D��HD��HD�  D�@ D�~�D���D���D�>�D�~�D���D�  D�@ D�� D���D��qD�>�D�� D��HD�  D�>�D�~�D���D�  D�AHD��HD�� D���D�@ D��HD�� D���D�@ D�� D�� D���D�=qD D��HD�  D�@ DÀ D��HD�  D�@ DĀ D�� D�  D�@ D�~�D��HD�HD�@ Dƀ D��HD�HD�@ Dǀ D�� D���D�@ DȁHD�� D�HD�@ Dɀ D�� D���D�@ Dʀ D��HD�  D�>�D�}qD�� D�HD�>�D̀ D��HD�HD�AHD͂�D�� D���D�AHD�~�D�� D�  D�@ Dπ D�� D���D�AHDЀ D�� D�  D�>�D�~�D�� D���D�>�D�~�DҾ�D�  D�AHDӁHD��HD�HD�@ DԀ DԾ�D�  D�B�DՁHD��HD�  D�AHDր D�� D���D�=qD�~�D�� D�  D�@ D؀ Dؾ�D�  D�@ Dـ D��HD�  D�>�Dڀ D�� D�  D�>�D�~�D۾�D��qD�=qD�}qDܽqD���D�>�D�~�Dݾ�D���D�>�D�~�D޾�D���D�>�D߀ D�� D���D�>�D�~�DྸD���D�@ D� D�� D�HD�B�D�HD�� D��D�AHD� D��HD��D�AHD� D侸D��qD�>�D�~�D��HD���D�=qD� D��HD�HD�@ D�HD��HD�HD�B�D�HD�� D�  D�@ D�HD��HD�  D�B�D�HD�� D���D�>�D�~�D�� D�  D�@ D� D�� D�  D�@ D�~�D���D���D�>�DD�D�  D�@ D�HD�� D�HD�B�D��HD�� D�  D�@ D�~�D�D�  D�AHD�~�D�D�  D�@ D�~�D�D���D�>�D�~�D���D���D�@ D��HD��HD�  D�AHD�� D�� D���D�=qD��HD�� D�  D�@ D�� D��HD�HD�@ D��HD�� D�  D�,�G�O�?L��?aG�?�=q?�{?Ǯ?�G�@�\@��@+�@8Q�@J=q@aG�@s33@��
@���@�z�@�G�@��@���@���@��
@У�@�Q�@�  @�@�z�@��RA�
AQ�A{A�
AQ�Ap�A"�\A(Q�A.{A1�A6ffA:�HA@��AEAJ=qAO\)ATz�AZ=qA_\)Ac�
AhQ�Amp�As33Aw�A|(�A���A�33A�A�Q�A��\A�p�A�  A��\A�z�A�\)A�=qA���A�
=A���A�z�A��A��A�(�A�
=A��A���A�
=A��A���A��A��A�z�A�
=A��A�(�AθRAљ�A�(�A�ffA���A�33A�{A��A��HA�p�A�  A��HA�p�A�A�=qA�p�A��A��A���A��B�B�\B�
B��B=qB�
B��B
=qB�B�BffB�B��B�\B�
B��B=qB�B��BffB�B��B{B�B ��B!B#33B$z�B%B'
=B((�B)p�B*�HB,  B-�B.=qB/�B1�B2=qB3\)B4Q�B5B7
=B8(�B9G�B:�\B;�
B=G�B>=qB?\)B@��BB{BC\)BDQ�BE��BF�HBH(�BIG�BJffBK�BL��BN=qBO\)BPz�BQBS33BTQ�BUp�BV�\BW�
BX��BY�B[
=B\(�B]p�B^�RB_�B`��BaBc
=Bd(�BeG�Bf{Bg
=Bg�
Bh��Bj=qBk33Bl(�Bm�Bn{Bo33Bpz�BqBs
=Bt  Bup�Bv�HBxQ�By�B{\)B|��B}�B\)B�ffB�33B��B��\B�33B��
B�z�B���B���B�(�B���B�
=B�\)B��B��B�(�B�ffB���B��HB��B�\)B��B�B��B�(�B�Q�B��\B���B�
=B�G�B��B�B�  B�=qB�ffB���B��HB�
=B�G�B�p�B��B�  B�=qB�z�B��RB���B�33B�\)B��B�B��B�(�B�Q�B��\B���B�
=B�G�B��B�B�  B�(�B�Q�B�z�B��RB���B�33B�p�B��B�  B�=qB�z�B��RB���B�33B�p�B��B��B�(�B�ffB���B���B�33B��B��
B�  B�Q�B��RB���B�G�B���B��B�=qB�z�B��RB�
=B�G�B���B��B�=qB��\B��HB�33B��B��B�(�B��\B���B�G�B��B�  B�Q�B���B�
=B�\)B��B�  B�Q�B���B���B�G�B��B�  B�ffB��RB��B�p�B��
B�=qB��\B��HB�G�B��B�  B�ffB���B�33B��B��B�=qB��\B���B�G�B���B�  B�Q�B���B�
=B�\)B�B�{B�ffB���B��B��B��
B�(�B�z�B��HB�G�B���B��B�=qB���B���B�\)B��B��B�Q�B���B���B�G�B���B��
B�(�B�z�B���B��B�\)B�B�  B�Q�B���B���B�33B��B��
B�(�B�ffB���B��B�p�B�B�{B�ffB��RB�
=B�\)B�B�  B�Q�B���B�
=B�\)B��B�  B�ffB��RB�
=B�p�B�B�(�B�z�B���B�33BÅB��B�Q�Bģ�B�
=B�p�B��
B�=qBƣ�B��BǅB��B�Q�BȸRB�33Bə�B�  B�ffBʸRB��B˅B��B�Q�B̸RB��BͅB�  B�ffB��HB�\)B�B�=qBУ�B��BхB�  B�ffB��HB�G�B�B�=qBԣ�B�33BծB�=qBָRB�33B�B�=qBظRB�G�B�B�=qBڸRB�33B�B�=qBܸRB�33BݮB�=qB���B�G�B��
B�ffB�
=B�B�{B��B�33B�B�Q�B���B�G�B��
B�ffB��HB�\)B��B�z�B���B�B�{B��B�33B��
B�ffB��HB�p�B��B�z�B���B�B�{B��B�33B�B�Q�B��HB�p�B�  B�z�B�
=B��B�{B��\B�
=B���B�=qB���B�G�B��
B�Q�B��HB�\)B��
B�Q�B���B�\)B��B�z�B�
=B��C   C =qC z�C �RC ��C33C�C��C
=CG�C�CC  C=qCz�CC
=CG�C�C��C  C=qCz�C�RC��CG�C�CC  C33Cp�C��C�C�CffC�C�C	(�C	ffC	��C	�
C

=C
G�C
�\C
��C
=CG�C�C�RC�C(�C\)C��C�
C�C\)C�\C��C��C33CffC�C�HC(�CffC��C�
C{CG�Cz�C�RC��C33Cp�C�C�HC{CG�C�C��C{CQ�C�CC  C=qC�C�
C�C\)C��C�
C�CffC�RC�C(�Cp�C�RC
=CG�C�CC
=CQ�C��C�C(�CffC��C  C=qC�C��C
=CG�C��C�HC(�Cz�C�RC��C33Cz�CC{C\)C��C�C (�C p�C �RC!
=C!\)C!��C!�
C"�C"ffC"�RC#  C#Q�C#�\C#�
C${C$ffC$�RC%
=C%Q�C%��C%�HC&33C&�C&�
C'{C'\)C'��C(  C(Q�C(��C(�HC)(�C)z�C)�
C*�C*p�C*�RC+  C+Q�C+�C,  C,G�C,�\C,�
C-(�C-�C-�
C.(�C.p�C.C/{C/ffC/C0
=C0\)C0�C0��C1G�C1��C2  C2G�C2�\C2�HC3(�C3z�C3�
C433C4�C4��C5{C5ffC5�RC6
=C6ffC6�RC7
=C7Q�C7��C8  C8Q�C8��C8��C9G�C9��C9��C:Q�C:��C:�C;33C;�\C;��C<G�C<��C<�HC=33C=�\C=�C>=qC>�\C>�HC?=qC?��C?�C@=qC@�\C@�HCA33CA�\CA�CBG�CB��CB�HCC33CC��CC��CDG�CD��CD�CE33CE��CE��CFG�CF�\CF�HCG=qCG��CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�VA�^5A�z�A�v�A�|�A�~�A�~�A�~�AہAہAۃAۃAۃAۃAۃAۅAۃAۅAۅAۅAۇ+Aۉ7AۋDAۋDAۋDAۍPAۍPAۍPAۏ\Aۏ\AۑhAۑhAۑhAۏ\A�z�A֋DAƇ+A�{A��/A��\A�p�A��^A��hA��!A��A���A��A��wA�
=A�S�A�|�A�-A��7A�1A��+A��A�{A��`A�&�A��A�1'A}��A{�Av{AsVAp�\Ap  Aot�An�AmAjȴAhAdI�AaS�A]�
A[�AX�AJ��AE�A>1A:JA;VA<{A<��A=�FA>E�A>bNA=�FA=\)A<ffA:�A9/A9oA8~�A8�A7�wA6n�A5"�A4�DA4A�A41'A4=qA4bA4ffA4E�A3�A3?}A2��A1��A1\)A0v�A0�A/��A.�jA-�TA,�+A*��A)��A(�`A'��A'
=A&M�A%��A%33A%�A$�DA#K�A"�9A"��A#33A#K�A"�/A!�A!�mA!�TA!�PA!l�A!`BA!G�A!+A ȴA M�A 1A�TA��AƨA�PAhsA7LA��AffAA�TA��A��A33A��A�jA�uAr�AI�A�;A�-A|�A�PA�;A�^At�A;dAAȴAv�A1A��AƨA��A�wA��A|�A"�A^5A$�A�A�A�A�A�FA�7A7LAVAjA �A��A�AS�A/A��A��A�AjA��A��A�Ax�A�A��AA��A��AM�A�AC�A�jAA�A�;A�A&�A�AVA��Ar�A^5A �A�TA�A�A�AĜA�DA �A�;A�FAK�A
��A
��A
ĜA
~�A
{A	��A	x�A	dZA	G�A��A(�A��A�hA
=A��AZA-A�A��A��AXA33A��A�!A5?A�PAO�AĜA1AAp�AC�A"�A �uA  �@��w@�l�@��@��@��\@���@��F@�o@���@�@�/@��9@�z�@�Z@��@��@��H@�5?@���@�V@�1'@�@�C�@�"�@���@�$�@�hs@�&�@�Ĝ@��
@�\@�@�9@�  @땁@�t�@��@�^@�D@�S�@��@�!@�$�@��@�j@�j@��m@�o@�\@��@�7@�&�@�@��@߾w@�S�@އ+@�J@ݺ^@�p�@�?}@�%@ܬ@���@ۅ@�l�@�K�@���@�O�@�Z@ם�@�+@��y@ְ!@և+@�~�@�n�@�5?@���@�@�G�@Ԭ@�A�@��@��@�;d@�-@ѡ�@��@�%@���@��@�9X@�l�@�K�@��@���@�n�@͡�@̣�@˥�@��y@ʧ�@�^5@��@�7L@��@�dZ@���@�E�@���@�X@ļj@ă@�1@�ƨ@öF@Ý�@ÍP@Å@�t�@\@�{@��@���@��@���@�l�@��+@�E�@��^@�?}@�/@��/@�j@��;@�33@��H@��!@�ff@�5?@���@�x�@��@��@�I�@��@��;@�\)@��@��R@�v�@���@�%@��/@��j@��@�|�@��+@��#@���@��^@��9@�(�@��P@�E�@��#@���@��-@�hs@�Q�@�|�@�;d@�ȴ@��+@�n�@���@���@��7@��7@��7@��7@�/@��/@�r�@��@��;@�|�@�
=@���@�M�@��@��-@��@�X@���@�z�@�1@���@�K�@�@�ȴ@��!@�~�@���@�%@�bN@�1@��
@��P@�C�@��@��y@���@�M�@��@��h@�X@��@���@��@�j@��
@�;d@���@���@���@��+@�$�@��T@�`B@��`@�z�@�I�@��@��
@���@�|�@�+@���@���@�ff@�M�@���@�x�@�G�@�V@��@�r�@��;@�;d@��@�@��\@�@��@���@��/@��@�j@�b@��
@���@�K�@�ȴ@��+@�=q@���@�X@�/@���@���@��9@�r�@�A�@�(�@�  @��;@�ƨ@��@�C�@�
=@���@��R@��!@���@�n�@�E�@��T@���@�X@�G�@�/@��/@��j@���@�z�@�Q�@�(�@���@�l�@�33@�+@�o@��y@���@�ff@�=q@��@��#@���@�X@��@�%@��D@�b@��@��@�S�@�;d@�
=@���@��+@�~�@�^5@�-@��@���@�p�@�/@�r�@�@K�@;d@+@~ȴ@~E�@}��@}V@|I�@{ƨ@{o@z�@z�H@z��@z��@z^5@y��@y��@yhs@x�@xA�@x  @w;d@vv�@v@u�T@u@u�-@u@u�-@up�@uV@t�D@tZ@t�@s�m@s�@r�@r^5@rJ@q��@qx�@qX@q�@p��@p�@pQ�@p  @o�@o��@o|�@o+@n��@n$�@m�T@m�h@l��@l��@lI�@k�@j�!@jM�@jJ@ix�@hĜ@h�@h �@g�@f�R@f�+@e�@e��@e��@e�h@d��@c�m@cC�@bM�@a�#@a��@ahs@`�`@`Q�@_�@_�@_��@_��@_l�@^�y@^V@^E�@^{@]?}@]V@\��@\�@\j@\(�@[ƨ@[t�@[dZ@[o@Zn�@Y�@Y�#@Y��@X��@Xr�@X1'@Xb@W��@W�P@WK�@W
=@Vv�@U@U�h@U/@T�D@T(�@S�
@S�@S@R�\@Q��@Q�#@Qhs@Q�@P�u@O��@O�P@Ol�@N��@Nv�@N5?@M�h@M/@L�j@L1@K�m@K��@Kt�@K33@J��@J^5@JJ@I��@I�7@IX@H��@H�9@HbN@HA�@G�@G�@Fȴ@FE�@E�@E��@E��@E�-@E`B@D�/@Dz�@D(�@C��@Cƨ@C��@C�@CdZ@C@B��@BM�@A�#@Ahs@A&�@@�`@@Ĝ@@�9@@��@@1'@?�;@?��@?l�@>��@>V@=�@=�h@=/@<�@<I�@<1@;�F@;o@:~�@:-@9�@9��@9hs@9X@9%@8��@8bN@8 �@7�w@6�R@6�+@6V@65?@6@5p�@4��@4�D@4Z@49X@41@3��@3�m@3�
@3�F@3��@3dZ@3"�@2�@2~�@2=q@2�@2�@1�#@1�7@17L@1�@1%@1%@0�`@0�@01'@0 �@/�w@/K�@/�@.�@.�+@.E�@-��@-?}@,��@,��@,�@,�D@,j@,9X@,�@+�@+o@*~�@*�@*-@*�@*�@*J@*J@)�#@)��@)X@(�`@(��@(Q�@(  @'�w@'l�@&�R@&ff@&E�@&@%@%��@%`B@$�/@$��@$Z@$9X@#�m@#ƨ@#t�@#@"��@"�\@"n�@"^5@"=q@"J@!��@!��@!��@!x�@!hs@!7L@ �`@ Ĝ@ r�@ 1'@�;@�P@+@ff@$�@�@�h@?}@V@�/@�@Z@�
@�F@��@��@��@��@��@�@C�@n�@hs@%@�9@�u@bN@b@�w@�P@K�@�@�@��@��@v�@ff@�@��@�-@��@�@?}@�@��@�j@j@��@�
@�
@�
@ƨ@�
@�
@ƨ@�F@dZ@C�@"�@�!@=q@�#@�7@hs@G�@7L@&�@%@�`@Q�@ �@�;@�@�P@;d@��@��@�+@v�@v�@ff@ff@V@E�@$�@�T@�-@O�@O�@�@V@V@�@��@�j@j@�@1@1@�m@�
@ƨ@��@��@�@dZ@dZA�M�A�M�A�XA�`BA�VA�VA�\)A�hsA�z�A�v�A�~�A�v�A�r�A�x�A�x�A�|�A�~�A�|�A�|�A�|�A�~�AہA�~�A�|�A�~�AہAہA�|�A�|�AۃAۅAہA�~�A�~�AۅAۅAہAہAۃAۇ+AۃA�~�AہAۅAۅAۃAہAۃAۇ+AۃAہAہAۃAۅAۅA�~�A�~�AۅAۅAہAۃAۅAۇ+Aۇ+AۃAۅAۉ7Aۇ+AۃAہAۅAۅAہAہAۅAۇ+AۃAہAۅAۇ+AۅAۃAۇ+Aۉ7Aۇ+AۃAۅAۅAۉ7Aۇ+AۃAۅAۉ7Aۇ+AۅAۅAۉ7Aۉ7AۅAۅAۉ7AۋDAۋDAۇ+Aۇ+Aۉ7AۋDAۋDAۇ+AۋDAۏ\AۍPAۉ7Aۇ+AۋDAۍPAۋDAۇ+AۋDAۍPAۍPAۉ7Aۉ7AۍPAۏ\AۋDAۉ7AۍPAۍPAۏ\Aۏ\AۋDAۉ7Aۏ\AۑhAۍPAۋDAۍPAۏ\AۍPAۋDAۋDAۏ\Aۏ\Aۏ\AۍPAۋDAۏ\AۑhAۍPAۋDAۍPAۑhAۑhAۏ\AۍPAۑhAۓuAۑhAۏ\Aۏ\AۑhAۓuAۑhAۏ\Aۏ\AۓuAۓuAۑhAۏ\AۑhAە�AۑhAۏ\Aۏ\AۓuAۓuAۏ\AۋDAۍPAۏ\Aۏ\Aۉ7A�/Aڙ�A�p�A�M�A�K�A�/A���A�E�A�ȴA�ĜA�$�A՝�A�oA��A� �A�G�Aǩ�A�ȴA��A�A�9XA��A��A�JA�I�A�/A���A�`BA�l�A��mA���A�l�A�G�A�33A�1A��`A���A���A��DA�~�A�p�A�VA�M�A�C�A�-A��A�1A��A��A�ƨA���A�I�A��A���A�/A�1A���A���A�ȴA�ĜA���A�hsA�XA�33A�+A�oA�ƨA��+A�ZA�/A�A��HA�ȴA��A��\A�t�A�K�A� �A��A�A��A��#A�ȴA��A��7A�~�A�ffA�5?A�  A���A�`BA�G�A�A�A�9XA�(�A�bA��A��`A��
A��-A���A��A�`BA� �A���A��A��-A�x�A�O�A�E�A�33A�"�A�A���A�ƨA���A�^5A���A���A���A��A�x�A�hsA�K�A�VA��mA�ƨA��hA��A���A���A��wA��FA��-A��!A��A���A��uA��DA�z�A�z�A�p�A�^5A��A��#A�ƨA��7A�K�A��
A���A�K�A�  A���A��TA��/A���A���A��!A�t�A�I�A�-A�VA��A���A��wA��hA�S�A��A�ȴA�r�A���A��A� �A�ĜA�hsA�oA��!A�33A��`A��wA���A��+A�dZA�9XA�bA��A��/A��jA���A��DA�jA�XA�I�A�5?A� �A�bA�
=A�%A�A�  A���A���A�t�A��A��FA�ffA�E�A�-A�A��A��A���A�+A��7A��A�ƨA�
=A��9A��PA�p�A�;dA�bA��A���A�K�A��A��DA�G�A��A�hsA���A��;A���A�\)A�VA���A��A�hsA�r�A�hsA�\)A�I�A�=qA���A���A�dZA���A�ffA�ȴA�dZA�t�A���A�E�A�A��A���A��jA�z�A�JA���A���A�r�A�\)A�E�A�{A��A���A�S�A� �A���A�JA�jA�$�A���A�dZA�A�A�/A�$�A�  A��-A�p�A�A�A��A��A�A��TA���A�ĜA���A���A��\A�r�A�VA�G�A�&�A�ȴA�?}A�p�A~�A}x�A}�A|��A|�A|�/A|��A|�jA|�uA|  A{K�AzA�Ax��Aw��Aw�Aw�AvQ�Au�^AudZAuC�Au"�At��AtM�As�
AsC�Ar��ArA�Aq��Aql�Aq�Ap�!Ap~�ApffApVApQ�ApI�Ap=qAp-ApAo�mAo�Ao�Ao�#Ao�^Ao��Ao�hAox�AohsAoO�Ao;dAo?}Ao33Ao"�AooAn��An�/An�jAnbNAn1Am�^Aml�Am\)AmO�Am%Al��Al9XAk�Ak�FAk\)Ak�Aj�DAjjAj^5Aj=qAi�AiXAh�`Ahr�Ah�AgƨAgXAf�AfffAfJAe�
AeO�Ad5?Ac\)Ac%Ab�HAb�jAbbNAb-Ab �Aa�AaS�Aa?}A`�A_dZA_?}A_�A^��A^5?A]��A]K�A\�9A\z�A\I�A\ �A[��A[�TA[�wA[��A[hsAZ��AZE�AZE�AZ=qAZ-AZ{AY�#AYt�AX5?AV�AT�APbAL5?AK�;AK|�AKG�AJ~�AG�AG�AF�/AF�RAF�uAFbAEl�AE�AD-AC��AC\)AB��A@~�A?t�A>^5A=�hA<�+A;��A;?}A:^5A9�A9�A9�A9��A:bA:�A:Q�A:~�A:�\A:��A;�A;l�A;�A;�PA;��A;�FA;��A;�A<1'A<^5A<r�A<��A<�A<�!A<�`A<��A=�A=;dA=?}A=C�A=O�A=��A=A=�
A=�A>A>�A>(�A>5?A>M�A>ZA>ZA>VA>VA>^5A>jA>�A>�A>bNA>=qA>bA=�#A=�wA=�FA=�-A=��A=x�A=hsA=`BA=dZA=`BA=K�A=XA=\)A=XA=G�A<�!A<ZA;�;A;�-A;�hA;;dA:��A:^5A9��A9`BA9�PA9VA9oA9+A9G�A9/A9/A9?}A97LA9;dA9+A9�A8�A8��A8ĜA8��A8�\A8z�A8jA8VA89XA8$�A8 �A8 �A8 �A8�A81A7�mA7��A7�wA7�A7��A7��A7�7A7VA6�9A6(�A5��A5A5�A5|�A5K�A5%A4�A4��A4��A4�RA4�A4��A4~�A4Q�A4M�A4VA4ZA4ZA4A�A4 �A4�A4JA41A4-A4Q�A4Q�A4M�A4I�A4E�A4A�A4=qA41'A4-A4{A4bA4JA4bA4bA4JA41A4ZA4jA4�uA4�uA4�A4v�A4ffA4E�A4-A4 �A4�A4�A4A3�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                 A�VA�^5A�z�A�v�A�|�A�~�A�~�A�~�AہAہAۃAۃAۃAۃAۃAۅAۃAۅAۅAۅAۇ+Aۉ7AۋDAۋDAۋDAۍPAۍPAۍPAۏ\Aۏ\AۑhAۑhAۑhAۏ\A�z�A֋DAƇ+A�{A��/A��\A�p�A��^A��hA��!A��A���A��A��wA�
=A�S�A�|�A�-A��7A�1A��+A��A�{A��`A�&�A��A�1'A}��A{�Av{AsVAp�\Ap  Aot�An�AmAjȴAhAdI�AaS�A]�
A[�AX�AJ��AE�A>1A:JA;VA<{A<��A=�FA>E�A>bNA=�FA=\)A<ffA:�A9/A9oA8~�A8�A7�wA6n�A5"�A4�DA4A�A41'A4=qA4bA4ffA4E�A3�A3?}A2��A1��A1\)A0v�A0�A/��A.�jA-�TA,�+A*��A)��A(�`A'��A'
=A&M�A%��A%33A%�A$�DA#K�A"�9A"��A#33A#K�A"�/A!�A!�mA!�TA!�PA!l�A!`BA!G�A!+A ȴA M�A 1A�TA��AƨA�PAhsA7LA��AffAA�TA��A��A33A��A�jA�uAr�AI�A�;A�-A|�A�PA�;A�^At�A;dAAȴAv�A1A��AƨA��A�wA��A|�A"�A^5A$�A�A�A�A�A�FA�7A7LAVAjA �A��A�AS�A/A��A��A�AjA��A��A�Ax�A�A��AA��A��AM�A�AC�A�jAA�A�;A�A&�A�AVA��Ar�A^5A �A�TA�A�A�AĜA�DA �A�;A�FAK�A
��A
��A
ĜA
~�A
{A	��A	x�A	dZA	G�A��A(�A��A�hA
=A��AZA-A�A��A��AXA33A��A�!A5?A�PAO�AĜA1AAp�AC�A"�A �uA  �@��w@�l�@��@��@��\@���@��F@�o@���@�@�/@��9@�z�@�Z@��@��@��H@�5?@���@�V@�1'@�@�C�@�"�@���@�$�@�hs@�&�@�Ĝ@��
@�\@�@�9@�  @땁@�t�@��@�^@�D@�S�@��@�!@�$�@��@�j@�j@��m@�o@�\@��@�7@�&�@�@��@߾w@�S�@އ+@�J@ݺ^@�p�@�?}@�%@ܬ@���@ۅ@�l�@�K�@���@�O�@�Z@ם�@�+@��y@ְ!@և+@�~�@�n�@�5?@���@�@�G�@Ԭ@�A�@��@��@�;d@�-@ѡ�@��@�%@���@��@�9X@�l�@�K�@��@���@�n�@͡�@̣�@˥�@��y@ʧ�@�^5@��@�7L@��@�dZ@���@�E�@���@�X@ļj@ă@�1@�ƨ@öF@Ý�@ÍP@Å@�t�@\@�{@��@���@��@���@�l�@��+@�E�@��^@�?}@�/@��/@�j@��;@�33@��H@��!@�ff@�5?@���@�x�@��@��@�I�@��@��;@�\)@��@��R@�v�@���@�%@��/@��j@��@�|�@��+@��#@���@��^@��9@�(�@��P@�E�@��#@���@��-@�hs@�Q�@�|�@�;d@�ȴ@��+@�n�@���@���@��7@��7@��7@��7@�/@��/@�r�@��@��;@�|�@�
=@���@�M�@��@��-@��@�X@���@�z�@�1@���@�K�@�@�ȴ@��!@�~�@���@�%@�bN@�1@��
@��P@�C�@��@��y@���@�M�@��@��h@�X@��@���@��@�j@��
@�;d@���@���@���@��+@�$�@��T@�`B@��`@�z�@�I�@��@��
@���@�|�@�+@���@���@�ff@�M�@���@�x�@�G�@�V@��@�r�@��;@�;d@��@�@��\@�@��@���@��/@��@�j@�b@��
@���@�K�@�ȴ@��+@�=q@���@�X@�/@���@���@��9@�r�@�A�@�(�@�  @��;@�ƨ@��@�C�@�
=@���@��R@��!@���@�n�@�E�@��T@���@�X@�G�@�/@��/@��j@���@�z�@�Q�@�(�@���@�l�@�33@�+@�o@��y@���@�ff@�=q@��@��#@���@�X@��@�%@��D@�b@��@��@�S�@�;d@�
=@���@��+@�~�@�^5@�-@��@���@�p�@�/@�r�@�@K�@;d@+@~ȴ@~E�@}��@}V@|I�@{ƨ@{o@z�@z�H@z��@z��@z^5@y��@y��@yhs@x�@xA�@x  @w;d@vv�@v@u�T@u@u�-@u@u�-@up�@uV@t�D@tZ@t�@s�m@s�@r�@r^5@rJ@q��@qx�@qX@q�@p��@p�@pQ�@p  @o�@o��@o|�@o+@n��@n$�@m�T@m�h@l��@l��@lI�@k�@j�!@jM�@jJ@ix�@hĜ@h�@h �@g�@f�R@f�+@e�@e��@e��@e�h@d��@c�m@cC�@bM�@a�#@a��@ahs@`�`@`Q�@_�@_�@_��@_��@_l�@^�y@^V@^E�@^{@]?}@]V@\��@\�@\j@\(�@[ƨ@[t�@[dZ@[o@Zn�@Y�@Y�#@Y��@X��@Xr�@X1'@Xb@W��@W�P@WK�@W
=@Vv�@U@U�h@U/@T�D@T(�@S�
@S�@S@R�\@Q��@Q�#@Qhs@Q�@P�u@O��@O�P@Ol�@N��@Nv�@N5?@M�h@M/@L�j@L1@K�m@K��@Kt�@K33@J��@J^5@JJ@I��@I�7@IX@H��@H�9@HbN@HA�@G�@G�@Fȴ@FE�@E�@E��@E��@E�-@E`B@D�/@Dz�@D(�@C��@Cƨ@C��@C�@CdZ@C@B��@BM�@A�#@Ahs@A&�@@�`@@Ĝ@@�9@@��@@1'@?�;@?��@?l�@>��@>V@=�@=�h@=/@<�@<I�@<1@;�F@;o@:~�@:-@9�@9��@9hs@9X@9%@8��@8bN@8 �@7�w@6�R@6�+@6V@65?@6@5p�@4��@4�D@4Z@49X@41@3��@3�m@3�
@3�F@3��@3dZ@3"�@2�@2~�@2=q@2�@2�@1�#@1�7@17L@1�@1%@1%@0�`@0�@01'@0 �@/�w@/K�@/�@.�@.�+@.E�@-��@-?}@,��@,��@,�@,�D@,j@,9X@,�@+�@+o@*~�@*�@*-@*�@*�@*J@*J@)�#@)��@)X@(�`@(��@(Q�@(  @'�w@'l�@&�R@&ff@&E�@&@%@%��@%`B@$�/@$��@$Z@$9X@#�m@#ƨ@#t�@#@"��@"�\@"n�@"^5@"=q@"J@!��@!��@!��@!x�@!hs@!7L@ �`@ Ĝ@ r�@ 1'@�;@�P@+@ff@$�@�@�h@?}@V@�/@�@Z@�
@�F@��@��@��@��@��@�@C�@n�@hs@%@�9@�u@bN@b@�w@�P@K�@�@�@��@��@v�@ff@�@��@�-@��@�@?}@�@��@�j@j@��@�
@�
@�
@ƨ@�
@�
@ƨ@�F@dZ@C�@"�@�!@=q@�#@�7@hs@G�@7L@&�@%@�`@Q�@ �@�;@�@�P@;d@��@��@�+@v�@v�@ff@ff@V@E�@$�@�T@�-@O�@O�@�@V@V@�@��@�j@j@�@1@1@�m@�
@ƨ@��@��@�@dZG�O�A�M�A�M�A�XA�`BA�VA�VA�\)A�hsA�z�A�v�A�~�A�v�A�r�A�x�A�x�A�|�A�~�A�|�A�|�A�|�A�~�AہA�~�A�|�A�~�AہAہA�|�A�|�AۃAۅAہA�~�A�~�AۅAۅAہAہAۃAۇ+AۃA�~�AہAۅAۅAۃAہAۃAۇ+AۃAہAہAۃAۅAۅA�~�A�~�AۅAۅAہAۃAۅAۇ+Aۇ+AۃAۅAۉ7Aۇ+AۃAہAۅAۅAہAہAۅAۇ+AۃAہAۅAۇ+AۅAۃAۇ+Aۉ7Aۇ+AۃAۅAۅAۉ7Aۇ+AۃAۅAۉ7Aۇ+AۅAۅAۉ7Aۉ7AۅAۅAۉ7AۋDAۋDAۇ+Aۇ+Aۉ7AۋDAۋDAۇ+AۋDAۏ\AۍPAۉ7Aۇ+AۋDAۍPAۋDAۇ+AۋDAۍPAۍPAۉ7Aۉ7AۍPAۏ\AۋDAۉ7AۍPAۍPAۏ\Aۏ\AۋDAۉ7Aۏ\AۑhAۍPAۋDAۍPAۏ\AۍPAۋDAۋDAۏ\Aۏ\Aۏ\AۍPAۋDAۏ\AۑhAۍPAۋDAۍPAۑhAۑhAۏ\AۍPAۑhAۓuAۑhAۏ\Aۏ\AۑhAۓuAۑhAۏ\Aۏ\AۓuAۓuAۑhAۏ\AۑhAە�AۑhAۏ\Aۏ\AۓuAۓuAۏ\AۋDAۍPAۏ\Aۏ\Aۉ7A�/Aڙ�A�p�A�M�A�K�A�/A���A�E�A�ȴA�ĜA�$�A՝�A�oA��A� �A�G�Aǩ�A�ȴA��A�A�9XA��A��A�JA�I�A�/A���A�`BA�l�A��mA���A�l�A�G�A�33A�1A��`A���A���A��DA�~�A�p�A�VA�M�A�C�A�-A��A�1A��A��A�ƨA���A�I�A��A���A�/A�1A���A���A�ȴA�ĜA���A�hsA�XA�33A�+A�oA�ƨA��+A�ZA�/A�A��HA�ȴA��A��\A�t�A�K�A� �A��A�A��A��#A�ȴA��A��7A�~�A�ffA�5?A�  A���A�`BA�G�A�A�A�9XA�(�A�bA��A��`A��
A��-A���A��A�`BA� �A���A��A��-A�x�A�O�A�E�A�33A�"�A�A���A�ƨA���A�^5A���A���A���A��A�x�A�hsA�K�A�VA��mA�ƨA��hA��A���A���A��wA��FA��-A��!A��A���A��uA��DA�z�A�z�A�p�A�^5A��A��#A�ƨA��7A�K�A��
A���A�K�A�  A���A��TA��/A���A���A��!A�t�A�I�A�-A�VA��A���A��wA��hA�S�A��A�ȴA�r�A���A��A� �A�ĜA�hsA�oA��!A�33A��`A��wA���A��+A�dZA�9XA�bA��A��/A��jA���A��DA�jA�XA�I�A�5?A� �A�bA�
=A�%A�A�  A���A���A�t�A��A��FA�ffA�E�A�-A�A��A��A���A�+A��7A��A�ƨA�
=A��9A��PA�p�A�;dA�bA��A���A�K�A��A��DA�G�A��A�hsA���A��;A���A�\)A�VA���A��A�hsA�r�A�hsA�\)A�I�A�=qA���A���A�dZA���A�ffA�ȴA�dZA�t�A���A�E�A�A��A���A��jA�z�A�JA���A���A�r�A�\)A�E�A�{A��A���A�S�A� �A���A�JA�jA�$�A���A�dZA�A�A�/A�$�A�  A��-A�p�A�A�A��A��A�A��TA���A�ĜA���A���A��\A�r�A�VA�G�A�&�A�ȴA�?}A�p�A~�A}x�A}�A|��A|�A|�/A|��A|�jA|�uA|  A{K�AzA�Ax��Aw��Aw�Aw�AvQ�Au�^AudZAuC�Au"�At��AtM�As�
AsC�Ar��ArA�Aq��Aql�Aq�Ap�!Ap~�ApffApVApQ�ApI�Ap=qAp-ApAo�mAo�Ao�Ao�#Ao�^Ao��Ao�hAox�AohsAoO�Ao;dAo?}Ao33Ao"�AooAn��An�/An�jAnbNAn1Am�^Aml�Am\)AmO�Am%Al��Al9XAk�Ak�FAk\)Ak�Aj�DAjjAj^5Aj=qAi�AiXAh�`Ahr�Ah�AgƨAgXAf�AfffAfJAe�
AeO�Ad5?Ac\)Ac%Ab�HAb�jAbbNAb-Ab �Aa�AaS�Aa?}A`�A_dZA_?}A_�A^��A^5?A]��A]K�A\�9A\z�A\I�A\ �A[��A[�TA[�wA[��A[hsAZ��AZE�AZE�AZ=qAZ-AZ{AY�#AYt�AX5?AV�AT�APbAL5?AK�;AK|�AKG�AJ~�AG�AG�AF�/AF�RAF�uAFbAEl�AE�AD-AC��AC\)AB��A@~�A?t�A>^5A=�hA<�+A;��A;?}A:^5A9�A9�A9�A9��A:bA:�A:Q�A:~�A:�\A:��A;�A;l�A;�A;�PA;��A;�FA;��A;�A<1'A<^5A<r�A<��A<�A<�!A<�`A<��A=�A=;dA=?}A=C�A=O�A=��A=A=�
A=�A>A>�A>(�A>5?A>M�A>ZA>ZA>VA>VA>^5A>jA>�A>�A>bNA>=qA>bA=�#A=�wA=�FA=�-A=��A=x�A=hsA=`BA=dZA=`BA=K�A=XA=\)A=XA=G�A<�!A<ZA;�;A;�-A;�hA;;dA:��A:^5A9��A9`BA9�PA9VA9oA9+A9G�A9/A9/A9?}A97LA9;dA9+A9�A8�A8��A8ĜA8��A8�\A8z�A8jA8VA89XA8$�A8 �A8 �A8 �A8�A81A7�mA7��A7�wA7�A7��A7��A7�7A7VA6�9A6(�A5��A5A5�A5|�A5K�A5%A4�A4��A4��A4�RA4�A4��A4~�A4Q�A4M�A4VA4ZA4ZA4A�A4 �A4�A4JA41A4-A4Q�A4Q�A4M�A4I�A4E�A4A�A4=qA41'A4-A4{A4bA4JA4bA4bA4JA41A4ZA4jA4�uA4�uA4�A4v�A4ffA4E�A4-A4 �A4�A4�A4A3�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                 ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B� B�B� B� B�4B� B� B�4B�4B�4B�iB�4B�iB�4B�4B�iB�4B�4B�iB�4B�4B� B�4B� B� B�B�B~�B~�B}VB{BxlBrB�B�zB
C�B
�B
�ZBDB+�BM�BZBbNBm]BuZBy�Bn�BZ�B5?B�B �B
ȴB
��B
qvB
iB
-�B
(B
  B
-�B
-�B
B
�B	�KB	ںB	��B	��B	�'B	��B	��B	��B	�\B	�\B	|�B	c�B	I�B	1[B	AB� B	�B	~B	=qB	qB	�=B	��B	�B
B
#�B
6�B
B�B
?�B
C-B
d�B
kQB
k�B
o B
��B
�_B
��B
��B
�*B
��B
��B
�B
�,B
��B
�2B
�
B
�
B
��B
�mB
��B
՛B
�B
��B
��B
��B
�eB
��B
�:B
�JB
��B
��B
��B
��B
�SB
�oB
|�B
�AB
�1B
�~B
��B
��B
��B
�VB
��B
��B
�B
��B
�B
�PB
��B
��B
�=B
�B
��B
�.B
�(B
��B
��B
�\B
�JB
��B
��B
�B
�rB
��B
�"B
�B
��B
�PB
��B
�~B
��B
�DB
��B
��B
��B
�uB
�hB
�VB
�B
��B
�B
�B
�B
�PB
�JB
�~B
��B
��B
��B
��B
�%B
�YB
��B
�rB
��B
�PB
�PB
�~B
�B
��B
�YB
�{B
��B
��B
��B
cB
~�B
~]B
zxB
y�B
y�B
zxB
�;B
.B
~�B
�B
�{B
{�B
zxB
y�B
x8B
s�B
s�B
qvB
p�B
p�B
p�B
n�B
m�B
ncB
m�B
k�B
i�B
g�B
f�B
ffB
e,B
b�B
a�B
aHB
_�B
^�B
^B
^B
\]B
Z�B
YB
YB
W�B
W
B
S�B
P�B
O�B
OvB
OB
PHB
O�B
N�B
N�B
N�B
MB
LdB
MB
K�B
L�B
LdB
J�B
J�B
G�B
EB
CaB
A B
@�B
>�B
=B
:�B
:�B
9�B
9$B
8�B
9�B
7LB
5tB
6B
6�B
7�B
9�B
:^B
;�B
=�B
<jB
;0B
<B
9XB
9$B
7B
4�B
3�B
3�B
4�B
5�B
5B
3�B
3hB
33B
1�B
2-B
/�B
/�B
.IB
-�B
-�B
-B
,=B
+B
(�B
(XB
(XB
(XB
'�B
&�B
%�B
&�B
%FB
%zB
%FB
$�B
%FB
%zB
$@B
%B
$�B
$B
#�B
#�B
#nB
#B
#:B
#:B
"�B
"�B
!�B
!�B
"�B
!bB
!bB
 �B
 'B
 'B
 'B
�B
�B
�B
�B
�B
!B
�B
�B
OB
�B
B
�B
�B
IB
IB
�B
�B
B
�B
CB
CB
�B
B
�B
CB
xB
CB
�B
�B
�B
B
�B
B
CB
CB
�B
CB
qB
=B
�B
=B
=B
=B
	B
�B
�B
B
=B
=B
	B
=B
�B
CB
�B
=B
B
�B
=B
�B
=B
B
�B
qB
qB
	B
	B
�B
=B
=B
qB
qB
=B
=B
�B
�B
�B
qB
B
CB
�B
qB
�B
�B
�B
=B
kB
�B
B
kB
�B
xB
CB
�B
�B
�B
B
OB
�B
�B
OB
�B
�B
�B
�B
 'B
 'B
 'B
!bB
 �B
!�B
"�B
"�B
#:B
#B
#B
"4B
!�B
!�B
!�B
!bB
!�B
"4B
!�B
!bB
!�B
!bB
 �B
 �B
 �B
"4B
"4B
"4B
!�B
!-B
!�B
!�B
!�B
#B
"hB
"�B
#�B
$B
$tB
$�B
$@B
$@B
$tB
%zB
%�B
%�B
&LB
%�B
%�B
&LB
&B
'RB
'B
'RB
'RB
'B
'�B
'�B
'�B
)*B
)�B
)�B
*0B
)�B
*�B
+6B
*�B
+kB
,B
+�B
-�B
-�B
-�B
-wB
/B
/�B
0�B
1'B
2aB
2�B
1�B
2�B
2�B
2-B
2-B
1�B
1�B
2-B
2aB
2�B
2-B
2�B
2�B
3�B
4�B
4�B
5B
5tB
5?B
5tB
5?B
5?B
5?B
7B
7B
6�B
7B
7B
7�B
8�B
9$B
9XB
9XB
9XB
9�B
8�B
8�B
8�B
8�B
9XB
:�B
;0B
;0B
:�B
;0B
;0B
<6B
<6B
<�B
<jB
=<B
=B
=�B
=�B
=�B
?HB
?}B
@OB
@OB
@�B
@OB
@�B
A�B
AUB
A B
AUB
AUB
A B
A B
@�B
@B
@�B
?�B
@OB
?�B
?�B
@OB
@�B
@�B
A B
A�B
B[B
B�B
B�B
B�B
B�B
B�B
C-B
C�B
CaB
C�B
DgB
DgB
D�B
EmB
FB
FtB
FtB
F�B
F�B
FtB
FtB
F�B
G�B
H�B
H�B
IB
I�B
I�B
IB
JXB
J#B
K)B
K)B
J�B
K^B
K�B
K�B
K�B
L0B
L0B
LdB
L0B
L�B
M6B
M6B
M6B
MjB
NpB
NB
N�B
OBB
PB
O�B
O�B
P�B
P�B
P�B
QB
R�B
R B
RTB
R�B
RTB
R B
R�B
R�B
S�B
S�B
U2B
T�B
T�B
UgB
VB
V�B
V�B
VmB
V�B
V�B
V�B
W�B
W�B
W?B
WsB
X�B
XEB
XEB
XEB
YB
YKB
YB
YKB
X�B
YB
Y�B
ZB
Y�B
ZB
[#B
Z�B
[�B
Z�B
[�B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
]dB
]dB
]�B
^jB
^5B
_B
^�B
^�B
_�B
_�B
`BB
a|B
`vB
`�B
aHB
a�B
aHB
b�B
a�B
c B
c B
b�B
b�B
c�B
c B
c�B
c�B
c�B
dZB
c�B
dZB
dZB
d�B
d�B
d�B
e`B
e,B
e�B
f2B
e�B
e�B
e`B
e�B
f�B
f�B
f�B
f�B
f�B
gB
gmB
g�B
g8B
gmB
g8B
g�B
g�B
g�B
g�B
h>B
h>B
h>B
h>B
h�B
h�B
h�B
iDB
i�B
jKB
jKB
jB
j�B
j�B
j�B
j�B
kB
k�B
k�B
l"B
l�B
l�B
l�B
l�B
lWB
lWB
m]B
l�B
m)B
ncB
n/B
n/B
m�B
n/B
n�B
o B
oiB
o�B
o�B
o�B
o5B
oiB
o5B
o B
oiB
o�B
o5B
o�B
p;B
pB
p�B
pB
p;B
poB
qAB
p�B
qB
poB
qAB
qB
qvB
p�B
q�B
rB
qvB
rB
r�B
r|B
r�B
r�B
sB
r�B
r�B
sMB
sMB
s�B
s�B
s�B
tB
t�B
t�B
s�B
tTB
tB
tTB
tB
t�B
tB
u%B
u�B
u�B
v+B
w2B
w2B
w�B
x�B
w�B
xB
w�B
x8B
xlB
x�B
y	B
x�B
x�B
xlB
x�B
xlB
x�B
y>B
yrB
y>B
y�B
z�B
z�B
z�B
zxB
{JB
z�B
{�B
{�B
{�B
|PB
|�B
|�B
|PB
|�B
}"B
}�B
~�B
~�B
.B
�B
� B
�B
cB
�4B
�iB
�oB
�;B
�oB
�;B
�oB
�;B
�oB
�B
�oB
�B
��B
��B
�B
�B
�uB
��B
��B
�B
�GB
��B
��B
��B
�{B
�MB
��B
��B
��B
��B
��B
�SB
��B
��B
��B
��B
��B
�_B
��B
�_B
�_B
��B
�_B
�+B
��B
��B
��B
�fB
�1B
�7B
��B
�	B
�rB
�rB
��B
��B
��B
��B
�rB
�B
��B
�DB
�DB
�xB
�B
��B
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
�"B
��B
��B
��B
��B
��B
��B
��B
��B
�\B
��B
��B
��B
��B
��B
�bB
��B
��B
��B
�4B
�.B��B�oB~�B�B�oB��B�B~�B� B��B.B�oB�B}�B�B~�B.B�iB��B��BcBcB��B��B� B~�B�B�B��BcB~�B�iB�B�oB.B�B��B�oB� B.B��B�oB�BcB�B�iB�;B�B~�B� B�oB�;B�4BcB.B��B�;B�B�B�;B��BcB.BcB�;B�B�B� B��B��B�B�B�;B�B�BcB�4B�oB�4BcB��B�B�B.B� B�;B�B��B.B� B�oB��B�B�B��B��B.B�B�oB�;B�B~�BcB�B�oB��B�B�B�;B�B.B.B��B�oB� B~�B�4B�;B�4BcB�B��B�;B� B~�B�iB�;B�4B�B~�B~�B�iB�BcB~�B� B��BcB~�BcB� B�iB~�B~(B~�B�B� B~�B}�B~�B�B~�B}�B}�B}�B.B}"B|PB}"B}�B}�B|PB{B|PB|�B|BzDBy�BzxBz�By	BwfBx8BxBw�Bu�Bs�BtBsMBrGBpBm]BlWB~]B�uB��B�oB{�B.B�JB��B��B�LB��B� B�<B��B	v�B
B
B[B
o�B
h
B
d�B
{B
poB
q�B
}VB
��B
��B
�BB
��B
�	B
�]B�BuB�B�B	lB�B~B~B
�B�BPBDBB\B B�BB�B�BqB!�B!�B*�B:*B2-B/OB6FB2�B2�B:^B?HB7LB=B9�B>�BF�BG�BJ#BJXBN<BP}BM6BM�BOBM6BS�BS�BQ�BS�BR�BV9BT,BT�BV�BP�BYBWsBVBgmB\�BZ�BX�BX�BZ�B\�B]dBZQB[�B\�B\�B\�Ba�Be`B`�B`�Ba�Bc�Be�B`BBdZBbNBh�BaHBi�BffBi�B��BoiBl"BjBe,Bf�BjBncBo�Bm]BuZB��Bt�BsMBrGBs�BsBrGBr�BqABtTBr�Bs�Bp�Bs�Bs�BzBx8Bw2BcB�_B�MB}"B�_BsBo Bn�Bk�Bk�Bi�Bq�Bg�BiBd&Bb�B^�B^jBZB]�B[�BY�BVBW?BV9BR BI�BIBC�BC�B;0B>wB0�B-�B'�B'RB(XB(�B%B!B�B�B�BkB�BFBBuB�B�B
�B
=B	7B_B�B
rB	lBMB�B
��B
��B
�B
�DB
�DB�B
�B
�?B
ѷB
��B
�B
��B
��B
�B
��B
�@B
�1B
��B
��B
�hB
�B
��B
��B
��B
�B
��B
��B
�7B
uZB
jKB
q�B
p�B
gmB
qAB
r�B
s�B
r�B
sB
�AB
w2B
u�B
w2B
t�B
n/B
^�B
zxB
\]B
EmB
B�B
<B
4�B
2-B
7�B
7�B
+�B
&�B
!�B
B
�B
IB
�B
�B
�B
�B
�B
~B	��B	�]B
  B	�B	�B	�/B	�B	�VB
*�B
$�B
(�B
(�B
 'B
7�B
2aB
0�B
2�B
2aB
.IB
,�B
/B
+6B
(XB
)�B
2�B
.IB
;�B
<6B
VB
hB
JB
	lB
�B
�B
YB
%B

	B
�B
�B
xB	��B	�B	��B	�B	�WB	�fB	�NB	�B	�B	��B	�B	�|B	��B	�B	ѷB	�RB	�RB	�?B	��B	��B	�B	�}B	��B	��B	��B	�[B	�}B	��B	��B	��B	��B	��B	��B	�gB	��B	ÖB	��B	��B	ŢB	ŢB	��B	�3B	�gB	ĜB	�#B	�zB	�mB	� B	��B	�$B	��B	��B	�B	��B	�hB	��B	��B	�[B	�hB	��B	��B	�LB	��B	��B	�:B	�!B	�B	��B	��B	��B	��B	�bB	��B	��B	�B	��B	��B	cB	�YB	z�B	{JB	|�B	t�B	sMB	�xB	sMB	h>B	h
B	i�B	h>B	b�B	e�B	`vB	U2B	R�B	QNB	JXB	J�B	C�B	I�B	B�B	O�B	?}B	6�B	4�B	3�B	/�B	0�B	0!B	9�B	&�B	0�B	poB	�B�B��B�;B��B	�B�fB��B��BޞB�B��B�B��B��BܒB�B��B	�B	;B��B	uB	  B	B	B	MB	�B		�B	�B	�B	�B	.B	!-B	&�B	,=B	9�B	R�B	U�B	[�B	`BB	aHB	gmB	gB	poB	�4B	� B	��B	�bB	�bB	�B	��B	�bB	�6B	��B	�B	��B	�B	�BB	�wB	�gB	�)B	�BB	уB	��B	��B	��B	�JB	�VB	��B
oB
;B
B
�B
�B
�B
�B
B
B
xB
 \B
*0B
1�B
3�B
4B
3�B
6�B
8�B
8B
8�B
8�B
9�B
FB
E�B
E�B
EmB
D�B
H�B
@�B
M�B
A B
33B
)�B
A B
;�B
@B
B[B
IB
C�B
Y�B
\�B
c�B
d�B
h
B
iDB
iDB
jKB
kQB
k�B
j�B
jKB
l"B
m)B
l�B
l"B
kQB
i�B
j�B
lWB
l�B
m�B
o�B
o B
pB
oiB
r|B
��B
��B
��B
��B
�xB
��B
�oB
�B
�-B
��B
�OB
��B
��B
�!B
��B
�4B
��B
��B
��B
�B
��B
��B
��B
��B
�tB
��B
�OB
�B
�B
��B
�UB
�[B
��B
�UB
�}B
��B
��B
��B
��B
� B
��B
��B
�'B
��B
�^B
�<B
бB
бB
ҽB
�aB
֡B
��B
�aB
��B
��B
֡B
�[G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                 B�AB.B�B�B�B�0B�B�B�-B�$B�4B�gB�5B�gB�+B�=B�^B�4B�3B�]B�)B�)B�B�1B�B�B�B�B~�B~�B}UB{�By-Bz?B��B	eB
f�B
�xBB�B;qBZtBe�Br�By�B{�B�BBzRBp�B@�B#�B#<B
�9B
�^B
�4B
}�B
=B
 �B
�B
3�B
A�B
&lB
5B	�OB	�nB	�#B	�sB	ČB	��B	�$B	�5B	��B	��B	�B	l�B	U�B	]�B	2B��B	-B		�B	:B	nB	��B	�aB	�`B
B
%B
:=B
JB
C0B
DYB
f�B
l�B
m(B
s�B
�CB
�PB
�B
��B
�B
�ZB
��B
ɥB
�[B
�HB
�zB
�KB
�8B
��B
��B
��B
رB
�B
��B
�=B
��B
�.B
�!B
��B
��B
��B
��B
��B
�0B
��B
�[B
{�B
�|B
�B
�%B
��B
��B
��B
�gB
�kB
��B
�{B
�jB
��B
��B
��B
�^B
��B
�WB
��B
��B
��B
�B
�jB
��B
��B
�GB
�XB
��B
�MB
��B
��B
��B
�`B
��B
�.B
�$B
��B
�WB
�#B
��B
��B
�NB
�EB
��B
��B
�{B
�(B
�B
�XB
��B
�B
��B
�>B
��B
��B
�RB
�-B
�hB
��B
�'B
�B
�$B
�mB
��B
��B
�4B
�B
�$B
�1B
��B
� B
�B
��B
4B
z�B
y�B
y�B
zRB
�PB
pB
B
��B
�B
}B
|bB
{cB
y�B
uFB
uB
q�B
qB
q&B
rbB
o2B
nyB
oRB
o.B
m"B
j�B
h<B
g�B
g�B
f#B
ciB
c]B
bWB
`�B
^�B
_B
_�B
^B
[eB
YsB
Y�B
Y�B
Y_B
U5B
Q�B
Q�B
P�B
PtB
P�B
PB
OgB
P<B
O�B
M�B
M8B
N,B
M�B
N�B
MkB
L�B
MB
IB
F+B
DB
A�B
B�B
@EB
=�B
;�B
;]B
9�B
9�B
:�B
=KB
8�B
6oB
7�B
7�B
8mB
9�B
:�B
=BB
>�B
<�B
<tB
= B
:sB
:�B
8B
5MB
3�B
4}B
5�B
6�B
5�B
4�B
5,B
5�B
3�B
3�B
1B
0�B
.�B
.�B
0)B
/?B
.VB
+�B
)/B
)_B
)dB
)�B
(7B
'�B
'eB
'�B
&/B
&�B
&B
%�B
&_B
&(B
%B
&�B
%�B
$�B
$bB
$B
#�B
#�B
$�B
#�B
#
B
"�B
"�B
$�B
$uB
"�B
"5B
!B
 �B
 uB
 ?B
�B
 cB
 1B
 B
�B
 ?B
�B
B
�B
B
�B
�B
�B
�B
bB
$B
B
�B
,B
�B
�B
�B
�B
�B
B
�B
�B
vB
�B
+B
iB
�B
-B
MB
!B
�B
ZB
�B
!B
B
fB
pB
bB
#B
B
?B
�B
�B
�B
"B
JB
B
�B
AB
LB
�B
�B
�B
�B
TB
GB
}B
�B
 B
uB
�B
�B
>B
�B
)B
�B
�B
;B
�B
B
=B
�B
B
�B
B
�B
B
qB
�B
mB
�B
�B
#B
�B
�B
>B
`B
�B
�B
�B
�B
�B
YB
iB
�B
�B
 /B
�B
�B
 .B
 =B
 �B
"B
!�B
"FB
#LB
#�B
$B
#�B
#�B
"�B
"�B
"gB
"^B
"B
"�B
#B
"�B
"B
""B
!�B
!:B
!B
"�B
#lB
#iB
"�B
"B
!�B
"#B
"XB
"mB
#�B
#B
#OB
$bB
$�B
$�B
%)B
$�B
$�B
%�B
&�B
&;B
&�B
&XB
%�B
&�B
&�B
'B
(?B
'�B
'�B
'�B
'�B
'�B
'�B
(bB
)�B
)�B
*:B
*uB
*�B
+|B
+�B
+B
,$B
,�B
,�B
.�B
.*B
.#B
._B
0B
0B
1'B
2�B
3B
2�B
2�B
3tB
3B
2�B
3$B
2�B
2�B
3B
3EB
3WB
2�B
3B
3LB
4B
56B
4�B
5\B
5�B
5xB
5�B
6B
5�B
5�B
7EB
7/B
7B
7zB
7wB
8�B
9B
9�B
9�B
9�B
9�B
9�B
9*B
9BB
9B
9LB
:B
;RB
;�B
;JB
;2B
;�B
;�B
<�B
<�B
<�B
<�B
=�B
=�B
>GB
>B
>�B
@6B
@:B
@�B
@�B
@�B
@�B
A}B
A�B
ApB
AhB
A�B
A�B
ApB
A�B
AWB
A�B
A�B
@�B
@fB
@B
@B
@�B
A,B
AB
A�B
BNB
CB
CB
CB
CB
B�B
C>B
C�B
C�B
C�B
D�B
D�B
D�B
E\B
F(B
F}B
F�B
F�B
F�B
F�B
F�B
F�B
GB
H^B
H�B
I-B
IWB
J"B
JSB
I�B
J�B
J�B
K[B
KQB
KB
K�B
K�B
K�B
LIB
LLB
L�B
L�B
L�B
M-B
M�B
M|B
M�B
M�B
N�B
NiB
O�B
PB
PwB
O�B
PGB
Q�B
Q0B
Q[B
RB
R�B
R\B
R�B
R�B
RaB
RsB
SPB
S�B
ToB
T�B
U�B
UCB
U<B
U�B
V�B
W3B
V�B
V�B
V�B
V�B
WZB
X0B
W�B
W�B
X:B
X�B
X\B
XcB
X�B
Y^B
Y�B
YlB
YgB
Y@B
ZB
Z2B
Z6B
Y�B
Z�B
[�B
[8B
[�B
[;B
\9B
[�B
[�B
\�B
]<B
]B
]eB
]�B
]�B
]�B
^*B
^�B
^�B
_�B
_B
_FB
`1B
`0B
`�B
a�B
`�B
aUB
a�B
a�B
a�B
cB
bcB
c�B
cIB
b�B
c"B
c�B
c�B
dRB
dB
d]B
drB
d-B
d�B
d�B
d�B
d�B
e%B
e�B
e�B
fB
f�B
e�B
e�B
e�B
fYB
gB
g1B
f�B
gB
f�B
g*B
g�B
g�B
g�B
g�B
g�B
hJB
hHB
hB
hB
haB
hTB
hZB
h�B
i,B
h�B
iB
i�B
j�B
j�B
j�B
j�B
k3B
kB
k0B
kEB
k�B
l~B
lDB
ljB
l�B
l�B
l�B
mB
l�B
l�B
m�B
mhB
nB
n�B
nbB
nUB
nB
n�B
o8B
oEB
o�B
o�B
pB
o�B
oKB
o~B
oZB
o(B
o�B
o�B
ouB
pB
p{B
p-B
p�B
pNB
p�B
p�B
qaB
p�B
qB
p�B
q�B
q_B
q�B
qBB
rIB
rIB
q�B
rhB
r�B
sB
sFB
r�B
sKB
s
B
sB
sqB
s�B
s�B
tFB
t2B
t�B
t�B
t�B
tB
tXB
t3B
t[B
tTB
t�B
t�B
u�B
vB
vB
v�B
wvB
w�B
x|B
x�B
w�B
xDB
w�B
x`B
x�B
y"B
yOB
x�B
x�B
x�B
x�B
x�B
yFB
y�B
y�B
ydB
y�B
{B
{B
z�B
z�B
{QB
{2B
{�B
|B
|B
|yB
|�B
}3B
|�B
}B
}�B
~GB
	B
2B
�B
�B
�4B
� B
�B
��B
��B
��B
�^B
�rB
�@B
�rB
�BB
��B
�[B
�CB
�B
�=B
��B
�3B
�CB
��B
��B
��B
�TB
�~B
��B
��B
��B
��B
�gB
�TB
��B
��B
� B
�B
��B
�B
�B
�8B
��B
�cB
��B
��B
�dB
�qB
��B
�dB
�AB
��B
��B
��B
��B
��B
��B
�B
�YB
��B
��B
��B
��B
��B
��B
� B
�FB
� B
�vB
�mB
��B
�[B
�B
��B
�,B
��B
��B
��B
��B
��B
�B
�+B
��B
�}B
��B
��B
�B
��B
�B
��B
��B
�B
��B
��B
��B
��B
�B
�B
��B
��B
��B
��B
�3G�O�B��B�oB~�B�B�oB��B�B~�B� B��B.B�oB�B}�B�B~�B.B�iB��B��BcBcB��B��B� B~�B�B�B��BcB~�B�iB�B�oB.B�B��B�oB� B.B��B�oB�BcB�B�iB�;B�B~�B� B�oB�;B�4BcB.B��B�;B�B�B�;B��BcB.BcB�;B�B�B� B��B��B�B�B�;B�B�BcB�4B�oB�4BcB��B�B�B.B� B�;B�B��B.B� B�oB��B�B�B��B��B.B�B�oB�;B�B~�BcB�B�oB��B�B�B�;B�B.B.B��B�oB� B~�B�4B�;B�4BcB�B��B�;B� B~�B�iB�;B�4B�B~�B~�B�iB�BcB~�B� B��BcB~�BcB� B�iB~�B~(B~�B�B� B~�B}�B~�B�B~�B}�B}�B}�B.B}"B|PB}"B}�B}�B|PB{B|PB|�B|BzDBy�BzxBz�By	BwfBx8BxBw�Bu�Bs�BtBsMBrGBpBm]BlWB~]B�uB��B�oB{�B.B�JB��B��B�LB��B� B�<B��B	v�B
B
B[B
o�B
h
B
d�B
{B
poB
q�B
}VB
��B
��B
�BB
��B
�	B
�]B�BuB�B�B	lB�B~B~B
�B�BPBDBB\B B�BB�B�BqB!�B!�B*�B:*B2-B/OB6FB2�B2�B:^B?HB7LB=B9�B>�BF�BG�BJ#BJXBN<BP}BM6BM�BOBM6BS�BS�BQ�BS�BR�BV9BT,BT�BV�BP�BYBWsBVBgmB\�BZ�BX�BX�BZ�B\�B]dBZQB[�B\�B\�B\�Ba�Be`B`�B`�Ba�Bc�Be�B`BBdZBbNBh�BaHBi�BffBi�B��BoiBl"BjBe,Bf�BjBncBo�Bm]BuZB��Bt�BsMBrGBs�BsBrGBr�BqABtTBr�Bs�Bp�Bs�Bs�BzBx8Bw2BcB�_B�MB}"B�_BsBo Bn�Bk�Bk�Bi�Bq�Bg�BiBd&Bb�B^�B^jBZB]�B[�BY�BVBW?BV9BR BI�BIBC�BC�B;0B>wB0�B-�B'�B'RB(XB(�B%B!B�B�B�BkB�BFBBuB�B�B
�B
=B	7B_B�B
rB	lBMB�B
��B
��B
�B
�DB
�DB�B
�B
�?B
ѷB
��B
�B
��B
��B
�B
��B
�@B
�1B
��B
��B
�hB
�B
��B
��B
��B
�B
��B
��B
�7B
uZB
jKB
q�B
p�B
gmB
qAB
r�B
s�B
r�B
sB
�AB
w2B
u�B
w2B
t�B
n/B
^�B
zxB
\]B
EmB
B�B
<B
4�B
2-B
7�B
7�B
+�B
&�B
!�B
B
�B
IB
�B
�B
�B
�B
�B
~B	��B	�]B
  B	�B	�B	�/B	�B	�VB
*�B
$�B
(�B
(�B
 'B
7�B
2aB
0�B
2�B
2aB
.IB
,�B
/B
+6B
(XB
)�B
2�B
.IB
;�B
<6B
VB
hB
JB
	lB
�B
�B
YB
%B

	B
�B
�B
xB	��B	�B	��B	�B	�WB	�fB	�NB	�B	�B	��B	�B	�|B	��B	�B	ѷB	�RB	�RB	�?B	��B	��B	�B	�}B	��B	��B	��B	�[B	�}B	��B	��B	��B	��B	��B	��B	�gB	��B	ÖB	��B	��B	ŢB	ŢB	��B	�3B	�gB	ĜB	�#B	�zB	�mB	� B	��B	�$B	��B	��B	�B	��B	�hB	��B	��B	�[B	�hB	��B	��B	�LB	��B	��B	�:B	�!B	�B	��B	��B	��B	��B	�bB	��B	��B	�B	��B	��B	cB	�YB	z�B	{JB	|�B	t�B	sMB	�xB	sMB	h>B	h
B	i�B	h>B	b�B	e�B	`vB	U2B	R�B	QNB	JXB	J�B	C�B	I�B	B�B	O�B	?}B	6�B	4�B	3�B	/�B	0�B	0!B	9�B	&�B	0�B	poB	�B�B��B�;B��B	�B�fB��B��BޞB�B��B�B��B��BܒB�B��B	�B	;B��B	uB	  B	B	B	MB	�B		�B	�B	�B	�B	.B	!-B	&�B	,=B	9�B	R�B	U�B	[�B	`BB	aHB	gmB	gB	poB	�4B	� B	��B	�bB	�bB	�B	��B	�bB	�6B	��B	�B	��B	�B	�BB	�wB	�gB	�)B	�BB	уB	��B	��B	��B	�JB	�VB	��B
oB
;B
B
�B
�B
�B
�B
B
B
xB
 \B
*0B
1�B
3�B
4B
3�B
6�B
8�B
8B
8�B
8�B
9�B
FB
E�B
E�B
EmB
D�B
H�B
@�B
M�B
A B
33B
)�B
A B
;�B
@B
B[B
IB
C�B
Y�B
\�B
c�B
d�B
h
B
iDB
iDB
jKB
kQB
k�B
j�B
jKB
l"B
m)B
l�B
l"B
kQB
i�B
j�B
lWB
l�B
m�B
o�B
o B
pB
oiB
r|B
��B
��B
��B
��B
�xB
��B
�oB
�B
�-B
��B
�OB
��B
��B
�!B
��B
�4B
��B
��B
��B
�B
��B
��B
��B
��B
�tB
��B
�OB
�B
�B
��B
�UB
�[B
��B
�UB
�}B
��B
��B
��B
��B
� B
��B
��B
�'B
��B
�^B
�<B
бB
бB
ҽB
�aB
֡B
��B
�aB
��B
��B
֡B
�[G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                 <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<��>=�S�=�=��<\��<vA<���<]�<O=�<�h<XH�<#�
<W�><M��<��<F�	<G;�=U<�s�<���<���<���<�H�<��<#�
<#�
<�Q�<9��<�`v<3��<#�
<#�
<#�
<#�
<#�
<#�
<.$6<V�<6��<F�$<#�
<P�|=2)�<��<�h�<U��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2020010819324920200108193249IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020010820010920200108200109QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020010820010920200108200109QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020010906573320200109065733IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                