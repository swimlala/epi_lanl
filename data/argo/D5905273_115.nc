CDF   	   
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-02-23T10:20:32Z creation; 2023-05-01T21:35:41Z DMQC;      
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20210223102032  20230501213541  5905273 5905273 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               s   sAA  AOAO7314_008642_115                 7314_008642_115                 2C  2C  DD  SOLO_II                         SOLO_II                         8642                            8642                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�`��r�(@�`��r�(11  @�`�ݗ�+@�`�ݗ�+@0�sl�&l@0�sl�&l�b�.۵���b�.۵��11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q@�\@@  @}p�@��R@��R@޸RA ��A��A\)A+�A>�RA`  A�Q�A�  A�  A�Q�A�  A�  A�  A�  B   B  B  B  B   B(  B/�
B7�
B?�
BH  BP  BX  B`(�Bh  Bp(�Bx(�B�{B�{B�{B�(�B�  B��B��B�  B�{B�  B��B��B��
B��
B��B�{B�=qB�{B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�{B�  B�  B�  C   C  C  C  C��C
  C  C  C  C  C  C��C��C  C  C  C 
=C"
=C$
=C&
=C(
=C*
=C,  C.  C0  C2  C4  C5��C7�C:  C<  C>
=C@
=CB
=CD{CF
=CG��CJ  CL  CN
=CP{CR  CS��CU��CX  CZ  C\  C^
=C`
=Cb
=Cd
=Cf
=Ch
=Ci��Ck��Cn  Co��Cq��Cs��Cv  Cx
=Cz
=C|
=C}��C�C���C�C�  C�  C�C�C���C���C���C�  C�  C�  C���C�  C�C�  C���C���C���C�  C�C�C�C�C�C�C�
=C�  C���C�  C�  C���C���C�  C�  C���C���C�C�C�C�  C�  C�C�  C���C�C�C���C�  C�  C�  C�C�C�  C���C�C�C���C�  C�  C���C���C�  C�  C�  C�  C���C���C���C�  C�  C�  C���C�  C�
=C�C�C�C�C�  C���C�  C���C���C�  C�C�C�C�C�  C�  C�C���C���C�  C���C�  C�C�  C�  C�  C���C���C�  C�  C���C�  C�C�  C���C�  C�  C�C�  C�  C���C���C���C���C���C�  C���C���C���C���C�C�  D   D � D �qD��D  D}qD�D}qD��D}qD�D�DD��D  D� D�D��D	�D	��D
�D
z�D
��Dz�D�qD}qD�qD}qD��D��D�D}qD  D� D  D� D  D� D�D��D�D�D�D�DD� D�D�D�qD}qD�qD}qD�D��D�D� D  D� D  D� D  D� D�D��D�qD }qD �qD!}qD"  D"� D#  D#� D$�D$��D%  D%� D&  D&� D'  D'� D'�qD(� D)D)��D*�D*��D*�qD+}qD,�D,��D-  D-� D-�qD.��D/D/��D/�qD0}qD0�qD1� D2  D2� D2�qD3}qD3�qD4}qD5�D5��D6�D6��D6�qD7}qD7�qD8� D9�D9}qD9�qD:��D;�D;� D<  D<� D=�D=��D>  D>��D?  D?��D@�D@��DA  DA� DB  DB� DC  DC��DD�DD��DE  DE�DFDF� DF�qDG}qDH�DH� DH�qDI� DI�qDJ� DK  DK}qDK��DL� DM�DM��DN  DN� DO�DO��DP  DP}qDP�qDQ� DR  DR��DS�DS� DS�qDT}qDU  DU��DV�DV��DW�DW}qDX  DX��DY�DY� DZ  DZ� D[  D[� D[�qD\}qD]  D]��D^  D^��D_�D_� D_�qD`� Da�Da��Db�Db��Dc  Dc� Dd�Dd��De�De� De�qDf� Dg  Dg}qDh  Dh}qDi  Di��Dj�Dj� Dk  Dk��Dl  Dl� Dm�Dm� Dn�Dn��Do  Do��Dp�Dp��Dq�Dq}qDq�qDr��Ds�Ds� Dt  Dt� Du�Du��Dv  Dv� Dw�Dw��Dx  Dx}qDy  Dy� Dz  Dz}qD{  D{� D|  D|� D}�D}��D~  D~}qD  D}qD�qD�@ D�� D���D���D�@ D��HD��HD�  D�>�D�~�D���D��qD�>�D��HD�� D�  D�AHD��HD�� D���D�@ D��HD���D���D�@ D�~�D���D�  D�@ D�� D�� D�  D�AHD��HD��HD�HD�>�D�� D�� D�  D�AHD���D��HD�HD�@ D�� D���D���D�@ D���D�D�HD�>�D�}qD���D���D�@ D��HD�� D��qD�=qD�~�D���D�  D�AHD��HD��HD��D�AHD�� D���D��qD�>�D�� D�� D�HD�AHD�~�D�� D�HD�@ D�~�D�� D���D�=qD�~�D��HD�  D�>�D�� D��HD�HD�>�D�� D���D��qD�>�D�� D���D��qD�>�D�� D�� D�HD�@ D�~�D��HD�  D�>�D�� D�� D���D�AHD��HD���D���D�AHD�� D�� D�  D�@ D���D�D���D�=qD�� D�D��D�@ D��HD��HD���D�>�D�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�=qD�}qD�� D�HD�>�D�� D��HD�  D�>�D�}qD���D�  D�@ D��HD���D��qD�>�D�� D���D��qD�@ D���D���D�  D�B�D��HD��HD�HD�AHD��HD�D�  D�>�D�~�D�� D��D�AHD�~�D��HD�  D�=qD�� D��HD�  D�@ D�~�D���D���D�=qD�� D���D���D�>�D�~�D��HD�HD�AHD��HD���D���D�>�D�~�D�D��D�>�D�� D��HD���D�>�D�� D���D�  D�AHD��HD�D�  D�AHD��HD�� D�  D�AHD�~�D���D���D�>�D��HD��HD�  D�@ D�� D��HD�  D�>�D�� D��HD�HD�@ D�� D�� D�HD�@ D�� D�� D�  D�@ D�� D��HD�  D�@ D��HD�� D�  D�>�D�~�D�� D���D�>�D�� D���D�  D�AHDHD��HD�  D�@ DÀ D��HD�HD�@ DĀ D�� D�  D�>�D�~�Dž�D�  D�@ Dƀ D�� D�HD�@ D�~�DǽqD���D�AHDȁHD�� D�HD�@ Dɀ D��HD�  D�@ Dʀ D�� D�HD�AHDˀ D˾�D���D�AHD́HD�� D���D�@ D́HD��HD�  D�@ D΀ Dξ�D�  D�@ DρHD��HD�  D�AHDЁHD�� D�HD�@ Dр D�D�  D�@ D�~�D�� D�  D�@ DӀ D�� D�  D�@ DԀ DԾ�D�  D�@ D�~�D�� D�  D�>�D�~�D־�D�  D�B�DׁHD��HD�HD�@ D�~�D�� D�HD�@ D�~�D�� D�  D�>�D�}qD�� D�HD�>�D�~�D��HD�HD�>�D�~�D�� D�HD�>�D�~�D�� D���D�=qDހ D�� D�HD�B�D߁HD�� D�  D�>�D�~�DྸD���D�>�D�~�DᾸD�  D�@ D� D�� D�HD�@ D�~�D�� D��D�AHD�~�D�� D�HD�AHD傏D��HD�  D�AHD�HD�� D�  D�=qD�}qD��HD�HD�AHD� D��HD�HD�>�D�~�D�� D��qD�>�D� D꾸D�  D�AHD�~�D�qD���D�@ D� D쾸D�HD�@ D�}qD���D�HD�@ D� D�� D�  D�AHD�HD��HD�HD�AHD�� D�� D�  D�>�D�}qD�qD�  D�AHD�HD��HD�HD�AHD� D��HD�HD�AHD�HD�D�HD�>�D�~�D���D�  D�@ D��HD��HD��D�@ D�}qD���D�  D�AHD��HD��HD�  D�@ D��HD���D�HD�B�D�y�?8Q�?B�\?W
=?��?�=q?���?�p�?��?�G�?��@   @�@��@�@��@#�
@0��@8Q�@=p�@L��@W
=@aG�@n{@z�H@��\@���@��@�Q�@��\@���@���@���@�G�@���@У�@ٙ�@�  @���@��@�(�A�AA
=qA�RA�AffA�HA�RA$z�A(��A.{A2�\A6ffA;�A@  AC�
AH��AN{AS33AXQ�A\��Aa�AfffAj=qAn�RAs�
AxQ�A|��A���A��A�p�A�\)A���A��
A�{A��A��A�(�A�ffA�Q�A��\A�z�A�ffA���A��HA�z�A�ffA���A��A�p�A�  A�=qA�z�A��RA���A�33A�p�A�\)A���A�(�A�ffA�Q�Aʏ\A�z�A�{A�  A��A��
A�A׮Aٙ�A��
A�A߮AᙚA�A�p�A�\)A�G�A��HA�z�A�{A�  A�=qA�(�A�A�  A���A��
A�p�A��B ��B{B\)Bz�B��B�\B�B��B	�B
=B  BG�B�\B�B��B=qB\)B��BB
=BQ�B��B�HB(�Bp�B�\B�B ��B!�B#33B$z�B%B'
=B((�B)G�B*ffB+�B,��B.{B/33B0��B1�B2�HB4Q�B5G�B6�\B7�
B8��B:ffB;�B<��B=B>�RB@  BAG�BBffBC�BD��BE�BF�HBH  BH��BJ{BK33BLQ�BMp�BN�RBO�
BP��BR{BR�HBT(�BT��BV=qBW�BX��BY��BZ�RB[�
B\��B^=qB_\)B`z�Ba��Bb�\Bc�
Be�Bf=qBg�Bh��BiBj�HBl(�Bmp�Bn�RBo�
Bp��Br{Bs33Bt��Bu�Bw
=Bx(�ByG�BzffB{�B|��B~{B\)B�=qB��RB�\)B�  B���B�G�B��
B�ffB���B���B�=qB���B�G�B��B��\B�33B��B�(�B���B�p�B�  B���B�33B�B�=qB��HB��B�(�B��\B��B�B�Q�B���B���B�{B��\B��B�B�Q�B���B�G�B��
B�ffB�
=B���B�{B���B�
=B���B�(�B��RB�33B��B�(�B��RB�\)B��
B�=qB���B�\)B��B�ffB���B�G�B��B�ffB��HB�\)B��
B�ffB���B��B��B�ffB���B��B�  B�z�B��HB�\)B��B�z�B�
=B�p�B��
B�z�B��B���B�{B���B�\)B�  B���B�33B��
B���B�\)B�  B��RB�\)B�{B��HB��B�(�B���B���B�Q�B���B��B�=qB��HB�p�B��B�z�B�
=B��B��B�Q�B£�B�
=B�p�BîB��B�(�Bď\B��HB�
=B�G�Bř�B��B�Q�BƏ\BƸRB�
=B�\)B�B�{B�Q�Bȏ\B���B�G�Bə�B��
B�(�Bʣ�B���B�G�B˅B��B�Q�B̏\B���B�33B͙�B��B�(�B�z�B��HB�G�BυB��
B�(�BЏ\B���B�33BхB��B�Q�Bҏ\B��HB�\)B�B�{B�ffB���B�33BՅB��B�=qBָRB���B�G�B�B�{B�ffBظRB�33Bٙ�B��
B�(�Bڣ�B���B�\)BۮB�{B�z�B���B�
=B�p�B��
B�=qB�z�B��HB�\)Bߙ�B�  B�ffB�RB���B�p�B��
B�{B�ffB��HB�G�B�B��
B�Q�B��B�
=B�\)B�B�(�B�z�B���B�G�B�B�  B�ffB���B��B�p�B��
B�=qB�\B���B�33B뙚B��B�(�B��B���B�G�B�B�  B�Q�B��B���B�\)B�B�  B�Q�B���B�33B�p�B�B�(�B�\B���B�33B�B��B�=qB���B���B�33B���B�  B�=qB��\B�
=B�\)B���B�{B�z�B���B���B�\)B�B�  B�ffB��RB���B�\)B�B�  B�Q�B��RB�
=B�G�B��B�{B�ffB���B��B�p�B��C 
=C 33C \)C �C �RC �C
=C=qCp�C�\CC��C{CG�C�C�C��C  C33CQ�C�\CC�C�C\)Cz�C�C�HC{C33Cp�C��CC  C(�CQ�C��CC�HC�CQ�Cz�C�C�C
=C=qCz�C��C��C	
=C	=qC	\)C	��C	�
C
  C
(�C
\)C
��C
�RC
��C33C\)C�C��C��C(�CffC��CC
=C=qCp�C�C�C{CQ�C�\C�RC�C(�C\)C�C��C��C33Cp�C��C�
C�CG�Cz�C�RC�C(�CffC�\C��C
=C33Cz�C�C�
C(�CQ�C�\CC��C33CffC��C�HC
=CQ�C�\C�RC
=CG�Cz�CC��C33Cz�C�C��C(�CffC�C�HC(�C\)C�\C�HC{CG�C�\C��C  CG�Cz�C�C��C(�CffC�C�
C�C\)C�C��C  C=qC�C�RC��C 33C ffC �C �C!�C!p�C!��C!�HC"�C"\)C"��C"�
C#�C#\)C#�\C#�
C$�C$Q�C$��C$��C%{C%Q�C%�\C%�
C&{C&\)C&��C&��C'�C'Q�C'�\C'�HC({C(Q�C(��C(��C)�C)Q�C)�\C)�HC*{C*Q�C*��C*�
C+(�C+\)C+��C+�C,{C,ffC,�C,�HC-(�C-p�C-�C-��C.(�C.�C.�RC/  C/=qC/z�C/��C/��C0G�C0�\C0��C1{C1G�C1��C1�
C2�C2ffC2��C2�C3(�C3z�C3�RC4  C4=qC4z�C4�
C5{C5Q�C5��C5�HC633C6p�C6�C7  C7=qC7�C7�
C8
=C8\)C8��C8�HC9(�C9p�C9C:  C:G�C:�\C:��C;(�C;\)C;��C;�C<33C<�C<C=
=C=\)C=�\C=�C>(�C>ffC>C>��C?G�C?�\C?��C@�C@\)C@�C@�CA33CA�CACB{CBG�CB��CB�HCC(�CCp�CC�CD  CDG�CD�\CD�
CE�CEp�CE��CE��CF=qCFz�CF�
CG
=CG\)CG��CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                      ?�=q@�\@@  @}p�@��R@��R@޸RA ��A��A\)A+�A>�RA`  A�Q�A�  A�  A�Q�A�  A�  A�  A�  B   B  B  B  B   B(  B/�
B7�
B?�
BH  BP  BX  B`(�Bh  Bp(�Bx(�B�{B�{B�{B�(�B�  B��B��B�  B�{B�  B��B��B��
B��
B��B�{B�=qB�{B��B�  B�  B�  B�  B�  B�  B�  B�  B�  B�{B�  B�  B�  C   C  C  C  C��C
  C  C  C  C  C  C��C��C  C  C  C 
=C"
=C$
=C&
=C(
=C*
=C,  C.  C0  C2  C4  C5��C7�C:  C<  C>
=C@
=CB
=CD{CF
=CG��CJ  CL  CN
=CP{CR  CS��CU��CX  CZ  C\  C^
=C`
=Cb
=Cd
=Cf
=Ch
=Ci��Ck��Cn  Co��Cq��Cs��Cv  Cx
=Cz
=C|
=C}��C�C���C�C�  C�  C�C�C���C���C���C�  C�  C�  C���C�  C�C�  C���C���C���C�  C�C�C�C�C�C�C�
=C�  C���C�  C�  C���C���C�  C�  C���C���C�C�C�C�  C�  C�C�  C���C�C�C���C�  C�  C�  C�C�C�  C���C�C�C���C�  C�  C���C���C�  C�  C�  C�  C���C���C���C�  C�  C�  C���C�  C�
=C�C�C�C�C�  C���C�  C���C���C�  C�C�C�C�C�  C�  C�C���C���C�  C���C�  C�C�  C�  C�  C���C���C�  C�  C���C�  C�C�  C���C�  C�  C�C�  C�  C���C���C���C���C���C�  C���C���C���C���C�C�  D   D � D �qD��D  D}qD�D}qD��D}qD�D�DD��D  D� D�D��D	�D	��D
�D
z�D
��Dz�D�qD}qD�qD}qD��D��D�D}qD  D� D  D� D  D� D�D��D�D�D�D�DD� D�D�D�qD}qD�qD}qD�D��D�D� D  D� D  D� D  D� D�D��D�qD }qD �qD!}qD"  D"� D#  D#� D$�D$��D%  D%� D&  D&� D'  D'� D'�qD(� D)D)��D*�D*��D*�qD+}qD,�D,��D-  D-� D-�qD.��D/D/��D/�qD0}qD0�qD1� D2  D2� D2�qD3}qD3�qD4}qD5�D5��D6�D6��D6�qD7}qD7�qD8� D9�D9}qD9�qD:��D;�D;� D<  D<� D=�D=��D>  D>��D?  D?��D@�D@��DA  DA� DB  DB� DC  DC��DD�DD��DE  DE�DFDF� DF�qDG}qDH�DH� DH�qDI� DI�qDJ� DK  DK}qDK��DL� DM�DM��DN  DN� DO�DO��DP  DP}qDP�qDQ� DR  DR��DS�DS� DS�qDT}qDU  DU��DV�DV��DW�DW}qDX  DX��DY�DY� DZ  DZ� D[  D[� D[�qD\}qD]  D]��D^  D^��D_�D_� D_�qD`� Da�Da��Db�Db��Dc  Dc� Dd�Dd��De�De� De�qDf� Dg  Dg}qDh  Dh}qDi  Di��Dj�Dj� Dk  Dk��Dl  Dl� Dm�Dm� Dn�Dn��Do  Do��Dp�Dp��Dq�Dq}qDq�qDr��Ds�Ds� Dt  Dt� Du�Du��Dv  Dv� Dw�Dw��Dx  Dx}qDy  Dy� Dz  Dz}qD{  D{� D|  D|� D}�D}��D~  D~}qD  D}qD�qD�@ D�� D���D���D�@ D��HD��HD�  D�>�D�~�D���D��qD�>�D��HD�� D�  D�AHD��HD�� D���D�@ D��HD���D���D�@ D�~�D���D�  D�@ D�� D�� D�  D�AHD��HD��HD�HD�>�D�� D�� D�  D�AHD���D��HD�HD�@ D�� D���D���D�@ D���D�D�HD�>�D�}qD���D���D�@ D��HD�� D��qD�=qD�~�D���D�  D�AHD��HD��HD��D�AHD�� D���D��qD�>�D�� D�� D�HD�AHD�~�D�� D�HD�@ D�~�D�� D���D�=qD�~�D��HD�  D�>�D�� D��HD�HD�>�D�� D���D��qD�>�D�� D���D��qD�>�D�� D�� D�HD�@ D�~�D��HD�  D�>�D�� D�� D���D�AHD��HD���D���D�AHD�� D�� D�  D�@ D���D�D���D�=qD�� D�D��D�@ D��HD��HD���D�>�D�~�D�� D�  D�@ D�� D�� D�  D�@ D�� D�� D�HD�=qD�}qD�� D�HD�>�D�� D��HD�  D�>�D�}qD���D�  D�@ D��HD���D��qD�>�D�� D���D��qD�@ D���D���D�  D�B�D��HD��HD�HD�AHD��HD�D�  D�>�D�~�D�� D��D�AHD�~�D��HD�  D�=qD�� D��HD�  D�@ D�~�D���D���D�=qD�� D���D���D�>�D�~�D��HD�HD�AHD��HD���D���D�>�D�~�D�D��D�>�D�� D��HD���D�>�D�� D���D�  D�AHD��HD�D�  D�AHD��HD�� D�  D�AHD�~�D���D���D�>�D��HD��HD�  D�@ D�� D��HD�  D�>�D�� D��HD�HD�@ D�� D�� D�HD�@ D�� D�� D�  D�@ D�� D��HD�  D�@ D��HD�� D�  D�>�D�~�D�� D���D�>�D�� D���D�  D�AHDHD��HD�  D�@ DÀ D��HD�HD�@ DĀ D�� D�  D�>�D�~�Dž�D�  D�@ Dƀ D�� D�HD�@ D�~�DǽqD���D�AHDȁHD�� D�HD�@ Dɀ D��HD�  D�@ Dʀ D�� D�HD�AHDˀ D˾�D���D�AHD́HD�� D���D�@ D́HD��HD�  D�@ D΀ Dξ�D�  D�@ DρHD��HD�  D�AHDЁHD�� D�HD�@ Dр D�D�  D�@ D�~�D�� D�  D�@ DӀ D�� D�  D�@ DԀ DԾ�D�  D�@ D�~�D�� D�  D�>�D�~�D־�D�  D�B�DׁHD��HD�HD�@ D�~�D�� D�HD�@ D�~�D�� D�  D�>�D�}qD�� D�HD�>�D�~�D��HD�HD�>�D�~�D�� D�HD�>�D�~�D�� D���D�=qDހ D�� D�HD�B�D߁HD�� D�  D�>�D�~�DྸD���D�>�D�~�DᾸD�  D�@ D� D�� D�HD�@ D�~�D�� D��D�AHD�~�D�� D�HD�AHD傏D��HD�  D�AHD�HD�� D�  D�=qD�}qD��HD�HD�AHD� D��HD�HD�>�D�~�D�� D��qD�>�D� D꾸D�  D�AHD�~�D�qD���D�@ D� D쾸D�HD�@ D�}qD���D�HD�@ D� D�� D�  D�AHD�HD��HD�HD�AHD�� D�� D�  D�>�D�}qD�qD�  D�AHD�HD��HD�HD�AHD� D��HD�HD�AHD�HD�D�HD�>�D�~�D���D�  D�@ D��HD��HD��D�@ D�}qD���D�  D�AHD��HD��HD�  D�@ D��HD���D�HD�B�G�O�?8Q�?B�\?W
=?��?�=q?���?�p�?��?�G�?��@   @�@��@�@��@#�
@0��@8Q�@=p�@L��@W
=@aG�@n{@z�H@��\@���@��@�Q�@��\@���@���@���@�G�@���@У�@ٙ�@�  @���@��@�(�A�AA
=qA�RA�AffA�HA�RA$z�A(��A.{A2�\A6ffA;�A@  AC�
AH��AN{AS33AXQ�A\��Aa�AfffAj=qAn�RAs�
AxQ�A|��A���A��A�p�A�\)A���A��
A�{A��A��A�(�A�ffA�Q�A��\A�z�A�ffA���A��HA�z�A�ffA���A��A�p�A�  A�=qA�z�A��RA���A�33A�p�A�\)A���A�(�A�ffA�Q�Aʏ\A�z�A�{A�  A��A��
A�A׮Aٙ�A��
A�A߮AᙚA�A�p�A�\)A�G�A��HA�z�A�{A�  A�=qA�(�A�A�  A���A��
A�p�A��B ��B{B\)Bz�B��B�\B�B��B	�B
=B  BG�B�\B�B��B=qB\)B��BB
=BQ�B��B�HB(�Bp�B�\B�B ��B!�B#33B$z�B%B'
=B((�B)G�B*ffB+�B,��B.{B/33B0��B1�B2�HB4Q�B5G�B6�\B7�
B8��B:ffB;�B<��B=B>�RB@  BAG�BBffBC�BD��BE�BF�HBH  BH��BJ{BK33BLQ�BMp�BN�RBO�
BP��BR{BR�HBT(�BT��BV=qBW�BX��BY��BZ�RB[�
B\��B^=qB_\)B`z�Ba��Bb�\Bc�
Be�Bf=qBg�Bh��BiBj�HBl(�Bmp�Bn�RBo�
Bp��Br{Bs33Bt��Bu�Bw
=Bx(�ByG�BzffB{�B|��B~{B\)B�=qB��RB�\)B�  B���B�G�B��
B�ffB���B���B�=qB���B�G�B��B��\B�33B��B�(�B���B�p�B�  B���B�33B�B�=qB��HB��B�(�B��\B��B�B�Q�B���B���B�{B��\B��B�B�Q�B���B�G�B��
B�ffB�
=B���B�{B���B�
=B���B�(�B��RB�33B��B�(�B��RB�\)B��
B�=qB���B�\)B��B�ffB���B�G�B��B�ffB��HB�\)B��
B�ffB���B��B��B�ffB���B��B�  B�z�B��HB�\)B��B�z�B�
=B�p�B��
B�z�B��B���B�{B���B�\)B�  B���B�33B��
B���B�\)B�  B��RB�\)B�{B��HB��B�(�B���B���B�Q�B���B��B�=qB��HB�p�B��B�z�B�
=B��B��B�Q�B£�B�
=B�p�BîB��B�(�Bď\B��HB�
=B�G�Bř�B��B�Q�BƏ\BƸRB�
=B�\)B�B�{B�Q�Bȏ\B���B�G�Bə�B��
B�(�Bʣ�B���B�G�B˅B��B�Q�B̏\B���B�33B͙�B��B�(�B�z�B��HB�G�BυB��
B�(�BЏ\B���B�33BхB��B�Q�Bҏ\B��HB�\)B�B�{B�ffB���B�33BՅB��B�=qBָRB���B�G�B�B�{B�ffBظRB�33Bٙ�B��
B�(�Bڣ�B���B�\)BۮB�{B�z�B���B�
=B�p�B��
B�=qB�z�B��HB�\)Bߙ�B�  B�ffB�RB���B�p�B��
B�{B�ffB��HB�G�B�B��
B�Q�B��B�
=B�\)B�B�(�B�z�B���B�G�B�B�  B�ffB���B��B�p�B��
B�=qB�\B���B�33B뙚B��B�(�B��B���B�G�B�B�  B�Q�B��B���B�\)B�B�  B�Q�B���B�33B�p�B�B�(�B�\B���B�33B�B��B�=qB���B���B�33B���B�  B�=qB��\B�
=B�\)B���B�{B�z�B���B���B�\)B�B�  B�ffB��RB���B�\)B�B�  B�Q�B��RB�
=B�G�B��B�{B�ffB���B��B�p�B��C 
=C 33C \)C �C �RC �C
=C=qCp�C�\CC��C{CG�C�C�C��C  C33CQ�C�\CC�C�C\)Cz�C�C�HC{C33Cp�C��CC  C(�CQ�C��CC�HC�CQ�Cz�C�C�C
=C=qCz�C��C��C	
=C	=qC	\)C	��C	�
C
  C
(�C
\)C
��C
�RC
��C33C\)C�C��C��C(�CffC��CC
=C=qCp�C�C�C{CQ�C�\C�RC�C(�C\)C�C��C��C33Cp�C��C�
C�CG�Cz�C�RC�C(�CffC�\C��C
=C33Cz�C�C�
C(�CQ�C�\CC��C33CffC��C�HC
=CQ�C�\C�RC
=CG�Cz�CC��C33Cz�C�C��C(�CffC�C�HC(�C\)C�\C�HC{CG�C�\C��C  CG�Cz�C�C��C(�CffC�C�
C�C\)C�C��C  C=qC�C�RC��C 33C ffC �C �C!�C!p�C!��C!�HC"�C"\)C"��C"�
C#�C#\)C#�\C#�
C$�C$Q�C$��C$��C%{C%Q�C%�\C%�
C&{C&\)C&��C&��C'�C'Q�C'�\C'�HC({C(Q�C(��C(��C)�C)Q�C)�\C)�HC*{C*Q�C*��C*�
C+(�C+\)C+��C+�C,{C,ffC,�C,�HC-(�C-p�C-�C-��C.(�C.�C.�RC/  C/=qC/z�C/��C/��C0G�C0�\C0��C1{C1G�C1��C1�
C2�C2ffC2��C2�C3(�C3z�C3�RC4  C4=qC4z�C4�
C5{C5Q�C5��C5�HC633C6p�C6�C7  C7=qC7�C7�
C8
=C8\)C8��C8�HC9(�C9p�C9C:  C:G�C:�\C:��C;(�C;\)C;��C;�C<33C<�C<C=
=C=\)C=�\C=�C>(�C>ffC>C>��C?G�C?�\C?��C@�C@\)C@�C@�CA33CA�CACB{CBG�CB��CB�HCC(�CCp�CC�CD  CDG�CD�\CD�
CE�CEp�CE��CE��CF=qCFz�CF�
CG
=CG\)CG��CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                      @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�2@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��-A��^A��^A��^A��wA���A�A��jA��^A��jA��jA���A���A�A�ĜA�A�ĜA�ƨA�ƨA�ƨA�ĜA�ƨA�ƨA�ȴA���A�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��\A��A��A�^5A���A�Q�A���A���A��uA�jA���A�%A��A�+A��uA�ƨA�  A�;dA�JA���A��yA�C�A���A�z�A��A�-A���A���A�%A�&�A~A�Ay�-Au��Aq;dAl�+Ai�;Ag&�Ac�hA_AY"�AW�AU�AQ+AM��AJAHbAE�PAB  A@��A?�A>��A=�hA;��A7�#A4 �A3��A4JA5�A6^5A5�7A7�A7��A5�TA5
=A4��A4�A4��A3K�A5A5�A5"�A4�A3C�A/+A.v�A,(�A&ĜA%7LA#�A$ �A$�A%�PA%�FA&JA&�\A&�DA&v�A&z�A�AO�AbNA(�AbA�AC�A7LA�AJAffA��A�uAbNAZA1A�\AjA^5A$�A�wA�A�Al�A
��A
VA	�#A	�AȴA~�A(�A�-A��A��A�DA~�A�AȴA�hA
=AA�/AC�@��@�\)@�\)@���@��@�w@��@�@��y@��@�bN@�@�5?@��y@��#@�\)@���@��/@���@�t�@�7@�hs@�@�A�@�
=@�;d@�|�@�@�Z@�%@�F@�\@���@��y@��H@�!@�n�@��#@�@�7L@��@��@�I�@߾w@ߥ�@�\)@���@��@ݙ�@�?}@�Q�@۝�@ڰ!@ٙ�@�`B@���@�Z@�A�@�b@��@ם�@��H@���@Ձ@�`B@��`@� �@ӶF@�l�@ӶF@Ӯ@�\)@��T@��@���@��@���@���@д9@�(�@϶F@�K�@�S�@�o@Χ�@�ff@���@�n�@�v�@͡�@��@� �@�S�@�+@ʗ�@ɺ^@��@�Ĝ@�j@�9X@�(�@�b@Ǯ@�t�@�C�@�V@�@őh@���@�9X@�1@�b@���@Å@�o@�;d@�V@���@���@�%@���@��T@���@�@��@��@���@�I�@�b@��m@��@�+@���@�~�@���@�bN@���@���@��h@��@�o@�@�O�@���@���@�I�@��@��;@��@�33@��y@��R@�~�@��^@�?}@��9@�(�@��w@�S�@��@���@�v�@�$�@��@��#@�@�p�@��H@���@�t�@�33@�@���@�ff@�E�@�@��@���@��@�r�@�A�@�9X@�Q�@�Z@��;@���@�C�@��@��R@��+@�n�@�M�@�@��h@���@�Z@�A�@��
@�S�@���@�5?@��@�@��@��#@�@���@�`B@�&�@�Q�@��P@�S�@��y@�n�@�V@�V@�-@���@�V@��`@���@��u@�A�@��P@��@��!@�ff@��T@��h@�hs@��@���@��D@�b@��@�+@�ȴ@�{@��-@�x�@�X@�G�@�/@�%@���@��@��/@��j@�r�@�A�@���@���@�|�@�S�@�K�@�
=@���@���@�^5@�=q@���@���@�x�@��@���@�bN@��@��F@��@�dZ@�o@��R@��\@�-@���@�hs@��@���@��`@���@�j@�  @�l�@�K�@��H@�~�@�@��@�G�@��`@�9X@�  @��;@��
@�ƨ@��@���@��@�ȴ@�$�@��-@��7@��7@�x�@�p�@�?}@��@���@��@�A�@�1@��
@��@�\)@��@��\@�=q@�{@��T@��@�&�@��/@��@�I�@�9X@��@�1@��;@�t�@�33@�@���@�^5@�=q@��@���@�@�`B@���@��`@���@���@��9@�r�@�Q�@�1@��@�@~�+@~5?@}��@}�h@}O�@|��@|��@|I�@|I�@{��@{@z��@z-@y�^@yhs@y&�@xĜ@x �@w�P@w
=@vv�@v$�@u@u/@t�@t9X@sƨ@s33@r�H@r�\@q�@q7L@p��@p�u@o�;@o�@n�+@n@m@mp�@m?}@l��@l�@k�
@k�@kdZ@j�\@j-@i�@i��@iX@h��@h1'@g�;@g�@g�P@g�@f�R@fv�@fV@fE�@f{@f{@e@e`B@d�/@d(�@c�@cS�@c@b�!@b-@a�^@a&�@`Ĝ@`�@`A�@` �@`b@`  @_�@_��@_;d@^ȴ@^ff@^E�@^{@]�h@\��@\�j@\�D@\j@[��@[�@[t�@[S�@["�@[@Z�!@Z=q@Y�7@Y7L@X��@Xr�@X1'@W�;@W�P@Wl�@W
=@V5?@U��@U`B@U?}@U�@T�@T�j@T�@T9X@SS�@R�H@R��@Q��@Q�@P�u@P  @O|�@OK�@N��@N�y@N��@N��@N@M��@L�@L�D@L9X@K�F@K�@Kt�@KS�@KC�@J��@I�#@I�7@IX@I%@Hr�@G�@G�;@G�w@G+@F�R@F�+@Fv�@FV@F$�@E�@Ep�@D�/@DI�@C��@C�F@C�@CdZ@B�H@B~�@BJ@Ax�@@��@@�@@Q�@@b@?�P@?K�@?;d@?�@>�@>��@>V@=��@=�@<��@<1@;��@;��@;�@;�@;dZ@;o@:�!@9��@9��@9X@9X@9&�@8�9@8r�@8bN@8A�@8 �@8b@8  @7�;@7l�@7�@6ȴ@6��@6ff@6E�@6$�@5@5�@5/@4��@4�D@4Z@49X@3�m@3�F@3��@3dZ@3S�@333@2��@2=q@1�@1��@1hs@0��@0�9@0 �@/�w@/l�@/;d@/�@.�y@.ȴ@.�+@.5?@-�T@-�h@-O�@,�/@,(�@+�F@+�@+33@*��@*�\@*�@)�#@)��@)x�@)X@)�@(�`@(�@(Q�@(1'@'�;@'\)@'K�@'K�@&��@&�+@&v�@&5?@&@%�T@%�-@%�@%�@$��@$��@$I�@$1@#��@#�m@#�@#33@"�H@"��@"n�@"^5@"^5@!�@!�7@!hs@!G�@!7L@!&�@!&�@!�@ ��@ �@  �@�@\)@�@ȴ@��@5?@�@p�@�@�@��@��@1@�
@��@�@S�@C�@@�@�H@��@�!@�\@n�@M�@�@�@��@��@hs@��@��@�9@�u@bN@Q�@1'@ �@b@  @�;@�@|�@l�@K�@+@
=@��@�@�+@V@{@�T@@p�@?}@V@�@��@Z@�@�m@�F@��@t�@dZ@"�@�H@��@n�@�@J@J@��@��@�@�@��@��@hs@&�@��@��@�@A�@  @  @�@�@�;@��@\)@K�@;d@�@��@5?@{@�@�T@�-@p�@?}@�@�D@Z@Z@I�@I�@9X@9X@(�@��@dZ@C�@o@
�@
��@
��@
��@
��@
=q@
�@	��@	�@	��@	�7@	x�@	hs@	7L@��@Ĝ@Ĝ@Ĝ@�9@�9@�9@�9@��@��@��@��@�u@r�@�@�w@�P@|�@\)@K�@K�@;d@�@
=@�y@ȴ@��@�+@V@E�@E�@E�@$�@$�@�@��@�-@�h@`B@`B@O�@?}@�@��@�/@�j@�j@�j@�@�@�@�@��@�D@I�@�@�@�@1@1A��A��A��RA��-A��-A��!A��!A��9A��wA��wA��^A��^A���A��jA��FA��^A��RA��jA��RA��wA��jA��RA��FA��FA��^A�ĜA��^A��^A�A���A���A���A���A�A���A�A�ƨA�A�A��wA��jA��^A��jA��wA��RA��^A��jA��jA��^A��wA��RA��wA��wA��wA�A�ĜA�A�A���A���A���A���A���A���A�A�ĜA�ĜA�A���A���A���A�A�ĜA�A�ĜA�ĜA�ĜA�A�ĜA�A�A�A�A�A�A�ĜA�ƨA�ƨA�ĜA�ƨA���A�A�A�ĜA�ĜA�A�ĜA�ƨA�ƨA�ƨA�ƨA�ĜA�ĜA�ĜA�ƨA�ƨA�ȴA�ȴA�ƨA�ȴA�ƨA�ȴA�ȴA�ƨA�ƨA�ƨA�ƨA�ĜA�ĜA�ƨA�ȴA�ȴA�ƨA�ĜA�A�A�A�ĜA�ĜA�ƨA�ȴA�ȴA�ȴA�ȴA�ĜA�ĜA�ƨA�ƨA�ƨA�ƨA�ȴA���A���A���A�ƨA�ĜA�ĜA�ĜA���A���A���A���A�ȴA�ƨA�ƨA�ƨA�A�ƨA�ƨA�ƨA�ȴA���A���A���A�ȴA���A���A���A���A���A�ƨA�ȴA���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��PA��PA��PA��\A��PA��DA��7A��A��A��A��A��A��A��A�jA�Q�A��A���A��mA��mA��`A���A�A���A��A�I�A�;dA��A�G�A��+A��A�M�A���A��uA�ffA��A���A�`BA���A�$�A�A��A���A��
A�A�M�A�VA���A�33A��A��/A��PA�M�A���A�ƨA��A��;A�ȴA�ffA��TA�x�A�-A��A��A��-A��wA�E�A���A�%A��TA�t�A��^A�5?A���A��HA�p�A�oA���A�r�A�A�A�
=A��mA�ĜA���A�r�A�;dA��;A��/A�ƨA��A��A���A��hA�~�A�r�A�l�A�ffA�XA�?}A��`A��RA��+A�`BA�+A�1A��
A��jA���A��uA�~�A�\)A�$�A��A�z�A��A�E�A�&�A��A�(�A��HA���A���A�S�A� �A�{A�A���A��A���A�ȴA��RA��A�|�A�I�A�33A��A�A��;A��hA���A�n�A�jA�`BA�`BA�^5A�ZA�XA�Q�A�S�A�S�A�Q�A�O�A�O�A�K�A�(�A���A�ĜA��A�x�A�VA�/A�"�A��A�
=A��A��A�hsA�C�A�oA���A���A�1A��A�A��/A��uA�XA�E�A�"�A�oA�{A�
=A���A���A���A���A��HA�ȴA��RA���A�hsA��yA���A�~�A�G�A�A�A��A�ĜA�ĜA�A��^A��RA��jA��^A��!A���A���A���A��PA��A�z�A�l�A�+A��mA���A�`BA��A��9A��DA�^5A�$�A���A�{A��
A���A�S�A�1'A�{A�ĜA��^A���A�ZA�7LA�bA�  A��A���A���A�x�A�XA�K�A�9XA�+A�VA��A���A�dZA�S�A�7LA��A�1A��A��A��PA��7A�|�A�S�A�?}A�JA��^A���A�VA�/A��A�1A���A��HA�ƨA���A�`BA�=qA��A���A��DA�M�A��A���A��A��
A��-A��+A��A�p�A���A�9XA��A���A�z�A���A�O�A��A;dA%A~��A~�`A~��A~��A~��A~~�A~^5A~(�A}��A}C�A|�9A|I�A{�TA{l�Az�Ay�Ax�AxbNAxAw�^Aw�PAwS�Aw/Aw�Av�yAv�!Av^5Au�hAt��As��Asx�Ar�HArz�Aq��Aq��Aq��Aq�AqVAp�RAp=qAp�Ao�Ao%AmXAl{Ak��AkAk��Akx�Akl�AkXAk/Aj��Aj��Aj(�Ai�
Ai�PAi33Ah��Ah�+AhA�Ag�Ag��Ag\)Ag33AgVAf��Af�Af9XAf1Ae��AeS�Ad�uAc�;Ac
=Ab9XAa��Aa��Aa�PAat�Aa\)A`�yA`E�A_�-A_;dA^~�A]�A[��AZ�RAY��AY+AX�AX1'AX{AX(�AX5?AX-AX�AXAW�AW�#AW��AW�wAW�AW��AW�AWG�AV�jAV�AUx�AT�AT9XAS`BAR�AQ�AQ�hAQG�AQ%APȴAPA�AO��AOXAN��AN-ANM�ANZAN �AM�AL��AL��AL�9AL^5AK7LAJ��AI|�AH��AHE�AH(�AH$�AH�AH{AHbAH1AHbAHAH1AG��AG�AG�-AEAD��AD=qAC�ACS�AB��ABr�ABI�ABE�AB1'AA�#AAhsAA?}AA33AA"�AA"�AA
=AAA@��A@�`A@��A@9XA?�;A?A?��A?l�A?;dA?
=A>��A>�A>�`A>�A>ĜA>�!A>bNA>9XA>�A=�A=�^A=��A=��A=�hA=XA="�A<��A<��A<�\A<M�A;��A:��A:5?A9�7A97LA9A8ffA7��A7hsA6��A6�A5`BA4��A4VA3�wA3��A3��A3��A3�A3�A3�7A3A3A3��A3�hA3|�A3dZA3\)A3l�A3��A4~�A4�yA4��A5�hA5�#A5�mA5�TA5�mA6bA65?A6M�A6ZA6bNA6bNA6bNA6n�A6ffA6M�A6$�A5��A5C�A5?}A5`BA5p�A5�PA6{A6^5A7
=A77LA7��A7��A7A7��A7��A7A7A7�hA7�7A7S�A6�`A6�+A65?A5�TA5�hA5XA5?}A5+A5�A5oA5VG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                      A��-A��^A��^A��^A��wA���A�A��jA��^A��jA��jA���A���A�A�ĜA�A�ĜA�ƨA�ƨA�ƨA�ĜA�ƨA�ƨA�ȴA���A�ƨA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��\A��A��A�^5A���A�Q�A���A���A��uA�jA���A�%A��A�+A��uA�ƨA�  A�;dA�JA���A��yA�C�A���A�z�A��A�-A���A���A�%A�&�A~A�Ay�-Au��Aq;dAl�+Ai�;Ag&�Ac�hA_AY"�AW�AU�AQ+AM��AJAHbAE�PAB  A@��A?�A>��A=�hA;��A7�#A4 �A3��A4JA5�A6^5A5�7A7�A7��A5�TA5
=A4��A4�A4��A3K�A5A5�A5"�A4�A3C�A/+A.v�A,(�A&ĜA%7LA#�A$ �A$�A%�PA%�FA&JA&�\A&�DA&v�A&z�A�AO�AbNA(�AbA�AC�A7LA�AJAffA��A�uAbNAZA1A�\AjA^5A$�A�wA�A�Al�A
��A
VA	�#A	�AȴA~�A(�A�-A��A��A�DA~�A�AȴA�hA
=AA�/AC�@��@�\)@�\)@���@��@�w@��@�@��y@��@�bN@�@�5?@��y@��#@�\)@���@��/@���@�t�@�7@�hs@�@�A�@�
=@�;d@�|�@�@�Z@�%@�F@�\@���@��y@��H@�!@�n�@��#@�@�7L@��@��@�I�@߾w@ߥ�@�\)@���@��@ݙ�@�?}@�Q�@۝�@ڰ!@ٙ�@�`B@���@�Z@�A�@�b@��@ם�@��H@���@Ձ@�`B@��`@� �@ӶF@�l�@ӶF@Ӯ@�\)@��T@��@���@��@���@���@д9@�(�@϶F@�K�@�S�@�o@Χ�@�ff@���@�n�@�v�@͡�@��@� �@�S�@�+@ʗ�@ɺ^@��@�Ĝ@�j@�9X@�(�@�b@Ǯ@�t�@�C�@�V@�@őh@���@�9X@�1@�b@���@Å@�o@�;d@�V@���@���@�%@���@��T@���@�@��@��@���@�I�@�b@��m@��@�+@���@�~�@���@�bN@���@���@��h@��@�o@�@�O�@���@���@�I�@��@��;@��@�33@��y@��R@�~�@��^@�?}@��9@�(�@��w@�S�@��@���@�v�@�$�@��@��#@�@�p�@��H@���@�t�@�33@�@���@�ff@�E�@�@��@���@��@�r�@�A�@�9X@�Q�@�Z@��;@���@�C�@��@��R@��+@�n�@�M�@�@��h@���@�Z@�A�@��
@�S�@���@�5?@��@�@��@��#@�@���@�`B@�&�@�Q�@��P@�S�@��y@�n�@�V@�V@�-@���@�V@��`@���@��u@�A�@��P@��@��!@�ff@��T@��h@�hs@��@���@��D@�b@��@�+@�ȴ@�{@��-@�x�@�X@�G�@�/@�%@���@��@��/@��j@�r�@�A�@���@���@�|�@�S�@�K�@�
=@���@���@�^5@�=q@���@���@�x�@��@���@�bN@��@��F@��@�dZ@�o@��R@��\@�-@���@�hs@��@���@��`@���@�j@�  @�l�@�K�@��H@�~�@�@��@�G�@��`@�9X@�  @��;@��
@�ƨ@��@���@��@�ȴ@�$�@��-@��7@��7@�x�@�p�@�?}@��@���@��@�A�@�1@��
@��@�\)@��@��\@�=q@�{@��T@��@�&�@��/@��@�I�@�9X@��@�1@��;@�t�@�33@�@���@�^5@�=q@��@���@�@�`B@���@��`@���@���@��9@�r�@�Q�@�1@��@�@~�+@~5?@}��@}�h@}O�@|��@|��@|I�@|I�@{��@{@z��@z-@y�^@yhs@y&�@xĜ@x �@w�P@w
=@vv�@v$�@u@u/@t�@t9X@sƨ@s33@r�H@r�\@q�@q7L@p��@p�u@o�;@o�@n�+@n@m@mp�@m?}@l��@l�@k�
@k�@kdZ@j�\@j-@i�@i��@iX@h��@h1'@g�;@g�@g�P@g�@f�R@fv�@fV@fE�@f{@f{@e@e`B@d�/@d(�@c�@cS�@c@b�!@b-@a�^@a&�@`Ĝ@`�@`A�@` �@`b@`  @_�@_��@_;d@^ȴ@^ff@^E�@^{@]�h@\��@\�j@\�D@\j@[��@[�@[t�@[S�@["�@[@Z�!@Z=q@Y�7@Y7L@X��@Xr�@X1'@W�;@W�P@Wl�@W
=@V5?@U��@U`B@U?}@U�@T�@T�j@T�@T9X@SS�@R�H@R��@Q��@Q�@P�u@P  @O|�@OK�@N��@N�y@N��@N��@N@M��@L�@L�D@L9X@K�F@K�@Kt�@KS�@KC�@J��@I�#@I�7@IX@I%@Hr�@G�@G�;@G�w@G+@F�R@F�+@Fv�@FV@F$�@E�@Ep�@D�/@DI�@C��@C�F@C�@CdZ@B�H@B~�@BJ@Ax�@@��@@�@@Q�@@b@?�P@?K�@?;d@?�@>�@>��@>V@=��@=�@<��@<1@;��@;��@;�@;�@;dZ@;o@:�!@9��@9��@9X@9X@9&�@8�9@8r�@8bN@8A�@8 �@8b@8  @7�;@7l�@7�@6ȴ@6��@6ff@6E�@6$�@5@5�@5/@4��@4�D@4Z@49X@3�m@3�F@3��@3dZ@3S�@333@2��@2=q@1�@1��@1hs@0��@0�9@0 �@/�w@/l�@/;d@/�@.�y@.ȴ@.�+@.5?@-�T@-�h@-O�@,�/@,(�@+�F@+�@+33@*��@*�\@*�@)�#@)��@)x�@)X@)�@(�`@(�@(Q�@(1'@'�;@'\)@'K�@'K�@&��@&�+@&v�@&5?@&@%�T@%�-@%�@%�@$��@$��@$I�@$1@#��@#�m@#�@#33@"�H@"��@"n�@"^5@"^5@!�@!�7@!hs@!G�@!7L@!&�@!&�@!�@ ��@ �@  �@�@\)@�@ȴ@��@5?@�@p�@�@�@��@��@1@�
@��@�@S�@C�@@�@�H@��@�!@�\@n�@M�@�@�@��@��@hs@��@��@�9@�u@bN@Q�@1'@ �@b@  @�;@�@|�@l�@K�@+@
=@��@�@�+@V@{@�T@@p�@?}@V@�@��@Z@�@�m@�F@��@t�@dZ@"�@�H@��@n�@�@J@J@��@��@�@�@��@��@hs@&�@��@��@�@A�@  @  @�@�@�;@��@\)@K�@;d@�@��@5?@{@�@�T@�-@p�@?}@�@�D@Z@Z@I�@I�@9X@9X@(�@��@dZ@C�@o@
�@
��@
��@
��@
��@
=q@
�@	��@	�@	��@	�7@	x�@	hs@	7L@��@Ĝ@Ĝ@Ĝ@�9@�9@�9@�9@��@��@��@��@�u@r�@�@�w@�P@|�@\)@K�@K�@;d@�@
=@�y@ȴ@��@�+@V@E�@E�@E�@$�@$�@�@��@�-@�h@`B@`B@O�@?}@�@��@�/@�j@�j@�j@�@�@�@�@��@�D@I�@�@�@�@1G�O�A��A��A��RA��-A��-A��!A��!A��9A��wA��wA��^A��^A���A��jA��FA��^A��RA��jA��RA��wA��jA��RA��FA��FA��^A�ĜA��^A��^A�A���A���A���A���A�A���A�A�ƨA�A�A��wA��jA��^A��jA��wA��RA��^A��jA��jA��^A��wA��RA��wA��wA��wA�A�ĜA�A�A���A���A���A���A���A���A�A�ĜA�ĜA�A���A���A���A�A�ĜA�A�ĜA�ĜA�ĜA�A�ĜA�A�A�A�A�A�A�ĜA�ƨA�ƨA�ĜA�ƨA���A�A�A�ĜA�ĜA�A�ĜA�ƨA�ƨA�ƨA�ƨA�ĜA�ĜA�ĜA�ƨA�ƨA�ȴA�ȴA�ƨA�ȴA�ƨA�ȴA�ȴA�ƨA�ƨA�ƨA�ƨA�ĜA�ĜA�ƨA�ȴA�ȴA�ƨA�ĜA�A�A�A�ĜA�ĜA�ƨA�ȴA�ȴA�ȴA�ȴA�ĜA�ĜA�ƨA�ƨA�ƨA�ƨA�ȴA���A���A���A�ƨA�ĜA�ĜA�ĜA���A���A���A���A�ȴA�ƨA�ƨA�ƨA�A�ƨA�ƨA�ƨA�ȴA���A���A���A�ȴA���A���A���A���A���A�ƨA�ȴA���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��PA��PA��PA��\A��PA��DA��7A��A��A��A��A��A��A��A�jA�Q�A��A���A��mA��mA��`A���A�A���A��A�I�A�;dA��A�G�A��+A��A�M�A���A��uA�ffA��A���A�`BA���A�$�A�A��A���A��
A�A�M�A�VA���A�33A��A��/A��PA�M�A���A�ƨA��A��;A�ȴA�ffA��TA�x�A�-A��A��A��-A��wA�E�A���A�%A��TA�t�A��^A�5?A���A��HA�p�A�oA���A�r�A�A�A�
=A��mA�ĜA���A�r�A�;dA��;A��/A�ƨA��A��A���A��hA�~�A�r�A�l�A�ffA�XA�?}A��`A��RA��+A�`BA�+A�1A��
A��jA���A��uA�~�A�\)A�$�A��A�z�A��A�E�A�&�A��A�(�A��HA���A���A�S�A� �A�{A�A���A��A���A�ȴA��RA��A�|�A�I�A�33A��A�A��;A��hA���A�n�A�jA�`BA�`BA�^5A�ZA�XA�Q�A�S�A�S�A�Q�A�O�A�O�A�K�A�(�A���A�ĜA��A�x�A�VA�/A�"�A��A�
=A��A��A�hsA�C�A�oA���A���A�1A��A�A��/A��uA�XA�E�A�"�A�oA�{A�
=A���A���A���A���A��HA�ȴA��RA���A�hsA��yA���A�~�A�G�A�A�A��A�ĜA�ĜA�A��^A��RA��jA��^A��!A���A���A���A��PA��A�z�A�l�A�+A��mA���A�`BA��A��9A��DA�^5A�$�A���A�{A��
A���A�S�A�1'A�{A�ĜA��^A���A�ZA�7LA�bA�  A��A���A���A�x�A�XA�K�A�9XA�+A�VA��A���A�dZA�S�A�7LA��A�1A��A��A��PA��7A�|�A�S�A�?}A�JA��^A���A�VA�/A��A�1A���A��HA�ƨA���A�`BA�=qA��A���A��DA�M�A��A���A��A��
A��-A��+A��A�p�A���A�9XA��A���A�z�A���A�O�A��A;dA%A~��A~�`A~��A~��A~��A~~�A~^5A~(�A}��A}C�A|�9A|I�A{�TA{l�Az�Ay�Ax�AxbNAxAw�^Aw�PAwS�Aw/Aw�Av�yAv�!Av^5Au�hAt��As��Asx�Ar�HArz�Aq��Aq��Aq��Aq�AqVAp�RAp=qAp�Ao�Ao%AmXAl{Ak��AkAk��Akx�Akl�AkXAk/Aj��Aj��Aj(�Ai�
Ai�PAi33Ah��Ah�+AhA�Ag�Ag��Ag\)Ag33AgVAf��Af�Af9XAf1Ae��AeS�Ad�uAc�;Ac
=Ab9XAa��Aa��Aa�PAat�Aa\)A`�yA`E�A_�-A_;dA^~�A]�A[��AZ�RAY��AY+AX�AX1'AX{AX(�AX5?AX-AX�AXAW�AW�#AW��AW�wAW�AW��AW�AWG�AV�jAV�AUx�AT�AT9XAS`BAR�AQ�AQ�hAQG�AQ%APȴAPA�AO��AOXAN��AN-ANM�ANZAN �AM�AL��AL��AL�9AL^5AK7LAJ��AI|�AH��AHE�AH(�AH$�AH�AH{AHbAH1AHbAHAH1AG��AG�AG�-AEAD��AD=qAC�ACS�AB��ABr�ABI�ABE�AB1'AA�#AAhsAA?}AA33AA"�AA"�AA
=AAA@��A@�`A@��A@9XA?�;A?A?��A?l�A?;dA?
=A>��A>�A>�`A>�A>ĜA>�!A>bNA>9XA>�A=�A=�^A=��A=��A=�hA=XA="�A<��A<��A<�\A<M�A;��A:��A:5?A9�7A97LA9A8ffA7��A7hsA6��A6�A5`BA4��A4VA3�wA3��A3��A3��A3�A3�A3�7A3A3A3��A3�hA3|�A3dZA3\)A3l�A3��A4~�A4�yA4��A5�hA5�#A5�mA5�TA5�mA6bA65?A6M�A6ZA6bNA6bNA6bNA6n�A6ffA6M�A6$�A5��A5C�A5?}A5`BA5p�A5�PA6{A6^5A7
=A77LA7��A7��A7A7��A7��A7A7A7�hA7�7A7S�A6�`A6�+A65?A5�TA5�hA5XA5?}A5+A5�A5oA5VG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                      ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B�EB��B�EB�EB��B�B�EB��BרB��B�BخB�yB�EB��B�yB�EBخB�EB��BخB�B�EB��B��B�sBרB�?B�?B�sBרB��B�mB��B��B՛B��B��B�sB�B��B� B�KB	B	*�B	1�B	RTB	��B	�2B
�B
��B
��B
��B
��B
��B
{JB
O�B
C�B
B�B
C�B
OB
h�B
��B
��B
��B
k�B
P�B
6�B
1�B
 'B	��B	��B	��B	��B	�uB	��B	wfB	k�B	d&B	\�B	C�B	5�B	+B	�B��B� B�B��BϫB��B�B��B�B�B�B�B�qBچB	B	-wB	8�B	X�B	y>B	v�B	l�B	j�B	sB	��B	��B	�^B	уB	�B	�dB	�;B	��B	�hB	�qB	q�B	gmB	_B	iB	�B	�OB	��B	��B	��B	�B	�mB	�B	�B	��B	}�B	WsB	T,B	P�B	GzB	+B	�B��B�]B��B��B	 �B	YB	�B	�B	.B	1'B	3hB	<�B	PB	WsB	aB	h�B	o5B	oiB	o�B	r�B	q�B	qAB	p�B	tB	w�B	y�B	z�B	y�B	t�B	oiB	j�B	p�B	m]B	h�B	VB	A�B	?�B	;�B	5?B	oB	:B	$B	�B	�B	\B	�B	4B	�B	7B	,�B	;�B	>BB	=<B	=qB	6zB	9$B	<�B	>�B	;�B	@OB	FtB	H�B	R B	aB	a|B	`�B	f2B	m�B	ncB	r�B	t�B	v�B	x�B	zDB	|PB	~(B	~�B	�AB	��B	�YB	��B	�B	�DB	��B	�DB	�B	��B	�B	�DB	�~B	��B	�B	��B	��B	�VB	� B	��B	�\B	��B	��B	��B	��B	�uB	�_B	��B	�~B	�:B	��B	��B	��B	��B	��B	�B	��B	�tB	�B	�zB	�XB	�XB	��B	�B	�RB	�=B	��B	�eB	��B	��B	�B	��B	��B	��B	��B	��B	�qB	��B	�[B	��B	�B	��B	��B	��B	�jB	�6B	��B	�qB	�B	�wB	��B	�[B	ƨB	�dB	̘B	�B	��B	�)B	уB	��B	�KB	�?B	�2B	՛B	֡B	�B	خB	�B	�KB	خB	�B	�EB	�mB	�TB	ΥB	�dB	��B	ȀB	�-B	ÖB	�gB	ɆB	��B	�9B	�RB	��B	ɆB	��B	˒B	��B	ϫB	�jB	�jB	��B	�^B	˒B	�0B	̘B	�B	�<B	��B	ϫB	уB	�,B	�]B	��B	�B	��B	�"B	�WB	��B	�B	�B	�B	�B	�vB	�|B	�GB	��B	��B	�lB
  B
 iB
B
 iB
 �B
 4B
  B	��B	��B
�B
�B
oB
B
uB
�B
�B
�B
B
GB
B
B
B
GB
{B
B
%B
�B
_B
	B

	B

	B

	B

�B
B
PB
PB
PB
�B
�B
�B
�B
�B
�B
"B
�B
�B
"B
�B
"B
VB
(B
�B
�B
�B
�B
\B
\B
�B
.B
�B
�B
bB
bB
�B
�B
�B
�B
�B
FB
MB
�B
�B
�B
�B
eB
eB
�B
�B
7B
�B
=B
	B
qB
�B
B
B
�B
IB
IB
B
B
�B
!B
VB
VB
 'B
 'B
 \B
!B
�B
 �B
"4B
#B
"�B
#:B
#�B
%FB
%�B
%�B
%�B
%�B
%�B
%�B
'�B
'�B
(�B
(�B
)*B
)*B
)*B
(�B
)�B
*0B
)�B
*0B
*�B
*�B
+B
+6B
+�B
,B
-B
-B
-CB
-CB
-�B
.}B
.}B
.�B
/�B
/OB
/�B
/�B
0UB
0!B
0UB
0�B
1'B
2�B
2�B
33B
3�B
49B
5tB
6B
6FB
6FB
6FB
6zB
6�B
7B
8B
8�B
8�B
9�B
9�B
9�B
9�B
9�B
:*B
:^B
:�B
:�B
;�B
;�B
<B
<6B
<6B
<jB
<�B
=B
=�B
=�B
>BB
>wB
>BB
>�B
>�B
?B
?B
?�B
@B
@B
@�B
@�B
A�B
A�B
A�B
B[B
B�B
C-B
C�B
CaB
C�B
C�B
D�B
E9B
EmB
EmB
E�B
FtB
F�B
F�B
F�B
F�B
GEB
G�B
G�B
GzB
G�B
HB
HB
HB
HB
HB
HB
G�B
HKB
H�B
IB
I�B
I�B
I�B
JXB
JXB
J�B
K^B
L0B
K�B
LdB
L�B
L�B
L�B
L�B
L�B
L�B
M�B
NB
NB
NB
NB
N�B
OB
OB
OBB
OvB
PHB
PHB
PHB
P}B
P�B
P}B
P�B
QB
R B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
R�B
RTB
S�B
S&B
S&B
S[B
S[B
S[B
S&B
S�B
T�B
TaB
TaB
UgB
VB
V9B
V�B
W?B
W
B
WsB
W�B
YB
YB
ZB
ZQB
Z�B
Z�B
Z�B
[WB
[WB
[�B
[�B
[WB
\)B
]/B
\�B
\�B
\�B
]dB
^B
]�B
]�B
^jB
^�B
^�B
^�B
_B
_B
_pB
`BB
`�B
aHB
a|B
a|B
a|B
a|B
a�B
bNB
b�B
c�B
c�B
d&B
c�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e`B
e�B
f�B
g�B
g�B
g�B
g�B
gmB
g�B
h
B
h
B
iDB
iB
iDB
iDB
iDB
i�B
i�B
i�B
jB
jB
jB
i�B
jKB
j�B
kB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
m)B
m]B
m�B
m�B
m�B
n/B
m�B
n/B
n/B
n/B
ncB
n�B
oiB
o5B
o�B
o�B
o�B
pB
poB
p�B
qB
qAB
qvB
qvB
qvB
q�B
rB
r|B
rB
rB
q�B
r|B
r�B
r�B
sMB
s�B
s�B
tB
tTB
t�B
t�B
t�B
t�B
uZB
u�B
tB
u�B
u�B
v`B
v+B
v+B
v�B
v�B
w2B
wfB
w2B
wfB
w�B
w�B
xlB
xlB
x�B
yrB
yrB
yrB
y�B
zB
zxB
zxB
z�B
z�B
z�B
z�B
{JB
{B
{�B
{�B
{�B
{�B
{�B
{�B
|B
|PB
|�B
}"B
}"B
}�B
}�B
}�B
}�B
~]B
~�B
.B
cB
.B
�B
� B
� B
�iB
�4B
�4B
�iB
�B
��B
��B
�B
�;B
�;B
�oB
��B
��B
��B
�AB
�B
��B
�B
�B
�{B
��B
��B
��B
��B
�B
��B
��B
�MB
��B
��B
��B
��B
�B
�B
�B
�SB
��B
��B
�%B
�YB
��B
��B
��B
�+B
�+B
��B
��B
��B
�1B
�fB
��B
��B
��B
��B
�7B
�7B
��B
�	B
�	B
�	B
�	B
�=B
�	B
�	B
�rB
�rB
�rB
��B
�B
�xB
�xB
��B
��B
��B
��B
��B
��B
�~B
��B
�~B
�~B
�B
�PB
��B
��B
��B
��B
�"B
��B
��B
��B
�\B
�\B
��B
��B
��B
��B
��B
��B
��B
��B
�bB
��B
�4B
�4B
� B
�4B
��B
��B
��B
�B
�oB
��B
�B
�@B
�@B
��B
�B
�B
�B
�B
�FB
�FB
�B
�FB
�{B
��B
��B
��B
��B
��B
�{B
��B
��B
��B
�B
�B
�MB
��B
��B
��B
��B
��B
��B
��B
�YB
��B
��B
��B
��B
��B
��B
��B
��B
�eB
��B
��B
��B
�B
�kB
�kB
��B
�	B
�	B
�	B
�	B
�	B
�	B
�	B
�=B
�=B
��B
�B
�B
�B
�CB
�=B�BٴB��B�B�yB�B�BیB�B�B��BچB�mB�B��B�?B�EB�yB�9BרB��B�KB�BخB�sBרBٴB�
B�EB�yB��B�B�BخB�B�EB�sB�BרB�yB��B�EB��B�BخB�yBרB�sB��B�sB�B��BخB�B�B�sB��B��B��BخB��B�B�KBخB�?B�?B�sB�EB��B�KBٴB�KB�EB�EB�B�B��BخBخBخB�B�BخB�B�B��B�B�sB��B�B��B�B�B�yBخB��B�EBרB�B�B�yBخB�B�KB�B��B�EB�EB�EB�sB�B��B�EB�EB�B��B��B�B��B��B�EBרB�EB��B�B�KBچB�KB�BخB�B��B��B�BٴBٴB�KB�KB�yB�KB�BרB�B�EB�B�B�yB�?B�EB�sB��B�sB�yB��B�B�
B�EB�mB�mB�B��B�gB�B�?BخB՛B�?B�,B�9B�yB�sB��B�
B�QB�mB�B՛B��B�BٴB��B��B�
B��B֡B��B�
B�sB�EBרB��BרB�?B֡B��B֡B��B�B�sB�B�B��B֡B֡B֡BרB��B�
B�9B�9B�B��B�
B�?B֡B��B�B�B֡B֡B�9B՛B��B�gB��B֡B֡B՛B�gB��B՛B�B�9B�9B՛B��B��B�gB�9B֡B�mB�2B�2B�B�mB�?B�
B֡B�mB��B�EB�?B�mB��B��B�EB�B�
B�
B��BخB�B��B�yB�BخB��B��B��B�B�KB�]B��B�,B��B� B�B��B�B�yB�QB�QB��B�B��B�B�fB	{B	CB	&LB	&�B	%zB	$�B	(XB	$�B	.IB	,qB	!�B	�B	(�B	]�B	>B	)_B	2�B	/�B	*eB	)�B	6B	,�B	HB	L0B	Q�B	M�B	TaB	\]B	^B	\�B	yrB	�oB	�xB	��B	�HB	��B	�kB	��B	�tB	�HB	یB	�B
�B
IB
x8B
jKB
�mB
{B
��B
ɺB
��B
��B
��B
��B
�B
��B
�LB
��B
�@B
�VB
�[B
��B
��B
�B
�qB
��B
�_B
��B
��B
�B
�B
�-B
�B
��B
�}B
��B
�=B
�CB
��B
��B
��B
�*B
��B
��B
��B
��B
�@B
�@B
��B
��B
��B
�YB
��B
��B
� B
�(B
�.B
�oB
��B
��B
�VB
��B
p;B
pB
VB
W�B
[�B
m�B
W�B
R�B
P�B
S&B
R B
O�B
L0B
I�B
L�B
S�B
J�B
JXB
NB
H�B
H�B
W�B
N<B
=B
9�B
:*B
8�B
:*B
:*B
:�B
=qB
=�B
>wB
?�B
@OB
B'B
CaB
J#B
MB
HKB
H�B
MjB
FB
D3B
?}B
?HB
B�B
F?B
A�B
C�B
>B
A�B
;�B
9XB
=B
I�B
MB
Q�B
[#B
bNB
a|B
g8B
`�B
`B
b�B
d&B
b�B
bNB
c�B
v`B
q�B
tB
rB
�{B
��B
�=B
�	B
��B
��B
��B
��B
�B
��B
��B
�FB
�?B
�B
�B
�B
��B
��B
�nB
�-B
�-B
�B
��B
��B
�\B
�B
��B
�7B
�eB
��B
��B
��B
}�B
��B
�B
w�B
k�B
u�B
kQB
e,B
hsB
d�B
d&B
^B
\�B
]�B
\�B
ZQB
T�B
U�B
OBB
O�B
NpB
NpB
N�B
OBB
D�B
@B
D�B
;�B
:�B
:^B
EmB
/�B
.�B
2�B
1'B
+�B
)_B
6B
.B
?�B
4�B
3�B
2aB
1�B
-CB
-CB
2aB
-�B
)_B
.�B
(XB
)_B
$B
!�B
B
�B
YB
�B
�B
OB
OB
�B
�B	��B	�lB	��B
	�B	��B	�B	�
B	�&B	��B	�B	�B	ޞB	�#B	�B	��B	��B	��B	��B	�B	��B	�9B	�&B	��B	�2B	�gB	��B	�B	��B	�RB	��B	��B	�9B	��B	��B	�FB	��B	��B	�B	�hB	�B	��B	��B	� B	�(B	�uB	��B	��B	�\B	��B	�B	��B	��B	��B	z�B	x8B	x�B	y�B	v�B	v�B	w�B	yrB	{JB	|PB	w�B	uZB	wfB	u�B	rB	rGB	p�B	p;B	m�B	jKB	h�B	jB	i�B	e�B	f2B	d�B	iyB	k�B	h
B	kQB	e�B	^jB	YKB	WsB	WsB	WsB	^�B	_pB	\�B	WsB	\�B	jKB	ZQB	[WB	L0B	E9B	C-B	<B	4�B	4nB	5tB	8�B	7�B	8�B	7�B	5B	4�B	1�B	1[B	/�B	0UB	/�B	2-B	,qB	+�B	$@B	#nB	#�B	B	�B		�B	�B�cB	�B�VB	 4B	�B	 4B�vB�GB��B��B��B�]B��B�vB�]B��B�ZB��B�2B�B��B�vB�HB�pB�B��BݘB�jB�]B�B��B�NB��B��B�QB� B�B�[B�}B�B�0BΥB��B�B�dB�)B��B�XB�XBɺB�B�^B�RB�aB�B�^B�B��B�pBϫB��B̘B�B�dB͟B�BуB�pB�<BΥBΥB�B�#B˒B�HB��B�6BȀB��B��B��B� B�BɆB�B��B��B�OBɺB�0BɺB�HB��B�OB��B��B��B�?B�nB�tB��B��B�[B��B�aB�gBƨB�gB��B��B��B	�B	
rB�B	�B	xB	�B	B	�B	$tB	+6B	,�B	,B	-B	-wB	,B	1[B	2�B	7�B	A�B	6zB	49B	5tB	8�B	6B	K^B	<�B	\)B	`�B	iB	jKB	dZB	s�B	tTB	l�B	z�B	��B	yrB	��B	}VB	}"B	wfB	y�B	v`B	q�B	p;B	n�B	m)B	n/B	k�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202305012135302023050121353020230501213530202305012135302023050121353020230501213530SI  SI  ARFMARFM                                                                                                                                                2021022310203220210223102032IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021030510003320210305100033QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021030510003320210305100033QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2021042714014420210427140144IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI      ARSQ    SIQC    V2.1                                                                                                                                    20220504162929              CF      PSAL                            ?�=qG�O�D�y�G�O�?�  G�O�Sensor Failure                      SI      ARSQ    SIQC    V2.1                                                                                                                                              20220504163440    CF                  PSAL            G�O�?8Q�G�O�CG�G�O�?�                  Sensor Failure  SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023050121353420230501213534IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                