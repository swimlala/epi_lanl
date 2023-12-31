CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-06-02T07:20:36Z creation; 2023-04-26T19:14:26Z DMQC;      
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20180602072036  20230426191426  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7316_008644_014                 7316_008644_014                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�gM:��p@�gM:��p11  @�gMk��~@�gMk��~@*}��|�@*}��|��d-D�)���d-D�)��11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u?��H@B�\@��\@�  @��R@�  A ��AG�A ��A,(�A?\)A`  A\)A�Q�A�  A��A��A�  A�  A�  B (�BQ�B(�B  B�
B(  B0  B7�
B@(�BH(�BP  BW�
B`  Bh  Bo�
Bx  B�{B�  B�  B�  B�  B���B�  B��B�B��B��B��B�{B�  B�{B�{B�{B�  B�  B�{B�  B��B��B�  B�  B��B��B�  B�  B��B�  B�{C   C��C��C
=C{C

=C��C�HC�C  C��C�C��C��C�C  C 
=C"
=C#��C%��C(
=C*
=C,
=C.  C0  C1��C3�C6  C8
=C9�C;��C>  C@  CB  CC��CE��CG�CI��CK��CM��CP  CR
=CT{CV  CX
=CZ
=C\  C]��C`  Cb  Cd
=Cf
=Ch  Cj  Ck�Cm��Co��Cr  Ct  Cv
=Cx
=Cz
=C|
=C}��C��C�C�C���C���C�  C�C�
=C�C�C�
=C�C�  C�C�C�  C�  C���C���C���C�  C�C�  C���C�  C�  C���C���C�  C�  C�  C�C�C�C�  C�  C�C�
=C�C�  C�C�C�  C���C���C���C�  C�  C���C���C�  C�  C�  C�
=C�C�  C�  C�  C���C�  C���C���C�  C�C�C�C�C�  C���C���C���C���C�C�C�  C�  C�C���C�  C�C�  C�  C���C���C���C���C���C���C���C�  C�  C�  C�C�C�  C�  C�  C�  C�  C�  C�C�  C�C�C�  C���C���C�C�C�  C���C���C�  C�  C�  C���C�  C�C�  C�  C�  C�C�  C���C���C�  C�C�  C���D � D  D� D�D�DD�D�D��D  D� D�D�D�D��D�D� D��D	}qD
  D
� D  D}qD��D� D�D��D  D}qD��D� D  D� D�qD}qD  D� D�D� D  D� D��D}qD�qD� DD��DD��D�D��D  D� D�D}qD  D� D�qD}qD�qD}qD  D}qD��D � D!�D!��D"�D"� D#  D#��D#�qD$}qD%�D%� D%�qD&� D'�D'}qD'��D(}qD)  D)��D*D*�D*�qD+}qD,�D,� D,�qD-z�D-��D.� D/D/�D0D0� D0��D1z�D1��D2}qD2�qD3}qD4  D4� D5  D5��D6  D6}qD7  D7��D8  D8z�D9  D9}qD:  D:��D;  D;��D<  D<}qD=  D=��D>  D>}qD?  D?��D@D@� D@��DA� DB�DB� DB�qDC� DD�DD� DD�qDE� DF�DF��DGDG�DHDH� DH�qDI}qDI��DJ}qDK  DK}qDK��DL}qDM  DM��DN�DN� DO�DO��DP  DP� DQDQ� DQ�qDR� DS  DS}qDS��DT}qDU�DU��DV  DV}qDW  DW� DX  DX� DX�qDY}qDZ  DZ��D[  D[}qD\  D\��D]�D]�D^�D^� D^�qD_� D`  D`� Da  Da}qDb  Db� Db�qDc}qDd  Dd� Dd�qDe}qDe�qDf}qDg�Dg�DhDh��Di  Di� Dj  Dj� Dk  Dk� Dl�Dl� Dm  Dm}qDn  Dn}qDn�qDo� Dp  Dp��Dq�Dq}qDq��Dr� Ds  Ds� Dt  Dt��Du�Du��Dv  Dv}qDv�qDw}qDx  Dx}qDy  Dy��Dz  Dz}qD{  D{��D|�D|��D}�D}��D~  D~}qD  D��D�  D�>�D�~�D���D�  D�>�D�� D�� D�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�>�D�~�D�� D���D�>�D�� D��HD�  D�>�D�~�D���D���D�@ D��HD�� D���D�@ D���D��HD�  D�@ D�� D��qD���D�@ D�~�D���D���D�>�D�~�D�� D�  D�@ D�~�D��)D��qD�@ D���D��HD�  D�@ D��HD��HD�HD�B�D���D��HD�  D�AHD��HD��HD�  D�>�D�~�D�� D���D�>�D�� D�D�HD�>�D�}qD���D�  D�@ D�~�D�� D�  D�@ D���D��HD�  D�@ D�~�D��HD�  D�@ D�� D�� D��D�AHD�~�D���D�  D�>�D��HD�� D�  D�AHD��HD���D���D�@ D�� D�� D�HD�AHD�� D�� D�HD�@ D�~�D�� D��D�AHD�� D��HD��D�AHD�� D�� D���D�>�D�� D��HD�  D�>�D�~�D��qD�  D�AHD��HD�D�HD�@ D�� D��HD�HD�AHD��HD�� D�HD�AHD��HD��HD��D�B�D�~�D�� D��D�AHD�~�D�� D�  D�AHD�� D���D�  D�AHD�� D���D�  D�>�D�~�D�� D�  D�>�D�� D�� D�  D�AHD�~�D���D�HD�AHD���D�D���D�@ D�� D�� D�HD�>�D��HD�D�HD�@ D�}qD���D�  D�>�D��HD�D�HD�@ D�~�D�� D��D�AHD�� D���D�  D�>�D�~�D���D�HD�@ D�� D�� D���D�@ D�� D��qD���D�>�D�� D��HD�HD�B�D���D�� D�  D�@ D��HD��HD���D�@ D�� D���D�  D�@ D�� D�� D�HD�AHD�~�D��HD�HD�AHD�� D��HD��D�AHD�� D���D�  D�AHD�� D�� D�  D�>�D�~�D���D���D�@ D��HD�� D���D�@ DHD��HD�HD�AHDÀ Dþ�D���D�@ DāHD�D�  D�AHDŀ D��HD�HD�AHDƁHDƾ�D��qD�>�D�~�D�� D�HD�@ DȀ D�� D�HD�AHD�~�D�� D�HD�@ DʁHD��HD�  D�@ DˁHD˾�D���D�AHD́HD��HD�  D�>�D̀ D��HD���D�>�D΀ D�� D�  D�@ Dς�D�� D�HD�AHD�~�D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D���D�@ DӀ D�� D���D�@ DԀ D�D�HD�>�D�~�Dվ�D�  D�AHD�~�D�� D�HD�AHD׀ D׾�D��qD�>�D؁HD�� D���D�@ DفHD��HD�HD�@ Dڀ D�� D��qD�=qDۀ D�D��D�B�D܁HD��HD�HD�B�D݂�D�D�HD�AHDނ�D��HD�  D�>�D�~�D�� D�  D�>�D�~�DྸD�HD�>�D� D�� D��qD�AHD�HD��HD�HD�@ D� D�� D�  D�B�D䂏D侸D���D�AHD�HD徸D���D�AHD�HD澸D���D�@ D� D�� D�  D�@ D�HD��HD�HD�AHD�~�D龸D�HD�AHD�HD��HD���D�>�D�HD��HD���D�=qD� D�� D�  D�>�D�~�D���D�  D�@ D� D��HD�HD�@ D� D�� D�  D�@ D�� D��HD�HD�@ D� D�� D�  D�@ D� D�� D�HD�AHD� D�� D�HD�AHD� D��HD�HD�@ D�� D�� D�HD�@ D�� D���D���D�@ D�~�D���D�  D�B�D��HD�� D�HD�@ D�~�D���D�  D�4{D�w
>�G�>�?#�
?��?�33?�G�@   @
=@5@G�@^�R@z�H@�=q@�@��\@���@�p�@Ǯ@�33@�  @���@���A�A�A{A�A=qA   A'
=A.{A3�
A:=qA@��AG�AN{AS33AZ=qA`��AfffAl(�Aq�AxQ�A~�RA��A���A�  A��HA�p�A��A�=qA�z�A��RA���A��HA���A�ffA�Q�A��\A���A�ffA�Q�A�=qA�z�A�{A�  A�=qA�(�A�A�Q�A�=qA�(�A�A��A�=qA�(�A�{A�  Aʏ\A���A�ffA�Q�A��HA���A�ffA�Q�Aڏ\A���A޸RA�Q�A�\A�z�A�ffA�  A��A�z�A�RA�Q�A�=qA�z�A�
=A���A��\A���A�
=B z�Bp�BffB�B��B��B�\B�
B��B	B
�RB  B�B�B
=B(�B�B{B\)B(�BG�BffB�Bz�Bp�B�\B�B��B��B�HB�
B ��B!�B#
=B$(�B$��B&{B'\)B(Q�B)G�B*ffB+�B,z�B-p�B.�\B/�
B0��B1B2�RB4  B5�B5�B7
=B8Q�B9G�B:=qB;33B<z�B=��B>ffB?\)B@��BABB�RBC�BD��BF{BG33BH  BIG�BJffBK�BLz�BM��BN�HBO�
BP��BQ�BS
=BT(�BT��BV{BW\)BXz�BYp�BZffB[�B\��B]B^�\B_�B`��Ba�Bb�RBd  BeG�Bf=qBg
=Bh(�BiG�Bj�\Bk�Blz�Bmp�Bn�RBo�
Bq�Bq�Br�HBt(�BuG�Bv=qBw33Bx(�Byp�Bz�\B{�B|Q�B}G�B~ffB�B�=qB���B�33B�B�=qB���B��B��B�=qB���B�
=B���B�{B���B�
=B�p�B�  B��\B�
=B�p�B��
B�Q�B���B�p�B��B�Q�B���B�p�B��B�ffB��HB�\)B��
B�ffB���B��B��B�Q�B���B�G�B��B�  B�=qB��\B��HB��B�33B�G�B�\)B�p�B��B��
B��
B��B�  B�(�B�ffB�z�B���B��RB���B���B��B�G�B�p�B���B��B�B��B�{B�=qB�ffB��\B���B��RB���B�
=B�33B�\)B�p�B��B���B�B��B�{B�=qB�=qB�Q�B�z�B��RB��HB���B�
=B�
=B��B�\)B��B��B�B��
B��
B�  B�(�B�ffB��\B��\B���B��RB���B��B�G�B�\)B�p�B��B�B��B�{B�(�B�=qB�ffB��\B��RB���B��B�33B�G�B�p�B���B��B�{B�=qB�ffB�z�B��RB���B�G�B��B��B��B�{B�ffB��\B��HB�33B�p�B�B�  B�=qB�z�B��RB���B�G�B���B��B�=qB��\B���B�
=B�G�B��B��
B�(�B�ffB��RB�
=B�\)B��B�  B�Q�B���B��HB�33B�p�B�B�{B�ffB��RB��B��B��
B�(�B��\B��HB�33B���B��B�Q�B���B��B�p�B�B�(�B��\B��HB�G�B���B��B�=qB��\B���B�\)B��B�{B�z�B���B�33B���B�  B�ffB���B��B��B��
B�=qB��\B��HB�G�B���B�  B�ffB���B�33B���B�  B�ffB���B�33B���B��B�Q�B���B���B�\)B�B�(�B\B���B�\)B�B�(�Bď\B���B�p�B��
B�Q�BƸRB�33BǙ�B�{B�z�B�
=B�p�B��B�ffB���B�G�B�B�=qḄ�B�33B͙�B�{BΣ�B�
=BυB�{BЏ\B�
=Bљ�B�{Bҏ\B��Bә�B�(�Bԣ�B�33BծB�(�BָRB�33B׮B�=qBظRB�33BٮB�=qB���B�G�B��
B�Q�B��HB�\)B�  B�z�B�
=B߅B�{B��B��B�B�(�B�RB�33B�B�=qB�RB�33B�B�=qB�RB�G�B��
B�ffB���B陚B�(�B�RB�33B�B�Q�B���B�\)B��
B�ffB���B�B�(�B�RB�G�B��
B�ffB���B�p�B�{B���B�33B��
B�ffB�
=B���B�=qB���B�p�B�  B�z�B��B��B�Q�B��HB��B�(�B���B�p�C 
=C Q�C ��C ��C=qC�C��C�Cp�C�RC{C\)C��C�HC(�Cp�C�RC
=CQ�C��C�C33Cz�C�RC  CG�C�\C�C(�Cp�C�RC	  C	=qC	�C	�
C
{C
ffC
�RC  C=qC�C��C{CffC�C  C=qC�C��C{CffC�C  CG�C��C�
C{CffC�C
=C\)C��C�C=qC�CC{CffC�RC  CQ�C�\C�
C�CffC�RC  CQ�C��C�
C�Cp�CC
=CQ�C��C�HC=qC�C�
C{C\)C�C  CQ�C�\C�
C�Cp�C��C
=CQ�C��C��C=qC�C��C{CffC�RC   C =qC �\C �HC!33C!p�C!�RC"  C"\)C"�C"��C#33C#�\C#�HC$=qC$�C$��C%�C%z�C%�
C&�C&ffC&C'{C'ffC'�C(  C(\)C(�C)  C)G�C)��C*  C*Q�C*��C*��C+G�C+�C,  C,Q�C,��C-  C-\)C-�C-��C.\)C.�RC/{C/ffC/�C0{C0ffC0C1
=C1p�C1�
C2(�C2z�C2�
C3=qC3��C3�C4G�C4�C5
=C5ffC5�RC6{C6z�C6�
C7=qC7�\C7�HC8G�C8�C9
=C9\)C9�RC:�C:�C:�HC;33C;�C;��C<\)C<�RC=
=C=ffC=�
C>33C>�\C>��C?\)C?C@{C@p�C@�
CA=qCA��CB  CBQ�CB�RCC(�CC�CC�
CD=qCD��CE  CEQ�CECF(�CF�CF�HCG=qCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                             ?u?��H@B�\@��\@�  @��R@�  A ��AG�A ��A,(�A?\)A`  A\)A�Q�A�  A��A��A�  A�  A�  B (�BQ�B(�B  B�
B(  B0  B7�
B@(�BH(�BP  BW�
B`  Bh  Bo�
Bx  B�{B�  B�  B�  B�  B���B�  B��B�B��B��B��B�{B�  B�{B�{B�{B�  B�  B�{B�  B��B��B�  B�  B��B��B�  B�  B��B�  B�{C   C��C��C
=C{C

=C��C�HC�C  C��C�C��C��C�C  C 
=C"
=C#��C%��C(
=C*
=C,
=C.  C0  C1��C3�C6  C8
=C9�C;��C>  C@  CB  CC��CE��CG�CI��CK��CM��CP  CR
=CT{CV  CX
=CZ
=C\  C]��C`  Cb  Cd
=Cf
=Ch  Cj  Ck�Cm��Co��Cr  Ct  Cv
=Cx
=Cz
=C|
=C}��C��C�C�C���C���C�  C�C�
=C�C�C�
=C�C�  C�C�C�  C�  C���C���C���C�  C�C�  C���C�  C�  C���C���C�  C�  C�  C�C�C�C�  C�  C�C�
=C�C�  C�C�C�  C���C���C���C�  C�  C���C���C�  C�  C�  C�
=C�C�  C�  C�  C���C�  C���C���C�  C�C�C�C�C�  C���C���C���C���C�C�C�  C�  C�C���C�  C�C�  C�  C���C���C���C���C���C���C���C�  C�  C�  C�C�C�  C�  C�  C�  C�  C�  C�C�  C�C�C�  C���C���C�C�C�  C���C���C�  C�  C�  C���C�  C�C�  C�  C�  C�C�  C���C���C�  C�C�  C���D � D  D� D�D�DD�D�D��D  D� D�D�D�D��D�D� D��D	}qD
  D
� D  D}qD��D� D�D��D  D}qD��D� D  D� D�qD}qD  D� D�D� D  D� D��D}qD�qD� DD��DD��D�D��D  D� D�D}qD  D� D�qD}qD�qD}qD  D}qD��D � D!�D!��D"�D"� D#  D#��D#�qD$}qD%�D%� D%�qD&� D'�D'}qD'��D(}qD)  D)��D*D*�D*�qD+}qD,�D,� D,�qD-z�D-��D.� D/D/�D0D0� D0��D1z�D1��D2}qD2�qD3}qD4  D4� D5  D5��D6  D6}qD7  D7��D8  D8z�D9  D9}qD:  D:��D;  D;��D<  D<}qD=  D=��D>  D>}qD?  D?��D@D@� D@��DA� DB�DB� DB�qDC� DD�DD� DD�qDE� DF�DF��DGDG�DHDH� DH�qDI}qDI��DJ}qDK  DK}qDK��DL}qDM  DM��DN�DN� DO�DO��DP  DP� DQDQ� DQ�qDR� DS  DS}qDS��DT}qDU�DU��DV  DV}qDW  DW� DX  DX� DX�qDY}qDZ  DZ��D[  D[}qD\  D\��D]�D]�D^�D^� D^�qD_� D`  D`� Da  Da}qDb  Db� Db�qDc}qDd  Dd� Dd�qDe}qDe�qDf}qDg�Dg�DhDh��Di  Di� Dj  Dj� Dk  Dk� Dl�Dl� Dm  Dm}qDn  Dn}qDn�qDo� Dp  Dp��Dq�Dq}qDq��Dr� Ds  Ds� Dt  Dt��Du�Du��Dv  Dv}qDv�qDw}qDx  Dx}qDy  Dy��Dz  Dz}qD{  D{��D|�D|��D}�D}��D~  D~}qD  D��D�  D�>�D�~�D���D�  D�>�D�� D�� D�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�>�D�~�D�� D���D�>�D�� D��HD�  D�>�D�~�D���D���D�@ D��HD�� D���D�@ D���D��HD�  D�@ D�� D��qD���D�@ D�~�D���D���D�>�D�~�D�� D�  D�@ D�~�D��)D��qD�@ D���D��HD�  D�@ D��HD��HD�HD�B�D���D��HD�  D�AHD��HD��HD�  D�>�D�~�D�� D���D�>�D�� D�D�HD�>�D�}qD���D�  D�@ D�~�D�� D�  D�@ D���D��HD�  D�@ D�~�D��HD�  D�@ D�� D�� D��D�AHD�~�D���D�  D�>�D��HD�� D�  D�AHD��HD���D���D�@ D�� D�� D�HD�AHD�� D�� D�HD�@ D�~�D�� D��D�AHD�� D��HD��D�AHD�� D�� D���D�>�D�� D��HD�  D�>�D�~�D��qD�  D�AHD��HD�D�HD�@ D�� D��HD�HD�AHD��HD�� D�HD�AHD��HD��HD��D�B�D�~�D�� D��D�AHD�~�D�� D�  D�AHD�� D���D�  D�AHD�� D���D�  D�>�D�~�D�� D�  D�>�D�� D�� D�  D�AHD�~�D���D�HD�AHD���D�D���D�@ D�� D�� D�HD�>�D��HD�D�HD�@ D�}qD���D�  D�>�D��HD�D�HD�@ D�~�D�� D��D�AHD�� D���D�  D�>�D�~�D���D�HD�@ D�� D�� D���D�@ D�� D��qD���D�>�D�� D��HD�HD�B�D���D�� D�  D�@ D��HD��HD���D�@ D�� D���D�  D�@ D�� D�� D�HD�AHD�~�D��HD�HD�AHD�� D��HD��D�AHD�� D���D�  D�AHD�� D�� D�  D�>�D�~�D���D���D�@ D��HD�� D���D�@ DHD��HD�HD�AHDÀ Dþ�D���D�@ DāHD�D�  D�AHDŀ D��HD�HD�AHDƁHDƾ�D��qD�>�D�~�D�� D�HD�@ DȀ D�� D�HD�AHD�~�D�� D�HD�@ DʁHD��HD�  D�@ DˁHD˾�D���D�AHD́HD��HD�  D�>�D̀ D��HD���D�>�D΀ D�� D�  D�@ Dς�D�� D�HD�AHD�~�D�� D�  D�@ Dр D�� D�  D�@ DҀ D�� D���D�@ DӀ D�� D���D�@ DԀ D�D�HD�>�D�~�Dվ�D�  D�AHD�~�D�� D�HD�AHD׀ D׾�D��qD�>�D؁HD�� D���D�@ DفHD��HD�HD�@ Dڀ D�� D��qD�=qDۀ D�D��D�B�D܁HD��HD�HD�B�D݂�D�D�HD�AHDނ�D��HD�  D�>�D�~�D�� D�  D�>�D�~�DྸD�HD�>�D� D�� D��qD�AHD�HD��HD�HD�@ D� D�� D�  D�B�D䂏D侸D���D�AHD�HD徸D���D�AHD�HD澸D���D�@ D� D�� D�  D�@ D�HD��HD�HD�AHD�~�D龸D�HD�AHD�HD��HD���D�>�D�HD��HD���D�=qD� D�� D�  D�>�D�~�D���D�  D�@ D� D��HD�HD�@ D� D�� D�  D�@ D�� D��HD�HD�@ D� D�� D�  D�@ D� D�� D�HD�AHD� D�� D�HD�AHD� D��HD�HD�@ D�� D�� D�HD�@ D�� D���D���D�@ D�~�D���D�  D�B�D��HD�� D�HD�@ D�~�D���D�  D�4{G�O�>�G�>�?#�
?��?�33?�G�@   @
=@5@G�@^�R@z�H@�=q@�@��\@���@�p�@Ǯ@�33@�  @���@���A�A�A{A�A=qA   A'
=A.{A3�
A:=qA@��AG�AN{AS33AZ=qA`��AfffAl(�Aq�AxQ�A~�RA��A���A�  A��HA�p�A��A�=qA�z�A��RA���A��HA���A�ffA�Q�A��\A���A�ffA�Q�A�=qA�z�A�{A�  A�=qA�(�A�A�Q�A�=qA�(�A�A��A�=qA�(�A�{A�  Aʏ\A���A�ffA�Q�A��HA���A�ffA�Q�Aڏ\A���A޸RA�Q�A�\A�z�A�ffA�  A��A�z�A�RA�Q�A�=qA�z�A�
=A���A��\A���A�
=B z�Bp�BffB�B��B��B�\B�
B��B	B
�RB  B�B�B
=B(�B�B{B\)B(�BG�BffB�Bz�Bp�B�\B�B��B��B�HB�
B ��B!�B#
=B$(�B$��B&{B'\)B(Q�B)G�B*ffB+�B,z�B-p�B.�\B/�
B0��B1B2�RB4  B5�B5�B7
=B8Q�B9G�B:=qB;33B<z�B=��B>ffB?\)B@��BABB�RBC�BD��BF{BG33BH  BIG�BJffBK�BLz�BM��BN�HBO�
BP��BQ�BS
=BT(�BT��BV{BW\)BXz�BYp�BZffB[�B\��B]B^�\B_�B`��Ba�Bb�RBd  BeG�Bf=qBg
=Bh(�BiG�Bj�\Bk�Blz�Bmp�Bn�RBo�
Bq�Bq�Br�HBt(�BuG�Bv=qBw33Bx(�Byp�Bz�\B{�B|Q�B}G�B~ffB�B�=qB���B�33B�B�=qB���B��B��B�=qB���B�
=B���B�{B���B�
=B�p�B�  B��\B�
=B�p�B��
B�Q�B���B�p�B��B�Q�B���B�p�B��B�ffB��HB�\)B��
B�ffB���B��B��B�Q�B���B�G�B��B�  B�=qB��\B��HB��B�33B�G�B�\)B�p�B��B��
B��
B��B�  B�(�B�ffB�z�B���B��RB���B���B��B�G�B�p�B���B��B�B��B�{B�=qB�ffB��\B���B��RB���B�
=B�33B�\)B�p�B��B���B�B��B�{B�=qB�=qB�Q�B�z�B��RB��HB���B�
=B�
=B��B�\)B��B��B�B��
B��
B�  B�(�B�ffB��\B��\B���B��RB���B��B�G�B�\)B�p�B��B�B��B�{B�(�B�=qB�ffB��\B��RB���B��B�33B�G�B�p�B���B��B�{B�=qB�ffB�z�B��RB���B�G�B��B��B��B�{B�ffB��\B��HB�33B�p�B�B�  B�=qB�z�B��RB���B�G�B���B��B�=qB��\B���B�
=B�G�B��B��
B�(�B�ffB��RB�
=B�\)B��B�  B�Q�B���B��HB�33B�p�B�B�{B�ffB��RB��B��B��
B�(�B��\B��HB�33B���B��B�Q�B���B��B�p�B�B�(�B��\B��HB�G�B���B��B�=qB��\B���B�\)B��B�{B�z�B���B�33B���B�  B�ffB���B��B��B��
B�=qB��\B��HB�G�B���B�  B�ffB���B�33B���B�  B�ffB���B�33B���B��B�Q�B���B���B�\)B�B�(�B\B���B�\)B�B�(�Bď\B���B�p�B��
B�Q�BƸRB�33BǙ�B�{B�z�B�
=B�p�B��B�ffB���B�G�B�B�=qḄ�B�33B͙�B�{BΣ�B�
=BυB�{BЏ\B�
=Bљ�B�{Bҏ\B��Bә�B�(�Bԣ�B�33BծB�(�BָRB�33B׮B�=qBظRB�33BٮB�=qB���B�G�B��
B�Q�B��HB�\)B�  B�z�B�
=B߅B�{B��B��B�B�(�B�RB�33B�B�=qB�RB�33B�B�=qB�RB�G�B��
B�ffB���B陚B�(�B�RB�33B�B�Q�B���B�\)B��
B�ffB���B�B�(�B�RB�G�B��
B�ffB���B�p�B�{B���B�33B��
B�ffB�
=B���B�=qB���B�p�B�  B�z�B��B��B�Q�B��HB��B�(�B���B�p�C 
=C Q�C ��C ��C=qC�C��C�Cp�C�RC{C\)C��C�HC(�Cp�C�RC
=CQ�C��C�C33Cz�C�RC  CG�C�\C�C(�Cp�C�RC	  C	=qC	�C	�
C
{C
ffC
�RC  C=qC�C��C{CffC�C  C=qC�C��C{CffC�C  CG�C��C�
C{CffC�C
=C\)C��C�C=qC�CC{CffC�RC  CQ�C�\C�
C�CffC�RC  CQ�C��C�
C�Cp�CC
=CQ�C��C�HC=qC�C�
C{C\)C�C  CQ�C�\C�
C�Cp�C��C
=CQ�C��C��C=qC�C��C{CffC�RC   C =qC �\C �HC!33C!p�C!�RC"  C"\)C"�C"��C#33C#�\C#�HC$=qC$�C$��C%�C%z�C%�
C&�C&ffC&C'{C'ffC'�C(  C(\)C(�C)  C)G�C)��C*  C*Q�C*��C*��C+G�C+�C,  C,Q�C,��C-  C-\)C-�C-��C.\)C.�RC/{C/ffC/�C0{C0ffC0C1
=C1p�C1�
C2(�C2z�C2�
C3=qC3��C3�C4G�C4�C5
=C5ffC5�RC6{C6z�C6�
C7=qC7�\C7�HC8G�C8�C9
=C9\)C9�RC:�C:�C:�HC;33C;�C;��C<\)C<�RC=
=C=ffC=�
C>33C>�\C>��C?\)C?C@{C@p�C@�
CA=qCA��CB  CBQ�CB�RCC(�CC�CC�
CD=qCD��CE  CEQ�CECF(�CF�CF�HCG=qCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                             @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�_G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�hsA�r�A�r�A�r�A�r�A�r�A�r�A�r�A�t�A�r�A�r�A�r�A�p�A�r�A�v�A�x�A�v�A�K�A�G�A�Q�A�/A��A�VA�oA�{A�bA�VA�VA�JA�JA�JA�
=A�%A�A�A�  A�  A���A۶FA�Q�AٓuAցA�oAω7A�C�A�\)A��A�O�A�ffA���A�5?A��A�33A�1'A�1'A�`BA�9XA�hsA���A���A���A��A��!A�jA���A�p�A�A��A�=qA��A��A�\)A���A��A�9XA��jA��A33Aw;dAt-Aol�Am��Ah��Ac�A]�AZ��AWhsAS�mAO��AMS�AK;dAG��AD�uA@��A?\)A>-A=�;A=�
A=p�A<=qA:(�A9�TA9��A9dZA9+A8�!A7��A5�A2$�A1K�A1A2�A1�mA2{A2JA1�A0�DA/��A/�A/+A.=qA-�A-+A,��A,��A,bNA,(�A+��A*ȴA)O�A'�;A'�7A'�^A&�jA%t�A#��A"�A"^5A"5?A!�mA!��A!��A"{A"ZA"ffA!�-A!�A ��A ��A �+A n�A�;Ax�AĜA  Ax�AoAM�A�FA�7A|�AK�A�yAr�A��A�AS�A"�A�9A�+AjA1A�-Al�A�A��A5?A�A�A�7A33A�A�A5?A1A�hA/A��AM�AM�A�HA��AĜA�TA�hAt�A��A�A;dA�A�+A�#AK�A��A�jA�An�AA�A{A�#AƨA�
A�AoA��AA�AJA�A��A��A7LA
�A	�
A	;dAĜA9XAI�A�A��AE�A��A��Al�A�A�yA�+A9XA�A�A�-A�yA��AVA$�A�7A�A9XA�A�PA ��A I�@�t�@�\)@�"�@�-@�/@��@�\)@���@��T@��7@��9@�
=@��@��@�@�x�@��@�K�@���@�M�@��@���@�^@�X@��`@��m@�\)@�@�V@��^@�G�@��@��@�n�@�@�x�@�z�@��;@畁@�l�@�5?@�@�&�@���@�j@�P@���@�V@�Z@�|�@ާ�@��T@�&�@�Q�@��m@�|�@ڰ!@ٲ-@��/@�ƨ@�+@��@���@�
=@ְ!@֗�@�ff@�V@�E�@ղ-@�`B@�&�@�r�@�C�@ҧ�@҇+@�ff@�$�@��@��#@�@љ�@�`B@�7L@���@�bN@�1@��@��;@Ϯ@�;d@���@Ώ\@�=q@͡�@�&�@���@̃@�I�@�1@ˮ@�33@��#@ɑh@�V@��@�\)@��@�@�hs@�V@ă@Õ�@�S�@�33@��@�ȴ@�@�ff@�@�?}@��j@��u@�9X@��@�  @��@��
@�t�@�o@�ȴ@��+@�n�@�V@�E�@�$�@��#@��h@��@��u@�I�@�1@��@�M�@�p�@���@��`@���@��@��@���@���@�C�@�@��!@���@�ff@���@���@�?}@���@��/@�Z@��@��m@��;@�ƨ@��P@�S�@��@��R@�n�@�V@��@�{@��@���@���@��P@���@���@��+@�n�@�$�@���@��h@��@�bN@�9X@��@��
@��@���@��#@���@�hs@�&�@��/@�Ĝ@�Ĝ@��9@�j@�(�@��F@�33@�o@��H@���@�^5@���@���@��@�Z@� �@��@��@�ff@��@��-@�O�@��`@��9@�r�@�Q�@�A�@�b@��m@���@��P@�C�@��@�-@��@�x�@���@���@�A�@��P@�K�@�;d@�"�@���@�n�@��@��^@��@�`B@�O�@�/@��`@��@�A�@��w@�+@��+@�5?@��#@���@��7@�G�@��`@�j@��@��;@��P@���@�~�@�V@��@��T@��-@�p�@���@���@��j@���@�Z@� �@���@�;d@�+@��@���@�v�@�5?@��-@�G�@�%@��j@��u@�Q�@��@�ƨ@���@��P@�|�@�\)@�+@�
=@�
=@���@��R@���@��+@�ff@�-@�J@��T@�p�@�/@���@��D@�1@��@�33@���@��!@�V@�-@���@��h@�hs@�/@���@���@�z�@�Z@�I�@�1'@� �@�1@���@��@�|�@�S�@�;d@�33@��H@��+@�n�@�V@�5?@�{@���@���@�X@��@��@���@��j@�Ĝ@�Ĝ@���@��D@�z�@�bN@�I�@�1@�w@+@~{@}��@}p�@|��@|1@z�@z-@yX@x��@xĜ@x��@xA�@w�@w�P@w�@v�R@vv�@v@u��@u�h@u?}@t�@tZ@t1@s�
@s�@s"�@r��@r~�@r�@r�@q�#@p�u@p  @o;d@n�R@m��@l�@lj@lZ@l1@kƨ@k��@k"�@j~�@i�@i7L@h�9@h1'@h  @g�@gl�@gK�@fv�@e�@e�h@e`B@d�@dZ@d�@d1@c�m@cdZ@cC�@c@bn�@b�@aX@`�9@`�u@`Q�@`  @_K�@^�R@^v�@^$�@]�-@]�@\�j@\j@\1@[t�@[33@["�@[@Z��@Z��@Z�\@Z-@Y��@YG�@X�9@Xb@W\)@Vȴ@Vff@VV@VE�@U@UO�@U/@U�@UV@T��@S��@SS�@R��@Rn�@Q��@Q��@Q��@Q��@Q�^@Q��@Q%@P�@PQ�@O�@O|�@OK�@N�@Nȴ@N��@N$�@M@Mp�@M/@L��@L�j@LI�@K�
@K��@K"�@J�H@J��@JM�@I��@I&�@H��@H�`@H��@H��@HQ�@H1'@G��@GK�@F�y@Fv�@F{@E�-@E�h@E?}@E/@D��@D�D@DI�@D�@C�@CS�@C33@Co@B��@B=q@B�@A��@A��@A&�@@�@@ �@?�;@?��@?l�@?K�@>�y@>�+@>ff@>E�@>{@=��@=��@=�@=p�@=`B@=?}@=/@<�@<��@<(�@;�
@;�F@;33@;o@:��@:��@:�\@:^5@9�#@9�7@97L@8Ĝ@8�9@8�@8A�@7�P@7K�@6��@6ff@65?@6@5��@5�@4�@4�j@4Z@49X@41@3�m@3��@3o@2��@2��@2��@2-@1��@1��@1��@1&�@0��@0bN@01'@/�@/l�@/
=@.�R@.�R@.�R@.�R@.�+@.V@.$�@-@-�h@-�@,�@,I�@,9X@,�@+��@+�m@+S�@*�H@*�!@*��@*n�@*^5@*^5@*M�@*=q@*-@)��@)�^@)G�@(�`@(�u@(Q�@(b@'�@'|�@'\)@'\)@'+@'�@&��@&ȴ@&��@&$�@%�@%�-@%�@%V@$�D@$1@#ƨ@#t�@#@"~�@"=q@"�@!�#@!��@!�^@!�^@!�#@!�7@ 1'@�@�@��@   @ b@�@K�@�@v�@E�@��@��@�@�@�@�j@�D@�@�
@��@t�@�@��@��@�\@�\@^5@^5@�@�7@7L@&�@&�@7L@G�@%@�9@��@r�@r�@Q�@1'@ �@  @b@ �@ �@ �@  @�;@��@K�@\)@�@�@��@v�@�T@�-@�h@�h@p�@?}@/@�@�@�@��@�/@�j@��@j@��@��@�m@ƨ@��@�@33@�@�H@��@��@�!@~�@n�@�@�#@�#@��@�^@��@�7@hs@Ĝ@�u@r�@bN@A�@  @�w@�A�XA�ZA�^5A�r�A�p�A�n�A�r�A�v�A�t�A�n�A�r�A�v�A�r�A�n�A�p�A�t�A�r�A�n�A�p�A�t�A�t�A�p�A�p�A�t�A�v�A�r�A�p�A�p�A�t�A�t�A�p�A�p�A�r�A�r�A�p�A�p�A�r�A�t�A�l�A�hsA�n�A�v�A�v�A�r�A�t�A�v�A�r�A�r�A�v�A�x�A�v�A�t�A�t�A�x�A�x�A�v�A�x�A�z�A�x�A�x�A�x�A�z�A�x�A�v�A�z�A�v�A�t�A�v�A�t�A�S�A�VA�7LA�C�A�C�A�A�A�C�A�;dA�C�A�;dA�9XA�VA�O�A�M�A�M�A�XA�Q�A�Q�A�O�A�S�A�VA�\)A�G�A�?}A�1'A�5?A�5?A�-A�$�A�1'A�-A�+A�&�A� �A��A��A�bA�VA�bA�VA�
=A�VA�oA�bA�VA�bA�oA�VA�VA�oA�{A�oA��A��A�{A��A��A�{A�bA�{A�{A�bA�VA�bA�oA�VA�VA�bA�VA�JA�
=A�VA�bA�JA�JA�bA�bA�JA�VA�bA�VA�
=A�JA�VA�bA�JA�
=A�JA�VA�JA�1A�VA�VA�JA�
=A�JA�VA�JA�1A�JA�VA�JA�1A�
=A�VA�VA�
=A�1A�JA�VA�1A�1A�
=A�1A�A�A�1A�1A�%A�A�A�%A�%A�A�A�%A�A�A�A�A�%A�A���A�A�A�A���A���A���A�A�A���A���A�  A�A�  A���A���A�  A�A�A���A���A�  A�A�  A���A���A���A���A��;A���A�ƨAۼjA۲-Aۧ�A۟�Aۛ�A۝�Aۗ�A�~�A�x�A�n�A�O�A�-A�  Aں^Aڝ�Aڏ\A�7LAٝ�A�`BA��Aؕ�A��A׸RA�G�A��A֮A�z�A�{A�33A��/AԍPA�l�A�33A���A�=qA�1'A��TAѶFAѓuA�t�A�S�A�oA���AЙ�A�|�A�dZA�XA�S�A�Q�A�O�A�I�A�C�A�?}A�7LA�1'A�+A��A��A�oA��A�ȴAϩ�A�\)A�33A�/A�+A��A��A�%A���A��HA���A�Aδ9Aΰ!AΟ�AΓuAΑhA΅A΁A΁A΃A·+A΃A�v�A�p�A�l�A�hsA�n�A�l�A�hsA�XA�Q�A�S�A�Q�A�I�A�7LA�-A�"�A� �A��A��A�VA�  A���A��A��yA��#A�ĜAʹ9Aͩ�A͝�A�~�A�9XA�+A�VA��A���A�K�A�E�A� �A�%A��TA�ȴA˶FAˬAˣ�Aˡ�A˝�A˕�A�XA�JA�AʅA�  A��`A���A�ȴAǝ�AǑhAǋDAǉ7AǇ+A�n�A��A�dZA�(�A�"�A�1A��
Aś�A�r�A�C�A�oA�  A���A���A��A��A��
Aė�A�M�A���A�ƨAìA�z�A�dZA�=qA�JA�A��A��/A�A���A�33A���A�5?A�A�/A��A���A���A�A��FA��A���A��A�l�A�`BA�O�A�A�A��A���A��A���A��DA�l�A�VA�9XA� �A�1A��mA���A���A�hsA�?}A��A���A���A�x�A�;dA�oA��HA���A��jA���A��A�hsA�`BA�XA�E�A�+A�"�A��A�{A�
=A���A��A��#A�ĜA��!A�{A�r�A�n�A�z�A���A�
=A���A�~�A���A��-A�XA��TA���A�%A���A�"�A�{A��A��uA�"�A��-A�bNA���A��hA�-A��`A��uA�ffA� �A���A�t�A�A�A�%A���A��A�v�A�l�A�O�A�9XA�/A�bA��TA��wA��!A��A���A���A��+A��/A���A��hA�v�A�A�A��yA�bNA�E�A�;dA�1'A��A�
=A���A��A��#A��/A���A��^A��FA��A��\A�A�A��A��
A��FA�l�A�$�A�bA���A�M�A��^A�n�A���A�v�A�XA�\)A�O�A�7LA��A��RA�K�A���A�ȴA��+A�hsA��A�\)A��/A�ZA�ĜA�l�A�C�A��;A�7LA���A�^5A���A�A�A�I�A�;dA�S�A��\A� �A���A�`BA��A��hA���A�v�A��FA�C�A���A�G�A���A��PA�p�A�hsA�hsA�jA�l�A�bNA�\)A�O�A�E�A�33A�"�A�1A���A��A�XA�
=A��HA��jA�;dA��#A�?}A��9A���A���A��jA���A�v�A�XA�1'A��^A���A�5?A�p�A��A��7A�JA�+A���A�ffA�$�A���A��A���A���A�`BA�A��uA�+A~��A|�/Az��AyS�AxVAw�Aw��Aw
=Au�AuS�Au;dAu"�At��Atz�As�;AsVAr~�Aq�TAp��An�/An�\Ann�AnZAn9XAn$�An{Am��Am�;AmXAl�jAlffAl{Aj5?Ai�AhI�Ag|�Af�+Ae�Adz�AdA�Ac�^Ab�/Ab�DAbz�Aa�TA`bA]�A]33A\�A\��A\bNA\E�A\(�A[��A[�AZI�AX�AX�AW��AW�FAW��AWx�AWG�AW�AVȴAV5?AUG�ATffAR�`AQ�#AQ�PAQ�APZAO��AO�hAO+AN�jANbNAN1'AM�^AM33AL�/AL�\ALffAL-AK�AK�AJ�yAJZAI�AIC�AHr�AG�7AF�HAF�AFZAF �AE�TAD�HAC��ABjAA�hAA+A@��A@�uA@�A@  A?�mA?�^A?�hA?hsA>�A>�+A>I�A>9XA>$�A>�A> �A>bA>bA=�A=��A=�FA=�-A=��A=�^A=�;A=�A=�A=��A=�-A=�A=dZA=+A=%A<�/A<�A<v�A<  A;l�A:�+A:5?A:$�A:�A:bA:  A9��A9�A9�TA9��A9�wA9�^A9�-A9��A9��A9�7A9�A9t�A9`BA9S�A9O�A9K�A9C�A933A9+A9oA8��A8�HA8��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                             A�hsA�r�A�r�A�r�A�r�A�r�A�r�A�r�A�t�A�r�A�r�A�r�A�p�A�r�A�v�A�x�A�v�A�K�A�G�A�Q�A�/A��A�VA�oA�{A�bA�VA�VA�JA�JA�JA�
=A�%A�A�A�  A�  A���A۶FA�Q�AٓuAցA�oAω7A�C�A�\)A��A�O�A�ffA���A�5?A��A�33A�1'A�1'A�`BA�9XA�hsA���A���A���A��A��!A�jA���A�p�A�A��A�=qA��A��A�\)A���A��A�9XA��jA��A33Aw;dAt-Aol�Am��Ah��Ac�A]�AZ��AWhsAS�mAO��AMS�AK;dAG��AD�uA@��A?\)A>-A=�;A=�
A=p�A<=qA:(�A9�TA9��A9dZA9+A8�!A7��A5�A2$�A1K�A1A2�A1�mA2{A2JA1�A0�DA/��A/�A/+A.=qA-�A-+A,��A,��A,bNA,(�A+��A*ȴA)O�A'�;A'�7A'�^A&�jA%t�A#��A"�A"^5A"5?A!�mA!��A!��A"{A"ZA"ffA!�-A!�A ��A ��A �+A n�A�;Ax�AĜA  Ax�AoAM�A�FA�7A|�AK�A�yAr�A��A�AS�A"�A�9A�+AjA1A�-Al�A�A��A5?A�A�A�7A33A�A�A5?A1A�hA/A��AM�AM�A�HA��AĜA�TA�hAt�A��A�A;dA�A�+A�#AK�A��A�jA�An�AA�A{A�#AƨA�
A�AoA��AA�AJA�A��A��A7LA
�A	�
A	;dAĜA9XAI�A�A��AE�A��A��Al�A�A�yA�+A9XA�A�A�-A�yA��AVA$�A�7A�A9XA�A�PA ��A I�@�t�@�\)@�"�@�-@�/@��@�\)@���@��T@��7@��9@�
=@��@��@�@�x�@��@�K�@���@�M�@��@���@�^@�X@��`@��m@�\)@�@�V@��^@�G�@��@��@�n�@�@�x�@�z�@��;@畁@�l�@�5?@�@�&�@���@�j@�P@���@�V@�Z@�|�@ާ�@��T@�&�@�Q�@��m@�|�@ڰ!@ٲ-@��/@�ƨ@�+@��@���@�
=@ְ!@֗�@�ff@�V@�E�@ղ-@�`B@�&�@�r�@�C�@ҧ�@҇+@�ff@�$�@��@��#@�@љ�@�`B@�7L@���@�bN@�1@��@��;@Ϯ@�;d@���@Ώ\@�=q@͡�@�&�@���@̃@�I�@�1@ˮ@�33@��#@ɑh@�V@��@�\)@��@�@�hs@�V@ă@Õ�@�S�@�33@��@�ȴ@�@�ff@�@�?}@��j@��u@�9X@��@�  @��@��
@�t�@�o@�ȴ@��+@�n�@�V@�E�@�$�@��#@��h@��@��u@�I�@�1@��@�M�@�p�@���@��`@���@��@��@���@���@�C�@�@��!@���@�ff@���@���@�?}@���@��/@�Z@��@��m@��;@�ƨ@��P@�S�@��@��R@�n�@�V@��@�{@��@���@���@��P@���@���@��+@�n�@�$�@���@��h@��@�bN@�9X@��@��
@��@���@��#@���@�hs@�&�@��/@�Ĝ@�Ĝ@��9@�j@�(�@��F@�33@�o@��H@���@�^5@���@���@��@�Z@� �@��@��@�ff@��@��-@�O�@��`@��9@�r�@�Q�@�A�@�b@��m@���@��P@�C�@��@�-@��@�x�@���@���@�A�@��P@�K�@�;d@�"�@���@�n�@��@��^@��@�`B@�O�@�/@��`@��@�A�@��w@�+@��+@�5?@��#@���@��7@�G�@��`@�j@��@��;@��P@���@�~�@�V@��@��T@��-@�p�@���@���@��j@���@�Z@� �@���@�;d@�+@��@���@�v�@�5?@��-@�G�@�%@��j@��u@�Q�@��@�ƨ@���@��P@�|�@�\)@�+@�
=@�
=@���@��R@���@��+@�ff@�-@�J@��T@�p�@�/@���@��D@�1@��@�33@���@��!@�V@�-@���@��h@�hs@�/@���@���@�z�@�Z@�I�@�1'@� �@�1@���@��@�|�@�S�@�;d@�33@��H@��+@�n�@�V@�5?@�{@���@���@�X@��@��@���@��j@�Ĝ@�Ĝ@���@��D@�z�@�bN@�I�@�1@�w@+@~{@}��@}p�@|��@|1@z�@z-@yX@x��@xĜ@x��@xA�@w�@w�P@w�@v�R@vv�@v@u��@u�h@u?}@t�@tZ@t1@s�
@s�@s"�@r��@r~�@r�@r�@q�#@p�u@p  @o;d@n�R@m��@l�@lj@lZ@l1@kƨ@k��@k"�@j~�@i�@i7L@h�9@h1'@h  @g�@gl�@gK�@fv�@e�@e�h@e`B@d�@dZ@d�@d1@c�m@cdZ@cC�@c@bn�@b�@aX@`�9@`�u@`Q�@`  @_K�@^�R@^v�@^$�@]�-@]�@\�j@\j@\1@[t�@[33@["�@[@Z��@Z��@Z�\@Z-@Y��@YG�@X�9@Xb@W\)@Vȴ@Vff@VV@VE�@U@UO�@U/@U�@UV@T��@S��@SS�@R��@Rn�@Q��@Q��@Q��@Q��@Q�^@Q��@Q%@P�@PQ�@O�@O|�@OK�@N�@Nȴ@N��@N$�@M@Mp�@M/@L��@L�j@LI�@K�
@K��@K"�@J�H@J��@JM�@I��@I&�@H��@H�`@H��@H��@HQ�@H1'@G��@GK�@F�y@Fv�@F{@E�-@E�h@E?}@E/@D��@D�D@DI�@D�@C�@CS�@C33@Co@B��@B=q@B�@A��@A��@A&�@@�@@ �@?�;@?��@?l�@?K�@>�y@>�+@>ff@>E�@>{@=��@=��@=�@=p�@=`B@=?}@=/@<�@<��@<(�@;�
@;�F@;33@;o@:��@:��@:�\@:^5@9�#@9�7@97L@8Ĝ@8�9@8�@8A�@7�P@7K�@6��@6ff@65?@6@5��@5�@4�@4�j@4Z@49X@41@3�m@3��@3o@2��@2��@2��@2-@1��@1��@1��@1&�@0��@0bN@01'@/�@/l�@/
=@.�R@.�R@.�R@.�R@.�+@.V@.$�@-@-�h@-�@,�@,I�@,9X@,�@+��@+�m@+S�@*�H@*�!@*��@*n�@*^5@*^5@*M�@*=q@*-@)��@)�^@)G�@(�`@(�u@(Q�@(b@'�@'|�@'\)@'\)@'+@'�@&��@&ȴ@&��@&$�@%�@%�-@%�@%V@$�D@$1@#ƨ@#t�@#@"~�@"=q@"�@!�#@!��@!�^@!�^@!�#@!�7@ 1'@�@�@��@   @ b@�@K�@�@v�@E�@��@��@�@�@�@�j@�D@�@�
@��@t�@�@��@��@�\@�\@^5@^5@�@�7@7L@&�@&�@7L@G�@%@�9@��@r�@r�@Q�@1'@ �@  @b@ �@ �@ �@  @�;@��@K�@\)@�@�@��@v�@�T@�-@�h@�h@p�@?}@/@�@�@�@��@�/@�j@��@j@��@��@�m@ƨ@��@�@33@�@�H@��@��@�!@~�@n�@�@�#@�#@��@�^@��@�7@hs@Ĝ@�u@r�@bN@A�@  @�wG�O�A�XA�ZA�^5A�r�A�p�A�n�A�r�A�v�A�t�A�n�A�r�A�v�A�r�A�n�A�p�A�t�A�r�A�n�A�p�A�t�A�t�A�p�A�p�A�t�A�v�A�r�A�p�A�p�A�t�A�t�A�p�A�p�A�r�A�r�A�p�A�p�A�r�A�t�A�l�A�hsA�n�A�v�A�v�A�r�A�t�A�v�A�r�A�r�A�v�A�x�A�v�A�t�A�t�A�x�A�x�A�v�A�x�A�z�A�x�A�x�A�x�A�z�A�x�A�v�A�z�A�v�A�t�A�v�A�t�A�S�A�VA�7LA�C�A�C�A�A�A�C�A�;dA�C�A�;dA�9XA�VA�O�A�M�A�M�A�XA�Q�A�Q�A�O�A�S�A�VA�\)A�G�A�?}A�1'A�5?A�5?A�-A�$�A�1'A�-A�+A�&�A� �A��A��A�bA�VA�bA�VA�
=A�VA�oA�bA�VA�bA�oA�VA�VA�oA�{A�oA��A��A�{A��A��A�{A�bA�{A�{A�bA�VA�bA�oA�VA�VA�bA�VA�JA�
=A�VA�bA�JA�JA�bA�bA�JA�VA�bA�VA�
=A�JA�VA�bA�JA�
=A�JA�VA�JA�1A�VA�VA�JA�
=A�JA�VA�JA�1A�JA�VA�JA�1A�
=A�VA�VA�
=A�1A�JA�VA�1A�1A�
=A�1A�A�A�1A�1A�%A�A�A�%A�%A�A�A�%A�A�A�A�A�%A�A���A�A�A�A���A���A���A�A�A���A���A�  A�A�  A���A���A�  A�A�A���A���A�  A�A�  A���A���A���A���A��;A���A�ƨAۼjA۲-Aۧ�A۟�Aۛ�A۝�Aۗ�A�~�A�x�A�n�A�O�A�-A�  Aں^Aڝ�Aڏ\A�7LAٝ�A�`BA��Aؕ�A��A׸RA�G�A��A֮A�z�A�{A�33A��/AԍPA�l�A�33A���A�=qA�1'A��TAѶFAѓuA�t�A�S�A�oA���AЙ�A�|�A�dZA�XA�S�A�Q�A�O�A�I�A�C�A�?}A�7LA�1'A�+A��A��A�oA��A�ȴAϩ�A�\)A�33A�/A�+A��A��A�%A���A��HA���A�Aδ9Aΰ!AΟ�AΓuAΑhA΅A΁A΁A΃A·+A΃A�v�A�p�A�l�A�hsA�n�A�l�A�hsA�XA�Q�A�S�A�Q�A�I�A�7LA�-A�"�A� �A��A��A�VA�  A���A��A��yA��#A�ĜAʹ9Aͩ�A͝�A�~�A�9XA�+A�VA��A���A�K�A�E�A� �A�%A��TA�ȴA˶FAˬAˣ�Aˡ�A˝�A˕�A�XA�JA�AʅA�  A��`A���A�ȴAǝ�AǑhAǋDAǉ7AǇ+A�n�A��A�dZA�(�A�"�A�1A��
Aś�A�r�A�C�A�oA�  A���A���A��A��A��
Aė�A�M�A���A�ƨAìA�z�A�dZA�=qA�JA�A��A��/A�A���A�33A���A�5?A�A�/A��A���A���A�A��FA��A���A��A�l�A�`BA�O�A�A�A��A���A��A���A��DA�l�A�VA�9XA� �A�1A��mA���A���A�hsA�?}A��A���A���A�x�A�;dA�oA��HA���A��jA���A��A�hsA�`BA�XA�E�A�+A�"�A��A�{A�
=A���A��A��#A�ĜA��!A�{A�r�A�n�A�z�A���A�
=A���A�~�A���A��-A�XA��TA���A�%A���A�"�A�{A��A��uA�"�A��-A�bNA���A��hA�-A��`A��uA�ffA� �A���A�t�A�A�A�%A���A��A�v�A�l�A�O�A�9XA�/A�bA��TA��wA��!A��A���A���A��+A��/A���A��hA�v�A�A�A��yA�bNA�E�A�;dA�1'A��A�
=A���A��A��#A��/A���A��^A��FA��A��\A�A�A��A��
A��FA�l�A�$�A�bA���A�M�A��^A�n�A���A�v�A�XA�\)A�O�A�7LA��A��RA�K�A���A�ȴA��+A�hsA��A�\)A��/A�ZA�ĜA�l�A�C�A��;A�7LA���A�^5A���A�A�A�I�A�;dA�S�A��\A� �A���A�`BA��A��hA���A�v�A��FA�C�A���A�G�A���A��PA�p�A�hsA�hsA�jA�l�A�bNA�\)A�O�A�E�A�33A�"�A�1A���A��A�XA�
=A��HA��jA�;dA��#A�?}A��9A���A���A��jA���A�v�A�XA�1'A��^A���A�5?A�p�A��A��7A�JA�+A���A�ffA�$�A���A��A���A���A�`BA�A��uA�+A~��A|�/Az��AyS�AxVAw�Aw��Aw
=Au�AuS�Au;dAu"�At��Atz�As�;AsVAr~�Aq�TAp��An�/An�\Ann�AnZAn9XAn$�An{Am��Am�;AmXAl�jAlffAl{Aj5?Ai�AhI�Ag|�Af�+Ae�Adz�AdA�Ac�^Ab�/Ab�DAbz�Aa�TA`bA]�A]33A\�A\��A\bNA\E�A\(�A[��A[�AZI�AX�AX�AW��AW�FAW��AWx�AWG�AW�AVȴAV5?AUG�ATffAR�`AQ�#AQ�PAQ�APZAO��AO�hAO+AN�jANbNAN1'AM�^AM33AL�/AL�\ALffAL-AK�AK�AJ�yAJZAI�AIC�AHr�AG�7AF�HAF�AFZAF �AE�TAD�HAC��ABjAA�hAA+A@��A@�uA@�A@  A?�mA?�^A?�hA?hsA>�A>�+A>I�A>9XA>$�A>�A> �A>bA>bA=�A=��A=�FA=�-A=��A=�^A=�;A=�A=�A=��A=�-A=�A=dZA=+A=%A<�/A<�A<v�A<  A;l�A:�+A:5?A:$�A:�A:bA:  A9��A9�A9�TA9��A9�wA9�^A9�-A9��A9��A9�7A9�A9t�A9`BA9S�A9O�A9K�A9C�A933A9+A9oA8��A8�HA8��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                             ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B�%B�ZB�ZB��B�%B��B�%B��B��B�ZB��B��B�%B�%B�%B�ZB�>B��B�lB�JB�(B�cB	 �B	B	�B	�B	B	B	AB	�B	�B	�B	B	�B	�B	B	�B	B	B	:*B	ZB	�DB	�B	��B
�B
��B
ɺB
��B<jBH�Bz�B��B�hB�RBʌB�[B�8B�5B��BݘBÖB��B��B�B�%BzDBRTB�B
�
B
�6B
�_B
��B
�xB
TaB
1[B	�xB	�dB	�CB	�B	�iB	i�B	^5B	49B	#B	\B��B��B�B�ZB� B��B��B��B	�B	hB	 �B	?HB	V�B	qAB	�SB	��B	��B	�CB	�\B	��B	��B	�3B	ŢB	��B	�9B	��B	��B
{B

�B
�B
IB
�B
_B
!bB
$tB
#B
0�B
5tB
:^B
=<B
=�B
;�B
;dB
2�B
&�B
&LB
-�B
4�B
'�B
!bB
uB
�B
!�B
($B
-B
0UB
5�B
L�B
QNB
W�B
XB
ZB
YB
W�B
W�B
XyB
XEB
WsB
U�B
U�B
VB
XEB
XyB
W�B
W?B
YKB
XB
[#B
[#B
[�B
[�B
\�B
]�B
\�B
]/B
]/B
]�B
]�B
]/B
^5B
]�B
]/B
\�B
\)B
[�B
Z�B
[�B
Y�B
ZQB
X�B
W�B
Y�B
XB
X�B
f2B
gB
iyB
b�B
^B
]�B
e�B
gB
f�B
d�B
b�B
^jB
Z�B
Z�B
X�B
W�B
X�B
ZB
[#B
[#B
Z�B
\]B
]/B
[WB
[#B
ZB
YKB
X�B
XyB
W
B
V�B
T�B
P�B
J�B
HB
F�B
H�B
MjB
PHB
O�B
NB
MB
LdB
J#B
J�B
J�B
JXB
I�B
H�B
G�B
D�B
B�B
B�B
?B
=qB
8�B
2�B
0�B
/�B
,�B
,qB
(�B
)_B
)�B
)�B
&�B
$�B
#B
!�B
 �B
�B
!bB
 �B
�B
�B
OB
�B
IB
�B
=B
�B
IB
IB
~B
�B
B
CB
�B
B
�B
�B
�B
FB
B
�B
\B
�B
�B
B
B
PB
�B
�B
JB
xB

�B

�B
	7B
+B
_B
YB
�B
YB
1B
�B
�B
�B
�B
SB
%B
�B
+B
	7B
	�B
"B
�B
�B
�B
�B
"B
\B
�B
VB
�B
�B
�B
�B
PB
�B
�B
"B
�B
�B
�B
�B
 B
 B
�B
�B
�B
�B
�B
�B
�B
�B
�B
\B
\B
(B
�B
�B
VB
�B
VB
�B
�B
	lB

�B
	lB
	lB
	7B
	B

	B

rB

	B
	�B

	B

	B
	�B
	�B

	B

=B
	�B
	�B
	�B
	�B
	lB
	lB
	lB
	�B

	B
	�B

	B
	lB
	�B
	7B
	�B
	lB
	B
	lB
	�B
	lB
	7B
	�B
B

�B

�B

=B

rB

�B

�B

�B
DB
�B
xB
DB
B
xB
�B
�B
JB
�B
PB
�B
PB
PB
�B
"B
VB
�B
�B
�B
�B
�B
�B
\B
(B
4B
.B
4B
�B
�B
hB
�B
:B
@B
�B
B
�B
{B
{B
{B
B
�B
�B
�B
$B
YB
�B
YB
YB
YB
�B
�B
+B
1B
eB
�B
�B
	B
qB
�B
7B
7B
7B
�B
1B
�B
�B
1B
1B
eB
eB
�B
�B
kB
�B
7B
B
B
B
kB
�B
kB
�B
=B
�B
qB
�B
CB
�B
IB
�B
OB
~B
B
~B
�B
B
OB
�B
�B
�B
 �B
 �B
 'B
!�B
!�B
"4B
"�B
#�B
#�B
$�B
$tB
$@B
$tB
%zB
%FB
%zB
%�B
%�B
%�B
&LB
&�B
&�B
&�B
&�B
&�B
'B
'�B
(XB
($B
(�B
(�B
(�B
)*B
)�B
*eB
,B
,qB
,qB
,�B
-B
-�B
.B
-�B
-�B
.B
/B
/�B
/OB
/�B
/�B
/�B
/�B
0UB
0UB
0!B
/�B
0UB
/�B
0!B
/�B
0�B
0�B
0�B
0�B
1'B
1�B
1[B
2-B
2-B
2aB
2�B
2�B
3hB
33B
3hB
3�B
3�B
3�B
3hB
3hB
2�B
3hB
3hB
3�B
3�B
4�B
4�B
49B
4�B
5B
4nB
49B
5B
5tB
5tB
5�B
5�B
6B
6zB
6�B
7LB
6�B
7B
7B
7B
7�B
7LB
8B
8�B
8B
8B
8RB
9XB
9�B
9�B
:�B
:�B
:^B
:�B
:�B
:�B
;�B
<B
;�B
<B
<�B
<jB
<B
;�B
<�B
=<B
=qB
=qB
>BB
>BB
>�B
>wB
>BB
=�B
>wB
@B
?�B
?�B
?HB
@OB
@�B
AUB
A�B
A�B
B'B
A�B
B[B
B[B
C-B
C�B
D3B
D�B
D�B
D�B
D�B
DgB
F?B
E�B
FB
E�B
FtB
F�B
F�B
F�B
F�B
GzB
GB
GB
G�B
GzB
HKB
HKB
HKB
H�B
H�B
I�B
I�B
I�B
I�B
JXB
J�B
K)B
K)B
K�B
LdB
L0B
L0B
L0B
LdB
LdB
L0B
L�B
MB
M6B
MjB
N<B
N�B
OB
OBB
N�B
N�B
O�B
O�B
O�B
O�B
OvB
PB
P}B
QB
QB
QB
RTB
RTB
Q�B
Q�B
Q�B
Q�B
R B
S&B
R�B
S[B
R�B
S�B
S�B
S�B
S�B
T,B
T�B
TaB
U2B
T�B
T�B
VB
U�B
U�B
V�B
V9B
V�B
W
B
W
B
W?B
WsB
WsB
W?B
W�B
W�B
W�B
W�B
XyB
XyB
X�B
YKB
Y�B
ZQB
Z�B
ZQB
ZQB
[#B
[�B
[�B
[�B
\)B
\]B
\]B
\�B
\�B
\�B
\�B
\�B
\]B
\�B
\�B
\�B
]dB
]dB
\�B
]�B
]�B
]�B
]�B
]�B
^B
^5B
^jB
^B
^5B
^�B
^�B
^B
^�B
_;B
_B
_;B
_�B
_pB
`BB
_�B
_�B
`vB
`�B
a|B
`�B
aHB
aHB
`�B
a�B
bB
a�B
bB
b�B
b�B
b�B
b�B
cTB
cTB
cTB
cTB
c�B
c�B
c�B
d�B
d�B
d�B
d&B
d�B
e�B
e�B
e`B
e,B
e�B
ffB
f�B
f�B
gB
gmB
hsB
iyB
iyB
iyB
iB
iB
iyB
jB
kQB
k�B
k�B
k�B
l�B
k�B
k�B
k�B
l"B
m�B
ncB
n/B
m�B
m�B
n/B
m]B
m)B
m)B
m]B
m]B
m]B
ncB
ncB
ncB
ncB
n�B
o5B
o�B
pB
oiB
pB
pB
o�B
pB
pB
p;B
p;B
p�B
p�B
qB
p�B
p;B
o�B
o�B
o�B
p;B
o�B
p;B
p�B
poB
qAB
r|B
s�B
t�B
r|B
rGB
q�B
qvB
r�B
s�B
s�B
tTB
r�B
sB
sB
s�B
s�B
s�B
tTB
tTB
s�B
t�B
u%B
u%B
t�B
t�B
u�B
u�B
u�B
u�B
v`B
w�B
xB
xlB
x8B
w�B
xB
x8B
y	B
z�B
{B
{B
{JB
{B
{�B
{�B
|B
|B
|�B
|�B
}"B
}"B
}VB
}VB
}VB
}�B
}"B
}�B
~(B
~�B
~�B
~�B
�B
�B
�B
�B
�B
� B
�4B
�4B
�4B
� B
�4B
�4B
�4B
�4B
�iB
�4B
� B
� B
�4B
��B
��B
�;B
�B
�oB
�oB
��B
��B
�B
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
��B
��B
��B
�B
��B
�B
�{B�B�B�+B�B��B��B��B�B�ZB��B��B�B�ZB�`B��B��B�B�`B��B��B��B��B�`B��B�TB��B�`B��B�B��B��B�ZB��B�B��B�+B�ZB��B�8B��B�%B�MB��B��B��B�TB��B��B��B�TB��B��B��B�B��B�+B�%B�TB��B��B��B�B�ZB�+B��B��B�ZB�B�TB�B�	B��B�fB�8B��B�DB��B��B��B�B�2B��B��B��B�`B�>B��B�	B�8B�fB��B�"B��B��B�B�B��B��B��B�B��B�B��B��B�(B	 �B�]B��B��B	 iB�.B��B�cB	 �B	  B��B	 iB	 �B��B��B	�B	B	 �B	�B	B	B	�B	B	oB	;B	uB	�B	oB	 �B	AB	�B	B	B	�B	�B	;B	 iB	�B	�B	B	 �B	uB	�B	�B	�B	�B	�B	�B	oB	�B	B	oB	B	B	B	oB	;B	AB	B	AB	;B	uB	{B	�B	�B	B	{B	GB	�B	B	�B	�B	uB	B	�B	{B	B	�B	GB	B	B	�B	AB	�B	{B	uB	AB	�B	{B	�B	B	{B	{B	B	�B	�B	�B	uB	B	uB	�B	�B	{B	B	B	B	�B	uB	�B	AB	{B	�B	�B	uB	�B	�B	MB	B	uB	�B	1B	�B	%B	%B	"B	B	�B	~B	�B	�B	�B	�B	PB	�B	{B	hB	:B	YB	�B	!�B	$@B	�B	OB	5tB	7LB	?�B	L�B	M�B	S�B	R�B	UgB	[�B	Q�B	S�B	XEB	o�B	bNB	b�B	b�B	ffB	lWB	��B	��B	��B	�B	�JB	�B	�B	��B	��B	��B	��B	��B	�0B	��B	��B	��B	�0B	�kB	��B	�B	��B	��B	�[B	�}B	��B	��B	�OB	�?B	��B	�?B	�B	ѷB	�9B	��B	��B	רB	�QB	��B	�B	� B	� B	�B	�mB	�B	�B	�
B	�B	�B	��B	��B	��B	��B	�B	� B	�B	�)B	�5B	�B	�B	�B	�)B	�B	�)B	��B	�B	��B	��B	�"B	�WB	�/B	�iB	�iB	��B	�B	�vB	�B	�|B	�|B	��B	��B	��B	��B	��B
�B
 \B
kB
 �B
#B
%�B
&�B
&�B
&�B
(�B
+6B
,�B
6�B
G�B
W�B
g�B
l�B
��B
��B
�aB
�-B
��B
��B
�'B
��B
��B
�@B
��B
�'B
�6B
�*B
��B
��B
ƨB
�#B
҉B
�sB
��B
��B
�TB
� B
�NB
�mB
�B
��B
��B
��B
�oB
��B
�B
��B
��B
�B
�PB
��B�B/B!�B3�B2�B7�BH�BF?BC�BD3BC�BD�BE�BGEBJ�BG�BG�BG�BF�BJ�BH�BM�BD�B`�Bm�BqBxBw�By�B{B~�B��B��B�B��B��B��B�=B��B��B��B��B��B��B��B�hB�'B�-B��B��B��B��B��B��B�hB��B�-B��B��B�9B�B�ABޞB�B�)B�B�B�B�aBȀB�BɺB�sB��B�aB��B�KB��B��B�BB��B�mB�sB�
B��BیB�
B�BB��B�B�B�B�B��B��B�B�B��B��B�,B�B�#B�WBٴB�yB�QB �B��B�[BҽB�9B�dB��B�zB�3B�gBŢB�gB�B�OB��B�0B�wB�^B�LB��B��B�UB��B��B�B��B�B��B�6B�B��B��B�oB��B~�B�B��B�AB��B�(B��B��B��B|�Bx�B��B��Bz�BuZBq�B]/BYBb�Bj�BK�BC�B@OBF�B:^B%�B1'BYB�B�B
�cB
�B
�TB
�B
ޞB
�B
ѷB
��B
�B
�UB
��B
��B
�6B
�0B
�_B
�*B
�_B
��B
��B
��B
��B
�@B
�*B
�*B
��B
�xB
�IB
��B
��B
�VB
��B
~�B
~]B
�JB
a�B
[#B
S�B
R B
Q�B
P}B
U�B
_;B
A�B
49B
*�B
/B
.�B
 'B
\B	��B
B
�B	�B	�B	�B	��B	�B	ܒB	��B	��B	��B	��B	�B	��B	��B	�nB	��B	��B	�B	�B	�B	�{B	��B	��B	�oB	��B	��B	�B	��B	v�B	o5B	oiB	poB	l"B	jKB	jB	e�B	s�B	d�B	_�B	`vB	�oB	c B	RTB	T,B	Q�B	P�B	:�B	2�B	;�B	4�B	(�B	 �B	0�B	E�B	3�B	xB	�B	oB	"B	B		�B	B	�B	�B	�B	�B��B�xB�DB��B��B�xB��B	uB	;B	_B	�B�`B�2B�B��B�B�B�"B�B��B�B��B�>B�TB�B�BߤB�NB� B�"B��B�TB�B�;B�B�B�DB�`B��B��B	MB	�B	�B�8B��B�B��B��B��B�B��B��B��B	~B	 B	 B	\B	 B	B	�B	SB	�B	VB	#nB	%B	)�B	0UB	49B	:�B	CaB	L�B	P�B	U�B	U2B	V9B	[WB	[�B	_pB	c�B	h�B	y�B	�fB	��B	�{B	�MB	�B	��B	�$B	��B	��B	�SB	�SB	��B	�YB	��B	��B	��B	��B	�7B	�7B	�xB	��B	�~B	��B	��B	��B	�\B	��B	��B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                             B�B�?B�tB�tB��B�?B��B�?B��B��B�tB��B��B�%B�%B�ZB�FB�XB��B�>B��B�]B�cB	 �B	'B	�B	�B	'B	'B	[B	�B	B	�B	-B	�B	�B	aB	zB	B	 �B	K^B	o5B	�qB	��B	�$B
0!B
�=B
յB	BDgBQ B��B�LB��B�B�dB�ZB��B��B�B�RB�+B��B��B�vB��B��Bn/B49B
�DB
�OB
�OB
��B
��B
k�B
F�B
B	��B	��B	�@B	��B	yXB	p�B	F%B	.B	kB	�B	�B��B�"B�OB�	B	1B��B	�B	�B	!|B	AB	Z�B	w�B	�
B	�sB	��B	�/B	�hB	��B	ªB	��B	�KB	��B	�gB	�%B	�0B
�B
JB
�B
!B
1B
kB
$tB
&�B
$�B
1�B
6B
;JB
>(B
?�B
>�B
@iB
7�B
'�B
&2B
1AB
9$B
-�B
%FB
,B
=B
"�B
'�B
-B
/�B
5%B
L�B
S�B
Y�B
X�B
[=B
Y�B
X_B
Y�B
ZB
Z�B
ZB
W�B
W?B
X�B
ZQB
YB
W�B
XB
Z�B
Y�B
]IB
\CB
\xB
\�B
^5B
^jB
]~B
^�B
^jB
^�B
_�B
^5B
_�B
^B
]�B
^B
]dB
\�B
\xB
\�B
Z�B
[�B
ZB
Y1B
[WB
W�B
W$B
e�B
g�B
lqB
d&B
^OB
\�B
fLB
h�B
g�B
f�B
e`B
`vB
[�B
[�B
YB
XEB
YeB
Z�B
[�B
[qB
Z�B
]�B
^�B
\�B
\xB
Z�B
Y�B
YB
YKB
X�B
YB
WsB
R�B
LJB
I�B
F�B
H1B
M6B
QhB
Q B
N�B
NpB
M�B
J�B
LJB
K�B
J�B
JrB
I�B
JrB
EmB
DMB
C{B
AUB
?�B
:�B
49B
2B
1�B
/iB
.cB
)B
)�B
+�B
+�B
(�B
&B
$ZB
"�B
!�B
!|B
$@B
"4B
!B
!B
B
!�B
�B
WB
xB
]B
~B
~B
5B
�B
�B
IB
�B
WB
B
�B
�B
9B
�B
�B
bB
vB
�B
�B
�B
vB
�B
B
�B
dB
dB
�B

�B
�B
	B
�B
1B
�B
	�B
_B
YB
?B
�B
�B
1B
�B
�B
	lB
	�B
�B
�B
�B
"B
"B
(B
�B
pB
�B
�B
B
�B
�B
�B
"B
�B
VB
�B
.B
bB
NB
�B
�B
 B
�B
�B
}B
bB
B
vB
B
�B
B
�B
�B
BB
vB
vB
�B
�B
B
�B

�B
B

=B

#B
	�B

#B
�B

�B

XB

XB

XB

rB

=B

�B
xB
)B

=B

XB

#B
	�B
	�B
	�B

#B

�B

�B

XB

=B
	�B

	B
	�B

#B

	B
	�B

rB

rB
	�B

XB
B
�B
�B

�B

�B
B
^B
)B
B
JB
0B
B
xB
�B
JB
dB
�B
�B
B
VB
�B
jB
jB
�B
�B
�B
BB
�B
B
�B
�B
�B
�B
 B
B
B
�B
�B
�B
�B
:B
�B
�B
�B
gB
2B
�B
B
9B
�B
�B
$B
sB
�B
�B
�B
sB
�B
�B
EB
�B
B
B
�B
B
�B
xB
�B
=B
�B
�B
qB
	B
B
kB
KB
B
B
�B
�B
B
B
�B
�B
kB
�B
�B
�B
�B
=B
WB
B
�B
�B
�B
]B
xB
�B
�B
�B
;B
�B
~B
�B
�B
jB
�B
jB
�B
 �B
"B
!�B
 �B
"4B
"4B
"hB
#TB
$ZB
$�B
%FB
$�B
$�B
%�B
&B
%�B
%�B
&B
&B
&2B
'B
'RB
&�B
'B
'mB
'mB
($B
(�B
(�B
(�B
)_B
)_B
)DB
*0B
*�B
*�B
,�B
,�B
,�B
-]B
-�B
-�B
./B
.B
./B
.}B
/OB
/�B
/�B
0B
0!B
0!B
0;B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
1�B
1[B
1vB
1[B
1�B
1�B
2B
2�B
2|B
2�B
2�B
3�B
3�B
3�B
3�B
3�B
4B
3�B
3�B
3�B
3hB
3�B
3�B
3�B
49B
5?B
4�B
4nB
5%B
5ZB
4�B
4�B
5�B
5�B
5�B
5�B
5�B
6B
6zB
72B
7fB
7B
7LB
7LB
7�B
7�B
7�B
9$B
9$B
8RB
8�B
9XB
:^B
:DB
:�B
;JB
:�B
:�B
:�B
;JB
;dB
<B
<jB
<B
<jB
<�B
<�B
<PB
<6B
="B
=�B
=�B
=�B
>�B
>�B
>�B
>�B
>]B
>BB
?�B
@�B
@�B
@iB
@4B
A B
AoB
AoB
BB
BAB
B[B
BAB
B�B
B�B
C�B
D�B
D�B
EB
D�B
EB
EB
E9B
F�B
FB
F?B
FYB
F�B
G+B
F�B
F�B
G_B
G�B
G_B
G�B
HKB
H1B
H�B
H�B
H�B
IB
I7B
J=B
J=B
J=B
JrB
J�B
K^B
KxB
K�B
LB
L�B
LJB
LdB
L~B
L~B
L~B
L�B
MPB
MjB
M�B
NB
N�B
O\B
OvB
O\B
N�B
O\B
PHB
O�B
O�B
O�B
O�B
P�B
QB
Q�B
Q�B
Q�B
R�B
RTB
Q�B
Q�B
R B
R�B
R�B
S[B
R�B
S�B
S@B
T,B
S�B
S�B
T,B
T�B
UB
T�B
UgB
T�B
UgB
VmB
VB
V9B
V�B
VSB
WYB
W�B
W�B
WsB
W�B
W�B
WsB
X+B
W�B
XB
X_B
X�B
X�B
YKB
Y�B
ZB
Z�B
Z�B
Z�B
Z�B
[qB
[�B
\xB
\)B
\CB
\�B
\�B
]B
\�B
\�B
\�B
]B
\�B
\�B
]B
]B
]�B
]�B
]dB
^B
]�B
]�B
]�B
^B
^5B
^OB
^�B
^B
^OB
^�B
^�B
^jB
_B
_�B
_;B
_�B
_�B
_�B
`vB
_�B
`B
`�B
a-B
a�B
aB
abB
a|B
`�B
bhB
bhB
bB
b�B
b�B
b�B
c B
cnB
c�B
c�B
c�B
c�B
c�B
d&B
d@B
eB
d�B
d�B
dZB
d�B
e�B
e�B
e�B
e�B
f�B
f�B
gB
gB
gRB
g�B
h�B
iyB
iyB
iyB
iDB
iDB
i�B
jB
k�B
lWB
lWB
l"B
l�B
lB
k�B
k�B
l�B
m�B
n�B
nIB
m�B
m�B
n/B
mwB
mCB
mCB
m�B
m�B
m�B
n�B
n�B
n�B
n�B
o5B
oiB
o�B
pB
o�B
p!B
p!B
pB
p;B
p�B
poB
p�B
p�B
qB
q�B
q'B
p�B
p!B
p;B
pUB
p�B
pB
p�B
p�B
poB
qAB
raB
tB
u�B
r�B
rGB
q�B
q[B
r�B
s�B
tTB
t�B
sB
sMB
s�B
tB
s�B
s�B
t�B
t�B
tB
u%B
utB
uZB
t�B
utB
u�B
u�B
u�B
u�B
v�B
w�B
xRB
x�B
x�B
w�B
xB
x8B
y	B
z�B
{dB
{�B
{B
{B
{�B
|B
|6B
|6B
|�B
|�B
}"B
}"B
}qB
}qB
}�B
}�B
}"B
~(B
~wB
~�B
~�B
}B
� B
�B
�B
�B
� B
�B
�OB
�4B
�4B
�B
�OB
�OB
�OB
�iB
��B
�4B
�B
�B
�OB
��B
��B
�oB
� B
��B
��B
��B
�B
�'B
�[B
��B
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
��B
�gB
��B
�3G�O�B�B�B�+B�B��B��B��B�B�ZB��B��B�B�ZB�`B��B��B�B�`B��B��B��B��B�`B��B�TB��B�`B��B�B��B��B�ZB��B�B��B�+B�ZB��B�8B��B�%B�MB��B��B��B�TB��B��B��B�TB��B��B��B�B��B�+B�%B�TB��B��B��B�B�ZB�+B��B��B�ZB�B�TB�B�	B��B�fB�8B��B�DB��B��B��B�B�2B��B��B��B�`B�>B��B�	B�8B�fB��B�"B��B��B�B�B��B��B��B�B��B�B��B��B�(B	 �B�]B��B��B	 iB�.B��B�cB	 �B	  B��B	 iB	 �B��B��B	�B	B	 �B	�B	B	B	�B	B	oB	;B	uB	�B	oB	 �B	AB	�B	B	B	�B	�B	;B	 iB	�B	�B	B	 �B	uB	�B	�B	�B	�B	�B	�B	oB	�B	B	oB	B	B	B	oB	;B	AB	B	AB	;B	uB	{B	�B	�B	B	{B	GB	�B	B	�B	�B	uB	B	�B	{B	B	�B	GB	B	B	�B	AB	�B	{B	uB	AB	�B	{B	�B	B	{B	{B	B	�B	�B	�B	uB	B	uB	�B	�B	{B	B	B	B	�B	uB	�B	AB	{B	�B	�B	uB	�B	�B	MB	B	uB	�B	1B	�B	%B	%B	"B	B	�B	~B	�B	�B	�B	�B	PB	�B	{B	hB	:B	YB	�B	!�B	$@B	�B	OB	5tB	7LB	?�B	L�B	M�B	S�B	R�B	UgB	[�B	Q�B	S�B	XEB	o�B	bNB	b�B	b�B	ffB	lWB	��B	��B	��B	�B	�JB	�B	�B	��B	��B	��B	��B	��B	�0B	��B	��B	��B	�0B	�kB	��B	�B	��B	��B	�[B	�}B	��B	��B	�OB	�?B	��B	�?B	�B	ѷB	�9B	��B	��B	רB	�QB	��B	�B	� B	� B	�B	�mB	�B	�B	�
B	�B	�B	��B	��B	��B	��B	�B	� B	�B	�)B	�5B	�B	�B	�B	�)B	�B	�)B	��B	�B	��B	��B	�"B	�WB	�/B	�iB	�iB	��B	�B	�vB	�B	�|B	�|B	��B	��B	��B	��B	��B
�B
 \B
kB
 �B
#B
%�B
&�B
&�B
&�B
(�B
+6B
,�B
6�B
G�B
W�B
g�B
l�B
��B
��B
�aB
�-B
��B
��B
�'B
��B
��B
�@B
��B
�'B
�6B
�*B
��B
��B
ƨB
�#B
҉B
�sB
��B
��B
�TB
� B
�NB
�mB
�B
��B
��B
��B
�oB
��B
�B
��B
��B
�B
�PB
��B�B/B!�B3�B2�B7�BH�BF?BC�BD3BC�BD�BE�BGEBJ�BG�BG�BG�BF�BJ�BH�BM�BD�B`�Bm�BqBxBw�By�B{B~�B��B��B�B��B��B��B�=B��B��B��B��B��B��B��B�hB�'B�-B��B��B��B��B��B��B�hB��B�-B��B��B�9B�B�ABޞB�B�)B�B�B�B�aBȀB�BɺB�sB��B�aB��B�KB��B��B�BB��B�mB�sB�
B��BیB�
B�BB��B�B�B�B�B��B��B�B�B��B��B�,B�B�#B�WBٴB�yB�QB �B��B�[BҽB�9B�dB��B�zB�3B�gBŢB�gB�B�OB��B�0B�wB�^B�LB��B��B�UB��B��B�B��B�B��B�6B�B��B��B�oB��B~�B�B��B�AB��B�(B��B��B��B|�Bx�B��B��Bz�BuZBq�B]/BYBb�Bj�BK�BC�B@OBF�B:^B%�B1'BYB�B�B
�cB
�B
�TB
�B
ޞB
�B
ѷB
��B
�B
�UB
��B
��B
�6B
�0B
�_B
�*B
�_B
��B
��B
��B
��B
�@B
�*B
�*B
��B
�xB
�IB
��B
��B
�VB
��B
~�B
~]B
�JB
a�B
[#B
S�B
R B
Q�B
P}B
U�B
_;B
A�B
49B
*�B
/B
.�B
 'B
\B	��B
B
�B	�B	�B	�B	��B	�B	ܒB	��B	��B	��B	��B	�B	��B	��B	�nB	��B	��B	�B	�B	�B	�{B	��B	��B	�oB	��B	��B	�B	��B	v�B	o5B	oiB	poB	l"B	jKB	jB	e�B	s�B	d�B	_�B	`vB	�oB	c B	RTB	T,B	Q�B	P�B	:�B	2�B	;�B	4�B	(�B	 �B	0�B	E�B	3�B	xB	�B	oB	"B	B		�B	B	�B	�B	�B	�B��B�xB�DB��B��B�xB��B	uB	;B	_B	�B�`B�2B�B��B�B�B�"B�B��B�B��B�>B�TB�B�BߤB�NB� B�"B��B�TB�B�;B�B�B�DB�`B��B��B	MB	�B	�B�8B��B�B��B��B��B�B��B��B��B	~B	 B	 B	\B	 B	B	�B	SB	�B	VB	#nB	%B	)�B	0UB	49B	:�B	CaB	L�B	P�B	U�B	U2B	V9B	[WB	[�B	_pB	c�B	h�B	y�B	�fB	��B	�{B	�MB	�B	��B	�$B	��B	��B	�SB	�SB	��B	�YB	��B	��B	��B	��B	�7B	�7B	�xB	��B	�~B	��B	��B	��B	�\B	��B	��B	��B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                             <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<=)<�`{<�s~<���<#�
<X:<�0<��U<Q��<�K<#�
<#�
<0��<#�
<i`�<ޮI<�4�<�.<?@M<#�
<Q�<2�<#�
<#�
<u�d<:�<�kC<��B<�\�<�N<�@<#�
<#�
<��V<��T<��<��T<��B<��<Jt<���<#�
<���<��l<���<C�<C�<]�<e'N<#�
<#�
<T^<C�<X��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<1WY<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2018060207203620180602072036IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018061207034020180612070340QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2018061207034020180612070340QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107550820190521075508IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619141920230426191419IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                