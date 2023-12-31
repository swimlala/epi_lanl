CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-01-03T18:35:18Z creation; 2022-09-06T18:25:47Z DMQC;      
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20220103183518  20220907192128  5905791 5905791 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               i   iAA  AOAO7825_008765_105                 7825_008765_105                 2C  2C  DD  SOLO_II                         SOLO_II                         8765                            8765                            V2.6; SBE602 06Mar19            V2.6; SBE602 06Mar19            853 853 @ٯ*�@��@ٯ*�@��11  @ٯ*�c�@ٯ*�c�@4ȕloT@4ȕloT�e%�ם
�e%�ם
11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u?�@@  @�G�@��R@��R@�  @��RA��A!G�A,��A@��A`  A�  A�  A�  A�
=A�\)A�  A�Q�A�Q�B   B�
B  B�
B (�B((�B0  B7�
B@  BH  BP  BX  B_�
Bg�
Bp  Bw�
B�  B�  B�  B�  B�  B��B��
B�  B�{B�(�B�  B�  B�  B��B�  B�  B��B��
B��
B��B��B��B��B�  B�{B�{B��B��B�(�B�(�B�{B�  B��C��C  C{C  C

=C
=C
=C
=C
=C  C��C  C�C��C��C��C!��C$  C&
=C(  C)��C+��C-��C0  C2  C4
=C6  C7�C:  C<  C>
=C@
=CB
=CD  CF  CH  CJ  CL
=CM��CP  CR  CS��CV  CX
=CY��C[��C]��C_��Ca�Cc�Cf  Ch
=Cj  Cl  Cm�Co��Cr
=Cs��Cu��Cx
=Cz�C|{C~
=C�
=C�C�C�C�C���C���C�C�C���C���C�C�
=C�  C���C�  C�C�
=C�  C�  C�  C���C���C�  C�C�
=C�C�C�
=C�  C�  C�  C�  C�C�  C�C�
=C�C�  C���C���C�C�
=C�C�  C�C�  C���C���C���C�  C�
=C�
=C�
=C�C�  C���C���C�  C���C���C���C�  C���C�  C�C�  C���C���C�  C�
=C�C�  C�  C���C���C���C�C�C�C�C���C�  C���C���C�C�
=C�C�C�C���C�  C���C���C�
=C�
=C�  C�  C�C�C�  C�  C�
=C�
=C���C�  C�  C�  C�  C�C���C���C�  C�  C���C���C�C�C�C�C�C�
=C�C���C�  C�  C�  C�  D   D ��D  D��D�Dz�D  D� D��D� D�D}qD�qD� D�D��D�D��D	D	� D	�qD
� D  D� D�D��D�D}qD��Dz�D  D}qD�RD}qD  D}qD  D��D  D}qD�qD� D�qD� D�D� D�D�D�qDz�D  D��D�qD}qD�qDz�D  D�DD� D�D��D�qD� D �D }qD �qD!� D"  D"� D#  D#��D#�qD$}qD%  D%��D&  D&z�D'  D'� D(�D(��D)�D)�D*�D*� D+�D+��D,�D,}qD-  D-��D.  D.��D/�D/��D0�D0� D1  D1� D2  D2}qD3�D3��D4D4��D5�D5� D6  D6� D7�D7��D8  D8� D8�qD9}qD:�D:��D;�D;�D;�qD<}qD=D=�D>�D>� D?  D?��D?��D@z�DA  DA� DB  DB�DC�DC��DDDD��DE�DE� DF  DF� DG  DG� DH  DHz�DH�qDI� DI�qDJ��DK�DK� DL  DL� DM�DM�DNDN��DO�DO}qDP  DP��DQ�DQ� DQ�qDR}qDS  DS}qDT�DT�DU  DU}qDV  DV}qDW�DW�DXDX� DX�qDY}qDY��DZz�D[�D[��D[�qD\z�D\�qD]��D^  D^� D^�qD_}qD_��D`z�D`�qDa� Db  Db� Dc  Dc� Dd  Ddz�Dd�qDe��Df�Df}qDf�RDgz�Dh  Dh�Di  Di}qDj  Dj� Dk�Dk�Dl�Dl� Dl�qDm� Dn�Dn� Dn�qDo� Dp  Dp}qDp�qDq� Dq��Dr}qDr�qDs}qDt�Dt}qDt�qDu� Du�qDv}qDv�qDw��Dx  Dx� Dy  Dy}qDy�qDz� Dz�qD{z�D|  D|}qD|��D}� D}�qD~}qD~��D}qD�HD�AHD�� D�� D���D�>�D�~�D�� D�HD�@ D�� D���D���D�>�D�� D�� D���D�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�� D���D��qD�@ D��HD���D���D�@ D�� D�� D�  D�=qD�~�D��HD���D�>�D��HD��HD���D�>�D�~�D���D�  D�@ D��HD�� D�HD�B�D��HD�� D�  D�B�D�� D�� D�  D�@ D�~�D���D�HD�AHD�� D�� D�HD�AHD�� D��HD�HD�@ D�� D�� D�  D�AHD��HD���D��qD�@ D���D��HD�  D�@ D�~�D���D�  D�>�D�� D�� D���D�>�D�� D���D�  D�@ D�~�D�� D���D�=qD�~�D��qD��qD�@ D��HD���D��D�@ D�� D�� D�  D�@ D�� D�� D�  D�B�D���D�D�  D�=qD�� D�� D�  D�@ D�� D�� D��qD�=qD�� D�� D�  D�B�D��HD��HD�HD�=qD�}qD��HD�HD�>�D�}qD��qD��)D�>�D�~�D���D���D�=qD�~�D��HD�HD�>�D�|)D��qD�  D�@ D��HD�D�  D�@ D��HD��HD��D�@ D��HD��HD�  D�>�D�� D��HD�  D�>�D�}qD�� D�HD�@ D�~�D�� D��D�B�D�~�D���D�HD�>�D�~�D���D���D�AHD���D��HD�  D�=qD�~�D�� D�HD�AHD�~�D��)D��qD�>�D�� D���D��qD�@ D�~�D��qD�  D�AHD��HD��qD��qD�=qD�}qD�� D���D�@ D�� D�� D���D�>�D�� D��qD��qD�AHD��HD��HD�HD�@ D�� D��HD���D�>�D�� D��qD���D�@ D�~�D�� D�  D�>�D�� D��HD�  D�AHD���D�� D�  D�AHD��HD��HD�  D�@ D�~�D���D���D�=qD�� D��HD���D�>�D�~�D¾�D���D�@ DÀ D�� D�HD�AHDāHD��HD�HD�AHDŀ D�� D�HD�>�Dƀ D�D�HD�>�D�~�D�� D���D�@ DȀ D��HD�HD�>�Dɀ D��HD�  D�AHDʁHD�� D���D�>�DˁHD��HD�HD�@ D̀ D̾�D���D�@ D́HD��HD�HD�AHD�~�D�� D�  D�AHDρHD��HD�HD�AHDЁHDо�D�  D�@ Dр DѾ�D���D�@ DҀ D��HD�HD�@ DӀ DӾ�D�HD�B�DԀ DԾ�D���D�@ DՁHD��HD�  D�>�Dր D־�D���D�@ D׀ D��HD�  D�@ D�~�Dؾ�D���D�>�D�~�D�� D�HD�@ Dڀ Dھ�D���D�AHDہHD�� D�  D�@ D܀ Dܾ�D���D�>�D݀ D�� D��qD�>�D�}qD޽qD�  D�@ D߀ D��HD�HD�@ D�� D�� D�  D�@ D� D�� D�HD�@ D� D�� D�  D�AHDわD�� D�  D�AHD� D侸D�HD�@ D�}qD�qD��qD�@ D�HD澸D�  D�@ D�~�D羸D���D�>�D� D�� D���D�@ D�HD��HD�HD�@ D� D��HD�  D�@ D�HD뾸D�  D�AHD� D쾸D��qD�>�D� D��HD�HD�AHD�HDD�  D�@ D� D��HD�HD�>�D�~�D�� D���D�>�D� D��HD�HD�AHD�~�D�D�  D�>�D�}qD�D�  D�@ D�~�D��HD��D�B�D��HD��HD�HD�AHD�� D�� D�  D�@ D��HD��HD���D�@ D��HD��HD�HD�AHD��HD�� D�  D�@ D�xR>�G�?\)?u?�\)?\?�(�@�@z�@.{@@  @Y��@c�
@�  @�ff@�33@�(�@�ff@���@���@��@���@ٙ�@�G�@�{@�33A   Az�A
=qAp�A�
A�A��A"�\A'
=A,(�A0  A6ffA9��A@  AC33AJ=qAMp�AS�
AW
=A]p�AaG�AhQ�Ak�Ar�\AuA|��A�Q�A�33A�A�Q�A��HA��A�  A�=qA�p�A�
=A��\A�(�A�\)A���A���A�ffA���A��
A��RA���A��A�ffA���A��
A�A���A��HA�{A�  A�33A��A�Q�Aҏ\A�p�A�  A�=qA��A�\)A�\A�(�A�A�G�A���A�RA�A�(�A��RA��A��
A�
=B z�B{B33Bz�B�B�HB��B	��B33B(�B�B�RBz�Bp�B
=BQ�B��B
=B  BB�\BQ�BG�B�HB   B!G�B"�HB#�
B%p�B&ffB((�B)�B*�\B+�
B,��B.�RB/�B1�B2=qB3�B5�B6{B7�
B8��B:ffB;\)B<��B>{B?33B@��BABC\)BD(�BF{BF�RBHz�BI��BJ�HBLQ�BMG�BN�HBO�BQp�BR�\BS�
BUG�BV=qBW�
BX��BZ�\B[�B\��B^ffB_\)B`��BaBc�Bdz�Bf=qBg
=Bh��Bip�Bk33Bl(�Bmp�Bn�HBo�
Bqp�BrffBt  Bu�Bv=qBw�
Bx��Bz=qB{33B|��B~{B\)B�ffB��HB��B�(�B���B���B�(�B��HB�\)B�=qB���B��B�(�B���B���B�{B��HB��B�(�B���B�p�B�=qB��RB�p�B�(�B��RB��B�  B���B�\)B��B���B�G�B�{B��RB�G�B�(�B��\B��B�  B���B�p�B�{B���B�p�B�Q�B���B��B�ffB��HB���B�ffB��HB�B�Q�B��HB�B�=qB�
=B��
B�Q�B�33B�B�Q�B�33B��B�z�B�33B��
B���B��B�{B��RB�\)B�=qB��RB���B�=qB���B�B�Q�B��B��
B�z�B�\)B��
B���B�G�B��B��HB�\)B�=qB��HB���B�Q�B���B��
B�ffB�
=B�  B�z�B�G�B�{BƏ\B�p�B�{Bȣ�BɅB�{BʸRB˙�B�(�B�
=B�B�ffB�G�B��BЏ\B�p�B�  B���BӮB�=qB�
=B��
B�ffB�33B�{B؏\B�p�B�(�BڸRBۮB�Q�B�
=B��B�z�B�\)B�(�B�RB�B�Q�B�
=B�  B�\B�p�B�=qB�RB�B�z�B��B�(�B�RB�B�z�B���B��
B��B�\)B�Q�B��HB�B�z�B�
=B�  B���B�\)B�Q�B���B��B��\B��B��
B���B�\)B�{B���B��B�ffB�33B�C Q�C C
=Cp�C�C33C��C{CffCC=qC�\C�Cp�CC�C��C�CG�CC
=Cz�C�C	=qC	��C
{C
ffC
C=qC��C�CffC�C{C�C��C=qC��C��CffC��C{Cz�C�C33C��C  C=qC��C��C(�Cz�C�
C  CQ�C�\C�RC
=C(�C\)C�C��C��CG�Cp�C��C�C
=C=qC�\C�RC�HC(�CffC�C�
C
=C(�Cp�C�C�
C�C\)Cz�C��C  C(�Cp�C�RC�HC{CffC�\C�RC{C=qCffC�RC�C�Cp�C��CC{CQ�Cz�C��C   C (�C z�C ��C �
C!(�C!\)C!�C!�
C"
=C"=qC"�C"C"�C#33C#z�C#��C#�HC$(�C$Q�C$�C$�
C%{C%33C%�C%��C%�C&33C&�C&��C&�C'33C'\)C'��C'��C({C(\)C(�C(��C){C)ffC)�\C)��C*�C*G�C*z�C*�
C+  C+=qC+�\C+��C+��C,=qC,�C,�C-  C-=qC-p�C-�RC.  C.(�C.z�C.C.�C/(�C/z�C/�C/�HC0=qC0p�C0��C0�C133C1ffC1��C1�HC2(�C2\)C2�\C2�C3�C3G�C3��C3�
C4
=C4=qC4�\C4��C4��C5(�C5z�C5C5�HC6�C6p�C6�C6��C7{C7ffC7�\C7��C8�C8G�C8z�C8�
C9{C933C9z�C9��C9�C:(�C:z�C:C:��C;�C;p�C;�RC;�HC<{C<\)C<��C<�HC=
=C=Q�C=��C=C>  C>G�C>��C>�RC>��C?G�C?�\C?C?�C@=qC@�C@�C@�CA=qCA�CA�RCA�CB(�CBz�CBCB�CC�CCp�CC�RCC��CD�CD\)CD�CD��CE�CE\)CE��CE�CF�CFQ�CF��CF�HCG�CG=qCG�\CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                 ?u?�@@  @�G�@��R@��R@�  @��RA��A!G�A,��A@��A`  A�  A�  A�  A�
=A�\)A�  A�Q�A�Q�B   B�
B  B�
B (�B((�B0  B7�
B@  BH  BP  BX  B_�
Bg�
Bp  Bw�
B�  B�  B�  B�  B�  B��B��
B�  B�{B�(�B�  B�  B�  B��B�  B�  B��B��
B��
B��B��B��B��B�  B�{B�{B��B��B�(�B�(�B�{B�  B��C��C  C{C  C

=C
=C
=C
=C
=C  C��C  C�C��C��C��C!��C$  C&
=C(  C)��C+��C-��C0  C2  C4
=C6  C7�C:  C<  C>
=C@
=CB
=CD  CF  CH  CJ  CL
=CM��CP  CR  CS��CV  CX
=CY��C[��C]��C_��Ca�Cc�Cf  Ch
=Cj  Cl  Cm�Co��Cr
=Cs��Cu��Cx
=Cz�C|{C~
=C�
=C�C�C�C�C���C���C�C�C���C���C�C�
=C�  C���C�  C�C�
=C�  C�  C�  C���C���C�  C�C�
=C�C�C�
=C�  C�  C�  C�  C�C�  C�C�
=C�C�  C���C���C�C�
=C�C�  C�C�  C���C���C���C�  C�
=C�
=C�
=C�C�  C���C���C�  C���C���C���C�  C���C�  C�C�  C���C���C�  C�
=C�C�  C�  C���C���C���C�C�C�C�C���C�  C���C���C�C�
=C�C�C�C���C�  C���C���C�
=C�
=C�  C�  C�C�C�  C�  C�
=C�
=C���C�  C�  C�  C�  C�C���C���C�  C�  C���C���C�C�C�C�C�C�
=C�C���C�  C�  C�  C�  D   D ��D  D��D�Dz�D  D� D��D� D�D}qD�qD� D�D��D�D��D	D	� D	�qD
� D  D� D�D��D�D}qD��Dz�D  D}qD�RD}qD  D}qD  D��D  D}qD�qD� D�qD� D�D� D�D�D�qDz�D  D��D�qD}qD�qDz�D  D�DD� D�D��D�qD� D �D }qD �qD!� D"  D"� D#  D#��D#�qD$}qD%  D%��D&  D&z�D'  D'� D(�D(��D)�D)�D*�D*� D+�D+��D,�D,}qD-  D-��D.  D.��D/�D/��D0�D0� D1  D1� D2  D2}qD3�D3��D4D4��D5�D5� D6  D6� D7�D7��D8  D8� D8�qD9}qD:�D:��D;�D;�D;�qD<}qD=D=�D>�D>� D?  D?��D?��D@z�DA  DA� DB  DB�DC�DC��DDDD��DE�DE� DF  DF� DG  DG� DH  DHz�DH�qDI� DI�qDJ��DK�DK� DL  DL� DM�DM�DNDN��DO�DO}qDP  DP��DQ�DQ� DQ�qDR}qDS  DS}qDT�DT�DU  DU}qDV  DV}qDW�DW�DXDX� DX�qDY}qDY��DZz�D[�D[��D[�qD\z�D\�qD]��D^  D^� D^�qD_}qD_��D`z�D`�qDa� Db  Db� Dc  Dc� Dd  Ddz�Dd�qDe��Df�Df}qDf�RDgz�Dh  Dh�Di  Di}qDj  Dj� Dk�Dk�Dl�Dl� Dl�qDm� Dn�Dn� Dn�qDo� Dp  Dp}qDp�qDq� Dq��Dr}qDr�qDs}qDt�Dt}qDt�qDu� Du�qDv}qDv�qDw��Dx  Dx� Dy  Dy}qDy�qDz� Dz�qD{z�D|  D|}qD|��D}� D}�qD~}qD~��D}qD�HD�AHD�� D�� D���D�>�D�~�D�� D�HD�@ D�� D���D���D�>�D�� D�� D���D�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�� D���D��qD�@ D��HD���D���D�@ D�� D�� D�  D�=qD�~�D��HD���D�>�D��HD��HD���D�>�D�~�D���D�  D�@ D��HD�� D�HD�B�D��HD�� D�  D�B�D�� D�� D�  D�@ D�~�D���D�HD�AHD�� D�� D�HD�AHD�� D��HD�HD�@ D�� D�� D�  D�AHD��HD���D��qD�@ D���D��HD�  D�@ D�~�D���D�  D�>�D�� D�� D���D�>�D�� D���D�  D�@ D�~�D�� D���D�=qD�~�D��qD��qD�@ D��HD���D��D�@ D�� D�� D�  D�@ D�� D�� D�  D�B�D���D�D�  D�=qD�� D�� D�  D�@ D�� D�� D��qD�=qD�� D�� D�  D�B�D��HD��HD�HD�=qD�}qD��HD�HD�>�D�}qD��qD��)D�>�D�~�D���D���D�=qD�~�D��HD�HD�>�D�|)D��qD�  D�@ D��HD�D�  D�@ D��HD��HD��D�@ D��HD��HD�  D�>�D�� D��HD�  D�>�D�}qD�� D�HD�@ D�~�D�� D��D�B�D�~�D���D�HD�>�D�~�D���D���D�AHD���D��HD�  D�=qD�~�D�� D�HD�AHD�~�D��)D��qD�>�D�� D���D��qD�@ D�~�D��qD�  D�AHD��HD��qD��qD�=qD�}qD�� D���D�@ D�� D�� D���D�>�D�� D��qD��qD�AHD��HD��HD�HD�@ D�� D��HD���D�>�D�� D��qD���D�@ D�~�D�� D�  D�>�D�� D��HD�  D�AHD���D�� D�  D�AHD��HD��HD�  D�@ D�~�D���D���D�=qD�� D��HD���D�>�D�~�D¾�D���D�@ DÀ D�� D�HD�AHDāHD��HD�HD�AHDŀ D�� D�HD�>�Dƀ D�D�HD�>�D�~�D�� D���D�@ DȀ D��HD�HD�>�Dɀ D��HD�  D�AHDʁHD�� D���D�>�DˁHD��HD�HD�@ D̀ D̾�D���D�@ D́HD��HD�HD�AHD�~�D�� D�  D�AHDρHD��HD�HD�AHDЁHDо�D�  D�@ Dр DѾ�D���D�@ DҀ D��HD�HD�@ DӀ DӾ�D�HD�B�DԀ DԾ�D���D�@ DՁHD��HD�  D�>�Dր D־�D���D�@ D׀ D��HD�  D�@ D�~�Dؾ�D���D�>�D�~�D�� D�HD�@ Dڀ Dھ�D���D�AHDہHD�� D�  D�@ D܀ Dܾ�D���D�>�D݀ D�� D��qD�>�D�}qD޽qD�  D�@ D߀ D��HD�HD�@ D�� D�� D�  D�@ D� D�� D�HD�@ D� D�� D�  D�AHDわD�� D�  D�AHD� D侸D�HD�@ D�}qD�qD��qD�@ D�HD澸D�  D�@ D�~�D羸D���D�>�D� D�� D���D�@ D�HD��HD�HD�@ D� D��HD�  D�@ D�HD뾸D�  D�AHD� D쾸D��qD�>�D� D��HD�HD�AHD�HDD�  D�@ D� D��HD�HD�>�D�~�D�� D���D�>�D� D��HD�HD�AHD�~�D�D�  D�>�D�}qD�D�  D�@ D�~�D��HD��D�B�D��HD��HD�HD�AHD�� D�� D�  D�@ D��HD��HD���D�@ D��HD��HD�HD�AHD��HD�� D�  D�@ G�O�>�G�?\)?u?�\)?\?�(�@�@z�@.{@@  @Y��@c�
@�  @�ff@�33@�(�@�ff@���@���@��@���@ٙ�@�G�@�{@�33A   Az�A
=qAp�A�
A�A��A"�\A'
=A,(�A0  A6ffA9��A@  AC33AJ=qAMp�AS�
AW
=A]p�AaG�AhQ�Ak�Ar�\AuA|��A�Q�A�33A�A�Q�A��HA��A�  A�=qA�p�A�
=A��\A�(�A�\)A���A���A�ffA���A��
A��RA���A��A�ffA���A��
A�A���A��HA�{A�  A�33A��A�Q�Aҏ\A�p�A�  A�=qA��A�\)A�\A�(�A�A�G�A���A�RA�A�(�A��RA��A��
A�
=B z�B{B33Bz�B�B�HB��B	��B33B(�B�B�RBz�Bp�B
=BQ�B��B
=B  BB�\BQ�BG�B�HB   B!G�B"�HB#�
B%p�B&ffB((�B)�B*�\B+�
B,��B.�RB/�B1�B2=qB3�B5�B6{B7�
B8��B:ffB;\)B<��B>{B?33B@��BABC\)BD(�BF{BF�RBHz�BI��BJ�HBLQ�BMG�BN�HBO�BQp�BR�\BS�
BUG�BV=qBW�
BX��BZ�\B[�B\��B^ffB_\)B`��BaBc�Bdz�Bf=qBg
=Bh��Bip�Bk33Bl(�Bmp�Bn�HBo�
Bqp�BrffBt  Bu�Bv=qBw�
Bx��Bz=qB{33B|��B~{B\)B�ffB��HB��B�(�B���B���B�(�B��HB�\)B�=qB���B��B�(�B���B���B�{B��HB��B�(�B���B�p�B�=qB��RB�p�B�(�B��RB��B�  B���B�\)B��B���B�G�B�{B��RB�G�B�(�B��\B��B�  B���B�p�B�{B���B�p�B�Q�B���B��B�ffB��HB���B�ffB��HB�B�Q�B��HB�B�=qB�
=B��
B�Q�B�33B�B�Q�B�33B��B�z�B�33B��
B���B��B�{B��RB�\)B�=qB��RB���B�=qB���B�B�Q�B��B��
B�z�B�\)B��
B���B�G�B��B��HB�\)B�=qB��HB���B�Q�B���B��
B�ffB�
=B�  B�z�B�G�B�{BƏ\B�p�B�{Bȣ�BɅB�{BʸRB˙�B�(�B�
=B�B�ffB�G�B��BЏ\B�p�B�  B���BӮB�=qB�
=B��
B�ffB�33B�{B؏\B�p�B�(�BڸRBۮB�Q�B�
=B��B�z�B�\)B�(�B�RB�B�Q�B�
=B�  B�\B�p�B�=qB�RB�B�z�B��B�(�B�RB�B�z�B���B��
B��B�\)B�Q�B��HB�B�z�B�
=B�  B���B�\)B�Q�B���B��B��\B��B��
B���B�\)B�{B���B��B�ffB�33B�C Q�C C
=Cp�C�C33C��C{CffCC=qC�\C�Cp�CC�C��C�CG�CC
=Cz�C�C	=qC	��C
{C
ffC
C=qC��C�CffC�C{C�C��C=qC��C��CffC��C{Cz�C�C33C��C  C=qC��C��C(�Cz�C�
C  CQ�C�\C�RC
=C(�C\)C�C��C��CG�Cp�C��C�C
=C=qC�\C�RC�HC(�CffC�C�
C
=C(�Cp�C�C�
C�C\)Cz�C��C  C(�Cp�C�RC�HC{CffC�\C�RC{C=qCffC�RC�C�Cp�C��CC{CQ�Cz�C��C   C (�C z�C ��C �
C!(�C!\)C!�C!�
C"
=C"=qC"�C"C"�C#33C#z�C#��C#�HC$(�C$Q�C$�C$�
C%{C%33C%�C%��C%�C&33C&�C&��C&�C'33C'\)C'��C'��C({C(\)C(�C(��C){C)ffC)�\C)��C*�C*G�C*z�C*�
C+  C+=qC+�\C+��C+��C,=qC,�C,�C-  C-=qC-p�C-�RC.  C.(�C.z�C.C.�C/(�C/z�C/�C/�HC0=qC0p�C0��C0�C133C1ffC1��C1�HC2(�C2\)C2�\C2�C3�C3G�C3��C3�
C4
=C4=qC4�\C4��C4��C5(�C5z�C5C5�HC6�C6p�C6�C6��C7{C7ffC7�\C7��C8�C8G�C8z�C8�
C9{C933C9z�C9��C9�C:(�C:z�C:C:��C;�C;p�C;�RC;�HC<{C<\)C<��C<�HC=
=C=Q�C=��C=C>  C>G�C>��C>�RC>��C?G�C?�\C?C?�C@=qC@�C@�C@�CA=qCA�CA�RCA�CB(�CBz�CBCB�CC�CCp�CC�RCC��CD�CD\)CD�CD��CE�CE\)CE��CE�CF�CFQ�CF��CF�HCG�CG=qCG�\CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                 @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�  A�VA�{A�bA�oA�bA�bA�VA�{A�oA�VA��A�VA��A��A��`A��A��
A���A��
A��#A��/A��;A��TA��TA��`A��`A��mA��mA��`A��HA���AȺ^AȶFA���Aȡ�A�E�A���A�x�A�dZA�A��#A�\)A���A�ZA��#A�oA�I�A�XA�
=A���A�  A�bNA�(�A���A�9XA���A��A�$�A���A�ZA� �A�bNA��A�1A�p�A�|�A��A�$�A�~�A��A��TA�-A��A��7A���A��A�"�A��/A�O�A�ĜA�S�A��`A��A�5?A���A�ƨA��;A��A�/A�%A��`A�33A���A�
=A��9A�hsA�Q�A�%A��A�ƨA�ĜA��A�t�A�;dA���A���A��yA�A��!A�jA%A}��Az�\Ax��Axv�Aw�^Av-As7LAq�Aq��An�yAj�jAe�;Ad5?Ab��A^AZ �AX�RAW;dAU�wAS�
AO�mAO�FAN�`AN��ANQ�AK��AJ��AI��AHJAG;dAFĜAF�!AFbAEAD�AB�\A?oA<�HA:��A9A7�mA6ZA5��A3|�A1+A.$�A+�TA+33A*�jA)O�A(5?A(bA'�
A'XA'
=A&��A%�A%"�A$M�A"E�A!��A!?}A ��A?}A��Ar�A�TA�uAG�A^5A�;AO�A�AbNA�A�A9XA�A�;A�`Ar�AI�A��At�A/A��A�A�wA��A~�AI�AJA�mA��A�7A�Av�AQ�A$�A�A�
Ax�A
�`A
�\A
Q�A
(�A	��A	G�AƨA�9A9XAl�A�A��AC�Ar�A-A�wA ��@��m@�dZ@���@�%@�S�@��@�w@��@�\@���@�p�@� �@���@�\@�J@�7L@��@�+@��@�x�@���@�9@�Ĝ@���@��@�D@�+@���@�5?@ܓu@�V@�@ٲ-@ٲ-@١�@��@�z�@���@�"�@ա�@�|�@��H@���@�?}@��`@�|�@ΰ!@�`B@�I�@�S�@ʏ\@���@�l�@ēu@�ƨ@���@��h@��9@�@���@��@�j@�
=@�V@��@���@��@�"�@�+@�+@��H@�O�@�t�@�^5@�G�@�9X@��w@�|�@�\)@�@�E�@���@��7@�p�@�X@�V@�j@�  @��@���@�=q@���@�hs@�&�@��j@��@� �@��P@�t�@��@���@���@���@���@��+@�v�@��!@�ȴ@�n�@�J@��@�?}@�&�@���@�A�@�ƨ@�l�@�;d@�+@�C�@�@���@��\@�v�@�ff@�v�@�V@�J@��@��@��T@�@�x�@���@�@��7@�7L@���@���@��
@���@�o@���@���@�n�@�hs@��j@��@�A�@�1@��m@�33@�@���@���@��+@�v�@�5?@�@�&�@���@� �@���@���@��@���@�t�@�@��@���@���@��+@�^5@���@�\)@�
=@��y@���@��+@��R@���@���@�n�@�J@�{@�$�@�5?@�E�@��@���@��#@���@���@���@�7L@���@��9@�1'@�1@��w@�l�@�dZ@�dZ@�K�@�
=@���@��\@��!@���@���@�v�@�@���@���@�Z@���@���@�K�@�C�@�;d@�C�@�C�@�+@�
=@��H@��!@��+@�E�@��@��#@��@��u@�z�@�r�@�  @���@�|�@�l�@�;d@�"�@���@��y@��H@��H@��@�ȴ@��!@�n�@�M�@�-@�@��@���@�`B@��@�%@���@��@���@��@�A�@�b@�ƨ@�\)@��@�V@�J@��T@���@��-@��@�hs@��@��`@��/@��@��u@�j@��@���@�dZ@�S�@�;d@�
=@��H@���@���@���@���@��+@��@�x�@�G�@�%@��`@�Ĝ@��u@� �@�b@�b@|�@�@~�R@~�+@~V@~@}�-@}�@|��@|Z@|1@{��@{C�@z��@z��@z�!@z��@z�\@y��@y�^@y7L@x1'@w��@w�@w�P@wl�@wl�@wl�@w\)@w;d@w+@v��@vȴ@v��@v��@vv�@vE�@v{@u��@u�h@up�@t�/@t�@t9X@sC�@r��@rn�@q��@pĜ@p�u@pA�@o��@o�@o�P@o|�@oK�@o+@n��@n�@n��@nv�@nv�@nE�@m@mp�@m?}@l�/@l�/@lj@k�
@k�F@kt�@k33@j�@j��@j�\@jn�@i��@i��@i7L@h�u@hb@g�;@g�@g�P@g\)@g+@f��@fE�@e�T@e��@e/@d�j@dz�@d(�@c��@cC�@c@b�\@a��@a��@aG�@a&�@`��@`Ĝ@`��@`1'@_�;@_\)@_+@^��@^��@^V@^@]�-@]/@\�/@\�@\I�@[�@Z�!@ZM�@Z�@Y�#@Y�^@Y��@YX@XĜ@XbN@W�w@V�+@V5?@U��@U�@UV@UV@T��@TZ@S��@SS�@So@R�H@R�\@R�@Q�@Q��@QG�@Q�@PĜ@PQ�@P  @O�P@O+@N�+@M�-@M��@M�@Mp�@M�@Mp�@MV@L�j@L�@K"�@J^5@JM�@J�@I�@I�@I��@I�7@I�7@I�7@IG�@H�@G|�@F��@Fȴ@F��@FV@E�h@D�j@DZ@D�@C�F@Ct�@Co@Bn�@A��@A�#@@�9@@ �@@  @?�P@?�@>��@>�R@>��@>E�@>@=�T@=@=p�@<�@<��@<��@<j@<�@;�
@;��@;S�@:�@:^5@9��@8Ĝ@8Q�@8 �@8  @7�w@7|�@7l�@7K�@7
=@6�@6��@6V@65?@5�@5�T@5�T@5�T@5��@5�@5O�@5?}@5/@4�j@4�D@4z�@4Z@4(�@4�@3�m@3��@3S�@3"�@2��@1��@1%@0�@0bN@01'@0  @/��@/|�@/|�@/|�@/|�@/l�@/�@.��@-��@-/@-V@,�j@,Z@,9X@,�@+��@+�m@+�
@+ƨ@+ƨ@+�F@+��@*=q@(��@'�@'l�@'l�@'l�@'l�@'\)@'l�@'l�@'l�@'+@&��@&�R@&ff@&5?@&{@%��@%?}@%V@$��@$(�@#��@#33@#33@#"�@#o@#o@#o@#@#@#@"�@"�@"�\@!�@!��@!x�@!�@ �`@ Ĝ@ �u@ r�@ bN@ A�@   @|�@
=@�@ȴ@ȴ@��@V@@�T@@�-@��@�@?}@��@j@Z@(�@�@��@�
@�F@��@t�@33@o@@��@=q@��@�#@�#@�#@��@hs@%@1'@ �@b@�@�;@��@�w@�@�P@l�@\)@K�@�@�y@v�@�@�T@��@��@@@�-@��@�h@�@`B@/@�j@�@��@��@�D@�D@z�@z�@z�@z�@z�@z�@z�@j@(�@ƨ@S�@@�H@�!@�@�@x�@Ĝ@��@bN@A�@ �@  @�@�@l�@�@�y@�y@�y@ȴ@�+@E�@�T@�-@�h@O�@�@�@V@�@��@z�@I�@�F@��@dZ@33@o@@
�H@
��@
n�@	��@	�#@	�#@	��@	x�@	hs@	X@	X@	X@	7L@	%@��@�9@�u@bN@1'@�@��@�w@��@��@�P@�P@�P@|�@l�@l�@\)@K�@
=@��@�y@�R@v�A��#A��yA�1A�A���A�
=A�{A��A��A�{A�oA�oA�bA�bA�bA�JA��A�bA�{A�JA�oA�VA��A�JA�
=A�bA�JA�oA�oA�{A�{A��A�bA�oA�bA�JA�JA��A��A��A��A��A��A�VA�oA�
=A�JA�1A��A��A��A��A��A��A���A��A���A��A��A��A��A��yA��yA��HA��TA��TA��/A��/A��
A��#A���A��#A���A��#A���A��A���A��
A��
A���A��
A���A��
A���A��
A���A��
A���A��A���A��A��
A��A��#A��
A��/A��
A��;A��A��;A��;A��#A��HA��#A��HA��#A��HA��/A��HA��;A��TA��HA��TA��TA��HA��`A��HA��mA��HA��mA��TA��mA��TA��`A��mA��TA��mA��HA��yA��TA��mA��`A��`A��yA��TA��yA��`A��mA��yA��`A��yA��`A��yA��`A��mA��mA��`A��mA��TA��yA��HA��mA��HA��`A��TA��HA��TA��;A��;A���A���A���A���A�ȴAȾwAȾwAȸRAȾwAȺ^AȼjAȺ^AȸRAȺ^AȰ!AȶFAȸRAȼjAȺ^AȾwAȾwA���A���A�ĜA�ƨA�A���A�ȴAȼjAȓuA�n�A�VA�M�A�M�A�I�A�M�A�C�A�"�A��A�  A�  A��A��A���Aǲ-A�G�A���A�9XA�ƨAŝ�AŅA�M�A��`A�z�AÃA�33A�JA�JA�A�%A���A���A���A��A��A��A���A¸RA�AhA�t�A�XA�=qA��A��mA�VA��A�z�A�
=A��mA�ƨA��\A�$�A��TA�5?A�1'A�(�A��A���A���A��FA��+A�bNA�"�A�
=A���A�A���A���A��`A��uA�7LA��FA�A�A� �A�A��mA�VA��A��+A��A���A���A�9XA��A��yA�ȴA���A�~�A�bNA�E�A�+A��A�%A���A��/A��^A���A��7A�ffA�?}A�$�A��A�A�t�A�A��uA�+A��A���A���A��uA��A�x�A�hsA�XA�=qA�oA���A��#A���A�ZA�ƨA�r�A���A�-A��;A��!A���A�VA�n�A�XA�5?A�
=A�  A��A��
A��wA���A���A�~�A�jA�r�A�l�A�bNA�I�A�A���A�ffA��A��/A��hA�p�A�jA�bNA�\)A�XA�S�A�S�A�?}A�1'A���A�5?A�A�
=A�JA�A��mA��A���A��!A�?}A��A�33A��A�r�A�=qA�%A��9A���A��DA�t�A�dZA�^5A�M�A�9XA�33A��A��RA��7A��A��A��A�hsA�hsA�dZA�E�A�JA���A�O�A��A�A��
A���A�v�A�`BA�I�A�33A�
=A���A��hA��yA�jA�&�A��TA�ĜA��A��7A�jA�M�A�"�A��A�oA��TA���A��RA���A��uA�~�A�XA�&�A�ƨA�|�A�A��!A�A�A�bA��A���A���A��wA��!A���A���A��DA�l�A�"�A�  A���A���A�=qA��A��DA���A�1'A���A��
A�ƨA��FA���A��+A��A�n�A�VA�O�A�"�A��A��A�1A���A���A��A��A��`A��HA���A�A��FA��RA��PA��A��A��A�r�A�dZA�Q�A�Q�A�K�A�C�A�G�A�=qA�-A�&�A��A�VA�1A���A��jA���A��!A���A�x�A�O�A�O�A� �A���A��`A��A���A���A�x�A�t�A�|�A�hsA�ffA��A��A��RA��FA�t�A�hsA��A��\A�r�A�5?A��A��mA��RA��hA��FA�ZA�E�A�?}A�5?A� �A��A�oA�oA�VA�
=A�A�
=A�  A�A�A�A�  A�  A���A��A��A��yA���A��9A���A�jA�`BA�Q�A�(�A��A�A��A��TA��TA��/A��
A��A���A�ȴA��FA�v�A�E�A�33A�$�A��A�bA���A���A��wA��jA��jA��jA��-A��!A��9A��A���A���A���A���A��\A��A�VA�9XA�$�A�{A���A��HA���A��A��A���A��9A��A�~�A�ffA�bA�A���A��A��;A���A���A��FA��A���A��A�|�A�r�A�dZA�A�A�"�A�
=A�  A��A��!A���A���A��A�`BA�;dA�1A��A���A��-A�|�A�\)A�=qA�;dA�;dA��A���A��
A��wA���A��uA��+A��+A�|�A�z�A�p�A�dZA�dZA�ZA�ZA�ZA�G�A�-A�+A�33A�33A�$�A��A�JA�JA�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                 A�  A�VA�{A�bA�oA�bA�bA�VA�{A�oA�VA��A�VA��A��A��`A��A��
A���A��
A��#A��/A��;A��TA��TA��`A��`A��mA��mA��`A��HA���AȺ^AȶFA���Aȡ�A�E�A���A�x�A�dZA�A��#A�\)A���A�ZA��#A�oA�I�A�XA�
=A���A�  A�bNA�(�A���A�9XA���A��A�$�A���A�ZA� �A�bNA��A�1A�p�A�|�A��A�$�A�~�A��A��TA�-A��A��7A���A��A�"�A��/A�O�A�ĜA�S�A��`A��A�5?A���A�ƨA��;A��A�/A�%A��`A�33A���A�
=A��9A�hsA�Q�A�%A��A�ƨA�ĜA��A�t�A�;dA���A���A��yA�A��!A�jA%A}��Az�\Ax��Axv�Aw�^Av-As7LAq�Aq��An�yAj�jAe�;Ad5?Ab��A^AZ �AX�RAW;dAU�wAS�
AO�mAO�FAN�`AN��ANQ�AK��AJ��AI��AHJAG;dAFĜAF�!AFbAEAD�AB�\A?oA<�HA:��A9A7�mA6ZA5��A3|�A1+A.$�A+�TA+33A*�jA)O�A(5?A(bA'�
A'XA'
=A&��A%�A%"�A$M�A"E�A!��A!?}A ��A?}A��Ar�A�TA�uAG�A^5A�;AO�A�AbNA�A�A9XA�A�;A�`Ar�AI�A��At�A/A��A�A�wA��A~�AI�AJA�mA��A�7A�Av�AQ�A$�A�A�
Ax�A
�`A
�\A
Q�A
(�A	��A	G�AƨA�9A9XAl�A�A��AC�Ar�A-A�wA ��@��m@�dZ@���@�%@�S�@��@�w@��@�\@���@�p�@� �@���@�\@�J@�7L@��@�+@��@�x�@���@�9@�Ĝ@���@��@�D@�+@���@�5?@ܓu@�V@�@ٲ-@ٲ-@١�@��@�z�@���@�"�@ա�@�|�@��H@���@�?}@��`@�|�@ΰ!@�`B@�I�@�S�@ʏ\@���@�l�@ēu@�ƨ@���@��h@��9@�@���@��@�j@�
=@�V@��@���@��@�"�@�+@�+@��H@�O�@�t�@�^5@�G�@�9X@��w@�|�@�\)@�@�E�@���@��7@�p�@�X@�V@�j@�  @��@���@�=q@���@�hs@�&�@��j@��@� �@��P@�t�@��@���@���@���@���@��+@�v�@��!@�ȴ@�n�@�J@��@�?}@�&�@���@�A�@�ƨ@�l�@�;d@�+@�C�@�@���@��\@�v�@�ff@�v�@�V@�J@��@��@��T@�@�x�@���@�@��7@�7L@���@���@��
@���@�o@���@���@�n�@�hs@��j@��@�A�@�1@��m@�33@�@���@���@��+@�v�@�5?@�@�&�@���@� �@���@���@��@���@�t�@�@��@���@���@��+@�^5@���@�\)@�
=@��y@���@��+@��R@���@���@�n�@�J@�{@�$�@�5?@�E�@��@���@��#@���@���@���@�7L@���@��9@�1'@�1@��w@�l�@�dZ@�dZ@�K�@�
=@���@��\@��!@���@���@�v�@�@���@���@�Z@���@���@�K�@�C�@�;d@�C�@�C�@�+@�
=@��H@��!@��+@�E�@��@��#@��@��u@�z�@�r�@�  @���@�|�@�l�@�;d@�"�@���@��y@��H@��H@��@�ȴ@��!@�n�@�M�@�-@�@��@���@�`B@��@�%@���@��@���@��@�A�@�b@�ƨ@�\)@��@�V@�J@��T@���@��-@��@�hs@��@��`@��/@��@��u@�j@��@���@�dZ@�S�@�;d@�
=@��H@���@���@���@���@��+@��@�x�@�G�@�%@��`@�Ĝ@��u@� �@�b@�b@|�@�@~�R@~�+@~V@~@}�-@}�@|��@|Z@|1@{��@{C�@z��@z��@z�!@z��@z�\@y��@y�^@y7L@x1'@w��@w�@w�P@wl�@wl�@wl�@w\)@w;d@w+@v��@vȴ@v��@v��@vv�@vE�@v{@u��@u�h@up�@t�/@t�@t9X@sC�@r��@rn�@q��@pĜ@p�u@pA�@o��@o�@o�P@o|�@oK�@o+@n��@n�@n��@nv�@nv�@nE�@m@mp�@m?}@l�/@l�/@lj@k�
@k�F@kt�@k33@j�@j��@j�\@jn�@i��@i��@i7L@h�u@hb@g�;@g�@g�P@g\)@g+@f��@fE�@e�T@e��@e/@d�j@dz�@d(�@c��@cC�@c@b�\@a��@a��@aG�@a&�@`��@`Ĝ@`��@`1'@_�;@_\)@_+@^��@^��@^V@^@]�-@]/@\�/@\�@\I�@[�@Z�!@ZM�@Z�@Y�#@Y�^@Y��@YX@XĜ@XbN@W�w@V�+@V5?@U��@U�@UV@UV@T��@TZ@S��@SS�@So@R�H@R�\@R�@Q�@Q��@QG�@Q�@PĜ@PQ�@P  @O�P@O+@N�+@M�-@M��@M�@Mp�@M�@Mp�@MV@L�j@L�@K"�@J^5@JM�@J�@I�@I�@I��@I�7@I�7@I�7@IG�@H�@G|�@F��@Fȴ@F��@FV@E�h@D�j@DZ@D�@C�F@Ct�@Co@Bn�@A��@A�#@@�9@@ �@@  @?�P@?�@>��@>�R@>��@>E�@>@=�T@=@=p�@<�@<��@<��@<j@<�@;�
@;��@;S�@:�@:^5@9��@8Ĝ@8Q�@8 �@8  @7�w@7|�@7l�@7K�@7
=@6�@6��@6V@65?@5�@5�T@5�T@5�T@5��@5�@5O�@5?}@5/@4�j@4�D@4z�@4Z@4(�@4�@3�m@3��@3S�@3"�@2��@1��@1%@0�@0bN@01'@0  @/��@/|�@/|�@/|�@/|�@/l�@/�@.��@-��@-/@-V@,�j@,Z@,9X@,�@+��@+�m@+�
@+ƨ@+ƨ@+�F@+��@*=q@(��@'�@'l�@'l�@'l�@'l�@'\)@'l�@'l�@'l�@'+@&��@&�R@&ff@&5?@&{@%��@%?}@%V@$��@$(�@#��@#33@#33@#"�@#o@#o@#o@#@#@#@"�@"�@"�\@!�@!��@!x�@!�@ �`@ Ĝ@ �u@ r�@ bN@ A�@   @|�@
=@�@ȴ@ȴ@��@V@@�T@@�-@��@�@?}@��@j@Z@(�@�@��@�
@�F@��@t�@33@o@@��@=q@��@�#@�#@�#@��@hs@%@1'@ �@b@�@�;@��@�w@�@�P@l�@\)@K�@�@�y@v�@�@�T@��@��@@@�-@��@�h@�@`B@/@�j@�@��@��@�D@�D@z�@z�@z�@z�@z�@z�@z�@j@(�@ƨ@S�@@�H@�!@�@�@x�@Ĝ@��@bN@A�@ �@  @�@�@l�@�@�y@�y@�y@ȴ@�+@E�@�T@�-@�h@O�@�@�@V@�@��@z�@I�@�F@��@dZ@33@o@@
�H@
��@
n�@	��@	�#@	�#@	��@	x�@	hs@	X@	X@	X@	7L@	%@��@�9@�u@bN@1'@�@��@�w@��@��@�P@�P@�P@|�@l�@l�@\)@K�@
=@��@�y@�RG�O�A��#A��yA�1A�A���A�
=A�{A��A��A�{A�oA�oA�bA�bA�bA�JA��A�bA�{A�JA�oA�VA��A�JA�
=A�bA�JA�oA�oA�{A�{A��A�bA�oA�bA�JA�JA��A��A��A��A��A��A�VA�oA�
=A�JA�1A��A��A��A��A��A��A���A��A���A��A��A��A��A��yA��yA��HA��TA��TA��/A��/A��
A��#A���A��#A���A��#A���A��A���A��
A��
A���A��
A���A��
A���A��
A���A��
A���A��A���A��A��
A��A��#A��
A��/A��
A��;A��A��;A��;A��#A��HA��#A��HA��#A��HA��/A��HA��;A��TA��HA��TA��TA��HA��`A��HA��mA��HA��mA��TA��mA��TA��`A��mA��TA��mA��HA��yA��TA��mA��`A��`A��yA��TA��yA��`A��mA��yA��`A��yA��`A��yA��`A��mA��mA��`A��mA��TA��yA��HA��mA��HA��`A��TA��HA��TA��;A��;A���A���A���A���A�ȴAȾwAȾwAȸRAȾwAȺ^AȼjAȺ^AȸRAȺ^AȰ!AȶFAȸRAȼjAȺ^AȾwAȾwA���A���A�ĜA�ƨA�A���A�ȴAȼjAȓuA�n�A�VA�M�A�M�A�I�A�M�A�C�A�"�A��A�  A�  A��A��A���Aǲ-A�G�A���A�9XA�ƨAŝ�AŅA�M�A��`A�z�AÃA�33A�JA�JA�A�%A���A���A���A��A��A��A���A¸RA�AhA�t�A�XA�=qA��A��mA�VA��A�z�A�
=A��mA�ƨA��\A�$�A��TA�5?A�1'A�(�A��A���A���A��FA��+A�bNA�"�A�
=A���A�A���A���A��`A��uA�7LA��FA�A�A� �A�A��mA�VA��A��+A��A���A���A�9XA��A��yA�ȴA���A�~�A�bNA�E�A�+A��A�%A���A��/A��^A���A��7A�ffA�?}A�$�A��A�A�t�A�A��uA�+A��A���A���A��uA��A�x�A�hsA�XA�=qA�oA���A��#A���A�ZA�ƨA�r�A���A�-A��;A��!A���A�VA�n�A�XA�5?A�
=A�  A��A��
A��wA���A���A�~�A�jA�r�A�l�A�bNA�I�A�A���A�ffA��A��/A��hA�p�A�jA�bNA�\)A�XA�S�A�S�A�?}A�1'A���A�5?A�A�
=A�JA�A��mA��A���A��!A�?}A��A�33A��A�r�A�=qA�%A��9A���A��DA�t�A�dZA�^5A�M�A�9XA�33A��A��RA��7A��A��A��A�hsA�hsA�dZA�E�A�JA���A�O�A��A�A��
A���A�v�A�`BA�I�A�33A�
=A���A��hA��yA�jA�&�A��TA�ĜA��A��7A�jA�M�A�"�A��A�oA��TA���A��RA���A��uA�~�A�XA�&�A�ƨA�|�A�A��!A�A�A�bA��A���A���A��wA��!A���A���A��DA�l�A�"�A�  A���A���A�=qA��A��DA���A�1'A���A��
A�ƨA��FA���A��+A��A�n�A�VA�O�A�"�A��A��A�1A���A���A��A��A��`A��HA���A�A��FA��RA��PA��A��A��A�r�A�dZA�Q�A�Q�A�K�A�C�A�G�A�=qA�-A�&�A��A�VA�1A���A��jA���A��!A���A�x�A�O�A�O�A� �A���A��`A��A���A���A�x�A�t�A�|�A�hsA�ffA��A��A��RA��FA�t�A�hsA��A��\A�r�A�5?A��A��mA��RA��hA��FA�ZA�E�A�?}A�5?A� �A��A�oA�oA�VA�
=A�A�
=A�  A�A�A�A�  A�  A���A��A��A��yA���A��9A���A�jA�`BA�Q�A�(�A��A�A��A��TA��TA��/A��
A��A���A�ȴA��FA�v�A�E�A�33A�$�A��A�bA���A���A��wA��jA��jA��jA��-A��!A��9A��A���A���A���A���A��\A��A�VA�9XA�$�A�{A���A��HA���A��A��A���A��9A��A�~�A�ffA�bA�A���A��A��;A���A���A��FA��A���A��A�|�A�r�A�dZA�A�A�"�A�
=A�  A��A��!A���A���A��A�`BA�;dA�1A��A���A��-A�|�A�\)A�=qA�;dA�;dA��A���A��
A��wA���A��uA��+A��+A�|�A�z�A�p�A�dZA�dZA�ZA�ZA�ZA�G�A�-A�+A�33A�33A�$�A��A�JA�JA�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                 ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B�B��B�B��B�FB�@B��B�{B��B�FB��B��B�.B�.B��B��B��B��B�B�SB��B��B�eB��B�7B�kB�=B�-B��B�UB�B�zB�$B��B�BB�dB�	B�BB:B@BVB)�B#nB#nB(�B.IB6FB-wB+6B,B4B)�B#:B-wB!�B�B�B�BB�B�B�MB�xB�B�dB�WBҽB� B�9B�XB�B��B��B� B�DBq�BiDBO�B5?B.B(�B#nB�B�BSB��B�)B�sB��BϫBɺB�aB��B��B�IB�xB�FB�xB��BuZBm)BjBf�Ba�BZ�BMjBGBB�B4�B$tB7BfBMBoB
�fB
�QB
�)B
خB
�B
�B
�JB
�B
r�B
h�B
PB
A B
<jB
1[B
5�B
�B
�B
�B
{B
{B
JB
B
 4B	�rB	�B	��B	��B	��B	�B	�vB	ݘB	ϫB	�B	�0B	��B	��B	�_B	��B	�=B	�kB	��B	�lB	��B	��B	��B	zxB	z�B	z�B	y>B	w�B	w�B	v`B	q�B	q�B	m)B	g8B	d�B	e,B	a�B	^5B	\)B	ZB	Y�B	UgB	PHB	PB	NpB	M�B	LdB	JXB	GzB	FB	EB	HKB	DgB	B�B	B'B	C�B	C�B	B�B	D�B	DgB	GEB	LdB	LdB	NB	PHB	RTB	S�B	U�B	V�B	XyB	X�B	ZQB	Z�B	XEB	W�B	V�B	T�B	W
B	W?B	W
B	W
B	Q�B	P}B	OB	N�B	L�B	L0B	K�B	J�B	IB	H�B	HB	I�B	FB	C�B	FtB	F?B	G�B	IB	J#B	J�B	L0B	N<B	T,B	W
B	V�B	Y�B	[WB	\)B	a�B	dZB	d�B	ffB	l"B	u�B	z�B	}VB	}�B	�B	��B	��B	�%B	�xB	�B	��B	�DB	�xB	�PB	��B	�PB	�~B	�B	�uB	�{B	�+B	��B	�_B	�OB	�B	�B	��B	�@B	��B	��B	�6B	��B	�B	�6B	�}B	�CB	�$B	��B	��B	�RB	��B	� B	ŢB	�KB	ȴB	��B	ȴB	�RB	�)B	�B	ӏB	�
B	�B	��B	�B	�BB	ߤB	��B	��B	�TB	�ZB	��B	��B	�fB	�KB	��B	�B	�2B	��B	�VB	�]B
B
_B
B
�B
�B
�B
.B
�B
�B
�B
�B
�B
+B
�B
IB
�B
!�B
"�B
$�B
&B
(�B
+B
)�B
-wB
/B
/�B
/�B
3�B
4�B
6zB
7�B
9�B
<�B
A B
EmB
G�B
H�B
J�B
L�B
U�B
Z�B
[WB
]/B
\�B
`�B
ffB
f�B
jKB
p;B
o5B
o�B
qB
u�B
s�B
s�B
tTB
tB
t�B
wfB
w�B
xlB
y�B
|�B
� B
��B
��B
�B
�+B
�=B
��B
�B
�DB
�xB
�xB
�=B
��B
��B
��B
�7B
�rB
�~B
�B
��B
��B
��B
�B
�{B
��B
��B
��B
��B
�kB
�}B
��B
��B
�zB
�B
��B
��B
��B
�^B
�dB
�B
�jB
��B
��B
�jB
�6B
��B
�B
�<B
�wB
��B
�wB
��B
�B
��B
��B
�UB
�UB
B
�aB
�gB
�mB
��B
��B
��B
��B
ŢB
�B
�B
��B
�EB
ǮB
�KB
�B
ɺB
�B
͟B
�6B
�6B
�B
ϫB
�B
�HB
��B
уB
� B
ҽB
ҽB
ҽB
��B
�&B
��B
ԕB
�aB
ԕB
��B
�2B
�mB
�sB
רB
�sB
�sB
�B
��B
��B
ںB
ںB
�#B
�)B
ݘB
�5B
�jB
ޞB
�jB
��B
�B
�;B
�BB
�B
�BB
��B
�B
��B
�B
�TB
�B
�B
��B
�B
��B
�,B
��B
��B
��B
�,B
�mB
�8B
��B
�sB
��B
��B
�yB
��B
�B
��B
�B
�B
��B
��B
�B
�B
�WB
��B
�)B
�B
�]B
��B
�/B
�B
�B
�B
�/B
�/B
� B
��B
�B
�oB
�B
��B
��B
��B
��B
��B
��B
�B
�B
�AB
�vB
�vB
�AB
�B
�B
��B
��B
�B
�B
�B
�GB
�B
�TB
��B
�TB
��B
�ZB
��B
��B
��B
��B
��B
��B
�`B
��B
��B
��B
��B
��B
��B
�B
�	B
�>B
�rB
��B
�rB
��B
��B
��B
�JB
�B
��B
��B
��B
��B
�B
�B
�PB
�"B
�VB
��B
��B
��B
��B
��B
��B
�.B
�.B
��B  B iB �B �BoBoB�BABBBGBGB{B{B{B�BMB�B�B�B�BB�BSB�B�B�B�BYB�B�B+B+B+B�B+B�B�B	7B	�B	�B	�B
�B
=B
	B
	BDB�B�B�B�BB�B~B�B�BPB�B"BVB�B�B�BbB�BbBbB�B B�B�B�B�BuB�B�BB{B�B�B{B{B{BMBSB�B�B�B�B$B�B�B+B_B+B_B�B�B�B�B�B�B�B�B�BBB7B7B7B7B	B=B	B	BqBqB�B�B�BBB~BBOB�B�B!B!B!BVB�B 'B \B �B �B!-B!-B!-B �B �B!�B!bB!bB!�B"4B!�B"4B"4B"hB"4B"hB"�B"�B"hB"�B$�B$�B%FB%FB%zB%FB%�B%�B%�B%�B%�B%zB%�B&�B'�B'�B'�B(XB(�B(�B(�B(�B(�B(�B(�B(�B(XB'�B)�B*�B,qB,qB,=B,=B,=B,=B,=B,B,=B,=B,qB,�B,�B,�B,�B-wB-wB-�B.}B.�B/�B/�B/�B/�B/�B/�B/�B/�B/�B/�B/�B/�B0UB0�B0�B0�B1�B1�B1�B1�B2-B1�B1�B2-B2�B2�B2�B2�B33B33B3hB3�B3�B3�B3�B3�B3�B4B49B4�B4�B4�B4�B4�B5B5B5?B5B5tB5tB5tB5�B6zB6�B6�B6zB6FB6�B6�B7LB8RB8B8RB8�B8�B8�B8�B8�B9$B9$B8�B9$B9XB9XB:*B:^B:^B:^B:^B:^B:^B:�B:�B:^B:�B:�B:�B;dB;dB;dB;dB;dB;dB;�B;�B;dB;�B;dB;dB;0B;0B;�B;�B<�B<�B<�B=B=�B=qB>�B>�B?}B?HB?�B?}B?�B@OB?�B@�B@�B@�B@�B@�B@�BAUBA�BA�BB'BB'BB�BB�BB�BB�BB�BB�BCaBCaBDgBD3BDgBD�BD�BD�BD�BD�BEmBFBE�BFBF?BF?BFtBFtBFtBFtBFtBF�BGBGBGEBGzBG�BG�BHBHBH�BHKBHKBHKBHBH�BH�BH�BH�BH�BIBH�BIBI�BJ#B��B�4B�B��B��B�:B��B��B��B��B��B�FB�B��B�B��B��B�{B��B�{B�B��B��B��B�:B�B�uB�B��B��B��B�B��B�@B��B��B��B�4B�FB�:B��B�+B�{B�$B�B��B�B�:B��B�oB��B�4B�"B��B��B�bB��B� B�"B�.B��B�bB��B��B��B��B��B��B��B��B��B��B�4B��B� B�(B�bB��B�.B��B��B� B��B�hB��B��B� B�uB��B��B�:B�uB��B�B��B��B�SB��B�$B��B�B�YB��B��B�{B�+B�SB��B��B��B��B��B��B�_B��B�+B�B�_B��B�+B�B��B�7B��B��B��B�_B��B��B�	B�eB�7B��B�1B�=B�1B��B�7B��B�=B��B��B�7B��B�qB��B��B�B��B��B��B��B�tB��B��B�RB��B��B��B�=B��B��B�!B��B�?B�3B�B��B�nB�hB�9B�B��B��B��B��B��B��B�B��B�0B��B�dB�*B�6B�B�}B�B�9B� B��B�KB�^B��B�B�[BޞBܒB�#B�B�dB�B�B�B�B��B�B+B�PB�B�B�B�B*eBxB�B�BB�BB�B4BhB.BuBuBoB{BB�BB�BFB�B+�B&�B�B!�B�BeBB(XBe�B%zB �B 'B$@B#�B&�B$B"4B(XB'RB$tB"4BVB�B!BOB(�B(�B4B.�B"4B!�B"�B)�BGEB+6B9�B/OBL�B<6B-wB-wB/�B,qB0!B+B-�B,=B*0B,B)�B+�B.�B'�B)�B-B(�B,�B/B-CB0!B;�B8RB3�B0�B%�B'�B&B&�B%�B$tB"hB"�B�B#�BVB�B \B6�BC�B2�B&�BxB�B�B7�B2-BoBqBB�B�B�B$B�BB$BMB B�BuB�B"�B	BBkB@B�BVB�B�B�B�B�BB��B��BCB��B��B�)B�AB�B�`B�B�AB�ZB�B�VB�;B�B��B��B�QB�|B��B�)BݘB��B�QB�B��B՛B�B�HB�gBҽB� BуB��B�<B�B��B�^B�ZBɆBʌB�'B�BÖB�HB�0B��B�RB�B��B�jB�BB��B�_B�B�hB��B��B�!B�~B�qB��B��B�@B��B�:B��B��B�B�B��B�B�{B��B�AB�;Bt�BsMBqABn/Bn/Bm)BiyBgmBk�Bj�Bv`Bd�BgmBXBe�BW
B\�BMBYB@�B?}B;dB>B;0B?HB4�B9�B8�B49B7B33B-�B33B.B-B/OB/�B,B-CB.B&�B*�B+kB4�B)�B'RB(XB)_B)*B)�B&B&B($B!�B$�B�B"�B$B"�BVB�B"�BFBMB�BVB$@BB�BB�B�B�B	B	B
rBB�B�B"B�.BB�cB�B��B��B 4B�|B��B�8B�fB�;B�B/�BںB�)B��B��B�B�sB��B�B�mB�EB�B�gB�EBרB��B��B��B�mB՛B�B��BуBٴBԕB��B�EB�<B�B��B�B�^B��B˒B��B�B��B�EB�KBɆB�#B�}B�B�B�-B��B��BȴBB�<B�B��B��B�HB�jB�^B��B��B��B�$B��B��B��B�B�B��B��B��B��B��B��B��B�kB��B�'B��B��B�B��B��B��B��B��B�$B�B��B�_B��B� B�bB�4B�FB�B�PB��B��B�FB��B��B�1B�7B�MB��B�4B��B�4B�B}"BsBtBt�Bv�B�iBs�BqABr�Bt�Bm�Bm]Bo Bm)Bm�Bm�BkBj�Bk�Bh>Bp�BiyBjBe�BffBk�Bh>Bf2Be,Bg8G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202209061825322022090618253220220906182532202209061825322022090618253220220906182532SI  SI  ARFMARFM                                                                                                                                                2022010318351820220103183518IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022011318011720220113180117QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022011318011720220113180117QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022090607225920220906072259IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022090618253820220906182538IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022090618253820220906182538IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022090618253820220906182538IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                