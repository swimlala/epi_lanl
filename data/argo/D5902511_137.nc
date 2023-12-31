CDF   	   
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-06-22T23:47:07Z creation; 2020-07-07T21:55:48Z DMQC;      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20200622234707  20210429202813  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_137                 6810_008521_137                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�#:@��@�#:@��11  @�#:ffff@�#:ffff@3�,�d�@3�,�d��e.�-�"��e.�-�"�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?���@   @@  @}p�@�  @�G�@�G�AG�A��A   A,(�A?\)A`  A�Q�A���A�Q�A�Q�A�  A�  A�  A�  B   B�
B�
B�
B�
B'�
B0(�B8(�B@  BH(�BPQ�BX  B_�Bg�Bo�
Bx  B�
B��B�{B�(�B�  B�  B�  B��
B��B�{B�{B��
B��B�{B��B�  B��B�{B�{B�  B�{B�(�B�{B��B��B�  B�{B�{B�  B�  B�{B�  C   C  C  C
=C
=C	��C��C
=C{C
=C��C  C��C  C  C��C��C!�C$  C&{C(
=C*  C+��C.  C0  C1��C3��C6
=C8  C9�C<  C>
=C@  CB  CD  CF  CH
=CJ
=CL
=CN  CP  CQ��CT  CV  CX  CY��C\  C^  C_�Ca��Cd  Cf
=Ch
=Cj
=Cl  Cm��Co��Cr  Ct  Cv  Cx�Cz
=C|  C}�C�C�C�C�C���C�C�  C�  C���C���C�  C�  C�
=C�C�  C�  C�C�
=C�  C���C���C�  C�  C�  C�  C�  C�C���C���C�  C�  C���C�C�C�C�  C�  C�  C�  C�  C���C�  C�  C���C���C�  C�C�  C���C���C���C�  C�  C�C�  C�  C�  C�C�C���C���C�  C�  C���C���C�  C�C�C�  C���C�  C�  C���C�  C�  C���C�  C���C�  C���C���C�C���C���C�  C�C�  C�  C�  C���C�  C�  C���C�  C�  C�  C�C�C�C�C�  C�  C���C���C���C���C���C�C�  C�  C�  C�  C���C�  C�  C�  C�  C�  C���C���C�  C�  C�C�  C�  C���C�C�  D �D ��D  D��D  D�D  D��D  D��D�qD}qD�qD�D  D� D�qD��D�qD	� D
  D
}qD  D}qD  D� D  D� DD��D  D��D  D� D  D}qD�D}qD�D��D  D� D  D}qDD}qD  D�D�D� D�D�D�D� DD� D��D}qD  D� D  D� D  D� D �D � D!  D!��D"  D"� D"��D#� D$  D$z�D$��D%z�D%��D&� D'�D'}qD(  D(� D(�qD)��D*D*� D*��D+� D,  D,��D-  D-� D-��D.z�D.��D/� D0D0� D1�D1��D2�D2z�D2��D3z�D3��D4}qD4�qD5}qD6�D6��D7�D7}qD8  D8�D9  D9� D:�D:� D;  D;� D;�qD<}qD<�qD=}qD>  D>�D?D?��D@�D@�DA�DA}qDB  DB��DC  DC}qDC��DDz�DD��DE}qDF�DF}qDF�qDG��DH�DH��DI�DI� DJ  DJ}qDJ�qDK��DL�DL��DM  DM}qDM��DN��DODO��DP�DP}qDP�qDQ� DR�DR�DS  DS}qDS��DT}qDT�qDU}qDU�qDV� DW�DW��DW�qDXz�DX�qDY��DZ�DZ��D[D[� D\�D\� D]  D]� D^  D^� D_  D_� D`D`� D`�qDa� Db  Db}qDc  Dc��Dd�Dd��De�De� Df  Dfz�Df�qDg� Dh  Dh� Di�Di��Dj  Dj}qDj�qDk}qDk��Dl}qDl�qDm}qDm�qDn��Do�Do��Dp�Dp� Dp�qDqz�Dq��Drz�Dr�qDsz�Ds��Dt}qDu  Du� Dv  Dv}qDv�qDwz�Dx  Dx��Dy�Dy��Dz�Dz� D{�D{��D|�D|}qD|�RD}z�D}�qD~� D�D��D�HD�@ D�~�D���D�  D�AHD�� D�� D�  D�@ D�� D��HD�HD�>�D�~�D��HD�  D�AHD��HD��HD�  D�>�D�~�D���D���D�@ D��HD��HD�HD�AHD�� D�� D���D�=qD�� D�D��D�AHD���D��HD�  D�@ D��HD�� D�  D�B�D�� D��HD�HD�B�D�� D�� D�HD�AHD�� D���D��qD�@ D�� D�� D�  D�AHD�� D���D�  D�>�D�~�D�� D�HD�AHD�~�D���D�HD�@ D�� D�� D�HD�@ D�� D���D�  D�@ D�� D���D���D�>�D�~�D���D���D�AHD��HD���D�  D�@ D�� D���D���D�@ D�� D��HD�  D�AHD��HD���D�  D�>�D�~�D�� D�  D�B�D��HD�� D�  D�@ D�~�D�� D���D�@ D�� D��HD�HD�=qD�� D���D���D�>�D�~�D�� D�HD�AHD�� D��HD��D�>�D�� D�� D�  D�@ D�� D��HD�  D�@ D�� D�� D���D�AHD�� D�� D�HD�AHD��HD�� D�  D�>�D�� D��HD�  D�AHD�~�D���D�  D�B�D�� D���D�  D�AHD��HD�� D�  D�@ D���D��HD�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�AHD��HD�D�HD�@ D�� D���D�HD�@ D�� D�� D���D�>�D��HD�� D�  D�AHD��HD��HD�  D�AHD�� D�� D�HD�@ D��HD��HD���D�=qD�~�D���D���D�@ D�~�D��HD�HD�@ D�� D���D�  D�>�D�� D��HD�HD�AHD��HD�� D���D�@ D�� D���D�  D�@ D�~�D���D�  D�AHD�~�D�� D�HD�@ D��HD��HD�  D�>�D��HD��HD�  D�AHD�� D���D�  D�AHD��HD�� D�  D�>�D�� D�� D�  D�@ D�� D���D�  D�B�DHD�� D�  D�@ DÀ D�� D�  D�>�DāHD��HD�  D�@ D�}qDž�D�  D�@ Dƀ D�� D�HD�B�DǁHD��HD�  D�@ DȁHD�� D�  D�@ DɁHD�� D�  D�AHDʁHDʾ�D��qD�AHDˀ D�� D�HD�AHD̀ D�� D���D�@ D�~�D�� D��D�AHD΀ D�D�HD�@ Dς�D�� D���D�@ DЁHD��HD�  D�@ DсHDѽqD��qD�@ DҀ D�� D��D�AHDӀ D��HD�  D�@ DԁHD��HD�  D�@ D�~�D�� D���D�>�DցHD�� D�HD�AHDׁHD��HD�  D�AHD�~�Dؾ�D�  D�@ Dـ D�� D��D�AHDڀ D�� D���D�@ DہHD۾�D���D�@ D܁HD��HD�HD�B�D݀ D�� D�HD�@ D�~�D�� D�  D�AHD߁HD�� D�  D�@ D�~�D�� D�HD�@ DႏD�� D��qD�@ D� D�� D�  D�>�D� D㾸D���D�=qD�~�D�� D�  D�@ D�~�D徸D�  D�@ D� D�� D�  D�@ D�HD�� D�  D�>�D�HD��HD�  D�@ D邏D��HD���D�>�D�~�D��HD�  D�@ D� D��HD�HD�@ D�HD�� D�  D�AHD�HD���D���D�AHD�HDD�  D�@ D� D�� D�HD�AHD�� D�� D���D�@ D� D�D�HD�AHD� D�� D�  D�@ D�HD��HD�  D�@ D�HD�� D�  D�AHD�� D���D�  D�@ D�� D�� D�  D�AHD�� D�� D���D�>�D�� D��HD��D�AHD�� D�� D�  D�:�?u?�=q?��
?���?��@�\@�@(��@8Q�@B�\@Y��@h��@u@��
@�{@�z�@�p�@���@�{@�Q�@\@���@У�@�(�@��
@�@�z�A   A�
A
=A��AG�Az�A��A�RA!�A%�A+�A/\)A2�\A8��A<(�A?\)AEAHQ�AN{AR�\AVffA[�A`  Ac33Ah��Amp�Ap��AuAz�HA}p�A�G�A�(�A�{A��A��\A���A��RA�G�A��
A�p�A�\)A��\A�(�A�ffA�G�A�33A��A�  A��HA�z�A��RA���A��
A�{A�  A��HA�p�A�
=A��A�(�A�A���A�33A���A�  A��A��
A�ffA�G�A��HA��A�Q�A�\A�(�A�\)A陚A�33A�{A���A�\A���A��A��A��A��RB Q�BG�B�\B  B��BB33BQ�B	G�B
=qB�B��B��B
=B(�B��B=qB�Bz�Bp�B�HB�
B��BB\)BQ�B�BffB�
B ��B!B#
=B$  B$��B&=qB'�B((�B)��B*�RB+�B,��B.{B/
=B0  B1G�B2�RB3�B4z�B5B6�RB7�B9�B:{B:�HB<(�B=G�B=�B?33B@Q�BA�BB{BC�BD(�BD��BF=qBG\)BH  BI�BJffBK33BL  BMG�BNffBN�HBP  BQ�BR=qBR�RBS�
BT��BUBV�\BW�BX��BY��BZffB[�B\��B]��B^ffB_�
B`��Bap�Bb�RBd  Bd��BeBg
=BhQ�BiG�Bj{Bk�Blz�BmG�Bn�HBo�
Bp��Br{Bs33Bt  Bup�BvffBw\)Bx��ByBz�\B{�B}�B~{B~�HB�(�B���B�
=B��B�Q�B���B�33B��B��\B���B�p�B�{B��RB��B��B�ffB���B�\)B��B���B�G�B��B�(�B���B�p�B��
B�Q�B�
=B��B�{B��RB�\)B��
B�Q�B���B��B�(�B���B�\)B��B�Q�B���B��B�{B���B�G�B��B�Q�B���B��B�(�B���B�G�B�  B�z�B���B���B�=qB���B�33B��B��\B�
=B���B�Q�B��HB�\)B��
B���B�G�B��B�=qB���B���B�  B��\B�\)B�  B�z�B��HB��B�ffB��HB�p�B�(�B���B�G�B��
B��\B�33B��B�=qB���B���B�{B��\B�\)B�{B��\B�
=B��B�z�B�33B���B�(�B��HB���B�(�B���B�\)B�{B��\B�
=B��
B�z�B��HB�p�B�  BĸRB�G�BŮB�{B���B�33BǙ�B�  Bȣ�B�
=B�\)B�B�=qBʸRB�
=B�p�B�  B�Q�Ḅ�B��BͮB�  B�=qB���B�G�B�B�  B�z�B���BхB��
B�(�Bң�B�33Bә�B��B�Q�B��HB�\)BծB�  B֏\B�
=B�p�B׮B�=qB���B��BمB�{Bڏ\B��HB�33B�B�=qB܏\B��HB�\)B��B�Q�Bޏ\B�
=Bߙ�B�  B�=qB�RB�33B�B�  B�Q�B���B�\)B�B�  B�z�B�
=B�p�B�B�(�B�RB�33B癚B��B�Q�B��HB�p�B�B�(�B�\B��B�B��B�Q�B���B�p�B�B�{B�\B��BB��
B�=qB���B�\)B�B�{B�z�B�
=B�B��B�=qB��RB�\)B�B�(�B��\B�33B���B��B�ffB���B��B�  B�Q�B��RB�G�B��B�=qB���B�
=B���B�Q�B��\B���B���C 
=C 33C ffC �C ��C33C\)C�\C�
C�CQ�Cz�C�RC  C=qCp�C��C�HC(�CffC�\CC
=CQ�C�\C�RC�C33Cz�C�RC�
C{CffC��C��C  C=qC�CC�C	�C	\)C	�C	�C
{C
G�C
�\C
��C
=C33CffC�C  C33CffC�\C�
C�CQ�C�C�C��C=qCp�C��C�
C
=C\)C��C�
C  C(�Cp�CC�C{CQ�C��C�
C
=C33Cp�C�RC��C(�CQ�C�\C�
C{C=qCz�C�C�C=qCp�C��C�
C{CQ�Cz�C�C�C(�C\)Cz�C�C�C(�C\)Cz�C�RC  C(�CG�C�CC��C{CG�C�C�RC  C33CQ�Cz�C�C�C(�CQ�Cz�CC  C33C\)C�CC
=CQ�Cp�C��C�HC(�CQ�C�CC 
=C (�C ffC ��C �HC!{C!33C!ffC!��C!�HC"�C"\)C"�\C"�RC#  C#G�C#z�C#��C#�
C${C$\)C$��C$C$��C%=qC%�C%�RC%�HC&{C&\)C&��C&C&��C'(�C'ffC'�C'�HC(
=C(33C(p�C(�RC(�C)
=C)33C)z�C)�RC)��C*{C*=qC*z�C*�RC+  C+(�C+Q�C+�C+��C,
=C,G�C,p�C,��C,�C-33C-ffC-�\C-��C.{C.\)C.�\C.C.�C/33C/z�C/C/��C0(�C0\)C0�C0��C1(�C1\)C1�\C1�
C2�C2Q�C2�C2�RC3  C3G�C3z�C3��C3�
C4(�C4ffC4��C4C5
=C5Q�C5�\C5�C5�HC6(�C6ffC6��C6C7  C7=qC7�C7��C8  C8(�C8Q�C8�\C8�HC9�C9Q�C9�C9C:{C:\)C:��C:�
C;  C;=qC;�C;��C<
=C<G�C<p�C<�C<�C=33C=z�C=C=��C>(�C>\)C>�\C>��C?{C?G�C?z�C?�C?�C@(�C@z�C@C@��CA(�CAffCA��CA�HCB33CBz�CBCC  CC33CCp�CC�CD  CDQ�CD��CD��CE
=CEG�CE�CE�
CF(�CFffCF��CF��CG
=CGQ�CG��CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                             ?���@   @@  @}p�@�  @�G�@�G�AG�A��A   A,(�A?\)A`  A�Q�A���A�Q�A�Q�A�  A�  A�  A�  B   B�
B�
B�
B�
B'�
B0(�B8(�B@  BH(�BPQ�BX  B_�Bg�Bo�
Bx  B�
B��B�{B�(�B�  B�  B�  B��
B��B�{B�{B��
B��B�{B��B�  B��B�{B�{B�  B�{B�(�B�{B��B��B�  B�{B�{B�  B�  B�{B�  C   C  C  C
=C
=C	��C��C
=C{C
=C��C  C��C  C  C��C��C!�C$  C&{C(
=C*  C+��C.  C0  C1��C3��C6
=C8  C9�C<  C>
=C@  CB  CD  CF  CH
=CJ
=CL
=CN  CP  CQ��CT  CV  CX  CY��C\  C^  C_�Ca��Cd  Cf
=Ch
=Cj
=Cl  Cm��Co��Cr  Ct  Cv  Cx�Cz
=C|  C}�C�C�C�C�C���C�C�  C�  C���C���C�  C�  C�
=C�C�  C�  C�C�
=C�  C���C���C�  C�  C�  C�  C�  C�C���C���C�  C�  C���C�C�C�C�  C�  C�  C�  C�  C���C�  C�  C���C���C�  C�C�  C���C���C���C�  C�  C�C�  C�  C�  C�C�C���C���C�  C�  C���C���C�  C�C�C�  C���C�  C�  C���C�  C�  C���C�  C���C�  C���C���C�C���C���C�  C�C�  C�  C�  C���C�  C�  C���C�  C�  C�  C�C�C�C�C�  C�  C���C���C���C���C���C�C�  C�  C�  C�  C���C�  C�  C�  C�  C�  C���C���C�  C�  C�C�  C�  C���C�C�  D �D ��D  D��D  D�D  D��D  D��D�qD}qD�qD�D  D� D�qD��D�qD	� D
  D
}qD  D}qD  D� D  D� DD��D  D��D  D� D  D}qD�D}qD�D��D  D� D  D}qDD}qD  D�D�D� D�D�D�D� DD� D��D}qD  D� D  D� D  D� D �D � D!  D!��D"  D"� D"��D#� D$  D$z�D$��D%z�D%��D&� D'�D'}qD(  D(� D(�qD)��D*D*� D*��D+� D,  D,��D-  D-� D-��D.z�D.��D/� D0D0� D1�D1��D2�D2z�D2��D3z�D3��D4}qD4�qD5}qD6�D6��D7�D7}qD8  D8�D9  D9� D:�D:� D;  D;� D;�qD<}qD<�qD=}qD>  D>�D?D?��D@�D@�DA�DA}qDB  DB��DC  DC}qDC��DDz�DD��DE}qDF�DF}qDF�qDG��DH�DH��DI�DI� DJ  DJ}qDJ�qDK��DL�DL��DM  DM}qDM��DN��DODO��DP�DP}qDP�qDQ� DR�DR�DS  DS}qDS��DT}qDT�qDU}qDU�qDV� DW�DW��DW�qDXz�DX�qDY��DZ�DZ��D[D[� D\�D\� D]  D]� D^  D^� D_  D_� D`D`� D`�qDa� Db  Db}qDc  Dc��Dd�Dd��De�De� Df  Dfz�Df�qDg� Dh  Dh� Di�Di��Dj  Dj}qDj�qDk}qDk��Dl}qDl�qDm}qDm�qDn��Do�Do��Dp�Dp� Dp�qDqz�Dq��Drz�Dr�qDsz�Ds��Dt}qDu  Du� Dv  Dv}qDv�qDwz�Dx  Dx��Dy�Dy��Dz�Dz� D{�D{��D|�D|}qD|�RD}z�D}�qD~� D�D��D�HD�@ D�~�D���D�  D�AHD�� D�� D�  D�@ D�� D��HD�HD�>�D�~�D��HD�  D�AHD��HD��HD�  D�>�D�~�D���D���D�@ D��HD��HD�HD�AHD�� D�� D���D�=qD�� D�D��D�AHD���D��HD�  D�@ D��HD�� D�  D�B�D�� D��HD�HD�B�D�� D�� D�HD�AHD�� D���D��qD�@ D�� D�� D�  D�AHD�� D���D�  D�>�D�~�D�� D�HD�AHD�~�D���D�HD�@ D�� D�� D�HD�@ D�� D���D�  D�@ D�� D���D���D�>�D�~�D���D���D�AHD��HD���D�  D�@ D�� D���D���D�@ D�� D��HD�  D�AHD��HD���D�  D�>�D�~�D�� D�  D�B�D��HD�� D�  D�@ D�~�D�� D���D�@ D�� D��HD�HD�=qD�� D���D���D�>�D�~�D�� D�HD�AHD�� D��HD��D�>�D�� D�� D�  D�@ D�� D��HD�  D�@ D�� D�� D���D�AHD�� D�� D�HD�AHD��HD�� D�  D�>�D�� D��HD�  D�AHD�~�D���D�  D�B�D�� D���D�  D�AHD��HD�� D�  D�@ D���D��HD�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�AHD��HD�D�HD�@ D�� D���D�HD�@ D�� D�� D���D�>�D��HD�� D�  D�AHD��HD��HD�  D�AHD�� D�� D�HD�@ D��HD��HD���D�=qD�~�D���D���D�@ D�~�D��HD�HD�@ D�� D���D�  D�>�D�� D��HD�HD�AHD��HD�� D���D�@ D�� D���D�  D�@ D�~�D���D�  D�AHD�~�D�� D�HD�@ D��HD��HD�  D�>�D��HD��HD�  D�AHD�� D���D�  D�AHD��HD�� D�  D�>�D�� D�� D�  D�@ D�� D���D�  D�B�DHD�� D�  D�@ DÀ D�� D�  D�>�DāHD��HD�  D�@ D�}qDž�D�  D�@ Dƀ D�� D�HD�B�DǁHD��HD�  D�@ DȁHD�� D�  D�@ DɁHD�� D�  D�AHDʁHDʾ�D��qD�AHDˀ D�� D�HD�AHD̀ D�� D���D�@ D�~�D�� D��D�AHD΀ D�D�HD�@ Dς�D�� D���D�@ DЁHD��HD�  D�@ DсHDѽqD��qD�@ DҀ D�� D��D�AHDӀ D��HD�  D�@ DԁHD��HD�  D�@ D�~�D�� D���D�>�DցHD�� D�HD�AHDׁHD��HD�  D�AHD�~�Dؾ�D�  D�@ Dـ D�� D��D�AHDڀ D�� D���D�@ DہHD۾�D���D�@ D܁HD��HD�HD�B�D݀ D�� D�HD�@ D�~�D�� D�  D�AHD߁HD�� D�  D�@ D�~�D�� D�HD�@ DႏD�� D��qD�@ D� D�� D�  D�>�D� D㾸D���D�=qD�~�D�� D�  D�@ D�~�D徸D�  D�@ D� D�� D�  D�@ D�HD�� D�  D�>�D�HD��HD�  D�@ D邏D��HD���D�>�D�~�D��HD�  D�@ D� D��HD�HD�@ D�HD�� D�  D�AHD�HD���D���D�AHD�HDD�  D�@ D� D�� D�HD�AHD�� D�� D���D�@ D� D�D�HD�AHD� D�� D�  D�@ D�HD��HD�  D�@ D�HD�� D�  D�AHD�� D���D�  D�@ D�� D�� D�  D�AHD�� D�� D���D�>�D�� D��HD��D�AHD�� D�� D�  G�O�?u?�=q?��
?���?��@�\@�@(��@8Q�@B�\@Y��@h��@u@��
@�{@�z�@�p�@���@�{@�Q�@\@���@У�@�(�@��
@�@�z�A   A�
A
=A��AG�Az�A��A�RA!�A%�A+�A/\)A2�\A8��A<(�A?\)AEAHQ�AN{AR�\AVffA[�A`  Ac33Ah��Amp�Ap��AuAz�HA}p�A�G�A�(�A�{A��A��\A���A��RA�G�A��
A�p�A�\)A��\A�(�A�ffA�G�A�33A��A�  A��HA�z�A��RA���A��
A�{A�  A��HA�p�A�
=A��A�(�A�A���A�33A���A�  A��A��
A�ffA�G�A��HA��A�Q�A�\A�(�A�\)A陚A�33A�{A���A�\A���A��A��A��A��RB Q�BG�B�\B  B��BB33BQ�B	G�B
=qB�B��B��B
=B(�B��B=qB�Bz�Bp�B�HB�
B��BB\)BQ�B�BffB�
B ��B!B#
=B$  B$��B&=qB'�B((�B)��B*�RB+�B,��B.{B/
=B0  B1G�B2�RB3�B4z�B5B6�RB7�B9�B:{B:�HB<(�B=G�B=�B?33B@Q�BA�BB{BC�BD(�BD��BF=qBG\)BH  BI�BJffBK33BL  BMG�BNffBN�HBP  BQ�BR=qBR�RBS�
BT��BUBV�\BW�BX��BY��BZffB[�B\��B]��B^ffB_�
B`��Bap�Bb�RBd  Bd��BeBg
=BhQ�BiG�Bj{Bk�Blz�BmG�Bn�HBo�
Bp��Br{Bs33Bt  Bup�BvffBw\)Bx��ByBz�\B{�B}�B~{B~�HB�(�B���B�
=B��B�Q�B���B�33B��B��\B���B�p�B�{B��RB��B��B�ffB���B�\)B��B���B�G�B��B�(�B���B�p�B��
B�Q�B�
=B��B�{B��RB�\)B��
B�Q�B���B��B�(�B���B�\)B��B�Q�B���B��B�{B���B�G�B��B�Q�B���B��B�(�B���B�G�B�  B�z�B���B���B�=qB���B�33B��B��\B�
=B���B�Q�B��HB�\)B��
B���B�G�B��B�=qB���B���B�  B��\B�\)B�  B�z�B��HB��B�ffB��HB�p�B�(�B���B�G�B��
B��\B�33B��B�=qB���B���B�{B��\B�\)B�{B��\B�
=B��B�z�B�33B���B�(�B��HB���B�(�B���B�\)B�{B��\B�
=B��
B�z�B��HB�p�B�  BĸRB�G�BŮB�{B���B�33BǙ�B�  Bȣ�B�
=B�\)B�B�=qBʸRB�
=B�p�B�  B�Q�Ḅ�B��BͮB�  B�=qB���B�G�B�B�  B�z�B���BхB��
B�(�Bң�B�33Bә�B��B�Q�B��HB�\)BծB�  B֏\B�
=B�p�B׮B�=qB���B��BمB�{Bڏ\B��HB�33B�B�=qB܏\B��HB�\)B��B�Q�Bޏ\B�
=Bߙ�B�  B�=qB�RB�33B�B�  B�Q�B���B�\)B�B�  B�z�B�
=B�p�B�B�(�B�RB�33B癚B��B�Q�B��HB�p�B�B�(�B�\B��B�B��B�Q�B���B�p�B�B�{B�\B��BB��
B�=qB���B�\)B�B�{B�z�B�
=B�B��B�=qB��RB�\)B�B�(�B��\B�33B���B��B�ffB���B��B�  B�Q�B��RB�G�B��B�=qB���B�
=B���B�Q�B��\B���B���C 
=C 33C ffC �C ��C33C\)C�\C�
C�CQ�Cz�C�RC  C=qCp�C��C�HC(�CffC�\CC
=CQ�C�\C�RC�C33Cz�C�RC�
C{CffC��C��C  C=qC�CC�C	�C	\)C	�C	�C
{C
G�C
�\C
��C
=C33CffC�C  C33CffC�\C�
C�CQ�C�C�C��C=qCp�C��C�
C
=C\)C��C�
C  C(�Cp�CC�C{CQ�C��C�
C
=C33Cp�C�RC��C(�CQ�C�\C�
C{C=qCz�C�C�C=qCp�C��C�
C{CQ�Cz�C�C�C(�C\)Cz�C�C�C(�C\)Cz�C�RC  C(�CG�C�CC��C{CG�C�C�RC  C33CQ�Cz�C�C�C(�CQ�Cz�CC  C33C\)C�CC
=CQ�Cp�C��C�HC(�CQ�C�CC 
=C (�C ffC ��C �HC!{C!33C!ffC!��C!�HC"�C"\)C"�\C"�RC#  C#G�C#z�C#��C#�
C${C$\)C$��C$C$��C%=qC%�C%�RC%�HC&{C&\)C&��C&C&��C'(�C'ffC'�C'�HC(
=C(33C(p�C(�RC(�C)
=C)33C)z�C)�RC)��C*{C*=qC*z�C*�RC+  C+(�C+Q�C+�C+��C,
=C,G�C,p�C,��C,�C-33C-ffC-�\C-��C.{C.\)C.�\C.C.�C/33C/z�C/C/��C0(�C0\)C0�C0��C1(�C1\)C1�\C1�
C2�C2Q�C2�C2�RC3  C3G�C3z�C3��C3�
C4(�C4ffC4��C4C5
=C5Q�C5�\C5�C5�HC6(�C6ffC6��C6C7  C7=qC7�C7��C8  C8(�C8Q�C8�\C8�HC9�C9Q�C9�C9C:{C:\)C:��C:�
C;  C;=qC;�C;��C<
=C<G�C<p�C<�C<�C=33C=z�C=C=��C>(�C>\)C>�\C>��C?{C?G�C?z�C?�C?�C@(�C@z�C@C@��CA(�CAffCA��CA�HCB33CBz�CBCC  CC33CCp�CC�CD  CDQ�CD��CD��CE
=CEG�CE�CE�
CF(�CFffCF��CF��CG
=CGQ�CG��CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                             @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A�ĜA�n�A�VA�AٸRAٰ!A٩�A٧�A٥�A٣�A١�Aٛ�Aٗ�Aٗ�AٓuAٕ�AٓuAٓuAّhAٓuAٓuAّhAٕ�A٧�AٶFA���A�/A� �A���Aُ\A�E�A؉7A�ĜA֣�A�C�A�ĜA�?}AԬA�$�A���A��AґhA�\)A��#AоwA��A���A��A�7LAͧ�A̩�A�?}A��A���A�t�Aʰ!A� �A���A�5?A�E�A���A�JA�&�AǬA�5?A��Aß�A�z�A��;A�n�A��wA�?}A���A�n�A��A�^5A�oA���A��+A�`BA�C�A��9A��A��;A�K�A�Q�A�"�A��#A���A��;A��A��mA�C�A��`A��jA�A�
=A���A���A�-A�ȴA�S�A�O�A��yA���A�A���A�{A���A��A��A���A��A���A�=qA��A�%A��
A���A��A��A�VA�VA��A��
A�JA���A��
A�1A��A��A�~�A��A~��A}��A{�PAx�jAv�\Ar��Ap{Am�Ajv�Af��AdȴA^��A]`BA\��A\��A\��A\��A\E�A[�AX^5AUC�AR�AP��AO�FANVAL�AK��AIdZAHE�AG�hAF=qADJABjAA+A?�A?%A>^5A=ƨA<�+A;�A8�A7�
A7"�A6ffA5�
A4��A3XA1VA/�A.ffA,�`A+�;A*��A(A�A%��A#�^A!�A��AXAĜA�TA"�A-A;dA��A��A9XA��A��AAl�A�AĜA��A?}A�mA�yAM�A �A�;AAO�A
Q�A	t�AȴA(�A��A�AAjA��AC�A1A\)A ��A �DA z�A �@���@�o@��@�r�@�dZ@��#@��@�ƨ@�^5@���@�@�l�@�33@�n�@�7L@�1'@�|�@�
=@�\@�`B@�@�l�@��y@�!@�^5@�-@�5?@�5?@��@�z�@��m@�;d@��@柾@�n�@�x�@�A�@�|�@�K�@��H@◍@�{@�`B@���@�1@ާ�@�I�@�V@ٙ�@�%@ش9@��m@�@�M�@ՙ�@�r�@���@љ�@�/@���@��@�E�@ͩ�@�Ĝ@��
@ˍP@�dZ@�o@�=q@���@�  @�33@Ƨ�@�M�@��#@�O�@��@Ĵ9@å�@�;d@���@\@�v�@�^5@�M�@�E�@�$�@���@���@�X@��@��u@�z�@�r�@�Z@�1@���@���@�o@��@��@�l�@�
=@��@��@���@�x�@��j@�p�@��^@��-@���@��-@���@��7@��@�Q�@��F@�|�@�|�@�t�@��@��@���@�=q@���@�O�@�7L@��@���@�r�@�  @��
@��w@���@�o@�=q@�-@��@��@��@���@��u@�r�@�9X@�(�@�1@��@��;@�ƨ@���@��P@��@�|�@�dZ@�\)@�+@�@��y@��@�v�@�M�@��@�@��-@��h@��7@��@�x�@�hs@�7L@�V@���@���@�Q�@��@�  @��m@�ƨ@���@�|�@�l�@�;d@�"�@�
=@��@���@�ff@�E�@�-@���@��T@�x�@��`@���@��
@�l�@�o@���@��/@��@�j@� �@���@�t�@�K�@�33@��y@��!@���@��+@�n�@�V@�$�@�`B@�V@���@��/@��9@�Z@��w@�C�@�+@�E�@��T@��^@��@�Ĝ@��D@�Z@�b@�ƨ@��@�"�@�@���@��H@��@��!@�^5@�-@�p�@��@���@���@�Ĝ@��j@�j@�9X@��
@��F@���@���@�|�@�;d@�@���@�v�@���@��@��-@�X@�Ĝ@�z�@�Z@�Q�@���@��F@���@�t�@�+@��y@��!@��\@�~�@�v�@�~�@�~�@�v�@�^5@��@���@��7@�X@�?}@�V@��9@�1'@�|�@�
=@���@���@���@��\@�n�@��@�@��^@���@���@��h@�`B@���@�9X@���@���@��F@���@�S�@�33@�
=@���@�V@�{@���@��@�/@��@���@�|�@�\)@�;d@��H@���@��+@�n�@�$�@��@��T@�@���@�hs@�G�@�V@��`@���@���@��j@��9@�z�@�r�@�I�@��@��@��@�o@���@�M�@�J@���@�&�@�1@�@
=@~ȴ@~��@~v�@~E�@~$�@~$�@}�T@}�-@}p�@|�/@{�
@{�F@{o@z��@z�\@y�@xĜ@xQ�@x �@w�w@w��@wK�@w+@w
=@vV@u@uO�@t�D@sƨ@s�@s@r�\@q��@q�@pĜ@pr�@o�@o�w@o;d@n�y@nV@m?}@mV@l�D@k�
@k33@j�!@jM�@jJ@i��@ihs@i&�@i�@i�@h��@h��@h1'@g��@f��@f�R@fv�@fE�@e�h@d��@dz�@d�@b�\@bJ@a�#@a��@a��@a��@ax�@a&�@` �@_+@^�@^�R@^�R@^�R@^v�@]�@]�-@]`B@]�@\�@\j@\1@[�m@[�m@[�
@[dZ@["�@[o@Z��@Z~�@Zn�@Zn�@Z=q@Y�7@YG�@X�`@X��@X�u@X�u@X�u@X�u@XbN@W�@Wl�@V��@Vv�@U?}@T9X@S�@Q��@Qhs@P��@P��@P��@PA�@O��@O;d@O�@N��@N�@N��@NV@NE�@NV@N5?@N$�@N{@M/@L��@L��@L�D@LZ@L1@K�
@K�F@K33@K@J~�@I�@I��@I�7@I�7@Ihs@HĜ@HQ�@G��@G+@F�+@E��@E�@EV@D�/@Dz�@D(�@C�
@C�F@C��@C��@C��@CS�@CS�@CC�@C@B��@Bn�@A�@AX@A�@@��@@�9@@r�@@b@?��@?�@?�P@?\)@?+@>��@>��@>��@>�+@>V@>5?@>@=�T@=@=?}@=V@<�/@<��@<�@<�D@<j@<Z@<j@<Z@<Z@<Z@<j@<j@<j@<j@<1@:�@:M�@:�@9��@9�^@9��@9G�@97L@8��@8��@8Ĝ@8��@8  @7|�@7
=@6�R@6v�@6E�@5@5O�@5/@4��@3ƨ@3t�@3dZ@333@3o@3@3@2�H@3@2�@2�H@2��@2�!@2��@2�!@2�!@2��@2^5@2M�@2-@1�@1�#@1�7@1X@17L@1�@1%@0�`@0�9@0�@0Q�@0  @/��@/l�@/�@.��@.ȴ@.��@.$�@.@-�@-�-@-�@,�/@,��@,z�@,(�@+C�@*��@*�!@*��@*�\@*^5@*M�@*M�@*=q@*-@*�@*�@*J@)�^@)%@(�u@( �@(  @'��@'�@'�@'\)@&�@&��@&5?@%@$j@#�F@#S�@"��@"�!@"�\@"^5@"J@!7L@ Ĝ@ Q�@�@+@��@��@�h@�@��@�D@z�@j@(�@�m@ƨ@�@dZ@S�@@�@�!@n�@=q@J@�#@��@��@��@�7@X@X@X@7L@%@�u@Q�@1'@ �@1'@ �@ �@b@b@b@b@b@b@  @�@�;@��@��@��@��@�w@��@��@�w@�@�P@�P@K�@;d@;d@�@�+@ff@ff@V@E�@{@�@@�h@O�@?}@O�@O�@O�@O�@O�@O�@`B@p�@p�@O�@?}@V@��@V@�@V@�/@��@�D@j@9X@ƨ@t�@C�@"�@�H@��@n�@-@�@�#@�^A���A���AڸRAڧ�A��
A��/A���Aڰ!Aڟ�A�E�A�7LA�7LA�"�A��mA��#A�AپwA�Aٺ^AټjAټjAٴ9Aٰ!Aٲ-Aٰ!A٬A٩�AٮA٩�A٣�A٩�A٧�A٣�A٥�A٩�A٥�A١�A٥�A٣�A١�A٥�A١�Aٟ�A٣�Aٝ�A١�Aٟ�Aٛ�Aٝ�Aٛ�Aٕ�Aٛ�Aٛ�Aٕ�Aٙ�Aٙ�Aٕ�Aٗ�Aٛ�Aٙ�Aٗ�Aٛ�Aٗ�AٓuAٗ�Aٗ�Aٕ�AٓuAٗ�AّhAّhAٗ�AٓuAّhAٕ�Aٗ�Aٕ�Aٕ�AٓuAٗ�AٓuAّhAٕ�AٓuAّhAٗ�AٓuAُ\Aٕ�AٓuAّhAٕ�AٓuAُ\AّhAٕ�AّhAُ\AٓuAٓuAُ\AٓuAّhAُ\AٓuAٕ�Aُ\AّhAٕ�Aٕ�AّhAٕ�Aٕ�AّhAٓuAٗ�AّhAُ\AٓuAّhAُ\AٍPAٓuAٓuAٍPAّhAُ\AٍPA٧�A٧�A٩�A٧�A٬A٥�Aٟ�A٥�A٬A٩�A٬A٧�Aٲ-AٮAپwA���A���A��HA��A�%A�A�bA�bA�JA�VA�-A�33A�/A�9XA�;dA�7LA�1'A�$�A�"�A��A��A��A��A��A��A�1A���A��A��TA��/A��HA��#AٶFAًDAٛ�Aه+A�x�AفA�l�A�hsA�ffA�bNA�ZA�O�A�K�A�33A�{A�oA���A��#Aة�A؅A�r�A�VA�(�A��A�bA��A��#A��
A׸RAׅA�r�A�+A���AֶFA֩�A֕�A�x�A�n�A�hsA�`BA�XA�Q�A�E�A�5?A�33A�+A�  A��A��HA���Aմ9Aգ�A՛�AՕ�AՑhAՇ+A�A�A� �A��A�1A���A�  A���A��AԾwA�v�A�ZA�I�A�K�A�E�A�33A�+A�"�A�{A�A���A���A���A��`A���AӸRAӗ�AӃA�O�A�"�A��AҮAҗ�Aҗ�AғuAҍPAҏ\AғuAґhAҏ\AґhAґhAҋDA҉7A�~�A�XA�9XA�-A��A�JA��A��HA��A�ƨAѬA�v�A�E�A��A�VAЧ�A�G�A�+A��yAυA�-A��A�bA�ƨA�x�A�&�A�JA���A��A��yA��yA��A���A��A�ĜA���A���A�1A�+A�A�A�ZA�Q�A�=qA�(�A��A�{A�1A�A��;AͰ!A�M�A��A��A���A�ĜA���A̧�A�ffA�A�A�S�A�\)A�Q�A�7LA�-A��A�
=A���A��A��A��mA��TA��HA��A���A˸RA˶FA˸RA˺^A���A���A���A˕�AˍPA�z�A�n�A�K�A�(�A�A��yA��`A��;A��#A���Aʴ9Aʉ7A�l�A�`BA�O�A�C�A�;dA�5?A�/A��A�oA��A�VA��A��A��A���A���A���A���A�A�
=A�
=A�1A�VA��A�"�A�/A�;dA�K�A�S�A�ZA�VA�Q�A�E�A�A�A�C�A�E�A�E�A�A�A�?}A�A�A�?}A�5?A�(�A��A��AɶFA�|�A�bNA�A�A�1'A�+A�(�A�&�A� �A�{A�bA�%A��yA���Aȕ�A�\)A�K�A�E�A�7LA�$�A���A��/A���A�ȴAǾwAǸRAǴ9Aǩ�Aǣ�Aǣ�Aǣ�AǕ�AǇ+A�|�A�r�A�\)A�G�A�-A�JA��yA��AƾwAƓuA�A�A�JA���A���Ať�A�x�A�5?A�  AĴ9A�{AÉ7A�?}A� �A�JA�JA��A���A´9A�A+A�ffA�S�A�C�A�-A�&�A��A�oA��A��A�ƨA��-A��A���A���A���A��PA��A�t�A�bNA�G�A�33A�(�A��A�  A��yA��^A���A��hA��A�z�A�r�A�VA�M�A�9XA�5?A�33A�33A�1'A�+A�(�A�$�A��A��A��A��A�oA���A�bA��9A���A���A��A�dZA�K�A�=qA��A��wA�n�A�Q�A�1'A��A���A��DA�v�A�x�A�t�A�l�A�hsA�dZA�jA�XA�Q�A�M�A�G�A�5?A�&�A�$�A� �A��A�1A�1A���A���A��yA��A��`A��A���A�ȴA���A��9A��^A���A���A���A��7A��A�t�A�t�A�l�A�dZA�ffA�jA�^5A�^5A�`BA�`BA�^5A�\)A�`BA�bNA�^5A�Q�A�I�A�A�A�A�A�=qA�33A�$�A��A�A��A�ffA�oA��A�E�A�JA���A��/A���A��!A�x�A�hsA�G�A�&�A���A��HA�ȴA���A��hA�t�A�E�A�+A��A���A���A��+A�jA�G�A�A�A�33A���A���A�5?A�{A��TA�x�A��A��#A��FA���A��\A��A�v�A�l�A�M�A�(�A��yA��wA���A��A�l�A�E�A��yA�jA�?}A�7LA�5?A�(�A�%A��/A���A���A��^A��7A�hsA�I�A�+A���A��mA��;A��/A��/A��#A���A��^A���A�z�A�S�A�(�A���A��A�9XA���A��7A��A�v�A�ffA�A�A��A��HA��+A�+A���A�~�A�C�A�bA��A�ĜA��A���A��hA��A�v�A�n�A�ffA�M�A�(�A�{A�{A�bA���A��A��yA��TA��;A��/A��#A��A��#A��/A��#A���A���A��jA��A���A��7A�z�A�hsA�E�A��A��yA���A��\A�1'A�A��A�z�A��A���A���A��DA�~�A�ffA�A�A��A��A�5?A���A���A��+A��wA���A�{A��A��`A��9A���A�v�A�E�A�9XA�{A���A�bNA�;dA� �A�oA���A��A��HA��#A���A���A�=qA�+A���A���A�K�A��A���A���A��A�XA�"�A���A��mA�t�A�dZA�S�A�K�A�K�A�K�A�K�A�E�A�;dA�A���A��A���A���A���A�ƨA��^A��-A��A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                             A���A�ĜA�n�A�VA�AٸRAٰ!A٩�A٧�A٥�A٣�A١�Aٛ�Aٗ�Aٗ�AٓuAٕ�AٓuAٓuAّhAٓuAٓuAّhAٕ�A٧�AٶFA���A�/A� �A���Aُ\A�E�A؉7A�ĜA֣�A�C�A�ĜA�?}AԬA�$�A���A��AґhA�\)A��#AоwA��A���A��A�7LAͧ�A̩�A�?}A��A���A�t�Aʰ!A� �A���A�5?A�E�A���A�JA�&�AǬA�5?A��Aß�A�z�A��;A�n�A��wA�?}A���A�n�A��A�^5A�oA���A��+A�`BA�C�A��9A��A��;A�K�A�Q�A�"�A��#A���A��;A��A��mA�C�A��`A��jA�A�
=A���A���A�-A�ȴA�S�A�O�A��yA���A�A���A�{A���A��A��A���A��A���A�=qA��A�%A��
A���A��A��A�VA�VA��A��
A�JA���A��
A�1A��A��A�~�A��A~��A}��A{�PAx�jAv�\Ar��Ap{Am�Ajv�Af��AdȴA^��A]`BA\��A\��A\��A\��A\E�A[�AX^5AUC�AR�AP��AO�FANVAL�AK��AIdZAHE�AG�hAF=qADJABjAA+A?�A?%A>^5A=ƨA<�+A;�A8�A7�
A7"�A6ffA5�
A4��A3XA1VA/�A.ffA,�`A+�;A*��A(A�A%��A#�^A!�A��AXAĜA�TA"�A-A;dA��A��A9XA��A��AAl�A�AĜA��A?}A�mA�yAM�A �A�;AAO�A
Q�A	t�AȴA(�A��A�AAjA��AC�A1A\)A ��A �DA z�A �@���@�o@��@�r�@�dZ@��#@��@�ƨ@�^5@���@�@�l�@�33@�n�@�7L@�1'@�|�@�
=@�\@�`B@�@�l�@��y@�!@�^5@�-@�5?@�5?@��@�z�@��m@�;d@��@柾@�n�@�x�@�A�@�|�@�K�@��H@◍@�{@�`B@���@�1@ާ�@�I�@�V@ٙ�@�%@ش9@��m@�@�M�@ՙ�@�r�@���@љ�@�/@���@��@�E�@ͩ�@�Ĝ@��
@ˍP@�dZ@�o@�=q@���@�  @�33@Ƨ�@�M�@��#@�O�@��@Ĵ9@å�@�;d@���@\@�v�@�^5@�M�@�E�@�$�@���@���@�X@��@��u@�z�@�r�@�Z@�1@���@���@�o@��@��@�l�@�
=@��@��@���@�x�@��j@�p�@��^@��-@���@��-@���@��7@��@�Q�@��F@�|�@�|�@�t�@��@��@���@�=q@���@�O�@�7L@��@���@�r�@�  @��
@��w@���@�o@�=q@�-@��@��@��@���@��u@�r�@�9X@�(�@�1@��@��;@�ƨ@���@��P@��@�|�@�dZ@�\)@�+@�@��y@��@�v�@�M�@��@�@��-@��h@��7@��@�x�@�hs@�7L@�V@���@���@�Q�@��@�  @��m@�ƨ@���@�|�@�l�@�;d@�"�@�
=@��@���@�ff@�E�@�-@���@��T@�x�@��`@���@��
@�l�@�o@���@��/@��@�j@� �@���@�t�@�K�@�33@��y@��!@���@��+@�n�@�V@�$�@�`B@�V@���@��/@��9@�Z@��w@�C�@�+@�E�@��T@��^@��@�Ĝ@��D@�Z@�b@�ƨ@��@�"�@�@���@��H@��@��!@�^5@�-@�p�@��@���@���@�Ĝ@��j@�j@�9X@��
@��F@���@���@�|�@�;d@�@���@�v�@���@��@��-@�X@�Ĝ@�z�@�Z@�Q�@���@��F@���@�t�@�+@��y@��!@��\@�~�@�v�@�~�@�~�@�v�@�^5@��@���@��7@�X@�?}@�V@��9@�1'@�|�@�
=@���@���@���@��\@�n�@��@�@��^@���@���@��h@�`B@���@�9X@���@���@��F@���@�S�@�33@�
=@���@�V@�{@���@��@�/@��@���@�|�@�\)@�;d@��H@���@��+@�n�@�$�@��@��T@�@���@�hs@�G�@�V@��`@���@���@��j@��9@�z�@�r�@�I�@��@��@��@�o@���@�M�@�J@���@�&�@�1@�@
=@~ȴ@~��@~v�@~E�@~$�@~$�@}�T@}�-@}p�@|�/@{�
@{�F@{o@z��@z�\@y�@xĜ@xQ�@x �@w�w@w��@wK�@w+@w
=@vV@u@uO�@t�D@sƨ@s�@s@r�\@q��@q�@pĜ@pr�@o�@o�w@o;d@n�y@nV@m?}@mV@l�D@k�
@k33@j�!@jM�@jJ@i��@ihs@i&�@i�@i�@h��@h��@h1'@g��@f��@f�R@fv�@fE�@e�h@d��@dz�@d�@b�\@bJ@a�#@a��@a��@a��@ax�@a&�@` �@_+@^�@^�R@^�R@^�R@^v�@]�@]�-@]`B@]�@\�@\j@\1@[�m@[�m@[�
@[dZ@["�@[o@Z��@Z~�@Zn�@Zn�@Z=q@Y�7@YG�@X�`@X��@X�u@X�u@X�u@X�u@XbN@W�@Wl�@V��@Vv�@U?}@T9X@S�@Q��@Qhs@P��@P��@P��@PA�@O��@O;d@O�@N��@N�@N��@NV@NE�@NV@N5?@N$�@N{@M/@L��@L��@L�D@LZ@L1@K�
@K�F@K33@K@J~�@I�@I��@I�7@I�7@Ihs@HĜ@HQ�@G��@G+@F�+@E��@E�@EV@D�/@Dz�@D(�@C�
@C�F@C��@C��@C��@CS�@CS�@CC�@C@B��@Bn�@A�@AX@A�@@��@@�9@@r�@@b@?��@?�@?�P@?\)@?+@>��@>��@>��@>�+@>V@>5?@>@=�T@=@=?}@=V@<�/@<��@<�@<�D@<j@<Z@<j@<Z@<Z@<Z@<j@<j@<j@<j@<1@:�@:M�@:�@9��@9�^@9��@9G�@97L@8��@8��@8Ĝ@8��@8  @7|�@7
=@6�R@6v�@6E�@5@5O�@5/@4��@3ƨ@3t�@3dZ@333@3o@3@3@2�H@3@2�@2�H@2��@2�!@2��@2�!@2�!@2��@2^5@2M�@2-@1�@1�#@1�7@1X@17L@1�@1%@0�`@0�9@0�@0Q�@0  @/��@/l�@/�@.��@.ȴ@.��@.$�@.@-�@-�-@-�@,�/@,��@,z�@,(�@+C�@*��@*�!@*��@*�\@*^5@*M�@*M�@*=q@*-@*�@*�@*J@)�^@)%@(�u@( �@(  @'��@'�@'�@'\)@&�@&��@&5?@%@$j@#�F@#S�@"��@"�!@"�\@"^5@"J@!7L@ Ĝ@ Q�@�@+@��@��@�h@�@��@�D@z�@j@(�@�m@ƨ@�@dZ@S�@@�@�!@n�@=q@J@�#@��@��@��@�7@X@X@X@7L@%@�u@Q�@1'@ �@1'@ �@ �@b@b@b@b@b@b@  @�@�;@��@��@��@��@�w@��@��@�w@�@�P@�P@K�@;d@;d@�@�+@ff@ff@V@E�@{@�@@�h@O�@?}@O�@O�@O�@O�@O�@O�@`B@p�@p�@O�@?}@V@��@V@�@V@�/@��@�D@j@9X@ƨ@t�@C�@"�@�H@��@n�@-@�@�#G�O�A���A���AڸRAڧ�A��
A��/A���Aڰ!Aڟ�A�E�A�7LA�7LA�"�A��mA��#A�AپwA�Aٺ^AټjAټjAٴ9Aٰ!Aٲ-Aٰ!A٬A٩�AٮA٩�A٣�A٩�A٧�A٣�A٥�A٩�A٥�A١�A٥�A٣�A١�A٥�A١�Aٟ�A٣�Aٝ�A١�Aٟ�Aٛ�Aٝ�Aٛ�Aٕ�Aٛ�Aٛ�Aٕ�Aٙ�Aٙ�Aٕ�Aٗ�Aٛ�Aٙ�Aٗ�Aٛ�Aٗ�AٓuAٗ�Aٗ�Aٕ�AٓuAٗ�AّhAّhAٗ�AٓuAّhAٕ�Aٗ�Aٕ�Aٕ�AٓuAٗ�AٓuAّhAٕ�AٓuAّhAٗ�AٓuAُ\Aٕ�AٓuAّhAٕ�AٓuAُ\AّhAٕ�AّhAُ\AٓuAٓuAُ\AٓuAّhAُ\AٓuAٕ�Aُ\AّhAٕ�Aٕ�AّhAٕ�Aٕ�AّhAٓuAٗ�AّhAُ\AٓuAّhAُ\AٍPAٓuAٓuAٍPAّhAُ\AٍPA٧�A٧�A٩�A٧�A٬A٥�Aٟ�A٥�A٬A٩�A٬A٧�Aٲ-AٮAپwA���A���A��HA��A�%A�A�bA�bA�JA�VA�-A�33A�/A�9XA�;dA�7LA�1'A�$�A�"�A��A��A��A��A��A��A�1A���A��A��TA��/A��HA��#AٶFAًDAٛ�Aه+A�x�AفA�l�A�hsA�ffA�bNA�ZA�O�A�K�A�33A�{A�oA���A��#Aة�A؅A�r�A�VA�(�A��A�bA��A��#A��
A׸RAׅA�r�A�+A���AֶFA֩�A֕�A�x�A�n�A�hsA�`BA�XA�Q�A�E�A�5?A�33A�+A�  A��A��HA���Aմ9Aգ�A՛�AՕ�AՑhAՇ+A�A�A� �A��A�1A���A�  A���A��AԾwA�v�A�ZA�I�A�K�A�E�A�33A�+A�"�A�{A�A���A���A���A��`A���AӸRAӗ�AӃA�O�A�"�A��AҮAҗ�Aҗ�AғuAҍPAҏ\AғuAґhAҏ\AґhAґhAҋDA҉7A�~�A�XA�9XA�-A��A�JA��A��HA��A�ƨAѬA�v�A�E�A��A�VAЧ�A�G�A�+A��yAυA�-A��A�bA�ƨA�x�A�&�A�JA���A��A��yA��yA��A���A��A�ĜA���A���A�1A�+A�A�A�ZA�Q�A�=qA�(�A��A�{A�1A�A��;AͰ!A�M�A��A��A���A�ĜA���A̧�A�ffA�A�A�S�A�\)A�Q�A�7LA�-A��A�
=A���A��A��A��mA��TA��HA��A���A˸RA˶FA˸RA˺^A���A���A���A˕�AˍPA�z�A�n�A�K�A�(�A�A��yA��`A��;A��#A���Aʴ9Aʉ7A�l�A�`BA�O�A�C�A�;dA�5?A�/A��A�oA��A�VA��A��A��A���A���A���A���A�A�
=A�
=A�1A�VA��A�"�A�/A�;dA�K�A�S�A�ZA�VA�Q�A�E�A�A�A�C�A�E�A�E�A�A�A�?}A�A�A�?}A�5?A�(�A��A��AɶFA�|�A�bNA�A�A�1'A�+A�(�A�&�A� �A�{A�bA�%A��yA���Aȕ�A�\)A�K�A�E�A�7LA�$�A���A��/A���A�ȴAǾwAǸRAǴ9Aǩ�Aǣ�Aǣ�Aǣ�AǕ�AǇ+A�|�A�r�A�\)A�G�A�-A�JA��yA��AƾwAƓuA�A�A�JA���A���Ať�A�x�A�5?A�  AĴ9A�{AÉ7A�?}A� �A�JA�JA��A���A´9A�A+A�ffA�S�A�C�A�-A�&�A��A�oA��A��A�ƨA��-A��A���A���A���A��PA��A�t�A�bNA�G�A�33A�(�A��A�  A��yA��^A���A��hA��A�z�A�r�A�VA�M�A�9XA�5?A�33A�33A�1'A�+A�(�A�$�A��A��A��A��A�oA���A�bA��9A���A���A��A�dZA�K�A�=qA��A��wA�n�A�Q�A�1'A��A���A��DA�v�A�x�A�t�A�l�A�hsA�dZA�jA�XA�Q�A�M�A�G�A�5?A�&�A�$�A� �A��A�1A�1A���A���A��yA��A��`A��A���A�ȴA���A��9A��^A���A���A���A��7A��A�t�A�t�A�l�A�dZA�ffA�jA�^5A�^5A�`BA�`BA�^5A�\)A�`BA�bNA�^5A�Q�A�I�A�A�A�A�A�=qA�33A�$�A��A�A��A�ffA�oA��A�E�A�JA���A��/A���A��!A�x�A�hsA�G�A�&�A���A��HA�ȴA���A��hA�t�A�E�A�+A��A���A���A��+A�jA�G�A�A�A�33A���A���A�5?A�{A��TA�x�A��A��#A��FA���A��\A��A�v�A�l�A�M�A�(�A��yA��wA���A��A�l�A�E�A��yA�jA�?}A�7LA�5?A�(�A�%A��/A���A���A��^A��7A�hsA�I�A�+A���A��mA��;A��/A��/A��#A���A��^A���A�z�A�S�A�(�A���A��A�9XA���A��7A��A�v�A�ffA�A�A��A��HA��+A�+A���A�~�A�C�A�bA��A�ĜA��A���A��hA��A�v�A�n�A�ffA�M�A�(�A�{A�{A�bA���A��A��yA��TA��;A��/A��#A��A��#A��/A��#A���A���A��jA��A���A��7A�z�A�hsA�E�A��A��yA���A��\A�1'A�A��A�z�A��A���A���A��DA�~�A�ffA�A�A��A��A�5?A���A���A��+A��wA���A�{A��A��`A��9A���A�v�A�E�A�9XA�{A���A�bNA�;dA� �A�oA���A��A��HA��#A���A���A�=qA�+A���A���A�K�A��A���A���A��A�XA�"�A���A��mA�t�A�dZA�S�A�K�A�K�A�K�A�K�A�E�A�;dA�A���A��A���A���A���A�ƨA��^A��-A��A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                             ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	��B	�`B	��B	�lB	��B	�+B	��B	�`B	��B	�`B	��B	�`B	�+B	�`B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B
xB
\B
$tB
2�B
5?B
1'B
-�B
*�B
F?B
h>B
��B
� B
�\B
�qB
��B
��B
��B
�B
��B;BoB
�B
��B
�oB
��B
��B
�mB
��B
�B
��B
��BB�B0!BCaBU2Bh
B~�B��B�OB�^B�B�)B��B 4B�B
	BhB�B	B"�B'�B#:B#�B#�B#nB �B�B(XB(�B'�B)*B&�B!�B�BB{BB�B�]B�DB��B�B�2B�KB�$B�OB��B��BzDBt�Bm]BlWB[�BQ�BP�BP}BPBOvBM6BK�B,B#nB!�BCB�BB\B�B
��B
��B
�GB
�B
�B
ӏB
�RB
��B
��B
��B
�%B
w�B
qB
gmB
Y�B
I�B
8RB
!�B
�B
 iB	�8B	�KB	�3B	��B	�XB	�LB	��B	��B	�4B	�IB	�oB	|�B	l�B	b�B	]/B	W
B	I�B	GEB	>�B	5B	0�B	,=B	$�B	B	7B	�B	�B	PB	�B	
rB		B�cB	 iB��B�B��B�2B�B��B�,B�B�B�B�B�,B�BΥB�pB�B�0B�0B̘B�^B�6B�)B��BɆB�KB�KB��B��B��B�^B�BбB��B�[B�B��B��B�B��B�sB��B�5B�jB�pB�B�BB�B�B�NB��B��B�ZB�ZB�ZB�B��B��B�,B�8B�DB�B�QB��B�)B��B�B�GB��B�AB��B��B��B��B��B�B��B�%B��B�`B��B��B��B�`B�`B�"B�B�B�>B�B�>B��B�PB��B�cB	 iB	�B	�B	B	�B	+B	
	B	\B	B	@B	B	�B	�B	�B	�B	~B	�B	($B	.�B	.�B	0�B	1�B	2-B	4�B	7B	;dB	>�B	@OB	A�B	B�B	G�B	M6B	Q�B	UgB	X�B	Z�B	]/B	_;B	_pB	`BB	f�B	h�B	l�B	ncB	o5B	p;B	p�B	qB	q�B	r|B	s�B	w2B	z�B	{B	{B	{�B	|B	|B	|�B	~(B	cB	�4B	��B	�uB	�B	��B	��B	�B	�SB	��B	��B	��B	�tB	��B	�zB	��B	�RB	��B	��B	�B	�=B	�B	�UB	��B	��B	�nB	�FB	�B	��B	��B	�*B	�0B	��B	�}B	��B	�UB	��B	�gB	�tB	�tB	��B	ɆB	�^B	̘B	͟B	�B	�B	�BB	�B	�HB	�}B	�B	ѷB	� B	� B	�TB	҉B	ҽB	�,B	ԕB	ԕB	��B	רB	��B	��B	ںB	�#B	�WB	�WB	یB	��B	��B	��B	��B	ݘB	ޞB	ߤB	�B	�B	�B	�B	�NB	��B	�B	�TB	�B	��B	��B	��B	�B	�`B	�B	�B	��B	�B	�;B	��B	�B	�TB	�B	�>B	�xB	�B	��B	�B	�PB	�B	��B	�PB	��B	��B	��B	��B	��B	��B	��B
 iB
;B
B
B
B
B
�B
B
B
�B
fB
�B

=B

�B

�B
DB
�B
�B
�B
PB
�B
�B
�B
�B
PB
VB
"B
�B
4B
 B
 B
4B
�B
�B
:B
@B
B
@B
@B
@B
�B
FB
B
MB
�B
�B
B
�B
�B
+B
+B
�B
1B
�B
1B
�B
eB
1B
�B
B
�B
�B
B
kB
�B
=B
=B
=B
�B
�B
�B
B
B
~B
�B
�B
�B
�B
�B
 'B
 \B
!�B
!bB
!bB
!�B
!bB
!bB
!�B
$B
#:B
$B
$@B
$@B
$tB
$�B
$�B
%B
%�B
%�B
&LB
&�B
&�B
'B
*eB
)�B
)�B
)�B
*0B
+B
+B
+B
+kB
+�B
,B
,B
,=B
,=B
,�B
,�B
-CB
-CB
-CB
-wB
-wB
-CB
.B
-�B
-�B
.B
.IB
.IB
0!B
/�B
0UB
0�B
0�B
1[B
4nB
4nB
49B
4�B
4�B
4�B
4�B
5B
4�B
4�B
5B
4�B
6B
6zB
6�B
7LB
7�B
7LB
7�B
9XB
8�B
9�B
9XB
9$B
9�B
9�B
9�B
:�B
:�B
:�B
;dB
:�B
;0B
:�B
:�B
<B
=<B
=qB
=qB
=�B
=�B
>B
>BB
>�B
?HB
>�B
?}B
@OB
@OB
@�B
AUB
A B
AUB
AUB
A�B
A�B
AUB
A�B
A�B
B'B
B[B
B�B
B�B
B�B
B�B
C�B
D3B
C�B
D3B
F?B
FB
FB
FB
FB
E�B
FB
E�B
GzB
G�B
G�B
G�B
GEB
GEB
G�B
G�B
HB
H�B
H�B
IRB
I�B
I�B
I�B
I�B
I�B
JXB
JXB
JXB
J�B
J�B
J�B
J�B
J�B
K^B
K^B
K�B
L0B
K�B
K�B
K�B
K�B
L0B
LdB
LdB
L�B
MB
NB
N�B
OB
P�B
P}B
P�B
P�B
P�B
QNB
Q�B
RTB
RTB
RTB
RTB
R�B
R�B
R�B
R�B
R�B
S&B
R�B
S�B
S�B
S�B
T,B
T,B
TaB
T�B
T�B
T�B
T�B
UgB
VB
VB
VB
VB
U�B
V�B
V�B
W
B
W�B
XEB
X�B
YKB
YKB
YB
Y�B
ZQB
ZQB
Z�B
Z�B
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
[WB
Z�B
\)B
\]B
[�B
\�B
\�B
\�B
]/B
]/B
]dB
]/B
]�B
^B
]�B
^jB
^B
^jB
^jB
^�B
_B
^�B
_B
_;B
_;B
_�B
_pB
_pB
_pB
_�B
_�B
_�B
_�B
_�B
_�B
_pB
_�B
_pB
_;B
`BB
`�B
a�B
a|B
a�B
a|B
a�B
bNB
a�B
b�B
bNB
b�B
bNB
c�B
c�B
c�B
d�B
d&B
d�B
d�B
e,B
d�B
e�B
f�B
ffB
ffB
gB
f�B
f�B
f�B
f�B
f�B
gB
gB
gB
gB
gB
g8B
gB
f�B
gmB
g8B
g�B
g�B
g�B
h>B
h
B
hsB
h>B
h>B
h�B
hsB
h�B
h�B
iDB
iDB
iyB
jB
i�B
jKB
jB
j�B
j�B
jB
kB
k�B
k�B
k�B
k�B
lWB
m)B
m�B
m�B
m�B
m�B
m�B
n/B
m�B
m�B
n/B
n/B
m�B
n/B
ncB
o5B
o�B
pB
pB
pB
poB
p;B
poB
qAB
qvB
q�B
q�B
s�B
tB
t�B
t�B
t�B
t�B
t�B
u%B
u�B
v+B
v�B
v�B
w�B
xlB
x�B
x�B
y�B
zB
y�B
zB
zB
zxB
z�B
z�B
z�B
{B
{JB
{B
{�B
{�B
|PB
|PB
|�B
|�B
|�B
|�B
}"B
}"B
}"B
}VB
}"B
}VB
}�B
~(B
~]B
~�B
~�B
~�B
~�B
~�B
~�B
~]B
~�B
~�B
~]B
~�B
~�B
~�B
~�B
~�B
~�B
~�B
.B
~�B
~�B
~�B
~�B
~�B
.B
~�B
�B
~�B
.B
~�B
~�B
~�B
~�B
~�B
.B
.B
�B
cB
�4B
��B
��B
�iB
�iB
�iB
�iB
��B
��B
�iB
��B
��B
�;B
�B
�oB
�oB
�;B
�B
�;B
�;B
�oB
�oB
�;B
��B
�B
�uB
�AB
�uB
��B
��B
�B
�{B
��B
��B
�B
B	�lB
 4B	�B	�B	�%B	�VB	��B	��B
�B	��B	��B	��B	�B	�>B	��B	��B	�%B	��B	��B	��B	�fB	�2B	�`B	�+B	��B	��B	��B	�+B	�fB	�%B	�ZB	��B	�`B	��B	��B	�fB	��B	��B	�2B	�%B	��B	��B	��B	��B	��B	�`B	��B	�`B	��B	��B	��B	��B	�2B	�+B	��B	��B	��B	��B	��B	��B	�ZB	�`B	��B	��B	�B	��B	�`B	�B	��B	��B	�B	��B	��B	�`B	��B	��B	�`B	��B	��B	��B	��B	�ZB	��B	��B	�B	��B	��B	��B	�ZB	�`B	�TB	�%B	��B	��B	��B	�`B	�`B	�ZB	�ZB	��B	�ZB	�+B	��B	�ZB	��B	�2B	��B	��B	��B	�2B	��B	��B	��B	��B	��B	�2B	��B	��B	�fB	�lB	�rB	��B	�`B	�rB	��B	�DB	��B	�.B
1B
"B
"B
�B

	B
	lB
�B

	B
�B
VB
xB
�B
�B
	�B
�B
_B
�B
 �B
&LB
*�B
*eB
1�B
3�B
&�B
/�B
5�B
6B
5B
6�B
7B
9XB
9�B
3�B
4�B
4nB
2�B
4nB
5?B
2�B
7B
33B
1'B
0!B
.�B
,�B
/B
0�B
9XB
+6B
-�B
0!B
(XB
1�B
(�B
)_B
'�B
)_B
)�B
(�B
,B
,�B
-wB
0�B
7�B
?B
H�B
I�B
P}B
[#B
X�B
[�B
_�B
e�B
c B
kQB
sB
t�B
�MB
��B
�B
�lB
��B
��B
�(B
��B
��B
�\B
��B
��B
��B
��B
�B
�'B
��B
��B
��B
�bB
�\B
�'B
�'B
��B
��B
�nB
�!B
��B
��B
��B
��B
��B
�RB
��B
ȴB
�B
�dB
˒B
�TB
�
B
خB
ںB
��B
�B
�HB
�B
�NB
�2B
��B
�B
�KB
�"B
�oB
��B
��B
�lB
�>B
�+B
��B
�	B
�B
��B
��B
�lB
��B
��B
��B
�B  B�B+B�B�B{B1B
��B
��B
�cB
�DB
�cB
��B
�)B
�dB
��B
��B
��B
��B
��B
��B
�VB
�B
��B
�~B
�fB
�MB
~�B
�iB
�iB
�4B
~�B
�lB
��B
�B
��B
��B
��B
�FB
�B
��B
��B
�WB
�B
�B
��B
��B
�QB
�B
�2B
�B
�dB
��B
�^B
�?B
��B
�0B
��B
ƨB
ԕB
یB
�TB
��B
�8B
�B
��B
�B
�)B
�B
�B
�)B
�/B
�B
��B
�PB
��B
��B iBAB
�B�BB�B7BkB�B�B�BBB�B=B�B$@B'RB#�B$@B*�B+�B*�B)*B)�B,B,�B.�B:�B;�B>wB>�B>�B@�BB�BCaBB'BGzBG�BK�BK�BOvBQ�BQNBR�BZ�B^�B_�BbNBdZBf�Bh�Bh>BgmBh>Bj�Bl"Bk�Bl"BrBt�Bv�BzDB�lB��B��B�hB��B�oB�4B��B�B�B�B�FB�eB��B�zB��B�eB�eB�wB��B�B��B�B��B��B��B��B�^B�dB��B�XB��B��B�}B��BB�3BȀBʌBɺB�RB��B�&BںB�?B�sB�BB��B� B�KB�>B�ZBB�B;B��B�xB�B��B��B�]B�VB��B;B�B�BB  B;B�B�B�B�B	�B+B�B�BfB	lB	lB�B
�BBB~BJB�B(BB:B4B�B:B�B�BSBBYB_B�B�BB�B�B�B$B�B�BSB$�B0�B1'B!BIB!�B �B"hB �B$B-CB'�B&�B%�B+B+B%FB(�B"4B#:B!�B#�B#:BOB'�B!�B$tB"hB($B"hB"hB"�B#�B&�B#�B#�B#nB'�B#nB"�B#:B$tB$B#�B$�B#�B"hB"hB!�B'�B�B&�B"hB#B$tB!�B �B"hB"4B \B�B �B!-B�B�BOB"4B!�B!bBIBIBB�B=B�B/�B!B/�B+6B+kB'B($B$�B'RB*eB,�B'�B.�B(�B#�B)�B'B(XB&�B&�B+kB'RB*0B)_B&LB&LB($B!�B�B 'B+B3hB)�B'B+B1�B,=B(XB"hB!�B!B�BqBeBBcTB4�B!BVB!BOB"hB*�B&LB�B7B+B�BOB7B+BB�B7B�B+B�B�BBoB�B4B B:BoB�BuB@BoB�BxBB!B	�B+B	lB�B�B�BDB�B�BPBMB	�B�B�B+B �B�cB iB �B 4B�]B�PB;B�cB��B�B��B�JB�B�B��B��B��B�>B��B��B�`B�`B�8B�B��B�`B��B�2B�TB�|B�ZB�`B�B�5B�TB�B�B�cB�vB�B�	B��B�BخBخB�B�pB��BרB�B�pB�B�KB��BȀB��B��B��B�B��B��B�hB��B�B��B��B��B�B�6B��B�RB��B��B�B�XB��B��B�'B�RB��B�FB�B��B��B��B~]B�PB�4Bv�B.By�By	BxlBw�BxByrB�DBy�By>Bo�BncBpBrGBq�Bm�Bp�Bm)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                             G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                             G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202007072154312020070721543120200707215431202007072154312020070721543120200707215431SI  SI  ARFMARFM                                                                                                                                                2020062223470720200622234707IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020070300003420200703000034QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020070300003420200703000034QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020070612390620200706123906IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI      ARSQ    SIQC    V2.1                                                                                                                                    20200707214339              CF      PSAL                            ?���G�O�D�:�G�O�?�  G�O�Bad PSAL Drift                      SI      ARSQ    SIQC    V2.1                                                                                                                                              20200707214835    CF                  PSAL            G�O�?uG�O�CG�HG�O�?�                  Bad PSAL Drift  SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2020070721552020200707215520IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2020V01                                            CTD_for_DMQC_2020V01                                            2020070721552020200707215520IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2020070721552020200707215520IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                