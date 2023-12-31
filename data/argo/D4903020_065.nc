CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-02-09T06:18:57Z creation; 2021-03-26T17:01:02Z DMQC;      
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20210209061857  20210326170212  4903020 4903020 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               A   AAA  AOAO7836_008777_065                 7836_008777_065                 2C  2C  DD  SOLO_II                         SOLO_II                         8777                            8777                            V2.6; SBE602 19Apr19            V2.6; SBE602 19Apr19            853 853 @�]	~���@�]	~���11  @�]	��n@�]	��n@;���>@;���>�d�&�q��d�&�q�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�\@=p�@z�H@�p�@�G�@�G�@�p�A  A ��A-p�A@  A^�RA\)A�Q�A�Q�A�  A��A�Q�A�Q�A��B   B�
B�
B�
B Q�B((�B/�
B7�
B@  BH  BP(�BXQ�B`(�Bh(�Bp  Bx  B�
B��
B��B�  B�{B�{B��B�  B�  B��
B��B�  B�{B�  B�  B�{B��B��B�{B�  B�  B��B�  B�{B�  B�{B�  B�{B�{B�  B�{B�  C 
=C
=C
=C
=C
=C
�C
=C  C  C�C  C
=C
=C  C
=C  C   C!��C$  C%��C(
=C*  C+��C.
=C0  C2
=C4
=C5��C7��C:  C<
=C>
=C@  CB  CC��CE��CG��CI��CL  CM��CO��CR  CT  CU��CW��CZ  C\
=C^  C`  Cb  Cc��Ce��Cg��Ci��Ck��Cm��Cp  Cq��Cs�Cu�Cx  Cz
=C|  C~  C�  C�  C�  C�  C�
=C�  C�  C�C���C���C���C���C���C�  C�C�  C�  C�C�  C�  C���C���C���C�  C�  C�  C���C�  C�
=C�
=C�\C�C�  C�C�C�  C�C�  C���C�  C�  C�C�  C���C���C�C�C���C�C�C�C�  C�  C���C�  C�  C�C�C�C�
=C���C�  C�C���C���C�  C�  C���C�  C�  C���C���C�
=C�\C�
=C�
=C�  C���C���C���C�  C�  C���C�  C�C�
=C�C�C�C�  C���C���C�  C�  C���C�  C�C�  C�  C�  C���C���C���C���C�  C�  C�C�C�
=C�
=C�
=C�
=C�
=C�C�  C���C�  C�
=C�
=C�C�C�
=C�C�  C�C�C�  C�  D   D ��D  D� D�D� D��D}qD  D}qD�RD}qD�D��D  D}qD�qD��D	�D	�D
  D
}qD�D� D  D� D�qDz�D�qD� D�qD��D  D}qD�qD� DD�D�D� D  D� D�D��D�D� D  D� D�D�DD��D�D��D  D}qD  D�D  D� DD�DD��D �D ��D!�D!� D!�qD"� D#�D#�D$�D$�D%D%��D&  D&� D'  D'}qD(  D(}qD(�qD)��D*�D*}qD*�qD+� D,�D,� D-  D-� D-�qD.��D/�D/� D0�D0� D0�qD1��D2D2� D3  D3��D4  D4� D5  D5��D6�D6}qD6��D7� D8D8}qD9  D9��D9�qD:z�D:�qD;� D;�qD<z�D=  D=��D>�D>� D>��D?z�D@  D@}qDA�DA�DBDB� DC  DC� DD  DDz�DE  DE��DE�qDF��DGDG� DH  DH�DIDI� DI��DJ� DKDK��DL  DL� DM�DM� DN  DN� DO  DO}qDO��DP}qDQ  DQ� DQ�qDRz�DS  DS��DT  DTz�DU  DU� DU�qDV� DW�DW��DX  DX}qDY�DY�DZDZ� DZ�qD[}qD[�qD\��D]  D]� D^�D^� D^��D_z�D`�D`�Da�Da� Db�Db��Dc  Dc}qDc�qDd� Dd�qDe� Df  Df��Dg�Dg�Dh  Dh� Di�Di��Dj  Dj��Dk  Dk}qDk�qDlz�Dl�qDm� Dn  Dn� Do�Do�Dp  Dp}qDq  Dq� Dr�Dr��Dr�qDsxRDs�qDt� Du�Du��Dv�Dv�Dv�qDw� Dx�Dx}qDx�qDy� Dz  Dz� Dz�qD{}qD|�D|�D}D}�D~D~}qD~�RD}qD�HD�>�D�|)D��)D��)D�=qD�� D��HD��qD�AHD���D�� D�  D�@ D�� D��HD�HD�AHD���D��HD���D�@ D�� D��HD��D�AHD��HD�D�  D�AHD�~�D��)D��qD�@ D��HD�� D���D�@ D�� D�� D�HD�AHD��HD��HD�  D�B�D���D��HD�HD�B�D���D��HD�HD�=qD�}qD�� D�  D�@ D��HD�� D��D�B�D�~�D���D���D�>�D�� D�� D�  D�@ D�~�D�D�HD�=qD�}qD���D�  D�@ D�� D���D�  D�AHD�~�D���D���D�>�D�~�D��qD���D�AHD���D��HD�  D�AHD���D��HD�  D�@ D��HD��HD��D�AHD�~�D�� D�  D�AHD��HD�� D�  D�AHD��HD���D��qD�@ D��HD��HD�  D�AHD��HD��HD�  D�@ D�� D��HD�HD�AHD���D�� D�  D�>�D�}qD���D�  D�@ D��HD��HD�HD�@ D�� D��HD�HD�=qD�� D��HD�  D�AHD���D��HD���D�@ D��HD��HD�  D�@ D�~�D���D�HD�B�D��HD�� D�  D�AHD���D���D�  D�@ D�� D�� D���D�AHD���D�� D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D��HD�� D���D�@ D�� D�� D�  D�>�D�~�D���D�  D�AHD�� D�� D���D�>�D�}qD�� D�HD�AHD�~�D�� D��D�@ D�� D��HD���D�@ D�� D��HD��D�@ D��HD��HD���D�>�D��HD�� D�  D�@ D�}qD��qD�  D�>�D�}qD�� D���D�>�D�~�D��HD�  D�=qD�~�D�� D�  D�>�D�}qD���D�HD�AHD�� D���D���D�@ D���D��HD�HD�@ D�� D���D���D�@ D��HD��HD�HD�@ D��HD�� D�  D�AHD�� D���D�  D�@ D D��HD��D�AHD�}qD�� D�HD�>�D�~�Dľ�D���D�AHDŀ Dž�D�  D�B�Dƀ D�� D�HD�@ D�~�D��HD��D�AHDȁHD��HD��D�AHDɀ Dɾ�D�  D�AHDʀ D��HD��D�C�D˂�D��HD�HD�AHD́HD��HD���D�>�D�~�DͽqD���D�@ D΁HD��HD�  D�@ D�~�DϾ�D�  D�AHDЁHD��HD�HD�@ Dр D��HD�  D�@ DҁHD�D�HD�@ DӀ DӾ�D�  D�>�D�~�D��HD�HD�@ D�~�D��HD�  D�>�Dւ�D��HD�HD�AHD׀ D��HD���D�=qD؀ D��HD�HD�AHDـ D��HD�  D�@ DځHD��HD���D�@ Dۀ D�� D�HD�@ D܀ D��HD�  D�AHD݂�D��HD���D�@ DށHD�� D�HD�AHD�~�D��HD�HD�>�D�}qD�� D�  D�@ D�HD�� D���D�@ D�}qD⾸D�  D�@ D�HD��HD�HD�>�D� D�� D���D�>�D�HD��HD��D�AHD�~�D�� D�HD�@ D�~�D羸D�  D�B�D�HD�� D��qD�@ D�HD龸D���D�@ D� D��HD���D�>�D�}qD뾸D�HD�>�D� D��HD���D�@ D�HD�� D�  D�AHD�HD�D�HD�AHDD��HD�  D�AHD���D�� D�  D�@ D� D��HD�  D�>�D� D��HD�HD�@ D�~�D�D�HD�B�D�HD�� D���D�=qD�� D��HD�HD�@ D��HD��HD���D�>�D�� D��HD�HD�AHD���D��HD��D�B�D�� D�� D��D�B�D�� ?#�
?k�?���?�Q�?�G�@�\@\)@�R@.{@:�H@Q�@fff@xQ�@�ff@���@��H@��\@���@���@���@�G�@Ǯ@�\)@�
=@޸R@�@��@�Q�A ��A�A��A��A  A�
A�A�A\)A#33A'
=A*�HA0  A4z�A8��A=p�AA�AE�AI��AL��AP��AS�
AW�A[�A`  Ae�Ah��Al��AqG�AuAy��A~{A���A�33A��A�
=A�G�A�(�A�ffA�G�A�(�A�
=A���A�(�A�ffA���A�33A��A�\)A���A�(�A�
=A�G�A�(�A�ffA�  A�=qA�z�A��RA���A�33A�ffA���A��
A�ffAУ�A�33A�p�A�  A��A�(�A�ffA���A�33A��A�A�=qA�z�A�\)A�A��
A��RA�G�A��
A�A��B ��B{B
=Bz�B��B
=B(�B	p�B
�RB�B��B��B�HB�B��B{B\)B��B�B33Bz�B{B�B�B�\B�
B!G�B"ffB#�B%G�B&ffB'�
B(��B*=qB+�B,��B.{B/�B1�B2�RB4  B5p�B6�HB8  B9G�B:�\B;�B<��B>{B?\)B@��BB{BC�BD��BF{BG�BH��BJ{BK33BLz�BM��BN�RBO�
BQ�BRffBS�
BT��BVffBW�BY�BZffB[�B\��B]�B_33B`Q�Bap�Bb�\Bc�Bd��Bf{Bg33Bhz�Bi��Bk
=Blz�BmBo
=BpQ�Bq��Br�RBs�
Bt��Bu�Bw
=Bx(�By�Bz�\B{�B|��B~{B\)B�=qB��RB�33B��B�=qB��RB�G�B��
B�ffB���B��B�(�B���B�G�B�  B��\B��B��B�Q�B���B��B�  B��\B�33B�B�Q�B��HB�p�B�{B���B�G�B��B���B�G�B�  B���B�\)B�  B���B�G�B��B�z�B��B��B�Q�B��HB��B�(�B���B�\)B�  B�z�B��B�B�ffB���B��B�{B���B�\)B�  B��\B�33B�B�ffB���B��B�(�B��RB�G�B��
B�ffB�
=B�p�B�{B���B�33B��
B�z�B���B���B�(�B���B�\)B�  B��\B�33B�B�ffB���B���B�(�B��HB�\)B��B��\B��B�B�ffB�
=B���B�(�B��RB�\)B��B��\B��B�B�Q�B��HB��B�(�B¸RB�G�B��B�z�B�
=Bř�B�=qB���B�G�B��B�z�B�
=BɮB�=qB��HB�p�B�  B�z�B�33B�B�ffB���Bϙ�B�=qB��HBхB�=qB���B�p�B�  B�z�B�
=BՅB�{B֣�B�\)B��BظRB�G�B�B�Q�B��HB�p�B�  B܏\B��B�B�Q�B��HB�p�B�{B��\B�33B��
B�z�B��B�B�ffB�
=B噚B�=qB���B�B�(�B�RB�\)B��B�z�B��B�B�=qB�RB�\)B��B�\B�33B�B�ffB���B�B�(�B���B�p�B�{B��RB�\)B��B���B�\)B��
B�ffB��HB�\)B�  B���B�\)B�B�Q�B��HB��B�Q�B���B���C 
=C =qC �\C �HC33C�\C�HC�CffC��C  CG�C��C��C=qC�\C��C{C\)C�RC{CffC�RC��C=qC��C��C33Cp�C�HC	33C	z�C	�RC
�C
z�C
C  CffCC
=CQ�C�C{CQ�C��C  CffC��C�CQ�C�RC
=C=qC��C
=CffC�C�C\)C�C�CffC��C
=Cp�C��C{C\)C�RC�C�C�HC33Cp�C��C33C��C�C33C�\C  CG�C�C��CQ�C�\C��C\)C��C�HCG�C�RC  C=qC�C{CQ�C�RC �C p�C �C!{C!�C!��C"{C"�C"�C#(�C#�\C#��C$=qC$�\C$��C%Q�C%�\C&  C&\)C&��C'�C'\)C'C(�C(ffC(�HC)33C)z�C)��C*=qC*�\C+
=C+Q�C+��C,�C,\)C,�C-�C-\)C-�C.{C.Q�C.��C/  C/33C/\)C/�RC/��C0
=C0ffC0��C0��C1�C1=qC1�\C1�RC1��C233C2ffC2�C2�HC3�C3Q�C3��C3��C4{C4=qC4�C4�RC4��C5(�C5z�C5�\C5�C6
=C6Q�C6�\C6�RC7{C7(�C7p�C7�C7�HC833C8G�C8��C8�
C9  C9\)C9�\C9�C:{C:=qC:p�C:��C:�HC;33C;z�C;��C;��C<�C<�C<�\C<��C=�C=\)C=��C=��C>�C>=qC>��C>��C?  C?G�C?ffC?��C?�C@(�C@z�C@�\C@�HCA{CAG�CA��CA�CB  CB=qCB\)CB�RCB�
CC{CC\)CCz�CC��CD  CD=qCDz�CD�CD�CE{CEffCEz�CE�
CF  CF=qCFz�CF��CF��CG{CGQ�CG�\CG�RCH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                      ?��@�\@=p�@z�H@�p�@�G�@�G�@�p�A  A ��A-p�A@  A^�RA\)A�Q�A�Q�A�  A��A�Q�A�Q�A��B   B�
B�
B�
B Q�B((�B/�
B7�
B@  BH  BP(�BXQ�B`(�Bh(�Bp  Bx  B�
B��
B��B�  B�{B�{B��B�  B�  B��
B��B�  B�{B�  B�  B�{B��B��B�{B�  B�  B��B�  B�{B�  B�{B�  B�{B�{B�  B�{B�  C 
=C
=C
=C
=C
=C
�C
=C  C  C�C  C
=C
=C  C
=C  C   C!��C$  C%��C(
=C*  C+��C.
=C0  C2
=C4
=C5��C7��C:  C<
=C>
=C@  CB  CC��CE��CG��CI��CL  CM��CO��CR  CT  CU��CW��CZ  C\
=C^  C`  Cb  Cc��Ce��Cg��Ci��Ck��Cm��Cp  Cq��Cs�Cu�Cx  Cz
=C|  C~  C�  C�  C�  C�  C�
=C�  C�  C�C���C���C���C���C���C�  C�C�  C�  C�C�  C�  C���C���C���C�  C�  C�  C���C�  C�
=C�
=C�\C�C�  C�C�C�  C�C�  C���C�  C�  C�C�  C���C���C�C�C���C�C�C�C�  C�  C���C�  C�  C�C�C�C�
=C���C�  C�C���C���C�  C�  C���C�  C�  C���C���C�
=C�\C�
=C�
=C�  C���C���C���C�  C�  C���C�  C�C�
=C�C�C�C�  C���C���C�  C�  C���C�  C�C�  C�  C�  C���C���C���C���C�  C�  C�C�C�
=C�
=C�
=C�
=C�
=C�C�  C���C�  C�
=C�
=C�C�C�
=C�C�  C�C�C�  C�  D   D ��D  D� D�D� D��D}qD  D}qD�RD}qD�D��D  D}qD�qD��D	�D	�D
  D
}qD�D� D  D� D�qDz�D�qD� D�qD��D  D}qD�qD� DD�D�D� D  D� D�D��D�D� D  D� D�D�DD��D�D��D  D}qD  D�D  D� DD�DD��D �D ��D!�D!� D!�qD"� D#�D#�D$�D$�D%D%��D&  D&� D'  D'}qD(  D(}qD(�qD)��D*�D*}qD*�qD+� D,�D,� D-  D-� D-�qD.��D/�D/� D0�D0� D0�qD1��D2D2� D3  D3��D4  D4� D5  D5��D6�D6}qD6��D7� D8D8}qD9  D9��D9�qD:z�D:�qD;� D;�qD<z�D=  D=��D>�D>� D>��D?z�D@  D@}qDA�DA�DBDB� DC  DC� DD  DDz�DE  DE��DE�qDF��DGDG� DH  DH�DIDI� DI��DJ� DKDK��DL  DL� DM�DM� DN  DN� DO  DO}qDO��DP}qDQ  DQ� DQ�qDRz�DS  DS��DT  DTz�DU  DU� DU�qDV� DW�DW��DX  DX}qDY�DY�DZDZ� DZ�qD[}qD[�qD\��D]  D]� D^�D^� D^��D_z�D`�D`�Da�Da� Db�Db��Dc  Dc}qDc�qDd� Dd�qDe� Df  Df��Dg�Dg�Dh  Dh� Di�Di��Dj  Dj��Dk  Dk}qDk�qDlz�Dl�qDm� Dn  Dn� Do�Do�Dp  Dp}qDq  Dq� Dr�Dr��Dr�qDsxRDs�qDt� Du�Du��Dv�Dv�Dv�qDw� Dx�Dx}qDx�qDy� Dz  Dz� Dz�qD{}qD|�D|�D}D}�D~D~}qD~�RD}qD�HD�>�D�|)D��)D��)D�=qD�� D��HD��qD�AHD���D�� D�  D�@ D�� D��HD�HD�AHD���D��HD���D�@ D�� D��HD��D�AHD��HD�D�  D�AHD�~�D��)D��qD�@ D��HD�� D���D�@ D�� D�� D�HD�AHD��HD��HD�  D�B�D���D��HD�HD�B�D���D��HD�HD�=qD�}qD�� D�  D�@ D��HD�� D��D�B�D�~�D���D���D�>�D�� D�� D�  D�@ D�~�D�D�HD�=qD�}qD���D�  D�@ D�� D���D�  D�AHD�~�D���D���D�>�D�~�D��qD���D�AHD���D��HD�  D�AHD���D��HD�  D�@ D��HD��HD��D�AHD�~�D�� D�  D�AHD��HD�� D�  D�AHD��HD���D��qD�@ D��HD��HD�  D�AHD��HD��HD�  D�@ D�� D��HD�HD�AHD���D�� D�  D�>�D�}qD���D�  D�@ D��HD��HD�HD�@ D�� D��HD�HD�=qD�� D��HD�  D�AHD���D��HD���D�@ D��HD��HD�  D�@ D�~�D���D�HD�B�D��HD�� D�  D�AHD���D���D�  D�@ D�� D�� D���D�AHD���D�� D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D��HD�� D���D�@ D�� D�� D�  D�>�D�~�D���D�  D�AHD�� D�� D���D�>�D�}qD�� D�HD�AHD�~�D�� D��D�@ D�� D��HD���D�@ D�� D��HD��D�@ D��HD��HD���D�>�D��HD�� D�  D�@ D�}qD��qD�  D�>�D�}qD�� D���D�>�D�~�D��HD�  D�=qD�~�D�� D�  D�>�D�}qD���D�HD�AHD�� D���D���D�@ D���D��HD�HD�@ D�� D���D���D�@ D��HD��HD�HD�@ D��HD�� D�  D�AHD�� D���D�  D�@ D D��HD��D�AHD�}qD�� D�HD�>�D�~�Dľ�D���D�AHDŀ Dž�D�  D�B�Dƀ D�� D�HD�@ D�~�D��HD��D�AHDȁHD��HD��D�AHDɀ Dɾ�D�  D�AHDʀ D��HD��D�C�D˂�D��HD�HD�AHD́HD��HD���D�>�D�~�DͽqD���D�@ D΁HD��HD�  D�@ D�~�DϾ�D�  D�AHDЁHD��HD�HD�@ Dр D��HD�  D�@ DҁHD�D�HD�@ DӀ DӾ�D�  D�>�D�~�D��HD�HD�@ D�~�D��HD�  D�>�Dւ�D��HD�HD�AHD׀ D��HD���D�=qD؀ D��HD�HD�AHDـ D��HD�  D�@ DځHD��HD���D�@ Dۀ D�� D�HD�@ D܀ D��HD�  D�AHD݂�D��HD���D�@ DށHD�� D�HD�AHD�~�D��HD�HD�>�D�}qD�� D�  D�@ D�HD�� D���D�@ D�}qD⾸D�  D�@ D�HD��HD�HD�>�D� D�� D���D�>�D�HD��HD��D�AHD�~�D�� D�HD�@ D�~�D羸D�  D�B�D�HD�� D��qD�@ D�HD龸D���D�@ D� D��HD���D�>�D�}qD뾸D�HD�>�D� D��HD���D�@ D�HD�� D�  D�AHD�HD�D�HD�AHDD��HD�  D�AHD���D�� D�  D�@ D� D��HD�  D�>�D� D��HD�HD�@ D�~�D�D�HD�B�D�HD�� D���D�=qD�� D��HD�HD�@ D��HD��HD���D�>�D�� D��HD�HD�AHD���D��HD��D�B�D�� D�� D��D�B�G�O�?#�
?k�?���?�Q�?�G�@�\@\)@�R@.{@:�H@Q�@fff@xQ�@�ff@���@��H@��\@���@���@���@�G�@Ǯ@�\)@�
=@޸R@�@��@�Q�A ��A�A��A��A  A�
A�A�A\)A#33A'
=A*�HA0  A4z�A8��A=p�AA�AE�AI��AL��AP��AS�
AW�A[�A`  Ae�Ah��Al��AqG�AuAy��A~{A���A�33A��A�
=A�G�A�(�A�ffA�G�A�(�A�
=A���A�(�A�ffA���A�33A��A�\)A���A�(�A�
=A�G�A�(�A�ffA�  A�=qA�z�A��RA���A�33A�ffA���A��
A�ffAУ�A�33A�p�A�  A��A�(�A�ffA���A�33A��A�A�=qA�z�A�\)A�A��
A��RA�G�A��
A�A��B ��B{B
=Bz�B��B
=B(�B	p�B
�RB�B��B��B�HB�B��B{B\)B��B�B33Bz�B{B�B�B�\B�
B!G�B"ffB#�B%G�B&ffB'�
B(��B*=qB+�B,��B.{B/�B1�B2�RB4  B5p�B6�HB8  B9G�B:�\B;�B<��B>{B?\)B@��BB{BC�BD��BF{BG�BH��BJ{BK33BLz�BM��BN�RBO�
BQ�BRffBS�
BT��BVffBW�BY�BZffB[�B\��B]�B_33B`Q�Bap�Bb�\Bc�Bd��Bf{Bg33Bhz�Bi��Bk
=Blz�BmBo
=BpQ�Bq��Br�RBs�
Bt��Bu�Bw
=Bx(�By�Bz�\B{�B|��B~{B\)B�=qB��RB�33B��B�=qB��RB�G�B��
B�ffB���B��B�(�B���B�G�B�  B��\B��B��B�Q�B���B��B�  B��\B�33B�B�Q�B��HB�p�B�{B���B�G�B��B���B�G�B�  B���B�\)B�  B���B�G�B��B�z�B��B��B�Q�B��HB��B�(�B���B�\)B�  B�z�B��B�B�ffB���B��B�{B���B�\)B�  B��\B�33B�B�ffB���B��B�(�B��RB�G�B��
B�ffB�
=B�p�B�{B���B�33B��
B�z�B���B���B�(�B���B�\)B�  B��\B�33B�B�ffB���B���B�(�B��HB�\)B��B��\B��B�B�ffB�
=B���B�(�B��RB�\)B��B��\B��B�B�Q�B��HB��B�(�B¸RB�G�B��B�z�B�
=Bř�B�=qB���B�G�B��B�z�B�
=BɮB�=qB��HB�p�B�  B�z�B�33B�B�ffB���Bϙ�B�=qB��HBхB�=qB���B�p�B�  B�z�B�
=BՅB�{B֣�B�\)B��BظRB�G�B�B�Q�B��HB�p�B�  B܏\B��B�B�Q�B��HB�p�B�{B��\B�33B��
B�z�B��B�B�ffB�
=B噚B�=qB���B�B�(�B�RB�\)B��B�z�B��B�B�=qB�RB�\)B��B�\B�33B�B�ffB���B�B�(�B���B�p�B�{B��RB�\)B��B���B�\)B��
B�ffB��HB�\)B�  B���B�\)B�B�Q�B��HB��B�Q�B���B���C 
=C =qC �\C �HC33C�\C�HC�CffC��C  CG�C��C��C=qC�\C��C{C\)C�RC{CffC�RC��C=qC��C��C33Cp�C�HC	33C	z�C	�RC
�C
z�C
C  CffCC
=CQ�C�C{CQ�C��C  CffC��C�CQ�C�RC
=C=qC��C
=CffC�C�C\)C�C�CffC��C
=Cp�C��C{C\)C�RC�C�C�HC33Cp�C��C33C��C�C33C�\C  CG�C�C��CQ�C�\C��C\)C��C�HCG�C�RC  C=qC�C{CQ�C�RC �C p�C �C!{C!�C!��C"{C"�C"�C#(�C#�\C#��C$=qC$�\C$��C%Q�C%�\C&  C&\)C&��C'�C'\)C'C(�C(ffC(�HC)33C)z�C)��C*=qC*�\C+
=C+Q�C+��C,�C,\)C,�C-�C-\)C-�C.{C.Q�C.��C/  C/33C/\)C/�RC/��C0
=C0ffC0��C0��C1�C1=qC1�\C1�RC1��C233C2ffC2�C2�HC3�C3Q�C3��C3��C4{C4=qC4�C4�RC4��C5(�C5z�C5�\C5�C6
=C6Q�C6�\C6�RC7{C7(�C7p�C7�C7�HC833C8G�C8��C8�
C9  C9\)C9�\C9�C:{C:=qC:p�C:��C:�HC;33C;z�C;��C;��C<�C<�C<�\C<��C=�C=\)C=��C=��C>�C>=qC>��C>��C?  C?G�C?ffC?��C?�C@(�C@z�C@�\C@�HCA{CAG�CA��CA�CB  CB=qCB\)CB�RCB�
CC{CC\)CCz�CC��CD  CD=qCDz�CD�CD�CE{CEffCEz�CE�
CF  CF=qCFz�CF��CF��CG{CGQ�CG�\CG�RCH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                      @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@��@�_G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A��A��9A��RA��RA��wA�A��FA��wA���A���A���A�ȴA�ĜA��
A��`A��mA��yA��yA��A��yA��`A��`A��`A��TA��TA��mA��mA��A��A��A��A��A��mA��yA��HA��`A��`A��`A��A��A��yA��/A���A�ȴA��jA��A���A���A���A���A��PA�~�A�bNA�O�A�1'A���A���A�n�A�C�A�bNA�-A�v�A�A��hA�p�A�r�A�ƨA�^5A��\A��+A�~�A�&�A�ffA��#A�C�A���A��DA�\)A��!A�1'A���A��A�|�A�I�A�p�A�I�A�hsA�ȴA���A���A�G�A��^A�\)A���A��`A���A���A�A�A|1'Az�!Ay�mAyG�AxjAwƨAv�At�ApVAn�`Ann�An9XAm�TAm\)Al�HAk�AkS�Aj�uAi�TAi�wAi&�Ah�Ae�PAb^5A_�#A^�jA\�AZ��AY��AYt�AX�AW�FAV�`AV�AU�7AT{ASp�AR��ARffAQ��AP��APv�APA�AOC�AN�RAM�TALĜAL  AK%AJ(�AI�AH�!AG��AG33AF5?AC�;ABbNAA
=A@Q�A@�A?�A>��A>�A=�FA=?}A=VA;�A:ȴA9�^A8��A8{A7t�A6ZA5�A5�A533A4�A3p�A2ZA1��A1t�A1?}A0�9A/�-A.�A.1A-��A-�FA-��A,�HA+��A*�`A(��A'�A'`BA'
=A&�jA&-A%��A$~�A"��A ��AVAA�AK�A��A�AG�AE�Ap�A��AbA�hA�HAE�A�wAhsA�A+AĜAA�\A��AhsA��AA�9A�FAO�A
��A
�jA
JA	O�A^5A|�A+A�\AbNA5?A  A��A�hA�A�Az�A{A��AdZA�jA�+A z�@���@��R@�-@�\)@�9X@�+@�v�@�?}@���@�(�@��D@�33@�+@�@���@��@��@�=q@�@�@��@���@�O�@��D@�A�@ߝ�@�o@�M�@��#@�  @��y@���@���@ו�@�^5@Չ7@�%@� �@�t�@�;d@��@��H@�^5@�@ѡ�@�Ĝ@�t�@���@�G�@ˍP@�x�@�bN@��@�&�@�|�@�@�G�@���@���@���@�V@�7L@��9@�bN@��;@�dZ@�
=@��7@��w@�@�M�@�x�@��@�Ĝ@��u@�z�@�I�@��@��
@��@�|�@�K�@�C�@�;d@�+@�o@�ȴ@��@�&�@�1@�S�@�^5@��T@���@��@��@�I�@��
@�S�@��@��@���@���@��^@��7@�/@���@� �@�|�@�V@�p�@��@�(�@�t�@�?}@���@�1@�l�@�E�@���@��@�x�@���@�z�@�1@��P@��@�ff@��9@�b@���@�;d@�~�@��-@��@�j@�  @��P@�K�@�K�@�K�@�33@��@�hs@��@��u@�z�@�I�@�1@�
=@���@�hs@��9@�1'@�  @��@�ƨ@��@�K�@�"�@�
=@��!@�J@�@�`B@�%@��`@���@�j@� �@��;@��@��P@��@�t�@��H@�~�@�{@�@�X@���@�Ĝ@��@�(�@��m@���@�
=@��\@�~�@�^5@�V@�=q@��@���@��@��`@�Ĝ@�I�@�1@|�@~ff@}�@}@}�-@}O�@|�@|j@|1@{ƨ@{@z�\@y�^@y�@xbN@w�@w�;@w�w@w\)@wK�@v�R@vV@v@u��@u�h@uO�@t��@t�/@tj@s�
@sS�@so@r�H@rn�@r�@q��@qhs@q&�@p��@pQ�@o�;@o�w@o��@o�@o��@o��@o�P@ol�@oK�@n�@nE�@m��@mO�@l�/@l�D@l�@k�
@k��@kt�@j��@j^5@i�@i��@i�^@i��@ihs@h�`@h�9@h��@hA�@g�;@g��@g�@g;d@f��@f�@fV@e��@e�-@e�-@e��@e�@eO�@e�@d�@d�j@dj@d9X@d1@c�m@c�m@cƨ@cƨ@c�F@c�@co@b�!@bM�@a��@a��@a��@a�7@aX@`��@`1'@`  @`  @_�@_�;@_��@_�w@_�P@_�@^��@^v�@^ff@^E�@^E�@^5?@^$�@]�@]�-@\�@\��@\�@[33@Z~�@ZJ@Y��@Y%@X1'@Xb@X  @X  @W�@W��@W�w@W��@Vȴ@V$�@T��@T9X@SC�@R�\@R^5@R^5@RM�@Q��@Qx�@P��@PĜ@PQ�@O�@O�@O�;@O��@O��@O�P@O\)@N�y@N�@Nȴ@N��@Nff@N{@M�@M�-@M`B@L��@L�D@L9X@K��@K��@K"�@J�H@J~�@J=q@I�#@I��@Ix�@I&�@H��@H��@H�u@H1'@G�@G�w@F��@F��@Fv�@F5?@E��@E�-@E�h@E`B@D�@D��@DI�@C��@C�F@Ct�@Co@B��@B~�@A��@AG�@@��@@��@@A�@@  @?�@?|�@?K�@?+@?
=@>��@>��@>{@=�T@=�h@=p�@=`B@=/@=V@<I�@;�
@;�F@;�@;"�@:��@:��@:�!@:��@:�\@:^5@:-@9x�@8��@8�@8bN@7�@7��@7��@7|�@7\)@7;d@7\)@7\)@6�y@6�+@6V@6V@6{@5�@5V@4��@4�@4(�@3t�@3"�@3o@2��@2�!@2�!@2��@2n�@2J@1�@1��@1hs@1%@0��@0��@0�9@0r�@0Q�@01'@/��@/��@/l�@/\)@/;d@.V@-�@-��@-�-@-�h@-?}@,�D@,I�@,(�@,9X@,9X@,9X@,(�@+��@+��@*�@*�\@*~�@*M�@*�@*J@)��@)��@)X@)7L@)7L@)&�@)%@(��@(��@(�9@(r�@(r�@'�@'�w@'�@'K�@'
=@&��@&�@&�R@&E�@&$�@&@%�T@%`B@%�@$��@$�@$�/@$�@$z�@$9X@$1@#�m@#�F@#t�@#dZ@#dZ@#C�@#C�@#33@#"�@"�@"��@"n�@"^5@"=q@!�#@!��@!��@!��@!��@!��@!��@!��@!�^@!�^@!�^@!��@!��@!�7@!X@ �`@ Q�@ 1'@   @�P@�@�@��@V@@�@`B@/@�@V@�/@z�@��@S�@o@�@�!@�!@��@n�@-@-@�@��@�#@��@G�@&�@��@�u@�;@�P@\)@K�@�@
=@
=@��@�y@�@ȴ@ff@�T@`B@��@�D@j@j@Z@Z@Z@I�@9X@(�@(�@��@�
@��@"�@�@�\@^5@M�@=q@-@-@J@��@�@��@�^@��@��@�7@x�@hs@X@X@G�@G�@G�@7L@�@��@�9@A�@�@��@�w@�@��@|�@
=@��@v�@V@E�@5?@5?@@�@@�h@p�@p�@O�@�@�@(�@�m@��@dZ@C�@33@"�@@
�@
��@
�\@
~�@
^5@
J@	��@	hs@	&�@�`@��@Ĝ@��@�@�@r�@bN@A�@�@�;@��@��@�@�y@��@ff@ff@V@V@5?@$�@@�T@��@�-@`B@/@V@�j@�D@Z@I�@(�@1@�m@�
@ƨ@�F@�F@��@�@�@t�@S�@C�@C�@C�@o@�@�!@��@�\@~�@n�@M�@-@J@�@�@�@�@�#@��A���A���A���A���A��A��A��-A��9A��9A��-A��9A��FA��FA��9A��^A��9A��jA���A���A���A���A���A��wA���A�A���A��9A��RA��RA��RA��RA��jA��jA�A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A�A���A�A�A���A���A���A�A�A�ƨA�ĜA�ĜA�ƨA���A�ȴA�A���A��
A��;A��HA��`A��TA��HA��HA��yA��yA��yA��mA��mA��mA��TA��mA��yA��yA��mA��A��A��A��yA��mA��yA��yA��yA��yA��A��A��A��A��A��A��A��A��A��yA��mA��yA��mA��yA��mA��mA��mA��mA��mA��mA��mA��`A��`A��`A��`A��`A��TA��TA��`A��mA��mA��mA��mA��mA��`A��`A��TA��TA��;A��HA��HA��TA��`A��TA��TA��`A��`A��mA��`A��`A��mA��mA��A��mA��mA��mA��mA��mA��`A��`A��`A��A��A��A��A��A��A��A��A��yA��yA��yA��mA��A��`A��A��A��A��A��A��A��A��yA��TA��A��A��A��A��A��A��A��A��A��`A��A��mA��`A��mA��A��A��A��TA��TA��mA��mA��mA��`A��`A��HA��HA��HA��TA��TA��mA��mA��mA��`A��`A��HA��`A��yA��mA��`A��mA��`A��`A��`A��HA��HA��`A��`A��TA��mA��`A��TA��`A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��`A��TA��HA��#A��/A��HA���A���A��
A��#A���A���A�ƨA�ĜA�ƨA���A�ƨA���A���A���A���A�ȴA�A��RA��^A��FA��-A��!A��9A��A��!A��!A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��hA��PA��DA��+A��A��A��A�~�A��A��A�~�A�|�A�p�A�hsA�dZA�bNA�^5A�\)A�ZA�VA�Q�A�Q�A�O�A�M�A�I�A�E�A�5?A�33A�-A�+A�(�A�&�A� �A��A�JA�A���A��/A���A��wA��A���A���A��\A��A��A�|�A�n�A�jA�ffA�`BA�^5A�XA�XA�XA�I�A�C�A�1'A��A�A��TA��!A�r�A�A��#A��9A�x�A�`BA�;dA��A�A���A���A�ƨA���A�z�A�bNA�K�A�=qA�&�A��A��A�
=A�A��`A���A���A��uA��PA��DA��+A��+A��A��A��A�|�A�jA�S�A�C�A�-A��A���A��A�bA���A��PA�jA�/A��
A���A�bNA�9XA��A��mA��RA��\A�S�A���A�|�A��A��#A��9A��A�jA�A�A� �A���A���A��A�v�A�XA�A�A��A�A��A��+A�n�A�A�A��yA��!A�\)A���A�S�A�C�A��;A�ffA��HA��+A�t�A�hsA�K�A���A�l�A�C�A���A��^A���A��\A�t�A�l�A�bNA�O�A�33A�
=A��mA�ȴA��jA���A�x�A�jA�(�A�7LA��RA�ZA�7LA�JA��HA��+A�ZA�O�A�7LA��A���A���A���A��RA��\A�v�A�dZA�XA�O�A�9XA�bA���A��
A��hA��A�t�A�+A���A�t�A�7LA�{A���A��A�|�A�1A��RA�|�A�`BA�VA���A��A�jA�O�A�&�A�bA���A���A���A�r�A�;dA��A��#A���A�ZA�?}A��A��A��+A�x�A�hsA�bNA�O�A�9XA�&�A���A���A���A�bNA�I�A�(�A�JA��A���A�;dA�&�A��A���A��/A�ȴA��DA�ZA�O�A�A�A�7LA�(�A�JA��;A���A��wA���A��PA�r�A�ffA�bNA�`BA�ZA�XA�ZA�S�A�VA�S�A�I�A�+A���A��RA���A���A�M�A�7LA�-A�(�A��A�oA�
=A��A���A�p�A�M�A��A��`A��A���A��+A�t�A�bNA�XA�I�A�7LA�&�A�"�A�VA��TA��wA��9A���A�|�A�G�A� �A��`A�^5A�A~�yA~(�A}�A}�A|�yA|�9A|M�A{��A{��A{x�A{XA{?}A{�Az�Az��Az�uAzn�Az^5AzI�AzA�Az5?Az(�Az{Ay�mAy�;AyƨAy��Ay�AyS�AyXAyS�AyO�AyXAyO�AyO�Ay7LAx�Ax�Ax��AxQ�AxVAxM�AxQ�AxQ�AxVAx(�Ax  Aw�mAw��Aw��Aw�^Aw�wAw�-Aw�Aw�7AwdZAw7LAw+Av��Av�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                      A���A��A��9A��RA��RA��wA�A��FA��wA���A���A���A�ȴA�ĜA��
A��`A��mA��yA��yA��A��yA��`A��`A��`A��TA��TA��mA��mA��A��A��A��A��A��mA��yA��HA��`A��`A��`A��A��A��yA��/A���A�ȴA��jA��A���A���A���A���A��PA�~�A�bNA�O�A�1'A���A���A�n�A�C�A�bNA�-A�v�A�A��hA�p�A�r�A�ƨA�^5A��\A��+A�~�A�&�A�ffA��#A�C�A���A��DA�\)A��!A�1'A���A��A�|�A�I�A�p�A�I�A�hsA�ȴA���A���A�G�A��^A�\)A���A��`A���A���A�A�A|1'Az�!Ay�mAyG�AxjAwƨAv�At�ApVAn�`Ann�An9XAm�TAm\)Al�HAk�AkS�Aj�uAi�TAi�wAi&�Ah�Ae�PAb^5A_�#A^�jA\�AZ��AY��AYt�AX�AW�FAV�`AV�AU�7AT{ASp�AR��ARffAQ��AP��APv�APA�AOC�AN�RAM�TALĜAL  AK%AJ(�AI�AH�!AG��AG33AF5?AC�;ABbNAA
=A@Q�A@�A?�A>��A>�A=�FA=?}A=VA;�A:ȴA9�^A8��A8{A7t�A6ZA5�A5�A533A4�A3p�A2ZA1��A1t�A1?}A0�9A/�-A.�A.1A-��A-�FA-��A,�HA+��A*�`A(��A'�A'`BA'
=A&�jA&-A%��A$~�A"��A ��AVAA�AK�A��A�AG�AE�Ap�A��AbA�hA�HAE�A�wAhsA�A+AĜAA�\A��AhsA��AA�9A�FAO�A
��A
�jA
JA	O�A^5A|�A+A�\AbNA5?A  A��A�hA�A�Az�A{A��AdZA�jA�+A z�@���@��R@�-@�\)@�9X@�+@�v�@�?}@���@�(�@��D@�33@�+@�@���@��@��@�=q@�@�@��@���@�O�@��D@�A�@ߝ�@�o@�M�@��#@�  @��y@���@���@ו�@�^5@Չ7@�%@� �@�t�@�;d@��@��H@�^5@�@ѡ�@�Ĝ@�t�@���@�G�@ˍP@�x�@�bN@��@�&�@�|�@�@�G�@���@���@���@�V@�7L@��9@�bN@��;@�dZ@�
=@��7@��w@�@�M�@�x�@��@�Ĝ@��u@�z�@�I�@��@��
@��@�|�@�K�@�C�@�;d@�+@�o@�ȴ@��@�&�@�1@�S�@�^5@��T@���@��@��@�I�@��
@�S�@��@��@���@���@��^@��7@�/@���@� �@�|�@�V@�p�@��@�(�@�t�@�?}@���@�1@�l�@�E�@���@��@�x�@���@�z�@�1@��P@��@�ff@��9@�b@���@�;d@�~�@��-@��@�j@�  @��P@�K�@�K�@�K�@�33@��@�hs@��@��u@�z�@�I�@�1@�
=@���@�hs@��9@�1'@�  @��@�ƨ@��@�K�@�"�@�
=@��!@�J@�@�`B@�%@��`@���@�j@� �@��;@��@��P@��@�t�@��H@�~�@�{@�@�X@���@�Ĝ@��@�(�@��m@���@�
=@��\@�~�@�^5@�V@�=q@��@���@��@��`@�Ĝ@�I�@�1@|�@~ff@}�@}@}�-@}O�@|�@|j@|1@{ƨ@{@z�\@y�^@y�@xbN@w�@w�;@w�w@w\)@wK�@v�R@vV@v@u��@u�h@uO�@t��@t�/@tj@s�
@sS�@so@r�H@rn�@r�@q��@qhs@q&�@p��@pQ�@o�;@o�w@o��@o�@o��@o��@o�P@ol�@oK�@n�@nE�@m��@mO�@l�/@l�D@l�@k�
@k��@kt�@j��@j^5@i�@i��@i�^@i��@ihs@h�`@h�9@h��@hA�@g�;@g��@g�@g;d@f��@f�@fV@e��@e�-@e�-@e��@e�@eO�@e�@d�@d�j@dj@d9X@d1@c�m@c�m@cƨ@cƨ@c�F@c�@co@b�!@bM�@a��@a��@a��@a�7@aX@`��@`1'@`  @`  @_�@_�;@_��@_�w@_�P@_�@^��@^v�@^ff@^E�@^E�@^5?@^$�@]�@]�-@\�@\��@\�@[33@Z~�@ZJ@Y��@Y%@X1'@Xb@X  @X  @W�@W��@W�w@W��@Vȴ@V$�@T��@T9X@SC�@R�\@R^5@R^5@RM�@Q��@Qx�@P��@PĜ@PQ�@O�@O�@O�;@O��@O��@O�P@O\)@N�y@N�@Nȴ@N��@Nff@N{@M�@M�-@M`B@L��@L�D@L9X@K��@K��@K"�@J�H@J~�@J=q@I�#@I��@Ix�@I&�@H��@H��@H�u@H1'@G�@G�w@F��@F��@Fv�@F5?@E��@E�-@E�h@E`B@D�@D��@DI�@C��@C�F@Ct�@Co@B��@B~�@A��@AG�@@��@@��@@A�@@  @?�@?|�@?K�@?+@?
=@>��@>��@>{@=�T@=�h@=p�@=`B@=/@=V@<I�@;�
@;�F@;�@;"�@:��@:��@:�!@:��@:�\@:^5@:-@9x�@8��@8�@8bN@7�@7��@7��@7|�@7\)@7;d@7\)@7\)@6�y@6�+@6V@6V@6{@5�@5V@4��@4�@4(�@3t�@3"�@3o@2��@2�!@2�!@2��@2n�@2J@1�@1��@1hs@1%@0��@0��@0�9@0r�@0Q�@01'@/��@/��@/l�@/\)@/;d@.V@-�@-��@-�-@-�h@-?}@,�D@,I�@,(�@,9X@,9X@,9X@,(�@+��@+��@*�@*�\@*~�@*M�@*�@*J@)��@)��@)X@)7L@)7L@)&�@)%@(��@(��@(�9@(r�@(r�@'�@'�w@'�@'K�@'
=@&��@&�@&�R@&E�@&$�@&@%�T@%`B@%�@$��@$�@$�/@$�@$z�@$9X@$1@#�m@#�F@#t�@#dZ@#dZ@#C�@#C�@#33@#"�@"�@"��@"n�@"^5@"=q@!�#@!��@!��@!��@!��@!��@!��@!��@!�^@!�^@!�^@!��@!��@!�7@!X@ �`@ Q�@ 1'@   @�P@�@�@��@V@@�@`B@/@�@V@�/@z�@��@S�@o@�@�!@�!@��@n�@-@-@�@��@�#@��@G�@&�@��@�u@�;@�P@\)@K�@�@
=@
=@��@�y@�@ȴ@ff@�T@`B@��@�D@j@j@Z@Z@Z@I�@9X@(�@(�@��@�
@��@"�@�@�\@^5@M�@=q@-@-@J@��@�@��@�^@��@��@�7@x�@hs@X@X@G�@G�@G�@7L@�@��@�9@A�@�@��@�w@�@��@|�@
=@��@v�@V@E�@5?@5?@@�@@�h@p�@p�@O�@�@�@(�@�m@��@dZ@C�@33@"�@@
�@
��@
�\@
~�@
^5@
J@	��@	hs@	&�@�`@��@Ĝ@��@�@�@r�@bN@A�@�@�;@��@��@�@�y@��@ff@ff@V@V@5?@$�@@�T@��@�-@`B@/@V@�j@�D@Z@I�@(�@1@�m@�
@ƨ@�F@�F@��@�@�@t�@S�@C�@C�@C�@o@�@�!@��@�\@~�@n�@M�@-@J@�@�@�@�@�#G�O�A���A���A���A���A��A��A��-A��9A��9A��-A��9A��FA��FA��9A��^A��9A��jA���A���A���A���A���A��wA���A�A���A��9A��RA��RA��RA��RA��jA��jA�A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A�A���A�A�A���A���A���A�A�A�ƨA�ĜA�ĜA�ƨA���A�ȴA�A���A��
A��;A��HA��`A��TA��HA��HA��yA��yA��yA��mA��mA��mA��TA��mA��yA��yA��mA��A��A��A��yA��mA��yA��yA��yA��yA��A��A��A��A��A��A��A��A��A��yA��mA��yA��mA��yA��mA��mA��mA��mA��mA��mA��mA��`A��`A��`A��`A��`A��TA��TA��`A��mA��mA��mA��mA��mA��`A��`A��TA��TA��;A��HA��HA��TA��`A��TA��TA��`A��`A��mA��`A��`A��mA��mA��A��mA��mA��mA��mA��mA��`A��`A��`A��A��A��A��A��A��A��A��A��yA��yA��yA��mA��A��`A��A��A��A��A��A��A��A��yA��TA��A��A��A��A��A��A��A��A��A��`A��A��mA��`A��mA��A��A��A��TA��TA��mA��mA��mA��`A��`A��HA��HA��HA��TA��TA��mA��mA��mA��`A��`A��HA��`A��yA��mA��`A��mA��`A��`A��`A��HA��HA��`A��`A��TA��mA��`A��TA��`A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��yA��`A��TA��HA��#A��/A��HA���A���A��
A��#A���A���A�ƨA�ĜA�ƨA���A�ƨA���A���A���A���A�ȴA�A��RA��^A��FA��-A��!A��9A��A��!A��!A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��hA��PA��DA��+A��A��A��A�~�A��A��A�~�A�|�A�p�A�hsA�dZA�bNA�^5A�\)A�ZA�VA�Q�A�Q�A�O�A�M�A�I�A�E�A�5?A�33A�-A�+A�(�A�&�A� �A��A�JA�A���A��/A���A��wA��A���A���A��\A��A��A�|�A�n�A�jA�ffA�`BA�^5A�XA�XA�XA�I�A�C�A�1'A��A�A��TA��!A�r�A�A��#A��9A�x�A�`BA�;dA��A�A���A���A�ƨA���A�z�A�bNA�K�A�=qA�&�A��A��A�
=A�A��`A���A���A��uA��PA��DA��+A��+A��A��A��A�|�A�jA�S�A�C�A�-A��A���A��A�bA���A��PA�jA�/A��
A���A�bNA�9XA��A��mA��RA��\A�S�A���A�|�A��A��#A��9A��A�jA�A�A� �A���A���A��A�v�A�XA�A�A��A�A��A��+A�n�A�A�A��yA��!A�\)A���A�S�A�C�A��;A�ffA��HA��+A�t�A�hsA�K�A���A�l�A�C�A���A��^A���A��\A�t�A�l�A�bNA�O�A�33A�
=A��mA�ȴA��jA���A�x�A�jA�(�A�7LA��RA�ZA�7LA�JA��HA��+A�ZA�O�A�7LA��A���A���A���A��RA��\A�v�A�dZA�XA�O�A�9XA�bA���A��
A��hA��A�t�A�+A���A�t�A�7LA�{A���A��A�|�A�1A��RA�|�A�`BA�VA���A��A�jA�O�A�&�A�bA���A���A���A�r�A�;dA��A��#A���A�ZA�?}A��A��A��+A�x�A�hsA�bNA�O�A�9XA�&�A���A���A���A�bNA�I�A�(�A�JA��A���A�;dA�&�A��A���A��/A�ȴA��DA�ZA�O�A�A�A�7LA�(�A�JA��;A���A��wA���A��PA�r�A�ffA�bNA�`BA�ZA�XA�ZA�S�A�VA�S�A�I�A�+A���A��RA���A���A�M�A�7LA�-A�(�A��A�oA�
=A��A���A�p�A�M�A��A��`A��A���A��+A�t�A�bNA�XA�I�A�7LA�&�A�"�A�VA��TA��wA��9A���A�|�A�G�A� �A��`A�^5A�A~�yA~(�A}�A}�A|�yA|�9A|M�A{��A{��A{x�A{XA{?}A{�Az�Az��Az�uAzn�Az^5AzI�AzA�Az5?Az(�Az{Ay�mAy�;AyƨAy��Ay�AyS�AyXAyS�AyO�AyXAyO�AyO�Ay7LAx�Ax�Ax��AxQ�AxVAxM�AxQ�AxQ�AxVAx(�Ax  Aw�mAw��Aw��Aw�^Aw�wAw�-Aw�Aw�7AwdZAw7LAw+Av��Av�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                      ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�>B�8B�lB�lB�8B�B�lB��B�B�B��B��B�8B�B�2B��B�`B��B��B��B��B�%B��B�TB�TB��B�B�B�|B�GB�B�B��B�5B��B�B�B��B�)B��B�B��B��B�`B��B�B�B�BޞB�B��B�/B�dB�]B�B�B�?B��B�B�KB��B�B�~B~�BrBjKBUgB2�B�B iB��B� B��B��Bn�Bc BWsBMB:*B.�B#�B�B;B�B�QBɺB�hB��B�~B�7B��Bz�Bo�Bf�B]�BPHB9XB$@BbB�WB��B�#B��B�HB�dB��B�B��B��B�~B�	B��B�B�oBy�Bt�Bo Bl�BkQBf�B_�BS�BB[B49B+kB!-B�B\B�B	�B;B
�]B
�xB
�TB
�cB
�B
�2B
�&B
�B
ޞB
�B
خB
ҽB
�6B
�mB
�HB
��B
��B
��B
�XB
�FB
�!B
��B
�$B
�~B
�oB
{B
u�B
s�B
rGB
p�B
l"B
h>B
c�B
`�B
[�B
WsB
Q�B
I�B
GzB
A�B
4B
($B
%�B
#:B
 'B
B
1B
�B
�B
B
�B
B
�B
{B
;B
 4B	��B	��B	�B	��B	�B	��B	ݘB	��B	��B	�B	�TB	͟B	ȴB	�wB	��B	��B	��B	��B	��B	�LB	��B	�OB	�B	�7B	�1B	��B	�YB	�uB	�MB	��B	�B	��B	�VB	�~B	�_B	��B	�SB	��B	|�B	xlB	v�B	t�B	t�B	uZB	rGB	o�B	k�B	i�B	h�B	hsB	gmB	f�B	g�B	e�B	d&B	h
B	b�B	bB	_pB	^�B	[WB	YB	\]B	T,B	T,B	QB	T�B	NB	LdB	M�B	P}B	M�B	M�B	PHB	J�B	JXB	I�B	K�B	IRB	H�B	D�B	E�B	A�B	B�B	B�B	B�B	?}B	=�B	=qB	=<B	<B	9�B	=B	9$B	:*B	9�B	9$B	8�B	7�B	8B	8RB	6�B	6zB	6B	6FB	5�B	4nB	4nB	6B	4nB	3�B	5tB	6zB	7�B	5�B	7LB	8B	:*B	9�B	9XB	9�B	9�B	:*B	9$B	:*B	9�B	9�B	:*B	9�B	8�B	=B	>wB	>�B	>�B	@�B	A B	AUB	A B	@�B	A�B	A B	A�B	A�B	B'B	B'B	B'B	AUB	A�B	B'B	B�B	E�B	F�B	H�B	JXB	MB	M�B	NB	P�B	QB	R�B	S[B	TaB	W�B	W
B	W�B	W�B	V�B	W?B	XB	YB	Z�B	\�B	`�B	c�B	ffB	gB	jB	p�B	p�B	r�B	t�B	xB	|�B	}�B	}VB	�B	�B	�~B	��B	��B	��B	�	B	�~B	��B	��B	�FB	�0B	�B	�UB	��B	�tB	�FB	�B	�B	�B	��B	�-B	ŢB	ŢB	ŢB	�tB	�EB	��B	бB	�B	�dB	�HB	�NB	�NB	�TB	��B	�2B	�8B	��B	�B	��B	��B	�B	��B	�fB	��B	�B	�"B	�.B
 �B
�B
uB
�B
�B

rB
�B
4B
�B
$B
�B
1B
�B
�B
 'B
#�B
'RB
(XB
)_B
)�B
+�B
-B
.�B
3hB
5?B
5�B
:^B
<B
>wB
C-B
D�B
EmB
E�B
F�B
H�B
J�B
K�B
L�B
P�B
Q�B
VB
X�B
[�B
^B
^5B
_B
`�B
aHB
dZB
ffB
h�B
jKB
jB
k�B
m]B
m�B
o�B
r�B
uZB
v�B
x8B
y�B
|B
}�B
�B
��B
��B
��B
��B
�%B
��B
��B
�fB
��B
��B
�7B
��B
��B
��B
�:B
�uB
�B
��B
��B
��B
�B
�=B
��B
�VB
��B
�hB
��B
�B
�zB
�*B
�0B
��B
�=B
�B
�IB
�IB
�UB
�'B
��B
�B
��B
��B
��B
�RB
��B
��B
�^B
��B
�B
�<B
�B
��B
�B
�HB
�gB
�B
�?B
�EB
��B
�XB
��B
�jB
��B
�pB
�pB
�BB
��B
��B
ӏB
�&B
�[B
�[B
ӏB
ӏB
��B
�gB
רB
�B
خB
�B
�B
ٴB
��B
��B
یB
�5B
ޞB
�vB
�B
�B
��B
�B
�DB
��B
�B
��B
��B
��B
��B
�"B
��B
�B
�B
�GB
�TB
�+B
�2B
��B
��B
��B
�8B
�B
�B
��B
�VB
��B
��B
��B
��B
��B
��B
�cB 4B 4B iB �B;BBBuBGB�BB�BYB�B�B	7B	�B
�BBBB�BVB�B�B�B�B4B@BuB@B�B�B�BMB�B�B�BBkB	B�BB�BIB!B�B 'B 'B!�B!�B"�B#:B#:B#�B#nB#nB$@B%B%zB&B&B&LB&�B&LB(XB(�B(�B)*B*0B+B+B+kB+kB+�B,B,=B-�B/OB/B/�B0�B1'B1�B1[B1�B2�B2�B2�B2�B33B3�B4B5?B6�B7�B7�B7�B8�B9$B9XB9XB9�B9�B9�B9�B:^B:�B;�B<jB=qB=�B>B>BB>�B?B>�B?}B?�B?�B@B?HB?B@�B@�BAUBAUBA BA�BC�BCaBCaBCaBC�BC�BCaBCaBD3BE�BE�BFBFtBF�BF�BGEBGzBHKBHKBHKBHKBH�BH�BH�BH�BIRBIBJ�BJ�BJ#BK)BK^BK�BK�BK�BM�BOBBPHBP�BQ�BQ�BQ�BQ�BQ�BQ�BQ�BS&BS�BS�BT,BT�BT�BT�BT�BT�BU2BU2BUgBVBVmBVmBV�BW�BW�BWsBWsBWsBWsBWsBWsBW�BW�BWsBWsBW�BW�BW�BX�BYBYBYBZB[#B[#B[�B\)B\�B]�B]�B]�B^B^B^B^�B_B_�B`BB`BB`�B`vB`vB`�BaBaHBaHBa|Ba|BbBbNBbNBb�Bc Bd&Bd�Bd�Bd�Be,Bd�Bd�Bd�Bd�Bd�Bd�Bd�Bd�Be�Be�Be�Be�Bf2Bf2Be�Be�Be�Be�Be�Be`Be,Bd�Be`Be�Be�BffBffBffBf2Bf2Be�Bf2Bf2Be�Bf2BffBe�Be�Be�Be�Be�Be`Be,Be`Be,Bd�Bd�Bc�Bd&Bc�Bd�Be�Bd�Be,Be,Be,Be,BffBf�Bf�BgBgBg8Be�Bf2Be�Bf2Be�Be�Be�Be�Be�BffBf�Bg�Bg�Bh
Bh
Bh>Bh�Bh�Bh�Bh�BiyBiyBi�BkQBlWBkQBkQBlWBl"Bl�Bl�BlWBl"Bl"Bk�Bl"Bl�BlWBk�BkQBl"BlWBl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bl�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm�Bm]Bl�BkQBhsBffBf�Bf�BgBgBgBf�BgBgBgBgBgmBg�Bg�BgmBgmBgB��B�PB�B�rB��B��B��B��B��B�>B�8B��B�B��B��B�`B�B�2B�2B�fB��B�8B��B�8B�8B�rB��B�8B�8B�8B�8B��B��B�fB��B��B��B�2B�lB�lB��B�lB�2B�fB�2B��B��B��B�GB�B�8B��B�	B�fB�B�xB��B��B��B��B�fB��B�B�	B��B��B�B�lB��B��B�+B�`B��B��B��B��B��B��B�2B��B��B��B�ZB�ZB�B��B��B�`B��B��B�+B��B�ZB�%B�%B�%B��B�B�B��B�%B��B�ZB��B�%B��B��B��B�ZB�+B�ZB�TB�B�TB�ZB��B��B��B��B��B�ZB�B�B�MB�MB�MB�B�TB�ZB��B�TB��B�ZB�B��B�B��B�B��B�MB�B�B�B�B��B�AB��B�MB��B�B�MB�B�B�B�|B�B�GB�B�AB�B�B�B�B�B�B��B�B�B�GB�B�vB�AB�;B�B�oB��B�AB�cB�B�B�B��B�;B�iB� B��B�;B�B�oB�B��B�B�;B�iB�|B��B�5B�B�;B�B��B�5B�5B�]B�]B� B��B� B�5B� B�5B�oB��B��B�B��B�"B��B��B�5B��B�WB��B��B�]B�]B��B��B�B�cB�)B�)B�)B�)B��B�B�DB�B�DB�B�B��B�B�B��B��B��B�B��B�B�
B�`B�B��B��B�`B�B��B�NB�B�B�|B� B�ZB��B� B��B�B�TB�B�NB�`B�TB�NB�B�|B�HB�|B�BB�B�HB�B�B�;B�pBޞB�B�B�5B�B�B��B�;B�;B�B�pBޞB��B�;B�;B�B��BݘB�B�/BޞB��B�/B�/B��B�/B��B�/B�B�/B�5B��B�)B�)BܒB�)B��B��BیB��BچB�QB�BخBخB�B�EB�yB�sB��B�B��BخB�EB�EB��BخB�B�B�mBخB��B��B�2B��B�TB�,B�B�HB�BбB�}B��B��BΥB�jB�jB�^B�BƨB��B�KBÖB��B�'B�XB� B�B��B�UB�wB��B��B�	B�B��B�B�hB�FB��B��B��B�{B�MB}�B}VB�B|�B�GBt�B~(Bs�BqABqABo�BncBm]Bm)Bl�Bk�Bm)BjBbNB`�Bc�BP}BQ�Bm]B?�B7�B<jB8�B8�B49B-CB%�B �B"4B�BqB�BVB$�B	7B�B�B�B�2B�PB�8B�GB�%B� B�ZB�2B��B�B�?B�B�&BΥB�B��B��B��B�B�0B��B�OB��B��B��B}�B}"B��B�B|PBr�Bv�Bl�BdZBgmBffBbNBaBc�B`BBg�B^5BZ�BW�BYBR�BNpB_�BZQBP�BJXB:�B>�BB'BD�B6�B5�B6FB2�B4�B1�B-�B-wB-�B)�B'�B%�B#nB%FB!�B�BIB$tB)�BhB{B�B
	B�B��B��B��B�B�B�B�B�`B�B�B��B��BںB��B֡BҽBӏB�B�)BƨB��B�6B�XB�B��B�?B�hB�LB�B�IB�B��B��B�IB��B�1B��B��B�CB��B��B��B��B��B��B�B�B�B~�B��Bw�By	Bx�Br�Bu�Bu�Br�Bm)Bl�Br|Bm]Bj�BlWBj�Bf�Bi�Bd�Bf�BaHB^�B`BBc�Bg�BhsBXyB]�BZ�B`�BMjBQBR BR�BPBL�BW?BN�BR�BE9BIBG�B=B6�B2�B1�B2�B0!B*�B+B#�B%FB(�B(�B#BIB=B�BYBB_B �B�BBB��B��B�B�B��B��B��B�
B�B�B��B� B��B�B�5BیB��B�B�]B��B�/BܒB�KBچB�)B��B֡B��BԕB�2B�&B�,B�&B�9B�B�B�mB�NB�dB�B͟B�B�6B֡B� B�B�vB̘BΥBȀB�RB��B�B��B�B��B��B�[G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         202103261700362021032617003620210326170036202103261700362021032617003620210326170036SI  SI  ARFMARFM                                                                                                                                                2021020906185720210209061857IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021021905004520210219050045QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021021905004520210219050045QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021032510164520210325101645IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021032617005220210326170052IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0ARGO_for_DMQC Climatology Version 2020V03                       ARGO_for_DMQC Climatology Version 2020V03                       2021032617005220210326170052IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021032617005220210326170052IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                