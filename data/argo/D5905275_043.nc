CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2019-03-18T20:40:26Z creation; 2023-04-26T19:14:27Z DMQC;      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20190318204026  20230426191427  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               +   +AA  AOAO7316_008644_043                 7316_008644_043                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @دK���@دK���11  @دL��@دL��@+k�'j@@+k�'j@�c�ud0+�c�ud0+11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�\@@  @}p�@��R@�  @�  A ��A��A\)A,(�A@  A`  A�  A��A�\)A��A�\)A�  A�  A�  B (�B  B  B  B   B(  B/�
B7�
B@  BH  BP  BX  B`  Bh  Bo�
Bw�
B�B�  B�{B�{B�  B��B�  B�{B�(�B�  B�{B�(�B�  B�{B��\B�  B��B��B��B��B�  B��B��B�  B�  B�{B�{B�{B�{B�  B�  B�  C   C��C��C��C��C	��C��C  C  C��C��C  C��C��C  C
=C 
=C!��C#��C%��C(  C*  C,
=C-��C/��C1��C3��C5��C7�C9�C;��C>  C@
=CA��CC��CF  CH  CJ  CK��CM��CP  CR  CS�CU�CW�CY�C[��C^
=C`  Cb  Cc��Ce��Cg��Ci��Cl
=Cn
=Cp  Cr{Ct
=Cv
=Cx  Cz  C|  C}��C�C�
=C�C�C�
=C�  C��C���C�C�  C�  C���C���C���C���C���C�  C���C�  C�C�  C�C�
=C�C�  C���C���C���C�C�C�C�  C���C�  C�C�  C�  C�  C�C�C�  C���C�  C�C�C�C�C�  C�C���C���C�  C�C�  C���C�  C�C�
=C�C�C�C�C�C���C���C�C�C�C�C���C���C�  C���C���C�C�  C���C�  C�C�  C���C�  C�
=C�C���C�  C�C�C�C�  C���C�  C�C�  C�  C�  C���C���C�  C�  C���C�  C�
=C�C���C���C�  C�C���C���C���C�C�  C���C�C�C���C���C���C���C�  C���C���C���C���C���C���C���C���D � D�D��D  D}qD  D��D  D}qD�qD}qD  D}qD��D}qD  D��D	D	�D
  D
� D
��D� D�D� D��D� D�D�DD�DD� D�qD}qD  D��D  D� D�D��D  D� D  D��D  D� D  D}qD�qD}qD�qD� D  D}qD  D��D�D� D  D� D  D� D �D ��D!�D!� D"  D"� D#�D#��D$  D$��D%  D%}qD&  D&� D'  D'� D(�D(��D)�D)��D*  D*� D+  D+��D,  D,� D-  D-� D.�D.� D/  D/� D/�qD0� D1�D1� D2  D2� D3�D3� D3�qD4��D5�D5� D6�D6��D7  D7� D8  D8� D9  D9�D:�D:� D;  D;� D<  D<��D=�D=��D>�D>�D?D?��D?�qD@� DA  DA� DB�DB��DCDC� DD  DD� DE�DE� DF�DF��DG  DG� DH  DH}qDI  DI� DI�qDJ}qDK  DK}qDL  DL� DM�DM��DN  DN}qDO  DO��DP  DP� DQ�DQ}qDQ�qDR��DS�DS� DT  DT� DU  DU� DV  DV� DW�DW��DX  DX��DY  DY}qDY�qDZ}qD[  D[��D\D\��D]  D]}qD]�qD^� D_  D_� D`  D`��DaDa��Da�qDb� Dc  Dc� Dd�Dd�De  De� Df�Df��Dg  Dgz�Dg�qDh� Di�Di� Di�qDj� Dk�Dk��Dl�Dl� Dl�qDm� Dn  Dn}qDn�qDo}qDo�qDp� DqDq� Dr  Dr��DsDs� Dt  Dt}qDu  Du��Du�qDv� Dw�Dw� Dx  Dx��Dy�Dy��Dz�Dz� D{�D{�D|D|� D|��D}z�D~  D~��D  D� D�  D�@ D��HD��HD�  D�AHD��HD���D���D�=qD�}qD�� D�HD�@ D�� D�� D���D�>�D��HD�� D�  D�AHD��HD�D�HD�@ D�~�D���D�  D�AHD�� D�� D�  D�@ D�� D�� D���D�>�D�� D�� D�HD�B�D��HD�� D�  D�AHD�� D���D�HD�AHD�~�D��qD���D�@ D���D��HD���D�>�D�� D��HD���D�>�D�~�D�� D�  D�>�D�� D�� D���D�<)D�~�D�� D���D�=qD�~�D���D���D�@ D�~�D��qD���D�@ D��HD��HD�  D�@ D��HD�� D���D�AHD��HD��qD�  D�AHD�� D�� D���D�=qD�~�D��HD�  D�>�D�� D�� D�  D�>�D�~�D���D�  D�AHD��HD��HD��D�@ D�}qD��qD�  D�B�D�� D�� D��D�@ D�~�D���D�  D�B�D��HD��HD�HD�@ D�� D���D�HD�AHD�~�D���D�HD�AHD�� D�� D�HD�AHD���D�� D���D�@ D�� D��HD��D�AHD�~�D��qD���D�@ D�~�D��qD���D�AHD�� D�� D���D�AHD��HD��qD�  D�AHD��HD�� D�  D�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD���D�D��D�@ D�~�D�� D���D�=qD�� D��HD��D�AHD�� D�D�HD�@ D��HD���D���D�@ D�� D���D���D�>�D�~�D���D���D�=qD�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��HD�HD�AHD��HD�D�  D�@ D��HD�� D���D�@ D�� D��HD�HD�>�D��HD��HD�  D�AHD���D�� D��qD�>�D�� D���D�  D�AHD�~�D��qD���D�@ D�~�D�� D�HD�@ D�� D��HD�HD�B�D�� D��HD��D�@ D��HD�D�  D�@ D D½qD���D�>�DÁHD�D�  D�>�D�~�Dľ�D�  D�AHD�~�D��HD��D�AHDƀ DƽqD�  D�AHD�~�D�� D�  D�@ DȁHD�D�  D�@ DɁHD��HD��D�AHD�~�D�� D�HD�@ DˁHD�� D���D�AHD�~�D��HD��D�@ D̀ D��HD�  D�>�D΀ D��HD�HD�@ Dπ D��HD�HD�>�D�~�D�� D�HD�AHDсHD��HD�  D�>�D�~�D��HD�  D�>�D�~�D�� D�  D�@ DԀ D��HD�HD�@ DՀ D�� D�  D�AHDր D־�D�HD�AHD׀ D�� D�HD�@ D؁HD��HD���D�=qD�~�D�� D���D�@ Dڂ�D�D�HD�@ DہHD��HD�  D�@ D܀ D��HD�HD�@ D݀ D��HD�  D�>�D�~�D�� D�HD�AHD߁HD��HD�  D�@ D�� D��HD�  D�>�D� DᾸD���D�@ D�HD��HD�HD�AHD�~�D�� D�HD�@ D�HD��HD�  D�@ D�~�D�� D�  D�@ D�HD澸D�  D�@ D�HD��HD�HD�@ D� D��HD�HD�AHD� D�� D�  D�@ D�HD��HD���D�@ D�HD�D�  D�>�D� D�� D���D�@ D�HD�� D���D�=qD�}qD�� D�HD�AHD� D�� D�  D�@ D�� D�� D�  D�AHD�HD�qD���D�@ D� D�� D�  D�@ D�HD��HD�HD�@ D�~�D��HD�  D�@ D��HD�� D���D�AHD���D�� D�  D�@ D�}qD�� D�HD�@ D�}qD�� D�HD�@ D�� D��HD��RD�@ >�?\)?aG�?���?�p�?�ff@
=q@��@.{@E�@Y��@k�@�G�@��@�z�@�p�@�=q@�33@�(�@Ǯ@��@ٙ�@��
@�\)@�Q�A ��AffA
�HA\)A�
A��Ap�A!G�A&ffA+�A.�RA3�
A8��A<��A@��AFffAJ=qAN{AR�\AW�A[�A`  Adz�Ai��Amp�AqG�AvffAz=qA~{A���A�33A���A�ffA���A��\A�z�A�{A�  A��A��
A�p�A�\)A���A��A�p�A�  A��\A�z�A�ffA���A��
A�{A�Q�A��\A��A�
=A���A��A�{A��A��A���A�\)A�G�A˅A�ffAУ�A��HA��A�\)A�=qA�z�A�ffA���A��
A�A�Q�A��HA�p�A�A�A�z�A�
=A�G�A�33A�{B Q�BG�BffB�B�B=qB33Bz�B	B
=B  BG�B�\B�
B��B�B\)B��BB�RB  Bp�B�\B�B��B=qB\)B z�B!B#
=B$Q�B%p�B&�\B(  B)G�B*ffB+�B,��B.=qB/33B0Q�B1��B333B4Q�B5G�B6�\B8  B9G�B:ffB;�B<��B>=qB?\)B@z�BABC33BDz�BE��BF�RBH  BIp�BJ�\BK�BM�BNffBO�
BP��BR{BS�BT��BV{BW\)BXz�BYB[33B\z�B]B^�HB`(�BaBc
=Bd(�Be��Bg
=Bhz�Bi��Bj�RBl(�Bm��Bo
=Bp(�Bqp�Br�HBtQ�BuBv�HBx  Byp�Bz�HB|Q�B}p�B~�RB�
B��RB�p�B�  B��\B�G�B�  B��RB�G�B��B��\B�G�B��B��\B�G�B��B��RB�\)B�{B��RB��B�Q�B��B�B�z�B�33B�{B��HB��B�Q�B�
=B�B�z�B�\)B�{B���B��B�(�B���B�B��\B�33B��B���B�p�B�=qB��HB��B�=qB�
=B�B�ffB�
=B��B�ffB�33B��B���B�G�B��B���B�\)B�(�B��HB���B�=qB��HB���B�Q�B�
=B��
B�z�B�
=B�B�Q�B���B���B�=qB��RB�G�B���B�  B�Q�B��\B��HB��B�\)B��B��B�B��
B��B�{B�=qB�z�B��\B��RB��HB���B�
=B�33B�\)B��B��B��
B�  B�(�B�Q�B�z�B��\B���B���B���B��B�G�B��B���B��B�B��B�{B�=qB�ffB��\B��RB���B���B��B�\)B��B�B�  B�=qB�ffB���B��HB��B�p�B�B�  B�Q�B£�B�
=B�G�BÅB�B�{B�ffBĸRB��B�p�B�B�  B�Q�BƏ\B��HB�33BǅB��
B�=qBȣ�B���B�G�BɅB��
B�(�B�z�B��HB�33B˙�B�  B�Q�B���B��BͅB��
B�=qBΏ\B���B�\)BϮB�(�B�z�B��HB�G�B�B�=qBң�B�
=B�p�B��B�Q�BԸRB��BՅB�  B�ffB���B�33B׮B�{B�z�B��HB�G�B�B�(�Bڣ�B�
=BۅB��B�Q�BܸRB��B݅B��B�Q�B޸RB��B߅B��B�Q�B�RB�
=B�B��B�Q�B���B�G�B�B�(�B�\B�
=B�p�B��B�Q�B���B�33B�B�(�B�\B�
=B�p�B��B�ffB��HB�G�B�B�(�B��B�
=B�B�  B�ffB���B�\)B�B�=qB�RB�33B�B�{B�z�B���B�\)B��
B�Q�B��RB�33B���B�{B��\B���B�p�B��B�Q�B���B�G�B��B�(�B��\B�
=B��B�  B�ffB��HB�\)B��
B�=qB��RB�33B��C 
=C Q�C �\C ��C
=C=qCz�CC  C=qCz�C�RC��C33Cp�C�C�C(�Cp�C�C�C(�CffC��C�HC�CffC��C�HC�CffC��C�HC�C\)C��C�HC	�C	\)C	��C	�HC
�C
ffC
�C
�C33Cp�C�RC  C=qC�C��C
=CQ�C�\C�
C�C\)C��C��CG�C�\C�
C�CffC�C�C33Cz�CC
=CG�C�\C�
C�C\)C�C�C=qC�C��C{C\)C��C��C=qC�\C�
C�Cp�C�C�CG�C�C��C{C\)C��C�C33Cz�C��C{C\)C��C�C33Cz�C�RC
=CQ�C�\C�
C�CffC�RC  CG�C�\C�HC 33C z�C ��C!{C!\)C!��C!�C"(�C"p�C"�RC#  C#G�C#��C#�HC$(�C$p�C$C%
=C%G�C%��C%�
C&�C&\)C&��C&�C'33C'p�C'C(
=C(Q�C(��C(�HC)33C)p�C)C*
=C*\)C*��C*�HC+�C+ffC+�C+�C,=qC,z�C,��C-
=C-Q�C-��C-�C.33C.�C.C/
=C/G�C/�\C/��C0{C0\)C0��C0�C133C1�C1C2
=C2Q�C2�\C2�
C3�C3ffC3�C3��C4=qC4�C4C5
=C5G�C5�C5��C6{C6\)C6�C6��C733C7z�C7C8  C8G�C8�C8��C9{C9Q�C9��C9�C:33C:z�C:C;{C;Q�C;��C;�HC<(�C<ffC<�RC=  C=Q�C=��C=�C>33C>z�C>�RC?  C?G�C?�\C?�
C@(�C@z�C@CA
=CAQ�CA��CA�
CB�CBffCB�RCC
=CCQ�CC��CC�CD(�CDp�CD�RCE
=CE\)CE��CE�CF33CF�CFCG
=CG\)CG�CH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                  ?��@�\@@  @}p�@��R@�  @�  A ��A��A\)A,(�A@  A`  A�  A��A�\)A��A�\)A�  A�  A�  B (�B  B  B  B   B(  B/�
B7�
B@  BH  BP  BX  B`  Bh  Bo�
Bw�
B�B�  B�{B�{B�  B��B�  B�{B�(�B�  B�{B�(�B�  B�{B��\B�  B��B��B��B��B�  B��B��B�  B�  B�{B�{B�{B�{B�  B�  B�  C   C��C��C��C��C	��C��C  C  C��C��C  C��C��C  C
=C 
=C!��C#��C%��C(  C*  C,
=C-��C/��C1��C3��C5��C7�C9�C;��C>  C@
=CA��CC��CF  CH  CJ  CK��CM��CP  CR  CS�CU�CW�CY�C[��C^
=C`  Cb  Cc��Ce��Cg��Ci��Cl
=Cn
=Cp  Cr{Ct
=Cv
=Cx  Cz  C|  C}��C�C�
=C�C�C�
=C�  C��C���C�C�  C�  C���C���C���C���C���C�  C���C�  C�C�  C�C�
=C�C�  C���C���C���C�C�C�C�  C���C�  C�C�  C�  C�  C�C�C�  C���C�  C�C�C�C�C�  C�C���C���C�  C�C�  C���C�  C�C�
=C�C�C�C�C�C���C���C�C�C�C�C���C���C�  C���C���C�C�  C���C�  C�C�  C���C�  C�
=C�C���C�  C�C�C�C�  C���C�  C�C�  C�  C�  C���C���C�  C�  C���C�  C�
=C�C���C���C�  C�C���C���C���C�C�  C���C�C�C���C���C���C���C�  C���C���C���C���C���C���C���C���D � D�D��D  D}qD  D��D  D}qD�qD}qD  D}qD��D}qD  D��D	D	�D
  D
� D
��D� D�D� D��D� D�D�DD�DD� D�qD}qD  D��D  D� D�D��D  D� D  D��D  D� D  D}qD�qD}qD�qD� D  D}qD  D��D�D� D  D� D  D� D �D ��D!�D!� D"  D"� D#�D#��D$  D$��D%  D%}qD&  D&� D'  D'� D(�D(��D)�D)��D*  D*� D+  D+��D,  D,� D-  D-� D.�D.� D/  D/� D/�qD0� D1�D1� D2  D2� D3�D3� D3�qD4��D5�D5� D6�D6��D7  D7� D8  D8� D9  D9�D:�D:� D;  D;� D<  D<��D=�D=��D>�D>�D?D?��D?�qD@� DA  DA� DB�DB��DCDC� DD  DD� DE�DE� DF�DF��DG  DG� DH  DH}qDI  DI� DI�qDJ}qDK  DK}qDL  DL� DM�DM��DN  DN}qDO  DO��DP  DP� DQ�DQ}qDQ�qDR��DS�DS� DT  DT� DU  DU� DV  DV� DW�DW��DX  DX��DY  DY}qDY�qDZ}qD[  D[��D\D\��D]  D]}qD]�qD^� D_  D_� D`  D`��DaDa��Da�qDb� Dc  Dc� Dd�Dd�De  De� Df�Df��Dg  Dgz�Dg�qDh� Di�Di� Di�qDj� Dk�Dk��Dl�Dl� Dl�qDm� Dn  Dn}qDn�qDo}qDo�qDp� DqDq� Dr  Dr��DsDs� Dt  Dt}qDu  Du��Du�qDv� Dw�Dw� Dx  Dx��Dy�Dy��Dz�Dz� D{�D{�D|D|� D|��D}z�D~  D~��D  D� D�  D�@ D��HD��HD�  D�AHD��HD���D���D�=qD�}qD�� D�HD�@ D�� D�� D���D�>�D��HD�� D�  D�AHD��HD�D�HD�@ D�~�D���D�  D�AHD�� D�� D�  D�@ D�� D�� D���D�>�D�� D�� D�HD�B�D��HD�� D�  D�AHD�� D���D�HD�AHD�~�D��qD���D�@ D���D��HD���D�>�D�� D��HD���D�>�D�~�D�� D�  D�>�D�� D�� D���D�<)D�~�D�� D���D�=qD�~�D���D���D�@ D�~�D��qD���D�@ D��HD��HD�  D�@ D��HD�� D���D�AHD��HD��qD�  D�AHD�� D�� D���D�=qD�~�D��HD�  D�>�D�� D�� D�  D�>�D�~�D���D�  D�AHD��HD��HD��D�@ D�}qD��qD�  D�B�D�� D�� D��D�@ D�~�D���D�  D�B�D��HD��HD�HD�@ D�� D���D�HD�AHD�~�D���D�HD�AHD�� D�� D�HD�AHD���D�� D���D�@ D�� D��HD��D�AHD�~�D��qD���D�@ D�~�D��qD���D�AHD�� D�� D���D�AHD��HD��qD�  D�AHD��HD�� D�  D�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD���D�D��D�@ D�~�D�� D���D�=qD�� D��HD��D�AHD�� D�D�HD�@ D��HD���D���D�@ D�� D���D���D�>�D�~�D���D���D�=qD�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��HD�HD�AHD��HD�D�  D�@ D��HD�� D���D�@ D�� D��HD�HD�>�D��HD��HD�  D�AHD���D�� D��qD�>�D�� D���D�  D�AHD�~�D��qD���D�@ D�~�D�� D�HD�@ D�� D��HD�HD�B�D�� D��HD��D�@ D��HD�D�  D�@ D D½qD���D�>�DÁHD�D�  D�>�D�~�Dľ�D�  D�AHD�~�D��HD��D�AHDƀ DƽqD�  D�AHD�~�D�� D�  D�@ DȁHD�D�  D�@ DɁHD��HD��D�AHD�~�D�� D�HD�@ DˁHD�� D���D�AHD�~�D��HD��D�@ D̀ D��HD�  D�>�D΀ D��HD�HD�@ Dπ D��HD�HD�>�D�~�D�� D�HD�AHDсHD��HD�  D�>�D�~�D��HD�  D�>�D�~�D�� D�  D�@ DԀ D��HD�HD�@ DՀ D�� D�  D�AHDր D־�D�HD�AHD׀ D�� D�HD�@ D؁HD��HD���D�=qD�~�D�� D���D�@ Dڂ�D�D�HD�@ DہHD��HD�  D�@ D܀ D��HD�HD�@ D݀ D��HD�  D�>�D�~�D�� D�HD�AHD߁HD��HD�  D�@ D�� D��HD�  D�>�D� DᾸD���D�@ D�HD��HD�HD�AHD�~�D�� D�HD�@ D�HD��HD�  D�@ D�~�D�� D�  D�@ D�HD澸D�  D�@ D�HD��HD�HD�@ D� D��HD�HD�AHD� D�� D�  D�@ D�HD��HD���D�@ D�HD�D�  D�>�D� D�� D���D�@ D�HD�� D���D�=qD�}qD�� D�HD�AHD� D�� D�  D�@ D�� D�� D�  D�AHD�HD�qD���D�@ D� D�� D�  D�@ D�HD��HD�HD�@ D�~�D��HD�  D�@ D��HD�� D���D�AHD���D�� D�  D�@ D�}qD�� D�HD�@ D�}qD�� D�HD�@ D�� D��HD��RG�O�>�?\)?aG�?���?�p�?�ff@
=q@��@.{@E�@Y��@k�@�G�@��@�z�@�p�@�=q@�33@�(�@Ǯ@��@ٙ�@��
@�\)@�Q�A ��AffA
�HA\)A�
A��Ap�A!G�A&ffA+�A.�RA3�
A8��A<��A@��AFffAJ=qAN{AR�\AW�A[�A`  Adz�Ai��Amp�AqG�AvffAz=qA~{A���A�33A���A�ffA���A��\A�z�A�{A�  A��A��
A�p�A�\)A���A��A�p�A�  A��\A�z�A�ffA���A��
A�{A�Q�A��\A��A�
=A���A��A�{A��A��A���A�\)A�G�A˅A�ffAУ�A��HA��A�\)A�=qA�z�A�ffA���A��
A�A�Q�A��HA�p�A�A�A�z�A�
=A�G�A�33A�{B Q�BG�BffB�B�B=qB33Bz�B	B
=B  BG�B�\B�
B��B�B\)B��BB�RB  Bp�B�\B�B��B=qB\)B z�B!B#
=B$Q�B%p�B&�\B(  B)G�B*ffB+�B,��B.=qB/33B0Q�B1��B333B4Q�B5G�B6�\B8  B9G�B:ffB;�B<��B>=qB?\)B@z�BABC33BDz�BE��BF�RBH  BIp�BJ�\BK�BM�BNffBO�
BP��BR{BS�BT��BV{BW\)BXz�BYB[33B\z�B]B^�HB`(�BaBc
=Bd(�Be��Bg
=Bhz�Bi��Bj�RBl(�Bm��Bo
=Bp(�Bqp�Br�HBtQ�BuBv�HBx  Byp�Bz�HB|Q�B}p�B~�RB�
B��RB�p�B�  B��\B�G�B�  B��RB�G�B��B��\B�G�B��B��\B�G�B��B��RB�\)B�{B��RB��B�Q�B��B�B�z�B�33B�{B��HB��B�Q�B�
=B�B�z�B�\)B�{B���B��B�(�B���B�B��\B�33B��B���B�p�B�=qB��HB��B�=qB�
=B�B�ffB�
=B��B�ffB�33B��B���B�G�B��B���B�\)B�(�B��HB���B�=qB��HB���B�Q�B�
=B��
B�z�B�
=B�B�Q�B���B���B�=qB��RB�G�B���B�  B�Q�B��\B��HB��B�\)B��B��B�B��
B��B�{B�=qB�z�B��\B��RB��HB���B�
=B�33B�\)B��B��B��
B�  B�(�B�Q�B�z�B��\B���B���B���B��B�G�B��B���B��B�B��B�{B�=qB�ffB��\B��RB���B���B��B�\)B��B�B�  B�=qB�ffB���B��HB��B�p�B�B�  B�Q�B£�B�
=B�G�BÅB�B�{B�ffBĸRB��B�p�B�B�  B�Q�BƏ\B��HB�33BǅB��
B�=qBȣ�B���B�G�BɅB��
B�(�B�z�B��HB�33B˙�B�  B�Q�B���B��BͅB��
B�=qBΏ\B���B�\)BϮB�(�B�z�B��HB�G�B�B�=qBң�B�
=B�p�B��B�Q�BԸRB��BՅB�  B�ffB���B�33B׮B�{B�z�B��HB�G�B�B�(�Bڣ�B�
=BۅB��B�Q�BܸRB��B݅B��B�Q�B޸RB��B߅B��B�Q�B�RB�
=B�B��B�Q�B���B�G�B�B�(�B�\B�
=B�p�B��B�Q�B���B�33B�B�(�B�\B�
=B�p�B��B�ffB��HB�G�B�B�(�B��B�
=B�B�  B�ffB���B�\)B�B�=qB�RB�33B�B�{B�z�B���B�\)B��
B�Q�B��RB�33B���B�{B��\B���B�p�B��B�Q�B���B�G�B��B�(�B��\B�
=B��B�  B�ffB��HB�\)B��
B�=qB��RB�33B��C 
=C Q�C �\C ��C
=C=qCz�CC  C=qCz�C�RC��C33Cp�C�C�C(�Cp�C�C�C(�CffC��C�HC�CffC��C�HC�CffC��C�HC�C\)C��C�HC	�C	\)C	��C	�HC
�C
ffC
�C
�C33Cp�C�RC  C=qC�C��C
=CQ�C�\C�
C�C\)C��C��CG�C�\C�
C�CffC�C�C33Cz�CC
=CG�C�\C�
C�C\)C�C�C=qC�C��C{C\)C��C��C=qC�\C�
C�Cp�C�C�CG�C�C��C{C\)C��C�C33Cz�C��C{C\)C��C�C33Cz�C�RC
=CQ�C�\C�
C�CffC�RC  CG�C�\C�HC 33C z�C ��C!{C!\)C!��C!�C"(�C"p�C"�RC#  C#G�C#��C#�HC$(�C$p�C$C%
=C%G�C%��C%�
C&�C&\)C&��C&�C'33C'p�C'C(
=C(Q�C(��C(�HC)33C)p�C)C*
=C*\)C*��C*�HC+�C+ffC+�C+�C,=qC,z�C,��C-
=C-Q�C-��C-�C.33C.�C.C/
=C/G�C/�\C/��C0{C0\)C0��C0�C133C1�C1C2
=C2Q�C2�\C2�
C3�C3ffC3�C3��C4=qC4�C4C5
=C5G�C5�C5��C6{C6\)C6�C6��C733C7z�C7C8  C8G�C8�C8��C9{C9Q�C9��C9�C:33C:z�C:C;{C;Q�C;��C;�HC<(�C<ffC<�RC=  C=Q�C=��C=�C>33C>z�C>�RC?  C?G�C?�\C?�
C@(�C@z�C@CA
=CAQ�CA��CA�
CB�CBffCB�RCC
=CCQ�CC��CC�CD(�CDp�CD�RCE
=CE\)CE��CE�CF33CF�CFCG
=CG\)CG�CH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                  @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�p�A�p�A�p�A�r�A�t�A�t�A�t�A�t�A�r�A�n�A�p�A�r�A�p�A�C�A�;dA�E�A�33A�(�A�bA���A���A���A���A�ȴA�ƨA�ƨA�ƨA�ƨA�ƨA�ĜA�ƨA�ƨA�ƨA�A���A;wA;wAͺ^AͲ-A͝�A��A�t�AʼjA� �AɼjA�"�A�hsA�A�33A���A��DA��A���A��A��yA���A�`BA���A�A���A�/A�$�A�VA��A�33A��A���A���A�=qA�ffA���A���A��A��Ax��At=qAp�Akl�Ad(�A_��A\�`AY�AX�AU�mAS�AP�DANr�AL�AKx�AJ1AH�+AF{AD��AD=qAC�AC7LAB�`A=��A:n�A89XA5?}A3��A3;dA3�A2bNA-�mA,9XA+x�A*��A(��A'�A&��A&�A%dZA%S�A%p�A%�A&�DA&�+A'��A(ĜA(��A(r�A'��A'�wA'ƨA'��A&�A&��A&�9A&�\A&jA&A%t�A%A$�\A$VA#�
A"ĜA" �A!��A!S�A!"�A z�A��AK�A�A��AVA�-A"�A�HA5?A  A��A��A/A�/A��AC�A��A��AE�A��A��Ar�AJA��A&�A�A9XA�#A�7AS�A�A�A�Ap�A+A��AbNA$�AJA�mA�wA��AS�A�A=qA5?A5?A�A�-A�hA`BAoA�An�AVA5?AA�A
��A
5?A	�;A	��A	hsA	�A��A�`A�AM�A�A��A��AO�A�A�A�A��AM�A�A��A��A�A7LA��A~�A{A��A��A?}AȴA�A�DAn�AZA5?AA;dA �`A ��A �\A M�A @�\)@��H@��@�`B@��@�o@���@�V@��@�x�@�?}@���@��@��m@�;d@���@��@�/@�z�@�dZ@���@���@�\@��@�^@��@�@�V@�z�@��@�
=@�!@�+@�@�Ĝ@�1'@�|�@�o@ꗍ@�$�@��@�X@�/@��@�j@��;@��y@�~�@��@噚@�7L@���@�j@��m@�^5@�h@�hs@���@�b@�|�@ާ�@�=q@�@ݲ-@���@ܴ9@�I�@۝�@�o@���@�^5@���@�x�@�%@�r�@ׅ@��y@�n�@�^5@�=q@���@�p�@�/@��`@ԋD@���@ӥ�@�33@ҸR@�n�@�^5@�=q@�@с@�%@Ϯ@��@���@ΰ!@�V@ͺ^@���@�z�@�1@˥�@�
=@�v�@���@��@���@��@ǝ�@�"�@��@�=q@��@ź^@�p�@�X@�7L@���@Ĭ@þw@��@��H@�@�^5@�-@��#@�7L@��j@�A�@�  @��;@�33@��R@���@�/@��@���@�1'@�;d@��@���@�V@�$�@���@��T@��^@�G�@��u@�Q�@��@���@�S�@���@�^5@���@���@��@��@�Z@� �@��;@���@�l�@��@�ff@�M�@�=q@�J@��@��h@�%@��9@�bN@�9X@�ƨ@��@���@���@�t�@�dZ@�C�@��y@�v�@�-@�{@��-@�V@���@�Q�@� �@�1@��@�@��H@�ȴ@�n�@��-@�O�@��/@�I�@��m@��@��@��@��T@��@�G�@�%@��j@���@�z�@�1'@��w@��@�l�@�\)@�ȴ@�{@��T@�@��h@�7L@�V@��@��@���@�"�@���@��@�{@���@�/@���@�I�@�  @�t�@��@��@���@��\@��+@�v�@�V@��^@�7L@���@��@���@��m@��;@��F@��@��y@��R@��!@��\@�V@�J@��#@���@��7@�%@��j@��@� �@���@�l�@�"�@���@���@�v�@�@��^@�x�@�&�@���@��`@��9@�A�@��;@���@�33@�@��!@�^5@�=q@�{@��@���@�G�@�V@���@�Ĝ@��D@�bN@�1'@�  @���@��w@��@�l�@�"�@�@��@��@���@�=q@���@�?}@���@�A�@� �@��@�b@�  @��F@��P@�K�@��@���@���@�~�@�$�@���@���@���@��@�&�@���@��/@���@�Ĝ@���@�z�@�Z@�b@�ƨ@�|�@�C�@���@�~�@�v�@�V@���@���@��-@�X@��@�r�@�j@�Q�@�(�@�;@~�@~ff@}@|�@|Z@{��@{C�@z��@z�\@z^5@z-@y��@y��@y�7@y7L@xĜ@x�@xQ�@wl�@v�+@vff@v{@u@up�@t��@t��@t�@sƨ@s��@sdZ@so@r�!@rn�@q�@q�^@q�^@q�7@qX@qG�@q�@p  @o�P@n�@m�T@m��@m�h@mO�@m/@mV@l�D@k��@kC�@k"�@k@jM�@i��@ihs@i&�@h�u@g��@g��@gl�@g�@f�@fv�@f5?@f{@e��@e�@e�@d�D@c�@co@b��@b��@b=q@a�^@`�`@`r�@`  @_�w@_�P@^ȴ@^�R@^�R@^��@^V@]�-@]�@]p�@]O�@]/@\��@\9X@[ƨ@Z�@ZM�@ZJ@Y�7@Y7L@Y�@X��@X �@W+@V�R@VV@V$�@V$�@V$�@U�@U��@TZ@T(�@T�@T1@S�@R��@Rn�@R�@QG�@P  @O�@OK�@O
=@N�y@N�R@Nff@NE�@M�@M@M�-@M�h@M?}@L�@Lj@L(�@K�
@K��@K"�@J�H@J^5@I�#@Ix�@I%@H��@Hb@G|�@G�@F�@Fȴ@FE�@F$�@F{@E@E/@D�D@C��@C��@C"�@B��@B�\@BM�@B-@B-@A��@A�^@A%@?�P@>�@>�R@>�+@>ff@>5?@=��@<��@<��@<z�@<1@;��@;"�@:�\@:n�@9��@9�7@9�7@9�7@9�7@9x�@9G�@9%@8Ĝ@8�9@8��@8�@8bN@8Q�@8 �@7��@7�@7l�@7K�@7+@6��@6�R@65?@5�h@5/@4��@4z�@4�@3�m@3��@3dZ@333@3o@2�H@2��@2M�@1�@1��@1��@1hs@17L@0��@01'@/��@/l�@/K�@.ȴ@.��@.ff@-��@,�@,�/@,��@,��@,��@,�@,1@,�@,(�@,(�@,1@+�F@+t�@+S�@+C�@+33@+"�@*�@*��@*�\@*-@)�@)��@)X@)�@)�@)�@)�@)%@(��@(�@(A�@'��@'��@'�P@'|�@';d@&�y@&v�@&ff@&5?@%�@%��@%��@%�@%O�@%/@$�j@$z�@$Z@$1@#��@#�@#t�@#C�@#@"�@"�H@"�!@"n�@"M�@"=q@"�@!��@!��@!�7@!X@!�@ ��@ �@ Q�@  �@   @��@\)@;d@�@
=@�y@�@�@�@��@E�@{@�T@�h@/@�/@j@Z@(�@��@dZ@dZ@t�@t�@S�@o@��@�!@�\@=q@��@��@��@X@&�@�@��@��@��@Q�@ �@�@�@|�@K�@
=@�@ȴ@ff@$�@��@p�@p�@?}@V@��@�j@�@z�@Z@9X@(�@1@��@�m@�
@��@S�@S�@dZ@C�@o@o@�H@��@~�@~�@~�@^5@J@��@��@X@%@Ĝ@��@�u@�@Q�@b@��@�@��@�P@|�@;d@�@v�@ff@V@E�A�l�A�r�A�n�A�l�A�r�A�r�A�n�A�n�A�r�A�r�A�n�A�p�A�t�A�r�A�p�A�t�A�v�A�p�A�t�A�v�A�r�A�t�A�v�A�v�A�r�A�t�A�v�A�r�A�p�A�r�A�v�A�ffA�p�A�n�A�p�A�p�A�p�A�t�A�n�A�r�A�x�A�p�A�p�A�v�A�t�A�n�A�n�A�p�A�p�A�p�A�S�A�G�A�K�A�C�A�G�A�E�A�1'A�-A�1'A�;dA�5?A�5?A�?}A�=qA�;dA�;dA�O�A�I�A�;dA�C�A�G�A�K�A�K�A�E�A�I�A�C�A�7LA�+A�&�A�/A�(�A�&�A�/A�/A� �A�&�A�(�A�(�A�bA�VA�VA�A�"�A� �A��yA���A��HA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ƨA�ȴA���A�ȴA�ƨA�ȴA���A�ȴA�ĜA�ĜA�ȴA�ȴA�ĜA�ĜA�ȴA�ȴA�ĜA�A�ƨA�ȴA�ĜA�ĜA�ƨA�ȴA�ĜA�A�ĜA�ȴA�ƨA�A�ĜA�ȴA�ȴA�ĜA�ĜA�ȴA�ȴA�ĜA�A�ĜA�ȴA�ĜA�ĜA�A�ƨA�ȴA�ƨA�ĜA�ĜA�ƨA�ȴA�ĜA�ĜA�ȴA�ȴA�ĜA�A�ƨA�ȴA�ȴA�ƨA�ĜA�ĜA�ƨA�ȴA�ĜA�A�A�A�A;wA���A�ĜA�A���A;wA;wA���A���A;wAͼjA;wA���A���A���Aͺ^AͼjA;wA���A;wAͺ^Aͺ^Aͺ^Aͺ^AͶFAͲ-Aʹ9A͸RAʹ9AͲ-AͮAͧ�Aͩ�Aͧ�A͛�A͙�A͗�A͏\AͅA�n�A�XA���A�Q�A�%A�ƨA�x�A�7LA�JA�A��TA���AʸRAʝ�A�A�A��A�oA��A�1'A�/A��A�VA���Aɡ�A�r�A�ZA�=qA�5?A��A��A�%A���A��AȓuA�5?A�JA���A�ffA�(�A�oA�
=AƸRA�E�A�dZA�"�Aě�A�G�A�A�C�A�  A�ƨA��A�33A��A�~�A��;A��A��+A�K�A�ȴA�bNA�%A�A��uA�ZA�"�A�%A��`A���A��9A��DA��hA�z�A�v�A�p�A�l�A�dZA�O�A�7LA�oA��A��^A���A��A�`BA�;dA�%A���A���A��A�XA�33A�A���A��A��mA��/A���A��9A�t�A�ZA�G�A�E�A�=qA�$�A��A�
=A�%A�A�A���A���A��mA��#A��wA�bNA�%A���A�?}A��\A��jA�I�A���A��hA��A�bA��FA�5?A��
A���A��7A�A�A���A�  A���A��DA�|�A�r�A�ffA�K�A�-A�{A��A��wA���A�hsA�C�A�VA���A�XA�-A��A�ĜA�~�A�+A���A���A��
A�7LA��A�5?A��A�Q�A�ĜA���A�"�A��A��-A��A�+A�n�A��A���A�{A�ĜA��9A��\A�dZA�$�A���A��\A�x�A�9XA�~�A�oA��#A�ȴA��-A��+A�n�A�bNA�^5A�I�A��A�VA�%A��TA�v�A�VA���A�A�A���A�C�A��A��A�ĜA�t�A�bA�S�A���A��-A���A��A�VA�A�9XA�A���A�n�A�(�A��A���A��RA���A���A��A�Q�A�
=A��A�33A��A��jA���A�XA�&�A�A��;A���A��FA���A��PA�n�A�K�A� �A�{A�
=A��/A��+A�t�A�`BA�G�A�+A�A��A�A��+A�(�A��A�/A�{A�A���A��A��yA��;A��!A�jA���A�C�A���A��PA�9XA��RA��PA�p�A�"�A�A�t�A�A�ffA��A��!A��A�?}A�33A�"�A��A��A���A��`A��A��A�"�A�O�A���A��A�  A��wA��9A��A���A���A��hA��7A�z�A�hsA�^5A�9XA`BA}VAz�`Az �Ay��Ay"�Ax~�AxJAw��Aw��Aw`BAwoAu�Atz�As�#Ar��Ar��Arr�Ar-Aq�wAq\)Aq�Ap��Ap^5AoƨAol�Ao+An��An  AkXAh�DAgVAfz�Af�Ae�-Ad�HAc�Ac33AbjAa�FAa&�A`��A`v�A`  A_�A_�A^�RA^=qA]�7A]K�A]33A\��A\-A[��A[/AZ��AZ �AY��AYhsAYK�AY33AY�AY
=AX��AX��AX�yAXĜAX^5AWp�AV�HAVZAU�AU�PAU+AT�jAT�ATQ�AS�TAS��AR�yAQ��AQƨAQ��AQO�AQ
=AP��APZAP{AO�
AOdZAOVAN�9ANr�AN9XAN  AM��AM�AMK�AL��ALv�AL^5AL=qAL(�ALbAK�AK�#AK�AKl�AK�AJ�AJ��AJ��AJVAJ{AI�mAIAI��AIx�AIC�AIAH�HAH�RAHz�AHM�AG33AFȴAF��AFjAF1'AE�AE��AE\)AE/AE�AD��AD��AD�AD��AD~�ADjADZADE�ADA�AD9XAD1'AD �ADbADAC�mAC�FAC�hAC�ACx�AC`BACK�ACC�AC33AC33AC&�AC+AC&�AC�ACoAC
=AB��AB�AB�/AB�!ABbNAA�#A@JA>�\A=�A;�7A;+A:�A:��A:�yA:�HA:�\A:=qA:{A9��A9��A9p�A9"�A8z�A7��A7+A6�A6�\A5��A5|�A5+A5VA4�/A4��A4n�A4 �A3A3��A3x�A3hsA3`BA3\)A3S�A3G�A3;dA3+A3"�A3"�A3"�A3�A3�A3�A3�A3�A3VA2��A2�A2�HA2��A2�+A1�FA1XA0ĜA/��A.�9A,��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                  A�p�A�p�A�p�A�r�A�t�A�t�A�t�A�t�A�r�A�n�A�p�A�r�A�p�A�C�A�;dA�E�A�33A�(�A�bA���A���A���A���A�ȴA�ƨA�ƨA�ƨA�ƨA�ƨA�ĜA�ƨA�ƨA�ƨA�A���A;wA;wAͺ^AͲ-A͝�A��A�t�AʼjA� �AɼjA�"�A�hsA�A�33A���A��DA��A���A��A��yA���A�`BA���A�A���A�/A�$�A�VA��A�33A��A���A���A�=qA�ffA���A���A��A��Ax��At=qAp�Akl�Ad(�A_��A\�`AY�AX�AU�mAS�AP�DANr�AL�AKx�AJ1AH�+AF{AD��AD=qAC�AC7LAB�`A=��A:n�A89XA5?}A3��A3;dA3�A2bNA-�mA,9XA+x�A*��A(��A'�A&��A&�A%dZA%S�A%p�A%�A&�DA&�+A'��A(ĜA(��A(r�A'��A'�wA'ƨA'��A&�A&��A&�9A&�\A&jA&A%t�A%A$�\A$VA#�
A"ĜA" �A!��A!S�A!"�A z�A��AK�A�A��AVA�-A"�A�HA5?A  A��A��A/A�/A��AC�A��A��AE�A��A��Ar�AJA��A&�A�A9XA�#A�7AS�A�A�A�Ap�A+A��AbNA$�AJA�mA�wA��AS�A�A=qA5?A5?A�A�-A�hA`BAoA�An�AVA5?AA�A
��A
5?A	�;A	��A	hsA	�A��A�`A�AM�A�A��A��AO�A�A�A�A��AM�A�A��A��A�A7LA��A~�A{A��A��A?}AȴA�A�DAn�AZA5?AA;dA �`A ��A �\A M�A @�\)@��H@��@�`B@��@�o@���@�V@��@�x�@�?}@���@��@��m@�;d@���@��@�/@�z�@�dZ@���@���@�\@��@�^@��@�@�V@�z�@��@�
=@�!@�+@�@�Ĝ@�1'@�|�@�o@ꗍ@�$�@��@�X@�/@��@�j@��;@��y@�~�@��@噚@�7L@���@�j@��m@�^5@�h@�hs@���@�b@�|�@ާ�@�=q@�@ݲ-@���@ܴ9@�I�@۝�@�o@���@�^5@���@�x�@�%@�r�@ׅ@��y@�n�@�^5@�=q@���@�p�@�/@��`@ԋD@���@ӥ�@�33@ҸR@�n�@�^5@�=q@�@с@�%@Ϯ@��@���@ΰ!@�V@ͺ^@���@�z�@�1@˥�@�
=@�v�@���@��@���@��@ǝ�@�"�@��@�=q@��@ź^@�p�@�X@�7L@���@Ĭ@þw@��@��H@�@�^5@�-@��#@�7L@��j@�A�@�  @��;@�33@��R@���@�/@��@���@�1'@�;d@��@���@�V@�$�@���@��T@��^@�G�@��u@�Q�@��@���@�S�@���@�^5@���@���@��@��@�Z@� �@��;@���@�l�@��@�ff@�M�@�=q@�J@��@��h@�%@��9@�bN@�9X@�ƨ@��@���@���@�t�@�dZ@�C�@��y@�v�@�-@�{@��-@�V@���@�Q�@� �@�1@��@�@��H@�ȴ@�n�@��-@�O�@��/@�I�@��m@��@��@��@��T@��@�G�@�%@��j@���@�z�@�1'@��w@��@�l�@�\)@�ȴ@�{@��T@�@��h@�7L@�V@��@��@���@�"�@���@��@�{@���@�/@���@�I�@�  @�t�@��@��@���@��\@��+@�v�@�V@��^@�7L@���@��@���@��m@��;@��F@��@��y@��R@��!@��\@�V@�J@��#@���@��7@�%@��j@��@� �@���@�l�@�"�@���@���@�v�@�@��^@�x�@�&�@���@��`@��9@�A�@��;@���@�33@�@��!@�^5@�=q@�{@��@���@�G�@�V@���@�Ĝ@��D@�bN@�1'@�  @���@��w@��@�l�@�"�@�@��@��@���@�=q@���@�?}@���@�A�@� �@��@�b@�  @��F@��P@�K�@��@���@���@�~�@�$�@���@���@���@��@�&�@���@��/@���@�Ĝ@���@�z�@�Z@�b@�ƨ@�|�@�C�@���@�~�@�v�@�V@���@���@��-@�X@��@�r�@�j@�Q�@�(�@�;@~�@~ff@}@|�@|Z@{��@{C�@z��@z�\@z^5@z-@y��@y��@y�7@y7L@xĜ@x�@xQ�@wl�@v�+@vff@v{@u@up�@t��@t��@t�@sƨ@s��@sdZ@so@r�!@rn�@q�@q�^@q�^@q�7@qX@qG�@q�@p  @o�P@n�@m�T@m��@m�h@mO�@m/@mV@l�D@k��@kC�@k"�@k@jM�@i��@ihs@i&�@h�u@g��@g��@gl�@g�@f�@fv�@f5?@f{@e��@e�@e�@d�D@c�@co@b��@b��@b=q@a�^@`�`@`r�@`  @_�w@_�P@^ȴ@^�R@^�R@^��@^V@]�-@]�@]p�@]O�@]/@\��@\9X@[ƨ@Z�@ZM�@ZJ@Y�7@Y7L@Y�@X��@X �@W+@V�R@VV@V$�@V$�@V$�@U�@U��@TZ@T(�@T�@T1@S�@R��@Rn�@R�@QG�@P  @O�@OK�@O
=@N�y@N�R@Nff@NE�@M�@M@M�-@M�h@M?}@L�@Lj@L(�@K�
@K��@K"�@J�H@J^5@I�#@Ix�@I%@H��@Hb@G|�@G�@F�@Fȴ@FE�@F$�@F{@E@E/@D�D@C��@C��@C"�@B��@B�\@BM�@B-@B-@A��@A�^@A%@?�P@>�@>�R@>�+@>ff@>5?@=��@<��@<��@<z�@<1@;��@;"�@:�\@:n�@9��@9�7@9�7@9�7@9�7@9x�@9G�@9%@8Ĝ@8�9@8��@8�@8bN@8Q�@8 �@7��@7�@7l�@7K�@7+@6��@6�R@65?@5�h@5/@4��@4z�@4�@3�m@3��@3dZ@333@3o@2�H@2��@2M�@1�@1��@1��@1hs@17L@0��@01'@/��@/l�@/K�@.ȴ@.��@.ff@-��@,�@,�/@,��@,��@,��@,�@,1@,�@,(�@,(�@,1@+�F@+t�@+S�@+C�@+33@+"�@*�@*��@*�\@*-@)�@)��@)X@)�@)�@)�@)�@)%@(��@(�@(A�@'��@'��@'�P@'|�@';d@&�y@&v�@&ff@&5?@%�@%��@%��@%�@%O�@%/@$�j@$z�@$Z@$1@#��@#�@#t�@#C�@#@"�@"�H@"�!@"n�@"M�@"=q@"�@!��@!��@!�7@!X@!�@ ��@ �@ Q�@  �@   @��@\)@;d@�@
=@�y@�@�@�@��@E�@{@�T@�h@/@�/@j@Z@(�@��@dZ@dZ@t�@t�@S�@o@��@�!@�\@=q@��@��@��@X@&�@�@��@��@��@Q�@ �@�@�@|�@K�@
=@�@ȴ@ff@$�@��@p�@p�@?}@V@��@�j@�@z�@Z@9X@(�@1@��@�m@�
@��@S�@S�@dZ@C�@o@o@�H@��@~�@~�@~�@^5@J@��@��@X@%@Ĝ@��@�u@�@Q�@b@��@�@��@�P@|�@;d@�@v�@ff@VG�O�A�l�A�r�A�n�A�l�A�r�A�r�A�n�A�n�A�r�A�r�A�n�A�p�A�t�A�r�A�p�A�t�A�v�A�p�A�t�A�v�A�r�A�t�A�v�A�v�A�r�A�t�A�v�A�r�A�p�A�r�A�v�A�ffA�p�A�n�A�p�A�p�A�p�A�t�A�n�A�r�A�x�A�p�A�p�A�v�A�t�A�n�A�n�A�p�A�p�A�p�A�S�A�G�A�K�A�C�A�G�A�E�A�1'A�-A�1'A�;dA�5?A�5?A�?}A�=qA�;dA�;dA�O�A�I�A�;dA�C�A�G�A�K�A�K�A�E�A�I�A�C�A�7LA�+A�&�A�/A�(�A�&�A�/A�/A� �A�&�A�(�A�(�A�bA�VA�VA�A�"�A� �A��yA���A��HA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ƨA�ȴA���A�ȴA�ƨA�ȴA���A�ȴA�ĜA�ĜA�ȴA�ȴA�ĜA�ĜA�ȴA�ȴA�ĜA�A�ƨA�ȴA�ĜA�ĜA�ƨA�ȴA�ĜA�A�ĜA�ȴA�ƨA�A�ĜA�ȴA�ȴA�ĜA�ĜA�ȴA�ȴA�ĜA�A�ĜA�ȴA�ĜA�ĜA�A�ƨA�ȴA�ƨA�ĜA�ĜA�ƨA�ȴA�ĜA�ĜA�ȴA�ȴA�ĜA�A�ƨA�ȴA�ȴA�ƨA�ĜA�ĜA�ƨA�ȴA�ĜA�A�A�A�A;wA���A�ĜA�A���A;wA;wA���A���A;wAͼjA;wA���A���A���Aͺ^AͼjA;wA���A;wAͺ^Aͺ^Aͺ^Aͺ^AͶFAͲ-Aʹ9A͸RAʹ9AͲ-AͮAͧ�Aͩ�Aͧ�A͛�A͙�A͗�A͏\AͅA�n�A�XA���A�Q�A�%A�ƨA�x�A�7LA�JA�A��TA���AʸRAʝ�A�A�A��A�oA��A�1'A�/A��A�VA���Aɡ�A�r�A�ZA�=qA�5?A��A��A�%A���A��AȓuA�5?A�JA���A�ffA�(�A�oA�
=AƸRA�E�A�dZA�"�Aě�A�G�A�A�C�A�  A�ƨA��A�33A��A�~�A��;A��A��+A�K�A�ȴA�bNA�%A�A��uA�ZA�"�A�%A��`A���A��9A��DA��hA�z�A�v�A�p�A�l�A�dZA�O�A�7LA�oA��A��^A���A��A�`BA�;dA�%A���A���A��A�XA�33A�A���A��A��mA��/A���A��9A�t�A�ZA�G�A�E�A�=qA�$�A��A�
=A�%A�A�A���A���A��mA��#A��wA�bNA�%A���A�?}A��\A��jA�I�A���A��hA��A�bA��FA�5?A��
A���A��7A�A�A���A�  A���A��DA�|�A�r�A�ffA�K�A�-A�{A��A��wA���A�hsA�C�A�VA���A�XA�-A��A�ĜA�~�A�+A���A���A��
A�7LA��A�5?A��A�Q�A�ĜA���A�"�A��A��-A��A�+A�n�A��A���A�{A�ĜA��9A��\A�dZA�$�A���A��\A�x�A�9XA�~�A�oA��#A�ȴA��-A��+A�n�A�bNA�^5A�I�A��A�VA�%A��TA�v�A�VA���A�A�A���A�C�A��A��A�ĜA�t�A�bA�S�A���A��-A���A��A�VA�A�9XA�A���A�n�A�(�A��A���A��RA���A���A��A�Q�A�
=A��A�33A��A��jA���A�XA�&�A�A��;A���A��FA���A��PA�n�A�K�A� �A�{A�
=A��/A��+A�t�A�`BA�G�A�+A�A��A�A��+A�(�A��A�/A�{A�A���A��A��yA��;A��!A�jA���A�C�A���A��PA�9XA��RA��PA�p�A�"�A�A�t�A�A�ffA��A��!A��A�?}A�33A�"�A��A��A���A��`A��A��A�"�A�O�A���A��A�  A��wA��9A��A���A���A��hA��7A�z�A�hsA�^5A�9XA`BA}VAz�`Az �Ay��Ay"�Ax~�AxJAw��Aw��Aw`BAwoAu�Atz�As�#Ar��Ar��Arr�Ar-Aq�wAq\)Aq�Ap��Ap^5AoƨAol�Ao+An��An  AkXAh�DAgVAfz�Af�Ae�-Ad�HAc�Ac33AbjAa�FAa&�A`��A`v�A`  A_�A_�A^�RA^=qA]�7A]K�A]33A\��A\-A[��A[/AZ��AZ �AY��AYhsAYK�AY33AY�AY
=AX��AX��AX�yAXĜAX^5AWp�AV�HAVZAU�AU�PAU+AT�jAT�ATQ�AS�TAS��AR�yAQ��AQƨAQ��AQO�AQ
=AP��APZAP{AO�
AOdZAOVAN�9ANr�AN9XAN  AM��AM�AMK�AL��ALv�AL^5AL=qAL(�ALbAK�AK�#AK�AKl�AK�AJ�AJ��AJ��AJVAJ{AI�mAIAI��AIx�AIC�AIAH�HAH�RAHz�AHM�AG33AFȴAF��AFjAF1'AE�AE��AE\)AE/AE�AD��AD��AD�AD��AD~�ADjADZADE�ADA�AD9XAD1'AD �ADbADAC�mAC�FAC�hAC�ACx�AC`BACK�ACC�AC33AC33AC&�AC+AC&�AC�ACoAC
=AB��AB�AB�/AB�!ABbNAA�#A@JA>�\A=�A;�7A;+A:�A:��A:�yA:�HA:�\A:=qA:{A9��A9��A9p�A9"�A8z�A7��A7+A6�A6�\A5��A5|�A5+A5VA4�/A4��A4n�A4 �A3A3��A3x�A3hsA3`BA3\)A3S�A3G�A3;dA3+A3"�A3"�A3"�A3�A3�A3�A3�A3�A3VA2��A2�A2�HA2��A2�+A1�FA1XA0ĜA/��A.�9A,��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                  ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�wB��B��B��B�wB��B��B��B��B�}B��B��B�IB�3B��B��B�B��B�LB��B�<B��B��B��B��B�'B�[BÖB�3B�gB�gBĜB��B�B�B�gB��BÖB�'B�B�B��B	8�B	IB	V�B	[�B	e�B	�LB
�B
�B
2�B
G�B
_B
�nB-CB1[B6zBK)B�jB�B��Bh
B
�B
�xB
��B
{�B
r|B
qAB
r�B
\�B
9$B
!�B	� B	��B	��B	��B	��B	y�B	R B	=�B	8�B	<�B	<6B	G�B	A�B	1�B	%zB	!�B	"hB	,qB	@�B	S[B	OvB	MjB	N�B	K)B	EB	9$B	�B	�B	�B�(B��B�B��B	GB��B	oB	YB	"�B	4nB	;�B	=<B	H�B	WsB	t�B	�;B	��B	�:B	��B	�B	�xB
:B
VB
 'B
!bB
-�B
-�B
-�B
0�B
2�B
5�B
8�B
9�B
:�B
;0B
;0B
;�B
6zB
2�B
/�B
.}B
-wB
1�B
2�B
1�B
1�B
2aB
5B
6zB
7LB
6FB
7�B
7B
7�B
7LB
9XB
8�B
9XB
?�B
<�B
<jB
>wB
>�B
?B
>�B
>�B
?�B
AUB
B�B
B�B
C�B
C�B
C�B
D3B
EB
C�B
C�B
DgB
EB
EB
E9B
E�B
F?B
F?B
E�B
F�B
F�B
C�B
C�B
C-B
B�B
DgB
A�B
C-B
C�B
B�B
A�B
A�B
A�B
B'B
B[B
@OB
@OB
>�B
>�B
>�B
>�B
>wB
>B
>BB
>�B
>�B
=�B
>B
=<B
<�B
<B
;dB
;�B
<B
;�B
;0B
;0B
;0B
:�B
:�B
9$B
9�B
8RB
6�B
8�B
7LB
6FB
6FB
5tB
5?B
4�B
5�B
49B
2�B
1�B
33B
2aB
2-B
1[B
/�B
/OB
,qB
,�B
)�B
)*B
)�B
*eB
(�B
(XB
)*B
'�B
'B
(XB
&B
'�B
%FB
%�B
%�B
$@B
#:B
#�B
#nB
#B
"4B
!�B
#B
!bB
"4B
!�B
 'B
VB
 �B
!B
!B
OB
�B
~B
�B
�B
xB
B
�B
qB
�B
�B
�B
kB
B
�B
eB
�B
1B
_B
�B
$B
�B
�B
�B
B
B
�B
�B
�B
B
B
FB
B
�B
@B
�B
B
oB
B
B
4B
hB
4B
 B
�B
�B
�B
bB
bB
bB
�B
.B
�B
�B
�B
�B
(B
\B
�B
�B
�B
�B
VB
�B
�B
�B
�B
PB
�B
�B
�B
�B
�B
B
"B
PB
�B
PB
�B
�B
�B
�B
PB
PB
PB
B
"B
�B
�B
�B
�B
PB
�B
�B
�B
�B
PB
PB
�B
"B
�B
�B
VB
�B
\B
(B
�B
�B
�B
(B
\B
�B
�B
�B
�B
�B
�B
�B
.B
�B
�B
4B
 B
B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
@B
�B
�B
B
B
{B
FB
B
B
{B
B
FB
�B
�B
�B
{B
MB
MB
�B
B
�B
�B
�B
�B
SB
SB
�B
�B
$B
�B
�B
_B
1B
�B
B
�B
7B
kB
�B
�B
�B
�B
�B
qB
=B
	B
�B
xB
B
CB
B
xB
xB
CB
B
~B
IB
�B
OB
B
�B
�B
�B
 'B
�B
 �B
 �B
 �B
!bB
!bB
!-B
!-B
 �B
 �B
!�B
!�B
"4B
#B
"�B
"�B
"�B
"�B
#�B
#nB
#�B
#�B
#�B
$B
$tB
$@B
$@B
$�B
%�B
%�B
&B
'RB
'RB
&�B
'�B
'B
'RB
'RB
($B
(XB
(�B
)*B
(�B
(�B
)_B
*eB
*eB
*eB
+�B
+kB
,=B
,=B
,=B
,�B
,�B
-CB
-�B
-�B
-�B
.}B
.}B
.�B
.�B
/B
/OB
.�B
/B
/�B
/�B
/�B
/�B
/�B
/�B
0UB
1[B
1�B
1�B
2�B
2-B
2-B
1�B
2-B
2�B
2aB
2�B
2�B
2�B
33B
33B
3�B
49B
49B
49B
4�B
5�B
5tB
5tB
5?B
5B
5�B
6FB
6B
6�B
7B
7LB
7�B
8�B
8�B
8�B
8�B
9XB
9$B
9$B
:*B
:�B
:^B
:^B
:^B
:^B
:�B
;�B
<6B
=B
=B
=qB
=�B
=�B
>BB
>wB
>wB
>�B
>�B
>�B
?B
?}B
?�B
?�B
@B
@�B
AUB
@�B
AUB
AUB
AUB
A�B
B�B
CaB
CaB
C�B
C�B
C�B
D3B
C�B
D�B
DgB
DgB
D�B
D�B
DgB
DgB
E9B
E9B
F?B
F�B
F?B
FtB
FtB
FtB
F?B
GB
G�B
GEB
GEB
GEB
I�B
I�B
I�B
I�B
I�B
H�B
H�B
I�B
I�B
I�B
I�B
J#B
JXB
K^B
K�B
LdB
L�B
MB
MB
MjB
MjB
NB
N�B
O�B
PB
PB
PHB
PB
P�B
PB
PB
PB
PHB
P}B
P}B
PHB
PHB
PHB
PHB
Q�B
QNB
RTB
RTB
RTB
R�B
R�B
R B
R�B
S&B
T,B
TaB
TaB
T�B
T,B
TaB
T,B
S�B
VmB
U2B
UgB
T�B
V9B
VB
VB
U�B
W�B
W�B
X�B
X�B
YB
YB
Y�B
Y�B
YB
ZB
Y�B
Y�B
Y�B
Y�B
ZB
Z�B
Z�B
Z�B
[WB
[�B
\)B
\�B
\�B
\�B
]dB
]�B
^5B
^jB
^�B
^�B
_B
_�B
_;B
_B
_�B
_;B
`BB
`B
`�B
aHB
aHB
a�B
a�B
a�B
a|B
a|B
a|B
c B
c�B
d&B
c�B
c�B
d&B
c�B
d�B
e�B
e,B
e,B
e�B
e�B
f�B
f�B
f�B
h
B
g�B
g�B
g�B
gmB
g�B
g�B
h>B
h>B
h>B
h>B
hsB
h>B
hsB
hsB
h�B
iB
iB
iDB
iB
iDB
iDB
jB
jB
j�B
kB
k�B
k�B
k�B
k�B
k�B
l"B
l"B
lWB
lWB
m]B
m)B
m)B
m)B
m]B
m]B
n/B
ncB
n�B
n�B
n�B
o5B
n�B
n�B
n�B
n/B
n/B
n/B
n/B
n/B
oiB
o�B
oiB
p;B
p�B
p�B
qAB
qAB
qvB
qvB
qvB
qvB
q�B
q�B
q�B
r|B
rGB
r�B
r�B
sB
r�B
r�B
r�B
r�B
sMB
sMB
s�B
tB
s�B
tB
s�B
tB
t�B
t�B
t�B
t�B
u%B
u%B
u�B
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
v`B
v�B
v�B
v�B
v�B
w2B
w2B
v�B
w2B
w�B
w�B
xB
x8B
x8B
xlB
xlB
x�B
x�B
x�B
y	B
yrB
y�B
y�B
y�B
zB
y�B
zB
y�B
zDB
zB
zB
zB
z�B
z�B
{JB
{JB
{B
{�B
|B
|�B
|�B
|�B
|�B
}"B
|�B
|�B
|�B
}"B
}"B
}VB
}VB
}�B
}�B
}�B
~(B
~(B
~]B
~�B
~�B
~�B
.B
.B
�B
~�B
cB
�B
.B
�B
��B
�;B
�;B
�iB
��B
�;B
�;B
��B
��B
�;B
�;B
�oB
�oB
�AB
�AB
�B
��B
��B
�GB
��B
�AB
�uB
��B
��B
�GB
�B
�B
��B
��B
�{B
�B
��B
��B
��B
�B
�B
��B
��B
��B
��B
�B
�SB
��B
��B
��B
��B
�YB
�+B
��B
�_B
�_B
�+B�B�qB�IB��B�B�B��B��B�qB�B��B�B��B��B��B��B�B��B�CB��B�IB�B�=B�CB�}B�wB��B�IB�B�wB��B��B��B��B�CB��B��B�qB�B��B�qB�B�}B�wB�CB��B��B��B�qB�OB�hB�[B��B��B�3B�-B��B�B�hB��B��B��B�aB��B�B�9B�B�aB�9B��B�-B�[B�'B��B�'B�3B�hB�tB�nB��B�B�B��B��B�LB�nB��B�tB��B��B��B��B��B��B�#B�B��B��B�dB��B��B��B�B��B�B��B�jB�qB�HB�BB�<B�BB��B��B��B��B�OB��B��B�HB�OB�UB�OB��B��B��B��B��B��B�[B��B��B� BB��B��B��BB�aB��B�UB��B�aBB��B��B��B�-B�'B��B�gB�3B��B��B�3B��BÖBÖB�9B�B�3B�aB�3B��B�9B��B�-B��B�B�B��B�aB�9B�B��BÖB�B��B��B�3B��B�gB��BŢBĜB��B�9B��BŢB�B�3B�B�9B�3B�3B�B��B�B��B��BĜB�mB��B��BÖB��B�9BĜB�aB��B�aBĜB�gB�aB��B�-B�aB�-B��B�[B��BB��B�B�B� B��B��B��B� B��B�XB�B%BN<B}�B�-B�2B	:B	&�B	(�B	3�B	:�B	?�B	XB	J#B	AUB	DgB	GEB	P�B	R�B	P�B	V9B	XyB	[#B	YB	YKB	YB	[�B	]/B	`�B	\]B	_�B	gB	e`B	h>B	qB	~�B	}�B	��B	��B	�/B	��B
�B
	7B
 �B
9�B
�B
�B
�B
B
�B
�B
B
�B
&B
v�B
?B
0�B
'�B
2�B
2aB
@OB
B�B
L�B
TaB
R�B
S�B
N<B
L�B
S[B
E�B
F?B
B'B
A�B
A�B
A�B
A�B
?�B
D�B
GzB
P�B
RTB
Q�B
O�B
R�B
PB
QNB
R�B
W
B
YB
\)B
c�B
d�B
hsB
i�B
k�B
ncB
v�B
��B
��B
��B
�=B
��B
��B
��B
��B
�7B
��B
��B
��B
��B
�$B
�FB
�{B
�~B
��B
�LB
�tB
��B
�fB
�B
�yB	�B
�>BxB2�B<6B2-B4B;dBE�BF�BT�B7LB5tB3hB2aB2aB3�B1'B0!B.�B2aB0UB0�B2�B0UB9$B5B7�B8�B33B3hB1[B/OB2-B?�BIRB>B.IB0�B.�B6FBMB!�BaHB�$B�~B��B��B��B�BB�B�EBרBیB��B��B��B�BȴB̘B�KB��B�B�B�B��B�*B�B��B�_B��B��B�hB�zB��B��B��B�MBv+B|�Bd�BU�BC�B6�B($B%B�BB
�yB
�6B
�dB
��B
�$B
��B
��B
�B
��B
�bB
�~B
��B
�_B
��B
��B
��B
��B
�bB
��B
�hB
��B
�1B
�xB
��B
�B
cB
|PB
|�B
y�B
{B
zDB
y�B
x8B
rGB
r|B
z�B
s�B
oiB
qB
pB
qB
r�B
kB
sB
o�B
v�B
��B
n/B
m�B
l�B
l�B
k�B
j�B
kQB
rB
t�B
�_B
zDB
w2B
i�B
oiB
d&B
V9B
T�B
[�B
VmB
L0B
T�B
NB
?HB
>B
5B
1�B
+B
)�B
$@B
#B
%FB
!�B
&B
4�B
B
"4B
�B
�B
SB	�mB	�&B	�B	�NB	��B	��B	�WB	�B	�?B	��B	�sB	�B	��B	�WB	��B	�dB	�0B	�6B	�!B	��B	�$B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�xB	��B	�B	�+B	��B	{�B	v�B	n/B	�B	�:B	��B	j�B	W
B	T�B	R�B	X�B	NB	P}B	K�B	J�B	?�B	>�B	@�B	@OB	>B	9XB	8�B	<6B	<6B	3�B	2-B	5B	;dB	:�B	6�B	;�B	B'B	?�B	:�B	:�B	;dB	:�B	;0B	:�B	:*B	9XB	=<B	B�B	OvB	A B	M�B	J�B	H�B	C�B	E9B	;�B	<6B	HB	?�B	YB	B[B	7�B	9�B	8�B	3hB	7LB	.�B	+�B	'RB	1[B	'RB	'B	%B	#nB	!�B	�B	 �B	%�B	%�B	 �B	�B	 �B	 �B	�B	!bB	 'B	!�B	#B	$B	$@B	#nB	'�B	,qB	/�B	-�B	.B	-�B	/OB	3hB	5?B	7B	9�B	<6B	I�B	c�B	T,B	PHB	RTB	S�B	S�B	S�B	S�B	PHB	PHB	PB	P�B	O�B	N<B	N<B	LdB	MjB	MjB	L�B	L�B	MjB	N�B	N�B	NpB	OvB	PHB	OvB	MB	L�B	N�B	NB	L�B	M�B	J�B	K�B	IB	H�B	G�B	G�B	F�B	HB	CaB	EmB	C�B	6�B	LdB	MjB	9�B	E9B	)�B	YB	FB	B	�B	�B	�B	_B	�B	B	$B	B		B	!bB	�B	7B	�B	B	�B	@B	�B	�B	1B	�B	�B	B	uB��B	AB��B�	B�8B��B��B��B�B��B��B��B�B��B�B��B�vB��B�vB�B�B�/B�B�B��B��B	B	�B	
rG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                  B�wB��B��B��B�wB��B��B��B��B�cB��B��B�5B�MB��B��B�9B�?B��B��B�<B��B��B��B��B�'B�[BÖB�3B�gB�gBĜB��B�B�B�gB�B��B��B��B��B��B	:xB	KxB	ZB	`�B	ncB	��B
(>B
5B
I�B
XEB
r�B
�XB>BA�BKDBI�B�yBбB��B�rB
��B
�sB
�1B
�GB
}<B
}B
��B
n}B
E�B
9�B	��B	�B	�{B	�CB	�B	�NB	`vB	HKB	B�B	A;B	E�B	Q4B	J	B	9	B	+�B	&2B	'mB	1�B	H�B	W�B	Q�B	OvB	PbB	M�B	U�B	D�B	/B	$B	�B��B��B��B	�B		B	 �B	�B	�B	%�B	8lB	>(B	?�B	I7B	W?B	sB	cB	�?B	��B	�wB	�B	��B
�B
�B
 'B
"B
/�B
.cB
.cB
1vB
3hB
7fB
:^B
;0B
<B
<B
="B
?HB
8�B
4nB
0�B
/iB
/�B
4�B
49B
2�B
2�B
4B
7LB
8lB
8RB
8�B
8�B
7�B
8lB
8�B
:�B
:B
=�B
AoB
=VB
=�B
@�B
A B
AB
@OB
@iB
A�B
B�B
D�B
C�B
D�B
D�B
ESB
GB
F�B
D�B
D�B
F%B
FB
E�B
E�B
F%B
F�B
F�B
F�B
I�B
G�B
DB
C�B
C�B
DgB
D�B
B�B
DgB
E�B
B�B
B[B
B'B
C�B
D�B
DB
A�B
A�B
?�B
?�B
@ B
?B
>�B
>�B
?�B
@B
?HB
>]B
?B
>B
=qB
<jB
<PB
<�B
<�B
<PB
;�B
<6B
<PB
;�B
<PB
:�B
;B
8�B
88B
:*B
7�B
6�B
6�B
5�B
5�B
6�B
7�B
5ZB
3hB
2�B
4B
3hB
3hB
2aB
1[B
0�B
.�B
.cB
*B
)�B
*�B
+QB
)yB
)�B
*B
(>B
(XB
)yB
'8B
)*B
&�B
'�B
'B
$ZB
#�B
$�B
$&B
#:B
"hB
"�B
$B
"�B
#�B
"�B
 �B
 �B
"�B
 BB
 vB
!B
�B
OB
�B
~B
�B
�B
]B
B
OB
xB
qB
qB
�B
QB
�B
7B
�B
�B
�B
EB
_B
�B
?B
�B
�B
�B
B
�B
�B
MB
MB
�B
�B
aB
FB
�B
�B
�B
&B
B
�B
�B
�B
�B
NB
4B
B
hB
 B
�B
B
}B
�B
�B
B
.B
bB
 B
NB
�B
�B
B
�B
HB
�B
�B
"B
�B
B
�B
B
<B
�B
�B
<B
"B
pB
VB
�B
B
�B
�B
�B
�B
�B
\B
"B
B
"B
�B
B
�B
�B
�B
pB
�B
�B
�B
�B
�B
BB
�B
�B
 B
�B
BB
�B
.B
vB
�B
\B
�B
 B
}B
HB
bB
�B
4B
�B
�B
�B
:B
�B
�B
B
 B
�B
�B
�B
�B
B
�B
B
&B
�B
FB
{B
{B
{B
�B
�B
aB
,B
aB
�B
aB
�B
�B
gB
�B
MB
�B
�B
mB
�B
�B
9B
�B

B
�B

B
�B
EB
B
B
�B
�B
�B
QB
�B
�B
�B
�B
#B
	B
�B
=B
�B
�B
qB
=B
�B
�B
xB
�B
xB
B
�B
IB
5B
5B
5B
�B
;B
OB
�B
�B
 �B
 �B
 \B
!�B
!|B
!HB
!�B
!|B
!HB
!bB
!HB
"B
#B
"hB
#�B
#TB
"�B
"�B
#B
#�B
$@B
#�B
#�B
#�B
$@B
$�B
$�B
$tB
$�B
%�B
&�B
&fB
&�B
(>B
'�B
'�B
'�B
'�B
(
B
($B
(�B
(�B
)_B
)yB
)*B
)_B
*KB
+B
*�B
+6B
,B
,B
,�B
,�B
,�B
-)B
-]B
-�B
.B
.B
.IB
.�B
.�B
/B
/OB
/�B
/iB
/B
/�B
0B
0B
0B
/�B
0!B
0�B
1vB
2-B
2|B
2�B
2�B
2GB
2GB
2-B
2�B
2�B
2�B
3hB
3MB
3MB
3�B
3�B
49B
4�B
4TB
4�B
5tB
6B
5�B
5�B
5tB
5ZB
5�B
6�B
6�B
7LB
7�B
7�B
8�B
9rB
9	B
9>B
9�B
9�B
9rB
9�B
;dB
;0B
:xB
:�B
:�B
:�B
;�B
<B
<�B
=�B
=�B
>(B
>(B
>]B
>�B
>�B
>�B
>�B
?B
?.B
?cB
?�B
@4B
@4B
@�B
A�B
A�B
A;B
A�B
A�B
A�B
B[B
CGB
C�B
C�B
C�B
DB
D3B
D�B
D�B
EB
DgB
D�B
D�B
D�B
D�B
EmB
E�B
E�B
G+B
F�B
FYB
F�B
F�B
F�B
F�B
G�B
HKB
GzB
GzB
G�B
J	B
J#B
I�B
J#B
JrB
H�B
H�B
I�B
J=B
JXB
J=B
JXB
J�B
K�B
LdB
MB
M�B
MjB
MPB
M�B
M�B
N�B
OvB
P.B
P}B
PbB
P}B
P�B
Q B
PB
P.B
PbB
P�B
P�B
P�B
PbB
P}B
P�B
Q B
RB
R B
R�B
R�B
R�B
R�B
R�B
RoB
S@B
TB
T�B
T�B
T�B
T�B
T,B
T�B
T�B
UB
V�B
UMB
U�B
U�B
V�B
VSB
VmB
V�B
X�B
XEB
YB
Y1B
Y�B
Y�B
ZB
Y�B
Y�B
ZQB
Y�B
Y�B
ZB
Z7B
Z�B
Z�B
Z�B
[	B
[�B
\B
\�B
]B
\�B
]IB
]�B
^5B
^�B
^�B
_B
^�B
_�B
_�B
_VB
_VB
`'B
_�B
`�B
`vB
a-B
a�B
a�B
a�B
a�B
a�B
a�B
a�B
bNB
dtB
d�B
dZB
c�B
dB
dZB
d�B
e�B
e�B
e`B
e�B
f2B
fLB
gRB
gB
gB
hsB
g�B
g�B
g�B
g�B
g�B
h$B
hsB
hXB
hXB
hXB
h�B
hXB
h�B
h�B
iB
i_B
i*B
i_B
iDB
i�B
i�B
j�B
j�B
kQB
kkB
l"B
k�B
k�B
l"B
l"B
l=B
lWB
l�B
l�B
m�B
m]B
m]B
m]B
m�B
m�B
n�B
o B
o B
n�B
oB
oiB
o B
oOB
oiB
nIB
nIB
n/B
ncB
n�B
o�B
o�B
oiB
p;B
p�B
q'B
q�B
q[B
q�B
q�B
q�B
q�B
q�B
q�B
rGB
r�B
r�B
r�B
sB
sB
r�B
r�B
r�B
sB
s�B
s�B
s�B
tTB
tB
t9B
t9B
tnB
u%B
t�B
u%B
u?B
u?B
uZB
u�B
u�B
u�B
v+B
vFB
u�B
u�B
u�B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
wLB
wLB
w2B
w�B
w�B
w�B
x8B
x�B
x�B
x�B
x�B
y	B
x�B
y	B
yXB
y�B
y�B
y�B
y�B
z*B
y�B
zB
zB
z�B
zDB
zDB
z^B
{B
{0B
{�B
{dB
{�B
|6B
|PB
|�B
|�B
|�B
|�B
}qB
}<B
}B
|�B
}qB
}qB
}�B
}�B
}�B
~(B
~B
~BB
~BB
~�B
~�B
~�B
.B
}B
cB
� B
B
�B
�B
�B
�B
� B
�oB
�;B
��B
��B
�oB
�UB
��B
�B
�UB
�UB
��B
��B
�[B
�[B
�'B
�'B
��B
�GB
��B
�[B
��B
��B
��B
��B
�3B
�B
��B
��B
��B
�gB
��B
�B
�9B
�SB
�9B
��B
��B
��B
�9B
�SB
�mB
��B
�B
�B
��B
��B
��B
��B
�zB
�zG�O�B�B�qB�IB��B�B�B��B��B�qB�B��B�B��B��B��B��B�B��B�CB��B�IB�B�=B�CB�}B�wB��B�IB�B�wB��B��B��B��B�CB��B��B�qB�B��B�qB�B�}B�wB�CB��B��B��B�qB�OB�hB�[B��B��B�3B�-B��B�B�hB��B��B��B�aB��B�B�9B�B�aB�9B��B�-B�[B�'B��B�'B�3B�hB�tB�nB��B�B�B��B��B�LB�nB��B�tB��B��B��B��B��B��B�#B�B��B��B�dB��B��B��B�B��B�B��B�jB�qB�HB�BB�<B�BB��B��B��B��B�OB��B��B�HB�OB�UB�OB��B��B��B��B��B��B�[B��B��B� BB��B��B��BB�aB��B�UB��B�aBB��B��B��B�-B�'B��B�gB�3B��B��B�3B��BÖBÖB�9B�B�3B�aB�3B��B�9B��B�-B��B�B�B��B�aB�9B�B��BÖB�B��B��B�3B��B�gB��BŢBĜB��B�9B��BŢB�B�3B�B�9B�3B�3B�B��B�B��B��BĜB�mB��B��BÖB��B�9BĜB�aB��B�aBĜB�gB�aB��B�-B�aB�-B��B�[B��BB��B�B�B� B��B��B��B� B��B�XB�B%BN<B}�B�-B�2B	:B	&�B	(�B	3�B	:�B	?�B	XB	J#B	AUB	DgB	GEB	P�B	R�B	P�B	V9B	XyB	[#B	YB	YKB	YB	[�B	]/B	`�B	\]B	_�B	gB	e`B	h>B	qB	~�B	}�B	��B	��B	�/B	��B
�B
	7B
 �B
9�B
�B
�B
�B
B
�B
�B
B
�B
&B
v�B
?B
0�B
'�B
2�B
2aB
@OB
B�B
L�B
TaB
R�B
S�B
N<B
L�B
S[B
E�B
F?B
B'B
A�B
A�B
A�B
A�B
?�B
D�B
GzB
P�B
RTB
Q�B
O�B
R�B
PB
QNB
R�B
W
B
YB
\)B
c�B
d�B
hsB
i�B
k�B
ncB
v�B
��B
��B
��B
�=B
��B
��B
��B
��B
�7B
��B
��B
��B
��B
�$B
�FB
�{B
�~B
��B
�LB
�tB
��B
�fB
�B
�yB	�B
�>BxB2�B<6B2-B4B;dBE�BF�BT�B7LB5tB3hB2aB2aB3�B1'B0!B.�B2aB0UB0�B2�B0UB9$B5B7�B8�B33B3hB1[B/OB2-B?�BIRB>B.IB0�B.�B6FBMB!�BaHB�$B�~B��B��B��B�BB�B�EBרBیB��B��B��B�BȴB̘B�KB��B�B�B�B��B�*B�B��B�_B��B��B�hB�zB��B��B��B�MBv+B|�Bd�BU�BC�B6�B($B%B�BB
�yB
�6B
�dB
��B
�$B
��B
��B
�B
��B
�bB
�~B
��B
�_B
��B
��B
��B
��B
�bB
��B
�hB
��B
�1B
�xB
��B
�B
cB
|PB
|�B
y�B
{B
zDB
y�B
x8B
rGB
r|B
z�B
s�B
oiB
qB
pB
qB
r�B
kB
sB
o�B
v�B
��B
n/B
m�B
l�B
l�B
k�B
j�B
kQB
rB
t�B
�_B
zDB
w2B
i�B
oiB
d&B
V9B
T�B
[�B
VmB
L0B
T�B
NB
?HB
>B
5B
1�B
+B
)�B
$@B
#B
%FB
!�B
&B
4�B
B
"4B
�B
�B
SB	�mB	�&B	�B	�NB	��B	��B	�WB	�B	�?B	��B	�sB	�B	��B	�WB	��B	�dB	�0B	�6B	�!B	��B	�$B	��B	��B	��B	��B	��B	��B	�B	��B	��B	��B	�xB	��B	�B	�+B	��B	{�B	v�B	n/B	�B	�:B	��B	j�B	W
B	T�B	R�B	X�B	NB	P}B	K�B	J�B	?�B	>�B	@�B	@OB	>B	9XB	8�B	<6B	<6B	3�B	2-B	5B	;dB	:�B	6�B	;�B	B'B	?�B	:�B	:�B	;dB	:�B	;0B	:�B	:*B	9XB	=<B	B�B	OvB	A B	M�B	J�B	H�B	C�B	E9B	;�B	<6B	HB	?�B	YB	B[B	7�B	9�B	8�B	3hB	7LB	.�B	+�B	'RB	1[B	'RB	'B	%B	#nB	!�B	�B	 �B	%�B	%�B	 �B	�B	 �B	 �B	�B	!bB	 'B	!�B	#B	$B	$@B	#nB	'�B	,qB	/�B	-�B	.B	-�B	/OB	3hB	5?B	7B	9�B	<6B	I�B	c�B	T,B	PHB	RTB	S�B	S�B	S�B	S�B	PHB	PHB	PB	P�B	O�B	N<B	N<B	LdB	MjB	MjB	L�B	L�B	MjB	N�B	N�B	NpB	OvB	PHB	OvB	MB	L�B	N�B	NB	L�B	M�B	J�B	K�B	IB	H�B	G�B	G�B	F�B	HB	CaB	EmB	C�B	6�B	LdB	MjB	9�B	E9B	)�B	YB	FB	B	�B	�B	�B	_B	�B	B	$B	B		B	!bB	�B	7B	�B	B	�B	@B	�B	�B	1B	�B	�B	B	uB��B	AB��B�	B�8B��B��B��B�B��B��B��B�B��B�B��B�vB��B�vB�B�B�/B�B�B��B��B	B	�B	
rG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                  <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<<(�<#�
<#�
<#�
<#�
<#�
<#�
<���<o:�<���<�,V<�gP<�X�=��<�<���<�l<#�
<;n<3D�<�0�=
�g<�Q<I2T<#�
<#�
<6b�<F�<�B3<��<T=�<��#<(g�<�v�<u=�<D!<���<���<mA�<14u<&Q�<#�
<&Q�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�
�<?L�<#�
<#�
<#�
<#�
<#�
<#�
<u=�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2019031820402620190318204026IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019032704004320190327040043QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019032704004320190327040043QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107551520190521075515IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                