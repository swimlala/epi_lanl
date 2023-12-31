CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-09-16T09:07:11Z creation; 2023-02-10T23:09:45Z DMQC;      
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
_FillValue        G�O�     0  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \8   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     0  d   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     0  �    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �0   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �`   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �,   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     0  �(   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 X   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 6�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 >T   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � ]�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     0 eP   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20220916090711  20230210230945  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_229                 6810_008521_229                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�����m@�����m11  @���i�C@���i�C@1�e�%@1�e�%�d�3	A�!�d�3	A�!11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @�\@B�\@�G�@�G�@��R@޸RAG�AG�A ��A,��A@��A`  A\)A�\)A�\)A�Q�A�Q�A�Q�A�Q�A�Q�B Q�Bz�BQ�B(�B�
B'�
B/�B7�
B@  BH(�BP(�BX(�B`z�BhQ�Bp  Bx  B�  B�  B�  B��B��B��B��B�  B�  B�  B�  B�  B�  B�  B��B�  B�{B�  B�  B��B�  B�(�B�(�B�{B�  B�(�B�{B�  B��B��
B��B��B��C
=C  C
=C
=C
  C�C��C��C�C  C
=C  C
=C
=C{C {C"
=C$
=C&{C({C*  C,  C.{C0
=C2
=C3��C6  C8{C9��C<  C>
=C?�CB  CD  CF  CH
=CJ
=CL  CN
=CP  CQ��CT
=CV  CX
=CZ  C\
=C^  C`  Ca��Cc��Cf  Ch  Ci��Cl  Cn  Cp  Cq��Cs�Cv  Cx  Cz  C|  C~
=C�  C�  C���C���C�  C���C���C�  C���C���C���C�C�
=C�
=C�C�C�C�  C���C���C�  C�C�
=C�
=C�C�C�C�C�C�C���C���C���C���C�  C�C�  C���C���C�  C�C���C���C���C���C�  C�C�C�C�  C���C���C���C���C���C�  C�C�C�  C�  C�C�  C�  C�C�  C���C���C���C�  C�C�  C���C���C�  C�C�C�  C���C���C���C�C�C���C�  C�C�C�
=C�C�  C�  C�  C�C�
=C���C���C�  C�  C�  C�C�C�C�C�C�C�C�C�C�  C�  C�  C�  C�C���C���C�  C�C�C���C���C���C���C�C�C�  C�  C�  C�C�C���D ��DD�D  D� D�qD� DD��D��Dz�D  D�D  D� D  Dz�D	  D	z�D	�RD
� D  D}qD  D��DD��DD� D  D}qD�qD��D�D� D��D}qD  D}qD  D� D  D� D�D�DD��D  D��D  Dz�D  D�D�D��D  D}qD  D}qD�D��D�qD� D   D }qD �qD!� D"  D"��D#�D#��D$�D$��D%  D%� D&  D&��D'  D'}qD'�qD(� D(�qD)}qD*  D*}qD*��D+}qD,  D,� D-�D-��D.  D.}qD/  D/��D0�D0� D0�qD1� D1�qD2}qD2�qD3� D4�D4z�D5  D5��D6  D6��D7�D7� D8�D8� D9  D9��D:  D:��D;�D;�D<  D<��D=�D=}qD=��D>}qD?  D?��D?�qD@}qDA  DA� DB  DB��DC  DC� DD  DD� DD�qDE� DE��DF}qDG  DG��DH�DH� DI  DI� DJ  DJ� DJ�qDK}qDL�DL��DM�DM��DNDN��DO  DO��DP�DP��DQ�DQ}qDQ�qDR��DSDS� DS��DT}qDT�qDU}qDV  DV��DWDW��DX  DX� DY  DY� DZ  DZ}qD[�D[�D\  D\��D]�D]� D]�qD^}qD_  D_��D`�D`��D`�qDaz�Da��Db� Dc  Dc� Dd  Dd}qDd��Dez�De��Dfz�Dg�Dg�Dh�Dh}qDh�qDi��Dj  Dj}qDk  Dk� Dl�Dl� Dl�qDm� Dn  Dn� Do�Do��Dp  Dp� Dq  Dq��Dr  Drz�Ds  Ds}qDt  Dt� Dt�qDu}qDv  Dv��Dw�Dw}qDw�qDx}qDx�qDy� Dz  Dz��Dz�qD{� D|  D|� D}  D}� D}�qD~}qD�D��D�  D�>�D�� D��HD�HD�AHD���D�� D��qD�>�D��HD�� D�HD�AHD��HD�� D���D�@ D���D��HD�HD�AHD��HD��HD�HD�AHD�� D���D�  D�>�D�~�D��HD�HD�B�D�� D��qD�  D�@ D�� D�� D��qD�=qD�~�D��HD�HD�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D��HD�HD�AHD��HD��HD��D�AHD�� D�� D�  D�AHD��HD�� D�  D�=qD�}qD���D���D�AHD���D�� D���D�>�D�~�D���D���D�@ D�� D�� D���D�=qD�~�D�� D��D�AHD�~�D�� D��D�>�D�}qD���D�  D�AHD�� D��qD�  D�AHD��HD�� D�  D�>�D�� D��HD�  D�@ D���D�� D���D�AHD��HD�� D���D�@ D���D�� D�  D�AHD�� D�� D�  D�>�D�~�D���D�HD�@ D�~�D�� D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�>�D�� D�� D�  D�=qD�~�D�� D�HD�@ D�~�D�� D���D�=qD�~�D���D��qD�=qD�� D�D�HD�@ D��HD�� D���D�>�D�� D��HD�  D�>�D�� D��HD�  D�>�D��HD��HD�HD�@ D�~�D���D�HD�AHD�� D�� D�  D�@ D�~�D�� D�  D�AHD��HD��HD�HD�AHD�� D�� D�HD�B�D��HD�� D�  D�AHD�~�D���D�  D�>�D�}qD�� D�HD�AHD�� D�� D�  D�AHD���D�� D�HD�@ D�~�D�� D�HD�AHD�� D�� D�HD�>�D�}qD�� D�  D�>�D�}qD���D�  D�>�D�~�D���D�HD�@ D�� D�� D��D�@ D�}qD��qD���D�@ D�� D�� D�  D�AHD�~�D�� D�HD�>�D�~�D�� D�HD�AHD�� D�� D�  D�@ D�~�D�� D�  D�@ DÁHD�� D�HD�AHD�~�DĽqD���D�>�Dŀ D�� D�  D�AHDƁHD�� D�  D�AHDǀ DǾ�D��qD�=qDȀ DȾ�D���D�@ D�~�Dɾ�D�  D�@ DʁHD�� D�HD�AHDˀ D��HD��D�AHD̀ D�� D���D�@ D́HD�� D�  D�>�D�}qD�� D��D�B�Dς�D�� D�  D�AHDЁHD�� D�  D�@ Dр DѾ�D���D�@ D�~�DҾ�D���D�AHDӁHDӽqD�  D�@ DԀ D�� D�  D�@ DՀ D��HD��D�AHDցHD�D��D�@ D׀ D�D��D�>�D�~�D��HD�HD�B�DفHDٽqD��qD�>�Dڀ Dھ�D��)D�>�DہHD�D�HD�AHD܂�D��HD�HD�AHD݀ D��HD�  D�AHDރ�D���D�  D�@ D߁HD߾�D�HD�@ D�� D�� D�HD�AHD� D��HD�HD�@ D�}qD⾸D���D�@ D� D�qD�  D�AHD�HD�D�  D�>�D� D�� D�  D�AHD� D��HD�HD�>�D�HD�� D��qD�>�D� D辸D�  D�@ D�}qD龸D�  D�>�D�~�D꾸D���D�AHD�HD뾸D�  D�AHD�HD��HD�  D�>�D� D�� D�  D�@ D� D�D�HD�@ D�HD�� D�  D�B�D���D�D���D�AHD�HD�� D�HD�AHD�HD��HD�  D�>�D� D��HD���D�@ D� D���D��qD�>�D�}qD���D���D�>�D�� D�� D�  D�AHD�� D��qD���>�Q�?W
=?��?�Q�?�G�@�\@
=@(��@@  @Q�@h��@�G�@��@�
=@��R@���@���@\@��@��H@���@�33A   AA�A�A
=A�RA#33A*�HA/\)A7
=A;�AB�\AG�AN�RATz�AY��A`��Ae�Al��AqG�Ax��A~{A�=qA��A�  A�33A�A�G�A��A�
=A�G�A���A�
=A��\A��A���A�33A�{A�G�A��
A��RA�G�A���A�ffA�=qA�z�A�
=A�=qA��
A�\)Aљ�A��
AָRAأ�A��
A�A���A�=qA�A�A��HA��A�A�\A�z�A�  A��A��A��B ��B�\B�B��BffB33B��B
=qB
=B��BB\)B��BB�BQ�B�B\)BQ�B{B33BQ�B�B
=B ��B"{B#33B$��B%�B'�B(��B*=qB+�B,��B.�\B/�B1G�B2�RB4  B5��B6�\B8z�B9p�B;33B<��B=��B?�B@��BA�BC�BD��BFffBG�
BH��BJ�RBK�
BMp�BN�RBP(�BQ��BR�RBTQ�BUp�BV�HBXQ�BYG�B[
=B[�B]p�B^ffB_�B`��Bb{Bc33Bd  Bep�Bf{Bg\)Bh(�Bh��Bj=qBj�RBl  BlQ�BmG�Bm�Bn�\Bo�Bo�
Bp��Bqp�BqBs
=Bs33BtQ�Bt��BuG�BvffBv�\Bw�Bx(�Bx��ByBy�B{
=B{\)B|(�B|��B}p�B~ffB~�RB�B�=qB�ffB��HB�G�B��B�  B�(�B��\B��HB�33B�B��B�ffB���B��HB�p�B���B�  B�Q�B��\B�
=B�G�B��B�{B�(�B���B���B�p�B��B�(�B��\B���B�33B�p�B��
B�Q�B��\B��HB�\)B��B�  B�ffB���B��B�p�B��B�=qB��\B��HB�\)B��B�{B�Q�B���B�33B�\)B��
B�(�B�ffB�
=B�33B�p�B�{B�=qB��RB��B�\)B�  B�(�B���B���B�\)B��
B�  B��\B��HB�G�B��
B�{B�Q�B���B�G�B��B�{B�ffB���B�\)B��B�  B�Q�B��RB�33B�p�B��B�=qB��\B��B�p�B��B�Q�B���B���B��B�B�=qB��RB���B��B��B�=qB���B��B��B�{B�Q�B���B�33B���B�{B�=qB���B�G�B��B�  B�z�B��RB�G�B���B�  B��\B���B�33B�B�  B�z�B�
=B�G�B�B�Q�B��\B��B��B��
B�ffB���B��B��B��B�=qB���B�33B�p�B�  B�Q�B���B�33B�p�B��
B�ffB��\B���B��B�B�(�B��\B���B�p�B��B�  B��\B��RB�G�B�B��B�Q�B���B��BÅB�{B�=qBģ�B�33B�\)B��
B�Q�Bƣ�B�
=BǅB�B�(�B���B�
=B�p�B�  B�Q�Bʏ\B�33B˙�B��
B�ffB���B��BͮB�{B�ffB���B�G�Bϙ�B�(�BЏ\B���B�p�B�B�(�BҸRB�
=B�\)B��B�ffBԏ\B��Bՙ�B�B�ffB���B���B�p�B�  B�ffBأ�B��BٮB��
B�Q�B���B�
=BۅB��B�(�Bܣ�B��B�p�B�B�Q�B�z�B���B߅B��
B�{B�RB��B�\)B��B�z�B���B��B㙚B�{B�ffB���B�\)B�B�  B�\B���B�G�B��
B�Q�B�\B���B�p�B��B�=qB��B�33B뙚B��B�ffB��HB�\)B홚B�  B��B���B�G�B�B�Q�B��B���B�B�  B�Q�B��HB�\)B�B�{B���B��HB�G�B��
B�(�B�z�B��HB�p�B�B�  B�z�B�
=B�G�B���B�(�B���B���B�G�B��B�=qB���B��HB�\)B��
B�(�B�z�B�
=B�p�B�C (�C ffC �C �RC  C=qCffC�C��C�CQ�C��C�HC  C=qC��C��C�C=qC�C��C�C33CQ�C��C�
C{C=qCp�CC�C{CQ�C��CC�C(�Cp�C��C��C	
=C	Q�C	�C	�C	�HC
=qC
\)C
�\C
�
C{CG�Cp�C��C
=C33Cp�C�RC  C33CffC�C��C=qCp�C��C��C=qCffC��C�
C(�CffC�\C�
C�CQ�C�C��C{C=qCz�CC
=C33Cp�CC��C�Cp�C�C�HC
=C\)C��C��C  CG�C�\C�RC�HC=qCz�C��C�HC33C\)C��C�
C�CffC��C��C  CQ�C�\C��C  C(�Cp�C�RC�HC{CG�C��C��C��C=qC�\CC�C(�C\)C�RC�HC{CffC��C��C {C Q�C ��C �RC ��C!=qC!�C!C!��C"�C"ffC"�RC#  C#=qC#p�C#��C#�HC$(�C$p�C$��C$�
C%�C%\)C%��C%�C&(�C&p�C&��C&��C'�C'p�C'�C'�C((�C(\)C(��C(�
C)(�C)p�C)�RC)�C*�C*p�C*C+
=C+33C+p�C+�C,  C,Q�C,�C,�RC-  C-G�C-��C-�HC.�C.Q�C.�\C.��C/{C/ffC/�C/��C033C0p�C0�C0�HC133C1�C1�
C2{C2\)C2�\C2�
C3{C3Q�C3�\C3�
C4�C4z�C4C5
=C5Q�C5��C5�C6(�C6ffC6��C6��C7G�C7�\C7�
C833C8z�C8�RC8��C933C9p�C9�RC:  C:Q�C:��C:�C;(�C;p�C;�RC;�C<(�C<ffC<�C=  C==qC=��C=�HC>�C>ffC>�C>�HC?�C?ffC?�C?��C@G�C@��C@�HCA(�CAffCA�RCA��CB=qCBz�CB�RCC  CCG�CC�CC��CD{CD\)CD��CD�CE33CEp�CE�RCF  CFG�CF�CF��CG
=CGQ�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                     ?�  @�\@B�\@�G�@�G�@��R@޸RAG�AG�A ��A,��A@��A`  A\)A�\)A�\)A�Q�A�Q�A�Q�A�Q�A�Q�B Q�Bz�BQ�B(�B�
B'�
B/�B7�
B@  BH(�BP(�BX(�B`z�BhQ�Bp  Bx  B�  B�  B�  B��B��B��B��B�  B�  B�  B�  B�  B�  B�  B��B�  B�{B�  B�  B��B�  B�(�B�(�B�{B�  B�(�B�{B�  B��B��
B��B��B��C
=C  C
=C
=C
  C�C��C��C�C  C
=C  C
=C
=C{C {C"
=C$
=C&{C({C*  C,  C.{C0
=C2
=C3��C6  C8{C9��C<  C>
=C?�CB  CD  CF  CH
=CJ
=CL  CN
=CP  CQ��CT
=CV  CX
=CZ  C\
=C^  C`  Ca��Cc��Cf  Ch  Ci��Cl  Cn  Cp  Cq��Cs�Cv  Cx  Cz  C|  C~
=C�  C�  C���C���C�  C���C���C�  C���C���C���C�C�
=C�
=C�C�C�C�  C���C���C�  C�C�
=C�
=C�C�C�C�C�C�C���C���C���C���C�  C�C�  C���C���C�  C�C���C���C���C���C�  C�C�C�C�  C���C���C���C���C���C�  C�C�C�  C�  C�C�  C�  C�C�  C���C���C���C�  C�C�  C���C���C�  C�C�C�  C���C���C���C�C�C���C�  C�C�C�
=C�C�  C�  C�  C�C�
=C���C���C�  C�  C�  C�C�C�C�C�C�C�C�C�C�  C�  C�  C�  C�C���C���C�  C�C�C���C���C���C���C�C�C�  C�  C�  C�C�C���D ��DD�D  D� D�qD� DD��D��Dz�D  D�D  D� D  Dz�D	  D	z�D	�RD
� D  D}qD  D��DD��DD� D  D}qD�qD��D�D� D��D}qD  D}qD  D� D  D� D�D�DD��D  D��D  Dz�D  D�D�D��D  D}qD  D}qD�D��D�qD� D   D }qD �qD!� D"  D"��D#�D#��D$�D$��D%  D%� D&  D&��D'  D'}qD'�qD(� D(�qD)}qD*  D*}qD*��D+}qD,  D,� D-�D-��D.  D.}qD/  D/��D0�D0� D0�qD1� D1�qD2}qD2�qD3� D4�D4z�D5  D5��D6  D6��D7�D7� D8�D8� D9  D9��D:  D:��D;�D;�D<  D<��D=�D=}qD=��D>}qD?  D?��D?�qD@}qDA  DA� DB  DB��DC  DC� DD  DD� DD�qDE� DE��DF}qDG  DG��DH�DH� DI  DI� DJ  DJ� DJ�qDK}qDL�DL��DM�DM��DNDN��DO  DO��DP�DP��DQ�DQ}qDQ�qDR��DSDS� DS��DT}qDT�qDU}qDV  DV��DWDW��DX  DX� DY  DY� DZ  DZ}qD[�D[�D\  D\��D]�D]� D]�qD^}qD_  D_��D`�D`��D`�qDaz�Da��Db� Dc  Dc� Dd  Dd}qDd��Dez�De��Dfz�Dg�Dg�Dh�Dh}qDh�qDi��Dj  Dj}qDk  Dk� Dl�Dl� Dl�qDm� Dn  Dn� Do�Do��Dp  Dp� Dq  Dq��Dr  Drz�Ds  Ds}qDt  Dt� Dt�qDu}qDv  Dv��Dw�Dw}qDw�qDx}qDx�qDy� Dz  Dz��Dz�qD{� D|  D|� D}  D}� D}�qD~}qD�D��D�  D�>�D�� D��HD�HD�AHD���D�� D��qD�>�D��HD�� D�HD�AHD��HD�� D���D�@ D���D��HD�HD�AHD��HD��HD�HD�AHD�� D���D�  D�>�D�~�D��HD�HD�B�D�� D��qD�  D�@ D�� D�� D��qD�=qD�~�D��HD�HD�@ D�� D�� D�  D�@ D�� D�� D���D�@ D�� D��HD�HD�AHD��HD��HD��D�AHD�� D�� D�  D�AHD��HD�� D�  D�=qD�}qD���D���D�AHD���D�� D���D�>�D�~�D���D���D�@ D�� D�� D���D�=qD�~�D�� D��D�AHD�~�D�� D��D�>�D�}qD���D�  D�AHD�� D��qD�  D�AHD��HD�� D�  D�>�D�� D��HD�  D�@ D���D�� D���D�AHD��HD�� D���D�@ D���D�� D�  D�AHD�� D�� D�  D�>�D�~�D���D�HD�@ D�~�D�� D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�@ D�� D���D���D�>�D�� D�� D�  D�=qD�~�D�� D�HD�@ D�~�D�� D���D�=qD�~�D���D��qD�=qD�� D�D�HD�@ D��HD�� D���D�>�D�� D��HD�  D�>�D�� D��HD�  D�>�D��HD��HD�HD�@ D�~�D���D�HD�AHD�� D�� D�  D�@ D�~�D�� D�  D�AHD��HD��HD�HD�AHD�� D�� D�HD�B�D��HD�� D�  D�AHD�~�D���D�  D�>�D�}qD�� D�HD�AHD�� D�� D�  D�AHD���D�� D�HD�@ D�~�D�� D�HD�AHD�� D�� D�HD�>�D�}qD�� D�  D�>�D�}qD���D�  D�>�D�~�D���D�HD�@ D�� D�� D��D�@ D�}qD��qD���D�@ D�� D�� D�  D�AHD�~�D�� D�HD�>�D�~�D�� D�HD�AHD�� D�� D�  D�@ D�~�D�� D�  D�@ DÁHD�� D�HD�AHD�~�DĽqD���D�>�Dŀ D�� D�  D�AHDƁHD�� D�  D�AHDǀ DǾ�D��qD�=qDȀ DȾ�D���D�@ D�~�Dɾ�D�  D�@ DʁHD�� D�HD�AHDˀ D��HD��D�AHD̀ D�� D���D�@ D́HD�� D�  D�>�D�}qD�� D��D�B�Dς�D�� D�  D�AHDЁHD�� D�  D�@ Dр DѾ�D���D�@ D�~�DҾ�D���D�AHDӁHDӽqD�  D�@ DԀ D�� D�  D�@ DՀ D��HD��D�AHDցHD�D��D�@ D׀ D�D��D�>�D�~�D��HD�HD�B�DفHDٽqD��qD�>�Dڀ Dھ�D��)D�>�DہHD�D�HD�AHD܂�D��HD�HD�AHD݀ D��HD�  D�AHDރ�D���D�  D�@ D߁HD߾�D�HD�@ D�� D�� D�HD�AHD� D��HD�HD�@ D�}qD⾸D���D�@ D� D�qD�  D�AHD�HD�D�  D�>�D� D�� D�  D�AHD� D��HD�HD�>�D�HD�� D��qD�>�D� D辸D�  D�@ D�}qD龸D�  D�>�D�~�D꾸D���D�AHD�HD뾸D�  D�AHD�HD��HD�  D�>�D� D�� D�  D�@ D� D�D�HD�@ D�HD�� D�  D�B�D���D�D���D�AHD�HD�� D�HD�AHD�HD��HD�  D�>�D� D��HD���D�@ D� D���D��qD�>�D�}qD���D���D�>�D�� D�� D�  D�AHD�� D��qG�O�>�Q�?W
=?��?�Q�?�G�@�\@
=@(��@@  @Q�@h��@�G�@��@�
=@��R@���@���@\@��@��H@���@�33A   AA�A�A
=A�RA#33A*�HA/\)A7
=A;�AB�\AG�AN�RATz�AY��A`��Ae�Al��AqG�Ax��A~{A�=qA��A�  A�33A�A�G�A��A�
=A�G�A���A�
=A��\A��A���A�33A�{A�G�A��
A��RA�G�A���A�ffA�=qA�z�A�
=A�=qA��
A�\)Aљ�A��
AָRAأ�A��
A�A���A�=qA�A�A��HA��A�A�\A�z�A�  A��A��A��B ��B�\B�B��BffB33B��B
=qB
=B��BB\)B��BB�BQ�B�B\)BQ�B{B33BQ�B�B
=B ��B"{B#33B$��B%�B'�B(��B*=qB+�B,��B.�\B/�B1G�B2�RB4  B5��B6�\B8z�B9p�B;33B<��B=��B?�B@��BA�BC�BD��BFffBG�
BH��BJ�RBK�
BMp�BN�RBP(�BQ��BR�RBTQ�BUp�BV�HBXQ�BYG�B[
=B[�B]p�B^ffB_�B`��Bb{Bc33Bd  Bep�Bf{Bg\)Bh(�Bh��Bj=qBj�RBl  BlQ�BmG�Bm�Bn�\Bo�Bo�
Bp��Bqp�BqBs
=Bs33BtQ�Bt��BuG�BvffBv�\Bw�Bx(�Bx��ByBy�B{
=B{\)B|(�B|��B}p�B~ffB~�RB�B�=qB�ffB��HB�G�B��B�  B�(�B��\B��HB�33B�B��B�ffB���B��HB�p�B���B�  B�Q�B��\B�
=B�G�B��B�{B�(�B���B���B�p�B��B�(�B��\B���B�33B�p�B��
B�Q�B��\B��HB�\)B��B�  B�ffB���B��B�p�B��B�=qB��\B��HB�\)B��B�{B�Q�B���B�33B�\)B��
B�(�B�ffB�
=B�33B�p�B�{B�=qB��RB��B�\)B�  B�(�B���B���B�\)B��
B�  B��\B��HB�G�B��
B�{B�Q�B���B�G�B��B�{B�ffB���B�\)B��B�  B�Q�B��RB�33B�p�B��B�=qB��\B��B�p�B��B�Q�B���B���B��B�B�=qB��RB���B��B��B�=qB���B��B��B�{B�Q�B���B�33B���B�{B�=qB���B�G�B��B�  B�z�B��RB�G�B���B�  B��\B���B�33B�B�  B�z�B�
=B�G�B�B�Q�B��\B��B��B��
B�ffB���B��B��B��B�=qB���B�33B�p�B�  B�Q�B���B�33B�p�B��
B�ffB��\B���B��B�B�(�B��\B���B�p�B��B�  B��\B��RB�G�B�B��B�Q�B���B��BÅB�{B�=qBģ�B�33B�\)B��
B�Q�Bƣ�B�
=BǅB�B�(�B���B�
=B�p�B�  B�Q�Bʏ\B�33B˙�B��
B�ffB���B��BͮB�{B�ffB���B�G�Bϙ�B�(�BЏ\B���B�p�B�B�(�BҸRB�
=B�\)B��B�ffBԏ\B��Bՙ�B�B�ffB���B���B�p�B�  B�ffBأ�B��BٮB��
B�Q�B���B�
=BۅB��B�(�Bܣ�B��B�p�B�B�Q�B�z�B���B߅B��
B�{B�RB��B�\)B��B�z�B���B��B㙚B�{B�ffB���B�\)B�B�  B�\B���B�G�B��
B�Q�B�\B���B�p�B��B�=qB��B�33B뙚B��B�ffB��HB�\)B홚B�  B��B���B�G�B�B�Q�B��B���B�B�  B�Q�B��HB�\)B�B�{B���B��HB�G�B��
B�(�B�z�B��HB�p�B�B�  B�z�B�
=B�G�B���B�(�B���B���B�G�B��B�=qB���B��HB�\)B��
B�(�B�z�B�
=B�p�B�C (�C ffC �C �RC  C=qCffC�C��C�CQ�C��C�HC  C=qC��C��C�C=qC�C��C�C33CQ�C��C�
C{C=qCp�CC�C{CQ�C��CC�C(�Cp�C��C��C	
=C	Q�C	�C	�C	�HC
=qC
\)C
�\C
�
C{CG�Cp�C��C
=C33Cp�C�RC  C33CffC�C��C=qCp�C��C��C=qCffC��C�
C(�CffC�\C�
C�CQ�C�C��C{C=qCz�CC
=C33Cp�CC��C�Cp�C�C�HC
=C\)C��C��C  CG�C�\C�RC�HC=qCz�C��C�HC33C\)C��C�
C�CffC��C��C  CQ�C�\C��C  C(�Cp�C�RC�HC{CG�C��C��C��C=qC�\CC�C(�C\)C�RC�HC{CffC��C��C {C Q�C ��C �RC ��C!=qC!�C!C!��C"�C"ffC"�RC#  C#=qC#p�C#��C#�HC$(�C$p�C$��C$�
C%�C%\)C%��C%�C&(�C&p�C&��C&��C'�C'p�C'�C'�C((�C(\)C(��C(�
C)(�C)p�C)�RC)�C*�C*p�C*C+
=C+33C+p�C+�C,  C,Q�C,�C,�RC-  C-G�C-��C-�HC.�C.Q�C.�\C.��C/{C/ffC/�C/��C033C0p�C0�C0�HC133C1�C1�
C2{C2\)C2�\C2�
C3{C3Q�C3�\C3�
C4�C4z�C4C5
=C5Q�C5��C5�C6(�C6ffC6��C6��C7G�C7�\C7�
C833C8z�C8�RC8��C933C9p�C9�RC:  C:Q�C:��C:�C;(�C;p�C;�RC;�C<(�C<ffC<�C=  C==qC=��C=�HC>�C>ffC>�C>�HC?�C?ffC?�C?��C@G�C@��C@�HCA(�CAffCA�RCA��CB=qCBz�CB�RCC  CCG�CC�CC��CD{CD\)CD��CD�CE33CEp�CE�RCF  CFG�CF�CF��CG
=CGQ�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                     @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A�  A�JA�VA�bA�VA�bA�bA�oA�oA�{A�oA�oA�{A��A��A�{A�A�1A�A���A޾wAލPA�~�A�E�A�%Aݰ!Aݥ�Aݡ�Aݙ�A�G�A���A�ZA�7LA�(�A�VA��Aۉ7A�  AڼjAک�A��A��/A���A�|�A�bA�ƨAش9A؁A�C�A�I�A�"�A�`BA�/A�(�A�"�A� �A͓uA̛�A��A�5?AȋDA�O�A��yAƮA�`BA�ZA�\)A�&�A�jA��A�v�A��^A��hA�VA��^A�|�A�33A���A�-A��A�z�A���A��7A�|�A��A�(�A�+A��A�VA�G�A�33A�=qA�^5A�hsA�Q�A��A�~�A���A��A��A�$�A���A���A�ĜA���A��mA��mA��;A���A�dZA��HA���A��hA�dZA���A�JA��A�bA��A��/A���A��A�5?A���A��A�$�A�M�A}A{Axr�Au&�Aq�;Am��Ajz�Af$�AbZA`ffA]�TAZ��AYG�AY�AX�AW�AT�ASARE�AQ%AM`BAL�+AK��AI��AGdZAE`BACx�AAVA<��A:ȴA9�A7�
A4M�A2�+A0n�A/S�A.M�A,�9A+\)A*~�A*5?A*1'A)�mA)&�A(��A'�TA&jA%dZA%&�A$��A$ĜA$VA$-A$JA#�A#|�A#7LA"��A"�DA!�
A!S�A �9A��A(�AG�AƨAĜA�A&�A��A|�AbNAz�AVA��A33A1'AƨAXA�A�PA�jA��A��A\)A/A
�A
=qA��A1'A��A�9A�
A��A5?AXA��A7LA �HA33A�A �!A �@���@��@���@�1@�\)@�ȴ@�~�@���@��H@��@��u@�+@��@��@��#@�^@���@�E�@�@�ƨ@��@�/@���@�&�@�n�@�(�@��y@��@�1@�p�@���@�?}@㝲@�o@�@ᙚ@�9X@ߝ�@�z�@��;@�5?@��@�O�@۝�@��T@��@�$�@��T@�hs@؃@�;d@�@֟�@��@�9X@�+@���@��@�7L@д9@�(�@�ƨ@ϝ�@��@�E�@Ͳ-@�hs@�&�@�%@���@�Z@��@��;@��m@��;@˅@��@��T@�%@�9X@��;@�"�@���@�X@��`@Ý�@�"�@�J@���@���@��!@�M�@�x�@��@�%@�z�@�I�@��;@��P@���@�M�@��@��@�hs@�`B@�p�@��@���@��@�dZ@�"�@�~�@��R@�@�-@�@�V@��@���@��@���@�9X@���@���@�J@�p�@�?}@���@���@���@�Z@�t�@�33@�\)@��@���@��@���@���@�ff@�$�@��^@�X@��@���@�Q�@�1'@���@�  @�b@�b@�  @��@���@��m@��@�dZ@�S�@�K�@�+@��@��\@�^5@�-@���@��#@�@��^@��-@�X@�V@���@�bN@�9X@��@��@��@�o@���@��\@�~�@�n�@�-@���@��@�&�@��`@��9@�I�@��w@�|�@�@��+@�-@��@��#@���@���@�`B@��9@�j@���@���@��
@��P@�
=@�ȴ@�^5@�E�@��@��@��7@�G�@��@��j@�I�@�b@���@�t�@�S�@�
=@���@�~�@�ff@�V@�@���@��7@�x�@�7L@�&�@��@���@��j@���@�Q�@��@��@��F@�t�@��@�ȴ@�n�@�J@��h@�?}@��@�V@���@��D@�1'@�  @��F@�t�@�;d@��@���@�E�@�J@���@�/@�%@���@��/@��@�1'@���@��P@�33@���@��H@�M�@��#@��^@�x�@�?}@��@��@���@�z�@�I�@�b@��m@���@�C�@�"�@�@��!@��@��h@�G�@��@�%@�Ĝ@��@�I�@��@��;@��@�|�@�o@���@��R@���@�n�@�5?@��@��#@��h@��h@�X@�7L@���@�Ĝ@��j@��@�I�@�A�@�(�@��@�1@��m@��
@�ƨ@���@��y@�^5@�{@��@��#@��@�?}@��@��D@�bN@�A�@�  @K�@~ȴ@~V@}@}/@|�@|(�@{��@z�@z~�@y�@y7L@y%@x��@x�9@xA�@x  @w��@w�P@v�@vE�@u��@u?}@uV@t�@t�/@t�@tj@t1@s@r-@q��@q7L@p1'@o��@o��@oK�@n�@n�+@nE�@m�T@m@m@m�h@lI�@k�
@l1@kƨ@kS�@j~�@j�@i��@ix�@ihs@iG�@i&�@h��@g�w@g;d@f��@fv�@fV@e�T@ep�@d�@dZ@d�@c�F@cS�@b�@b^5@b�@ax�@a%@`��@_�@_|�@_K�@^$�@]@]@]��@]�@\z�@\�@\1@[�m@[S�@Z�H@Z~�@Z-@Y�^@Yhs@X�`@X  @V��@U�h@Up�@U�h@Up�@U?}@T��@T��@Tz�@T1@S�
@Sƨ@S��@SdZ@S@R�\@R^5@R-@Q�@Q��@Q�^@Q�^@Qx�@Q�@Pr�@PA�@O�@OK�@N�R@N$�@M��@MO�@LZ@KC�@Ko@J�@J�@J�H@J�!@J��@Jn�@I��@I7L@H��@H�u@G�;@G+@G
=@G;d@G
=@Fȴ@Fv�@FE�@F{@E�-@E/@D�@D�@C�m@C�F@Ct�@C33@B�H@A��@A�#@A��@AX@@Ĝ@@b@?�@?+@>ȴ@>��@>ff@=�-@=/@=�@<��@<9X@<1@;ƨ@;t�@;��@;��@;�@:��@9�@9��@9�7@9�7@9hs@97L@97L@8�9@8 �@8Q�@8�u@8 �@8 �@8 �@7��@7�@7|�@7+@7
=@6�@6�+@6E�@5�@5��@5`B@4�@49X@4(�@41@3�m@3�F@3��@3t�@3"�@2��@2��@2�@2J@1�#@1x�@1hs@1X@1&�@1%@0��@0��@0bN@0Q�@0A�@0 �@/�@/�P@/\)@/�@.��@.�@.�y@.�@.v�@.E�@.$�@-�@-�h@-�@,�j@,��@,z�@,Z@,I�@,I�@,9X@,9X@,(�@+��@+�
@+ƨ@+��@*�@*�\@*n�@*-@)��@)�^@)��@)X@)&�@(��@(A�@( �@'�@'�w@'|�@';d@&�y@&ȴ@&ff@&5?@&@%@%/@$�@$�D@$(�@$1@#�
@#��@#dZ@#33@#o@"�!@"n�@"-@"J@!��@!hs@!&�@ ��@ �`@ Ĝ@ �u@ bN@ Q�@ A�@ 1'@ b@ b@   @�;@��@l�@
=@��@��@��@��@ff@@��@@�-@�-@O�@��@��@�@��@Z@1@�m@ƨ@�@C�@@��@~�@=q@J@�#@��@&�@��@�`@r�@�@K�@�@+@;d@;d@�@��@ff@E�@5?@5?@5?@$�@��@/@�@�@�@�@�@�@V@�@�@��@��@(�@ƨ@�F@��@dZ@S�@C�@o@��@�!@^5@=q@�@J@��@��@��@x�@X@��@��@��@�u@�@A�@�@�@�@|�@\)@��@�y@��@��@E�@$�@@�T@��@��@@��@��@p�@p�@?}@/@/@�@�/@�/@�@j@I�@�@�F@t�A���A�  A���A���A���A���A���A���A���A��A���A���A�1A�JA�
=A�VA�VA�VA�oA�JA�{A�VA�bA�bA�VA�oA�VA�oA�VA�{A�VA�oA�VA�oA�bA�{A�oA�oA�{A�bA��A�bA��A�oA��A�bA�{A�oA�oA�{A�bA�{A�bA��A�bA��A�{A��A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A��A�%A�A���A���A���A�bA�JA�JA���A�VA�1A�%A�
=A�A�
=A�A�1A�%A�JA�JA��HA��mA���A���A���A���A޺^A���A޺^A�A���A޸RAީ�Aޡ�Aޣ�Aޡ�AޅA�~�A�t�A�t�A�~�A�~�Aއ+AޅA�x�A�x�A�^5A�5?A�-A�-A�-A�"�A�(�A��A��A���AݾwAݴ9Aݰ!Aݰ!Aݧ�Aݩ�Aݩ�Aݣ�Aݩ�Aݧ�Aݣ�Aݩ�Aݡ�Aݥ�Aݥ�Aݝ�Aݟ�Aݙ�Aݝ�Aݛ�Aݛ�Aݝ�Aݕ�AݓuA݃A�dZA�E�A�&�A�oA�  A���A��mA��;A���A�ƨAܾwAܬAܗ�A�jA�dZA�S�A�C�A�?}A�9XA�?}A�7LA�;dA�9XA�7LA�;dA�5?A�9XA�9XA�5?A�9XA�33A�7LA�33A�/A�/A�$�A�+A�(�A�&�A�+A�"�A�$�A��A� �A��A��A��A�oA�oA�JA�%A�A�A���A���A��A��A��A��A��yA��;A��A���A�A�A۸RA۸RAۮA۩�Aۥ�A۝�Aە�AۅA�z�A�z�A�p�A�hsA�S�A�9XA��A�VA�bA�A��A��A��A��A��A��mA��
A�ĜAھwAڼjAڲ-AڬAڴ9Aڴ9AڶFA���A���A��#A��yA���A���Aڰ!Aڝ�Aڝ�Aڝ�Aڕ�AځA�t�A�ffA�K�A�=qA�1'A�&�A��A�JA�  A���A��A��A��yA��A��mA��mA��A��mA���Aٺ^AټjA��/A��A��A��TA��mA�ƨA���A�ȴA���AٶFA٬A٩�A٥�Aٟ�Aٟ�AّhAه+A�z�A�n�A�dZA�VA�C�A�?}A�5?A��A�JA�
=A�%A�%A���A���A��A��#A���A�AؼjAؾwAغ^AؼjA���AؼjA���A�ĜAضFAغ^Aغ^AظRAغ^AجAأ�Aأ�AؓuA؋DA؋DA؅A؇+A؅A�z�A�t�A�p�A�hsA�n�A�dZA�XA�O�A�=qA�9XA�33A�(�A��A�{A�A��A��A׏\A�dZA��A��yAּjA֩�A�dZA�I�A�=qA�5?A�5?A�/A�"�A��A�{A�JA�A���A��AծA�x�A�t�A�p�A�ZA�7LA�VA���A��A��
AԼjA�hsA�1AӃA���AҲ-A��A��
A���A�ȴAиRAУ�AЁA�E�A�Aϝ�AϋDAύPAρA�bNA�`BA�XA�M�A�C�A�-A���A�ĜA·+A�\)A�K�A�33A� �A� �A��A�bA�VA�1A���A��A��HA�ĜAͲ-AͰ!A͑hA�hsA�S�A�E�A�5?A�5?A�5?A�1'A�$�A���A�l�A�33A�{A�A��A��TA���A˴9Aˉ7A�33AʑhA��A���A��A���Aɣ�A�x�A�C�A�$�A��A�{A�oA�{A�VA���A��HA�ĜAȧ�Aȝ�Aș�A�|�A�bNA�9XA�oAǺ^AǕ�A�bNA�C�A�/A�(�A�-A�&�A�JA���A���A���A��A��yA��yA��TA��/A���A���A���Aƴ9AƲ-AƬAƬAƮAƥ�AƟ�Aƛ�AƑhAƅAƁAƃA�z�A�n�A�dZA�Q�A� �A���A���AŰ!Aŗ�AŃA�p�A�p�A�/A�A��A��yA���Aĺ^Aġ�Aĕ�AąA�|�A�jA� �AÛ�A�1'A���A�ffA���A�`BA���A��A��A��HA�A���A���A�t�A�\)A�ZA�M�A�1'A��A�1A���A���A���A��A��HA��;A��;A��A���A���A���A��wA���A��A�;dA���A��A���A�hsA���A���A��+A��A�t�A�ffA�VA�E�A�bA���A��\A�z�A�bNA�I�A�;dA�33A�/A��A�bA�VA�JA�A���A��`A���A���A��^A��^A��jA��9A���A���A���A���A��hA�~�A�t�A�ffA�`BA�^5A�ZA�M�A�C�A�A�A�+A�$�A�(�A�"�A��A�  A���A���A��mA��/A�ĜA��!A���A��DA�v�A�n�A�ZA�K�A�"�A�  A��mA���A���A���A��A���A���A���A���A���A���A��uA��uA���A��DA�~�A�p�A�hsA�XA�E�A�&�A�1A��A���A��9A��A��uA�t�A�M�A�33A�oA��yA���A���A�9XA��PA��;A�^5A��A��mA��RA��+A�?}A���A���A�G�A��A���A���A��A���A��A��-A��A��uA�E�A�A�A�{A�A��
A���A��RA��A�I�A�A�A�(�A�A��;A���A���A�ȴA���A��FA�|�A�K�A��A��\A�K�A�1'A�(�A�
=A�  A��A���A��A���A�z�A�bNA�O�A�$�A��A�  A��RA���A�jA�A�A�{A��A��/A�A��9A���A��A�VA�A��A��FA���A��A�v�A�hsA�K�A�A�A�/A�VA��
A��A�t�A�O�A�&�A��A���A���A��\A�XA�;dA�+A��A�A���A��A��A��A��A���A��A��A��A���A��PA�C�A��A�bA�%A���A��TA���A�A���A��PA�x�A�bNA�=qA��A�%A��;A�t�A��A��mA��wA���A�G�A��A��7A�A�A�  A��;A��DA�/A���A�ĜA��A���A��PA���A���A��-A��9A���A���A��A�l�A�M�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                     A���A���A���A�  A�JA�VA�bA�VA�bA�bA�oA�oA�{A�oA�oA�{A��A��A�{A�A�1A�A���A޾wAލPA�~�A�E�A�%Aݰ!Aݥ�Aݡ�Aݙ�A�G�A���A�ZA�7LA�(�A�VA��Aۉ7A�  AڼjAک�A��A��/A���A�|�A�bA�ƨAش9A؁A�C�A�I�A�"�A�`BA�/A�(�A�"�A� �A͓uA̛�A��A�5?AȋDA�O�A��yAƮA�`BA�ZA�\)A�&�A�jA��A�v�A��^A��hA�VA��^A�|�A�33A���A�-A��A�z�A���A��7A�|�A��A�(�A�+A��A�VA�G�A�33A�=qA�^5A�hsA�Q�A��A�~�A���A��A��A�$�A���A���A�ĜA���A��mA��mA��;A���A�dZA��HA���A��hA�dZA���A�JA��A�bA��A��/A���A��A�5?A���A��A�$�A�M�A}A{Axr�Au&�Aq�;Am��Ajz�Af$�AbZA`ffA]�TAZ��AYG�AY�AX�AW�AT�ASARE�AQ%AM`BAL�+AK��AI��AGdZAE`BACx�AAVA<��A:ȴA9�A7�
A4M�A2�+A0n�A/S�A.M�A,�9A+\)A*~�A*5?A*1'A)�mA)&�A(��A'�TA&jA%dZA%&�A$��A$ĜA$VA$-A$JA#�A#|�A#7LA"��A"�DA!�
A!S�A �9A��A(�AG�AƨAĜA�A&�A��A|�AbNAz�AVA��A33A1'AƨAXA�A�PA�jA��A��A\)A/A
�A
=qA��A1'A��A�9A�
A��A5?AXA��A7LA �HA33A�A �!A �@���@��@���@�1@�\)@�ȴ@�~�@���@��H@��@��u@�+@��@��@��#@�^@���@�E�@�@�ƨ@��@�/@���@�&�@�n�@�(�@��y@��@�1@�p�@���@�?}@㝲@�o@�@ᙚ@�9X@ߝ�@�z�@��;@�5?@��@�O�@۝�@��T@��@�$�@��T@�hs@؃@�;d@�@֟�@��@�9X@�+@���@��@�7L@д9@�(�@�ƨ@ϝ�@��@�E�@Ͳ-@�hs@�&�@�%@���@�Z@��@��;@��m@��;@˅@��@��T@�%@�9X@��;@�"�@���@�X@��`@Ý�@�"�@�J@���@���@��!@�M�@�x�@��@�%@�z�@�I�@��;@��P@���@�M�@��@��@�hs@�`B@�p�@��@���@��@�dZ@�"�@�~�@��R@�@�-@�@�V@��@���@��@���@�9X@���@���@�J@�p�@�?}@���@���@���@�Z@�t�@�33@�\)@��@���@��@���@���@�ff@�$�@��^@�X@��@���@�Q�@�1'@���@�  @�b@�b@�  @��@���@��m@��@�dZ@�S�@�K�@�+@��@��\@�^5@�-@���@��#@�@��^@��-@�X@�V@���@�bN@�9X@��@��@��@�o@���@��\@�~�@�n�@�-@���@��@�&�@��`@��9@�I�@��w@�|�@�@��+@�-@��@��#@���@���@�`B@��9@�j@���@���@��
@��P@�
=@�ȴ@�^5@�E�@��@��@��7@�G�@��@��j@�I�@�b@���@�t�@�S�@�
=@���@�~�@�ff@�V@�@���@��7@�x�@�7L@�&�@��@���@��j@���@�Q�@��@��@��F@�t�@��@�ȴ@�n�@�J@��h@�?}@��@�V@���@��D@�1'@�  @��F@�t�@�;d@��@���@�E�@�J@���@�/@�%@���@��/@��@�1'@���@��P@�33@���@��H@�M�@��#@��^@�x�@�?}@��@��@���@�z�@�I�@�b@��m@���@�C�@�"�@�@��!@��@��h@�G�@��@�%@�Ĝ@��@�I�@��@��;@��@�|�@�o@���@��R@���@�n�@�5?@��@��#@��h@��h@�X@�7L@���@�Ĝ@��j@��@�I�@�A�@�(�@��@�1@��m@��
@�ƨ@���@��y@�^5@�{@��@��#@��@�?}@��@��D@�bN@�A�@�  @K�@~ȴ@~V@}@}/@|�@|(�@{��@z�@z~�@y�@y7L@y%@x��@x�9@xA�@x  @w��@w�P@v�@vE�@u��@u?}@uV@t�@t�/@t�@tj@t1@s@r-@q��@q7L@p1'@o��@o��@oK�@n�@n�+@nE�@m�T@m@m@m�h@lI�@k�
@l1@kƨ@kS�@j~�@j�@i��@ix�@ihs@iG�@i&�@h��@g�w@g;d@f��@fv�@fV@e�T@ep�@d�@dZ@d�@c�F@cS�@b�@b^5@b�@ax�@a%@`��@_�@_|�@_K�@^$�@]@]@]��@]�@\z�@\�@\1@[�m@[S�@Z�H@Z~�@Z-@Y�^@Yhs@X�`@X  @V��@U�h@Up�@U�h@Up�@U?}@T��@T��@Tz�@T1@S�
@Sƨ@S��@SdZ@S@R�\@R^5@R-@Q�@Q��@Q�^@Q�^@Qx�@Q�@Pr�@PA�@O�@OK�@N�R@N$�@M��@MO�@LZ@KC�@Ko@J�@J�@J�H@J�!@J��@Jn�@I��@I7L@H��@H�u@G�;@G+@G
=@G;d@G
=@Fȴ@Fv�@FE�@F{@E�-@E/@D�@D�@C�m@C�F@Ct�@C33@B�H@A��@A�#@A��@AX@@Ĝ@@b@?�@?+@>ȴ@>��@>ff@=�-@=/@=�@<��@<9X@<1@;ƨ@;t�@;��@;��@;�@:��@9�@9��@9�7@9�7@9hs@97L@97L@8�9@8 �@8Q�@8�u@8 �@8 �@8 �@7��@7�@7|�@7+@7
=@6�@6�+@6E�@5�@5��@5`B@4�@49X@4(�@41@3�m@3�F@3��@3t�@3"�@2��@2��@2�@2J@1�#@1x�@1hs@1X@1&�@1%@0��@0��@0bN@0Q�@0A�@0 �@/�@/�P@/\)@/�@.��@.�@.�y@.�@.v�@.E�@.$�@-�@-�h@-�@,�j@,��@,z�@,Z@,I�@,I�@,9X@,9X@,(�@+��@+�
@+ƨ@+��@*�@*�\@*n�@*-@)��@)�^@)��@)X@)&�@(��@(A�@( �@'�@'�w@'|�@';d@&�y@&ȴ@&ff@&5?@&@%@%/@$�@$�D@$(�@$1@#�
@#��@#dZ@#33@#o@"�!@"n�@"-@"J@!��@!hs@!&�@ ��@ �`@ Ĝ@ �u@ bN@ Q�@ A�@ 1'@ b@ b@   @�;@��@l�@
=@��@��@��@��@ff@@��@@�-@�-@O�@��@��@�@��@Z@1@�m@ƨ@�@C�@@��@~�@=q@J@�#@��@&�@��@�`@r�@�@K�@�@+@;d@;d@�@��@ff@E�@5?@5?@5?@$�@��@/@�@�@�@�@�@�@V@�@�@��@��@(�@ƨ@�F@��@dZ@S�@C�@o@��@�!@^5@=q@�@J@��@��@��@x�@X@��@��@��@�u@�@A�@�@�@�@|�@\)@��@�y@��@��@E�@$�@@�T@��@��@@��@��@p�@p�@?}@/@/@�@�/@�/@�@j@I�@�@�FG�O�A���A�  A���A���A���A���A���A���A���A��A���A���A�1A�JA�
=A�VA�VA�VA�oA�JA�{A�VA�bA�bA�VA�oA�VA�oA�VA�{A�VA�oA�VA�oA�bA�{A�oA�oA�{A�bA��A�bA��A�oA��A�bA�{A�oA�oA�{A�bA�{A�bA��A�bA��A�{A��A��A��A��A��A��A��A��A��A� �A��A��A��A��A��A��A�%A�A���A���A���A�bA�JA�JA���A�VA�1A�%A�
=A�A�
=A�A�1A�%A�JA�JA��HA��mA���A���A���A���A޺^A���A޺^A�A���A޸RAީ�Aޡ�Aޣ�Aޡ�AޅA�~�A�t�A�t�A�~�A�~�Aއ+AޅA�x�A�x�A�^5A�5?A�-A�-A�-A�"�A�(�A��A��A���AݾwAݴ9Aݰ!Aݰ!Aݧ�Aݩ�Aݩ�Aݣ�Aݩ�Aݧ�Aݣ�Aݩ�Aݡ�Aݥ�Aݥ�Aݝ�Aݟ�Aݙ�Aݝ�Aݛ�Aݛ�Aݝ�Aݕ�AݓuA݃A�dZA�E�A�&�A�oA�  A���A��mA��;A���A�ƨAܾwAܬAܗ�A�jA�dZA�S�A�C�A�?}A�9XA�?}A�7LA�;dA�9XA�7LA�;dA�5?A�9XA�9XA�5?A�9XA�33A�7LA�33A�/A�/A�$�A�+A�(�A�&�A�+A�"�A�$�A��A� �A��A��A��A�oA�oA�JA�%A�A�A���A���A��A��A��A��A��yA��;A��A���A�A�A۸RA۸RAۮA۩�Aۥ�A۝�Aە�AۅA�z�A�z�A�p�A�hsA�S�A�9XA��A�VA�bA�A��A��A��A��A��A��mA��
A�ĜAھwAڼjAڲ-AڬAڴ9Aڴ9AڶFA���A���A��#A��yA���A���Aڰ!Aڝ�Aڝ�Aڝ�Aڕ�AځA�t�A�ffA�K�A�=qA�1'A�&�A��A�JA�  A���A��A��A��yA��A��mA��mA��A��mA���Aٺ^AټjA��/A��A��A��TA��mA�ƨA���A�ȴA���AٶFA٬A٩�A٥�Aٟ�Aٟ�AّhAه+A�z�A�n�A�dZA�VA�C�A�?}A�5?A��A�JA�
=A�%A�%A���A���A��A��#A���A�AؼjAؾwAغ^AؼjA���AؼjA���A�ĜAضFAغ^Aغ^AظRAغ^AجAأ�Aأ�AؓuA؋DA؋DA؅A؇+A؅A�z�A�t�A�p�A�hsA�n�A�dZA�XA�O�A�=qA�9XA�33A�(�A��A�{A�A��A��A׏\A�dZA��A��yAּjA֩�A�dZA�I�A�=qA�5?A�5?A�/A�"�A��A�{A�JA�A���A��AծA�x�A�t�A�p�A�ZA�7LA�VA���A��A��
AԼjA�hsA�1AӃA���AҲ-A��A��
A���A�ȴAиRAУ�AЁA�E�A�Aϝ�AϋDAύPAρA�bNA�`BA�XA�M�A�C�A�-A���A�ĜA·+A�\)A�K�A�33A� �A� �A��A�bA�VA�1A���A��A��HA�ĜAͲ-AͰ!A͑hA�hsA�S�A�E�A�5?A�5?A�5?A�1'A�$�A���A�l�A�33A�{A�A��A��TA���A˴9Aˉ7A�33AʑhA��A���A��A���Aɣ�A�x�A�C�A�$�A��A�{A�oA�{A�VA���A��HA�ĜAȧ�Aȝ�Aș�A�|�A�bNA�9XA�oAǺ^AǕ�A�bNA�C�A�/A�(�A�-A�&�A�JA���A���A���A��A��yA��yA��TA��/A���A���A���Aƴ9AƲ-AƬAƬAƮAƥ�AƟ�Aƛ�AƑhAƅAƁAƃA�z�A�n�A�dZA�Q�A� �A���A���AŰ!Aŗ�AŃA�p�A�p�A�/A�A��A��yA���Aĺ^Aġ�Aĕ�AąA�|�A�jA� �AÛ�A�1'A���A�ffA���A�`BA���A��A��A��HA�A���A���A�t�A�\)A�ZA�M�A�1'A��A�1A���A���A���A��A��HA��;A��;A��A���A���A���A��wA���A��A�;dA���A��A���A�hsA���A���A��+A��A�t�A�ffA�VA�E�A�bA���A��\A�z�A�bNA�I�A�;dA�33A�/A��A�bA�VA�JA�A���A��`A���A���A��^A��^A��jA��9A���A���A���A���A��hA�~�A�t�A�ffA�`BA�^5A�ZA�M�A�C�A�A�A�+A�$�A�(�A�"�A��A�  A���A���A��mA��/A�ĜA��!A���A��DA�v�A�n�A�ZA�K�A�"�A�  A��mA���A���A���A��A���A���A���A���A���A���A��uA��uA���A��DA�~�A�p�A�hsA�XA�E�A�&�A�1A��A���A��9A��A��uA�t�A�M�A�33A�oA��yA���A���A�9XA��PA��;A�^5A��A��mA��RA��+A�?}A���A���A�G�A��A���A���A��A���A��A��-A��A��uA�E�A�A�A�{A�A��
A���A��RA��A�I�A�A�A�(�A�A��;A���A���A�ȴA���A��FA�|�A�K�A��A��\A�K�A�1'A�(�A�
=A�  A��A���A��A���A�z�A�bNA�O�A�$�A��A�  A��RA���A�jA�A�A�{A��A��/A�A��9A���A��A�VA�A��A��FA���A��A�v�A�hsA�K�A�A�A�/A�VA��
A��A�t�A�O�A�&�A��A���A���A��\A�XA�;dA�+A��A�A���A��A��A��A��A���A��A��A��A���A��PA�C�A��A�bA�%A���A��TA���A�A���A��PA�x�A�bNA�=qA��A�%A��;A�t�A��A��mA��wA���A�G�A��A��7A�A�A�  A��;A��DA�/A���A�ĜA��A���A��PA���A���A��-A��9A���A���A��A�l�A�M�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                     ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B{BB{B�B�BB@B�BuBuBuBuB�BB�BuBuBuB�B�BoBuB�BhB�B�B�BVB�B7BxB�B,�B7�B:�B<�B>�B>�B>BB8B4�B7�B@�B=�BEB[#Bd&B�iB��B��B�.B��B��B�oB��B�BB�B�TB�VBBMB'RB)�B,B2-B0�B1�B2�B5tB7LBI�BHBGBI�BT�BW?BT,BT�BTaBTaBTaBU�BS�BR BQ�BS[BVBD�B?�B:�B6zB2-B.�B*�B+�B+kB-B!bB�B�B�BB�)B̘B�B��B�B�B��B��B��Bv�BpBi�B`vBEB1�B$B(B�B
�5B
�EB
ɺB
��B
��B
x�B
[�B
Q�B
:^B
1�B
-wB
.B
kB
�B	�>B	��B	�HB	��B	��B	�_B	�	B	�;B	m�B	k�B	h>B	c�B	[�B	K^B	B�B	@B	-B	!B	�B	�B	7B	�B	1B	MB��B�B�B�`B�B��B�<B˒B��B�HB��B��B͟B�B��B�KB�B��BӏB��B�&B�
B�;B��B��B�B��B��B�|B��B��B�rB�B�`B��B�oB�5B��B�B�pB�ZB�B��B�%B	�B��B�/B	
	B	�B	 \B	 �B	"hB	#nB	�B	�B	B	B	$B	!bB	�B	%zB	#B	"�B	�B	B	�B	�B	!B	!-B	$B	&�B	+�B	@B	;dB	4nB	0�B	1�B	2�B	4�B	@�B	C�B	G�B	OBB	I�B	FtB	EB	A B	=<B	=�B	?}B	A�B	B�B	D�B	J�B	I�B	B�B	EmB	FB	>�B	:�B	4nB	7B	9XB	D�B	O�B	UgB	YB	`BB	c B	m�B	iB	e�B	_;B	f�B	p;B	l�B	w2B	v�B	t�B	p�B	r|B	|PB	�B	�AB	�AB	~�B	� B	��B	��B	��B	��B	��B	�~B	�.B	��B	�B	��B	��B	��B	�YB	�$B	�$B	�+B	�+B	��B	�IB	�'B	�bB	�4B	�:B	��B	��B	��B	�B	�XB	��B	�B	��B	��B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�kB	�B	�B	�=B	�kB	�B	�qB	�6B	�0B	��B	�_B	�6B	�CB	�}B	��B	�=B	�kB	�}B	��B	��B	�?B	�XB	��B	�wB	�HB	�HB	�B	��B	��B	�wB	��B	��B	�'B	��B	�tB	��B	˒B	��B	˒B	�0B	�BB	� B	�aB	��B	�9B	֡B	�B	��B	خB	ٴB	�)B	ݘB	�;B	�HB	��B	��B	�B	��B	�>B	�B	�KB	��B	�B	�B	�B	��B	�MB	�TB	��B	��B	��B	��B	�+B	��B	��B	�lB	��B	�xB	�PB	��B	�VB	��B	��B
  B
AB
B
�B
oB
;B
�B
�B
YB
�B
�B
�B
_B
�B
�B
+B
�B
	lB

	B
DB
�B
JB
JB
�B
�B
�B
(B
�B
:B
�B
MB
SB
SB
B
B
�B
_B
eB
�B
kB
�B
=B
kB
7B
kB
xB
�B
B
~B
�B
!B
�B
�B
 �B
 �B
 �B
 �B
 �B
 �B
!bB
!�B
!�B
!�B
!�B
"hB
#B
#�B
$�B
&B
&LB
&LB
&�B
'B
&�B
&B
%�B
&�B
&�B
'RB
($B
)*B
)�B
*0B
+kB
,qB
,�B
,�B
-B
-CB
.}B
.}B
.�B
/B
/OB
/B
0UB
0�B
0�B
1�B
1�B
1�B
2-B
2aB
2�B
2�B
33B
33B
4B
49B
4B
49B
5B
5tB
6FB
6FB
6�B
6zB
7LB
7LB
7�B
7�B
7�B
8B
8�B
8�B
8�B
8�B
8�B
9$B
9�B
9�B
9�B
:�B
:�B
;0B
<B
<�B
<�B
<�B
=qB
=�B
=�B
=�B
=�B
=�B
=�B
=�B
>B
=�B
>B
=<B
=qB
=�B
=�B
=qB
=�B
>B
>B
=�B
=�B
>B
>�B
>�B
?HB
?}B
?�B
@OB
AUB
A�B
A�B
B'B
B�B
B�B
B�B
B�B
B�B
CaB
C�B
C�B
C�B
EB
EB
EmB
E9B
E9B
EmB
FtB
GEB
GzB
GzB
H�B
G�B
GzB
H�B
H�B
J#B
JXB
J�B
J#B
I�B
I�B
J#B
JXB
K�B
L�B
L�B
K�B
L�B
NB
N<B
N�B
N�B
OB
OB
OB
N�B
OB
OBB
P�B
P�B
P�B
QB
P�B
R B
RTB
S&B
S[B
S�B
S�B
S�B
T�B
T�B
U2B
U�B
V9B
VB
W
B
VmB
VmB
W
B
VmB
W
B
WsB
WsB
W
B
V9B
VB
VB
VB
VmB
V�B
V�B
W
B
W
B
V�B
V�B
W�B
W�B
X�B
YB
ZB
ZB
Z�B
[WB
\)B
\�B
]dB
]�B
^B
^jB
^�B
^5B
^�B
_pB
_�B
`B
`B
`B
`BB
`BB
aHB
`�B
a|B
a�B
b�B
b�B
b�B
b�B
b�B
`�B
`BB
`BB
`BB
`�B
aHB
a�B
bB
bB
a�B
bNB
cTB
dZB
d�B
d�B
gmB
gmB
g�B
h
B
h
B
h
B
hsB
h>B
iDB
h�B
h�B
h�B
iB
i�B
jB
j�B
jB
j�B
kQB
k�B
lWB
l�B
l�B
l�B
l�B
l�B
m]B
l�B
lWB
lWB
l"B
k�B
lWB
l�B
m�B
m�B
ncB
n�B
m)B
l�B
l�B
m�B
m�B
m�B
n/B
n�B
n/B
n�B
poB
poB
qB
qAB
q�B
q�B
rB
r�B
r�B
r�B
r�B
sB
sMB
sMB
s�B
tB
s�B
tB
tB
s�B
s�B
tB
tB
t�B
t�B
t�B
uZB
uZB
u�B
u�B
u�B
u�B
u�B
v+B
v+B
v`B
v�B
v�B
v�B
v�B
v�B
v�B
v+B
v�B
v�B
v�B
v�B
w�B
w2B
w2B
wfB
w�B
w�B
x8B
x�B
xlB
xlB
xlB
x�B
xlB
x�B
xlB
x�B
x�B
y	B
y>B
y�B
z�B
{B
{B
{B
|B
|PB
|PB
|�B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
~�B
~�B
~�B
�B
.B
�B
�B
��B
��B
��B
�;B
�;B
�B
�oB
�oB
��B
��B
�AB
�uB
�uB
�uB
��B
�B
�{B
�B
�B
�{B
�B
�B
�B
�B
�B
��B
�B
��B
��B
��B
��B
��B
�SB
�SB
�B
�SB
��B
��B
��B
�YB
�%B
�YB
��B
��B
��B
�+B
�_B
��B
�1B
�fB
��B
��B
��B
�B
��B
�B
�7B
�7B
�B
��B
�lB
�7B
�B
��B
��B
��B
�7B
�lB
�	B
�=B
��B
��B
��B
�rB
�rB
�rB
�=B
�rB
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�B
�B
�xB
��B
��B
�B
��B
��B
��B
�PB
�VB
�VB
��B
��B
��B
�\B
�\B
�.B
�bB
�bB
�.B
�.B
�.B
��B
��B
� B
�4B
�hB
�hB
�4B
�4B
�:B
��B
��B
��B
�@B
�B
�uB
�@B
�uB
�uB
�B
�B
�FB
�{B
��B
�B
��B
��B
��B
��B
�B
��B
��B
��B
��B
��B
�B
��B
��B�B�B�B�BuB�BB�BuB�BFB�B�B�B�BBB�BoB�BB�B@BB�B�B{B�B{BB{B�BBBFBB�BB@BB�BMB�B�B@B�B@B�BFBBBoB{BoBFB�BFB.B�B@BuBBFB�B:B{BoBBFBB{BoBB_B�B�B\BB�B�BFB�B4B@BuB�BB:BuB:B:BbBB�B�B�B�B.B�B�B�BB"B(B�BVB B"BPB�B"B�B~BB�B	lB�B�B
�B�BfB�BB
�B~B	B\B�BBFB�B�BSBB1BeB=B�BB�B�BB�B=B�B�B�B�B�BBB �B�B%B*0B-�B2aB3�B5�B5?B7�B7B9$B7�B8�B7�B:^B9XB8B:�B=B:�B<jB9�B=B:�B<6B<�B;0B=qB<B<jB=�B<jB>wB=B>wB?HB=<B?�B=�B>�B>�B=�B?}B=�B?�B>�B>B?}B=B?B>B>BB@B?�B>�B@OB>wB@�B>wB>wB@�B=�B?}B=<B>wB=�B;dB=B;0B<B:*B:�B:*B6�B7�B7�B5tB6FB5?B6�B8�B6�B4�B2�B5?B5?B33B4�B49B2�B5B8�B6FB6B5B5�B6FB4�B8�B8�B9�B=�B@�BH�BMjBB�BC�B<�B<B@B>BBA B>BB;0B=B9XB:*B<6B>�B>�B@�B@OBA B?�BA�BA�BA�BAUBAUBEBMjBF?BB'BDgBY�B\�BYB\�BU�B\]B\�B^BYBZ�BYKBYB\)B[�Ba�BffBf2BhsBk�Bi�Bl�Bj�Bm�B��B�uB�GB�SB�B�YB��B��B��B�+B�7B��B��B��B��B�1B�lB��B�7B��B�B�B��B��B��B��B�B��B��B�VB��B�(B��B��B�4B��B�B��B��B�uB� B�@B��B��B��B��B��B�eB��B�CB��B�B��B�~B��B�oB��B��B�B�AB��B��B�_B��B�B�'B��B�4B�B�'B��B��B��B��B��B��B��B�IB��B�nB��B��B��B�EB��B��B�cB�;B�B�B�B�
B�iBB�/B�oB�/B��B�B�5B�B�AB�oB�B�B�rB��B�PB��B��B��B��B��B��B�"B��B 4B�]B�(B{BB�.BB�B�B%BfB�B�B�B�BB�B!-B�B�B 'B~B!bB!B�B)�B>BB-B%FB&�B)*B)�B,qB.�B+kB($B'�B(�B&�B'B+�B+kB+6B,qB)�B(XB-�B-B-�B/�B7�B0UB6B0UB2aB/OB.B/�B4�B0�B/�B0�B1�B1�B/B1[B1�B1'B/�B3�B2�B0�B2-B1�B0UB2�B2�B0�B1�B2�B1�B/�B0�B2-B2�B2aB6�B7�B6FB7B5�B4�B5?B0�B=�B7�B4�B3hB5?B7�B4B2�B3�B1�B0�B:�BIB>wB8�BN�BL0BFBZQBN�BF?BD�BHBH�BEmBI�BH�BE�BF�BK�BI�BE�BH�BF�BEBH�BG�BGzBFBFtBGBE�BE9BFBIBI�BS�BK)BIBQ�BW?Bb�BU2BQNBPHBQ�BT�BOBT,B[#BR�BV�BT,BT�BZQBY�BR BS�BW
BS�BS&BR BT�BU2BV�BU2BUgBTaBTaBS&BT�BU�BT,BR�BS�BU�BV9BS&BU2BT�BS&BR�BV�BU2BR BV9BT,BQ�BR�BT,BT�BS�BR�BTaBUgBT�BTaBS�BTaBV�BR�BT�BXBV�BS&BT�BYBS�BUgBU2BTaBT�BS[BRTBQ�BR�BS[BR�BO�BR�BRTBS�BP�BQ�BS[BU2BQ�BP�BR�BQBO�BNpBQBO�BM6BP�BN�BL�BNpBXEBa�BbNBXyBPHBN�BNpBMBQNB`Bc�BGEBI�BD�BB[BA BGB@�B>�B>wBB�BJ�B=BB[B:�B<�B8B8�B?}B6zB8RB<jB8B;dB8B5B5�B4nB0�BFB<jBDgBD�B1[B1�B0!B2�B+kB/�B0UB/OB-�B1[B.�B,B1'B+6B.}B,B+�B0�B)*B+kB,=B)�B(�B'�B,�B&�B,qB1'B,=B/�B+�B+�B+kB+�B,B(�B)�B+�B/�B-wB3�B)_B-B*0B)�B&B&LB(XB�B�BIB�B�BB�B�B�B�BBMBMB�B%�B�B�B(BhB�BB�BfB
rB�BAB�B�BB�cBAB	�B��B�ZB�B�B�B�8B�
B��B��BѷB�)B�B�RB�dB��B�gB�'B�mB�0BуB�?B�B�sB�?B�9B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                     G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                     G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022091609071120220916090711IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022092604011320220926040113QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022092604011320220926040113QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194720230210131947IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094320230210230943IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                