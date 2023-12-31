CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-04-17T07:07:00Z creation; 2023-02-10T23:09:44Z DMQC;      
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
_FillValue        G�O�       =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�       c�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�       ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ȱ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       �t   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �|   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�       �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�      H   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 5P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�      =   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � \   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�      c�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �H   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �H   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �H   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �H   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �<   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �X   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �`   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20220417070700  20230210230944  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_209                 6810_008521_209                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @��y���@��y���11  @����}V@����}V@0JG���|@0JG���|�d؋�bw��d؋�bw�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?k�@   @@  @}p�@��R@�  @�\AG�A��A ��A+�A@  A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A��A��A��B�
B(�B(�B�
B((�B0Q�B8(�B@(�BHQ�BO�
BW�
B`  Bh(�Bp(�Bx(�B�(�B�  B��
B�  B�{B��B�  B�{B�{B��B�  B�{B�{B�  B�  B��
B�  B�{B�{B�{B��B�{B�{B�  B�{B��B�  B��B��B��B��B�  B��C�C��C��C�C
  C
=C  C  C  C  C  C  C��C  C
=C   C"  C#��C%��C(  C*
=C,  C-��C0
=C2
=C3��C5��C7�HC9��C;��C>  C@{CA��CD  CF  CG�CJ  CL  CM��CP  CR
=CT
=CV  CX  CZ  C\  C^
=C`{Cb  Cd  Cf  Ch  Cj  Cl
=Cn  Co��Cq��Ct
=Cu��Cw��Cz  C{��C~  C�C���C���C�C�  C�  C�  C�C�C�  C�C�C�  C�  C�  C�  C���C�  C�  C�  C�  C���C���C�  C�  C���C���C���C���C�C�C�  C���C�  C�  C�C�  C���C�C�C�  C���C�  C�  C�  C�  C�C�C�  C�  C�C���C���C�C�C�  C�C�  C�  C�  C�  C���C���C���C�  C�C�  C���C���C�  C�
=C�
=C�C���C���C���C���C���C�C�  C���C���C�C�  C���C�  C�C�  C���C���C���C���C�  C�  C�  C�C�  C�  C�C���C���C���C�  C�  C�C�
=C�C�  C�
=C�C�  C�  C�C�C�  C���C�  C�  C���C�C�
=C�
=C�  C�  C�  C�
=C�C�  C���D � D�D}qD�qD}qD  D� D�qD� D��Dz�D  D�DD�DD� D	�D	��D
�D
� D
��D}qD�qD}qD�qD}qD  D� D  D}qD  D� D  D}qD  D��D�qDz�D  D� D  D��D  D}qD  D� D�D��D�qD}qD  D� D�qDxRD�RD� D�D}qDD��D  D��D �D }qD!  D!� D"  D"}qD#�D#� D#�qD$z�D$�RD%z�D%�qD&� D'  D'��D(�D(� D)  D)�D*�D*}qD*�qD+� D,D,}qD,�qD-��D.  D.}qD/  D/��D/�qD0� D0��D1� D2�D2� D2�qD3}qD4  D4� D4�qD5}qD5�qD6}qD7�D7��D8  D8��D8�qD9� D:�D:��D;�D;}qD;�qD<� D=  D=� D>�D>� D?  D?� D@  D@� DA�DA� DA�qDB� DC  DC�DD�DD��DD�qDE}qDE�qDF� DG  DG}qDG�qDH� DH�qDI��DJ�DJz�DJ��DK}qDK��DL� DMDM��DN  DN�DODO� DP  DP��DP��DQz�DR�DR�DS�DS� DT  DT� DU�DU�DV  DV��DW  DWz�DW�RDX}qDY  DY� DZ�DZ��D[  D[�D\D\��D]  D]��D^�D^�D^�qD_}qD`�D`��Da  Da}qDa�qDb}qDb��Dc}qDd  Dd� De�De� Df  Df��DgDg� Dh�Dh��Di  Di��Di�qDjz�Dj�qDk}qDl  Dl� Dm  Dm� Dn�Dn� Dn��Do��DpDp�DqDq�Dr  Dr}qDs  Ds��Dt�Dt� DuDu��Du�qDv� DwDw� Dx  Dx��Dy  Dy��Dz�Dz� Dz�qD{� D|  D|� D|�qD}}qD}�qD~}qD  D��D�HD�B�D���D��HD�HD�AHD�� D���D�  D�@ D�~�D�� D�  D�AHD���D�D�  D�>�D�~�D�� D�HD�@ D�~�D�� D�HD�@ D��HD��HD�  D�>�D�� D�� D�HD�B�D��HD�� D�  D�AHD���D�D�HD�AHD�� D���D��qD�>�D�~�D�� D�  D�>�D�~�D���D�  D�@ D�~�D���D�  D�>�D�~�D���D���D�>�D�}qD�� D��D�B�D��HD�� D���D�AHD��HD���D�  D�@ D�� D�� D���D�>�D�� D��HD��D�AHD�� D��HD�  D�>�D�~�D��qD�  D�AHD��HD�� D�  D�@ D�}qD���D�  D�>�D�}qD���D�HD�AHD�� D�� D�  D�>�D�~�D��qD���D�@ D�~�D���D���D�@ D�~�D��qD�  D�@ D�� D��HD�HD�@ D�� D�� D�  D�@ D�~�D��qD���D�@ D�� D���D���D�AHD�� D���D�  D�AHD�� D���D�HD�B�D�~�D���D�HD�@ D��HD���D�  D�AHD��HD�� D���D�@ D�~�D��qD���D�AHD�� D���D���D�@ D���D��HD�HD�@ D�� D���D�HD�AHD�~�D��qD���D�AHD��HD���D�  D�AHD���D�D��D�B�D���D��HD���D�@ D�� D���D�  D�>�D�}qD���D���D�AHD�� D���D�  D�@ D�~�D��qD��qD�>�D�~�D�� D�  D�AHD�� D�� D�HD�@ D�� D�� D�  D�@ D�~�D�� D�HD�@ D�~�D��HD�HD�@ D�� D��HD�  D�@ D��HD��HD���D�>�D�� D�� D�HD�AHD�}qD���D���D�@ D�� D��qD��qD�@ D��HD��HD�HD�AHD��HD�� D�  D�AHD��HD�D�HD�AHD��HD���D�  D�B�D��HD��HD�  D�>�D�� D�D�  D�>�D�~�D½qD��qD�>�DÀ DýqD���D�AHDāHD�� D���D�@ D�~�D�� D�HD�>�D�}qDƽqD���D�@ Dǀ D�� D�HD�B�DȁHD�D�HD�@ D�~�Dɾ�D�HD�@ D�~�Dʾ�D�  D�AHDˀ D��HD��D�B�D̀ D�� D��qD�>�D̀ D;�D�  D�@ D΀ Dξ�D���D�>�Dπ D��HD�  D�>�DЀ Dо�D���D�@ DсHDѾ�D�  D�AHDҀ D��HD�  D�@ Dӂ�D��HD���D�>�DԀ D�D�HD�@ DՂ�D��HD�  D�>�DցHD�� D���D�@ D׀ D�� D�  D�@ D؀ D�� D���D�>�D�~�Dپ�D���D�>�D�}qDھ�D�  D�>�Dۀ D۾�D��qD�=qD�~�Dܾ�D���D�>�D�~�D��HD�  D�@ DށHD�� D�  D�AHD߁HD߾�D�  D�@ D�� D�� D���D�@ D�~�DᾸD�  D�>�D� D��HD�HD�>�D�}qD�� D��D�AHD� D��HD�  D�@ D�~�D�� D�  D�@ D� D澸D�  D�AHD� D羸D���D�@ D� D�� D���D�>�D� D�qD���D�>�D�}qD꾸D���D�AHD� D뾸D�  D�>�D� D쾸D���D�@ D� D��HD�HD�AHD� D�� D�  D�@ D� DﾸD�  D�AHD�� D�� D�  D�>�D�~�D�� D���D�=qD�}qD�� D�  D�@ D� D��HD�HD�AHD�D��HD�  D�@ D�}qD���D�  D�AHD���D���?\)?.{?�\)?�p�?��@�@��@#�
@:�H@O\)@fff@u@��@��@�Q�@��
@�\)@�z�@�  @�=q@��@�G�@���@�\)@�(�A�\AffA(�A��Az�A�HA   A#�
A(Q�A.�RA5�A8Q�A>�RAE�AH��AMp�ATz�AXQ�A]p�Adz�Ag�Al(�Ar�\Ax��A|(�A���A��
A�{A���A��A�p�A�Q�A��A�A��A�33A�A�  A�33A�A�  A�33A�{A��A��HA�{A��A��\A��A�
=A�=qA��A�
=A��A�z�AθRA�G�A�z�A�
=A���A�(�A޸RA�Q�A�33A�A�A��A��A�\)A�G�A�z�A�
=A���A�33A�{B Q�B�B�RB(�B�BffB  B	�B
=qB�
BG�B=qB\)B��B{B
=B��B{B
=Bz�B�B33B  B��B33B (�B!G�B#
=B$Q�B%p�B&�\B(Q�B)��B*�RB+�
B-��B.�RB/�
B1p�B2�HB3�B5G�B6�RB7�B8��B:ffB;�B<z�B>{B?\)B@(�BABC
=BD  BEG�BF�HBH  BH��BJ�\BK�
BL��BN{BO�BP��BQBS\)BT��BU��BW33BX��BY��BZ�HB\Q�B]��B^�\B_�
Bap�Bb�\Bc�Be�Bf�\Bg�Bhz�Bj{Bk�Blz�Bm��Bo33Bp(�BqG�Br�HBt  Bt��Bv=qBw�Bx��By�B{
=B|��B}B~�RB�(�B��HB�p�B��B���B�\)B��
B�Q�B��B�B�ffB���B��B�=qB��RB�G�B�  B���B��B��B�z�B��B��B�=qB�
=B���B�(�B���B���B�{B���B�\)B�{B��\B��B��
B�ffB���B��B�=qB��HB�G�B��B��RB�33B��B�ffB�
=B�p�B�(�B��HB�p�B��B���B�p�B�  B�z�B�
=B��B��\B���B���B�z�B�
=B��B�=qB���B���B�{B��RB��B�(�B��RB�G�B��B���B�G�B��
B���B�\)B��
B�z�B�33B��B�z�B�
=B�B��\B��B��B�ffB�33B�B�Q�B�
=B��
B�ffB���B��B�z�B���B��B�=qB���B��B�=qB¸RB�p�B�=qB���B�G�B��BƸRB�\)B��B�z�B��B��BʸRB�G�B�B̏\B�\)B��
B�z�B�33B��B�ffB���B�B�ffB���BӅB�=qB���B�p�B�  B֣�B�p�B�  B�z�B��B�B�Q�B��HB�G�B��
B�z�B�
=B�\)B��
B�Q�B���B�
=B�p�B��B�Q�B��B��HB��BᙚB��B�(�B�Q�B��B�
=B�p�B�B��
B�=qB��B��HB�33B�\)B�B�(�B�\B���B�
=B�G�B�B�{B�z�B���B���B�G�B�B�(�B�ffB��B��HB�p�B�B�{B�=qB�RB��B�p�B��B�  B�z�B��HB��B�\)B��
B�Q�B��B���B�33B�B�{B�Q�B�z�B���B�p�B�B��
B�Q�B���B���B�33B�p�B��B�Q�B��RB��HB�33B���B�{B��\B��HB��B��B�{B�z�B��RB��B���B�{B�ffB���B�
=B���B�  B�=qB��\B���B�p�B��
C 
=C 33C p�C �C �HC  C(�C\)C��C�HC{C33CffC��C�HC(�C\)C�C�C��C33C\)C�CC  CG�Cp�C��C�
C�CG�Cz�C��C�HC(�CQ�Cp�C��C�
C�CQ�Cz�C��C�C	�C	G�C	p�C	�C	�C
�C
=qC
p�C
�C
�C
=C33Cp�C�C�HC  C(�Cz�C��CC��C=qCp�C��CC
=CG�Cp�C��C�C(�CG�Cz�C��C  C�C\)C��CC��C33C\)C�C�RC��C(�CG�Cz�C�RC��C
=C=qC�C�RC�
C{C\)C�C�C�HC�CQ�Cz�C��C�
C�CG�C\)C��C�HC  C(�C\)C��C��C�C{C\)C�C�RC�
C  C33Cz�C��CC  C=qCffC�C�RC
=C=qCffC�C�C�C(�C\)C�C��C�
C�CQ�Cz�C��CC��C=qCp�C��C��C��C(�CffC��C�
C   C (�C \)C ��C �
C!
=C!G�C!p�C!��C!�HC"�C"\)C"�\C"�RC"�HC#�C#ffC#��C#�
C$
=C$33C$ffC$�C$�C%(�C%Q�C%�C%�C%��C&33C&ffC&��C&�
C'
=C'33C'ffC'�C'��C((�C(\)C(�C(�RC(��C)=qC)z�C)�RC)��C*�C*Q�C*z�C*�RC+  C+G�C+z�C+�C+�
C,{C,G�C,�\C,�
C-
=C-33C-ffC-��C-�HC.�C.ffC.��C.�
C/
=C/=qC/z�C/C0
=C0G�C0p�C0��C0�HC1�C1\)C1��C1�HC2�C2Q�C2z�C2�RC3
=C3G�C3�C3C3�HC4{C4Q�C4�\C4�
C5{C5Q�C5z�C5�C5�HC6�C6ffC6��C6�HC7�C7Q�C7�C7�RC7�C8�C8\)C8��C8�HC9�C9\)C9�\C9C9��C:(�C:\)C:��C:��C;�C;\)C;�C;C;�C<(�C<Q�C<��C<�HC=(�C=ffC=��C=�
C>{C>G�C>�C>�RC>��C?33C?�C?C?��C@=qC@p�C@�C@�HCA{CAG�CAp�CA�RCA��CB33CBp�CB�RCB��CC33CCffCC��CC�
CD
=CDG�CDz�CD�CD�CE(�CEp�CE�CE��CF�CF\)CF�\CFCG  CG=qCGz�CG�CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                             ?k�@   @@  @}p�@��R@�  @�\AG�A��A ��A+�A@  A`��A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A��A��A��B�
B(�B(�B�
B((�B0Q�B8(�B@(�BHQ�BO�
BW�
B`  Bh(�Bp(�Bx(�B�(�B�  B��
B�  B�{B��B�  B�{B�{B��B�  B�{B�{B�  B�  B��
B�  B�{B�{B�{B��B�{B�{B�  B�{B��B�  B��B��B��B��B�  B��C�C��C��C�C
  C
=C  C  C  C  C  C  C��C  C
=C   C"  C#��C%��C(  C*
=C,  C-��C0
=C2
=C3��C5��C7�HC9��C;��C>  C@{CA��CD  CF  CG�CJ  CL  CM��CP  CR
=CT
=CV  CX  CZ  C\  C^
=C`{Cb  Cd  Cf  Ch  Cj  Cl
=Cn  Co��Cq��Ct
=Cu��Cw��Cz  C{��C~  C�C���C���C�C�  C�  C�  C�C�C�  C�C�C�  C�  C�  C�  C���C�  C�  C�  C�  C���C���C�  C�  C���C���C���C���C�C�C�  C���C�  C�  C�C�  C���C�C�C�  C���C�  C�  C�  C�  C�C�C�  C�  C�C���C���C�C�C�  C�C�  C�  C�  C�  C���C���C���C�  C�C�  C���C���C�  C�
=C�
=C�C���C���C���C���C���C�C�  C���C���C�C�  C���C�  C�C�  C���C���C���C���C�  C�  C�  C�C�  C�  C�C���C���C���C�  C�  C�C�
=C�C�  C�
=C�C�  C�  C�C�C�  C���C�  C�  C���C�C�
=C�
=C�  C�  C�  C�
=C�C�  C���D � D�D}qD�qD}qD  D� D�qD� D��Dz�D  D�DD�DD� D	�D	��D
�D
� D
��D}qD�qD}qD�qD}qD  D� D  D}qD  D� D  D}qD  D��D�qDz�D  D� D  D��D  D}qD  D� D�D��D�qD}qD  D� D�qDxRD�RD� D�D}qDD��D  D��D �D }qD!  D!� D"  D"}qD#�D#� D#�qD$z�D$�RD%z�D%�qD&� D'  D'��D(�D(� D)  D)�D*�D*}qD*�qD+� D,D,}qD,�qD-��D.  D.}qD/  D/��D/�qD0� D0��D1� D2�D2� D2�qD3}qD4  D4� D4�qD5}qD5�qD6}qD7�D7��D8  D8��D8�qD9� D:�D:��D;�D;}qD;�qD<� D=  D=� D>�D>� D?  D?� D@  D@� DA�DA� DA�qDB� DC  DC�DD�DD��DD�qDE}qDE�qDF� DG  DG}qDG�qDH� DH�qDI��DJ�DJz�DJ��DK}qDK��DL� DMDM��DN  DN�DODO� DP  DP��DP��DQz�DR�DR�DS�DS� DT  DT� DU�DU�DV  DV��DW  DWz�DW�RDX}qDY  DY� DZ�DZ��D[  D[�D\D\��D]  D]��D^�D^�D^�qD_}qD`�D`��Da  Da}qDa�qDb}qDb��Dc}qDd  Dd� De�De� Df  Df��DgDg� Dh�Dh��Di  Di��Di�qDjz�Dj�qDk}qDl  Dl� Dm  Dm� Dn�Dn� Dn��Do��DpDp�DqDq�Dr  Dr}qDs  Ds��Dt�Dt� DuDu��Du�qDv� DwDw� Dx  Dx��Dy  Dy��Dz�Dz� Dz�qD{� D|  D|� D|�qD}}qD}�qD~}qD  D��D�HD�B�D���D��HD�HD�AHD�� D���D�  D�@ D�~�D�� D�  D�AHD���D�D�  D�>�D�~�D�� D�HD�@ D�~�D�� D�HD�@ D��HD��HD�  D�>�D�� D�� D�HD�B�D��HD�� D�  D�AHD���D�D�HD�AHD�� D���D��qD�>�D�~�D�� D�  D�>�D�~�D���D�  D�@ D�~�D���D�  D�>�D�~�D���D���D�>�D�}qD�� D��D�B�D��HD�� D���D�AHD��HD���D�  D�@ D�� D�� D���D�>�D�� D��HD��D�AHD�� D��HD�  D�>�D�~�D��qD�  D�AHD��HD�� D�  D�@ D�}qD���D�  D�>�D�}qD���D�HD�AHD�� D�� D�  D�>�D�~�D��qD���D�@ D�~�D���D���D�@ D�~�D��qD�  D�@ D�� D��HD�HD�@ D�� D�� D�  D�@ D�~�D��qD���D�@ D�� D���D���D�AHD�� D���D�  D�AHD�� D���D�HD�B�D�~�D���D�HD�@ D��HD���D�  D�AHD��HD�� D���D�@ D�~�D��qD���D�AHD�� D���D���D�@ D���D��HD�HD�@ D�� D���D�HD�AHD�~�D��qD���D�AHD��HD���D�  D�AHD���D�D��D�B�D���D��HD���D�@ D�� D���D�  D�>�D�}qD���D���D�AHD�� D���D�  D�@ D�~�D��qD��qD�>�D�~�D�� D�  D�AHD�� D�� D�HD�@ D�� D�� D�  D�@ D�~�D�� D�HD�@ D�~�D��HD�HD�@ D�� D��HD�  D�@ D��HD��HD���D�>�D�� D�� D�HD�AHD�}qD���D���D�@ D�� D��qD��qD�@ D��HD��HD�HD�AHD��HD�� D�  D�AHD��HD�D�HD�AHD��HD���D�  D�B�D��HD��HD�  D�>�D�� D�D�  D�>�D�~�D½qD��qD�>�DÀ DýqD���D�AHDāHD�� D���D�@ D�~�D�� D�HD�>�D�}qDƽqD���D�@ Dǀ D�� D�HD�B�DȁHD�D�HD�@ D�~�Dɾ�D�HD�@ D�~�Dʾ�D�  D�AHDˀ D��HD��D�B�D̀ D�� D��qD�>�D̀ D;�D�  D�@ D΀ Dξ�D���D�>�Dπ D��HD�  D�>�DЀ Dо�D���D�@ DсHDѾ�D�  D�AHDҀ D��HD�  D�@ Dӂ�D��HD���D�>�DԀ D�D�HD�@ DՂ�D��HD�  D�>�DցHD�� D���D�@ D׀ D�� D�  D�@ D؀ D�� D���D�>�D�~�Dپ�D���D�>�D�}qDھ�D�  D�>�Dۀ D۾�D��qD�=qD�~�Dܾ�D���D�>�D�~�D��HD�  D�@ DށHD�� D�  D�AHD߁HD߾�D�  D�@ D�� D�� D���D�@ D�~�DᾸD�  D�>�D� D��HD�HD�>�D�}qD�� D��D�AHD� D��HD�  D�@ D�~�D�� D�  D�@ D� D澸D�  D�AHD� D羸D���D�@ D� D�� D���D�>�D� D�qD���D�>�D�}qD꾸D���D�AHD� D뾸D�  D�>�D� D쾸D���D�@ D� D��HD�HD�AHD� D�� D�  D�@ D� DﾸD�  D�AHD�� D�� D�  D�>�D�~�D�� D���D�=qD�}qD�� D�  D�@ D� D��HD�HD�AHD�D��HD�  D�@ D�}qD���D�  D�AHD���G�O�?\)?.{?�\)?�p�?��@�@��@#�
@:�H@O\)@fff@u@��@��@�Q�@��
@�\)@�z�@�  @�=q@��@�G�@���@�\)@�(�A�\AffA(�A��Az�A�HA   A#�
A(Q�A.�RA5�A8Q�A>�RAE�AH��AMp�ATz�AXQ�A]p�Adz�Ag�Al(�Ar�\Ax��A|(�A���A��
A�{A���A��A�p�A�Q�A��A�A��A�33A�A�  A�33A�A�  A�33A�{A��A��HA�{A��A��\A��A�
=A�=qA��A�
=A��A�z�AθRA�G�A�z�A�
=A���A�(�A޸RA�Q�A�33A�A�A��A��A�\)A�G�A�z�A�
=A���A�33A�{B Q�B�B�RB(�B�BffB  B	�B
=qB�
BG�B=qB\)B��B{B
=B��B{B
=Bz�B�B33B  B��B33B (�B!G�B#
=B$Q�B%p�B&�\B(Q�B)��B*�RB+�
B-��B.�RB/�
B1p�B2�HB3�B5G�B6�RB7�B8��B:ffB;�B<z�B>{B?\)B@(�BABC
=BD  BEG�BF�HBH  BH��BJ�\BK�
BL��BN{BO�BP��BQBS\)BT��BU��BW33BX��BY��BZ�HB\Q�B]��B^�\B_�
Bap�Bb�\Bc�Be�Bf�\Bg�Bhz�Bj{Bk�Blz�Bm��Bo33Bp(�BqG�Br�HBt  Bt��Bv=qBw�Bx��By�B{
=B|��B}B~�RB�(�B��HB�p�B��B���B�\)B��
B�Q�B��B�B�ffB���B��B�=qB��RB�G�B�  B���B��B��B�z�B��B��B�=qB�
=B���B�(�B���B���B�{B���B�\)B�{B��\B��B��
B�ffB���B��B�=qB��HB�G�B��B��RB�33B��B�ffB�
=B�p�B�(�B��HB�p�B��B���B�p�B�  B�z�B�
=B��B��\B���B���B�z�B�
=B��B�=qB���B���B�{B��RB��B�(�B��RB�G�B��B���B�G�B��
B���B�\)B��
B�z�B�33B��B�z�B�
=B�B��\B��B��B�ffB�33B�B�Q�B�
=B��
B�ffB���B��B�z�B���B��B�=qB���B��B�=qB¸RB�p�B�=qB���B�G�B��BƸRB�\)B��B�z�B��B��BʸRB�G�B�B̏\B�\)B��
B�z�B�33B��B�ffB���B�B�ffB���BӅB�=qB���B�p�B�  B֣�B�p�B�  B�z�B��B�B�Q�B��HB�G�B��
B�z�B�
=B�\)B��
B�Q�B���B�
=B�p�B��B�Q�B��B��HB��BᙚB��B�(�B�Q�B��B�
=B�p�B�B��
B�=qB��B��HB�33B�\)B�B�(�B�\B���B�
=B�G�B�B�{B�z�B���B���B�G�B�B�(�B�ffB��B��HB�p�B�B�{B�=qB�RB��B�p�B��B�  B�z�B��HB��B�\)B��
B�Q�B��B���B�33B�B�{B�Q�B�z�B���B�p�B�B��
B�Q�B���B���B�33B�p�B��B�Q�B��RB��HB�33B���B�{B��\B��HB��B��B�{B�z�B��RB��B���B�{B�ffB���B�
=B���B�  B�=qB��\B���B�p�B��
C 
=C 33C p�C �C �HC  C(�C\)C��C�HC{C33CffC��C�HC(�C\)C�C�C��C33C\)C�CC  CG�Cp�C��C�
C�CG�Cz�C��C�HC(�CQ�Cp�C��C�
C�CQ�Cz�C��C�C	�C	G�C	p�C	�C	�C
�C
=qC
p�C
�C
�C
=C33Cp�C�C�HC  C(�Cz�C��CC��C=qCp�C��CC
=CG�Cp�C��C�C(�CG�Cz�C��C  C�C\)C��CC��C33C\)C�C�RC��C(�CG�Cz�C�RC��C
=C=qC�C�RC�
C{C\)C�C�C�HC�CQ�Cz�C��C�
C�CG�C\)C��C�HC  C(�C\)C��C��C�C{C\)C�C�RC�
C  C33Cz�C��CC  C=qCffC�C�RC
=C=qCffC�C�C�C(�C\)C�C��C�
C�CQ�Cz�C��CC��C=qCp�C��C��C��C(�CffC��C�
C   C (�C \)C ��C �
C!
=C!G�C!p�C!��C!�HC"�C"\)C"�\C"�RC"�HC#�C#ffC#��C#�
C$
=C$33C$ffC$�C$�C%(�C%Q�C%�C%�C%��C&33C&ffC&��C&�
C'
=C'33C'ffC'�C'��C((�C(\)C(�C(�RC(��C)=qC)z�C)�RC)��C*�C*Q�C*z�C*�RC+  C+G�C+z�C+�C+�
C,{C,G�C,�\C,�
C-
=C-33C-ffC-��C-�HC.�C.ffC.��C.�
C/
=C/=qC/z�C/C0
=C0G�C0p�C0��C0�HC1�C1\)C1��C1�HC2�C2Q�C2z�C2�RC3
=C3G�C3�C3C3�HC4{C4Q�C4�\C4�
C5{C5Q�C5z�C5�C5�HC6�C6ffC6��C6�HC7�C7Q�C7�C7�RC7�C8�C8\)C8��C8�HC9�C9\)C9�\C9C9��C:(�C:\)C:��C:��C;�C;\)C;�C;C;�C<(�C<Q�C<��C<�HC=(�C=ffC=��C=�
C>{C>G�C>�C>�RC>��C?33C?�C?C?��C@=qC@p�C@�C@�HCA{CAG�CAp�CA�RCA��CB33CBp�CB�RCB��CC33CCffCC��CC�
CD
=CDG�CDz�CD�CD�CE(�CEp�CE�CE��CF�CF\)CF�\CFCG  CG=qCGz�CG�CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                             @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�C�A�S�A�S�A�Q�A�M�A�M�A�O�A�O�A�Q�A�Q�A�S�A�Q�A�O�A�G�A�A�A�?}A�=qA�$�A�$�A� �A�1A��A���Aѝ�AэPAыDAхAсAсA�~�A�|�A�t�A�dZA�G�A��A�ĜA�XA��mA�bNA�XA�z�AϼjA���AσA���A�"�A�v�A���Ȧ+A��yA�JA˰!A�l�A�5?A�-A��A˧�A�5?Aʴ9A�=qA���A�\)A�ZA�`BAœuAė�A�ZA¬A�`BA��yA��A��A�G�A��A�bA���A�v�A�1A���A��;A�dZA��/A��A�S�A��HA�S�A��A��mA�O�A��A�
=A�|�A��;A��;A�9XA���A��A��A�r�A�5?A�VA�9XA��9A��A���A��A�"�A��\A��AS�A|$�Aw�^Aq33An�AlĜAj��Ah��Af1AdZAb�A_�#A[t�AX�/AX��AW�#AS�AP�\AM��AK�AJ5?AIt�AH�HAG;dAE�
AE�AE&�AC�AA��A<r�A9"�A89XA6$�A49XA3S�A1�A1K�A0bNA/?}A.Q�A,��A+�-A+�A)�A(�+A'A%�#A$��A#�wA#�^A#��A#��A#|�A"�\A!�7A �`A ��A jA  �A7LA�A�-A?}A�HA�-A�A�uAA�`AoA5?A�wAĜA^5A��A�AȴA%A�A
��A
VA	�FA�!A�A��A�-AS�An�A1A ��@�@��@�v�@��@��H@��jA 1'A �yA �/A z�A r�A;dA��AJA-A�#A��A33A��A��A"�A&�A�A��A{AoA 9XA 1'@��H@��-@��@�t�@�v�@�@���@��7@�%@��@�7L@�I�@�E�@�@��@��@���@�@�~�@��#@�@�V@�9X@��@�"�@�V@�`B@���@��;@�+@�^5@�O�@���@�r�@�(�@��;@◍@�5?@�$�@���@�`B@��@ߍP@߅@�33@���@�=q@�X@�z�@�\)@ڏ\@�=q@��@ٙ�@؃@׾w@���@�=q@���@ՙ�@�7L@��@�t�@�@ҟ�@���@�hs@�%@�\)@ͩ�@̃@˶F@�;d@�@���@ʟ�@�M�@ə�@�X@�&�@��@��m@�dZ@�+@ư!@�~�@�p�@���@�ƨ@å�@�dZ@�33@��@�n�@���@�O�@���@�r�@�9X@�9X@�1'@���@��y@���@�ff@�=q@�@��^@���@��7@��@�7L@��`@��j@��@�9X@�
=@�V@��^@�7L@�V@���@�r�@�1@��@�K�@��+@�@��@�O�@��@�Z@�I�@��@�
=@���@��@�x�@�X@�%@��D@� �@�1@��@�K�@���@��+@�ff@�$�@��@�@�O�@��/@��@�bN@��F@�;d@��@���@��\@�ff@�-@��^@��7@��@��@��@�x�@�?}@�?}@��/@�r�@�1@���@�"�@��\@���@�&�@��u@�b@��;@�ƨ@���@�t�@�S�@��@���@�ff@�-@�{@���@�&�@�%@���@��@��D@�j@�A�@� �@�b@�  @��;@���@���@��@�M�@���@��@�`B@�?}@��@�%@��`@��j@��u@�I�@�|�@��R@��+@�M�@�-@��@�G�@���@��u@�r�@�bN@�I�@��@��@���@���@�t�@�"�@��\@�V@��@��-@�X@�%@���@��D@�(�@���@�dZ@�C�@��@���@�~�@�ff@�5?@�hs@�V@���@��/@��9@���@�I�@�1@�  @��;@�\)@�o@��R@�$�@���@��7@�G�@���@���@�j@��@�"�@���@��+@�n�@�E�@�@�?}@��@�bN@�A�@�  @���@�C�@��@��+@�-@��^@��@�hs@���@���@�A�@��@��F@��P@�dZ@���@�ȴ@��R@���@��\@�v�@��@���@���@��7@�Ĝ@��@�bN@�Q�@�I�@��@�;@+@~@}��@}?}@|��@|�/@|j@{�
@{�F@{��@{@zn�@z=q@y�#@yX@x��@xA�@x �@x �@w�;@w|�@v��@u�-@u/@t��@t�j@s�m@r��@rM�@q�@q�@p�u@o��@o|�@n��@nv�@m�@m?}@m/@m�@l��@l�j@l��@l�D@lZ@k��@k��@k33@j��@j=q@j-@i�^@ix�@h��@hQ�@h1'@h1'@g�@fȴ@f$�@e�@d�@d�@c��@a��@`�`@`�u@`bN@`A�@` �@_�@_\)@^$�@]�-@]/@\��@\Z@\1@[�@[@Z-@Z-@Z-@Z-@Z-@Z-@Z-@Y�#@Y�7@Y�@X��@X1'@W�@W�w@W��@W�P@W|�@Wl�@W\)@Vȴ@V{@Up�@T�/@T�@Tj@T(�@S�F@SC�@R��@R^5@Q��@Q�^@QX@PĜ@PbN@O�w@N��@N$�@M@M�@L��@L�/@L�/@L��@LZ@K�
@K��@K@J��@J^5@J=q@J-@J�@I��@I��@I%@H��@H�u@HA�@G�;@G�P@Gl�@G�@F�R@Fff@FE�@E��@Ep�@E�@D�D@D9X@C��@C��@Ct�@Ct�@CS�@Co@B��@BM�@A��@A��@Ahs@AX@A&�@@��@@��@@�9@@A�@?�@?��@?��@?l�@?+@>�@>�R@>�R@>��@>v�@>V@=�T@=p�@=V@<�j@<j@;ƨ@:��@:n�@:M�@:M�@:M�@:M�@:M�@:=q@:-@:-@:�@:J@9�#@9��@9hs@9�@9%@8�`@8�u@8A�@7�;@7�w@7l�@7;d@7�@6�@6$�@5p�@5/@4��@4Z@41@3�m@3�
@3ƨ@3�F@3S�@2��@2-@1X@1�@0�`@0Ĝ@0�@0r�@01'@/�@/�@/|�@/;d@/�@/
=@/
=@/
=@/
=@/
=@/
=@/
=@.��@.�@.V@-@-?}@,I�@+�m@+��@+t�@+dZ@+S�@+S�@*�!@*M�@*=q@*-@*�@)��@)��@)�7@)G�@)%@(�@(b@'�@'l�@'�@&�@&��@&ff@&E�@%�T@%��@%@%�@%/@$�/@$�/@$��@$��@$j@#��@#t�@#t�@#dZ@#C�@#33@#"�@"��@"^5@!��@!��@!��@!��@!��@!�7@!�7@!x�@!x�@!x�@!x�@!hs@!hs@!hs@!X@!7L@!&�@!&�@!%@ ��@ �@ bN@ b@�w@�@��@�@�h@V@�/@j@ƨ@C�@�@��@~�@^5@-@��@�@��@�9@A�@ �@b@  @�;@��@�@��@�P@|�@|�@\)@�@�R@{@�T@��@�h@�h@�h@�@�@p�@?}@�@��@�j@�@�@�@�@��@�D@z�@I�@9X@(�@�m@�m@�
@�
@��@��@�@dZ@S�@S�@C�@33@@�!@�\@n�@M�@-@-@J@�#@�^@�^@hs@&�@%@�`@�`@��@��@��@Ĝ@�9@�u@�@�w@��@|�@;d@
=@
=@��@�y@�@ȴ@��@�+@�+@v�@v�@V@$�@�T@�h@O�@�@��@��@�@��@j@1@ƨ@t�@S�@
�@
��@
�!@
n�@
M�@
-@
�@	�@	�^@	�7@	x�@	G�@	&�@	%A�C�A�;dA�?}A�VA�Q�A�VA�VA�Q�A�Q�A�S�A�S�A�Q�A�Q�A�Q�A�I�A�M�A�M�A�K�A�O�A�O�A�M�A�Q�A�Q�A�M�A�Q�A�Q�A�O�A�S�A�S�A�O�A�S�A�S�A�Q�A�O�A�S�A�VA�O�A�S�A�VA�Q�A�M�A�S�A�M�A�M�A�O�A�M�A�M�A�M�A�O�A�G�A�E�A�C�A�A�A�G�A�M�A�G�A�E�A�I�A�;dA�&�A�9XA�?}A�=qA�E�A�E�A�=qA�E�A�E�A�?}A�A�A�=qA� �A�$�A�$�A��A�$�A�(�A�$�A�&�A�&�A� �A� �A�+A�$�A�&�A�/A�&�A��A�+A�{A�  A��A�VA�
=A�JA�oA�{A�1A�{A�oA�1AѬAѴ9AѾwAѾwA���A��/A��
A���A���A�ĜAѓuAѓuAѕ�AёhAэPAя\Aя\AыDAщ7AэPAэPAщ7AыDAя\Aщ7AхAыDAыDAхAхAщ7AхAсAсAуAуA�~�AуAуA�~�AуAуA�~�A�|�AсA�~�A�|�AуAсA�|�A�~�A�~�A�x�A�z�A�~�A�z�A�x�A�|�A�|�A�z�A�t�A�v�A�v�A�p�A�r�A�p�A�ffA�dZA�bNA�\)A�\)A�XA�O�A�G�A�E�A�G�A�=qA�33A�5?A�5?A�"�A��A�VA�1A���A��A��`A���AЧ�AЗ�AЉ7A�v�A�ffA�VA�Q�A�M�A�=qA�5?A��A���A��A���Aϲ-Aϕ�A�jA�dZA�^5A�ZA�ZA�\)A�ZA�VA�XA�XA�S�A�^5A�ffA�l�A�p�A�r�A�~�AύPAϑhAϕ�Aϲ-AϺ^AϺ^A�ƨA���A���A���A���A���A���A���A�ȴA�AϬAϬAϗ�A�Q�A�;dA�(�A�"�A�
=A�  A��A��A��HAβ-A�S�A�=qA�(�A�A��TAͶFA͝�A�|�A�x�A�r�A�hsA�?}A�&�A��A�A��A��HA���A̸RA̴9A̩�A̋DA�hsA�S�A�O�A�+A���A��;A���A�A˸RA��A�A��A�/A�+A��A�%A��#A˸RAˁA�p�A�ZA�M�A�O�A�VA�ZAˋDA���A�-A�33A�1'A�33A�C�A�I�A�G�A�=qA�7LA�/A�"�A��A�oA�bA�%A���A��A��A��
A��A�ȴAˬA˗�Aˇ+A�l�A�\)A�XA�K�A�+A�oA��A���A�ĜAʴ9Aʲ-AʮAʝ�Aʏ\A�p�A�dZA�33A�&�A��A�JA�%A��A��TA���A�ƨAɶFAɡ�AɑhAɋDAɉ7A�t�A�jA�dZA�Q�A�C�A�;dA�7LA�9XA�7LA�33A�/A�-A�-A�"�A�1AȓuA�l�A���AǗ�A�5?A�JA�A�%A�%A�A��HAƏ\A�A�A�+A��A�
=A��A��`A��#A��A���A���Aź^Aź^AŶFAŬAŝ�A�x�A�^5A�C�A�$�A��A��A���AĮAď\A�x�A�r�A�n�A�jA�ffA�hsA�l�A�ffA�bNA�dZA�hsA�dZA�\)A�VA�S�A�M�A�I�A�G�A�K�A�=qA��A��A�1'A��A¬A�/A�|�A��A���A��A��/A��A�ZA�I�A�?}A�5?A�/A�(�A�(�A�&�A��A�
=A�A���A���A��A��mA��#A��^A���A�|�A�ffA�1'A��;A�r�A�C�A�/A��A�A�n�A�&�A�bA�A���A��A��A��A��
A��hA�x�A�ffA�XA�M�A�I�A�7LA�7LA�5?A�/A�1'A�/A�&�A�oA�JA�  A��#A���A��jA��FA��!A���A��A�ZA�oA�A��A��/A���A�ĜA��!A��PA�l�A�ZA�G�A�/A��A�1A���A��+A�Q�A�;dA��A�A�{A��hA�Q�A�{A���A�oA���A��wA���A�dZA�5?A�VA���A��FA���A�n�A�^5A�I�A�1'A��A�bA���A��HA�ƨA���A��wA��jA���A�`BA�{A���A���A�`BA�1'A��A�|�A�Q�A�7LA�VA�ĜA���A��+A�K�A�+A�JA��A��FA�`BA�C�A�7LA�(�A�
=A���A���A��7A��+A�~�A�\)A�/A�JA��A�ƨA��!A��A���A��hA�t�A�1'A���A���A�|�A�XA��A��+A�A��^A�"�A���A�ĜA�ƨA��!A��7A�dZA�=qA� �A��A�~�A�
=A��jA���A�^5A��TA�v�A�M�A�1'A�oA���A���A��FA��hA�jA�?}A�VA��yA���A��9A���A��A�jA�M�A�&�A��A�  A��A��#A��wA���A��A�r�A�p�A�l�A�ffA�ffA�VA�9XA� �A�bA��/A�jA�  A���A���A�?}A�A��mA�ƨA��wA���A�jA��A��A��/A���A���A���A�ȴA�ƨA��-A���A��hA��A�x�A�bNA���A���A�M�A���A�9XA�ȴA�C�A�oA��A�A�p�A�1'A�JA��A��/A���A��FA���A��A�n�A�S�A�C�A�9XA�/A�&�A�$�A� �A�"�A� �A��A�VA�  A��A��HA���A��RA�`BA�{A��;A��wA�jA��FA�bA���A�ȴA���A�t�A�M�A�33A�$�A�JA��;A���A�dZA�$�A���A���A�n�A�M�A�;dA�1'A�(�A�oA��FA�XA� �A��A��A��A�A�A�VA��/A�bNA��9A�33A��HA�9XA�ȴA��A�VA�O�A�?}A�I�A�;dA�33A�7LA�33A�5?A�33A�(�A�&�A�oA�%A��HA��DA�`BA�-A��HA���A��wA���A��7A�l�A�ffA�ZA�K�A�K�A�I�A�G�A�A�A��A���A�ȴA���A���A�~�A�XA�1'A�
=A��`G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                             A�C�A�S�A�S�A�Q�A�M�A�M�A�O�A�O�A�Q�A�Q�A�S�A�Q�A�O�A�G�A�A�A�?}A�=qA�$�A�$�A� �A�1A��A���Aѝ�AэPAыDAхAсAсA�~�A�|�A�t�A�dZA�G�A��A�ĜA�XA��mA�bNA�XA�z�AϼjA���AσA���A�"�A�v�A���Ȧ+A��yA�JA˰!A�l�A�5?A�-A��A˧�A�5?Aʴ9A�=qA���A�\)A�ZA�`BAœuAė�A�ZA¬A�`BA��yA��A��A�G�A��A�bA���A�v�A�1A���A��;A�dZA��/A��A�S�A��HA�S�A��A��mA�O�A��A�
=A�|�A��;A��;A�9XA���A��A��A�r�A�5?A�VA�9XA��9A��A���A��A�"�A��\A��AS�A|$�Aw�^Aq33An�AlĜAj��Ah��Af1AdZAb�A_�#A[t�AX�/AX��AW�#AS�AP�\AM��AK�AJ5?AIt�AH�HAG;dAE�
AE�AE&�AC�AA��A<r�A9"�A89XA6$�A49XA3S�A1�A1K�A0bNA/?}A.Q�A,��A+�-A+�A)�A(�+A'A%�#A$��A#�wA#�^A#��A#��A#|�A"�\A!�7A �`A ��A jA  �A7LA�A�-A?}A�HA�-A�A�uAA�`AoA5?A�wAĜA^5A��A�AȴA%A�A
��A
VA	�FA�!A�A��A�-AS�An�A1A ��@�@��@�v�@��@��H@��jA 1'A �yA �/A z�A r�A;dA��AJA-A�#A��A33A��A��A"�A&�A�A��A{AoA 9XA 1'@��H@��-@��@�t�@�v�@�@���@��7@�%@��@�7L@�I�@�E�@�@��@��@���@�@�~�@��#@�@�V@�9X@��@�"�@�V@�`B@���@��;@�+@�^5@�O�@���@�r�@�(�@��;@◍@�5?@�$�@���@�`B@��@ߍP@߅@�33@���@�=q@�X@�z�@�\)@ڏ\@�=q@��@ٙ�@؃@׾w@���@�=q@���@ՙ�@�7L@��@�t�@�@ҟ�@���@�hs@�%@�\)@ͩ�@̃@˶F@�;d@�@���@ʟ�@�M�@ə�@�X@�&�@��@��m@�dZ@�+@ư!@�~�@�p�@���@�ƨ@å�@�dZ@�33@��@�n�@���@�O�@���@�r�@�9X@�9X@�1'@���@��y@���@�ff@�=q@�@��^@���@��7@��@�7L@��`@��j@��@�9X@�
=@�V@��^@�7L@�V@���@�r�@�1@��@�K�@��+@�@��@�O�@��@�Z@�I�@��@�
=@���@��@�x�@�X@�%@��D@� �@�1@��@�K�@���@��+@�ff@�$�@��@�@�O�@��/@��@�bN@��F@�;d@��@���@��\@�ff@�-@��^@��7@��@��@��@�x�@�?}@�?}@��/@�r�@�1@���@�"�@��\@���@�&�@��u@�b@��;@�ƨ@���@�t�@�S�@��@���@�ff@�-@�{@���@�&�@�%@���@��@��D@�j@�A�@� �@�b@�  @��;@���@���@��@�M�@���@��@�`B@�?}@��@�%@��`@��j@��u@�I�@�|�@��R@��+@�M�@�-@��@�G�@���@��u@�r�@�bN@�I�@��@��@���@���@�t�@�"�@��\@�V@��@��-@�X@�%@���@��D@�(�@���@�dZ@�C�@��@���@�~�@�ff@�5?@�hs@�V@���@��/@��9@���@�I�@�1@�  @��;@�\)@�o@��R@�$�@���@��7@�G�@���@���@�j@��@�"�@���@��+@�n�@�E�@�@�?}@��@�bN@�A�@�  @���@�C�@��@��+@�-@��^@��@�hs@���@���@�A�@��@��F@��P@�dZ@���@�ȴ@��R@���@��\@�v�@��@���@���@��7@�Ĝ@��@�bN@�Q�@�I�@��@�;@+@~@}��@}?}@|��@|�/@|j@{�
@{�F@{��@{@zn�@z=q@y�#@yX@x��@xA�@x �@x �@w�;@w|�@v��@u�-@u/@t��@t�j@s�m@r��@rM�@q�@q�@p�u@o��@o|�@n��@nv�@m�@m?}@m/@m�@l��@l�j@l��@l�D@lZ@k��@k��@k33@j��@j=q@j-@i�^@ix�@h��@hQ�@h1'@h1'@g�@fȴ@f$�@e�@d�@d�@c��@a��@`�`@`�u@`bN@`A�@` �@_�@_\)@^$�@]�-@]/@\��@\Z@\1@[�@[@Z-@Z-@Z-@Z-@Z-@Z-@Z-@Y�#@Y�7@Y�@X��@X1'@W�@W�w@W��@W�P@W|�@Wl�@W\)@Vȴ@V{@Up�@T�/@T�@Tj@T(�@S�F@SC�@R��@R^5@Q��@Q�^@QX@PĜ@PbN@O�w@N��@N$�@M@M�@L��@L�/@L�/@L��@LZ@K�
@K��@K@J��@J^5@J=q@J-@J�@I��@I��@I%@H��@H�u@HA�@G�;@G�P@Gl�@G�@F�R@Fff@FE�@E��@Ep�@E�@D�D@D9X@C��@C��@Ct�@Ct�@CS�@Co@B��@BM�@A��@A��@Ahs@AX@A&�@@��@@��@@�9@@A�@?�@?��@?��@?l�@?+@>�@>�R@>�R@>��@>v�@>V@=�T@=p�@=V@<�j@<j@;ƨ@:��@:n�@:M�@:M�@:M�@:M�@:M�@:=q@:-@:-@:�@:J@9�#@9��@9hs@9�@9%@8�`@8�u@8A�@7�;@7�w@7l�@7;d@7�@6�@6$�@5p�@5/@4��@4Z@41@3�m@3�
@3ƨ@3�F@3S�@2��@2-@1X@1�@0�`@0Ĝ@0�@0r�@01'@/�@/�@/|�@/;d@/�@/
=@/
=@/
=@/
=@/
=@/
=@/
=@.��@.�@.V@-@-?}@,I�@+�m@+��@+t�@+dZ@+S�@+S�@*�!@*M�@*=q@*-@*�@)��@)��@)�7@)G�@)%@(�@(b@'�@'l�@'�@&�@&��@&ff@&E�@%�T@%��@%@%�@%/@$�/@$�/@$��@$��@$j@#��@#t�@#t�@#dZ@#C�@#33@#"�@"��@"^5@!��@!��@!��@!��@!��@!�7@!�7@!x�@!x�@!x�@!x�@!hs@!hs@!hs@!X@!7L@!&�@!&�@!%@ ��@ �@ bN@ b@�w@�@��@�@�h@V@�/@j@ƨ@C�@�@��@~�@^5@-@��@�@��@�9@A�@ �@b@  @�;@��@�@��@�P@|�@|�@\)@�@�R@{@�T@��@�h@�h@�h@�@�@p�@?}@�@��@�j@�@�@�@�@��@�D@z�@I�@9X@(�@�m@�m@�
@�
@��@��@�@dZ@S�@S�@C�@33@@�!@�\@n�@M�@-@-@J@�#@�^@�^@hs@&�@%@�`@�`@��@��@��@Ĝ@�9@�u@�@�w@��@|�@;d@
=@
=@��@�y@�@ȴ@��@�+@�+@v�@v�@V@$�@�T@�h@O�@�@��@��@�@��@j@1@ƨ@t�@S�@
�@
��@
�!@
n�@
M�@
-@
�@	�@	�^@	�7@	x�@	G�@	&�G�O�A�C�A�;dA�?}A�VA�Q�A�VA�VA�Q�A�Q�A�S�A�S�A�Q�A�Q�A�Q�A�I�A�M�A�M�A�K�A�O�A�O�A�M�A�Q�A�Q�A�M�A�Q�A�Q�A�O�A�S�A�S�A�O�A�S�A�S�A�Q�A�O�A�S�A�VA�O�A�S�A�VA�Q�A�M�A�S�A�M�A�M�A�O�A�M�A�M�A�M�A�O�A�G�A�E�A�C�A�A�A�G�A�M�A�G�A�E�A�I�A�;dA�&�A�9XA�?}A�=qA�E�A�E�A�=qA�E�A�E�A�?}A�A�A�=qA� �A�$�A�$�A��A�$�A�(�A�$�A�&�A�&�A� �A� �A�+A�$�A�&�A�/A�&�A��A�+A�{A�  A��A�VA�
=A�JA�oA�{A�1A�{A�oA�1AѬAѴ9AѾwAѾwA���A��/A��
A���A���A�ĜAѓuAѓuAѕ�AёhAэPAя\Aя\AыDAщ7AэPAэPAщ7AыDAя\Aщ7AхAыDAыDAхAхAщ7AхAсAсAуAуA�~�AуAуA�~�AуAуA�~�A�|�AсA�~�A�|�AуAсA�|�A�~�A�~�A�x�A�z�A�~�A�z�A�x�A�|�A�|�A�z�A�t�A�v�A�v�A�p�A�r�A�p�A�ffA�dZA�bNA�\)A�\)A�XA�O�A�G�A�E�A�G�A�=qA�33A�5?A�5?A�"�A��A�VA�1A���A��A��`A���AЧ�AЗ�AЉ7A�v�A�ffA�VA�Q�A�M�A�=qA�5?A��A���A��A���Aϲ-Aϕ�A�jA�dZA�^5A�ZA�ZA�\)A�ZA�VA�XA�XA�S�A�^5A�ffA�l�A�p�A�r�A�~�AύPAϑhAϕ�Aϲ-AϺ^AϺ^A�ƨA���A���A���A���A���A���A���A�ȴA�AϬAϬAϗ�A�Q�A�;dA�(�A�"�A�
=A�  A��A��A��HAβ-A�S�A�=qA�(�A�A��TAͶFA͝�A�|�A�x�A�r�A�hsA�?}A�&�A��A�A��A��HA���A̸RA̴9A̩�A̋DA�hsA�S�A�O�A�+A���A��;A���A�A˸RA��A�A��A�/A�+A��A�%A��#A˸RAˁA�p�A�ZA�M�A�O�A�VA�ZAˋDA���A�-A�33A�1'A�33A�C�A�I�A�G�A�=qA�7LA�/A�"�A��A�oA�bA�%A���A��A��A��
A��A�ȴAˬA˗�Aˇ+A�l�A�\)A�XA�K�A�+A�oA��A���A�ĜAʴ9Aʲ-AʮAʝ�Aʏ\A�p�A�dZA�33A�&�A��A�JA�%A��A��TA���A�ƨAɶFAɡ�AɑhAɋDAɉ7A�t�A�jA�dZA�Q�A�C�A�;dA�7LA�9XA�7LA�33A�/A�-A�-A�"�A�1AȓuA�l�A���AǗ�A�5?A�JA�A�%A�%A�A��HAƏ\A�A�A�+A��A�
=A��A��`A��#A��A���A���Aź^Aź^AŶFAŬAŝ�A�x�A�^5A�C�A�$�A��A��A���AĮAď\A�x�A�r�A�n�A�jA�ffA�hsA�l�A�ffA�bNA�dZA�hsA�dZA�\)A�VA�S�A�M�A�I�A�G�A�K�A�=qA��A��A�1'A��A¬A�/A�|�A��A���A��A��/A��A�ZA�I�A�?}A�5?A�/A�(�A�(�A�&�A��A�
=A�A���A���A��A��mA��#A��^A���A�|�A�ffA�1'A��;A�r�A�C�A�/A��A�A�n�A�&�A�bA�A���A��A��A��A��
A��hA�x�A�ffA�XA�M�A�I�A�7LA�7LA�5?A�/A�1'A�/A�&�A�oA�JA�  A��#A���A��jA��FA��!A���A��A�ZA�oA�A��A��/A���A�ĜA��!A��PA�l�A�ZA�G�A�/A��A�1A���A��+A�Q�A�;dA��A�A�{A��hA�Q�A�{A���A�oA���A��wA���A�dZA�5?A�VA���A��FA���A�n�A�^5A�I�A�1'A��A�bA���A��HA�ƨA���A��wA��jA���A�`BA�{A���A���A�`BA�1'A��A�|�A�Q�A�7LA�VA�ĜA���A��+A�K�A�+A�JA��A��FA�`BA�C�A�7LA�(�A�
=A���A���A��7A��+A�~�A�\)A�/A�JA��A�ƨA��!A��A���A��hA�t�A�1'A���A���A�|�A�XA��A��+A�A��^A�"�A���A�ĜA�ƨA��!A��7A�dZA�=qA� �A��A�~�A�
=A��jA���A�^5A��TA�v�A�M�A�1'A�oA���A���A��FA��hA�jA�?}A�VA��yA���A��9A���A��A�jA�M�A�&�A��A�  A��A��#A��wA���A��A�r�A�p�A�l�A�ffA�ffA�VA�9XA� �A�bA��/A�jA�  A���A���A�?}A�A��mA�ƨA��wA���A�jA��A��A��/A���A���A���A�ȴA�ƨA��-A���A��hA��A�x�A�bNA���A���A�M�A���A�9XA�ȴA�C�A�oA��A�A�p�A�1'A�JA��A��/A���A��FA���A��A�n�A�S�A�C�A�9XA�/A�&�A�$�A� �A�"�A� �A��A�VA�  A��A��HA���A��RA�`BA�{A��;A��wA�jA��FA�bA���A�ȴA���A�t�A�M�A�33A�$�A�JA��;A���A�dZA�$�A���A���A�n�A�M�A�;dA�1'A�(�A�oA��FA�XA� �A��A��A��A�A�A�VA��/A�bNA��9A�33A��HA�9XA�ȴA��A�VA�O�A�?}A�I�A�;dA�33A�7LA�33A�5?A�33A�(�A�&�A�oA�%A��HA��DA�`BA�-A��HA���A��wA���A��7A�l�A�ffA�ZA�K�A�K�A�I�A�G�A�A�A��A���A�ȴA���A���A�~�A�XA�1'A�
=A��`G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                             ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
1B
�B
1B
1B
fB
fB
fB
�B
fB
fB
�B
�B
1B
�B
�B
�B
�B
�B
�B
_B
B
�B
�B
�B
�B
�B
B
�B
�B
B
{B
�B
GB
�B
�B
�B
B
'B
=�B
_�B
cB
��B
�UB@BA�BJ�BI�BD�BY�B\)BN�B�+B��B�B�!B��B�B�EB��B՛B�B�B �BfB%BeB$@B%FB1�B7�B8�B8�B8�B8RBIBGEB@BAUB49B-�B&�B!�B_B�BSBoB�B��B�TBרB�aB��B��B�IB�7B� BkQBU�B*eB
�PB
��B
��B
�WB
ʌB
��B
�bB
��B
o�B
Y�B
;�B
�B
�B	�	B	��B	��B	��B	��B	�qB	��B	�:B	��B	��B	��B	r�B	e�B	Q�B	M6B	C�B	>wB	<jB	7LB	.�B	,B	(�B	#nB	qB	�B	�B�DB�8B�B�)B�
B�B�TB�B��BרB� B�B��B��B�3B�?B�aB�B�XB�dB�<BуB��B�9B�yB�B�KB��B��BخB��B�EB�EB�jB�`B��B��B�B�lB�B�B��B�8B�xB��B��B�"B	 �B		�B	�B	uB	.B	DB	 �B�]B�B��B��B�KB�`B��B��B	�B	;B	+B	9$B	J#B	L�B	LdB	S&B	hsB	��B	�4B	��B	��B	��B	�wB	��B	��B	�qB	�}B	�HB	�B	�OB	�B	�3B	��B	�B	��B	�B	�B	ƨB	�?B	ŢB	�mB	�B	��B	�HB	�OB	�}B	�qB	�wB	�BB	�wB	�3B	��B	�gB	ÖB	�mB	�B	��B	�B	��B	��B	�KB	ǮB	�EB	��B	�^B	�0B	�dB	��B	͟B	�B	�<B	�<B	��B	�B	бB	ѷB	�B	҉B	��B	�TB	ӏB	�aB	��B	�gB	�B	��B	֡B	�B	�B	��B	�WB	�WB	یB	ܒB	��B	��B	�vB	�B	�|B	�HB	�B	�B	�B	��B	�KB	��B	�B	�B	�QB	�B	�cB	��B	� B	�5B	��B	��B	�B	�B	�GB	��B	��B	��B	�ZB	�ZB	��B	�TB	��B	��B	��B	�+B	��B	��B	��B	��B	��B	�B	�DB	��B	�JB	��B	��B	��B	��B	��B	��B	�]B	�(B	��B	�]B
B
B
�B
SB
�B
SB
�B
+B
+B
�B
�B

	B
	�B
	7B

�B
	�B
	�B

rB
�B
�B
"B
�B
�B
�B
�B
\B
�B
�B
 B
hB
hB
hB
oB
B
B
@B
B
B
uB
�B
B
�B
MB
�B
B
�B
�B
YB
$B
YB
$B
YB
�B
_B
=B
�B
�B
IB
�B
OB
�B
 �B
 �B
!�B
!bB
!�B
!�B
!�B
!�B
#:B
#:B
#�B
#�B
#nB
%zB
&B
&B
&�B
&�B
&�B
&�B
'RB
'�B
'�B
'�B
'�B
'RB
'�B
(�B
*0B
*�B
*eB
*�B
*�B
*�B
*�B
+B
+B
*�B
+�B
-B
-wB
-wB
-�B
-�B
.B
/�B
/�B
0UB
0!B
/�B
0!B
0UB
0UB
0UB
0�B
1'B
1�B
3hB
33B
4B
4nB
4�B
5?B
5tB
5�B
6�B
7�B
7�B
7�B
8�B
8�B
9$B
8�B
8�B
:�B
:�B
;dB
;dB
<6B
;�B
=B
=<B
=B
=B
=�B
>B
>�B
>�B
?B
>�B
?B
>�B
>wB
>BB
>�B
?�B
?�B
?}B
?HB
?B
?HB
?�B
@�B
A�B
A�B
B[B
B�B
CaB
C�B
C�B
D�B
E�B
EmB
E9B
E9B
E�B
E�B
F�B
GEB
G�B
HB
IRB
IB
I�B
J�B
J�B
K^B
L0B
L�B
L�B
MB
OB
N�B
N�B
N�B
N�B
N�B
N�B
O�B
O�B
O�B
O�B
O�B
O�B
PHB
P}B
PHB
PHB
P�B
QB
QNB
Q�B
R B
R�B
S[B
S&B
S&B
S&B
S�B
T,B
UgB
U�B
U�B
U�B
W?B
X�B
X�B
YKB
ZB
Z�B
[WB
[�B
\)B
\)B
\�B
]/B
]/B
]/B
]/B
]�B
]�B
]�B
]�B
^B
^B
^jB
^�B
_B
^�B
_;B
_;B
`B
`BB
`B
_�B
`B
a|B
aB
a�B
a�B
a�B
c B
dZB
d�B
d�B
d�B
dZB
d�B
dZB
d�B
e�B
e�B
e�B
ffB
ffB
ffB
f�B
g�B
g�B
g�B
g�B
g�B
g�B
g�B
gmB
g�B
g�B
h>B
h�B
iB
iB
iyB
iyB
iyB
iyB
iyB
iyB
jKB
j�B
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
n�B
ncB
o5B
p�B
qAB
qB
qvB
rB
rB
q�B
q�B
r�B
r|B
r�B
sMB
sMB
s�B
s�B
sMB
sMB
sMB
s�B
s�B
tB
tB
t�B
t�B
t�B
t�B
t�B
t�B
u%B
u%B
u�B
u�B
u�B
v�B
v�B
v�B
w2B
v�B
v�B
v�B
wfB
xB
x8B
x�B
y>B
y	B
y	B
yrB
y�B
y�B
y�B
zDB
zxB
zDB
zxB
zxB
z�B
{B
{B
{B
{B
{B
{B
{B
{�B
|PB
|B
|�B
}"B
~]B
~(B
~]B
~(B
~]B
~]B
~]B
~]B
~�B
~]B
~]B
~]B
~�B
~�B
~�B
.B
.B
.B
�B
�B
�4B
� B
�iB
�iB
��B
�iB
��B
�B
��B
�uB
�uB
��B
��B
��B
��B
��B
�B
�{B
�B
��B
��B
��B
�B
��B
�B
��B
��B
��B
�YB
�YB
��B
��B
��B
��B
��B
��B
��B
�YB
�YB
��B
��B
�_B
��B
��B
��B
��B
�B
�B
�B
�B
�	B
�	B
�	B
�	B
�=B
�=B
�=B
�rB
��B
��B
�DB
��B
��B
�JB
�JB
��B
��B
�B
��B
��B
��B
�PB
��B
��B
�"B
�"B
��B
�VB
��B
�(B
�\B
�\B
��B
�\B
��B
�\B
��B
�.B
��B
��B
��B
��B
� B
� B
� B
� B
�4B
� B
� B
�4B
�4B
� B
�4B
�hB
�hB
�hB
��B
��B
�B
�:B
�:B
�:B
�@B
�uB
�FB
�FB
�B
�B
��B
��B
��B
�$B
��B
��B
��B
��B
��B
�eB
��B
��B
�kB
��B
��B
�kB
��B
��B
�	B
�	B
�=B
�=B
�	B
�=B
�qB
�B
��B
�B
�~B
�IB
�IB
�IB
�IB
�~B
�IB
��B
��B
�B
�OB
�OB
�OB
�B
�B
�OB
�OB
�OB
��B
��B
��B
�!B
��B
�!B
��B
�VB
�VB
�VB
��B
��B
��B
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
�-B
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
��B
�B
��B
�B
�:B
��B
��B
��B
��B
��B
��B
��B
�@B
�@B
�@B
�@B
�@B
�@B
�tB
�B
�FB
��B
��B
�B
��B
�LB
��B
��B
�B
��B
��B
��B
��B
��B
��B
�*B
�*B
�*B
�_B
�_B
��B
��B
��B
��B
��B
��B
_B

=B
	B
GB
	B
+B
+B
	B
�B
+B
_B
�B
�B
�B
	�B
_B
fB
	B
�B
�B
	B
�B
�B
	lB
+B
1B
	lB
_B
�B
	�B
�B
_B
	B
�B
�B
�B
	B
�B
_B
�B
	�B
_B
	�B
1B
fB
	7B
	lB
�B
_B
�B
	B
�B
1B
_B
�B
1B
1B
�B
	�B
_B
B
�B
�B
�B
+B

	B
MB
�B
	7B
+B
�B
�B
YB
fB
�B
�B
YB
�B
+B
YB
�B
_B
B
�B
fB
�B
�B
fB
�B
~B
�B

	B
{B
YB
B
�B
B
�B
�B
%B
�B
GB
�B
B
oB
�B
MB
�B
�B
�B
�B
(B
B
B
�B
%B
�B
MB
SB
�B
�B
MB
�B
�B
{B
�B
%B
B
�B
�B
�B
�B
�B
�B
�B
�B
MB
�B
B
B
�B
{B
{B
�B
�B
GB
�B
SB
GB
B
SB
MB
�B
B
�B
B
�B
�B
B
GB
B
SB
{B
�B
B
B
GB
�B
B
AB
�B
B
�B
B
�B
GB
 �B
�B
B
B
�B
�B
�B
�B
�B
�B
�B
B
�B
B
�B
_B

	B
	�B
�B
�B
�B

=B

�B
�B
 B
(B
@B
�B
qB
VB
B
B
B
�B
�B
�B
#�B
$B
%�B
+�B
0�B
5tB
5B
9�B
>BB
>wB
>�B
H�B
IB
N�B
`B
h�B
gmB
poB
sMB
zDB
|B
}�B
��B
�YB
�7B
��B
��B
�B
�B
��B
�kB
��B
�!B
��B
�B
��B
�)B
ӏB
�DB
��BAB�B#�B+B6B=�BA�B@�B@�BC�BK)BHKBJ�BL0BK�BJ#BJ�BL0BJ�BK)BK�BIRBF?BFtBH�BE�BFtB>�BG�BA�BF�BFBW?Bd�Bi�Bg�BiyBa|B_;BY�BO�BPHBK)BI�BI�BI�B`BBffB��B��B��B��B�eB��B��B�nB�B��B��B��B�B��B��B�FB��B�0B��B��B�CB��B��B��B�zB��B��B�B�*B��B�}B��B��B��B�dB�6B�6B��B��B�'B��B��B��B�)B��B�pBΥBбB��B��B�[B��B��B� B�mB֡B�9B��B��B��B��B��B�,B�aB�gB��B��B�&B��B�B�vB�B�B�lB��B�oB�B��B��B�%B��BuB�JB��B�.B;B��B�cB�PB�(B �B��B��B��B 4B�B_BoB{B�BB	�B�B
�B
=B
	B�B�B1B�B_B%B�B�B�BSB�B�B�B�B_B�B�B{B�B�B�B�B{BB!bB(�B#nB �B�B!B)*B(XB$tB#nB#�B$�B$�B"�B"4B#�B&�B%B#nB#B$�B$�B%zB(�B&B($B($B/OB1�B5B.�B.}B1�B<6B<6B8�B7�B5�B7�B49B4B4�B9$B?�B:�B9$B7�B9�B9XB:�B7�B8RB9$B7�B7LB7�B:*B8�B:�B:�B8B7�B7�B7�B7�B8B>BB6FB7�B7�B8RB7�B7�B7�B:�B8�B8�B6FB6zB6B6B;�B<�B:*B6zB5�B?}BM�BN�BF�BH�BU�BR BGBC�BGEBL�BHKBGzBJ�BG�BC�BG�BB'BB�BB'BB'BA�B@�BA�B?�B=�B;�B:*B>wBA�BHKBP}BS�B<6B9�BB�B?B49B1[B4B7LB3�B4B6zB1�B1�B0�B7B6B.}B,qB.�B/�B,qB8�B)�B(XB(�B+�B.�B)_B+B$@B%FB#nB!�B#nB$@B,=B'�BB �B�B"�B6FB!�B�B'�B�BYB�B1B�B�BSB�B�B!�B$�B�B�BB!�B#nB�B�BkB�B�BeB�BkB1B�B$B�BoBBuB�BFB�B\B"B"B�B�B�B?�BMB�BB�B{BSBSBoB �B�B�B�B��B��BB�2B��B�%B�QB�B��B�B�B��B�jB�dBیB��B�KB�B�
B�KB��B҉B՛BߤB�dB��B�aB�gB�dB�B�hB�-B�?B��B��B��B��B�tB�B��B�@B��B�B�hB�B�~B�~B��B��B�B�xB�CB��B��B�OB�!B�xB��B�OB�tB��B�{B�SB��B�-B�hB�B��B��B�B��B~(B{B}�B�B}VByrBy>Bt�Bl"Bm�Bk�Be�Be,Bb�Be,BiyBd�BU�BWsBxBR�BDgB<6B@BF�B<�B4B/�B$�B%�B,B	7B;BB
�(B
�(B
�PB
�B
�DB
�B
�rB
��B
�TB
��B
��B;B
�]B
�MB
�TB
�B
�B
�B
�B
�B
��B
�B
�`B
��B
�B
ߤB
�/B
�5B
ݘB
��B
��B
�EB
ӏB
�B
�TB
�HB
��B
�)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                             G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                             G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202302102309422023021023094220230210230942202302102309422023021023094220230210230942SI  SI  ARFMARFM                                                                                                                                                2022041707070020220417070700IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022042702011820220427020118QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022042702011820220427020118QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2023021013194520230210131945IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023021023094220230210230942IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                