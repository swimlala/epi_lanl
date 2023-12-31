CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2019-11-30T02:25:15Z creation; 2023-04-26T19:14:29Z DMQC;      
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20191130022515  20230426191429  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               E   EAA  AOAO7316_008644_069                 7316_008644_069                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @���΅3�@���΅3�11  @������@������@* ��:э@* ��:э�c���g�c���g11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u@   @B�\@�G�@�G�@\@޸R@�(�A\)A ��A,(�A@  A`  A\)A�  A�  A�  A�  A�  A�  A�A��B  B  B  B (�B'�
B/�
B8  B@  BH  BP  BX  B`  Bg�
Bo�
Bx(�B�  B�  B�  B�  B��B�  B�{B�{B�=qB�(�B�{B��
B�  B�  B�{B�{B�{B�  B�  B�  B�  B�  B�  B�  B��B��
B�  B�{B�  B��B��B�  C 
=C
=C
=C
=C
=C

=C  C  C
=C  C��C��C��C��C  C{C   C"  C$
=C&
=C(  C)�C,  C.  C/��C2  C4
=C6  C8  C:  C;��C>  C@
=CB
=CC��CF
=CH
=CI��CL  CN
=CP  CR
=CT
=CV  CX
=CZ  C\  C^  C_��Cb  Cd  Ce�Cg��Ci��Cl  Cn
=Co��Cr  Ct
=Cu��Cw��Cz  C{��C}��C��C�  C�
=C�
=C�C�C�  C�  C�  C�C�C�C���C�  C�  C���C�  C���C���C���C���C���C���C���C���C���C���C�  C�C�C�  C���C���C�  C�
=C�C�  C�  C�C���C�  C�  C���C�  C�C���C���C�  C�C�  C�C�
=C�C�  C�  C���C�C�
=C�
=C�C�C���C���C�
=C�
=C�
=C�C�C�C�C�C�C�C�C�  C�  C�  C�  C�  C���C�C�
=C�C�C�  C���C�  C�
=C�  C�  C���C���C�  C�C�  C�C�
=C�C�  C�C�
=C�  C���C�  C�C�  C�  C�  C���C���C���C�  C�
=C�  C���C�C�  C���C�  C�
=C�  C���C�  C�
=C�C�  C�C�  C���D � D�D�DD}qD��D� DD��D�D��D�qD� D�D}qD�qD� D�qD	� D
  D
}qD  D��D  D}qD  D�D�D��D  D��DD��D�D� D�qDz�D��D� D�D� D  D��D  DxRD��D}qD  D��D  D� D�D� D�qD� D�D�D�D� D�D��DD�D �D z�D ��D!z�D!��D"}qD#  D#� D$�D$�D%�D%}qD%�qD&� D'�D'}qD'�RD(}qD(�qD)}qD)�qD*}qD+  D+��D,�D,}qD,��D-z�D-�qD.}qD.��D/}qD/��D0z�D0�RD1xRD1��D2}qD3  D3��D3�qD4z�D4�qD5� D6  D6��D7�D7� D8�D8��D9D9�D:D:��D:�qD;� D<�D<��D=  D=}qD=�qD>}qD?�D?��D@  D@� DA  DA� DA�qDB� DC�DC�DD  DD}qDE�DE� DE�qDF� DF�qDG}qDH  DH� DI�DI��DJ�DJ� DK�DK��DL  DL}qDL�qDM}qDN�DN��DO  DO��DP  DPz�DP��DQ� DR�DR� DS�DS��DT�DT� DU�DU}qDU�qDV��DWDW��DX�DX��DY�DY� DY�qDZ}qD[  D[}qD[�qD\� D\�qD]� D^D^� D_�D_� D_�qD`��Da�Da}qDa�qDb� Dc�Dc��Dd  Dd}qDd�qDe}qDf  Df� Dg  Dg}qDg�qDh��Di�Di� Dj  Dj� Dk  Dk}qDk�qDl� Dm�Dm� Dm�qDn}qDn��Do}qDp  Dp}qDp�qDq� Dr  Dr� Ds  Ds� Dt  Dt� Du�Du��Dv�Dv��Dv�qDw}qDx  Dx� Dy  Dy� Dz�Dz��Dz�qD{}qD{��D|� D}  D}}qD}�qD~��D�D��D�HD�AHD��HD�� D�  D�>�D�� D��HD�  D�@ D�� D�� D�  D�@ D�� D��HD�HD�@ D��HD��HD�  D�AHD�� D�� D�HD�@ D�� D�� D�  D�AHD��HD��HD���D�>�D�~�D���D�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD��HD�D�HD�@ D�� D���D���D�@ D�� D�� D�HD�@ D�|)D���D�  D�@ D�~�D�� D�HD�@ D�~�D���D���D�>�D�~�D�� D�  D�>�D��HD�D�HD�AHD��HD�� D�HD�AHD�~�D���D�HD�B�D���D�� D���D�>�D�� D���D��qD�>�D�� D��HD�HD�@ D�}qD���D�  D�AHD�~�D��qD���D�@ D��HD�� D���D�@ D�� D���D���D�>�D�~�D�� D���D�@ D�� D�� D�  D�@ D�~�D��qD�  D�AHD�� D��HD�HD�@ D�~�D���D�  D�AHD��HD�� D�  D�>�D�~�D��qD�  D�AHD�� D���D��qD�@ D���D���D��qD�@ D�� D��qD���D�>�D�� D���D���D�@ D��HD�D�HD�@ D�~�D���D�HD�@ D��HD��HD�  D�@ D�~�D��HD�HD�AHD���D�� D�  D�AHD��HD�D�  D�=qD�~�D�� D���D�AHD���D�D�HD�@ D�}qD��)D���D�AHD�� D��HD��D�AHD��HD��HD�  D�>�D��HD�D��D�AHD�� D�� D���D�>�D�~�D���D���D�@ D�~�D���D���D�>�D�~�D�� D��D�>�D�~�D��HD�HD�AHD��HD�D�  D�=qD�}qD��qD��qD�>�D�� D��HD��D�AHD�� D��HD��D�AHD�~�D�� D�HD�@ D�~�D�� D�HD�@ D�� D�D�HD�@ D��HD��HD�  D�@ D�~�D��HD�HD�@ D��HD�� D���D�B�D�D�D�HD�@ D�~�Dþ�D�HD�AHDĀ D�� D�  D�@ D�~�D��HD��D�>�Dƀ D�� D�  D�>�D�~�DǾ�D�  D�AHD�~�DȾ�D���D�@ DɁHD��HD��D�@ D�~�D�� D�  D�@ Dˀ D˽qD���D�@ D̀ D̾�D�  D�B�D́HD�� D�  D�>�D΀ Dξ�D���D�@ D�~�D��HD�  D�@ DЂ�D��HD���D�AHDсHD��HD�HD�AHD�~�DҾ�D�  D�AHDӀ D�� D�  D�@ DԁHD��HD�  D�@ DՀ D�� D�  D�B�Dւ�D�� D�  D�AHD׀ D׾�D�  D�@ D�~�D�� D�HD�AHDـ D��HD��D�B�Dڂ�D��HD�  D�@ Dۀ D��HD�HD�AHD�~�DܽqD���D�>�D݁HD�� D�  D�@ DށHD�D�HD�@ D߀ D�� D�  D�>�D�� D��HD�  D�>�D� D��HD�HD�AHD� D⾸D���D�>�D� D㾸D���D�@ D� D侸D�  D�@ D�~�D�� D�  D�>�D� D�� D�HD�AHD�HD�� D�  D�>�D�~�D�� D���D�@ D� D龸D�  D�@ D�~�D�� D�HD�@ D�~�D�� D�HD�@ D� D��HD�  D�@ D�HD�D�HD�AHD� D�� D�  D�@ D� D�� D�HD�>�D�~�D��HD�HD�@ D�~�D�� D�  D�>�D� D�� D���D�@ D�D�� D��qD�>�D�~�D�� D�  D�>�D���D�� D�  D�@ D�}qD���D�HD�@ D�� D��HD�HD�@ D�~�D��qD��qD�@ D��HD�� D���D�@ D�p�>���>�G�?B�\?�z�?Ǯ@�\@��@5@O\)@h��@�G�@�{@�p�@��@�z�@�G�@�\)@�(�@�=q@�z�AG�AQ�A  AA��A#�
A*�HA2�\A8Q�A@  AG
=AN{ATz�A\(�Ac�
Aj�HAqG�AxQ�A�  A�33A��RA�=qA�A���A��
A�
=A��A���A�
=A�=qA���A��RA�G�A��A��A��A��A��
A�A��A��A�(�A�A��A��A�z�A�ffA�  A�=qA���AθRAУ�Aҏ\A���A�
=Aأ�Aڏ\A��A�
=A���A�33A�p�A�A�G�A�A�A�  A�A�A�A�  A��A��
A�{B Q�B ��B{B\)B(�B�B{B\)Bz�B	G�B
=qB\)Bz�BG�BffB�B��Bp�B�\B�
B��Bp�B�\B�B��Bp�BffB�B��B��BffB�B ��B!B"�\B#�B$��B%B&�\B'�B(��B)B*�\B+�B,z�B-B.�\B/\)B0z�B1��B2�RB3�B4Q�B5p�B6�\B7�B8(�B9p�B:ffB;\)B<Q�B=�B>{B?\)B@Q�BA�BA�BC
=BD  BE�BEBF�\BG�BH��BI��BJffBK�BLz�BMG�BN{BO
=BP(�BP��BQBR�\BS�BTz�BU��BVffBW
=BW�
BX��BYBZ�RB[\)B\(�B]�B^{B^�HB_�B`Q�Ba�Bb{Bc
=Bc�BdQ�Be�Bf{Bg
=Bg�BhQ�Bi�Bj{Bj�HBk�Bl(�Bm�Bn{Bn�HBo�Bpz�Bqp�BrffBs33Bt  Bu�Bv=qBw33Bx  Bx��Bz{B{33B|(�B}G�B~{B
=B�{B���B�G�B�B�=qB��RB�\)B��B�z�B���B�p�B�  B���B�33B�B�=qB��RB�G�B��
B�z�B�
=B���B�{B��\B�33B��
B�z�B���B���B�{B���B��B�B�Q�B���B���B�=qB���B�\)B��B�z�B��B��
B�ffB�
=B���B�(�B���B�p�B�  B��RB�G�B�  B��\B��B���B�{B�z�B��HB�33B���B��B�=qB�z�B��RB��HB�
=B��B�G�B�p�B��B��B��
B��B�(�B�Q�B�z�B���B���B���B���B�
=B�33B�\)B��B�B��
B�  B�=qB�ffB�z�B���B��HB���B��B�33B�\)B��B��B��
B�  B�(�B�ffB�z�B��RB��HB�
=B��B�G�B�p�B��B��B��
B�  B�{B�=qB�ffB���B���B�
=B�33B�\)B��B�B�  B�=qB�z�B���B���B��B�\)B���B��
B�{B�ffB���B���B��B�\)B��B��B�(�B�ffB���B���B�33B��B��B�{B�Q�B��\B���B��B�\)B��B��B�(�B�z�B���B���B�\)B���B��
B�(�B�ffB���B���B�G�B��B��
B�  B�Q�B��\B��HB��B�\)B��B��B�=qB��\B���B�
=B�\)B��B�  B�=qB���B���B�G�B��B��B�=qB��\B��HB�33B���B��B�=qB��\B��HB�G�B��B��B�=qB£�B�
=B�\)BîB�{B�ffBĸRB��B�p�B��B�Q�Bƣ�B�
=B�p�B�B�=qBȏ\B�
=B�p�B��
B�=qBʣ�B��B˅B��B�ffB���B�G�BͮB�(�BΏ\B�
=BυB��B�Q�B���B�G�BѮB�(�Bҏ\B�
=B�p�B��B�ffB��HB�\)B��
B�Q�B��HB�\)B��
B�ffB��HB�p�B��B�ffB���B�p�B��B�ffB��HB�\)B��
B�Q�B���B�G�B��
B�ffB��HB�B�  B�z�B�
=B�B�{B�\B�
=B噚B�{B��B��B癚B�(�B��B��B�B�Q�B���B�G�B��B�ffB��HB�\)B��B�z�B�
=BB�(�B�RB�G�B�B�Q�B��HB�B�{B��RB�33B�B�=qB���B�\)B�  B��\B��B���B�{B��\B��B�B�Q�B���B�\)B��
B�Q�B��HB�p�C 
=C Q�C ��C �
C{C\)C�C�C33Cz�C�RC��C=qC�C�
C(�CffC�C�C33C�C�
C�CffC��C�C33C�C�
C(�CffC��C�C	=qC	�\C	�
C
�C
ffC
�C
��CG�C��C�HC(�Cp�C�RC
=C\)C��C�C33Cz�C��C�CffC��C�C=qC�\C�HC(�CffC�C��CQ�C��C�HC(�Cp�C�RC
=CQ�C��C�C(�Cp�CC{C\)C��C�HC(�Cp�CC
=CQ�C��C�HC(�Cz�CC
=CQ�C�\C�HC33Cz�C�RC  CG�C��C�HC(�CffC�C  CG�C��C�
C�CffC�RC 
=C Q�C ��C �C!(�C!�C!��C"{C"\)C"��C"��C#G�C#��C#�C$(�C$p�C$C%�C%ffC%��C%�C&=qC&�\C&�HC'33C'z�C'C(
=C(Q�C(�C)  C)G�C)�\C)�
C*�C*p�C*C+{C+\)C+��C+�HC,=qC,�\C,�
C-{C-\)C-�C.  C.Q�C.��C.�
C/�C/z�C/C0  C0G�C0��C0�C133C1z�C1��C2�C2ffC2�C2��C3Q�C3��C3�C4(�C4�C4�
C5�C5\)C5�C6
=C6Q�C6��C6�HC7=qC7�\C7�
C8�C8p�C8��C9{C9\)C9��C:  C:G�C:�\C:�C;=qC;z�C;��C<(�C<p�C<�RC=
=C=\)C=��C=��C>Q�C>��C>�C?=qC?��C?�C@33C@z�C@�
CA33CA�CA��CB{CBz�CB��CC
=CCffCCCD
=CD\)CD�RCE{CE\)CE�CF
=CFffCF�CG  CG\)CG�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                             ?u@   @B�\@�G�@�G�@\@޸R@�(�A\)A ��A,(�A@  A`  A\)A�  A�  A�  A�  A�  A�  A�A��B  B  B  B (�B'�
B/�
B8  B@  BH  BP  BX  B`  Bg�
Bo�
Bx(�B�  B�  B�  B�  B��B�  B�{B�{B�=qB�(�B�{B��
B�  B�  B�{B�{B�{B�  B�  B�  B�  B�  B�  B�  B��B��
B�  B�{B�  B��B��B�  C 
=C
=C
=C
=C
=C

=C  C  C
=C  C��C��C��C��C  C{C   C"  C$
=C&
=C(  C)�C,  C.  C/��C2  C4
=C6  C8  C:  C;��C>  C@
=CB
=CC��CF
=CH
=CI��CL  CN
=CP  CR
=CT
=CV  CX
=CZ  C\  C^  C_��Cb  Cd  Ce�Cg��Ci��Cl  Cn
=Co��Cr  Ct
=Cu��Cw��Cz  C{��C}��C��C�  C�
=C�
=C�C�C�  C�  C�  C�C�C�C���C�  C�  C���C�  C���C���C���C���C���C���C���C���C���C���C�  C�C�C�  C���C���C�  C�
=C�C�  C�  C�C���C�  C�  C���C�  C�C���C���C�  C�C�  C�C�
=C�C�  C�  C���C�C�
=C�
=C�C�C���C���C�
=C�
=C�
=C�C�C�C�C�C�C�C�C�  C�  C�  C�  C�  C���C�C�
=C�C�C�  C���C�  C�
=C�  C�  C���C���C�  C�C�  C�C�
=C�C�  C�C�
=C�  C���C�  C�C�  C�  C�  C���C���C���C�  C�
=C�  C���C�C�  C���C�  C�
=C�  C���C�  C�
=C�C�  C�C�  C���D � D�D�DD}qD��D� DD��D�D��D�qD� D�D}qD�qD� D�qD	� D
  D
}qD  D��D  D}qD  D�D�D��D  D��DD��D�D� D�qDz�D��D� D�D� D  D��D  DxRD��D}qD  D��D  D� D�D� D�qD� D�D�D�D� D�D��DD�D �D z�D ��D!z�D!��D"}qD#  D#� D$�D$�D%�D%}qD%�qD&� D'�D'}qD'�RD(}qD(�qD)}qD)�qD*}qD+  D+��D,�D,}qD,��D-z�D-�qD.}qD.��D/}qD/��D0z�D0�RD1xRD1��D2}qD3  D3��D3�qD4z�D4�qD5� D6  D6��D7�D7� D8�D8��D9D9�D:D:��D:�qD;� D<�D<��D=  D=}qD=�qD>}qD?�D?��D@  D@� DA  DA� DA�qDB� DC�DC�DD  DD}qDE�DE� DE�qDF� DF�qDG}qDH  DH� DI�DI��DJ�DJ� DK�DK��DL  DL}qDL�qDM}qDN�DN��DO  DO��DP  DPz�DP��DQ� DR�DR� DS�DS��DT�DT� DU�DU}qDU�qDV��DWDW��DX�DX��DY�DY� DY�qDZ}qD[  D[}qD[�qD\� D\�qD]� D^D^� D_�D_� D_�qD`��Da�Da}qDa�qDb� Dc�Dc��Dd  Dd}qDd�qDe}qDf  Df� Dg  Dg}qDg�qDh��Di�Di� Dj  Dj� Dk  Dk}qDk�qDl� Dm�Dm� Dm�qDn}qDn��Do}qDp  Dp}qDp�qDq� Dr  Dr� Ds  Ds� Dt  Dt� Du�Du��Dv�Dv��Dv�qDw}qDx  Dx� Dy  Dy� Dz�Dz��Dz�qD{}qD{��D|� D}  D}}qD}�qD~��D�D��D�HD�AHD��HD�� D�  D�>�D�� D��HD�  D�@ D�� D�� D�  D�@ D�� D��HD�HD�@ D��HD��HD�  D�AHD�� D�� D�HD�@ D�� D�� D�  D�AHD��HD��HD���D�>�D�~�D���D�HD�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD��HD�D�HD�@ D�� D���D���D�@ D�� D�� D�HD�@ D�|)D���D�  D�@ D�~�D�� D�HD�@ D�~�D���D���D�>�D�~�D�� D�  D�>�D��HD�D�HD�AHD��HD�� D�HD�AHD�~�D���D�HD�B�D���D�� D���D�>�D�� D���D��qD�>�D�� D��HD�HD�@ D�}qD���D�  D�AHD�~�D��qD���D�@ D��HD�� D���D�@ D�� D���D���D�>�D�~�D�� D���D�@ D�� D�� D�  D�@ D�~�D��qD�  D�AHD�� D��HD�HD�@ D�~�D���D�  D�AHD��HD�� D�  D�>�D�~�D��qD�  D�AHD�� D���D��qD�@ D���D���D��qD�@ D�� D��qD���D�>�D�� D���D���D�@ D��HD�D�HD�@ D�~�D���D�HD�@ D��HD��HD�  D�@ D�~�D��HD�HD�AHD���D�� D�  D�AHD��HD�D�  D�=qD�~�D�� D���D�AHD���D�D�HD�@ D�}qD��)D���D�AHD�� D��HD��D�AHD��HD��HD�  D�>�D��HD�D��D�AHD�� D�� D���D�>�D�~�D���D���D�@ D�~�D���D���D�>�D�~�D�� D��D�>�D�~�D��HD�HD�AHD��HD�D�  D�=qD�}qD��qD��qD�>�D�� D��HD��D�AHD�� D��HD��D�AHD�~�D�� D�HD�@ D�~�D�� D�HD�@ D�� D�D�HD�@ D��HD��HD�  D�@ D�~�D��HD�HD�@ D��HD�� D���D�B�D�D�D�HD�@ D�~�Dþ�D�HD�AHDĀ D�� D�  D�@ D�~�D��HD��D�>�Dƀ D�� D�  D�>�D�~�DǾ�D�  D�AHD�~�DȾ�D���D�@ DɁHD��HD��D�@ D�~�D�� D�  D�@ Dˀ D˽qD���D�@ D̀ D̾�D�  D�B�D́HD�� D�  D�>�D΀ Dξ�D���D�@ D�~�D��HD�  D�@ DЂ�D��HD���D�AHDсHD��HD�HD�AHD�~�DҾ�D�  D�AHDӀ D�� D�  D�@ DԁHD��HD�  D�@ DՀ D�� D�  D�B�Dւ�D�� D�  D�AHD׀ D׾�D�  D�@ D�~�D�� D�HD�AHDـ D��HD��D�B�Dڂ�D��HD�  D�@ Dۀ D��HD�HD�AHD�~�DܽqD���D�>�D݁HD�� D�  D�@ DށHD�D�HD�@ D߀ D�� D�  D�>�D�� D��HD�  D�>�D� D��HD�HD�AHD� D⾸D���D�>�D� D㾸D���D�@ D� D侸D�  D�@ D�~�D�� D�  D�>�D� D�� D�HD�AHD�HD�� D�  D�>�D�~�D�� D���D�@ D� D龸D�  D�@ D�~�D�� D�HD�@ D�~�D�� D�HD�@ D� D��HD�  D�@ D�HD�D�HD�AHD� D�� D�  D�@ D� D�� D�HD�>�D�~�D��HD�HD�@ D�~�D�� D�  D�>�D� D�� D���D�@ D�D�� D��qD�>�D�~�D�� D�  D�>�D���D�� D�  D�@ D�}qD���D�HD�@ D�� D��HD�HD�@ D�~�D��qD��qD�@ D��HD�� D���D�@ G�O�>���>�G�?B�\?�z�?Ǯ@�\@��@5@O\)@h��@�G�@�{@�p�@��@�z�@�G�@�\)@�(�@�=q@�z�AG�AQ�A  AA��A#�
A*�HA2�\A8Q�A@  AG
=AN{ATz�A\(�Ac�
Aj�HAqG�AxQ�A�  A�33A��RA�=qA�A���A��
A�
=A��A���A�
=A�=qA���A��RA�G�A��A��A��A��A��
A�A��A��A�(�A�A��A��A�z�A�ffA�  A�=qA���AθRAУ�Aҏ\A���A�
=Aأ�Aڏ\A��A�
=A���A�33A�p�A�A�G�A�A�A�  A�A�A�A�  A��A��
A�{B Q�B ��B{B\)B(�B�B{B\)Bz�B	G�B
=qB\)Bz�BG�BffB�B��Bp�B�\B�
B��Bp�B�\B�B��Bp�BffB�B��B��BffB�B ��B!B"�\B#�B$��B%B&�\B'�B(��B)B*�\B+�B,z�B-B.�\B/\)B0z�B1��B2�RB3�B4Q�B5p�B6�\B7�B8(�B9p�B:ffB;\)B<Q�B=�B>{B?\)B@Q�BA�BA�BC
=BD  BE�BEBF�\BG�BH��BI��BJffBK�BLz�BMG�BN{BO
=BP(�BP��BQBR�\BS�BTz�BU��BVffBW
=BW�
BX��BYBZ�RB[\)B\(�B]�B^{B^�HB_�B`Q�Ba�Bb{Bc
=Bc�BdQ�Be�Bf{Bg
=Bg�BhQ�Bi�Bj{Bj�HBk�Bl(�Bm�Bn{Bn�HBo�Bpz�Bqp�BrffBs33Bt  Bu�Bv=qBw33Bx  Bx��Bz{B{33B|(�B}G�B~{B
=B�{B���B�G�B�B�=qB��RB�\)B��B�z�B���B�p�B�  B���B�33B�B�=qB��RB�G�B��
B�z�B�
=B���B�{B��\B�33B��
B�z�B���B���B�{B���B��B�B�Q�B���B���B�=qB���B�\)B��B�z�B��B��
B�ffB�
=B���B�(�B���B�p�B�  B��RB�G�B�  B��\B��B���B�{B�z�B��HB�33B���B��B�=qB�z�B��RB��HB�
=B��B�G�B�p�B��B��B��
B��B�(�B�Q�B�z�B���B���B���B���B�
=B�33B�\)B��B�B��
B�  B�=qB�ffB�z�B���B��HB���B��B�33B�\)B��B��B��
B�  B�(�B�ffB�z�B��RB��HB�
=B��B�G�B�p�B��B��B��
B�  B�{B�=qB�ffB���B���B�
=B�33B�\)B��B�B�  B�=qB�z�B���B���B��B�\)B���B��
B�{B�ffB���B���B��B�\)B��B��B�(�B�ffB���B���B�33B��B��B�{B�Q�B��\B���B��B�\)B��B��B�(�B�z�B���B���B�\)B���B��
B�(�B�ffB���B���B�G�B��B��
B�  B�Q�B��\B��HB��B�\)B��B��B�=qB��\B���B�
=B�\)B��B�  B�=qB���B���B�G�B��B��B�=qB��\B��HB�33B���B��B�=qB��\B��HB�G�B��B��B�=qB£�B�
=B�\)BîB�{B�ffBĸRB��B�p�B��B�Q�Bƣ�B�
=B�p�B�B�=qBȏ\B�
=B�p�B��
B�=qBʣ�B��B˅B��B�ffB���B�G�BͮB�(�BΏ\B�
=BυB��B�Q�B���B�G�BѮB�(�Bҏ\B�
=B�p�B��B�ffB��HB�\)B��
B�Q�B��HB�\)B��
B�ffB��HB�p�B��B�ffB���B�p�B��B�ffB��HB�\)B��
B�Q�B���B�G�B��
B�ffB��HB�B�  B�z�B�
=B�B�{B�\B�
=B噚B�{B��B��B癚B�(�B��B��B�B�Q�B���B�G�B��B�ffB��HB�\)B��B�z�B�
=BB�(�B�RB�G�B�B�Q�B��HB�B�{B��RB�33B�B�=qB���B�\)B�  B��\B��B���B�{B��\B��B�B�Q�B���B�\)B��
B�Q�B��HB�p�C 
=C Q�C ��C �
C{C\)C�C�C33Cz�C�RC��C=qC�C�
C(�CffC�C�C33C�C�
C�CffC��C�C33C�C�
C(�CffC��C�C	=qC	�\C	�
C
�C
ffC
�C
��CG�C��C�HC(�Cp�C�RC
=C\)C��C�C33Cz�C��C�CffC��C�C=qC�\C�HC(�CffC�C��CQ�C��C�HC(�Cp�C�RC
=CQ�C��C�C(�Cp�CC{C\)C��C�HC(�Cp�CC
=CQ�C��C�HC(�Cz�CC
=CQ�C�\C�HC33Cz�C�RC  CG�C��C�HC(�CffC�C  CG�C��C�
C�CffC�RC 
=C Q�C ��C �C!(�C!�C!��C"{C"\)C"��C"��C#G�C#��C#�C$(�C$p�C$C%�C%ffC%��C%�C&=qC&�\C&�HC'33C'z�C'C(
=C(Q�C(�C)  C)G�C)�\C)�
C*�C*p�C*C+{C+\)C+��C+�HC,=qC,�\C,�
C-{C-\)C-�C.  C.Q�C.��C.�
C/�C/z�C/C0  C0G�C0��C0�C133C1z�C1��C2�C2ffC2�C2��C3Q�C3��C3�C4(�C4�C4�
C5�C5\)C5�C6
=C6Q�C6��C6�HC7=qC7�\C7�
C8�C8p�C8��C9{C9\)C9��C:  C:G�C:�\C:�C;=qC;z�C;��C<(�C<p�C<�RC=
=C=\)C=��C=��C>Q�C>��C>�C?=qC?��C?�C@33C@z�C@�
CA33CA�CA��CB{CBz�CB��CC
=CCffCCCD
=CD\)CD�RCE{CE\)CE�CF
=CFffCF�CG  CG\)CG�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                             @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�oG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�"�A�33A�1'A�+A�-A�9XA�=qA�?}A�VA�DA�\AᙚA�A�^A�ƨA���A��/A��TA��A��A���A���A���A���A�A�1A�VA�bA�VA�bA�{A�oA�bA���A���A���A�I�A��A�$�A�(�A�XAɇ+AƏ\A�
=A�x�A��FA�{A��A�"�A� �A�~�A���A���A�JA��
A�r�A�Q�A�bNA���A�A��-A���A���A��AAzjAr�HAo�mAm�TAj�`Ag\)Ac�A`�RA]�wAZA�AWXAU�AT^5AN�AL��AK?}AI�AH��AH�AH�AB�A?�^A>��A=C�A;dZA:bA81'A7�A6�!A6�uA5p�A5p�A5x�A5?}A4��A5/A6v�A6��A5��A4ZA2VA2E�A0ȴA/�;A/�hA/XA-�TA-�A,�HA,��A,JA+`BA*�A)�
A(r�A&=qA&^5A%t�A$�A#�A"�RA"�9A"��A"��A"E�A!�hA!��A"�A"$�A" �A!�#A!hsA ��A �/A M�A7LA��A��A�FA��A��Ar�A(�A�^A33A
=A��AbNA��A�#A\)AoAĜAz�AM�A�FAdZA��A��AVA�wA\)A"�A��A�RAA�A�#A�A��A��A��A\)A�A��AffAƨA�PA|�A?}A�/A�AZA=qA1A��A
=A�A�/A�AA��A��Al�AG�A�A��A5?A(�A�A�A��At�A;dA
��A
bA	��A	�PA	�PA	XA	
=A�/A��AI�A1A�TAƨA�hA/A��A��Av�AVA�AdZA&�A�A��A(�AbA�^AO�A�jAbNA5?A�mA��A��Al�A ��A ��A  �@��;@���@��@�"�@��-@���@�1@���@��@�J@��h@�/@��@��`@�z�@�\)@��H@���@�/@��j@�z�@�I�@�w@�@�!@�n�@�@�&�@���@�@�9X@�|�@��@�ȴ@��@�h@�`B@�/@���@�9X@��m@�K�@�!@�=q@��@�?}@蛦@�F@�t�@�"�@�+@��@�h@���@��@�K�@�33@���@�n�@�hs@�r�@�t�@�~�@ݙ�@�O�@�V@ܬ@���@�-@���@�p�@��@���@ؓu@� �@�  @��@׶F@�
=@���@�O�@��`@ԓu@�z�@�A�@��;@��@�@�X@�O�@�?}@�/@�%@�z�@�S�@�@��@�^5@Ͳ-@�x�@���@�Z@˕�@�o@���@��@��@��@�E�@�7L@ȼj@� �@�dZ@�
=@Ɵ�@���@�@�O�@��/@ļj@ă@�(�@��@Ý�@�
=@�~�@��@���@�V@��@��j@��@��;@��@�@�?}@�Ĝ@�j@�|�@�ff@��@���@�/@��j@�1'@�S�@��@���@��#@�G�@���@�b@�S�@���@�M�@�J@���@���@�X@�&�@�%@��@��/@���@�Ĝ@��9@�Z@���@��@��y@�v�@�V@�{@���@�`B@�&�@�%@��/@���@�b@���@�+@���@���@�n�@���@�&�@�%@��`@���@�z�@�1'@���@���@��@��P@�K�@�o@�^5@�G�@���@��9@�j@���@�S�@�o@��y@��@��R@�M�@�@�p�@��@���@�bN@�(�@��;@��@�+@�ȴ@��!@�n�@���@�p�@��@�A�@��;@��@���@���@��@���@�x�@�V@��j@�Z@�9X@�(�@� �@��@��@�b@�ƨ@�|�@��H@�M�@��@��#@��-@�X@��9@��@��y@�n�@�=q@�-@��@��@���@�x�@�/@��9@�9X@���@��@���@��@�l�@�;d@���@�ff@���@�`B@�V@�V@�V@���@��`@�Ĝ@���@�bN@�b@��;@�ƨ@���@�S�@�;d@�+@�o@��@���@�M�@���@��@��T@��^@�x�@�hs@�?}@�/@��@���@�Ĝ@��D@�A�@�b@��w@�l�@�S�@�C�@�
=@���@���@��+@�M�@�@���@��7@�p�@�7L@�%@��`@���@�r�@�Z@�9X@�1@���@��w@���@�dZ@�
=@���@��R@��+@��@��@��#@�x�@���@���@���@��u@�j@�I�@�1@��@K�@~�y@~��@}�@}O�@|�@|�j@|��@|z�@|I�@{�m@{dZ@{"�@z�\@y�^@yG�@y%@x�@w�@w��@w�@v��@vV@v5?@v{@v@u�-@u�-@up�@t�@tZ@t(�@s��@s@r�!@rJ@qhs@p��@p��@pQ�@o�;@o
=@n�+@nE�@m��@m�@l�j@lI�@k��@kdZ@kC�@kC�@j��@j~�@j=q@i�#@iX@h�9@hbN@hb@g��@gl�@g;d@f��@f��@e�@e�-@e�@d�j@d�@dj@d�@cdZ@b��@b-@a�@a�^@a�7@aX@a%@`�u@`b@_��@^E�@]`B@\�@\�@[�F@["�@Z�@Z�H@Z��@Z�\@Z^5@Z-@Y�#@Y�7@Y7L@Y%@X��@X��@XĜ@X�u@Xr�@XQ�@X  @W\)@V��@V�+@Vv�@Vff@VV@V5?@U�T@U�@T��@TI�@S��@S�@S"�@R��@R~�@Rn�@R�@Q��@QG�@P��@PbN@P �@O�@O��@OK�@Nv�@N{@M�T@M�-@M�h@M�@M?}@M/@L��@L�/@L�@LZ@K�m@K@J�@IX@HĜ@H��@H�@Hr�@Hr�@HbN@HA�@G�@Gl�@G+@Fȴ@FV@E��@E��@EO�@E?}@E?}@EV@D�/@D��@D�D@DZ@D�@C�m@CdZ@Bn�@A�@A�^@AG�@@��@@�9@@��@@A�@?|�@>�@>5?@=�h@=?}@=V@<��@<��@<�@<�D@<z�@<I�@;ƨ@;S�@:�\@9x�@9G�@9�@8��@9%@9%@8��@8�9@8�u@8Q�@81'@8  @7�P@7
=@6E�@5�@5�-@5�h@5�@5p�@5p�@5`B@5O�@5O�@5�@4�@4�@4j@3��@3ƨ@3��@3C�@2��@2��@2=q@1��@1hs@1%@0��@0Ĝ@0bN@0A�@0 �@/�@/;d@.��@.ȴ@.��@.ff@.@-@-��@-`B@-?}@,��@,z�@,�@+�@+t�@+"�@*�\@*M�@*-@)��@)��@)��@)hs@(�`@(r�@(bN@(A�@(1'@'�;@'�@'\)@'K�@'
=@&v�@%�@%@%p�@%�@$�j@$�D@$j@$9X@$1@#�F@#��@#�@#t�@#C�@#@"��@"n�@"�@!�#@!��@!�7@!x�@!hs@!X@ Ĝ@   @�@��@��@|�@l�@K�@
=@�R@��@�+@v�@v�@ff@V@E�@$�@@�h@�h@p�@O�@/@�@�/@��@��@9X@�@1@�m@ƨ@��@t�@dZ@o@��@�\@^5@=q@�@��@hs@��@r�@ �@��@\)@K�@+@�@��@5?@@�T@��@�-@�h@�@O�@?}@?}@�@�@�j@��@Z@��@�@dZ@33@��@�!@�!@��@�!@��@^5@=q@��@G�@7L@&�@�@��@�9@��@�u@�@r�@bN@A�@ �@�@�@��@�P@l�@K�@�@��@ȴ@�R@��@v�@ff@V@E�@{@@�@?}@��@��@(�@1A�{A�{A�"�A�$�A�/A�5?A�/A�/A�/A�-A�(�A�&�A�-A�-A�-A�33A�K�A�E�A�33A�9XA�?}A�M�A�S�A�bNA�+A�\A�hA�hAᕁAᛦAᙚA��A��A�-A�A�-A�9A�^A�^A�^A�jA�ĜA�ƨA�ĜA�ƨA���A���A���A���A���A���A���A��#A��A��A��;A��;A��#A��/A��;A��TA��TA��;A��TA��`A��`A��`A��mA��A��A��A��A��A���A��A��A��A���A��A��A��A���A��A��A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A���A���A�  A�  A���A���A�A�A���A�  A�A�A�  A�  A�A�%A�%A�A�A�1A�JA�1A�1A�JA�bA�JA�JA�VA�bA�JA�JA�JA�oA�oA�bA�VA�oA�{A�oA�VA�JA�bA�bA�JA�VA�bA�oA�VA�JA�bA�bA�oA�bA�JA�bA�{A�oA�bA�bA�{A��A�{A�oA�{A��A�{A�bA�bA�{A��A�oA�bA�bA�oA�{A�oA�bA�bA�bA�{A�{A�bA�VA�VA�JA�1A�%A�A�A�  A��A��mA��HA��/A��
A���A�ȴA�ƨA�RA�-A��A��A��AᗍA�7A�l�A�C�A��A�t�A�33A�bA�A�A��;A��#A���A�G�A��
A�z�A�
=A���A׉7A�-A��A֑hA�\)A���A��A�ZAӉ7A���A�VA�"�A��/Aѡ�A�^5A���A���A�~�A�&�A��/AΧ�A�r�A�XA�(�A��/A�l�A�&�A�1A�ĜȦ+A��Aɏ\A�-A�JA��AȰ!A�bNA��TA�hsA��A�bNA��A�`BA���Aě�A�"�A×�A´9A�oA�VA���A�n�A�ĜA�(�A���A��7A�E�A���A�`BA��mA��A�x�A�dZA�;dA���A���A�9XA���A�C�A���A�M�A���A�$�A�ƨA�E�A�VA��A�ƨA�bNA�5?A�A���A�r�A�+A��A���A���A��\A��uA�v�A��A�ƨA�dZA�{A��#A�r�A���A�|�A�G�A�"�A���A�l�A��A�Q�A��A�1A��/A��;A���A���A��+A�x�A�S�A�9XA�(�A��A��
A��RA��7A�hsA�G�A�5?A��A���A��A�ƨA�?}A��A��A�33A�n�A���A���A���A��A�  A�Q�A��A��A�n�A�  A�p�A���A���A�v�A�A�A��A���A��A��`A��`A��A���A���A�ƨA���A��RA��9A��FA���A��\A�z�A�jA�Q�A�
=A���A��A��mA��#A���A�A��jA��jA��^A��-A���A���A���A��uA��A�ffA�I�A�;dA�-A���A���A��A�ffA���A��mA�n�A��A��#A���A�x�A�C�A��A��9A�jA�1A��;A���A���A�v�A�dZA�O�A�7LA�&�A�VA���A��A��`A���A���A��-A���A���A��7A�t�A�XA�M�A�/A�A��A��/A�G�A���A��9A�S�A��TA�l�A�"�A��A��A��A���A��^A���A��`A��A�ĜA���A�bNA�?}A��`A�p�A�{A�ƨA�jA�$�A��
A�^5A��/A�jA�  A���A�7LA���A���A�|�A�bNA�?}A� �A�1A�  A���A���A���A�v�A�I�A��A��A���A��A��uA�t�A�\)A�A�A��A���A��A���A��jA��\A�(�A���A���A�l�A�
=A��
A���A��RA��FA���A��A�x�A�^5A�Q�A��AC�A}��A|��A|ffA{l�A{�Az��Ay�Ax�RAw?}Aux�At{As33Arz�Aq�FAqVAp�\ApVAp1'ApAo��AoƨAo�Ao�PAoXAn��An~�An �Am�wAmC�Al��AlZAk�mAk�Ak`BAk
=Aj�Aj�Ai�^AiS�AhĜAh$�Ag��Ag%AfE�Ae�Ae�-Ae\)Ad�/AdbNAc�;AcO�Ab�\Ab �Aa��Aax�Aa&�A`��A`9XA_��A_\)A_A^�!A^1A]O�A]�A\�A\^5A[�A[�AZ�uAZ�AY��AY33AX��AXv�AX(�AW�PAV��AV�+AVbNAVQ�AV$�AU��AU�AU�#AU�wAU�FAU�AU��AUC�AT�+AT �AS�^AR��AP�/AO"�AN=qAM�AMC�AM�AL�ALȴAL�RAL�!AL�9AL�9ALffAL$�AK��AK��AKhsAK�AJ��AJ��AJ�DAJQ�AJ1'AJ�AJ  AI�TAI|�AI/AH�`AH��AH��AH��AHȴAH��AHȴAHȴAH��AH��AH��AH��AH��AH�AH�AH�AH��AH��AH��AHv�AH(�AGp�AFI�AC��AA��A@��A@��A@��A@�uA@jA?�A?p�A??}A?�A>�A>�HA>��A>�jA>�!A>�\A>5?A=�A=A=�A=G�A=�A<�A<��A<9XA;�A;��A;G�A:�A:ĜA:��A:�A:jA:E�A9��A9��A9hsA9VA8�uA8Q�A85?A8JA7�;A7�A7t�A7?}A7�A7%A6��A6�A6�HA6��A6��A6ĜA6�RA6��A6z�A6r�A6~�A6��A6�!A6�RA6�!A6��A6$�A5|�A5O�A5\)A5dZA5t�A5|�A5|�A5t�A5l�A5hsA5l�A5p�A5p�A5l�A5t�A5|�A5|�A5t�A5x�A5t�A5l�A5O�A5K�A533A5�A4��A4�/A4ĜA4�DA4jA4jA4~�A4�A4�\A4��A4�A5��A5�A6$�A6(�A6n�A6�DA6�\A6�uA6�uA6��A6�/A7VA6�yA6��A6�A6A�A5�TA5A5��A5K�A5oA4�/A4�HA4��A4��A4�+A3�^A3�A2ȴA2(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                             A�"�A�33A�1'A�+A�-A�9XA�=qA�?}A�VA�DA�\AᙚA�A�^A�ƨA���A��/A��TA��A��A���A���A���A���A�A�1A�VA�bA�VA�bA�{A�oA�bA���A���A���A�I�A��A�$�A�(�A�XAɇ+AƏ\A�
=A�x�A��FA�{A��A�"�A� �A�~�A���A���A�JA��
A�r�A�Q�A�bNA���A�A��-A���A���A��AAzjAr�HAo�mAm�TAj�`Ag\)Ac�A`�RA]�wAZA�AWXAU�AT^5AN�AL��AK?}AI�AH��AH�AH�AB�A?�^A>��A=C�A;dZA:bA81'A7�A6�!A6�uA5p�A5p�A5x�A5?}A4��A5/A6v�A6��A5��A4ZA2VA2E�A0ȴA/�;A/�hA/XA-�TA-�A,�HA,��A,JA+`BA*�A)�
A(r�A&=qA&^5A%t�A$�A#�A"�RA"�9A"��A"��A"E�A!�hA!��A"�A"$�A" �A!�#A!hsA ��A �/A M�A7LA��A��A�FA��A��Ar�A(�A�^A33A
=A��AbNA��A�#A\)AoAĜAz�AM�A�FAdZA��A��AVA�wA\)A"�A��A�RAA�A�#A�A��A��A��A\)A�A��AffAƨA�PA|�A?}A�/A�AZA=qA1A��A
=A�A�/A�AA��A��Al�AG�A�A��A5?A(�A�A�A��At�A;dA
��A
bA	��A	�PA	�PA	XA	
=A�/A��AI�A1A�TAƨA�hA/A��A��Av�AVA�AdZA&�A�A��A(�AbA�^AO�A�jAbNA5?A�mA��A��Al�A ��A ��A  �@��;@���@��@�"�@��-@���@�1@���@��@�J@��h@�/@��@��`@�z�@�\)@��H@���@�/@��j@�z�@�I�@�w@�@�!@�n�@�@�&�@���@�@�9X@�|�@��@�ȴ@��@�h@�`B@�/@���@�9X@��m@�K�@�!@�=q@��@�?}@蛦@�F@�t�@�"�@�+@��@�h@���@��@�K�@�33@���@�n�@�hs@�r�@�t�@�~�@ݙ�@�O�@�V@ܬ@���@�-@���@�p�@��@���@ؓu@� �@�  @��@׶F@�
=@���@�O�@��`@ԓu@�z�@�A�@��;@��@�@�X@�O�@�?}@�/@�%@�z�@�S�@�@��@�^5@Ͳ-@�x�@���@�Z@˕�@�o@���@��@��@��@�E�@�7L@ȼj@� �@�dZ@�
=@Ɵ�@���@�@�O�@��/@ļj@ă@�(�@��@Ý�@�
=@�~�@��@���@�V@��@��j@��@��;@��@�@�?}@�Ĝ@�j@�|�@�ff@��@���@�/@��j@�1'@�S�@��@���@��#@�G�@���@�b@�S�@���@�M�@�J@���@���@�X@�&�@�%@��@��/@���@�Ĝ@��9@�Z@���@��@��y@�v�@�V@�{@���@�`B@�&�@�%@��/@���@�b@���@�+@���@���@�n�@���@�&�@�%@��`@���@�z�@�1'@���@���@��@��P@�K�@�o@�^5@�G�@���@��9@�j@���@�S�@�o@��y@��@��R@�M�@�@�p�@��@���@�bN@�(�@��;@��@�+@�ȴ@��!@�n�@���@�p�@��@�A�@��;@��@���@���@��@���@�x�@�V@��j@�Z@�9X@�(�@� �@��@��@�b@�ƨ@�|�@��H@�M�@��@��#@��-@�X@��9@��@��y@�n�@�=q@�-@��@��@���@�x�@�/@��9@�9X@���@��@���@��@�l�@�;d@���@�ff@���@�`B@�V@�V@�V@���@��`@�Ĝ@���@�bN@�b@��;@�ƨ@���@�S�@�;d@�+@�o@��@���@�M�@���@��@��T@��^@�x�@�hs@�?}@�/@��@���@�Ĝ@��D@�A�@�b@��w@�l�@�S�@�C�@�
=@���@���@��+@�M�@�@���@��7@�p�@�7L@�%@��`@���@�r�@�Z@�9X@�1@���@��w@���@�dZ@�
=@���@��R@��+@��@��@��#@�x�@���@���@���@��u@�j@�I�@�1@��@K�@~�y@~��@}�@}O�@|�@|�j@|��@|z�@|I�@{�m@{dZ@{"�@z�\@y�^@yG�@y%@x�@w�@w��@w�@v��@vV@v5?@v{@v@u�-@u�-@up�@t�@tZ@t(�@s��@s@r�!@rJ@qhs@p��@p��@pQ�@o�;@o
=@n�+@nE�@m��@m�@l�j@lI�@k��@kdZ@kC�@kC�@j��@j~�@j=q@i�#@iX@h�9@hbN@hb@g��@gl�@g;d@f��@f��@e�@e�-@e�@d�j@d�@dj@d�@cdZ@b��@b-@a�@a�^@a�7@aX@a%@`�u@`b@_��@^E�@]`B@\�@\�@[�F@["�@Z�@Z�H@Z��@Z�\@Z^5@Z-@Y�#@Y�7@Y7L@Y%@X��@X��@XĜ@X�u@Xr�@XQ�@X  @W\)@V��@V�+@Vv�@Vff@VV@V5?@U�T@U�@T��@TI�@S��@S�@S"�@R��@R~�@Rn�@R�@Q��@QG�@P��@PbN@P �@O�@O��@OK�@Nv�@N{@M�T@M�-@M�h@M�@M?}@M/@L��@L�/@L�@LZ@K�m@K@J�@IX@HĜ@H��@H�@Hr�@Hr�@HbN@HA�@G�@Gl�@G+@Fȴ@FV@E��@E��@EO�@E?}@E?}@EV@D�/@D��@D�D@DZ@D�@C�m@CdZ@Bn�@A�@A�^@AG�@@��@@�9@@��@@A�@?|�@>�@>5?@=�h@=?}@=V@<��@<��@<�@<�D@<z�@<I�@;ƨ@;S�@:�\@9x�@9G�@9�@8��@9%@9%@8��@8�9@8�u@8Q�@81'@8  @7�P@7
=@6E�@5�@5�-@5�h@5�@5p�@5p�@5`B@5O�@5O�@5�@4�@4�@4j@3��@3ƨ@3��@3C�@2��@2��@2=q@1��@1hs@1%@0��@0Ĝ@0bN@0A�@0 �@/�@/;d@.��@.ȴ@.��@.ff@.@-@-��@-`B@-?}@,��@,z�@,�@+�@+t�@+"�@*�\@*M�@*-@)��@)��@)��@)hs@(�`@(r�@(bN@(A�@(1'@'�;@'�@'\)@'K�@'
=@&v�@%�@%@%p�@%�@$�j@$�D@$j@$9X@$1@#�F@#��@#�@#t�@#C�@#@"��@"n�@"�@!�#@!��@!�7@!x�@!hs@!X@ Ĝ@   @�@��@��@|�@l�@K�@
=@�R@��@�+@v�@v�@ff@V@E�@$�@@�h@�h@p�@O�@/@�@�/@��@��@9X@�@1@�m@ƨ@��@t�@dZ@o@��@�\@^5@=q@�@��@hs@��@r�@ �@��@\)@K�@+@�@��@5?@@�T@��@�-@�h@�@O�@?}@?}@�@�@�j@��@Z@��@�@dZ@33@��@�!@�!@��@�!@��@^5@=q@��@G�@7L@&�@�@��@�9@��@�u@�@r�@bN@A�@ �@�@�@��@�P@l�@K�@�@��@ȴ@�R@��@v�@ff@V@E�@{@@�@?}@��@��@(�G�O�A�{A�{A�"�A�$�A�/A�5?A�/A�/A�/A�-A�(�A�&�A�-A�-A�-A�33A�K�A�E�A�33A�9XA�?}A�M�A�S�A�bNA�+A�\A�hA�hAᕁAᛦAᙚA��A��A�-A�A�-A�9A�^A�^A�^A�jA�ĜA�ƨA�ĜA�ƨA���A���A���A���A���A���A���A��#A��A��A��;A��;A��#A��/A��;A��TA��TA��;A��TA��`A��`A��`A��mA��A��A��A��A��A���A��A��A��A���A��A��A��A���A��A��A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A���A���A�  A�  A���A���A�A�A���A�  A�A�A�  A�  A�A�%A�%A�A�A�1A�JA�1A�1A�JA�bA�JA�JA�VA�bA�JA�JA�JA�oA�oA�bA�VA�oA�{A�oA�VA�JA�bA�bA�JA�VA�bA�oA�VA�JA�bA�bA�oA�bA�JA�bA�{A�oA�bA�bA�{A��A�{A�oA�{A��A�{A�bA�bA�{A��A�oA�bA�bA�oA�{A�oA�bA�bA�bA�{A�{A�bA�VA�VA�JA�1A�%A�A�A�  A��A��mA��HA��/A��
A���A�ȴA�ƨA�RA�-A��A��A��AᗍA�7A�l�A�C�A��A�t�A�33A�bA�A�A��;A��#A���A�G�A��
A�z�A�
=A���A׉7A�-A��A֑hA�\)A���A��A�ZAӉ7A���A�VA�"�A��/Aѡ�A�^5A���A���A�~�A�&�A��/AΧ�A�r�A�XA�(�A��/A�l�A�&�A�1A�ĜȦ+A��Aɏ\A�-A�JA��AȰ!A�bNA��TA�hsA��A�bNA��A�`BA���Aě�A�"�A×�A´9A�oA�VA���A�n�A�ĜA�(�A���A��7A�E�A���A�`BA��mA��A�x�A�dZA�;dA���A���A�9XA���A�C�A���A�M�A���A�$�A�ƨA�E�A�VA��A�ƨA�bNA�5?A�A���A�r�A�+A��A���A���A��\A��uA�v�A��A�ƨA�dZA�{A��#A�r�A���A�|�A�G�A�"�A���A�l�A��A�Q�A��A�1A��/A��;A���A���A��+A�x�A�S�A�9XA�(�A��A��
A��RA��7A�hsA�G�A�5?A��A���A��A�ƨA�?}A��A��A�33A�n�A���A���A���A��A�  A�Q�A��A��A�n�A�  A�p�A���A���A�v�A�A�A��A���A��A��`A��`A��A���A���A�ƨA���A��RA��9A��FA���A��\A�z�A�jA�Q�A�
=A���A��A��mA��#A���A�A��jA��jA��^A��-A���A���A���A��uA��A�ffA�I�A�;dA�-A���A���A��A�ffA���A��mA�n�A��A��#A���A�x�A�C�A��A��9A�jA�1A��;A���A���A�v�A�dZA�O�A�7LA�&�A�VA���A��A��`A���A���A��-A���A���A��7A�t�A�XA�M�A�/A�A��A��/A�G�A���A��9A�S�A��TA�l�A�"�A��A��A��A���A��^A���A��`A��A�ĜA���A�bNA�?}A��`A�p�A�{A�ƨA�jA�$�A��
A�^5A��/A�jA�  A���A�7LA���A���A�|�A�bNA�?}A� �A�1A�  A���A���A���A�v�A�I�A��A��A���A��A��uA�t�A�\)A�A�A��A���A��A���A��jA��\A�(�A���A���A�l�A�
=A��
A���A��RA��FA���A��A�x�A�^5A�Q�A��AC�A}��A|��A|ffA{l�A{�Az��Ay�Ax�RAw?}Aux�At{As33Arz�Aq�FAqVAp�\ApVAp1'ApAo��AoƨAo�Ao�PAoXAn��An~�An �Am�wAmC�Al��AlZAk�mAk�Ak`BAk
=Aj�Aj�Ai�^AiS�AhĜAh$�Ag��Ag%AfE�Ae�Ae�-Ae\)Ad�/AdbNAc�;AcO�Ab�\Ab �Aa��Aax�Aa&�A`��A`9XA_��A_\)A_A^�!A^1A]O�A]�A\�A\^5A[�A[�AZ�uAZ�AY��AY33AX��AXv�AX(�AW�PAV��AV�+AVbNAVQ�AV$�AU��AU�AU�#AU�wAU�FAU�AU��AUC�AT�+AT �AS�^AR��AP�/AO"�AN=qAM�AMC�AM�AL�ALȴAL�RAL�!AL�9AL�9ALffAL$�AK��AK��AKhsAK�AJ��AJ��AJ�DAJQ�AJ1'AJ�AJ  AI�TAI|�AI/AH�`AH��AH��AH��AHȴAH��AHȴAHȴAH��AH��AH��AH��AH��AH�AH�AH�AH��AH��AH��AHv�AH(�AGp�AFI�AC��AA��A@��A@��A@��A@�uA@jA?�A?p�A??}A?�A>�A>�HA>��A>�jA>�!A>�\A>5?A=�A=A=�A=G�A=�A<�A<��A<9XA;�A;��A;G�A:�A:ĜA:��A:�A:jA:E�A9��A9��A9hsA9VA8�uA8Q�A85?A8JA7�;A7�A7t�A7?}A7�A7%A6��A6�A6�HA6��A6��A6ĜA6�RA6��A6z�A6r�A6~�A6��A6�!A6�RA6�!A6��A6$�A5|�A5O�A5\)A5dZA5t�A5|�A5|�A5t�A5l�A5hsA5l�A5p�A5p�A5l�A5t�A5|�A5|�A5t�A5x�A5t�A5l�A5O�A5K�A533A5�A4��A4�/A4ĜA4�DA4jA4jA4~�A4�A4�\A4��A4�A5��A5�A6$�A6(�A6n�A6�DA6�\A6�uA6�uA6��A6�/A7VA6�yA6��A6�A6A�A5�TA5A5��A5K�A5oA4�/A4�HA4��A4��A4�+A3�^A3�A2ȴA2(�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                             ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B��B�9B��B��B�$B�aB��B�B�B��BBB�B#nB*�B1�B6�B=�B@OBA BB�BC�BEBFBHBI�BK)BK�BL�BN<BNBL�BS�Bu%B	 4B	��B	��B	�B	ϫB	�6B	��B	�aB	�pB
�B
-�B
H�B
4nB
5tB
V�B
Y�B
R�B
NpB
6�B
$�B
�B
$B
GB
>wB
'B
�B
"B
�B	��B	�B	�XB	�wB	��B	�B	t�B	h�B	[�B	V�B	OvB	H�B	<�B	0�B	0�B	#:B	�B	B	�B	4B	xB	!�B	�B��B	�B	:B	VB	�B��B��B�"B	�B	5?B	@B	GEB	N�B	K�B	XB	��B	�B	�*B	�\B	��B	�B	ɆB	՛B	�2B	��B	�fB	��B	��B	�xB
�B
+B
B
4B

�B	�"B

�B
	�B
�B
 4B	��B
%B
�B
fB
�B
�B
'�B
8RB
8�B
9XB
<�B
=qB
;�B
<�B
=<B
:�B
8�B
@�B
DgB
EmB
EmB
F?B
GzB
I�B
H�B
I�B
K�B
N�B
M6B
N<B
P�B
OB
N�B
OvB
OB
P}B
OvB
P}B
OBB
P�B
P�B
Q�B
QNB
QNB
RTB
R�B
S�B
R�B
R�B
R B
Q�B
S[B
S[B
R�B
S[B
S�B
R�B
RTB
R�B
R�B
R�B
R B
RTB
R�B
UgB
R�B
Q�B
QB
Q�B
QB
O�B
Q�B
Q�B
Q�B
P�B
Q�B
R�B
U2B
W?B
W�B
V�B
U2B
S�B
R�B
P�B
OB
N�B
RTB
R B
Q�B
P}B
P�B
OB
MB
K�B
MB
NB
K�B
I�B
H�B
F�B
EB
F?B
F?B
C�B
CaB
B�B
A�B
EmB
GB
DgB
C�B
?�B
A�B
?�B
?HB
?}B
?B
=qB
<B
<�B
:�B
9XB
9$B
9�B
6FB
2�B
0�B
0�B
1[B
/�B
0�B
1�B
1�B
1'B
0�B
2-B
2�B
1[B
0�B
0UB
/B
.}B
0�B
1'B
/�B
0�B
1�B
0�B
0UB
0�B
0UB
1'B
/�B
/�B
/�B
.}B
.}B
.IB
.IB
.B
-B
-B
+�B
+�B
)�B
*eB
)*B
*�B
(�B
(�B
)_B
'�B
'�B
'�B
&�B
&B
$�B
$�B
#�B
%zB
"�B
"�B
!�B
 �B
�B
�B
�B
!-B
�B
VB
�B
�B
B
�B
B
~B
B
~B
xB
�B
�B
xB
�B
=B
�B
	B
=B
qB
7B
B
B
�B
�B
xB
B
xB
CB
�B
IB
IB
IB
�B
IB
�B
CB
B
qB
�B
CB
B
�B
�B
�B
�B
7B
�B
�B
�B
�B
7B
�B
�B
B
7B
	B
�B
�B
kB
1B
�B
_B
+B
�B
1B
�B
�B
SB
SB
�B
MB
{B
B
B
uB
�B
�B
uB
�B
B
B
B
�B
@B
B
�B
uB
uB
uB
uB
uB
�B
�B
�B
�B
uB
uB
�B
FB
�B
�B
�B
�B
�B
$B
�B
�B
�B
�B
+B
eB
B
7B
kB
7B
=B
B
�B
�B
�B
IB
B
�B
�B
�B
�B
~B
~B
B
�B
!B
�B
�B
!B
�B
VB
!B
�B
�B
�B
 \B
 \B
 �B
 �B
 �B
 �B
 �B
!-B
 �B
 �B
!-B
 �B
!�B
"�B
"�B
#nB
%B
%FB
%B
$�B
&�B
'B
&�B
'�B
'�B
'�B
($B
'�B
($B
'�B
'�B
'�B
'�B
($B
'�B
)_B
*eB
+6B
+kB
+�B
,�B
-�B
/�B
/�B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
1'B
0�B
1�B
1�B
1�B
1[B
1'B
0�B
0�B
1[B
0�B
1�B
2-B
2aB
2-B
2-B
2-B
2aB
2aB
2aB
2�B
3hB
3hB
3hB
3�B
49B
49B
49B
49B
4�B
4�B
5�B
5�B
5tB
5�B
6B
6zB
6zB
6�B
6�B
6�B
7B
7LB
7�B
8RB
8�B
8�B
9�B
9XB
9XB
:*B
:*B
9�B
:�B
:�B
;0B
<6B
=B
=B
=�B
>B
>B
>�B
>�B
>�B
?}B
@B
@OB
@�B
A B
AUB
A�B
B'B
A�B
B�B
C-B
B�B
B�B
C�B
D�B
D�B
D�B
D�B
D�B
D�B
EmB
EmB
E�B
E�B
E�B
F�B
GB
GEB
GEB
GEB
GEB
GEB
G�B
HB
HB
H�B
IB
IB
IRB
I�B
I�B
JXB
J�B
J�B
J�B
K^B
K)B
J�B
K�B
K)B
K�B
K�B
L0B
K�B
L�B
L�B
L�B
MjB
MjB
MjB
M�B
M�B
M�B
N�B
N<B
N<B
N�B
N�B
OB
OvB
OvB
O�B
OvB
OBB
PB
O�B
PB
P}B
P�B
QNB
QNB
Q�B
QNB
Q�B
Q�B
Q�B
R B
R�B
R�B
S[B
S[B
S&B
S[B
S[B
TaB
T�B
T�B
T�B
T�B
T�B
T�B
U2B
U�B
U�B
VB
W�B
XB
XB
YKB
YB
Y�B
Y�B
Y�B
YB
Y�B
Y�B
Y�B
ZQB
ZQB
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
Z�B
[#B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\)B
\)B
\�B
\�B
]dB
\�B
]dB
\�B
]/B
\�B
]dB
]�B
]�B
^jB
^B
^5B
^B
]�B
^5B
^�B
^�B
^�B
^�B
_B
^�B
_B
_B
^�B
^�B
_B
_;B
_pB
`BB
`�B
a|B
a|B
a|B
a�B
a|B
a|B
a|B
a�B
bB
bB
bNB
b�B
c B
c�B
c�B
d&B
c�B
c�B
c�B
dZB
dZB
dZB
dZB
d�B
dZB
d�B
f2B
e�B
f2B
f�B
f�B
f�B
f2B
gB
g�B
h
B
h�B
iyB
i�B
i�B
iyB
iyB
iyB
i�B
i�B
i�B
jKB
jB
k�B
l"B
l"B
lWB
l�B
l�B
m)B
m)B
m]B
m)B
m)B
m)B
m�B
m]B
ncB
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
o B
n�B
o�B
oiB
o�B
o�B
p;B
poB
p;B
qB
p�B
q�B
q�B
qvB
q�B
q�B
rGB
rGB
sB
r�B
r�B
sB
sB
sMB
s�B
s�B
s�B
tB
s�B
tTB
t�B
u%B
u�B
uZB
u�B
v`B
v`B
v�B
v�B
v�B
v�B
w2B
w�B
w�B
w�B
xB
w�B
x8B
x8B
xlB
xlB
x�B
yrB
y�B
y�B
zB
zB
zxB
zxB
zxB
zxB
z�B
{B
{B
{B
{B
{�B
{�B
{�B
|PB
|PB
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}VB
}�B
}VB
}VB
}VB
}�B
}�B
~(B
~]B
~]B
~�B
~]B
~�B
~�B
~�B
cB
� B
� B
�B
�iB
�iB
��B
��B
��B
��B
�;B
��B
��B
�oB
��B
��B
�AB
�uB
�uB
�B
�GB
�GB
�{B
�{B
��B
�{B
�MB
�MB
�B
�MB
�B
�MB
�B
�B
�MB
�SB
��B
��B
��B
��B
��B
�%B
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
�_B
��B
�B
��B
�B
�B
�7B
�B
��B
�	B
��B
�xB
�DB
�DB
�xB
��B
�B
�B
�B
��B
�~B
��B
��B
�B
��B
��B
��B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
��B
�"B
�"B
��B
�(B
��B
�(B
��B
��B
��B�0B�eB�B��B��B�6B�RB�B�-B��B��B�B�OB��B�LB��B�B��B��B�^B�[B��B�pBбB��B�B  B;BuB�B�B�B�B:B:BSB�B�B�BB�BOB$B&�B&�B%�B'�B(�B,B*eB,=B-CB.�B0UB2aB2�B1�B4�B4�B4�B33B6�B7�B7B6zB6�B8B:�B8�B7�B>�B@�B@OB>�B@�BAUB?�B?B@�B@�B?HB?�B@�BA�B@B?}B@�BB�BA�BA BA�BB�BB[BAUBB[BC�BA�BB�BD3BC�BB�BC-BD3BE9BEBCaBD�BEmBDgBD�BE�BE�BD�BE9BFtBFBE�BD�BF�BGEBF?BEBF?BH�BGzBGzBGzBG�BJ�BI�BH�BJ�BK)BI�BH�BI�BJ�BJ�BI�BI�BK�BLdBJ�BJ#BK�BLdBL0BJ�BJ�BL0BK)BJ�BK�BL0BM6BL�BL0BK�BL�BNBL�BK�BL0BM�BOBMjBM�BNpBOBBOBBM�BM�BOBBN�BM�BM�BM�BN�BN<BM6BL0BL�BMjBMjBMBK�BK^BL�BM�BMBMBMjBN<BOBQ�BU2B[WB]/B_�BbNBc�BjKBq�Bs�BzxB�B�uB��B�1B��B�=B��B�6B��B	YB	c�B	wfB	rB	p�B	t�B	~�B	��B
�B	��B	��B	�9B	�B	ǮB	��B	�9B	��B	ŢB	��B	��B	ںB	҉B	�aB	��B	�3B	�OB	�9B	�NB	�B	ȴB	ѷB	��B	�jB	�#B	�?B	ɆB	��B	��B	�0B	��B	��B	��B	��B
B	�
B	�gB	уB	�pB	�sB	�B	ҽB	�2B	��B	�B	�2B	��B	��B	�tB	�vB	҉B	�EB	��B	��B	�TB
7B
~B

�B
	lB
bB
(XB
5tB
0�B
/�B
.}B
)_B
(XB
2�B
+6B
EB
>BB
OvB
W�B
]/B
YKB
m�B
s�B
XB
^B
H�B
N<B
S�B
L�B
D�B
A�B
A�B
>wB
>�B
?}B
6�B
5?B
2�B
4nB
>�B
4�B
;�B
HB
=�B
4�B
1'B
3hB
/�B
-wB
+kB
B�B
49B
^jB
/�B
4�B
<jB
0�B
.IB
4B
/�B
-�B
-�B
2�B
3�B
>�B
,qB
0�B
3hB
/OB
1�B
+�B
,�B
/�B
,=B
'RB
=B
:^B
4�B
1�B
>�B
:�B
-�B
F?B
J�B
6FB
=B
/OB
D3B
T�B
aB
YKB
a|B
VmB
T�B
]dB
VmB
Z�B
YB
XyB
XEB
Z�B
X�B
X�B
X�B
Y�B
YB
W?B
YB
\�B
[#B
YB
Z�B
]dB
c B
W?B
V9B
S�B
T�B
V�B
R�B
S�B
S[B
T�B
O�B
Q�B
Q�B
P}B
Q�B
P}B
Q�B
QNB
LdB
J�B
OB
^jB
U�B
S�B
N�B
S&B
PB
F�B
B[B
A�B
<6B
B[B
;�B
9$B
GEB
9�B
6FB
1[B
33B
/OB
-CB
-B
,�B
($B
)�B
$�B
$�B
#nB
($B
!bB
!bB
"hB
�B
�B
�B
�B
�B
�B
_B
�B
1�B
CB
�B
�B
�B
�B
�B
�B

rB
B
�B
$�B
B
xB
:�B
H�B
N�B
Q�B
J�B
H�B
Q�B
OB
GB
?�B
EmB
>�B
;dB
A B
=qB
;0B
4nB
2�B
3hB
3hB
#�B
#�B
�B
 \B
B
qB
�B
�B
�B
�B
�B
$B
�B
�B
�B
�B
�B
�B
�B
_B
�B
B
B
B
B
	7B
�B	�PB
�B
oB	��B	�AB	�iB	�B	�"B	�B	�B	�B	��B	�;B	��B	�B	�B	��B	�?B	ɺB	��B	��B	�EB	�B	�B	��B	��B	��B	�B	��B	��B	�_B	�B	�B	��B	�VB	��B	�7B	�1B	��B	�	B	�7B	�{B	��B	�GB	~]B	zDB	~�B	sMB	y>B	wfB	r�B	l�B	poB	m�B	o B	k�B	ffB	k�B	lWB	]dB	\�B	_B	\]B	^�B	[WB	[�B	[WB	T,B	TaB	S�B	W?B	YKB	WsB	\�B	T�B	PHB	PB	\]B	OvB	F�B	E9B	U�B	Q�B	IB	K^B	E�B	EmB	B�B	B'B	B[B	9�B	;�B	?HB	:�B	4B	3hB	5tB	/�B	0!B	0�B	0!B	-wB	+kB	/B	3�B	8B	(XB	-�B	7LB	IRB	6FB	#�B	CB	kB	{B	�B	�B	oB	�B	B	+B	�B	�B	�B	B	�B	�B	4B	�B	uB	oB	uB	�B	oB	�B	MB	hB	�B	�B	�B	�B	B	�B	YB	IB	�B	=B		B		B	B	B	�B	�B	 �B	!�B	"�B	#�B	 �B	/OB	VB	T�B	�B	�B�	B��B�fB��B	B	uB��B��B	  B��B	{B	�B	�B	�B	B	�B	 B	B	B	�B	4B	FB	FB	VB	�B	hB	PB	B		�B	�B	�B	_B	1B	�B	GB	�B	AB��B�JB�]B��B��B��B�B�B�B�B��B��B�JB��B�B�B	 iB	 4B	B	%B	(B	�B	7B	IB	 'B	,B	7LB	2�B	3�B	4�B	6�B	8RB	:�B	=�B	?}B	@�B	A B	B�B	CaB	DgB	CaB	EB	HKB	K)B	J�B	N�B	N�B	P�B	N�B	OBB	N�B	N�B	J�B	J�B	PHB	I�B	I�B	GzB	M6B	MB	N<B	K�B	c�B	p�B	x�B	�SB	�fB	��B	��B	�	B	�OB	�IB	�IB	�qB	��B	��B	�!B	�zB	��B	��B	�B	��B	��B	��B	�eB	��B	��B	��B	��B	�YB	�-B	�_G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                             B��B��B�hB�hB��B�B�^B�B�B�pB��B�B�B�B#<B*PB1�B6uB=�B@?BABBzBC�BD�BE�BG�BI�BK.BK�BL�BNDBNBMmBVCB��B	�B	�5B	�oB	֏B	�B	��B	�B	�>B	��B
".B
U�B
r|B
U�B
f�B
aB
`HB
d�B
diB
?EB
.�B
0�B
�B
W�B
PB
.eB
 (B
B
�B	��B	�*B	�B	��B	�B	��B	�QB	tB	f�B	`�B	[
B	RB	A�B	7gB	D�B	(�B	yB	�B	{B	4B	�B	3�B	"�B	MB	�B	�B	WB	�B	YB�fB�CB	 B	59B	@B	H2B	P�B	I�B	TyB	��B	�8B	�|B	��B	��B	��B	�B	֭B	�vB	�B	�B	��B	��B	��B
CB
	�B
vB
4B
B	��B
�B
�B
	2B
{B	�$B
aB
�B
	�B
�B
�B
&<B
7�B
8�B
:^B
>aB
>�B
<<B
?B
@�B
<qB
9�B
C�B
F�B
F�B
F?B
G[B
IB
KtB
I�B
J�B
MB
P5B
M�B
O�B
Q�B
P1B
O�B
P:B
QB
Q�B
QB
Q�B
P�B
R�B
RB
RNB
Q�B
RXB
S�B
T B
TcB
R�B
R�B
RqB
R�B
T�B
TiB
S�B
UmB
T�B
R�B
S>B
TB
T5B
S�B
R�B
S/B
T9B
WVB
SB
RJB
RrB
SIB
Q�B
P�B
RYB
RHB
R.B
RB
S�B
R�B
UpB
W�B
X�B
W�B
V B
U�B
U7B
RB
O}B
N�B
SB
S2B
R9B
QnB
Q�B
O�B
M�B
LB
M�B
O^B
L�B
J�B
J%B
GAB
F�B
HB
G!B
D|B
D�B
D>B
BB
F�B
H�B
FrB
D�B
@yB
B�B
@bB
@B
@DB
@�B
>�B
=�B
=�B
;mB
9�B
:B
<.B
8!B
3�B
1�B
2(B
2�B
0�B
1AB
2%B
2B
2B
2�B
37B
4�B
2:B
1�B
0�B
/�B
/�B
24B
1�B
0B
1�B
2�B
1.B
1B
1�B
1�B
1�B
0>B
1[B
0�B
.�B
.�B
.�B
/�B
.�B
.0B
.%B
,�B
,�B
*�B
+�B
*�B
+QB
)�B
*!B
*tB
(�B
(�B
)B
'�B
&bB
%jB
%wB
%�B
'_B
$�B
$aB
#�B
!�B
 �B
 �B
 `B
$eB
 �B
 B
VB
yB
�B
�B
_B
�B
�B
�B
zB
�B
�B
B
B
�B
�B
�B
RB
1B
YB
)B
,B
1B
�B
�B
�B
�B
:B
B
�B
GB
sB
HB
6B
B
[B
B
�B
�B
)B
B
B
,B
�B
�B
]B
B
yB
nB
!B
�B
GB
B
�B
NB
B
�B
�B
B
}B
�B
�B
cB
�B
�B
iB
�B
'B
'B
�B
5B
B
B
�B
�B
rB
oB
0B
B
(B
�B
�B
B
B
NB
1B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
)B
�B
3B
�B
�B
B
BB
�B
�B
�B
�B
�B
LB
5B
WB
�B
�B
�B
�B
�B
B
,B
�B
jB
�B
�B
#B
B
�B
�B
B
B
�B
 {B
�B
B
UB
 AB
 yB
�B
vB
�B
�B
 �B
!B
!xB
!�B
!�B
!;B
!B
!_B
!�B
!|B
!�B
!mB
!bB
#CB
#DB
#^B
$�B
%�B
%�B
%gB
&`B
(IB
'wB
'jB
(RB
(fB
(�B
(iB
(B
(8B
(B
'�B
'�B
(B
(�B
)!B
*qB
*�B
+�B
+�B
,�B
-�B
/�B
1-B
0�B
1B
0�B
0�B
0�B
1B
1lB
1�B
2!B
1�B
2�B
2<B
1�B
1�B
1aB
1]B
1zB
2B
2B
2�B
2�B
2iB
26B
2TB
2cB
2�B
2�B
2�B
3iB
3�B
3�B
3�B
4[B
4mB
4aB
4uB
4�B
5PB
5B
6=B
5�B
5�B
5�B
6�B
6�B
6�B
7B
6�B
7B
7~B
7�B
8FB
8�B
9\B
9�B
9�B
9�B
9�B
:�B
:XB
:kB
;=B
;%B
;�B
<�B
=BB
=B
>=B
>YB
>�B
?B
>�B
?(B
?�B
@FB
@�B
AB
A�B
BB
BbB
BcB
B`B
CTB
C�B
C=B
C�B
D�B
EgB
D�B
D�B
D�B
EB
EPB
E�B
E�B
FB
E�B
FKB
GDB
GsB
G{B
GkB
GkB
G}B
G�B
H2B
HfB
H�B
I{B
I�B
IlB
I�B
JFB
JIB
J�B
KB
K:B
KB
KB
KCB
KGB
K�B
KtB
LB
LSB
LoB
L`B
M#B
L�B
MAB
NB
M�B
M�B
M�B
NB
NkB
O$B
N�B
N�B
OSB
OCB
O�B
O�B
O�B
O�B
O�B
O�B
PZB
P*B
P|B
QB
QNB
Q�B
Q�B
Q�B
Q�B
Q�B
R3B
RB
R�B
SB
SB
S�B
SuB
SlB
S�B
TB
UB
UpB
UEB
U2B
UB
U5B
UXB
U�B
VQB
VWB
WKB
X�B
X�B
X�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZB
Z!B
Z;B
Z�B
Z�B
Z�B
Z�B
Z�B
[B
[!B
[B
[B
[B
[�B
\�B
\B
[�B
\B
[�B
[�B
[�B
\�B
\�B
]DB
]SB
]�B
]/B
]�B
]%B
]IB
]!B
]�B
]�B
^>B
^�B
^IB
^iB
^0B
^%B
_B
_6B
_
B
_B
^�B
_B
_B
_B
_9B
^�B
_B
_]B
_�B
`cB
a%B
aiB
bB
a�B
a�B
a�B
a�B
a�B
a�B
b>B
b_B
b`B
b�B
c+B
c�B
c�B
dB
d9B
c�B
d%B
d*B
d�B
drB
d�B
d�B
d�B
d�B
e�B
f�B
f9B
f�B
g
B
f�B
f�B
f�B
g�B
hKB
h�B
iGB
i�B
i�B
i�B
i�B
i�B
i�B
i�B
jB
j6B
j�B
kMB
l�B
lZB
lVB
lxB
l�B
l�B
mAB
miB
m�B
mnB
mPB
meB
n;B
m�B
oB
n�B
oB
n�B
n�B
n�B
n�B
n�B
n�B
n�B
n�B
oB
oFB
oB
pB
o�B
o�B
p	B
p�B
p�B
p�B
qXB
q6B
rB
q�B
q�B
r	B
rB
rqB
r�B
s�B
s*B
sB
s?B
sbB
s�B
s�B
s�B
t/B
tEB
t7B
t�B
uZB
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
v�B
v�B
w�B
x	B
w�B
w�B
xB
x"B
xmB
x�B
x�B
x�B
y3B
y�B
y�B
y�B
zfB
zsB
z�B
z�B
z�B
z�B
{1B
{�B
{�B
{�B
{�B
{�B
|-B
|=B
|�B
|�B
|�B
|�B
}B
}B
|�B
}MB
~DB
}�B
}}B
}�B
}yB
}mB
}B
~B
~EB
~LB
~qB
~pB
~�B
~rB
~�B
~�B
#B
�B
�0B
�B
�B
��B
��B
��B
��B
��B
�	B
��B
��B
��B
��B
��B
�B
�dB
��B
��B
�dB
�{B
�{B
��B
��B
��B
�B
�B
��B
�tB
��B
�\B
�cB
�=B
�8B
��B
��B
�"B
��B
��B
��B
�B
�=B
��B
��B
��B
��B
��B
�)B
� B
�?B
�`B
�B
��B
��B
�6B
�B
��B
��B
�B
�MB
�GB
�B
��B
�TB
��B
�YB
�ZB
��B
��B
�)B
�(B
�)B
��B
��B
�B
�B
�LB
�&B
��B
��B
�>B
�BB
��B
��B
��B
��B
��B
��B
�B
��B
��B
�\B
�vB
��B
�kB
�>B
��B
�4B
��G�O�B�0B�eB�B��B��B�6B�RB�B�-B��B��B�B�OB��B�LB��B�B��B��B�^B�[B��B�pBбB��B�B  B;BuB�B�B�B�B:B:BSB�B�B�BB�BOB$B&�B&�B%�B'�B(�B,B*eB,=B-CB.�B0UB2aB2�B1�B4�B4�B4�B33B6�B7�B7B6zB6�B8B:�B8�B7�B>�B@�B@OB>�B@�BAUB?�B?B@�B@�B?HB?�B@�BA�B@B?}B@�BB�BA�BA BA�BB�BB[BAUBB[BC�BA�BB�BD3BC�BB�BC-BD3BE9BEBCaBD�BEmBDgBD�BE�BE�BD�BE9BFtBFBE�BD�BF�BGEBF?BEBF?BH�BGzBGzBGzBG�BJ�BI�BH�BJ�BK)BI�BH�BI�BJ�BJ�BI�BI�BK�BLdBJ�BJ#BK�BLdBL0BJ�BJ�BL0BK)BJ�BK�BL0BM6BL�BL0BK�BL�BNBL�BK�BL0BM�BOBMjBM�BNpBOBBOBBM�BM�BOBBN�BM�BM�BM�BN�BN<BM6BL0BL�BMjBMjBMBK�BK^BL�BM�BMBMBMjBN<BOBQ�BU2B[WB]/B_�BbNBc�BjKBq�Bs�BzxB�B�uB��B�1B��B�=B��B�6B��B	YB	c�B	wfB	rB	p�B	t�B	~�B	��B
�B	��B	��B	�9B	�B	ǮB	��B	�9B	��B	ŢB	��B	��B	ںB	҉B	�aB	��B	�3B	�OB	�9B	�NB	�B	ȴB	ѷB	��B	�jB	�#B	�?B	ɆB	��B	��B	�0B	��B	��B	��B	��B
B	�
B	�gB	уB	�pB	�sB	�B	ҽB	�2B	��B	�B	�2B	��B	��B	�tB	�vB	҉B	�EB	��B	��B	�TB
7B
~B

�B
	lB
bB
(XB
5tB
0�B
/�B
.}B
)_B
(XB
2�B
+6B
EB
>BB
OvB
W�B
]/B
YKB
m�B
s�B
XB
^B
H�B
N<B
S�B
L�B
D�B
A�B
A�B
>wB
>�B
?}B
6�B
5?B
2�B
4nB
>�B
4�B
;�B
HB
=�B
4�B
1'B
3hB
/�B
-wB
+kB
B�B
49B
^jB
/�B
4�B
<jB
0�B
.IB
4B
/�B
-�B
-�B
2�B
3�B
>�B
,qB
0�B
3hB
/OB
1�B
+�B
,�B
/�B
,=B
'RB
=B
:^B
4�B
1�B
>�B
:�B
-�B
F?B
J�B
6FB
=B
/OB
D3B
T�B
aB
YKB
a|B
VmB
T�B
]dB
VmB
Z�B
YB
XyB
XEB
Z�B
X�B
X�B
X�B
Y�B
YB
W?B
YB
\�B
[#B
YB
Z�B
]dB
c B
W?B
V9B
S�B
T�B
V�B
R�B
S�B
S[B
T�B
O�B
Q�B
Q�B
P}B
Q�B
P}B
Q�B
QNB
LdB
J�B
OB
^jB
U�B
S�B
N�B
S&B
PB
F�B
B[B
A�B
<6B
B[B
;�B
9$B
GEB
9�B
6FB
1[B
33B
/OB
-CB
-B
,�B
($B
)�B
$�B
$�B
#nB
($B
!bB
!bB
"hB
�B
�B
�B
�B
�B
�B
_B
�B
1�B
CB
�B
�B
�B
�B
�B
�B

rB
B
�B
$�B
B
xB
:�B
H�B
N�B
Q�B
J�B
H�B
Q�B
OB
GB
?�B
EmB
>�B
;dB
A B
=qB
;0B
4nB
2�B
3hB
3hB
#�B
#�B
�B
 \B
B
qB
�B
�B
�B
�B
�B
$B
�B
�B
�B
�B
�B
�B
�B
_B
�B
B
B
B
B
	7B
�B	�PB
�B
oB	��B	�AB	�iB	�B	�"B	�B	�B	�B	��B	�;B	��B	�B	�B	��B	�?B	ɺB	��B	��B	�EB	�B	�B	��B	��B	��B	�B	��B	��B	�_B	�B	�B	��B	�VB	��B	�7B	�1B	��B	�	B	�7B	�{B	��B	�GB	~]B	zDB	~�B	sMB	y>B	wfB	r�B	l�B	poB	m�B	o B	k�B	ffB	k�B	lWB	]dB	\�B	_B	\]B	^�B	[WB	[�B	[WB	T,B	TaB	S�B	W?B	YKB	WsB	\�B	T�B	PHB	PB	\]B	OvB	F�B	E9B	U�B	Q�B	IB	K^B	E�B	EmB	B�B	B'B	B[B	9�B	;�B	?HB	:�B	4B	3hB	5tB	/�B	0!B	0�B	0!B	-wB	+kB	/B	3�B	8B	(XB	-�B	7LB	IRB	6FB	#�B	CB	kB	{B	�B	�B	oB	�B	B	+B	�B	�B	�B	B	�B	�B	4B	�B	uB	oB	uB	�B	oB	�B	MB	hB	�B	�B	�B	�B	B	�B	YB	IB	�B	=B		B		B	B	B	�B	�B	 �B	!�B	"�B	#�B	 �B	/OB	VB	T�B	�B	�B�	B��B�fB��B	B	uB��B��B	  B��B	{B	�B	�B	�B	B	�B	 B	B	B	�B	4B	FB	FB	VB	�B	hB	PB	B		�B	�B	�B	_B	1B	�B	GB	�B	AB��B�JB�]B��B��B��B�B�B�B�B��B��B�JB��B�B�B	 iB	 4B	B	%B	(B	�B	7B	IB	 'B	,B	7LB	2�B	3�B	4�B	6�B	8RB	:�B	=�B	?}B	@�B	A B	B�B	CaB	DgB	CaB	EB	HKB	K)B	J�B	N�B	N�B	P�B	N�B	OBB	N�B	N�B	J�B	J�B	PHB	I�B	I�B	GzB	M6B	MB	N<B	K�B	c�B	p�B	x�B	�SB	�fB	��B	��B	�	B	�OB	�IB	�IB	�qB	��B	��B	�!B	�zB	��B	��B	�B	��B	��B	��B	�eB	��B	��B	��B	��B	�YB	�-B	�_G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                             <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<_�<� ?<�5A<�T<��<L%�<��7<�u�<�4�=+��<�c="(7=)�0=a�=Gǝ<>�F<#�
<�i{<�D<#�
<6M�<��<#�
<���<�56<#�
<#�
<#�
<9�<#�
<���<×�<:Pp<#�
<6J<Q��<M1�<C��<:��<N�-<2��<#�
<#�
<�$�<#�
<#�
<#�
<#�
<#�
<#�
<�W/<?�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2019113002251520191130022515IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019121016241220191210162412QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019121016241220191210162412QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020010906573220200109065732IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                