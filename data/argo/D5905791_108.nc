CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2022-02-02T12:26:03Z creation; 2022-09-06T18:25:48Z DMQC;      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20220202122603  20220907192129  5905791 5905791 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               l   lAA  AOAO7825_008765_108                 7825_008765_108                 2C  2C  DD  SOLO_II                         SOLO_II                         8765                            8765                            V2.6; SBE602 06Mar19            V2.6; SBE602 06Mar19            853 853 @ٶ�\��N@ٶ�\��N11  @ٶ���p;@ٶ���p;@4��X�@4��X��e/�>WS��e/�>WS�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?u@   @B�\@�  @��R@��R@�p�A ��A  A�RA,(�AAG�A`��A�  A��A�Q�A�  A��A�Q�A��A�  A��B�
B�B  B   B(  B0  B8  B@  BH  BP  BW�
B_�
Bh  Bp  Bx  B�  B��B��B�{B��B��B�  B��B�{B�(�B�  B�  B�  B�{B�{B�  B�{B��B�  B�{B�  B�{B��
B�  B�{B��B��B�  B�  B��B�{B�(�C   C��C��C��C��C	��C
=C{C�C
=C  C  C
=C
=C��C�HC   C"
=C$  C&  C(  C)��C+��C-��C0  C2
=C4
=C6
=C7�C9�C;�C=��C@
=CB
=CD
=CF
=CH  CJ
=CL
=CM��CO��CR
=CS��CU��CX  CY��C[��C^  C`
=Cb
=Cd
=Cf  Cg��Cj  Cl
=Cn{Cp{Cr
=Cs��Cv
=Cx
=Cz
=C|
=C~  C�  C�  C���C���C���C�  C�  C�C�C�C�C�  C���C���C�C�
=C�  C�  C�C�C���C���C�  C�C�C�C���C���C���C�C���C���C���C�C�
=C�
=C�
=C�C�C�  C�C���C���C�  C�C�  C�C�C�C�C�C���C���C�  C���C���C���C���C���C���C���C���C�C�  C���C���C�  C���C�  C�  C�  C���C�C�C�  C���C�  C�
=C�  C�  C�  C���C���C�  C���C���C���C�  C���C�  C�
=C�C�
=C�C�C���C�  C�
=C���C���C�  C�C�C�C�C�  C�  C���C���C�  C�  C�  C�  C�  C�  C�  C�  C���C���C�C�  C���C���C���C�  C�C�C�  D �D ��D�D��D  D� D  D� D  D� D  D� D  D� D  D��D��Dz�D	  D	��D
�D
� D�D��D  D� D�qD� D�D}qD  D}qD  D��D  D� D  D� D�D�D�qD}qD��DxRD��D}qD  D��D�qD��DD}qD��D� DD�D  D}qD  D� D  D��D  Dz�D�qD � D!  D!� D"�D"� D"�qD#}qD$  D$� D%  D%��D&  D&� D'  D'� D(  D(}qD(��D)z�D*  D*�D+�D+}qD,  D,��D-  D-� D-�qD.� D/  D/}qD0  D0� D1�D1� D2�D2��D3  D3� D4�D4� D4�qD5}qD6  D6z�D6�qD7� D7�qD8}qD9  D9�D:�D:� D;�D;��D<  D<��D=D=� D>  D>��D?  D?� D@  D@��DA�DA��DB  DB�DC  DC}qDC�qDD}qDD�qDE}qDE��DF� DG�DG}qDG��DH� DI�DI� DJ  DJ�DK�DK��DK��DL� DM  DMz�DN  DN� DN�qDO��DP�DP�DQ�DQ� DQ�qDRz�DR��DSz�DT�DT��DU  DU� DV�DV}qDW  DW��DX  DX�DY  DY}qDZ  DZ��D[  D[}qD\  D\��D]  D]� D]�qD^z�D^�qD_� D_�qD`z�Da  Da}qDa�qDb��Dc�Dc�Dd�Dd� De�De� Df  Df� Df��Dg� DhDh� Dh�qDi}qDj  Djz�Dj�qDk� Dl�Dl��Dm  Dm��Dn�Dn� Do  Do}qDo��Dp}qDqDq��Dr  Drz�Ds  Ds��Dt  Dt}qDt�qDu� DvDv�Dw�Dw}qDw�qDx��Dy�Dy��Dz�Dz� D{  D{�D|�D|� D}�D}��D~�D~��D�D��D�HD�>�D�}qD��qD��qD�@ D���D��HD�  D�@ D�~�D�� D���D�>�D�~�D�� D�  D�AHD�� D���D�HD�@ D�~�D���D��qD�@ D�� D�� D�  D�@ D���D�� D��qD�>�D�}qD���D�  D�>�D�~�D���D��qD�>�D��HD��HD�  D�@ D��HD��HD�  D�B�D��HD�� D�  D�>�D�}qD���D�HD�@ D��HD��HD��qD�@ D���D���D���D�AHD�� D��HD��D�B�D���D�D�HD�B�D���D��HD���D�@ D��HD���D��qD�>�D�� D��qD���D�@ D�~�D�� D�  D�AHD�� D���D�  D�@ D�~�D�� D�HD�AHD�� D�D�HD�AHD�� D��HD�HD�=qD�|)D���D���D�=qD�� D��HD��qD�>�D��HD��HD�  D�@ D��HD�� D���D�@ D���D�D�HD�AHD��HD��HD�HD�AHD��HD���D�  D�@ D�}qD���D���D�@ D�~�D��)D��qD�>�D�~�D�� D���D�>�D�~�D��HD��D�AHD�~�D�� D�  D�>�D�~�D���D��)D�=qD�}qD���D��qD�>�D��HD���D�  D�>�D�}qD���D�HD�@ D�~�D��HD�  D�@ D�~�D�� D��D�B�D�~�D���D�HD�B�D�~�D�� D�HD�=qD�~�D��HD�HD�AHD��HD��HD��D�@ D��HD���D��qD�@ D�� D�� D�  D�@ D�� D���D���D�>�D�~�D�� D��D�AHD�� D���D��qD�=qD�}qD�� D�HD�@ D�� D���D���D�=qD��HD���D�  D�<)D�|)D��qD�  D�AHD�~�D���D���D�>�D�~�D��HD�  D�@ D���D�� D�  D�B�D�~�D�� D��D�C�D���D�D�HD�AHD�� D���D�  D�@ D�� D�� D���D�=qD�}qD��qD��qD�@ D��HD�� D���D�@ D D��HD�  D�>�D�~�Dþ�D�HD�B�DāHD�� D�HD�@ Dŀ D��HD�  D�@ D�~�Dƾ�D��qD�>�Dǀ D�� D�  D�@ DȀ D�� D�HD�AHDɀ D�� D�  D�AHDʁHD�� D��D�AHDˀ D˾�D���D�AHD́HD�� D�HD�@ D�~�D�� D���D�>�D΀ D�D�HD�@ Dπ DϾ�D���D�=qD�~�D�� D���D�>�Dт�D��HD��qD�>�D҂�D���D�HD�>�DӀ D�� D�HD�B�DԂ�D�� D���D�=qD�~�Dվ�D��qD�@ Dր D�� D�HD�AHDׁHD�� D��qD�>�D�~�Dؾ�D�  D�@ Dـ DٽqD��)D�>�Dڀ D�� D�  D�AHDۀ D��HD�HD�@ D܁HD��HD�HD�@ D݁HD��HD�  D�@ Dހ D�� D�  D�AHD�~�D߽qD���D�@ D��HD�� D���D�>�D�}qD�� D�HD�=qD�~�D�� D�  D�@ D�~�D㾸D�HD�AHD� D�qD���D�>�D�HD�D�  D�@ D�~�D�� D��D�AHD� D�� D�  D�>�D�~�D��HD��D�AHD� D�� D�  D�>�D� D�� D�  D�AHD낏D��HD�  D�@ D�~�D�qD���D�@ D�~�D��qD���D�@ DD��HD�  D�B�DD�qD��qD�>�D�� D�D��qD�>�D� D��HD�HD�AHD� D�D�HD�AHD�}qD�qD���D�@ D�HD�D�HD�@ D���D�D�  D�>�D�� D���D���D�AHD��HD��HD�  D�@ D��HD��HD�HD�AHD�� D��HD��D�4{?\)?��?aG�?���?Ǯ?�ff@
=q@(�@333@G�@\(�@p��@�G�@�{@�z�@��
@�=q@�Q�@��R@���@�z�@�G�@�@�
=A   AA�A  A�A=qA ��A$z�A+�A/\)A6ffA:=qA@  AE�AJ=qAP  ATz�AZ�HA^�RAeAh��Ap  As�
Az=qA~{A��\A�(�A�\)A���A��
A�
=A���A��
A�p�A���A��\A�A�\)A�=qA�z�A��RA���A��A��RA�Q�A��A��A�Q�A��A���A�
=A�G�A�(�A�{A���Aʏ\A�A�\)A�=qA�(�AָRA���A�33A�p�A߮A��HA�(�A�\)A��A��
A�p�A�  A�=qA�z�A�ffA���A��A�p�B Q�B�B�RB�BG�B{B�B��B
{B\)BQ�B��B�RBQ�B�B�RB�B�B{B\)B��BB33B(�BB�\B (�B ��B"ffB#�B$��B&=qB'33B(��B)��B+33B,(�B-B.�RB0(�B1G�B2�\B3�
B4��B6ffB733B8��B9B;33B<Q�B=p�B?
=B?�
BAp�BB=qBC�BD��BF{BG�BHQ�BJ{BJ�HBLz�BMG�BN�HBO�
BQ�BR�\BS�BU�BV{BW�BX��BZ{B[\)B\��B]�B_
=B`��Ba��Bc33BdQ�BeBg
=BhQ�Bi�Bj�HBlz�Bmp�Bo33BpQ�Bq��Bs33Bt  Bu�Bw
=BxQ�ByBz�HB|��B}p�B
=B�{B���B��B�{B���B�\)B�(�B���B�p�B�(�B��RB��B�  B��HB�\)B�=qB���B��B�{B��RB�p�B��B��RB�33B�  B���B��B�  B�z�B�G�B��B�z�B�\)B��
B���B�G�B�  B���B�G�B�=qB��RB���B�=qB���B��
B�Q�B�G�B��
B���B�p�B�  B���B��B�Q�B��B��B���B�33B�  B��RB�\)B�Q�B��HB��B�ffB�
=B��B�z�B�G�B�(�B���B��B�=qB��HB�B�Q�B�G�B��B��\B�\)B�  B��HB�p�B�=qB���B��B�z�B�
=B��
B���B�33B�(�B��RB��B�Q�B��HB��
B�z�B�33B�{Bƣ�BǅB�(�B��HB��
B�Q�B��B�  B̏\B�\)B�(�BθRBυB�Q�B���B�B�Q�B�
=B�B�=qB�33B�B�ffB�33B�Bأ�B�33B�  B���B�\)B�=qB��HBݙ�B�z�B�
=B�  B��\B�G�B�{B�RB�B�=qB���B��
B�ffB�G�B�  B�\B�B�(�B�RB뙚B�(�B���B��B�=qB��HB�B�=qB��B�B�Q�B�33B�B�\B��B�B��\B�G�B��
B���B�\)B��B���B��B�  B���B��B�(�B�
=B��C (�C �\C ��C=qC�\C�
C=qC��C�
C=qC��C�
CG�C�\C�
CG�C�C�HCG�C�C��C=qC�\C  CQ�C��C	
=C	ffC	��C
{C
p�C
�C(�Cz�CC=qCp�C��C33Cz�C��C=qCp�C�
C=qCz�C�
C=qCz�C��C=qC�\C��C=qC��C�HCG�C�C��C\)CC  Cp�C�HC�C�C��CG�C��C�CffC�RC33Cz�C�
CQ�C�\C��C\)C��C  Cp�C�RC(�C��C�C33C�C
=CQ�CC �C \)C C!�C!\)C!C"{C"G�C"��C"�C#�C#p�C#�RC#�HC${C$p�C$�\C$C%
=C%33C%\)C%�C%��C&  C&Q�C&z�C&��C&�C'�C'=qC'�\C'C'�C(33C(ffC(�\C(�
C)
=C)(�C)z�C)�RC)�
C*(�C*\)C*�C*�
C+  C+(�C+z�C+�RC+�HC,{C,ffC,�C,C-{C-=qC-ffC-�RC-�C.{C.\)C.��C.C/
=C/G�C/p�C/�C/��C033C0\)C0��C0�C1{C1Q�C1��C1��C2  C2G�C2�C2�C3  C3=qC3ffC3��C3�C4{C4Q�C4��C4�
C4��C5=qC5�C5�C5�C633C6\)C6�C6�
C7
=C733C7z�C7C7�HC8�C8p�C8�C8�
C9{C9ffC9��C9C:{C:Q�C:z�C:�RC;
=C;33C;ffC;�RC;��C<�C<\)C<�C<�
C=  C=Q�C=��C=C=��C>G�C>�C>�C>��C?=qC?ffC?��C?�C@�C@G�C@��C@�
CA  CAG�CA�\CA�RCA��CBG�CBz�CB��CC  CC=qCCp�CC��CC��CD33CD\)CD��CD�CE33CE\)CE�\CE�HCF(�CFQ�CF�\CF�
CG�CGG�CGz�CG��CH{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                         ?u@   @B�\@�  @��R@��R@�p�A ��A  A�RA,(�AAG�A`��A�  A��A�Q�A�  A��A�Q�A��A�  A��B�
B�B  B   B(  B0  B8  B@  BH  BP  BW�
B_�
Bh  Bp  Bx  B�  B��B��B�{B��B��B�  B��B�{B�(�B�  B�  B�  B�{B�{B�  B�{B��B�  B�{B�  B�{B��
B�  B�{B��B��B�  B�  B��B�{B�(�C   C��C��C��C��C	��C
=C{C�C
=C  C  C
=C
=C��C�HC   C"
=C$  C&  C(  C)��C+��C-��C0  C2
=C4
=C6
=C7�C9�C;�C=��C@
=CB
=CD
=CF
=CH  CJ
=CL
=CM��CO��CR
=CS��CU��CX  CY��C[��C^  C`
=Cb
=Cd
=Cf  Cg��Cj  Cl
=Cn{Cp{Cr
=Cs��Cv
=Cx
=Cz
=C|
=C~  C�  C�  C���C���C���C�  C�  C�C�C�C�C�  C���C���C�C�
=C�  C�  C�C�C���C���C�  C�C�C�C���C���C���C�C���C���C���C�C�
=C�
=C�
=C�C�C�  C�C���C���C�  C�C�  C�C�C�C�C�C���C���C�  C���C���C���C���C���C���C���C���C�C�  C���C���C�  C���C�  C�  C�  C���C�C�C�  C���C�  C�
=C�  C�  C�  C���C���C�  C���C���C���C�  C���C�  C�
=C�C�
=C�C�C���C�  C�
=C���C���C�  C�C�C�C�C�  C�  C���C���C�  C�  C�  C�  C�  C�  C�  C�  C���C���C�C�  C���C���C���C�  C�C�C�  D �D ��D�D��D  D� D  D� D  D� D  D� D  D� D  D��D��Dz�D	  D	��D
�D
� D�D��D  D� D�qD� D�D}qD  D}qD  D��D  D� D  D� D�D�D�qD}qD��DxRD��D}qD  D��D�qD��DD}qD��D� DD�D  D}qD  D� D  D��D  Dz�D�qD � D!  D!� D"�D"� D"�qD#}qD$  D$� D%  D%��D&  D&� D'  D'� D(  D(}qD(��D)z�D*  D*�D+�D+}qD,  D,��D-  D-� D-�qD.� D/  D/}qD0  D0� D1�D1� D2�D2��D3  D3� D4�D4� D4�qD5}qD6  D6z�D6�qD7� D7�qD8}qD9  D9�D:�D:� D;�D;��D<  D<��D=D=� D>  D>��D?  D?� D@  D@��DA�DA��DB  DB�DC  DC}qDC�qDD}qDD�qDE}qDE��DF� DG�DG}qDG��DH� DI�DI� DJ  DJ�DK�DK��DK��DL� DM  DMz�DN  DN� DN�qDO��DP�DP�DQ�DQ� DQ�qDRz�DR��DSz�DT�DT��DU  DU� DV�DV}qDW  DW��DX  DX�DY  DY}qDZ  DZ��D[  D[}qD\  D\��D]  D]� D]�qD^z�D^�qD_� D_�qD`z�Da  Da}qDa�qDb��Dc�Dc�Dd�Dd� De�De� Df  Df� Df��Dg� DhDh� Dh�qDi}qDj  Djz�Dj�qDk� Dl�Dl��Dm  Dm��Dn�Dn� Do  Do}qDo��Dp}qDqDq��Dr  Drz�Ds  Ds��Dt  Dt}qDt�qDu� DvDv�Dw�Dw}qDw�qDx��Dy�Dy��Dz�Dz� D{  D{�D|�D|� D}�D}��D~�D~��D�D��D�HD�>�D�}qD��qD��qD�@ D���D��HD�  D�@ D�~�D�� D���D�>�D�~�D�� D�  D�AHD�� D���D�HD�@ D�~�D���D��qD�@ D�� D�� D�  D�@ D���D�� D��qD�>�D�}qD���D�  D�>�D�~�D���D��qD�>�D��HD��HD�  D�@ D��HD��HD�  D�B�D��HD�� D�  D�>�D�}qD���D�HD�@ D��HD��HD��qD�@ D���D���D���D�AHD�� D��HD��D�B�D���D�D�HD�B�D���D��HD���D�@ D��HD���D��qD�>�D�� D��qD���D�@ D�~�D�� D�  D�AHD�� D���D�  D�@ D�~�D�� D�HD�AHD�� D�D�HD�AHD�� D��HD�HD�=qD�|)D���D���D�=qD�� D��HD��qD�>�D��HD��HD�  D�@ D��HD�� D���D�@ D���D�D�HD�AHD��HD��HD�HD�AHD��HD���D�  D�@ D�}qD���D���D�@ D�~�D��)D��qD�>�D�~�D�� D���D�>�D�~�D��HD��D�AHD�~�D�� D�  D�>�D�~�D���D��)D�=qD�}qD���D��qD�>�D��HD���D�  D�>�D�}qD���D�HD�@ D�~�D��HD�  D�@ D�~�D�� D��D�B�D�~�D���D�HD�B�D�~�D�� D�HD�=qD�~�D��HD�HD�AHD��HD��HD��D�@ D��HD���D��qD�@ D�� D�� D�  D�@ D�� D���D���D�>�D�~�D�� D��D�AHD�� D���D��qD�=qD�}qD�� D�HD�@ D�� D���D���D�=qD��HD���D�  D�<)D�|)D��qD�  D�AHD�~�D���D���D�>�D�~�D��HD�  D�@ D���D�� D�  D�B�D�~�D�� D��D�C�D���D�D�HD�AHD�� D���D�  D�@ D�� D�� D���D�=qD�}qD��qD��qD�@ D��HD�� D���D�@ D D��HD�  D�>�D�~�Dþ�D�HD�B�DāHD�� D�HD�@ Dŀ D��HD�  D�@ D�~�Dƾ�D��qD�>�Dǀ D�� D�  D�@ DȀ D�� D�HD�AHDɀ D�� D�  D�AHDʁHD�� D��D�AHDˀ D˾�D���D�AHD́HD�� D�HD�@ D�~�D�� D���D�>�D΀ D�D�HD�@ Dπ DϾ�D���D�=qD�~�D�� D���D�>�Dт�D��HD��qD�>�D҂�D���D�HD�>�DӀ D�� D�HD�B�DԂ�D�� D���D�=qD�~�Dվ�D��qD�@ Dր D�� D�HD�AHDׁHD�� D��qD�>�D�~�Dؾ�D�  D�@ Dـ DٽqD��)D�>�Dڀ D�� D�  D�AHDۀ D��HD�HD�@ D܁HD��HD�HD�@ D݁HD��HD�  D�@ Dހ D�� D�  D�AHD�~�D߽qD���D�@ D��HD�� D���D�>�D�}qD�� D�HD�=qD�~�D�� D�  D�@ D�~�D㾸D�HD�AHD� D�qD���D�>�D�HD�D�  D�@ D�~�D�� D��D�AHD� D�� D�  D�>�D�~�D��HD��D�AHD� D�� D�  D�>�D� D�� D�  D�AHD낏D��HD�  D�@ D�~�D�qD���D�@ D�~�D��qD���D�@ DD��HD�  D�B�DD�qD��qD�>�D�� D�D��qD�>�D� D��HD�HD�AHD� D�D�HD�AHD�}qD�qD���D�@ D�HD�D�HD�@ D���D�D�  D�>�D�� D���D���D�AHD��HD��HD�  D�@ D��HD��HD�HD�AHD�� D��HD��G�O�?\)?��?aG�?���?Ǯ?�ff@
=q@(�@333@G�@\(�@p��@�G�@�{@�z�@��
@�=q@�Q�@��R@���@�z�@�G�@�@�
=A   AA�A  A�A=qA ��A$z�A+�A/\)A6ffA:=qA@  AE�AJ=qAP  ATz�AZ�HA^�RAeAh��Ap  As�
Az=qA~{A��\A�(�A�\)A���A��
A�
=A���A��
A�p�A���A��\A�A�\)A�=qA�z�A��RA���A��A��RA�Q�A��A��A�Q�A��A���A�
=A�G�A�(�A�{A���Aʏ\A�A�\)A�=qA�(�AָRA���A�33A�p�A߮A��HA�(�A�\)A��A��
A�p�A�  A�=qA�z�A�ffA���A��A�p�B Q�B�B�RB�BG�B{B�B��B
{B\)BQ�B��B�RBQ�B�B�RB�B�B{B\)B��BB33B(�BB�\B (�B ��B"ffB#�B$��B&=qB'33B(��B)��B+33B,(�B-B.�RB0(�B1G�B2�\B3�
B4��B6ffB733B8��B9B;33B<Q�B=p�B?
=B?�
BAp�BB=qBC�BD��BF{BG�BHQ�BJ{BJ�HBLz�BMG�BN�HBO�
BQ�BR�\BS�BU�BV{BW�BX��BZ{B[\)B\��B]�B_
=B`��Ba��Bc33BdQ�BeBg
=BhQ�Bi�Bj�HBlz�Bmp�Bo33BpQ�Bq��Bs33Bt  Bu�Bw
=BxQ�ByBz�HB|��B}p�B
=B�{B���B��B�{B���B�\)B�(�B���B�p�B�(�B��RB��B�  B��HB�\)B�=qB���B��B�{B��RB�p�B��B��RB�33B�  B���B��B�  B�z�B�G�B��B�z�B�\)B��
B���B�G�B�  B���B�G�B�=qB��RB���B�=qB���B��
B�Q�B�G�B��
B���B�p�B�  B���B��B�Q�B��B��B���B�33B�  B��RB�\)B�Q�B��HB��B�ffB�
=B��B�z�B�G�B�(�B���B��B�=qB��HB�B�Q�B�G�B��B��\B�\)B�  B��HB�p�B�=qB���B��B�z�B�
=B��
B���B�33B�(�B��RB��B�Q�B��HB��
B�z�B�33B�{Bƣ�BǅB�(�B��HB��
B�Q�B��B�  B̏\B�\)B�(�BθRBυB�Q�B���B�B�Q�B�
=B�B�=qB�33B�B�ffB�33B�Bأ�B�33B�  B���B�\)B�=qB��HBݙ�B�z�B�
=B�  B��\B�G�B�{B�RB�B�=qB���B��
B�ffB�G�B�  B�\B�B�(�B�RB뙚B�(�B���B��B�=qB��HB�B�=qB��B�B�Q�B�33B�B�\B��B�B��\B�G�B��
B���B�\)B��B���B��B�  B���B��B�(�B�
=B��C (�C �\C ��C=qC�\C�
C=qC��C�
C=qC��C�
CG�C�\C�
CG�C�C�HCG�C�C��C=qC�\C  CQ�C��C	
=C	ffC	��C
{C
p�C
�C(�Cz�CC=qCp�C��C33Cz�C��C=qCp�C�
C=qCz�C�
C=qCz�C��C=qC�\C��C=qC��C�HCG�C�C��C\)CC  Cp�C�HC�C�C��CG�C��C�CffC�RC33Cz�C�
CQ�C�\C��C\)C��C  Cp�C�RC(�C��C�C33C�C
=CQ�CC �C \)C C!�C!\)C!C"{C"G�C"��C"�C#�C#p�C#�RC#�HC${C$p�C$�\C$C%
=C%33C%\)C%�C%��C&  C&Q�C&z�C&��C&�C'�C'=qC'�\C'C'�C(33C(ffC(�\C(�
C)
=C)(�C)z�C)�RC)�
C*(�C*\)C*�C*�
C+  C+(�C+z�C+�RC+�HC,{C,ffC,�C,C-{C-=qC-ffC-�RC-�C.{C.\)C.��C.C/
=C/G�C/p�C/�C/��C033C0\)C0��C0�C1{C1Q�C1��C1��C2  C2G�C2�C2�C3  C3=qC3ffC3��C3�C4{C4Q�C4��C4�
C4��C5=qC5�C5�C5�C633C6\)C6�C6�
C7
=C733C7z�C7C7�HC8�C8p�C8�C8�
C9{C9ffC9��C9C:{C:Q�C:z�C:�RC;
=C;33C;ffC;�RC;��C<�C<\)C<�C<�
C=  C=Q�C=��C=C=��C>G�C>�C>�C>��C?=qC?ffC?��C?�C@�C@G�C@��C@�
CA  CAG�CA�\CA�RCA��CBG�CBz�CB��CC  CC=qCCp�CC��CC��CD33CD\)CD��CD�CE33CE\)CE�\CE�HCF(�CFQ�CF�\CF�
CG�CGG�CGz�CG��CH{G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                         @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�M�A�S�A�S�A�VA�ZA�XA�XA�VA�S�A�VA�Q�A�Q�A�Q�A�Q�A�Q�A�K�A�I�A�E�A�A�A�;dA�9XA�/A���A��`A��;A��/A��;A��/A��
A���A˟�A�x�A�33A���A��/A�ȴAʼjAʼjAʗ�A�I�A�VAɡ�AǾwA��HA�bNA�{Aå�A�l�A�M�A�5?A��yA�hsA��;A��A�jA��mA�A���A�33A�E�A���A��A���A��A�z�A�x�A�v�A�(�A��yA��!A�|�A���A��PA�bNA��TA��+A���A�9XA���A�ffA���A�G�A�{A���A��jA�C�A�ƨA��#A���A��PA��A�  A�1'A���A�33A�ĜA�Q�A�XA�"�A�{A�A�;dA��RA��A�ffA�{A��A�+A��A� �A��PA�bNA��A���A�5?A�ĜA��7A��A|�\Ay�Av��At(�Ao��Al9XAj-Ah�RAf1Ab��Ab-Aa��A`�!A_G�A_oA^$�AY�#AW��AT��AShsAR�!AR�\ARz�AP��AO��ANbNAM+AK��AH�yAGVAFAC;dAA�PA@��A?�A>ĜA=�-A<{A;+A9oA7&�A5�
A5`BA4��A4$�A3��A3l�A3&�A1��A.��A-�A,�\A,A�A+
=A*Q�A)l�A(��A'��A&Q�A%33A$��A#�mA"r�A!7LAt�A�-A+A�HA�+AVA9XA{AA�A  AbAJA�yA�A^5A�A
�9A	x�A�jA1A�#AVA�A�7AhsA?}AoA~�AK�AA A�@���@�M�@�J@���@�Z@�ƨ@�\)@�;d@�o@���@��@�x�@�7L@���@�(�@��@@��@���@�{@�j@�dZ@�-@�7L@蛦@��@�ƨ@�F@�
=@��T@�@�  @�V@�(�@���@��@�b@۶F@ڰ!@�n�@�ff@�V@�=q@�{@�@��@���@ى7@�/@أ�@��@�7L@ҟ�@�O�@Л�@�Z@�  @�E�@���@�Q�@��;@˅@��@���@�ȴ@�ff@ə�@�/@��`@���@ȼj@�A�@�n�@�1'@�t�@�
=@��@���@\@���@�G�@��@���@�(�@�(�@� �@�o@�=q@���@�@��^@�/@���@��@��@���@�Ĝ@��9@��@�A�@�$�@��;@�|�@�S�@�
=@��T@�&�@��u@�A�@�(�@���@��P@�dZ@�^5@���@�?}@���@���@�+@��@���@�5?@��@��-@�x�@��@��D@�9X@��m@���@�+@�V@�x�@�j@�(�@��w@�C�@��R@�n�@�=q@���@��#@���@�?}@��@�I�@� �@��@��m@���@���@��@��F@��@�1@�b@�b@��
@�\)@���@��!@�M�@�{@���@�G�@��@��/@���@��@��D@��D@��D@��@�r�@�z�@�z�@�bN@�(�@��@��F@���@�l�@�dZ@�S�@�@��R@�~�@�v�@�^5@�=q@�5?@�-@�@���@�?}@��@��/@��j@�Z@��m@�dZ@��@�v�@�~�@�~�@�^5@�5?@���@��@�p�@��-@���@���@��7@���@�X@�/@��@�Ĝ@��@��@��;@��@�|�@�S�@�K�@�K�@�K�@�K�@�S�@�S�@�K�@�33@�"�@��@��!@���@��h@�G�@�7L@�%@�Z@���@�S�@�;d@�"�@��y@���@�E�@��@�@��@��@�p�@�7L@�&�@�&�@��@��@���@�Ĝ@���@�z�@���@��P@�dZ@�33@�+@�"�@�o@��@��+@�5?@��#@��h@��@��@�&�@�  @���@��@�l�@�K�@��@��@���@��R@�-@��-@���@��@�7L@���@���@�Ĝ@�z�@��@��@�33@���@�^5@�-@���@�G�@�Ĝ@��j@���@�I�@���@��@��m@��;@��
@�ƨ@���@�t�@�\)@�+@��H@���@��!@���@���@���@�v�@�{@���@�hs@�?}@��@��@���@��9@��@�(�@�w@K�@~�y@~�R@~��@~��@~�+@~�+@~v�@~$�@}�@}�T@}�h@}`B@}p�@{�
@z�@z�H@z�!@z~�@z�@y�@w�@w�P@w
=@v��@vV@v$�@v@u@u�@uO�@uV@tZ@sƨ@st�@r�\@r=q@q�^@qX@q7L@p�`@p�@p �@o�w@o
=@nV@m?}@l�j@l(�@k��@kS�@i�@h�u@hr�@hQ�@g�;@g|�@g\)@g+@f�R@fv�@f5?@f{@e�-@e`B@d��@dj@d�@c�
@cS�@c33@c@b�H@b��@b��@b=q@b�@a�^@a��@aX@`�9@`�@`bN@`Q�@`  @_;d@\�@[�F@[�@[t�@[t�@[t�@[t�@[t�@[dZ@[C�@Z�@Z��@Z�@YG�@Y&�@Y�@Y%@X�`@XbN@X �@W�@W\)@WK�@W+@W
=@V�@Vȴ@Vv�@VE�@V@U��@U��@U?}@T�j@T(�@S��@So@R�!@Rn�@R-@Q�@Q�7@QX@PĜ@Pr�@PQ�@Pb@P  @O�@O�@O�@O��@O�P@O\)@O�@N�+@Nff@N$�@M�T@M@M@MV@Lz�@Lj@K��@K�@KdZ@KdZ@KC�@J�@J�\@J-@I��@I�7@IG�@I�@H�9@H��@H�u@H�u@H�u@H�@HbN@HA�@H  @G�@G
=@Fv�@Fv�@FV@E�@E��@E��@E?}@D�j@DI�@D9X@D(�@D�@C��@B��@B�\@B=q@Ahs@A%@A%@A%@A%@@��@@�`@@�`@@�`@@Ĝ@@ �@@  @?�w@?�@?��@?�P@?|�@?|�@?l�@>��@>ff@=�-@=/@<�@<(�@;��@;ƨ@;��@;��@;t�@;o@:~�@9�^@9x�@9hs@9X@97L@7�@6��@6{@5�-@5`B@4�/@4�@3�F@3"�@2�H@2��@2��@2�!@2~�@2^5@1�@1�^@1�7@1G�@1�@1%@0��@0�u@0Q�@01'@0 �@/��@/\)@/
=@.��@.�+@.$�@-�h@,�@+t�@+S�@+33@+o@*�!@*�@)�@)�7@)%@'��@'|�@'\)@&��@%�h@%�@$��@$��@$z�@$j@$�@#�
@#��@#S�@#o@"��@"�\@"^5@"�@!�#@!��@!X@!�@ ��@ �u@ r�@ Q�@ A�@ 1'@  �@   @�;@�w@;d@�@
=@��@�@�R@��@ff@V@$�@�@@�-@�@p�@`B@`B@`B@O�@V@Z@9X@1@�F@��@�\@M�@-@�@7L@��@A�@A�@1'@  @��@��@l�@+@�y@�R@ff@�T@�-@�h@V@�/@�/@��@�j@��@I�@9X@��@�
@��@��@t�@33@"�@@��@M�@�@J@J@��@�@�#@�^@�7@X@G�@7L@7L@&�@%@%@��@��@�`@�`@�`@�`@�`@�`@�9@r�@Q�@ �@��@|�@;d@�y@�+@�@��@�@��@��@j@j@Z@Z@Z@Z@I�@(�@��@ƨ@��@33@@
�@
��@
��@
�\@
�\@
~�@
~�@
~�@
^5@
M�@
J@	�@	�@	�#@	��@	x�@	hs@	�@��@r�@bN@Q�@�@K�@��@ȴ@��@�+@E�@{@@�@�T@@��@`B@?}@V@�@z�@j@jA�I�A�K�A�K�A�O�A�Q�A�Q�A�S�A�Q�A�ZA�M�A�ZA�O�A�\)A�VA�\)A�XA�ZA�VA�ZA�XA�ZA�VA�S�A�VA�Q�A�XA�Q�A�XA�Q�A�XA�Q�A�VA�O�A�S�A�Q�A�Q�A�S�A�O�A�VA�O�A�XA�O�A�S�A�M�A�S�A�Q�A�Q�A�S�A�Q�A�Q�A�Q�A�VA�Q�A�VA�O�A�S�A�M�A�O�A�K�A�M�A�M�A�K�A�K�A�G�A�M�A�G�A�K�A�G�A�M�A�K�A�I�A�I�A�I�A�I�A�?}A�E�A�A�A�C�A�A�A�G�A�C�A�C�A�?}A�=qA�A�A�;dA�?}A�9XA�?}A�5?A�=qA�9XA�7LA�9XA�7LA�;dA�7LA�;dA�7LA�;dA�33A�5?A�5?A�+A�+A�{A�
=A�A���A��A��A��A��A��mA��A��;A��HA��;A��/A��;A��/A��HA��/A��HA��/A��;A��/A��/A��;A��/A��;A��#A��HA��#A��HA��/A��;A��HA��/A��;A��/A��HA��#A��HA��A��/A��A��A��
A���A���A���A���A�ĜA�A˼jA˸RAˮA˩�AˮA˗�A˝�A˕�A˕�Aˏ\A�|�A�v�A�x�A�r�A�p�A�p�A�hsA�`BA��A� �A�oA�A���A���A���A���A���A���A��A��TA��#A��/A���A��#A���A���A���A�ƨA�ȴA�A�A���Aʺ^AʾwAʺ^A���AʼjAʾwA���Aʺ^AʾwAʶFAʺ^AʮAʡ�Aʛ�Aʇ+A�v�A�dZA�bNA�O�A�G�A�9XA�5?A�"�A��A��A�bA�{A�A��A���A�ȴAɥ�Aɕ�A�x�A���A�?}A��Aǥ�A�\)A�O�A� �A�bA�  A��mA�ƨAƬA�t�A�{Aŉ7A���AĲ-Aď\A�VA�&�A��A�oA���A�ȴA�ĜAþwAá�AÅAÅA�v�A�n�A�p�A�hsA�dZA�\)A�VA�VA�I�A�I�A�A�A�=qA�;dA�7LA�;dA�&�A�A�A���A���A��#A�A\A�n�A�hsA�`BA�A�A�?}A�33A�+A��TA��A��A���A�=qA��FA��`A�ȴA��FA��A��A�7LA�&�A�1A�%A��A��#A��A���A�ȴA�ȴA�A���A��jA��-A���A���A���A���A��PA�x�A�n�A�O�A�
=A���A���A��A��A�bNA���A�I�A�l�A�E�A�XA�1'A���A���A���A�M�A�1A��jA�r�A�K�A�oA�ĜA�v�A�+A���A���A��RA�t�A�bNA�;dA�%A��`A��9A�ZA�1'A�A��HA���A��PA�l�A�=qA��A���A��
A��A�x�A��A�l�A���A�A�A�(�A���A��/A���A�&�A��A��#A���A�ƨA��jA��!A��A��A��A��A��A���A��\A�x�A�M�A�(�A���A��-A���A���A�v�A�XA�$�A��yA���A�hsA��A��!A�ZA�+A�+A���A��+A�XA��A��yA���A��A���A���A���A���A���A��A�=qA�%A��A���A��9A�p�A�oA��;A��A�hsA�33A�  A��FA��A�XA��A�A���A��PA�|�A�p�A�ffA�bNA�I�A�7LA�VA���A��RA���A��+A�S�A�ffA�G�A�+A�-A�$�A��A��A�bA�VA�1A�A��TA���A���A�r�A�bNA�7LA�A�n�A���A�ĜA���A�p�A�"�A��A��mA��
A�ĜA��PA��/A�7LA���A�v�A��A�t�A�A��FA��-A�jA�9XA��A���A��A��A��A��mA��A�ƨA��RA��A���A���A���A���A��\A��hA��uA��PA�t�A�hsA�\)A�\)A�VA�Q�A�;dA�$�A���A��jA��DA�v�A�dZA�?}A��A��A���A���A�A��jA���A�v�A�E�A�/A�$�A��A��A�A���A��A��RA��9A��A���A��hA��A�ffA�\)A�\)A�Q�A�O�A�=qA�1'A�/A�$�A�VA�
=A�A���A��A��;A��jA���A��\A��A�v�A�p�A�ffA�^5A�\)A�Q�A�G�A�;dA�1'A��A��A���A��;A���A��`A���A��\A�/A�7LA�-A� �A�?}A��A�bA�{A��A�bA�oA��A��A�oA�{A��A��A�oA�{A��A�VA�bA�{A�oA�A��`A��!A��hA�|�A�5?A�33A�(�A�&�A��A�JA�
=A�1A���A��mA�ƨA���A�z�A�p�A�jA�A�A�{A���A��HA���A���A���A��-A���A���A��+A�t�A�O�A�K�A�A�A�=qA�-A�"�A�$�A� �A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                         A�M�A�S�A�S�A�VA�ZA�XA�XA�VA�S�A�VA�Q�A�Q�A�Q�A�Q�A�Q�A�K�A�I�A�E�A�A�A�;dA�9XA�/A���A��`A��;A��/A��;A��/A��
A���A˟�A�x�A�33A���A��/A�ȴAʼjAʼjAʗ�A�I�A�VAɡ�AǾwA��HA�bNA�{Aå�A�l�A�M�A�5?A��yA�hsA��;A��A�jA��mA�A���A�33A�E�A���A��A���A��A�z�A�x�A�v�A�(�A��yA��!A�|�A���A��PA�bNA��TA��+A���A�9XA���A�ffA���A�G�A�{A���A��jA�C�A�ƨA��#A���A��PA��A�  A�1'A���A�33A�ĜA�Q�A�XA�"�A�{A�A�;dA��RA��A�ffA�{A��A�+A��A� �A��PA�bNA��A���A�5?A�ĜA��7A��A|�\Ay�Av��At(�Ao��Al9XAj-Ah�RAf1Ab��Ab-Aa��A`�!A_G�A_oA^$�AY�#AW��AT��AShsAR�!AR�\ARz�AP��AO��ANbNAM+AK��AH�yAGVAFAC;dAA�PA@��A?�A>ĜA=�-A<{A;+A9oA7&�A5�
A5`BA4��A4$�A3��A3l�A3&�A1��A.��A-�A,�\A,A�A+
=A*Q�A)l�A(��A'��A&Q�A%33A$��A#�mA"r�A!7LAt�A�-A+A�HA�+AVA9XA{AA�A  AbAJA�yA�A^5A�A
�9A	x�A�jA1A�#AVA�A�7AhsA?}AoA~�AK�AA A�@���@�M�@�J@���@�Z@�ƨ@�\)@�;d@�o@���@��@�x�@�7L@���@�(�@��@@��@���@�{@�j@�dZ@�-@�7L@蛦@��@�ƨ@�F@�
=@��T@�@�  @�V@�(�@���@��@�b@۶F@ڰ!@�n�@�ff@�V@�=q@�{@�@��@���@ى7@�/@أ�@��@�7L@ҟ�@�O�@Л�@�Z@�  @�E�@���@�Q�@��;@˅@��@���@�ȴ@�ff@ə�@�/@��`@���@ȼj@�A�@�n�@�1'@�t�@�
=@��@���@\@���@�G�@��@���@�(�@�(�@� �@�o@�=q@���@�@��^@�/@���@��@��@���@�Ĝ@��9@��@�A�@�$�@��;@�|�@�S�@�
=@��T@�&�@��u@�A�@�(�@���@��P@�dZ@�^5@���@�?}@���@���@�+@��@���@�5?@��@��-@�x�@��@��D@�9X@��m@���@�+@�V@�x�@�j@�(�@��w@�C�@��R@�n�@�=q@���@��#@���@�?}@��@�I�@� �@��@��m@���@���@��@��F@��@�1@�b@�b@��
@�\)@���@��!@�M�@�{@���@�G�@��@��/@���@��@��D@��D@��D@��@�r�@�z�@�z�@�bN@�(�@��@��F@���@�l�@�dZ@�S�@�@��R@�~�@�v�@�^5@�=q@�5?@�-@�@���@�?}@��@��/@��j@�Z@��m@�dZ@��@�v�@�~�@�~�@�^5@�5?@���@��@�p�@��-@���@���@��7@���@�X@�/@��@�Ĝ@��@��@��;@��@�|�@�S�@�K�@�K�@�K�@�K�@�S�@�S�@�K�@�33@�"�@��@��!@���@��h@�G�@�7L@�%@�Z@���@�S�@�;d@�"�@��y@���@�E�@��@�@��@��@�p�@�7L@�&�@�&�@��@��@���@�Ĝ@���@�z�@���@��P@�dZ@�33@�+@�"�@�o@��@��+@�5?@��#@��h@��@��@�&�@�  @���@��@�l�@�K�@��@��@���@��R@�-@��-@���@��@�7L@���@���@�Ĝ@�z�@��@��@�33@���@�^5@�-@���@�G�@�Ĝ@��j@���@�I�@���@��@��m@��;@��
@�ƨ@���@�t�@�\)@�+@��H@���@��!@���@���@���@�v�@�{@���@�hs@�?}@��@��@���@��9@��@�(�@�w@K�@~�y@~�R@~��@~��@~�+@~�+@~v�@~$�@}�@}�T@}�h@}`B@}p�@{�
@z�@z�H@z�!@z~�@z�@y�@w�@w�P@w
=@v��@vV@v$�@v@u@u�@uO�@uV@tZ@sƨ@st�@r�\@r=q@q�^@qX@q7L@p�`@p�@p �@o�w@o
=@nV@m?}@l�j@l(�@k��@kS�@i�@h�u@hr�@hQ�@g�;@g|�@g\)@g+@f�R@fv�@f5?@f{@e�-@e`B@d��@dj@d�@c�
@cS�@c33@c@b�H@b��@b��@b=q@b�@a�^@a��@aX@`�9@`�@`bN@`Q�@`  @_;d@\�@[�F@[�@[t�@[t�@[t�@[t�@[t�@[dZ@[C�@Z�@Z��@Z�@YG�@Y&�@Y�@Y%@X�`@XbN@X �@W�@W\)@WK�@W+@W
=@V�@Vȴ@Vv�@VE�@V@U��@U��@U?}@T�j@T(�@S��@So@R�!@Rn�@R-@Q�@Q�7@QX@PĜ@Pr�@PQ�@Pb@P  @O�@O�@O�@O��@O�P@O\)@O�@N�+@Nff@N$�@M�T@M@M@MV@Lz�@Lj@K��@K�@KdZ@KdZ@KC�@J�@J�\@J-@I��@I�7@IG�@I�@H�9@H��@H�u@H�u@H�u@H�@HbN@HA�@H  @G�@G
=@Fv�@Fv�@FV@E�@E��@E��@E?}@D�j@DI�@D9X@D(�@D�@C��@B��@B�\@B=q@Ahs@A%@A%@A%@A%@@��@@�`@@�`@@�`@@Ĝ@@ �@@  @?�w@?�@?��@?�P@?|�@?|�@?l�@>��@>ff@=�-@=/@<�@<(�@;��@;ƨ@;��@;��@;t�@;o@:~�@9�^@9x�@9hs@9X@97L@7�@6��@6{@5�-@5`B@4�/@4�@3�F@3"�@2�H@2��@2��@2�!@2~�@2^5@1�@1�^@1�7@1G�@1�@1%@0��@0�u@0Q�@01'@0 �@/��@/\)@/
=@.��@.�+@.$�@-�h@,�@+t�@+S�@+33@+o@*�!@*�@)�@)�7@)%@'��@'|�@'\)@&��@%�h@%�@$��@$��@$z�@$j@$�@#�
@#��@#S�@#o@"��@"�\@"^5@"�@!�#@!��@!X@!�@ ��@ �u@ r�@ Q�@ A�@ 1'@  �@   @�;@�w@;d@�@
=@��@�@�R@��@ff@V@$�@�@@�-@�@p�@`B@`B@`B@O�@V@Z@9X@1@�F@��@�\@M�@-@�@7L@��@A�@A�@1'@  @��@��@l�@+@�y@�R@ff@�T@�-@�h@V@�/@�/@��@�j@��@I�@9X@��@�
@��@��@t�@33@"�@@��@M�@�@J@J@��@�@�#@�^@�7@X@G�@7L@7L@&�@%@%@��@��@�`@�`@�`@�`@�`@�`@�9@r�@Q�@ �@��@|�@;d@�y@�+@�@��@�@��@��@j@j@Z@Z@Z@Z@I�@(�@��@ƨ@��@33@@
�@
��@
��@
�\@
�\@
~�@
~�@
~�@
^5@
M�@
J@	�@	�@	�#@	��@	x�@	hs@	�@��@r�@bN@Q�@�@K�@��@ȴ@��@�+@E�@{@@�@�T@@��@`B@?}@V@�@z�@jG�O�A�I�A�K�A�K�A�O�A�Q�A�Q�A�S�A�Q�A�ZA�M�A�ZA�O�A�\)A�VA�\)A�XA�ZA�VA�ZA�XA�ZA�VA�S�A�VA�Q�A�XA�Q�A�XA�Q�A�XA�Q�A�VA�O�A�S�A�Q�A�Q�A�S�A�O�A�VA�O�A�XA�O�A�S�A�M�A�S�A�Q�A�Q�A�S�A�Q�A�Q�A�Q�A�VA�Q�A�VA�O�A�S�A�M�A�O�A�K�A�M�A�M�A�K�A�K�A�G�A�M�A�G�A�K�A�G�A�M�A�K�A�I�A�I�A�I�A�I�A�?}A�E�A�A�A�C�A�A�A�G�A�C�A�C�A�?}A�=qA�A�A�;dA�?}A�9XA�?}A�5?A�=qA�9XA�7LA�9XA�7LA�;dA�7LA�;dA�7LA�;dA�33A�5?A�5?A�+A�+A�{A�
=A�A���A��A��A��A��A��mA��A��;A��HA��;A��/A��;A��/A��HA��/A��HA��/A��;A��/A��/A��;A��/A��;A��#A��HA��#A��HA��/A��;A��HA��/A��;A��/A��HA��#A��HA��A��/A��A��A��
A���A���A���A���A�ĜA�A˼jA˸RAˮA˩�AˮA˗�A˝�A˕�A˕�Aˏ\A�|�A�v�A�x�A�r�A�p�A�p�A�hsA�`BA��A� �A�oA�A���A���A���A���A���A���A��A��TA��#A��/A���A��#A���A���A���A�ƨA�ȴA�A�A���Aʺ^AʾwAʺ^A���AʼjAʾwA���Aʺ^AʾwAʶFAʺ^AʮAʡ�Aʛ�Aʇ+A�v�A�dZA�bNA�O�A�G�A�9XA�5?A�"�A��A��A�bA�{A�A��A���A�ȴAɥ�Aɕ�A�x�A���A�?}A��Aǥ�A�\)A�O�A� �A�bA�  A��mA�ƨAƬA�t�A�{Aŉ7A���AĲ-Aď\A�VA�&�A��A�oA���A�ȴA�ĜAþwAá�AÅAÅA�v�A�n�A�p�A�hsA�dZA�\)A�VA�VA�I�A�I�A�A�A�=qA�;dA�7LA�;dA�&�A�A�A���A���A��#A�A\A�n�A�hsA�`BA�A�A�?}A�33A�+A��TA��A��A���A�=qA��FA��`A�ȴA��FA��A��A�7LA�&�A�1A�%A��A��#A��A���A�ȴA�ȴA�A���A��jA��-A���A���A���A���A��PA�x�A�n�A�O�A�
=A���A���A��A��A�bNA���A�I�A�l�A�E�A�XA�1'A���A���A���A�M�A�1A��jA�r�A�K�A�oA�ĜA�v�A�+A���A���A��RA�t�A�bNA�;dA�%A��`A��9A�ZA�1'A�A��HA���A��PA�l�A�=qA��A���A��
A��A�x�A��A�l�A���A�A�A�(�A���A��/A���A�&�A��A��#A���A�ƨA��jA��!A��A��A��A��A��A���A��\A�x�A�M�A�(�A���A��-A���A���A�v�A�XA�$�A��yA���A�hsA��A��!A�ZA�+A�+A���A��+A�XA��A��yA���A��A���A���A���A���A���A��A�=qA�%A��A���A��9A�p�A�oA��;A��A�hsA�33A�  A��FA��A�XA��A�A���A��PA�|�A�p�A�ffA�bNA�I�A�7LA�VA���A��RA���A��+A�S�A�ffA�G�A�+A�-A�$�A��A��A�bA�VA�1A�A��TA���A���A�r�A�bNA�7LA�A�n�A���A�ĜA���A�p�A�"�A��A��mA��
A�ĜA��PA��/A�7LA���A�v�A��A�t�A�A��FA��-A�jA�9XA��A���A��A��A��A��mA��A�ƨA��RA��A���A���A���A���A��\A��hA��uA��PA�t�A�hsA�\)A�\)A�VA�Q�A�;dA�$�A���A��jA��DA�v�A�dZA�?}A��A��A���A���A�A��jA���A�v�A�E�A�/A�$�A��A��A�A���A��A��RA��9A��A���A��hA��A�ffA�\)A�\)A�Q�A�O�A�=qA�1'A�/A�$�A�VA�
=A�A���A��A��;A��jA���A��\A��A�v�A�p�A�ffA�^5A�\)A�Q�A�G�A�;dA�1'A��A��A���A��;A���A��`A���A��\A�/A�7LA�-A� �A�?}A��A�bA�{A��A�bA�oA��A��A�oA�{A��A��A�oA�{A��A�VA�bA�{A�oA�A��`A��!A��hA�|�A�5?A�33A�(�A�&�A��A�JA�
=A�1A���A��mA�ƨA���A�z�A�p�A�jA�A�A�{A���A��HA���A���A���A��-A���A���A��+A�t�A�O�A�K�A�A�A�=qA�-A�"�A�$�A� �A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                         ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B�B��B�`B��B�`B�`B��B�B�,B�B�`B�`B�`B��B��B��B�fB�B�8B�8B�
B�B�B�]B��B��B�/B�cB�oB��B�B��B�+B��B�>B��B�B�.B�.B�]BuB%B��B�B�B �B�BB�B�B�BB#�B!bB!-BVBOBOB$tB,�B(�B&�B"hB!�B�B=B�BbB�B�B 4B�B�B�HB��B��B�B�gB�#BуB��BƨB��B��B�UB��B�VB{BwfBtTBg�B]�BVmBP�BK�BD�BA�B,=B*�B)�B"hBqB:B
rB�B��B��B�B�9B��B�+B~]Bt�Bh>BVBB�B%�BJB
�B
�fB
֡B
�UB
�RB
��B
��B
�fB
o�B
iDB
ffB
c B
WsB
S�B
OBB
B�B
3�B
)�B
�B
+B
�B
�B
\B
	lB
uB	��B	��B	�>B	�|B	�)B	�
B	�KB	�9B	��B	�dB	��B	�qB	��B	��B	��B	��B	��B	��B	��B	��B	�\B	��B	�B	��B	y�B	tB	s�B	p�B	m]B	i�B	gB	cTB	a�B	[�B	W?B	W?B	R B	MjB	LdB	F?B	B�B	A B	A B	>�B	>B	;�B	<6B	3�B	1�B	-�B	)�B	&�B	#nB	#:B	 \B	%�B	VB	 �B	�B	�B	 �B	 �B	!bB	�B	�B	�B	IB	�B	#nB	�B	�B	%FB	+B	*�B	+kB	,qB	,�B	,qB	,�B	-CB	.B	1'B	0�B	/�B	0�B	6FB	5?B	3�B	2aB	33B	5�B	6�B	>B	>B	?�B	A B	A B	@OB	C�B	F?B	HB	F�B	J�B	O�B	T�B	VB	YB	[�B	_pB	_;B	_pB	_�B	`B	`BB	`B	_�B	_�B	`B	`vB	b�B	h
B	l�B	s�B	s�B	sB	r|B	r�B	yrB	x�B	xB	w�B	yrB	y�B	zDB	zxB	{�B	~�B	~�B	~(B	}�B	|�B	�B	��B	��B	��B	�~B	�JB	�B	�~B	��B	��B	�bB	� B	�{B	��B	�MB	�	B	�qB	��B	�VB	��B	��B	��B	�OB	�B	��B	�B	�xB	�CB	�B	�FB	��B	��B	�tB	�FB	��B	�wB	��B	�B	��B	ȀB	ʌB	��B	�gB	רB	خB	��B	�/B	� B	�B	�mB	�B	��B	��B	�/B	�B	�B	�+B	��B	��B	�B	�.B
�B
�B
�B
�B
	lB

�B
�B
�B
{B
�B
�B
CB
$B
%�B
'�B
*0B
*�B
0�B
1[B
2�B
5tB
8�B
9�B
:*B
>�B
D�B
I�B
L�B
MB
OvB
PB
P�B
U�B
V�B
Z�B
[#B
^5B
`vB
`vB
`vB
`�B
aHB
aB
aB
a�B
cTB
e,B
f�B
h
B
iB
iDB
iB
jB
lWB
m�B
m�B
o B
o�B
o�B
o�B
o�B
q�B
rB
rGB
rB
q�B
rB
qAB
r�B
u�B
z�B
|�B
}�B
��B
��B
�MB
��B
��B
��B
�xB
�xB
�xB
�(B
�B
�uB
��B
�MB
��B
��B
�1B
�B
��B
��B
�=B
�=B
�=B
�=B
�=B
�	B
�=B
�=B
�qB
��B
��B
�B
��B
��B
��B
�hB
��B
�zB
��B
�B
��B
��B
��B
��B
�B
�B
�B
��B
�B
�CB
��B
��B
�B
��B
��B
�B
��B
�!B
�9B
�hB
��B
�nB
�9B
�9B
�9B
�B
��B
��B
��B
��B
�RB
�B
��B
�qB
��B
��B
��B
�BB
�wB
�BB
��B
�HB
� B
�[B
�'B
B
�aB
��B
��B
ÖB
�3B
��B
�9B
�B
��B
ȀB
�RB
ɆB
�dB
��B
��B
��B
�6B
͟B
͟B
��B
��B
��B
�B
�pB
�<B
�pB
�pB
�vB
�vB
�vB
�vB
�BB
�B
�vB
бB
�B
�NB
�NB
�NB
��B
ѷB
ѷB
� B
�&B
��B
��B
ԕB
ԕB
�,B
ԕB
��B
ԕB
�aB
��B
��B
�2B
՛B
�2B
�aB
�EB
רB
רB
��B
�B
�yB
ٴB
�QB
�B
��B
��B
�WB
�#B
�WB
یB
یB
��B
��B
�/B
ݘB
�B
��B
�jB
�;B
�;B
�;B
�pB
��B
��B
�vB
��B
��B
�B
� B
�B
� B
�&B
�2B
��B
��B
��B
��B
��B
�B
�
B
�B
�B
��B
�B
�DB
�B
�B
�B
�B
�B
�B
�B
�B
��B
�"B
�B
��B
��B
��B
�B
�/B
��B
� B
�B
�B
�B
�;B
��B
��B
�B
�B
�B
�B
�B
�MB
�MB
�MB
�B
�B
�B
��B
��B
��B
�B
�B
�ZB
��B
��B
��B
��B
��B
��B
�+B
��B
�`B
�+B
�+B
�`B
�+B
��B
��B
�fB
�fB
��B
��B
��B
��B
�B
��B
�B
��B
��B
��B
�>B
�rB
�>B
�rB
�rB
�rB
��B
��B
�DB
�B
�B
�B
��B
�JB
��B
��B
��B
��B
��B
�VB
�VB
��B
�VB
��B
�VB
��B
��B
��B
��B
�(B
��B
��B
��B
��B
��B
��B
��B
��B  B
�cBoBBBB;BoB�BBB�B{BGBGBMB�BBGB�B�B�BSB�B�B�B�B�B%B_B+B_B+B+B�B�B�B�BfB�B�BfB_B�B�B�B�B�B�B1B�B	�B	lB	7B	BfBxBB�B�BPB�B"B�B�B(B(B\B\B\B\B.BbBbB�B B�B B�BBB�BoBoB�B�B@B@B�B�B�B�B�B�BBSBSB�B�B_B�B�B�BB�B	B	BqB=B�BBBCBxBxB�B�BIBIBIB�B�B�B�B�B�B�B�B�B�B�B�B \B \B \B 'B \B \B �B �B �B �B!-B!bB!�B!�B!�B!�B!�B!�B!�B!�B"�B"�B"�B#B$@B#�B$tB$tB%B&�B'B'RB'RB'�B'�B'RB'RB'�B'�B($B(XB(�B)_B)*B)_B*0B)�B)�B)�B)�B)�B*0B*0B*�B*�B+B*�B+6B+6B+6B+6B,B,=B,qB,�B,qB,qB,�B,�B,�B-CB-wB-CB-wB-wB-�B-�B-�B-�B-�B-�B.B.B-�B.B-�B-�B.}B.IB.�B/B.�B.�B/B/�B0�B0�B1�B1�B1�B1�B2-B2-B1�B1�B1�B1�B2aB2aB2aB2�B3hB3hB3�B3�B3�B4B4B49B49B49B4nB4�B5tB5tB5?B5?B5tB5tB5?B5�B6�B6�B6zB6�B6�B8�B8�B8�B9XB9�B:^B:^B:*B:^B:^B:�B:�B;0B;�B<B<�B<�B<�B=B��B��B�B�B��B�B��B�B�B��B��B�`B��B�`B�B��B�B�`B�ZB�B��B�2B�`B��B��B��B��B��B�fB�B�fB�B�2B�,B�,B�2B��B�2B�&B�B��B��B�&B��B�ZB��B�,B�,B��B�`B��B��B�2B�ZB�B�&B�8B�`B�fB�`B�B��B��B�B��B�mB�,B�B�`B�B��B��B�B�B�B��B�B�B�B�,B�8B�B�B�
B�B�
B��B�>B�2B�DB�B�mB�B�8B�
B�B�>B�fB�
B��B��B��B�>B�B�B�WB�B�B�B�B�B��B�KB��B�QB�B�B�)B��B��B�/B�WB�/B�"B�/B�]B��B��B�B�cB�)B��B��B��B��B�B��B��B�B�B��B�]B� B��B�iB�)B� B�/B�/B�iB��B�B�cB�B�5B�B��B�B�GB�B��B��B�B�vB�|B�`B��B��B�|B�|B�vB�|B�B��B�ZB��B��B�%B�2B��B��B��B��B��B��B�	B��B�8B��B�B�B�8B��B�lB��B�xB��B��B��B��B�xB�JB��B��B��B�B��B��B;B��B��BB��B��B�BB �B��B�(B�B��B�VB�]B��B  B�BGB��B�B�"B�B�B"B iBYBB��B��B�B�cB��B�]B��B�.B
rB�B_B1BB	B�B��B��B�B��B��B��BBMB��BAB�B 4BAB�BBAB �B{BB�BGB  BB�cBB�B iBBB	B�BMB
�B�B�B	�BMB%BB{B�BqB �B7B+�B8�B�B�B�B#B%�B�B%�B�B#�B�B \B \B�BB 'BVB!B!B 'B�B�BxB~B�B	B!bB#�B7B�B$B�B-CB�B7�B�Bu%B8�B \B.�B�B,qB&�B'RB$�B,B"hB'�B&�B,qB'�B 'B"�B"�B 'B~B!�B"hB �B"�B$B�B�B�B!-B	BOB�BeB�B_B�B�B&LB!�B-�B@B�BB@BqB"B�B~B�B	B�B1B+B�B�B�B�BSB_BfBB1B�BuB�B��B iB�B �B��B�B�B��B��B�|B��B+B�)B�>B�8B��B�B��B�NB�#B��BܒB��B�5B�dB�B�jB�?BٴBӏB�B�B�B��B��B�tB�HB�^B��BȀBΥBǮBÖB�BĜB��B��B��B�TB�6B��B�WB͟B͟BΥB��B��B�aB��B�?BɆBȴB�B�B�B��B��B�[B��B�6B�B��B��B�mB�aB�?B�hB�hB��B��B�3B��B�LB�LB�$B�OB�XB��B�{B��B��B�:B�B��B�{B��B�;BzDB.BzDB{B{�B|�ByrBz�Bz�BwfBw�BxBtTBw�Bt�Bu%Bv�B}"Bu�Bu�Bs�Br�BrBv�Br�By�Bw�Bl"BkBjBk�BjBk�Bb�Bb�BbNBa�B`�Bf2Bh
B\�B\�BZ�BYB\�BW�BW�BbBWsBVmBR�BV�BV�BT�BQNBQ�BVBQ�BQNBPHBM�BP�BP�BL�BNBM�BI�BL�BM�BI�BJ#BH�BG�BFBGBEmBCaBD3BE�BB�BB[BA�B@OB@OB?HBI�B^5B8�B5tBIRB,�B/B0!B(�B0�B.B*0B)�B+�B*�B*�B*0B,�B)�B)�B)�B*eB(�B)�B)�B(�B'RB'B,B$�B#�B=<BMB�BB�B�B�BB�B�B�B!B!�BVB=B�B�B �B�B�B4B�B�B�B�B�B�B�B	�B
�B�BfBB
=B�B�B�B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                         G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                         G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202209061825322022090618253220220906182532202209061825322022090618253220220906182532SI  SI  ARFMARFM                                                                                                                                                2022020212260320220202122603IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022021211005720220212110057QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022021211005720220212110057QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022090607225920220906072259IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022090618253920220906182539IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022090618253920220906182539IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022090618253920220906182539IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                