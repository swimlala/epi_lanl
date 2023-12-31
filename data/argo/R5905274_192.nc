CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:39Z creation      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.2    Conventions       Argo-3.2 CF-1.6    featureType       trajectoryProfile         @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
_FillValue                    6�   FORMAT_VERSION                 	long_name         File format version    
_FillValue                    6�   HANDBOOK_VERSION               	long_name         Data handbook version      
_FillValue                    6�   REFERENCE_DATE_TIME                 	long_name         !Date of reference for Julian days      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_CREATION                   	long_name         Date of file creation      conventions       YYYYMMDDHHMISS     
_FillValue                    6�   DATE_UPDATE                 	long_name         Date of update of this file    conventions       YYYYMMDDHHMISS     
_FillValue                    6�   PLATFORM_NUMBER                   	long_name         Float unique identifier    conventions       WMO float identifier : A9IIIII     
_FillValue                    6�   PROJECT_NAME                  	long_name         Name of the project    
_FillValue                  �  6�   PI_NAME                   	long_name         "Name of the principal investigator     
_FillValue                  �  7p   STATION_PARAMETERS           	            	long_name         ,List of available parameters for the station   conventions       Argo reference table 3     
_FillValue                  `  7�   CYCLE_NUMBER               	long_name         Float cycle number     conventions       =0...N, 0 : launch cycle (if exists), 1 : first complete cycle      
_FillValue         ��        8P   	DIRECTION                  	long_name         !Direction of the station profiles      conventions       -A: ascending profiles, D: descending profiles      
_FillValue                    8X   DATA_CENTRE                   	long_name         .Data centre in charge of float data processing     conventions       Argo reference table 4     
_FillValue                    8\   DC_REFERENCE                  	long_name         (Station unique identifier in data centre   conventions       Data centre convention     
_FillValue                  @  8`   DATA_STATE_INDICATOR                  	long_name         1Degree of processing the data have passed through      conventions       Argo reference table 6     
_FillValue                    8�   	DATA_MODE                  	long_name         Delayed mode or real time data     conventions       >R : real time; D : delayed mode; A : real time with adjustment     
_FillValue                    8�   PLATFORM_TYPE                     	long_name         Type of float      conventions       Argo reference table 23    
_FillValue                  @  8�   FLOAT_SERIAL_NO                   	long_name         Serial number of the float     
_FillValue                  @  8�   FIRMWARE_VERSION                  	long_name         Instrument firmware version    
_FillValue                  @  9,   WMO_INST_TYPE                     	long_name         Coded instrument type      conventions       Argo reference table 8     
_FillValue                    9l   JULD               	long_name         ?Julian day (UTC) of the station relative to REFERENCE_DATE_TIME    standard_name         time   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~       axis      T           9t   JULD_QC                	long_name         Quality on date and time   conventions       Argo reference table 2     
_FillValue                    9�   JULD_LOCATION                  	long_name         @Julian day (UTC) of the location relative to REFERENCE_DATE_TIME   units         "days since 1950-01-01 00:00:00 UTC     conventions       8Relative julian days with decimal part (as parts of day)   
resolution        >�EȠ      
_FillValue        A.�~            9�   LATITUDE               	long_name         &Latitude of the station, best estimate     standard_name         latitude   units         degree_north   
_FillValue        @�i�       	valid_min         �V�        	valid_max         @V�        axis      Y           9�   	LONGITUDE                  	long_name         'Longitude of the station, best estimate    standard_name         	longitude      units         degree_east    
_FillValue        @�i�       	valid_min         �f�        	valid_max         @f�        axis      X           9�   POSITION_QC                	long_name         ,Quality on position (latitude and longitude)   conventions       Argo reference table 2     
_FillValue                    9�   POSITIONING_SYSTEM                    	long_name         Positioning system     
_FillValue                    9�   VERTICAL_SAMPLING_SCHEME                  	long_name         Vertical sampling scheme   conventions       Argo reference table 16    
_FillValue                    9�   CONFIG_MISSION_NUMBER                  	long_name         :Unique number denoting the missions performed by the float     conventions       !1...N, 1 : first complete mission      
_FillValue         ��        ;�   PROFILE_PRES_QC                	long_name         #Global quality flag of PRES profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_TEMP_QC                	long_name         #Global quality flag of TEMP profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PROFILE_PSAL_QC                	long_name         #Global quality flag of PSAL profile    conventions       Argo reference table 2a    
_FillValue                    ;�   PRES         
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [`   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  c@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ɠ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  р   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � `   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 7�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � ?�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � g    	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �    SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �    SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �    SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �    HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �T   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �\   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �d   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �l   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �t   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �8   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �@   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �H   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �PArgo profile    3.1 1.2 19500101000000  20230426223239  20230426223239  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @� }�^�Y@� }�^�Y11  @� ~��@� ~��@0lؘ���@0lؘ����dG3���dG3��11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AB  AB  ?�  @   @@  @�  @�G�@��
@�G�A   A  A ��A-p�A?\)A_\)A�  A�  A��A�
=A�  AУ�A�  A�  B (�B  B(�BQ�B Q�B(Q�B0(�B7�
B?�
BH  BP(�BW�
B`  Bh(�Bo�Bw�
B�
B��B�{B�  B�{B�  B��B�  B�  B�  B�  B�  B�{B�  B�  B�{B�{B�{B�{B�{B��B��B�{B�=qB�(�B�  B�{B�{B�  B�  B��
B�B��C
=C{C{C��C	��C
=C{C{C{C��C  C��C  C��C��C��C!��C$  C&  C'��C)��C,
=C.  C/��C2  C4  C6  C8  C:
=C<
=C>  C@
=CB
=CD  CF  CH
=CJ  CK�CN  CP  CQ��CS��CV  CX  CY��C\  C^{C`  Ca��Cc��Ce�Cg��Cj
=Cl{Cn
=Co��Cq��Ct
=Cv{Cw��Cy��C|
=C~
=C�  C�  C�C�C���C���C�
=C�
=C�C�C�C�C���C���C���C���C�C�C�C�  C���C�C�C���C�  C�  C���C�  C���C���C���C�  C�  C�  C�  C�  C�\C�\C�C�  C�  C�C�  C���C�  C�  C�C�
=C���C�  C���C���C���C���C���C�C�  C�C�
=C�C�C�C�  C���C���C���C���C�  C�C�C�
=C�C�C�C�  C�  C�  C�  C�  C���C��C���C�
=C�  C�  C�  C�  C�
=C�  C�C�C�  C�  C���C�  C�C�C�  C�  C�  C�  C���C�C�  C���C���C���C�  C�C�  C�C�C�
=C�C�C�  C�  C�  C�  C�
=C�C�
=C�  C�C�C�  C�  C�D   D z�D  D� D  D}qD  D��D  D� D�D��D  D}qD�qD� D�D��D	D	� D	�qD
� D  D� D�qD��DD�D�D}qD�qD}qD�qDz�D  D��D  D� D  D}qD��Dz�D��D}qD�D��D  D� D  D� D�D��DD��D  D��D�D��D  D� D  D� DD��D��D z�D!  D!��D"  D"� D#D#�D$  D$� D%�D%}qD&  D&� D'  D'��D'�qD(z�D(��D)� D*  D*��D+�D+� D,  D,��D-�D-�D.�D.}qD.��D/z�D0�D0�D1  D1��D2�D2}qD3  D3}qD3�RD4z�D4��D5}qD6D6�D7�D7}qD7�qD8� D8��D9xRD9��D:� D;  D;}qD;��D<xRD<��D=}qD>�D>��D?D?�D@D@��DA�DA��DB�DB��DC  DC}qDC�qDD}qDE  DE}qDE�qDF}qDG�DG��DG�qDH}qDH�qDI}qDJ  DJ� DJ�qDK}qDK�qDL� DM�DM� DM��DNz�DN��DO� DP�DP� DP�qDQ}qDQ��DR}qDR�qDS� DT�DT��DUDU}qDU�qDV� DV�qDW}qDW�qDX� DY  DY��DZ�DZ��DZ�qD[� D\D\� D\�qD]}qD]�qD^}qD^�qD_}qD_�qD`� Da  Da� Da�qDb}qDb�qDc}qDd�Dd��Dd��DexRDe�RDf}qDg  Dg� Dh�Dh� Dh�qDi� Dj  Dj� Dk  Dk� Dl�Dl��Dm  Dm� Dm�qDn� DoDo�Dp�Dp� Dq  Dq� Dq�qDr� Dr�qDs}qDs�qDt� Du  Du}qDu�qDv� Dw  Dw}qDx  Dx�Dy�Dy� Dz  Dz� D{  D{� D{�qD|� D|�qD}}qD}�qD~z�D~�qD� D�  D�=qD�� D��HD���D�=qD�|)D��qD�  D�B�D���D�� D�HD�@ D�� D��HD���D�>�D��HD�D�  D�>�D�~�D��qD���D�@ D�� D�� D�  D�@ D��HD�D�  D�@ D�� D���D���D�@ D��HD��HD���D�=qD�� D��HD��D�B�D���D�� D��qD�>�D�~�D��HD��D�>�D�~�D�� D�  D�@ D�� D�� D���D�>�D�~�D���D�  D�=qD�� D�� D���D�@ D�� D���D���D�AHD��HD���D�HD�AHD�� D��HD��D�B�D�~�D��qD���D�B�D���D�� D���D�>�D��HD���D�HD�@ D�� D�� D���D�B�D��HD�� D���D�B�D�� D���D�HD�B�D�� D�� D�  D�>�D�}qD�� D�  D�B�D���D��HD���D�@ D��HD��HD�HD�@ D�~�D��HD��)D�>�D�}qD���D�  D�AHD�~�D�D�  D�@ D�� D��HD���D�@ D�� D��HD��qD�AHD�� D���D��qD�AHD�~�D�� D���D�B�D�� D���D�  D�=qD�� D���D��D�>�D��HD���D��D�@ D�� D���D�HD�B�D��HD��HD���D�@ D��HD���D�HD�@ D�� D�D��D�@ D�� D��HD���D�@ D�~�D��qD��qD�@ D���D�D�HD�=qD�}qD�� D���D�>�D�~�D�� D���D�=qD�� D���D�  D�AHD��HD��HD��D�>�D��HD�� D��)D�>�D�� D���D�HD�B�D�� D���D��qD�AHD���D���D�  D�@ D�� D��HD���D�>�D�}qD���D���D�AHD�~�D���D��D�AHD�~�D���D�HD�B�D�� D��HD���D�>�D��HD��HD��D�AHD�� D�� D���D�>�D�~�D�� D�  D�B�D���D�� D���D�>�D�~�D���D��qD�>�D�� D��HD�  D�AHD�D�D��D�B�DÀ Dþ�D��D�@ DĀ D�� D�  D�AHDŀ D�� D�HD�@ DƁHDƽqD�HD�>�Dǀ D�� D�HD�>�DȀ DȽqD�  D�@ Dɀ D�D�HD�@ D�~�Dʾ�D�  D�=qDˁHD�� D�HD�@ D́HD��HD���D�@ D̀ D�� D�  D�=qD΀ Dξ�D�HD�AHD�~�DϾ�D�  D�@ DЁHD��HD�HD�AHD�~�D��HD�HD�@ DҁHDҾ�D��qD�>�DӀ D�� D���D�>�D�~�D�� D���D�@ DՀ D�� D�  D�@ DցHD�� D�  D�>�D�~�D�� D���D�@ D؀ D�� D�HD�AHDـ D�� D��D�AHDڀ D�� D���D�>�D�~�D��HD�  D�@ D܁HD��HD���D�=qD�}qD�D�HD�C�DށHD��HD��D�@ D�~�D�� D�HD�=qD�~�D�� D���D�@ D� D��HD��qD�@ D� D�qD�  D�AHD�~�D��HD��qD�@ D� D��HD�  D�@ D傏D��HD��D�AHD悏D澸D�HD�AHD炏D�� D�HD�AHD�HD��HD��qD�=qD� D�� D�  D�@ D�~�D꾸D�HD�>�D� D�D�HD�>�D�~�D�� D�  D�>�D�~�D���D��D�@ D�HD��HD�  D�AHD�~�D�� D�HD�>�D�� D�D�HD�@ D�}qD�� D�HD�<)D�~�D���D��D�@ D� D�D���D�@ D�D���D�  D�@ D��HD�� D���D�>�D��HD��HD���D�@ D��HD�� D���D�@ D�~�D���D�  D�@ D��HD��HD��D�C�D�h�?#�
?aG�?�=q?���?���?�@�\@\)@�R@(��@:�H@J=q@Y��@k�@}p�@��@���@�
=@��R@�ff@�\)@�
=@�  @�ff@�{@�z�@�p�@��@���@��@�(�AG�A�A��Ap�A�\AffA��A{A!G�A#�
A'�A,(�A0  A4z�A8Q�A<(�A@  AC�
AH��AL��AQG�AVffAZ=qA^�RAb�\AfffAj=qAn{Ar�\AvffAz=qA~{A���A��HA��A�
=A���A��\A�z�A�ffA�Q�A��A��A�p�A��A���A��A�A�  A��A��
A�{A�Q�A��A�(�A�ffA���A�=qA�(�A�{A�Q�A��HA��A�\)A���AÅA�AǮA��A˅A�A�\)Aљ�AӅA�p�A׮Aٙ�A��
A�p�A߮A�G�A��
A�{A�Q�A��HA��A�
=A���A�33A��A�\)A���A��A��A��RB Q�B��BffB�B��B�B33B(�B	G�B
ffB�Bz�B��B�\B�Bz�Bp�B�\B�B��BB
=Bz�Bp�B�RB�
B��B{B
=B Q�B!p�B"ffB#\)B$Q�B%G�B&�\B'�B(��B)B+
=B,  B-G�B.�\B/�
B0��B1�B3
=B4(�B5p�B6ffB7�B8��B9B:�RB;�B<z�B=��B>�HB@  B@��BB=qBC\)BDQ�BEp�BF�RBH(�BIp�BJ�\BK�BL��BM�BO33BP(�BQ�BRffBS33BT(�BU�BVffBW�BXz�BY��BZ�RB\  B]G�B^�\B_\)B`z�Ba��Bb�RBc�Bdz�Be��Bf�\Bg�Bh��Bj{Bk33BlQ�Bmp�BnffBo\)Bpz�Bqp�Br{Bs33Bt(�Bu�Bv{Bw33BxQ�By��Bz�\B{�B|��B}��B~�RB�B�Q�B��RB��B��B�{B��\B�
=B��B�=qB���B�G�B��
B�Q�B��HB�G�B�B�=qB���B�G�B�B�Q�B���B���B�{B���B��B��B�(�B��RB�33B���B�  B�z�B�
=B��B��B��\B�33B�B�=qB��RB��B��B�{B�z�B���B��B�(�B��RB�G�B��B�(�B��RB�33B���B�  B�z�B��HB�\)B��
B�Q�B���B��B�  B�z�B�
=B�p�B��B�=qB��RB�33B��B�{B��RB�\)B��
B�Q�B���B��B��B�{B�z�B�
=B���B�(�B��RB�33B��B��B�ffB��HB�\)B��
B�Q�B���B��B�{B��\B�
=B�p�B��B�Q�B���B�p�B��B�z�B�
=B���B�{B���B�
=B��B�  B�Q�B��HB�\)B��
B��\B�
=B��B�{B�z�B��HB�\)B��
B�=qB�
=B��B�{B���B�
=B��B��
B�z�B��HB�p�B�(�Bģ�B�33BŮB�(�BƏ\B��BǅB�(�B���B�p�B��B�z�B���B�G�B��
B�Q�B��HB͙�B�Q�B���B�p�B�B�Q�B���B�p�B�(�B���B�p�B�  Bԏ\B��B�p�B�{B֣�B�33B��
Bأ�B�33B��B�ffB��HB�p�B�(�B��HBݙ�B�  Bޣ�B�33B�(�B���B�p�B��
B�z�B�
=B��B��B�\)B�B�Q�B���B癚B�ffB�G�B��B�z�B�
=B�B�Q�B�
=B��B�\B�G�B�B�ffB�
=B�  B��B�\)B�B�z�B�
=B�  B���B�G�B�B�ffB��B�  B���B�33B��
B�z�B�\)B�{B��RB�33B��
C G�C �C  CG�C�C�
C=qC��C�C�Cp�C�RC(�Cp�C�C��C\)CC{CG�C��C�C\)C�C�C=qC�C	
=C	Q�C	��C	�C
\)C
�C
�C=qC�C�C=qCz�C��C33C�\CC{Cz�C�HC�CffC�C�Cp�C��C�CG�C�\C�RC�C=qCp�C��C�C��C
=C33CQ�CQ�CffC��C�RC��C�
C�HC(�CG�C=qCQ�C�C��C�RCC�HC{C33C(�CG�C�C��C��C�RC�
C  C(�C(�C=qCz�C��C��C�C�
C  C�C�C33Cp�C�\C��C�C��C  C(�C�C=qCz�C��C��C�RC��C�C�C=qCQ�C�\C�C�RC�C{C�C=qCffC�\C�RC�RC�
C{C33CG�C\)C�C�RC��C�HC  C33CG�C\)Cz�C�RC�
C��C  C�C\)Cp�C�C�C�
C�HC  C33C\)CffC�CC�HC�HC  CG�CffCp�C��C��C�HC��C �C Q�C Q�C p�C �C �
C �HC!�C!G�C!=qC!ffC!��C!C!�
C!��C"(�C"Q�C"\)C"z�C"�RC"��C"�HC#�C#G�C#Q�C#p�C#�C#�RC#�
C$�C$G�C$G�C$ffC$�C$��C$�
C%  C%=qC%G�C%ffC%��C%��C%�
C&  C&=qC&\)C&p�C&��C&�
C&�HC'  C'=qC'p�C'p�C'��C'�
C'�C(
=C(G�C(p�C(z�C(��C(�HC)
=C){C)=qC)z�C)�\C)�C)�HC*{C*�C*G�C*�C*��C*�RC*��C+�C+�C+G�C+�\C+�C+�RC+��C,�C,(�C,G�C,�C,�C,�RC,�C-�C-�C-G�C-�C-�C-�C-�C-�C.{C.Q�C.ffC.z�C.�C.�
C.�HC/
=C/=qC/G�C/p�C/��C/�C/�C0{C0(�C0ffC0�C0�\C0�
C0�C1
=C1G�C1\)C1z�C1C1�
C1��C2=qC2ffC2p�C2�RC2�
C2�C333C3\)C3p�C3�C3�HC3��C4=qC4\)C4z�C4�RC4�
C5  C5Q�C5p�C5�\C5��C6  C6{C6Q�C6�\C6��C6�
C7{C733C7ffC7��C7�RC7��C833C8G�C8z�C8�RC8��C9
=C9=qC9Q�C9��C9C9�
C:�C:=qC:\)C:��C:�RC:��C;33C;=qC;�C;��C;�RC<
=C<�C<G�C<�\C<��C<��C=�C=33C=ffC=�C=�RC>  C>33C>G�C>��C>��C>�HC?(�C?=qC?�C?��C?�RC@
=C@�C@Q�C@��C@�RCA  CA{CA=qCA�\CA��CA�HCB(�CB33CB�\CB��CB�
CC(�CC=qCC�\CC�CC�
CD(�CD=qCD�\CD��CD�HCE(�CEG�CEz�CE��CE�CF(�CF\)CFz�CF��CF�HCG33CGQ�CGz�CG��CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111114111111114111411114111411114111111114111411141111111111114111111111111111111111111111111114111111111141111114111111111111111111111411111111111111111111141111111111111111111111114111111111111141141141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                               ?�  @   @@  @�  @�G�@��
@�G�A   A  A ��A-p�A?\)A_\)A�  A�  A��A�
=A�  AУ�A�  A�  B (�B  B(�BQ�B Q�B(Q�B0(�B7�
B?�
BH  BP(�BW�
B`  Bh(�Bo�Bw�
B�
B��B�{B�  B�{B�  B��B�  B�  B�  B�  B�  B�{B�  B�  B�{B�{B�{B�{B�{B��B��B�{B�=qB�(�B�  B�{B�{B�  B�  B��
B�B��C
=C{C{C��C	��C
=C{C{C{C��C  C��C  C��C��C��C!��C$  C&  C'��C)��C,
=C.  C/��C2  C4  C6  C8  C:
=C<
=C>  C@
=CB
=CD  CF  CH
=CJ  CK�CN  CP  CQ��CS��CV  CX  CY��C\  C^{C`  Ca��Cc��Ce�Cg��Cj
=Cl{Cn
=Co��Cq��Ct
=Cv{Cw��Cy��C|
=C~
=C�  C�  C�C�C���C���C�
=C�
=C�C�C�C�C���C���C���C���C�C�C�C�  C���C�C�C���C�  C�  C���C�  C���C���C���C�  C�  C�  C�  C�  C�\C�\C�C�  C�  C�C�  C���C�  C�  C�C�
=C���C�  C���C���C���C���C���C�C�  C�C�
=C�C�C�C�  C���C���C���C���C�  C�C�C�
=C�C�C�C�  C�  C�  C�  C�  C���C��C���C�
=C�  C�  C�  C�  C�
=C�  C�C�C�  C�  C���C�  C�C�C�  C�  C�  C�  C���C�C�  C���C���C���C�  C�C�  C�C�C�
=C�C�C�  C�  C�  C�  C�
=C�C�
=C�  C�C�C�  C�  C�D   D z�D  D� D  D}qD  D��D  D� D�D��D  D}qD�qD� D�D��D	D	� D	�qD
� D  D� D�qD��DD�D�D}qD�qD}qD�qDz�D  D��D  D� D  D}qD��Dz�D��D}qD�D��D  D� D  D� D�D��DD��D  D��D�D��D  D� D  D� DD��D��D z�D!  D!��D"  D"� D#D#�D$  D$� D%�D%}qD&  D&� D'  D'��D'�qD(z�D(��D)� D*  D*��D+�D+� D,  D,��D-�D-�D.�D.}qD.��D/z�D0�D0�D1  D1��D2�D2}qD3  D3}qD3�RD4z�D4��D5}qD6D6�D7�D7}qD7�qD8� D8��D9xRD9��D:� D;  D;}qD;��D<xRD<��D=}qD>�D>��D?D?�D@D@��DA�DA��DB�DB��DC  DC}qDC�qDD}qDE  DE}qDE�qDF}qDG�DG��DG�qDH}qDH�qDI}qDJ  DJ� DJ�qDK}qDK�qDL� DM�DM� DM��DNz�DN��DO� DP�DP� DP�qDQ}qDQ��DR}qDR�qDS� DT�DT��DUDU}qDU�qDV� DV�qDW}qDW�qDX� DY  DY��DZ�DZ��DZ�qD[� D\D\� D\�qD]}qD]�qD^}qD^�qD_}qD_�qD`� Da  Da� Da�qDb}qDb�qDc}qDd�Dd��Dd��DexRDe�RDf}qDg  Dg� Dh�Dh� Dh�qDi� Dj  Dj� Dk  Dk� Dl�Dl��Dm  Dm� Dm�qDn� DoDo�Dp�Dp� Dq  Dq� Dq�qDr� Dr�qDs}qDs�qDt� Du  Du}qDu�qDv� Dw  Dw}qDx  Dx�Dy�Dy� Dz  Dz� D{  D{� D{�qD|� D|�qD}}qD}�qD~z�D~�qD� D�  D�=qD�� D��HD���D�=qD�|)D��qD�  D�B�D���D�� D�HD�@ D�� D��HD���D�>�D��HD�D�  D�>�D�~�D��qD���D�@ D�� D�� D�  D�@ D��HD�D�  D�@ D�� D���D���D�@ D��HD��HD���D�=qD�� D��HD��D�B�D���D�� D��qD�>�D�~�D��HD��D�>�D�~�D�� D�  D�@ D�� D�� D���D�>�D�~�D���D�  D�=qD�� D�� D���D�@ D�� D���D���D�AHD��HD���D�HD�AHD�� D��HD��D�B�D�~�D��qD���D�B�D���D�� D���D�>�D��HD���D�HD�@ D�� D�� D���D�B�D��HD�� D���D�B�D�� D���D�HD�B�D�� D�� D�  D�>�D�}qD�� D�  D�B�D���D��HD���D�@ D��HD��HD�HD�@ D�~�D��HD��)D�>�D�}qD���D�  D�AHD�~�D�D�  D�@ D�� D��HD���D�@ D�� D��HD��qD�AHD�� D���D��qD�AHD�~�D�� D���D�B�D�� D���D�  D�=qD�� D���D��D�>�D��HD���D��D�@ D�� D���D�HD�B�D��HD��HD���D�@ D��HD���D�HD�@ D�� D�D��D�@ D�� D��HD���D�@ D�~�D��qD��qD�@ D���D�D�HD�=qD�}qD�� D���D�>�D�~�D�� D���D�=qD�� D���D�  D�AHD��HD��HD��D�>�D��HD�� D��)D�>�D�� D���D�HD�B�D�� D���D��qD�AHD���D���D�  D�@ D�� D��HD���D�>�D�}qD���D���D�AHD�~�D���D��D�AHD�~�D���D�HD�B�D�� D��HD���D�>�D��HD��HD��D�AHD�� D�� D���D�>�D�~�D�� D�  D�B�D���D�� D���D�>�D�~�D���D��qD�>�D�� D��HD�  D�AHD�D�D��D�B�DÀ Dþ�D��D�@ DĀ D�� D�  D�AHDŀ D�� D�HD�@ DƁHDƽqD�HD�>�Dǀ D�� D�HD�>�DȀ DȽqD�  D�@ Dɀ D�D�HD�@ D�~�Dʾ�D�  D�=qDˁHD�� D�HD�@ D́HD��HD���D�@ D̀ D�� D�  D�=qD΀ Dξ�D�HD�AHD�~�DϾ�D�  D�@ DЁHD��HD�HD�AHD�~�D��HD�HD�@ DҁHDҾ�D��qD�>�DӀ D�� D���D�>�D�~�D�� D���D�@ DՀ D�� D�  D�@ DցHD�� D�  D�>�D�~�D�� D���D�@ D؀ D�� D�HD�AHDـ D�� D��D�AHDڀ D�� D���D�>�D�~�D��HD�  D�@ D܁HD��HD���D�=qD�}qD�D�HD�C�DށHD��HD��D�@ D�~�D�� D�HD�=qD�~�D�� D���D�@ D� D��HD��qD�@ D� D�qD�  D�AHD�~�D��HD��qD�@ D� D��HD�  D�@ D傏D��HD��D�AHD悏D澸D�HD�AHD炏D�� D�HD�AHD�HD��HD��qD�=qD� D�� D�  D�@ D�~�D꾸D�HD�>�D� D�D�HD�>�D�~�D�� D�  D�>�D�~�D���D��D�@ D�HD��HD�  D�AHD�~�D�� D�HD�>�D�� D�D�HD�@ D�}qD�� D�HD�<)D�~�D���D��D�@ D� D�D���D�@ D�D���D�  D�@ D��HD�� D���D�>�D��HD��HD���D�@ D��HD�� D���D�@ D�~�D���D�  D�@ D��HD��HD��D�C�D�h�?#�
?aG�?�=q?���?���?�@�\@\)@�R@(��@:�H@J=q@Y��@k�@}p�@��@���@�
=@��R@�ff@�\)@�
=@�  @�ff@�{@�z�@�p�@��@���@��@�(�AG�A�A��Ap�A�\AffA��A{A!G�A#�
A'�A,(�A0  A4z�A8Q�A<(�A@  AC�
AH��AL��AQG�AVffAZ=qA^�RAb�\AfffAj=qAn{Ar�\AvffAz=qA~{A���A��HA��A�
=A���A��\A�z�A�ffA�Q�A��A��A�p�A��A���A��A�A�  A��A��
A�{A�Q�A��A�(�A�ffA���A�=qA�(�A�{A�Q�A��HA��A�\)A���AÅA�AǮA��A˅A�A�\)Aљ�AӅA�p�A׮Aٙ�A��
A�p�A߮A�G�A��
A�{A�Q�A��HA��A�
=A���A�33A��A�\)A���A��A��A��RB Q�B��BffB�B��B�B33B(�B	G�B
ffB�Bz�B��B�\B�Bz�Bp�B�\B�B��BB
=Bz�Bp�B�RB�
B��B{B
=B Q�B!p�B"ffB#\)B$Q�B%G�B&�\B'�B(��B)B+
=B,  B-G�B.�\B/�
B0��B1�B3
=B4(�B5p�B6ffB7�B8��B9B:�RB;�B<z�B=��B>�HB@  B@��BB=qBC\)BDQ�BEp�BF�RBH(�BIp�BJ�\BK�BL��BM�BO33BP(�BQ�BRffBS33BT(�BU�BVffBW�BXz�BY��BZ�RB\  B]G�B^�\B_\)B`z�Ba��Bb�RBc�Bdz�Be��Bf�\Bg�Bh��Bj{Bk33BlQ�Bmp�BnffBo\)Bpz�Bqp�Br{Bs33Bt(�Bu�Bv{Bw33BxQ�By��Bz�\B{�B|��B}��B~�RB�B�Q�B��RB��B��B�{B��\B�
=B��B�=qB���B�G�B��
B�Q�B��HB�G�B�B�=qB���B�G�B�B�Q�B���B���B�{B���B��B��B�(�B��RB�33B���B�  B�z�B�
=B��B��B��\B�33B�B�=qB��RB��B��B�{B�z�B���B��B�(�B��RB�G�B��B�(�B��RB�33B���B�  B�z�B��HB�\)B��
B�Q�B���B��B�  B�z�B�
=B�p�B��B�=qB��RB�33B��B�{B��RB�\)B��
B�Q�B���B��B��B�{B�z�B�
=B���B�(�B��RB�33B��B��B�ffB��HB�\)B��
B�Q�B���B��B�{B��\B�
=B�p�B��B�Q�B���B�p�B��B�z�B�
=B���B�{B���B�
=B��B�  B�Q�B��HB�\)B��
B��\B�
=B��B�{B�z�B��HB�\)B��
B�=qB�
=B��B�{B���B�
=B��B��
B�z�B��HB�p�B�(�Bģ�B�33BŮB�(�BƏ\B��BǅB�(�B���B�p�B��B�z�B���B�G�B��
B�Q�B��HB͙�B�Q�B���B�p�B�B�Q�B���B�p�B�(�B���B�p�B�  Bԏ\B��B�p�B�{B֣�B�33B��
Bأ�B�33B��B�ffB��HB�p�B�(�B��HBݙ�B�  Bޣ�B�33B�(�B���B�p�B��
B�z�B�
=B��B��B�\)B�B�Q�B���B癚B�ffB�G�B��B�z�B�
=B�B�Q�B�
=B��B�\B�G�B�B�ffB�
=B�  B��B�\)B�B�z�B�
=B�  B���B�G�B�B�ffB��B�  B���B�33B��
B�z�B�\)B�{B��RB�33B��
C G�C �C  CG�C�C�
C=qC��C�C�Cp�C�RC(�Cp�C�C��C\)CC{CG�C��C�C\)C�C�C=qC�C	
=C	Q�C	��C	�C
\)C
�C
�C=qC�C�C=qCz�C��C33C�\CC{Cz�C�HC�CffC�C�Cp�C��C�CG�C�\C�RC�C=qCp�C��C�C��C
=C33CQ�CQ�CffC��C�RC��C�
C�HC(�CG�C=qCQ�C�C��C�RCC�HC{C33C(�CG�C�C��C��C�RC�
C  C(�C(�C=qCz�C��C��C�C�
C  C�C�C33Cp�C�\C��C�C��C  C(�C�C=qCz�C��C��C�RC��C�C�C=qCQ�C�\C�C�RC�C{C�C=qCffC�\C�RC�RC�
C{C33CG�C\)C�C�RC��C�HC  C33CG�C\)Cz�C�RC�
C��C  C�C\)Cp�C�C�C�
C�HC  C33C\)CffC�CC�HC�HC  CG�CffCp�C��C��C�HC��C �C Q�C Q�C p�C �C �
C �HC!�C!G�C!=qC!ffC!��C!C!�
C!��C"(�C"Q�C"\)C"z�C"�RC"��C"�HC#�C#G�C#Q�C#p�C#�C#�RC#�
C$�C$G�C$G�C$ffC$�C$��C$�
C%  C%=qC%G�C%ffC%��C%��C%�
C&  C&=qC&\)C&p�C&��C&�
C&�HC'  C'=qC'p�C'p�C'��C'�
C'�C(
=C(G�C(p�C(z�C(��C(�HC)
=C){C)=qC)z�C)�\C)�C)�HC*{C*�C*G�C*�C*��C*�RC*��C+�C+�C+G�C+�\C+�C+�RC+��C,�C,(�C,G�C,�C,�C,�RC,�C-�C-�C-G�C-�C-�C-�C-�C-�C.{C.Q�C.ffC.z�C.�C.�
C.�HC/
=C/=qC/G�C/p�C/��C/�C/�C0{C0(�C0ffC0�C0�\C0�
C0�C1
=C1G�C1\)C1z�C1C1�
C1��C2=qC2ffC2p�C2�RC2�
C2�C333C3\)C3p�C3�C3�HC3��C4=qC4\)C4z�C4�RC4�
C5  C5Q�C5p�C5�\C5��C6  C6{C6Q�C6�\C6��C6�
C7{C733C7ffC7��C7�RC7��C833C8G�C8z�C8�RC8��C9
=C9=qC9Q�C9��C9C9�
C:�C:=qC:\)C:��C:�RC:��C;33C;=qC;�C;��C;�RC<
=C<�C<G�C<�\C<��C<��C=�C=33C=ffC=�C=�RC>  C>33C>G�C>��C>��C>�HC?(�C?=qC?�C?��C?�RC@
=C@�C@Q�C@��C@�RCA  CA{CA=qCA�\CA��CA�HCB(�CB33CB�\CB��CB�
CC(�CC=qCC�\CC�CC�
CD(�CD=qCD�\CD��CD�HCE(�CEG�CEz�CE��CE�CF(�CF\)CFz�CF��CF�HCG33CGQ�CGz�CG��CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111114111111114111411114111411114111111114111411141111111111114111111111111111111111111111111114111111111141111114111111111111111111111411111111111111111111141111111111111111111111114111111111111141141141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A���A���A���A���A���A���A���A��
A��
A��
A��
A��A��/A��/A��
A���A��
A��
A��A��A��A��A��A��#A��A��#A��#A���A���A���A���A��
A���A���A���A�ȴAѺ^Aя\AН�A�n�A·+A��Aͧ�A��#AΕ�A��A�A��A��;A�ĜA��A�ȴAΕ�A��A͸RA�ZA˅A�;dA��TA�
=A��A� �A�A�A�A�hsA�/A��FA���A�1'A��/A��yA�A��A��
A�+A���A��mA���A��A��A���A���A�x�A���A��A��/A�9XA��9A��A���A�t�A�ZA�I�A�ƨA��A�K�A�%A��uA��jA��9A��A�ĜA��hA�/A��A��9A�hsA��/A�C�A���A};dAw�wAqC�Aj�Ah��AbbAahsA`~�A_�FA^��A^1'AY�PAU�ATA�ARAL�AGC�AC�;A?+A=�TA;�FA9�-A8bA7l�A7�A7��A7�mA4��A3��A2��A1��A1�7A/�;A.�A,�A+A+�A+;dA)��A(��A'/A#��A"I�A!p�A ��A ��A -A�#A+Ar�A  A/A~�A��A%AO�A�A
=A;dA(�A��A��A5?Ax�A��A�A�TA�7AC�A�TA^5A1'AO�A
1'A	�A��A9XA�-A33A��AbA"�A�A�AjA�-A r�@��w@���@��@�@�bN@���@�n�@�n�@��#@���@�|�@��@�{@�&�@��`@�A�@�~�@��#@�hs@�%@�%@���@�j@���@�5?@�`B@�V@�b@�P@��H@�E�@���@�x�@���@�(�@�  @�33@�{@�@��@�X@���@��@��
@ߍP@��m@��y@ޟ�@�hs@ݑh@ޏ\@��@݉7@���@���@�V@�V@�dZ@�r�@�@�$�@�X@���@��`@���@�|�@�ȴ@֏\@֗�@���@җ�@�V@��@���@���@�Q�@�(�@�(�@�z�@�Z@�Q�@��@�l�@�
=@�5?@ͩ�@�7L@�%@�j@� �@˥�@˅@ʸR@���@�%@�bN@�j@�bN@�b@��
@�l�@ư!@�n�@�J@��@���@�@���@�@�=q@��y@��@�`B@ģ�@�  @�@��@��@�j@�1'@�b@�  @�ƨ@�33@��@���@�ȴ@��R@���@��\@�J@��@��#@���@��7@�hs@�`B@�`B@�`B@�X@�G�@�?}@�?}@���@���@�bN@�b@���@�|�@�K�@�+@���@��y@���@��!@���@���@��\@�J@�/@��`@��@��D@�Z@��@�l�@�"�@���@�=q@�@�x�@�`B@��j@��@�o@�ȴ@��\@�5?@�`B@��j@�j@�I�@�1'@�b@��@���@���@�K�@�ȴ@�ff@�@���@�@��^@�@���@���@���@���@���@�X@�V@��@���@���@�z�@� �@��@�33@�
=@�ȴ@���@��+@�ff@�{@�?}@��@� �@���@�\)@�v�@�M�@�@�p�@��j@��@��@�t�@�dZ@�S�@�C�@�C�@�C�@�C�@�K�@�S�@�S�@�K�@�+@�M�@��@�x�@�V@���@�A�@�ƨ@�"�@�ȴ@�^5@��@��@���@�X@�7L@�V@�%@��`@���@���@��m@���@�|�@�K�@��y@��!@��\@�M�@���@���@���@��@�j@�Z@��@���@�l�@�33@���@��\@�M�@��h@��/@�j@�9X@�  @�ƨ@��P@�\)@�K�@�+@���@�J@���@�X@�G�@��`@�z�@�1@�l�@�33@��y@�ff@�V@�=q@�-@��#@��^@��7@�/@��@��D@�b@�1@���@��@��w@�l�@�"�@��H@�V@�=q@�5?@�-@�-@�$�@�{@���@��@���@���@���@��7@�`B@��@��`@�Ĝ@�r�@��@�  @�ƨ@�|�@�33@�
=@��H@��+@�5?@��h@�X@�V@���@��@��u@��u@���@���@���@��D@�b@��P@�S�@�"�@��@�o@�o@�o@�@�@��y@��H@��@��!@���@��+@�v�@�ff@�$�@��@�@�&�@��9@��u@�9X@� �@�w@K�@~��@~E�@}�-@}p�@}?}@|��@{ƨ@{"�@{@zM�@y�#@y�7@y7L@xr�@wl�@v��@v��@vȴ@u�-@u�h@uV@t��@t��@tz�@t9X@t�@s�
@s�F@so@r��@r�\@q��@qG�@p�9@o�w@o+@o
=@nȴ@n�+@n$�@m��@l��@lj@l1@kdZ@jM�@i�^@ihs@i&�@h�@hA�@g�@g��@g|�@g;d@f��@f��@fv�@fV@e@d�D@d1@cƨ@c��@c��@cdZ@co@b�@b�H@b��@bn�@b=q@b-@bJ@a��@a&�@`�@_�@_+@^ȴ@^��@^�+@^V@]�@]�-@\�@[��@[dZ@["�@["�@[o@[@Z�H@Z��@X��@X�@X  @W�w@W;d@V��@VV@U��@U�@U`B@T��@T�j@T��@T�D@Tz�@Tj@Tj@TZ@T�@S�m@S��@S"�@R�@R�!@R�\@R^5@Q��@Q7L@P1'@O|�@N�y@Nff@NV@NV@NE�@N5?@M��@M�h@M`B@MO�@L��@L��@LZ@L9X@L1@K33@J�@J��@J��@J�\@I��@I%@H�@H1'@G�@G��@Gl�@F�@F��@F�R@F$�@F@E�-@E�@D9X@C�F@C�@CS�@CC�@B�H@B^5@BM�@A�@Ahs@A�@@r�@@Q�@@A�@@ �@?��@?;d@>��@=�T@=/@<�j@<�@<��@<z�@<I�@;�F@;S�@;33@:�H@:~�@:J@9x�@8��@8bN@7�;@7�w@7�@7�@7��@7��@7K�@6ff@6{@6@6@6V@6$�@5�@5`B@5/@4��@4j@49X@41@3�F@3dZ@333@3"�@3@3@2�H@2��@2~�@2J@1��@1hs@1G�@1&�@0��@0�@01'@/�@.��@.ȴ@.��@.�+@.V@.$�@.@.@-�@-��@-�h@-O�@-V@,��@,�@,1@+�
@+��@+C�@*�@*n�@*J@)�^@)x�@)�@(�`@(Ĝ@(�9@(b@'\)@'
=@&�y@&ȴ@&�R@&�+@&V@&{@%�-@%�-@%�@%?}@$��@$�j@$�@$1@#�m@#��@#dZ@#33@#o@"��@"-@!�^@!X@!%@ ��@ �`@ �u@  �@�w@|�@K�@+@
=@�y@�y@�@��@�+@��@�y@��@V@$�@{@�@�T@�@�@�@�j@�D@z�@�@�@dZ@C�@@�@�@�\@n�@M�@�@��@��@��@G�@%@��@bN@A�@A�@ �@�;@�w@�P@ȴ@��@ff@$�@@@��@p�@/@��@j@9X@(�@�@��@ƨ@��@��@S�@33@�@��@n�@-@�@��@hs@G�@��@��@bN@Q�@Q�@1'@ �@ �@ �@  @  @�@�@\)@+@��@�@�@ȴ@��@v�@$�@$�@{@{@@�@@��@�@p�@/@��@��@��@(�@�m@ƨ@ƨ@�F@��@�@"�@@
��@
�\@
n�@
-@	�^@	��@	hs@	G�@	G�@	7L@	�@	%@	%@��@Ĝ@�9A��
A��
A��A���A���A���A���A���A��
A���A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��A��A��A��A��#A��#A��#A��#A��A��A��A��A��A���A��
A��
A��A��A��#A��#A��#A��#A��#A��#A��#A��#A��#A��#A��#A��A��#A��#A��#A��#A��/A��#A��A��
A��A��
A��
A��
A��
A��
A��A��
A��
A��
A��A��
A��
A��
A���A��
A���A���A���A���A���A���A���A��
A��
A��
A��A��A��A��#A��#A��#A��#A��#A��#A��A��
A��
A��
A��
A��
A��
A��
A��
A��A��
A��A��#A��#A��#A��#A��#A��A��
A��
A��
A���A��
A��
A��
A��
A��A��#A��A��#A��#A��/A��/A��/A��#A��#A��#A��#A��A��
A��
A��
A��
A��
A��
A��A��#A��#A��#A��/A��/A��/A��/A��/A��#A��#A��#A��A��A��
A���A���A���A���A���A���A���A���A���A��
A��
A��A��
A��
A��
A��
A���A��
A���A���A���A���A���A���A���A���A���A��
A��
A��
A��A��A��A��A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�A���AѼjAѼjAѸRAѸRAѶFAѸRAѸRAѸRAѸRAѸRAѲ-Aѥ�Aѡ�AёhA�|�A�n�A�XA�G�A�bA�ĜAЛ�A�z�A�l�A�9XA� �A�
=A��#Aϲ-A�hsA�(�A��A��TA��
A���A���AήA�`BA�ZA�\)A�Q�A�+A��A�JA��A��HA��
A�ȴA;wAͺ^AͰ!Aͧ�A͝�A͛�A͟�Aͧ�Aͩ�Aͧ�Aͧ�Aͩ�A͸RA;wA��mA�  A�+A�;dA�G�A�v�A�~�AΏ\AΛ�AμjA���A���A���A���A��
A��#A��A��A��;A��TA��yA��A���A�A�
=A�bA�bA�bA�oA� �A�&�A�+A�-A�&�A�VA�A���A���A���A��yA��;A��
A���A�A���A���A�ĜA�A�A�A�A���A��#A��/A��mA��A���A���A���A��mA��/A���A��
A��
A���A�ĜAμjAθRAΰ!AΧ�AΡ�AΙ�AΕ�A΍PA΋DAΉ7A΅A�r�A�E�A� �A���A��A��A��A��A��A��A��A��yA���A�`BA�?}A���A̡�A�p�A�C�A�;dA�+A�oA���Aˡ�AˋDAˁA�v�A�dZA�VA�I�A�C�A�=qA�5?A�33A�5?A�1'A�/A�-A��A�%A���A���A�~�A�{Aȡ�A�+A��AǼjAǋDA�G�A�$�A�JA���A��A���AƧ�Aƙ�A�O�A�E�A�33A��/A�33A���A�bNA��A��;Aò-A�7LA���A�A�x�A��A��
A��jA���A�p�A�^5A�Q�A�K�A�G�A�9XA�7LA�9XA�5?A�+A�  A��A���A��!A���A��+A�\)A�E�A��HA���A�~�A�`BA�E�A�C�A�=qA�1'A�&�A� �A��A�bA�A���A��TA��A��uA��DA�hsA�(�A���A��mA��RA�z�A�$�A���A��PA��yA��yA��mA��/A��9A���A��#A�-A��A��mA��mA��TA���A��^A���A��PA�x�A�dZA�I�A�
=A�n�A�A��A��A��/A�E�A���A�ZA�S�A�(�A�ƨA���A��\A��A�`BA�9XA��!A�~�A�v�A�O�A�?}A�&�A�$�A�VA�A���A��A���A���A��A�K�A�1A��#A��wA��!A���A���A��7A�z�A�jA�`BA�XA�M�A�G�A�E�A�E�A�=qA�33A� �A�{A�oA�JA�1A�1A�1A�A�  A�  A�  A�  A���A���A���A��A��TA��#A���A���A�ȴA��9A���A��A�n�A�^5A�O�A�Q�A�7LA��A��TA���A�bNA�5?A�A��mA��A�ĜA��!A���A���A��A�t�A�ZA�I�A�7LA�&�A�bA�  A��yA�ȴA���A�n�A�O�A�1'A�-A�/A�1'A�1'A�/A�$�A��A��A�{A�VA�
=A�%A��A��HA��
A���A���A��9A���A��+A�hsA�XA�VA�Q�A�E�A�9XA�+A�{A�A���A��mA�ȴA��wA��wA��^A���A��hA�p�A�`BA�M�A�9XA�(�A�bA�1A��mA���A��-A���A���A��+A��A�v�A�n�A�jA�ffA�ffA�dZA�`BA�XA�O�A�+A��A��A��A�
=A�A�  A���A��A��TA��
A���A���A��RA��A���A���A���A���A�z�A�l�A�\)A�=qA��A��A���A���A�l�A�O�A�+A��A�A���A��A��yA��yA��mA��TA��TA��TA��HA��
A��
A��wA��^A��!A���A���A��hA��PA��7A��A�z�A�z�A�t�A�l�A�l�A�l�A�hsA�ffA�ffA�bNA�bNA�bNA�bNA�^5A�`BA�`BA�\)A�^5A�`BA�\)A�\)A�\)A�K�A�A�A�C�A�?}A� �A�A��yA�ĜA��-A���A�~�A�^5A��A��HA�$�A���A�O�A�+A��A��`A���A�~�A�ƨA�;dA�bA�A���A���A��-A���A�ZA�oA�&�A��wA�^5A�C�A�XA�K�A�G�A��A�VA�
=A���A��/A���A�dZA�VA�;dA��A��A��RA�z�A�I�A��A�1A�  A�A�A���A�  A���A���A���A���A���A���A��HA���A��RA���A��PA�x�A�G�A�1A��A���A���A�ZA�A��#A���A�~�A�?}A�+A�"�A��A�bA�1A���A�|�A�bNA�
=A��A��A���A�+A�1'A�1'A�?}A�;dA�33A�A�A�I�A��A��FA���A�v�A�M�A��A���A��!A��7A�5?A���A��^A�%A��DA� �A�
=A�JA�A�ƨA���A�n�A�XA�(�A��A�"�A��A�%A��A���A�`BA�{A�z�A�S�A�C�A�=qA�+A��A�bA���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                               A��A���A���A���A���A���A���A���A��
A��
A��
A��
A��A��/A��/A��
A���A��
A��
A��A��A��A��A��A��#A��A��#A��#A���A���A���A���A��
A���A���A���A�ȴAѺ^Aя\AН�A�n�A·+A��Aͧ�A��#AΕ�A��A�A��A��;A�ĜA��A�ȴAΕ�A��A͸RA�ZA˅A�;dA��TA�
=A��A� �A�A�A�A�hsA�/A��FA���A�1'A��/A��yA�A��A��
A�+A���A��mA���A��A��A���A���A�x�A���A��A��/A�9XA��9A��A���A�t�A�ZA�I�A�ƨA��A�K�A�%A��uA��jA��9A��A�ĜA��hA�/A��A��9A�hsA��/A�C�A���A};dAw�wAqC�Aj�Ah��AbbAahsA`~�A_�FA^��A^1'AY�PAU�ATA�ARAL�AGC�AC�;A?+A=�TA;�FA9�-A8bA7l�A7�A7��A7�mA4��A3��A2��A1��A1�7A/�;A.�A,�A+A+�A+;dA)��A(��A'/A#��A"I�A!p�A ��A ��A -A�#A+Ar�A  A/A~�A��A%AO�A�A
=A;dA(�A��A��A5?Ax�A��A�A�TA�7AC�A�TA^5A1'AO�A
1'A	�A��A9XA�-A33A��AbA"�A�A�AjA�-A r�@��w@���@��@�@�bN@���@�n�@�n�@��#@���@�|�@��@�{@�&�@��`@�A�@�~�@��#@�hs@�%@�%@���@�j@���@�5?@�`B@�V@�b@�P@��H@�E�@���@�x�@���@�(�@�  @�33@�{@�@��@�X@���@��@��
@ߍP@��m@��y@ޟ�@�hs@ݑh@ޏ\@��@݉7@���@���@�V@�V@�dZ@�r�@�@�$�@�X@���@��`@���@�|�@�ȴ@֏\@֗�@���@җ�@�V@��@���@���@�Q�@�(�@�(�@�z�@�Z@�Q�@��@�l�@�
=@�5?@ͩ�@�7L@�%@�j@� �@˥�@˅@ʸR@���@�%@�bN@�j@�bN@�b@��
@�l�@ư!@�n�@�J@��@���@�@���@�@�=q@��y@��@�`B@ģ�@�  @�@��@��@�j@�1'@�b@�  @�ƨ@�33@��@���@�ȴ@��R@���@��\@�J@��@��#@���@��7@�hs@�`B@�`B@�`B@�X@�G�@�?}@�?}@���@���@�bN@�b@���@�|�@�K�@�+@���@��y@���@��!@���@���@��\@�J@�/@��`@��@��D@�Z@��@�l�@�"�@���@�=q@�@�x�@�`B@��j@��@�o@�ȴ@��\@�5?@�`B@��j@�j@�I�@�1'@�b@��@���@���@�K�@�ȴ@�ff@�@���@�@��^@�@���@���@���@���@���@�X@�V@��@���@���@�z�@� �@��@�33@�
=@�ȴ@���@��+@�ff@�{@�?}@��@� �@���@�\)@�v�@�M�@�@�p�@��j@��@��@�t�@�dZ@�S�@�C�@�C�@�C�@�C�@�K�@�S�@�S�@�K�@�+@�M�@��@�x�@�V@���@�A�@�ƨ@�"�@�ȴ@�^5@��@��@���@�X@�7L@�V@�%@��`@���@���@��m@���@�|�@�K�@��y@��!@��\@�M�@���@���@���@��@�j@�Z@��@���@�l�@�33@���@��\@�M�@��h@��/@�j@�9X@�  @�ƨ@��P@�\)@�K�@�+@���@�J@���@�X@�G�@��`@�z�@�1@�l�@�33@��y@�ff@�V@�=q@�-@��#@��^@��7@�/@��@��D@�b@�1@���@��@��w@�l�@�"�@��H@�V@�=q@�5?@�-@�-@�$�@�{@���@��@���@���@���@��7@�`B@��@��`@�Ĝ@�r�@��@�  @�ƨ@�|�@�33@�
=@��H@��+@�5?@��h@�X@�V@���@��@��u@��u@���@���@���@��D@�b@��P@�S�@�"�@��@�o@�o@�o@�@�@��y@��H@��@��!@���@��+@�v�@�ff@�$�@��@�@�&�@��9@��u@�9X@� �@�w@K�@~��@~E�@}�-@}p�@}?}@|��@{ƨ@{"�@{@zM�@y�#@y�7@y7L@xr�@wl�@v��@v��@vȴ@u�-@u�h@uV@t��@t��@tz�@t9X@t�@s�
@s�F@so@r��@r�\@q��@qG�@p�9@o�w@o+@o
=@nȴ@n�+@n$�@m��@l��@lj@l1@kdZ@jM�@i�^@ihs@i&�@h�@hA�@g�@g��@g|�@g;d@f��@f��@fv�@fV@e@d�D@d1@cƨ@c��@c��@cdZ@co@b�@b�H@b��@bn�@b=q@b-@bJ@a��@a&�@`�@_�@_+@^ȴ@^��@^�+@^V@]�@]�-@\�@[��@[dZ@["�@["�@[o@[@Z�H@Z��@X��@X�@X  @W�w@W;d@V��@VV@U��@U�@U`B@T��@T�j@T��@T�D@Tz�@Tj@Tj@TZ@T�@S�m@S��@S"�@R�@R�!@R�\@R^5@Q��@Q7L@P1'@O|�@N�y@Nff@NV@NV@NE�@N5?@M��@M�h@M`B@MO�@L��@L��@LZ@L9X@L1@K33@J�@J��@J��@J�\@I��@I%@H�@H1'@G�@G��@Gl�@F�@F��@F�R@F$�@F@E�-@E�@D9X@C�F@C�@CS�@CC�@B�H@B^5@BM�@A�@Ahs@A�@@r�@@Q�@@A�@@ �@?��@?;d@>��@=�T@=/@<�j@<�@<��@<z�@<I�@;�F@;S�@;33@:�H@:~�@:J@9x�@8��@8bN@7�;@7�w@7�@7�@7��@7��@7K�@6ff@6{@6@6@6V@6$�@5�@5`B@5/@4��@4j@49X@41@3�F@3dZ@333@3"�@3@3@2�H@2��@2~�@2J@1��@1hs@1G�@1&�@0��@0�@01'@/�@.��@.ȴ@.��@.�+@.V@.$�@.@.@-�@-��@-�h@-O�@-V@,��@,�@,1@+�
@+��@+C�@*�@*n�@*J@)�^@)x�@)�@(�`@(Ĝ@(�9@(b@'\)@'
=@&�y@&ȴ@&�R@&�+@&V@&{@%�-@%�-@%�@%?}@$��@$�j@$�@$1@#�m@#��@#dZ@#33@#o@"��@"-@!�^@!X@!%@ ��@ �`@ �u@  �@�w@|�@K�@+@
=@�y@�y@�@��@�+@��@�y@��@V@$�@{@�@�T@�@�@�@�j@�D@z�@�@�@dZ@C�@@�@�@�\@n�@M�@�@��@��@��@G�@%@��@bN@A�@A�@ �@�;@�w@�P@ȴ@��@ff@$�@@@��@p�@/@��@j@9X@(�@�@��@ƨ@��@��@S�@33@�@��@n�@-@�@��@hs@G�@��@��@bN@Q�@Q�@1'@ �@ �@ �@  @  @�@�@\)@+@��@�@�@ȴ@��@v�@$�@$�@{@{@@�@@��@�@p�@/@��@��@��@(�@�m@ƨ@ƨ@�F@��@�@"�@@
��@
�\@
n�@
-@	�^@	��@	hs@	G�@	G�@	7L@	�@	%@	%@��@Ĝ@�9A��
A��
A��A���A���A���A���A���A��
A���A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��A��A��A��A��#A��#A��#A��#A��A��A��A��A��A���A��
A��
A��A��A��#A��#A��#A��#A��#A��#A��#A��#A��#A��#A��#A��A��#A��#A��#A��#A��/A��#A��A��
A��A��
A��
A��
A��
A��
A��A��
A��
A��
A��A��
A��
A��
A���A��
A���A���A���A���A���A���A���A��
A��
A��
A��A��A��A��#A��#A��#A��#A��#A��#A��A��
A��
A��
A��
A��
A��
A��
A��
A��A��
A��A��#A��#A��#A��#A��#A��A��
A��
A��
A���A��
A��
A��
A��
A��A��#A��A��#A��#A��/A��/A��/A��#A��#A��#A��#A��A��
A��
A��
A��
A��
A��
A��A��#A��#A��#A��/A��/A��/A��/A��/A��#A��#A��#A��A��A��
A���A���A���A���A���A���A���A���A���A��
A��
A��A��
A��
A��
A��
A���A��
A���A���A���A���A���A���A���A���A���A��
A��
A��
A��A��A��A��A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�A���AѼjAѼjAѸRAѸRAѶFAѸRAѸRAѸRAѸRAѸRAѲ-Aѥ�Aѡ�AёhA�|�A�n�A�XA�G�A�bA�ĜAЛ�A�z�A�l�A�9XA� �A�
=A��#Aϲ-A�hsA�(�A��A��TA��
A���A���AήA�`BA�ZA�\)A�Q�A�+A��A�JA��A��HA��
A�ȴA;wAͺ^AͰ!Aͧ�A͝�A͛�A͟�Aͧ�Aͩ�Aͧ�Aͧ�Aͩ�A͸RA;wA��mA�  A�+A�;dA�G�A�v�A�~�AΏ\AΛ�AμjA���A���A���A���A��
A��#A��A��A��;A��TA��yA��A���A�A�
=A�bA�bA�bA�oA� �A�&�A�+A�-A�&�A�VA�A���A���A���A��yA��;A��
A���A�A���A���A�ĜA�A�A�A�A���A��#A��/A��mA��A���A���A���A��mA��/A���A��
A��
A���A�ĜAμjAθRAΰ!AΧ�AΡ�AΙ�AΕ�A΍PA΋DAΉ7A΅A�r�A�E�A� �A���A��A��A��A��A��A��A��A��yA���A�`BA�?}A���A̡�A�p�A�C�A�;dA�+A�oA���Aˡ�AˋDAˁA�v�A�dZA�VA�I�A�C�A�=qA�5?A�33A�5?A�1'A�/A�-A��A�%A���A���A�~�A�{Aȡ�A�+A��AǼjAǋDA�G�A�$�A�JA���A��A���AƧ�Aƙ�A�O�A�E�A�33A��/A�33A���A�bNA��A��;Aò-A�7LA���A�A�x�A��A��
A��jA���A�p�A�^5A�Q�A�K�A�G�A�9XA�7LA�9XA�5?A�+A�  A��A���A��!A���A��+A�\)A�E�A��HA���A�~�A�`BA�E�A�C�A�=qA�1'A�&�A� �A��A�bA�A���A��TA��A��uA��DA�hsA�(�A���A��mA��RA�z�A�$�A���A��PA��yA��yA��mA��/A��9A���A��#A�-A��A��mA��mA��TA���A��^A���A��PA�x�A�dZA�I�A�
=A�n�A�A��A��A��/A�E�A���A�ZA�S�A�(�A�ƨA���A��\A��A�`BA�9XA��!A�~�A�v�A�O�A�?}A�&�A�$�A�VA�A���A��A���A���A��A�K�A�1A��#A��wA��!A���A���A��7A�z�A�jA�`BA�XA�M�A�G�A�E�A�E�A�=qA�33A� �A�{A�oA�JA�1A�1A�1A�A�  A�  A�  A�  A���A���A���A��A��TA��#A���A���A�ȴA��9A���A��A�n�A�^5A�O�A�Q�A�7LA��A��TA���A�bNA�5?A�A��mA��A�ĜA��!A���A���A��A�t�A�ZA�I�A�7LA�&�A�bA�  A��yA�ȴA���A�n�A�O�A�1'A�-A�/A�1'A�1'A�/A�$�A��A��A�{A�VA�
=A�%A��A��HA��
A���A���A��9A���A��+A�hsA�XA�VA�Q�A�E�A�9XA�+A�{A�A���A��mA�ȴA��wA��wA��^A���A��hA�p�A�`BA�M�A�9XA�(�A�bA�1A��mA���A��-A���A���A��+A��A�v�A�n�A�jA�ffA�ffA�dZA�`BA�XA�O�A�+A��A��A��A�
=A�A�  A���A��A��TA��
A���A���A��RA��A���A���A���A���A�z�A�l�A�\)A�=qA��A��A���A���A�l�A�O�A�+A��A�A���A��A��yA��yA��mA��TA��TA��TA��HA��
A��
A��wA��^A��!A���A���A��hA��PA��7A��A�z�A�z�A�t�A�l�A�l�A�l�A�hsA�ffA�ffA�bNA�bNA�bNA�bNA�^5A�`BA�`BA�\)A�^5A�`BA�\)A�\)A�\)A�K�A�A�A�C�A�?}A� �A�A��yA�ĜA��-A���A�~�A�^5A��A��HA�$�A���A�O�A�+A��A��`A���A�~�A�ƨA�;dA�bA�A���A���A��-A���A�ZA�oA�&�A��wA�^5A�C�A�XA�K�A�G�A��A�VA�
=A���A��/A���A�dZA�VA�;dA��A��A��RA�z�A�I�A��A�1A�  A�A�A���A�  A���A���A���A���A���A���A��HA���A��RA���A��PA�x�A�G�A�1A��A���A���A�ZA�A��#A���A�~�A�?}A�+A�"�A��A�bA�1A���A�|�A�bNA�
=A��A��A���A�+A�1'A�1'A�?}A�;dA�33A�A�A�I�A��A��FA���A�v�A�M�A��A���A��!A��7A�5?A���A��^A�%A��DA� �A�
=A�JA�A�ƨA���A�n�A�XA�(�A��A�"�A��A�%A��A���A�`BA�{A�z�A�S�A�C�A�=qA�+A��A�bA���A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
l�B
l�B
lWB
lWB
lWB
lWB
lWB
lWB
lWB
lWB
l�B
lWB
lWB
lWB
lWB
l�B
lWB
l�B
lWB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m)B
m]B
n/B
qvB
sMB
x�B
�B
y�B
WsB
@B
6�B
I�B
��B
�nB
�LB
�KB
�#B
՛B
��B
�"B
�5B�B�B,�B?HBF�B[WB��BǮB��B�]B$BBxB �B*�B+kB-�B8�BM�BX�BS�BW�BdZBc�B\�BPHBH�BFtB;�B3hB.�B)_B"�B�BB�B_B�B  B�fBӏB�3B�B��B|PBf�BQNB3hB7B
�B
�B
�6B
��B
��B
y�B
e�B
=<B
YB	�TB	��B	��B	��B	{�B	s�B	p�B	l�B	e`B	\�B	ZB	B�B	4�B	/OB	�B	
=B��B�B��B�TB��B��B�B��B	�B	�B	B	�B	�B	�B	�B	 \B	 'B	OB	eB	�B	�B	�B	;B��B�B�B�	B��B�>B�fB��B��B��B��B��B��B��B	�B��B�DB�B	SB	 \B	%FB	*0B	"4B	,�B		�B�(B��B�B��B	B	6�B	.IB	($B	!bB	 �B	�B	7B	�B	SB	�B	�B	�B	�B	$B	$B	�B	1B	1B	+B	hB	�B	\B	�B	�B	�B	�B	�B	�B	�B	+B	_B	OB	$B	"�B	"4B	&�B	'�B	'RB	'�B	(�B	/B	49B	49B	7�B	9�B	9�B	>BB	AUB	A�B	B�B	E�B	IRB	L0B	R B	V�B	W
B	W�B	XyB	YB	Z�B	]�B	_B	ncB	q�B	u�B	|B	�B	��B	��B	�bB	�B	�DB	��B	�1B	��B	�DB	�~B	�-B	�:B	�hB	�:B	�B	��B	�tB	��B	��B	��B	��B	�zB	��B	�B	��B	��B	�6B	��B	�hB	��B	�XB	�$B	��B	�B	��B	��B	��B	�HB	�}B	�wB	�qB	��B	��B	��B	��B	��B	�gB	�B	ȀB	ɆB	�0B	͟B	��B	�?B	��B	�EB	��B	��B	��B	�HB	�B	�iB	�"B	��B	��B	�B	�B	�B	�sB	�
B	�B	�>B	�B	�B	�B	�B	�WB	�]B	� B	��B	��B	�B	�TB	�B	��B	�B	�B	�TB	�TB	�TB	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�lB	��B	�	B	�	B	��B	��B	�xB	�JB	�B	��B	�JB	�B	��B	�PB	��B	��B	�"B	��B	�B	�B	�PB	��B	��B	��B	�(B	��B
�B
�B
�B
B
B
{B
{B
{B
�B
�B
%B
�B
�B
_B
_B
�B
_B
�B
�B
_B
+B
�B
�B
�B
�B
�B
	B
�B
	�B
	�B

=B

	B

	B

	B

=B

	B

	B
	�B
	�B

	B

=B

�B
~B
�B
�B
~B
B
�B
xB
�B
.B
�B
�B
 B
 B
4B
hB
oB
�B
oB
:B
B
B
�B
�B
�B
@B
{B
MB
�B
�B
�B
�B
�B
�B
�B
�B
�B
1B
1B
�B
kB
kB
kB
�B
=B
qB
�B
B
B
�B
�B
VB
!B
!B
�B
�B
�B
 'B
 'B
!B
OB
B
xB
�B
xB
�B
CB
xB
CB
CB
B
~B
�B
�B
 'B
�B
 �B
 �B
!�B
"4B
"�B
#:B
#B
"�B
"�B
"hB
"�B
"hB
#B
#:B
#:B
$�B
%FB
%B
%FB
%FB
%�B
&�B
&�B
'B
(XB
(XB
($B
(XB
(XB
(XB
(�B
(�B
(�B
(�B
(�B
)*B
)_B
)�B
*�B
*�B
*�B
,B
,B
,B
,qB
,�B
,�B
-B
,�B
-CB
.B
.�B
.}B
/B
/B
/�B
/�B
/�B
/OB
/OB
/B
.�B
0UB
1'B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
1�B
2-B
49B
3�B
3�B
4nB
49B
4�B
4�B
5tB
5�B
6FB
6B
6B
6FB
7�B
7LB
7B
7LB
7�B
7�B
7�B
8�B
9$B
8�B
8RB
8�B
9XB
9XB
9�B
9�B
9�B
:*B
:�B
:*B
:�B
:*B
;�B
:�B
:�B
;�B
;dB
;�B
<�B
=B
<6B
<�B
<�B
<�B
=<B
=�B
<�B
<jB
=�B
>�B
>B
>�B
>�B
>�B
?HB
@�B
@�B
@�B
A B
A B
@�B
@�B
@OB
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
B�B
B[B
B�B
CaB
B�B
C-B
CaB
CaB
C�B
E�B
E9B
E�B
E9B
EmB
E9B
E�B
EmB
F�B
G�B
GEB
HB
GzB
HB
G�B
G�B
GzB
K)B
I�B
K^B
J�B
K�B
L�B
L0B
M6B
MjB
L�B
L�B
M6B
M6B
M�B
M�B
M6B
L�B
MjB
M�B
M6B
MjB
NpB
NB
NB
N�B
N�B
OB
OvB
QNB
P�B
QB
RTB
R B
Q�B
RTB
R�B
R B
R�B
S&B
R�B
R�B
S�B
S�B
S[B
T,B
TaB
U2B
T�B
TaB
T�B
VmB
VB
W
B
V�B
W
B
WsB
W
B
XEB
XyB
W�B
Y�B
YB
YB
ZB
[#B
Z�B
[#B
[#B
[#B
\�B
[�B
[�B
\]B
\]B
\)B
^B
]�B
^B
]�B
^5B
^�B
_B
_pB
_�B
`B
`vB
`BB
`vB
`vB
aB
a|B
a|B
bB
b�B
b�B
b�B
c B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d&B
e`B
e`B
e`B
ffB
gmB
g�B
g�B
h>B
h>B
h�B
h
B
g�B
hsB
g�B
h
B
h
B
g�B
h
B
g8B
g�B
g8B
h
B
g�B
h
B
h>B
h�B
hsB
iDB
h�B
iB
jKB
j�B
kB
kB
kQB
k�B
k�B
lWB
l"B
l"B
l"B
l�B
m�B
m)B
m)B
n/B
o B
n�B
o B
pB
o�B
poB
qB
q�B
q�B
qvB
q�B
rB
q�B
rGB
r|B
rB
r�B
rB
q�B
r�B
r�B
r�B
sMB
r�B
r�B
sB
sMB
s�B
t�B
s�B
s�B
s�B
tTB
tTB
t�B
t�B
u%B
u%B
uZB
t�B
uZB
u%B
t�B
t�B
uZB
u�B
v+B
u�B
u�B
u�B
v`B
v`B
v�B
wfB
x�B
y�B
zxB
z�B
z�B
z�B
{B
zxB
z�B
z�B
{B
{B
{�B
{B
|B
|�B
|�B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~(B
~�B
}�B
~�B
~�B
~�B
cB
cB
~�B
� B
�B
� B
�B
��B
� B
�iB
�;B
�B
�oB
�oB
��B
�oB
�B
��B
�uB
�AB
��B
�uB
��B
��B
�B
��B
�{B
��B
�GB
�B
��B
�GB
�B
��B
�B
�B
�SB
��B
�SB
��B
��B
��B
�B
�B
��B
�SB
��B
��B
�SB
��B
��B
��B
��B
��B
�YB
��B
�YB
�%B
�%B
�YB
�YB
�%B
��B
�+B
�+B
�+B
��B
��B
��B
��B
��B
�fB
�B
��B
�fB
�B
��B
��B
�lB
��B
�B
��B
�	B
�=B
��B
�rB
��B
��B
��B
�B
�DB
�B
�DB
�B
��B
l�B
m)B
m]B
ncB
m�B
m)B
m�B
l�B
lWB
lWB
lWB
l�B
l�B
l�B
l�B
lWB
m)B
l�B
m)B
l�B
m]B
l�B
m)B
m]B
m]B
m)B
m)B
m)B
m)B
l�B
m)B
l�B
m)B
l�B
l�B
m]B
m]B
l�B
l�B
l�B
l�B
k�B
k�B
k�B
k�B
kQB
kQB
kQB
kQB
k�B
k�B
l"B
l�B
k�B
m)B
l�B
m)B
l�B
m]B
l�B
m)B
m)B
m]B
m)B
l�B
m�B
m]B
m)B
m�B
m)B
l�B
l�B
l�B
l�B
m)B
kQB
l�B
l"B
k�B
kB
k�B
k�B
k�B
k�B
k�B
kB
k�B
k�B
k�B
kQB
k�B
k�B
l"B
l�B
l�B
l�B
l�B
m)B
m�B
m)B
l�B
l�B
l�B
l�B
l�B
lWB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
l�B
l�B
m)B
m)B
m]B
m]B
m�B
m�B
m]B
l�B
m)B
lWB
k�B
k�B
k�B
k�B
k�B
l�B
l�B
m)B
m)B
m�B
m]B
m)B
l�B
m)B
lWB
l"B
lWB
l"B
kQB
k�B
k�B
k�B
lWB
l�B
lWB
l�B
m�B
m]B
m)B
m]B
m�B
m�B
m]B
l�B
k�B
l"B
lWB
kB
k�B
k�B
k�B
k�B
lWB
l�B
l�B
m]B
m�B
ncB
m]B
m�B
ncB
ncB
m�B
m�B
m]B
l�B
l�B
l"B
lWB
k�B
l"B
l"B
l"B
lWB
l�B
lWB
l�B
m)B
m�B
m�B
m�B
m�B
m�B
m]B
m�B
m)B
l�B
l�B
lWB
k�B
k�B
l"B
l�B
l�B
m)B
m]B
ncB
m�B
m�B
m�B
m�B
m)B
l�B
l�B
lWB
l�B
m)B
m]B
m�B
m�B
ncB
ncB
oiB
o�B
n�B
n�B
n�B
o5B
o�B
p;B
qAB
sMB
r|B
s�B
s�B
tTB
tB
t�B
s�B
s�B
r�B
sB
q�B
r�B
t�B
tTB
w�B
|�B
~�B
�B
��B
�{B
��B
�B
��B
��B
�xB
��B
�1B
��B
�;B
�uB
t�B
m�B
b�B
b�B
^�B
_pB
^5B
XB
L�B
LdB
N�B
H�B
F�B
C-B
@�B
B[B
=qB
<B
8�B
8�B
6�B
5�B
49B
3�B
3hB
:*B
;0B
<6B
<B
<6B
@OB
A B
C�B
VB
h>B
m�B
tB
}"B
��B
��B
��B
��B
�!B
�VB
�'B
��B
��B
��B
�4B
�hB
�B
�6B
�B
�!B
��B
�B
�B
�B
�jB
�B
�B
��B
�mB
��B
�XB
�0B
��B
�RB
ȴB
��B
ȀB
�)B
�B
�B
��B
�^B
�jB
ϫB
ҽB
��B
��B
��B
�B
�yB
�B
�TB
�B
�B
�QB
�B
�"B
�B
�KB
��B
�DB
��B
��B
��B
��B
�)B
��B
�B
�)B
�cB
� B
��B
�;B
�oB
��B
��BMBfB
�BJB"B�B�B�B�B�BBFB�BIB,�B,qB*0B+�B*eB-�B0�B=qB=qB>wB>�B>BA BB[BD3BEmBGEBGEBGzBG�BI�BIRBIBN�BS&BW?B]�Bs�B�!B��B��B��B�zB�UB�B��B�3B�mBɆBʌB�0B��B�EBбB��B�2B��B�iB�B�B�8B��B
	B�BB�B�BB�B�B!�BIBB�B�B�BCBCB�BxB�B�B!bB�B#�B$B#:B#nB0�B)�B1�B-B.B*�B+6B,B*�B,=B+6B,=B,qB*0B-�B33B.IB-�B2�B7B3�B/OB8BL�BC-BU2Bf2BE�BB�BA�BB�BH�BdZBb�BgmBX�BR�BQ�BRTBTaBT�BT�BRTBR�BS[BR�BZ�BffBYBS&BX�Bv�B`�Bu�BiDB_;Bh�Bf�B_�Ba�B\]Bd&Be�BjKBX�BXBaBYKBTaBTaBW?BT,BUgBW
BTaBOvBR�BU�BS�BP}BOBL�BK�BK�BK�BMjBM6BJ�BJXBJ�BK^BI�BHKBJ#BJ�BK^BK)BG�BI�BJ#BIRBGBHBH�BHKBF�BF�BIBG�BFtBF�BFtBG�BG�BDgBC�BHKBJ�BE�BFBGEBD3B@�BB�B=�BB�BF?B@OB@�B?�B=<B8�B9�B9$B9XB9$B8�B8RB9�B7�B6zB4�B6�B6zB5�B6zB8�B6B4�B1�B1'B0�B/�B/B0!B1�B1[B/OB.�B/�B0UB/�B.�B0!B/�B/�B-wB/B/OB/OB.�B,�B*eB)�B+kB+�B*�B+�B*�B)_B)*B)�B'RB&LB%�B($B)�B'�B%FB'�B$@B#�B$tB$�B'�B#B"4B"�B �B!BVB �B�BOB�B~BB�B 'B!B#BVB�B�B�B�B�BCBBB	B�B�B�B�B+B1B_B_B7B�BB�B1B�BB�BoB�B�B"BB1B�B
	BfB+B�B1B%B_B	�B�BxB_B�B�B%BYB�B�B�B�B�B�B�B{B�B{BGB�BABB;B �BB �B  BB 4B��B��B�cB�]B 4B��B�B��BB��B��B�B�ZB�`B�+B�TB��B��B�B�B��B�,B��B�dBԕB�B��B�gBƨB�3BĜB�dB��B�jB��B�wB��BޞB��B��B�&B�zB��B��B�hB��B��B��B��B�kB��B�B�FB�{B��B�VB�oB�+B��B��B�oB��B��B�B�AB��B�4B�iB��B.B�B~�B�B|Bz�BxB}"Bx8Bp;Bu�Bq�Bt�BoiBf�Bk�Bf�B`BB[WB[WBZ�BXEBW?B[�BXyBRTBZQBT�BWsB[�B8�B2�B1�B/B8�B5�B(�B3�BN�B-wB/OB/�B+�B7B 'B�B#nB1B(�B�B>�B%BMB
�	B
��B
�BB
��B
�|B
�vB
��B
��B
�B
�
B
��B
�KB
�DB
�B
�&B
��B
�gB
�aB
��B
�TB
� B
�BB
��B
ΥG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                               B
d�B
d�B
dcB
dcB
dcB
dcB
dcB
dcB
dcB
dcB
d�B
dcB
dcB
dcB
dcB
d�B
dcB
d�B
dcB
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
d�B
e B
e B
e B
e B
e B
e5B
eiB
f;B
i�B
kYB
p�B
�B
q�B
OB
8&B
.�B
A�B
��B
�zB
�XB
�WB
�/B
ͧB
��B
�.B
�AB �B
�B$�B7TB>�BScB��B��B��B�iB0B!B�B�B"�B#wB%�B0�BE�BP�BK�BO�B\fB[�BUBHTB@�B>�B3�B+tB&�B!kB�B�BB	�B�kB��B�B�rB˛B�?B�B{�Bt\B^�BIZB+tBCB
��B
��B
�BB
�B
��B
q�B
]�B
5HB
eB	�`B	��B	��B	��B	s�B	k�B	h�B	e B	]lB	T�B	R)B	:�B	,�B	'[B	�B	IB��B�+B�B�`B��B��B�B�B��B	�B	B	B	�B	�B	�B	hB	3B	[B	qB	�B	�B	 �B�GB� B�B�B�B�B�JB�rB��B�	B��B�B��B�B��B��B��B�PB�B�_B	hB	RB	"<B	@B	$�B	�B�4B��B�(B��B	B	.�B	&UB	 0B	nB	�B	�B	CB	�B	_B	�B	�B	�B		�B	0B	0B	�B	=B	=B	7B		tB	�B	hB	�B	�B	�B		�B		�B	�B	�B	7B	kB	[B	B	�B	@B	�B	�B	^B	�B	!B	''B	,EB	,EB	/�B	1�B	2B	6NB	9aB	9�B	;B	=�B	A^B	D<B	J,B	N�B	OB	O�B	P�B	Q#B	R�B	U�B	WB	foB	i�B	m�B	t(B	|%B	��B	��B	�nB	�'B	�PB	�B	�=B	{�B	�PB	��B	�9B	�FB	�tB	�FB	�B	��B	��B	��B	��B	��B	��B	��B	��B	�*B	��B	��B	�BB	��B	�tB	��B	�dB	�0B	��B	�B	��B	�B	��B	�TB	��B	��B	�}B	��B	��B	��B	��B	��B	�sB	�B	��B	��B	�<B	ūB	��B	�KB	��B	�QB	��B	�B	��B	�TB	�(B	�uB	�.B	��B	� B	�"B	�"B	�B	�B	�B	�B	�JB	�B	�B	�"B	�B	�cB	�iB	�B	��B	��B	�B	�`B	�+B	��B	�B	�B	�`B	�`B	�`B	��B	�B	�B	�B	�B	��B	�B	��B	�	B	�B	�B	�B	�xB	��B	�B	�B	��B	��B	�B	�VB	�B	�B	�VB	�B	��B	�\B	�B	��B	�.B	��B	�(B	�(B	�\B	��B	� B	��B	�4B	�B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	�1B	��B	��B	�kB	�kB	��B	�kB	��B	��B	�kB	�7B
 	B
 �B
 �B
 �B
 �B
B
 �B
�B
�B
IB
B
B
B
IB
B
B
�B
�B
B
IB
�B
�B
�B
�B
�B
'B
�B
�B
�B
:B
�B
�B
	B
	B
	@B
	tB

{B

�B

{B

FB
B
B
�B
�B
�B
LB
�B
YB
�B
�B
�B
B
�B
B
B
B
B
=B
=B
�B
wB
wB
wB
�B
IB
}B
�B
B
'B
�B
�B
bB
-B
-B
�B
�B
�B
3B
3B
-B
[B
'B
�B
�B
�B
�B
OB
�B
OB
OB
B
�B
�B
�B
3B
�B
B
B
�B
@B
�B
FB
B
�B
�B
tB
�B
tB
B
FB
FB
�B
RB
B
RB
RB
�B
�B
�B
*B
 dB
 dB
 0B
 dB
 dB
 dB
 �B
 �B
 �B
 �B
!B
!6B
!kB
!�B
"�B
"�B
"�B
$B
$B
$B
$}B
$�B
$�B
%B
$�B
%OB
& B
&�B
&�B
''B
''B
'�B
'�B
'�B
'[B
'[B
''B
&�B
(aB
)3B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
)�B
*B
)�B
)�B
*B
*B
*B
)�B
)�B
*B
*B
*9B
,EB
+�B
+�B
,zB
,EB
,�B
,�B
-�B
-�B
.RB
.B
.B
.RB
/�B
/XB
/#B
/XB
/�B
/�B
/�B
0�B
10B
0�B
0^B
0�B
1dB
1dB
2B
2B
2B
26B
2�B
26B
2�B
26B
3�B
3B
2�B
3�B
3pB
3�B
4�B
5B
4BB
4�B
4�B
4�B
5HB
5�B
4�B
4vB
5�B
6�B
6B
6�B
6�B
6�B
7TB
8�B
8�B
8�B
9,B
9,B
8�B
8�B
8[B
8�B
9�B
9�B
9�B
9�B
9�B
9�B
9�B
:�B
:�B
:gB
;B
;mB
:�B
;9B
;mB
;mB
<
B
=�B
=EB
=�B
=EB
=yB
=EB
=�B
=yB
>�B
?�B
?QB
@#B
?�B
@#B
?�B
?�B
?�B
C5B
A�B
CjB
CB
C�B
D�B
D<B
EBB
EvB
D�B
D�B
EBB
EBB
E�B
E�B
EBB
D�B
EvB
E�B
EBB
EvB
F|B
FB
FB
F�B
F�B
GB
G�B
IZB
H�B
I&B
J`B
J,B
I�B
J`B
J�B
J,B
J�B
K2B
J�B
J�B
K�B
K�B
KgB
L8B
LmB
M>B
M
B
LmB
M
B
NyB
NB
OB
N�B
OB
OB
OB
PQB
P�B
O�B
Q�B
Q�B
Q#B
R)B
S/B
R�B
S/B
S/B
S/B
T�B
S�B
S�B
TiB
TiB
T5B
VB
U�B
VB
U�B
VAB
V�B
WB
W|B
W�B
XB
X�B
XNB
X�B
X�B
YB
Y�B
Y�B
Z%B
Z�B
Z�B
Z�B
[,B
[�B
[�B
[�B
[�B
[�B
[�B
[�B
\2B
]lB
]lB
]lB
^rB
_yB
_�B
_�B
`JB
`JB
`�B
`B
_�B
`B
_�B
`B
`B
_�B
`B
_DB
_�B
_DB
`B
_�B
`B
`JB
`�B
`B
aPB
`�B
aB
bWB
b�B
c(B
c(B
c]B
c�B
c�B
dcB
d.B
d.B
d.B
d�B
e�B
e5B
e5B
f;B
gB
f�B
gB
hB
g�B
h{B
iB
i�B
i�B
i�B
i�B
jB
i�B
jSB
j�B
jB
j�B
jB
i�B
j�B
j�B
j�B
kYB
j�B
j�B
k%B
kYB
k�B
l�B
k�B
k�B
k�B
l`B
l`B
l�B
l�B
m1B
m1B
mfB
l�B
mfB
m1B
l�B
l�B
mfB
nB
n7B
m�B
m�B
nB
nlB
nlB
o	B
orB
p�B
q�B
r�B
r�B
r�B
r�B
s"B
r�B
r�B
r�B
s�B
s"B
s�B
s�B
t(B
t�B
t�B
t�B
t�B
t�B
t�B
u�B
u�B
u�B
v B
v4B
v�B
u�B
wB
wB
v�B
woB
woB
wB
xB
w�B
xB
w�B
x�B
xB
xuB
yGB
yB
y{B
y{B
y�B
y{B
zB
y�B
z�B
zMB
z�B
z�B
z�B
z�B
{B
z�B
{�B
{�B
{SB
{B
{�B
{SB
|%B
{�B
|%B
}+B
}_B
}�B
}_B
|�B
}�B
}�B
}+B
}+B
}�B
}_B
}�B
}�B
}_B
}�B
}�B
}�B
}�B
}�B
~eB
}�B
~eB
~1B
~1B
~eB
~eB
~1B
~�B
7B
7B
7B
�B
�B
�B
�	B
��B
�rB
�B
��B
�rB
�B
��B
��B
�xB
��B
�B
��B
�B
�IB
��B
�~B
��B
��B
��B
�B
�PB
�B
�PB
�!B
��B
e B
e5B
eiB
foB
fB
e5B
e�B
d�B
dcB
dcB
dcB
d�B
d�B
d�B
d�B
dcB
e5B
d�B
e5B
e B
eiB
e B
e5B
eiB
eiB
e5B
e5B
e5B
e5B
d�B
e5B
d�B
e5B
d�B
d�B
eiB
eiB
e B
e B
e B
d�B
c�B
c�B
c�B
c�B
c]B
c]B
c]B
c]B
c�B
c�B
d.B
d�B
c�B
e5B
d�B
e5B
e B
eiB
e B
e5B
e5B
eiB
e5B
e B
e�B
eiB
e5B
e�B
e5B
e B
e B
d�B
d�B
e5B
c]B
e B
d.B
c�B
c(B
c�B
c�B
c�B
c�B
c�B
c(B
c�B
c�B
c�B
c]B
c�B
c�B
d.B
d�B
d�B
e B
d�B
e5B
e�B
e5B
e B
d�B
d�B
d�B
d�B
dcB
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
d�B
e B
e5B
e5B
eiB
eiB
e�B
e�B
eiB
e B
e5B
dcB
c�B
c�B
c�B
c�B
c�B
d�B
e B
e5B
e5B
e�B
eiB
e5B
e B
e5B
dcB
d.B
dcB
d.B
c]B
c�B
c�B
c�B
dcB
d�B
dcB
e B
e�B
eiB
e5B
eiB
e�B
e�B
eiB
e B
c�B
d.B
dcB
c(B
c�B
c�B
c�B
c�B
dcB
e B
e B
eiB
e�B
foB
eiB
e�B
foB
foB
fB
e�B
eiB
e B
d�B
d.B
dcB
c�B
d.B
d.B
d.B
dcB
d�B
dcB
e B
e5B
fB
fB
fB
e�B
e�B
eiB
e�B
e5B
d�B
d�B
dcB
c�B
c�B
d.B
d�B
e B
e5B
eiB
foB
fB
fB
e�B
e�B
e5B
d�B
e B
dcB
d�B
e5B
eiB
e�B
e�B
foB
foB
guB
g�B
f�B
f�B
f�B
gAB
g�B
hGB
iMB
kYB
j�B
k�B
k�B
l`B
l+B
l�B
k�B
k�B
j�B
k%B
i�B
j�B
l�B
l`B
o�B
t�B
v�B
|%B
B
��B
��B
�B
��B
��B
��B
��B
�=B
�B
yGB
z�B
l�B
e�B
Z�B
Z�B
V�B
W|B
VAB
PB
D�B
DpB
F�B
@�B
>�B
;9B
8�B
:gB
5}B
4B
0�B
0�B
.�B
-�B
,EB
+�B
+tB
26B
3<B
4BB
4B
4BB
8[B
9,B
;�B
NB
`JB
fB
l+B
u.B
|�B
��B
��B
��B
�-B
�bB
�3B
��B
�B
��B
�@B
�tB
�*B
�BB
�B
�-B
��B
�B
�#B
�B
�vB
�B
�B
��B
�yB
��B
�dB
�<B
��B
�^B
��B
��B
��B
�5B
�)B
�)B
��B
�jB
�vB
ǷB
��B
��B
��B
��B
�B
ЅB
ٽB
�`B
ݡB
۔B
�]B
�(B
�.B
�(B
�WB
��B
�PB
��B
�B
��B
��B
�5B
��B
�(B
�5B
�oB
�B
��B
�GB
�{B
��B
�B
�YB rB�BVB.B�B�B�B�B�B!BRB�BUB$�B$}B"<B#�B"qB%�B(�B5}B5}B6�B6�B6B9,B:gB<?B=yB?QB?QB?�B?�BA�BA^BA)BF�BK2BOKBU�Bk�B�-B��B��B��B��B�aB� B��B�?B�yB��BB�<B��B�QBȽB��B�>B��B�uB�B��B�DB��BB�B'B
�B�BB�B�BBUBB�B�B�BOBOBB�B�B�BnB�B�BBFBzB(�B!�B)�B%B& B"�B#BB$B"�B$IB#BB$IB$}B"<B%�B+?B&UB%�B*�B/#B+�B'[B0)BD�B;9BM>B^>B=�B:�B9�B:�B@�B\fBZ�B_yBP�BJ�BI�BJ`BLmBL�BL�BJ`BJ�BKgBJ�BR�B^rBQ�BK2BP�Bo	BX�Bm�BaPBWGB`�B^�BW�BY�BTiB\2B]�BbWBP�BPBYBQWBLmBLmBOKBL8BMsBOBLmBG�BJ�BM�BK�BH�BGBD�BC�BDBC�BEvBEBBB�BBdBB�BCjBA�B@WBB/BB�BCjBC5B?�BA�BB/BA^B?B@#B@�B@WB>�B>�BA)B?�B>�B>�B>�B?�B?�B<sB;�B@WBB�B=�B>B?QB<?B8�B;B5�B:�B>KB8[B8�B7�B5HB0�B1�B10B1dB10B0�B0^B1�B/�B.�B,�B.�B.�B-�B.�B0�B.B,�B)�B)3B(�B'�B''B(-B)�B)gB'[B&�B'�B(aB'�B&�B(-B'�B'�B%�B''B'[B'[B&�B$�B"qB!�B#wB#�B"�B#�B"�B!kB!6B!�B^BXB�B 0B!�B�BRB�BLB�B�B�B�BB@B�B�B-BbBB�B[B�B�B!B�B3B-BBbB�B�B�B�B�BOB!B!BB�B�B�B�B7B=BkBkBCB�B*BB=B�BB�B
{B�B�B.B'B =B��BB rB�7B �B =B�1B�kB�B��B�B�kB��B��B�1B�eB��B��B��B��B��B��B��B��B��B��B�SB��B�MB�B�GB��B�B��B�B�B�@B��B��B�oB�iB�@B� B�(B��B�B��B��B�B�fB�lB�7B�`B��B��B��B�%B�B�8B�B�pB̡B۔B��B�sB��B�?B��B�pB��B�vB��B��B��B֪B��B��B�2B��B��B�B�tB��B��B��B��B�wB��B�B�RB��B��B�bB�{B7B{�B{�By{By�Bz�ByBzMBy�Bx@BxuBx�Bw:ByBv�ByBt(Br�BpBu.BpDBhGBm�Bi�Bl�BguB^�Bc�B^�BXNBScBScBR�BPQBOKBS�BP�BJ`BR]BL�BOBTB0�B*�B)�B''B0�B-�B �B+�BF�B%�B'[B'�B#�B/#B3B�BzB =B �B�B6�B
�1BYB
�B
��B
�B
�B
��B
�B
�B
��B
��B
߭B
�B
��B
�WB
�PB
ܛB
�2B
��B
�sB
�mB
��B
�`B
�,B
�NB
��B
ƱG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223239                            20230426223239AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622323920230426223239  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622323920230426223239QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622323920230426223239QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               