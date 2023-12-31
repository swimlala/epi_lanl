CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-30T02:01:02Z creation      
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
_FillValue                 �  [h   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  cL   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �@   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  Ѭ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8(   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � @   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � gx   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �`   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �`   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �`   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �`   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �T   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �p   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �x   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230730020102  20230730020102  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�;Ð���@�;Ð���11  @�;��s�@�;��s�@0$P�ܜN@0$P�ܜN�d.�DR?h�d.�DR?h11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AB  AB  ?�  @�\@B�\@�  @�  @�G�@��
AG�A\)A�RA+�A@��AaG�A���A���A�Q�A�Q�A�  A�  A��A�A�\)B(�B(�B�
B Q�B((�B0(�B8  B@  BG�
BO�
BX  B`(�Bh(�Bp(�Bx  B�
B��B��B��B��B�  B�{B�(�B�{B�{B�{B�{B��B��
B��B�  B�{B�(�B�{B�  B�{B�{B�  B�  B�(�B�  B��B�  B�  B�(�B�  B��B��C  C
=C
=C
=C	��C
=C{C{C{C��C  C{C  C��C  C 
=C"  C$  C&
=C(
=C*
=C,
=C.  C0  C2  C4  C5��C7��C:  C;��C=��C@
=CB  CC��CF  CH  CJ  CL
=CM��CP  CR  CT{CV{CX
=CY��C\
=C^
=C_��Cb  Cd
=Ce��Cg��Cj  Ck��Cn  Cp
=Cr
=Ct{Cv
=Cx  Cy��C{��C}��C��C�  C�C�  C���C���C���C���C���C���C���C���C�C�  C���C���C���C���C�  C���C���C�  C�
=C�C���C�C�C�C���C���C�
=C�
=C�  C���C���C�  C�  C���C�C�\C�\C�C���C�  C�  C�C�
=C�  C���C��C���C�  C���C���C�  C���C�  C�
=C���C�  C�
=C�C���C�  C�  C�  C���C���C���C���C���C���C�  C���C���C���C���C���C�  C�  C�  C�
=C���C���C�  C�  C���C�  C�  C���C���C�C�  C���C�  C�  C�  C�  C���C�  C���C���C�  C�  C�
=C�
=C�C�C�  C�  C�
=C�C���C���C�  C���C���C���C��C���C���C���C���C��C��C���C�C�  C���D }qD  D� D�D�DD��D�qD� D�qDz�D��D}qD  D��DD��D	�D	� D
  D
� D
�qD}qD�qD}qD  D� D  D}qD��DxRD�qD��D�D� D�qD}qD�D��DD��DD��D�qD}qD�D� D�qD}qD  D��D  D� D�qDz�D�qD��D  D}qD��D}qD  D}qD �D � D �qD!}qD!��D"}qD#�D#�D$�D$� D$�qD%� D%�qD&z�D&�qD'}qD'�qD(}qD)  D)�D*D*�D+�D+� D+�qD,� D-D-��D.  D.� D.�qD/}qD/�qD0z�D0�qD1��D2  D2��D2�qD3xRD4�D4��D5�D5��D6D6}qD6��D7}qD8�D8� D8�qD9}qD:  D:�D;�D;xRD;�qD<� D=  D=�D>�D>� D?�D?z�D?��D@}qDA  DA� DA�qDBz�DB�RDCz�DD�DD�DE�DE� DE�qDFz�DG  DG�DH�DH� DI  DI� DJ  DJz�DK  DK��DL  DL��DM�DM��DM�qDN� DN�qDO� DP�DP�DQ  DQ}qDQ�qDRz�DS�DS�DT  DT}qDU�DU�DV�DVz�DW  DW��DW�qDX}qDY  DY��DZ�DZ�D[D[��D[�qD\}qD]  D]�D^�D^� D_�D_��D_�qD`}qDa  Da� Db�Db� Dc  Dc}qDd  Dd��Dd��Dez�Df  Df��Dg  Dg� Dh�Dh��DiDi��Dj�Dj�Dk  Dk��Dl�Dl��Dm�Dm}qDn  Dn�DoDo� Dp  Dp��DqDq� Dq�qDr}qDs�Ds��Ds�qDt}qDu  Du}qDu�qDv}qDv�qDw}qDw�qDx� Dx�qDy}qDz�Dz��Dz�qD{� D|  D|� D|�qD}� D~�D~}qD  D}qD�qD�AHD�~�D���D��qD�@ D�~�D���D�  D�AHD�~�D�� D��D�AHD�� D�D�  D�AHD�� D�D�  D�=qD��HD�� D�  D�>�D�� D���D�  D�AHD��HD��HD���D�AHD�� D���D�  D�AHD��HD�� D�  D�@ D��HD��HD��qD�B�D�� D��HD�  D�>�D�� D��HD�HD�@ D�� D���D���D�AHD�� D��HD�HD�@ D�� D�� D���D�@ D�� D�� D���D�AHD�� D�� D�  D�@ D�� D��HD�  D�AHD�� D��HD���D�>�D�~�D�� D�  D�>�D��HD��HD�HD�@ D��HD��HD�  D�>�D��HD�D�  D�>�D�� D�� D�  D�@ D�� D���D���D�@ D���D�D�HD�@ D�� D�� D���D�@ D�~�D�� D�HD�@ D��HD��HD���D�>�D�~�D�� D�HD�AHD�� D���D���D�=qD�~�D��HD�HD�=qD�~�D�D��D�>�D�� D�� D���D�>�D�� D�� D���D�@ D��HD���D�  D�@ D�~�D�� D��D�AHD�� D�� D���D�>�D�}qD���D���D�AHD��HD�� D�  D�>�D�}qD���D�  D�=qD�� D�� D���D�>�D�}qD���D��qD�>�D�� D���D���D�>�D�}qD�� D�  D�AHD�� D���D�  D�AHD��HD��HD�HD�@ D��HD���D�  D�@ D�}qD��qD���D�>�D�� D��HD���D�AHD���D�D��D�AHD��HD�� D�  D�@ D�� D��HD�  D�B�D��HD���D�  D�>�D�� D���D�  D�B�D��HD�� D�HD�AHD�� D�D�HD�AHD��HD��HD���D�>�D�~�D�� D��D�AHD�� D�� D�HD�AHD���D��HD�HD�@ D�� D���D��qD�AHD�}qD��qD�  D�@ D�� D��qD��qD�=qD��HD�� D�  D�@ D�}qD�� D��D�@ D�~�D�� D�  D�@ DĂ�Dľ�D�HD�@ DŁHDž�D�HD�@ DƁHD��HD�HD�=qDǂ�DǽqD�  D�@ D�~�DȾ�D�  D�AHDɂ�D�D�HD�AHDʂ�D�� D�  D�>�Dˀ D��HD�HD�@ D�~�D��HD��D�B�D̀ D�� D�HD�>�D΁HD��HD���D�@ Dπ D��HD���D�=qDЀ D�D�  D�@ DсHD��HD���D�<)DҀ DҾ�D�HD�>�DӀ DӽqD��qD�@ DԁHD��HD�HD�@ D�}qDվ�D�  D�@ Dւ�D��HD�HD�>�D�~�D�� D��)D�>�D؀ D�� D���D�@ D�~�D��HD���D�@ Dڀ DڽqD��qD�@ DہHD�� D�HD�@ D�~�Dܾ�D�HD�>�D݀ D��HD��)D�@ DށHD��HD�HD�@ D߀ D��HD��D�AHD�}qD�� D��D�AHD�HD�D��D�@ D� D��HD�HD�=qD�~�D��HD��D�@ D� D�� D���D�@ D� D�� D�HD�AHD� D�� D�  D�AHD�~�D羸D�HD�=qD�}qD�� D�HD�AHD�~�D龸D�HD�>�D�HD��HD���D�>�D�~�D�� D�HD�B�D�HD�� D�  D�AHD� D���D���D�@ D� D�� D�  D�AHD�HD��HD�  D�@ D�� D�� D�  D�@ D� D�� D��D�@ D�HD�� D��qD�=qD� D�� D�  D�AHD� D�� D�  D�>�D�~�D�� D�HD�@ D�� D��HD�HD�AHD�� D���D���D�>�D�� D��HD��D�@ D�~�D���D���D�@ D�~�D��
?\)?��?L��?�  ?��R?�33?��?�@�\@\)@�R@+�@8Q�@E�@W
=@h��@p��@z�H@��@�\)@�Q�@�  @��@�{@�@�p�@�ff@�\)@�
=@�  @���@�\)@�
=A   Az�AQ�A(�A��Az�A��A��A ��A%�A(��A-p�A1�A6ffA9��A>{AB�\AG
=AJ�HAN�RAR�\AUAY��A^�RAb�\Ag
=Aj�HAo\)As�
AxQ�A|��A���A��\A�p�A��A��A�(�A�ffA�Q�A��\A���A�
=A���A�33A�p�A��A���A��A�p�A��A���A�(�A�ffA���A��HA�p�A�  A�=qA�(�A��RA���A�33A�p�AǮAə�A�(�A�{A�Q�Aҏ\A��A�\)Aٙ�A��
A�ffA�Q�A�\A���A�
=A�G�A�A�{A�  A��A��
A�ffA�Q�A��HA��A�\)B ��B=qB33BQ�BB33BQ�B	G�B
�RB�
B�BffB�B��B�B
=B(�Bp�B�\B�
B��B=qB\)B��BB
=B   B!p�B"�\B#�
B%�B&ffB'�B(��B)�B+33B,Q�B-B.�RB0  B1G�B2�\B3�B4��B6{B7\)B8z�B9B:�HB<(�B=p�B>�\B?�
BA�BBffBC�BD��BF{BG33BHz�BIBJ�HBL(�BMG�BN�\BO�BP��BR=qBS�BT��BUBW33BXQ�BY��BZ�\B[�
B\��B^{B_
=B`��Ba�Bb�RBc�
Be�BfffBg�Bh��Bj{Bk33Blz�BmBo
=Bp(�Bqp�Br�\Bs�
Bu�Bv=qBw\)Bx��By�B{33B|Q�B}�B
=B�(�B���B�\)B�  B�z�B�
=B���B�=qB��HB��B�(�B��RB�G�B��B�ffB�
=B���B�(�B��RB�G�B��
B�ffB���B��B�{B���B�G�B�B�(�B��HB�\)B��
B�z�B���B�\)B��B�ffB���B��B�  B��\B��B���B�{B���B�G�B��
B�Q�B��HB�\)B��B�z�B���B��B�  B��\B�
=B��B�{B��\B�
=B��B�  B��\B��B��B�  B�ffB��HB�\)B��
B�z�B�
=B���B�=qB��RB�33B�B�Q�B���B�\)B��B�ffB��HB�p�B��B�ffB��HB�\)B�  B�z�B�
=B���B�(�B���B��B�B�z�B���B�p�B�  B��\B�
=B��B�{B�z�B���B�p�B�  B�ffB���B��B�=qB���B�G�B��
B�=qB���B�33B�B�=qB���B�G�B��B\B��B�B�{Bď\B���Bř�B�  BƏ\B��B��
B�Q�B��HB�\)B�B�Q�BʸRB�G�B��B̏\B�33BͮB�=qB���B�G�BϮB�Q�B���B�\)B��
Bҏ\B�33B��
B�ffBԸRB�G�BծB�ffB�
=B׮B�=qB���B�\)B��
B�Q�B���B�\)B��
B܏\B�G�B��
B�ffB޸RB�\)B��
B��B�G�B��B�z�B���B�p�B�  B��B�B�{B�RB�33B�B�=qB��HB陚B�ffB���B뙚B�{B��B�G�B�{B���B�B�  B�z�B�33B��
B�RB�\)B�{B�z�B�G�B��
B���B�p�B�{B��\B�G�B�{B��HB��B�=qB��RB�p�B�{B���B��C 33C p�C ��C(�C��C��C\)C�C�CG�C��C{Cp�C�
C{Cp�C��C=qC��C��CG�C��C  Cz�C�
C	�C	z�C	�
C
G�C
�C{CQ�C�C�C�\C��C33C��C�Cp�C��C{Cp�C��CQ�C��C  C\)C�
C33Cz�C�HCQ�C�RC�C\)CC33C��C��C=qC�\C  C\)C�\C�
C�Cz�CC�HC
=CG�C�\CC��C��C{CQ�C�C�CC�HC
=CQ�Cp�C��C�C�
C��C=qCp�Cz�C��C��C
=C33CQ�CffC�\C��C  C�C33CQ�C��C��C��C��C(�CQ�C�C�C�C�
C 
=C =qC =qC \)C ��C C �HC ��C!{C!Q�C!ffC!�C!��C!�HC"
=C"{C"(�C"G�C"�\C"�RC"�C"��C#{C#33C#=qC#Q�C#��C#C#C#�C$
=C$=qC$Q�C$ffC$��C$C$�C$�C%
=C%33C%p�C%�\C%��C%�RC%��C&�C&�C&=qC&z�C&��C&�C&C&��C'(�C'G�C'Q�C'p�C'�C'�
C'�HC(  C(�C(\)C(p�C(�C(�RC(�C(�C){C)Q�C)p�C)z�C)�\C)�
C*
=C*  C*�C*ffC*�C*��C*C*�C*��C+{C+\)C+�C+�C+��C+�C,
=C,{C,=qC,\)C,��C,�RC,��C,��C-33C-\)C-\)C-�C-C-�C-��C.{C.\)C.�C.�C.�C.�C/�C/(�C/G�C/�\C/�RC/C/�HC0�C0G�C0Q�C0z�C0�RC0�C0�C1{C1Q�C1p�C1�C1�RC1�C1��C2�C2Q�C2�C2��C2�RC2�C3�C333C3Q�C3�\C3C3C3�C4(�C4Q�C4ffC4�\C4��C4�HC5  C5G�C5p�C5z�C5��C5�HC6  C6�C6ffC6�C6��C6C7  C733C7=qC7ffC7�C7C7�HC833C8G�C8ffC8�RC8�HC8�C9�C9\)C9�\C9��C9�HC:{C:�C:Q�C:�\C:��C:��C;{C;G�C;Q�C;p�C;�RC;�C;��C<(�C<ffC<z�C<��C<�HC=
=C={C==qC=�C=�\C=�C=��C>(�C>33C>p�C>��C>�C>�C?�C?33C?p�C?��C?�C?�
C@(�C@33C@z�C@�C@�RCA  CA�CA33CA�CA��CACB
=CB(�CB=qCB�\CB�CBCC{CC(�CCffCC��CC�RCC��CD�CD33CDz�CD��CDCE  CE(�CEG�CE�CE�CECF{CF(�CF\)CF��CF�CF��CG�CG33CGz�CG��CG�RCH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111411114111111111111111114111111141111111141111111141111111111111111111141111111411111111114111111111111411111114111111111111111411111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                   ?�  @�\@B�\@�  @�  @�G�@��
AG�A\)A�RA+�A@��AaG�A���A���A�Q�A�Q�A�  A�  A��A�A�\)B(�B(�B�
B Q�B((�B0(�B8  B@  BG�
BO�
BX  B`(�Bh(�Bp(�Bx  B�
B��B��B��B��B�  B�{B�(�B�{B�{B�{B�{B��B��
B��B�  B�{B�(�B�{B�  B�{B�{B�  B�  B�(�B�  B��B�  B�  B�(�B�  B��B��C  C
=C
=C
=C	��C
=C{C{C{C��C  C{C  C��C  C 
=C"  C$  C&
=C(
=C*
=C,
=C.  C0  C2  C4  C5��C7��C:  C;��C=��C@
=CB  CC��CF  CH  CJ  CL
=CM��CP  CR  CT{CV{CX
=CY��C\
=C^
=C_��Cb  Cd
=Ce��Cg��Cj  Ck��Cn  Cp
=Cr
=Ct{Cv
=Cx  Cy��C{��C}��C��C�  C�C�  C���C���C���C���C���C���C���C���C�C�  C���C���C���C���C�  C���C���C�  C�
=C�C���C�C�C�C���C���C�
=C�
=C�  C���C���C�  C�  C���C�C�\C�\C�C���C�  C�  C�C�
=C�  C���C��C���C�  C���C���C�  C���C�  C�
=C���C�  C�
=C�C���C�  C�  C�  C���C���C���C���C���C���C�  C���C���C���C���C���C�  C�  C�  C�
=C���C���C�  C�  C���C�  C�  C���C���C�C�  C���C�  C�  C�  C�  C���C�  C���C���C�  C�  C�
=C�
=C�C�C�  C�  C�
=C�C���C���C�  C���C���C���C��C���C���C���C���C��C��C���C�C�  C���D }qD  D� D�D�DD��D�qD� D�qDz�D��D}qD  D��DD��D	�D	� D
  D
� D
�qD}qD�qD}qD  D� D  D}qD��DxRD�qD��D�D� D�qD}qD�D��DD��DD��D�qD}qD�D� D�qD}qD  D��D  D� D�qDz�D�qD��D  D}qD��D}qD  D}qD �D � D �qD!}qD!��D"}qD#�D#�D$�D$� D$�qD%� D%�qD&z�D&�qD'}qD'�qD(}qD)  D)�D*D*�D+�D+� D+�qD,� D-D-��D.  D.� D.�qD/}qD/�qD0z�D0�qD1��D2  D2��D2�qD3xRD4�D4��D5�D5��D6D6}qD6��D7}qD8�D8� D8�qD9}qD:  D:�D;�D;xRD;�qD<� D=  D=�D>�D>� D?�D?z�D?��D@}qDA  DA� DA�qDBz�DB�RDCz�DD�DD�DE�DE� DE�qDFz�DG  DG�DH�DH� DI  DI� DJ  DJz�DK  DK��DL  DL��DM�DM��DM�qDN� DN�qDO� DP�DP�DQ  DQ}qDQ�qDRz�DS�DS�DT  DT}qDU�DU�DV�DVz�DW  DW��DW�qDX}qDY  DY��DZ�DZ�D[D[��D[�qD\}qD]  D]�D^�D^� D_�D_��D_�qD`}qDa  Da� Db�Db� Dc  Dc}qDd  Dd��Dd��Dez�Df  Df��Dg  Dg� Dh�Dh��DiDi��Dj�Dj�Dk  Dk��Dl�Dl��Dm�Dm}qDn  Dn�DoDo� Dp  Dp��DqDq� Dq�qDr}qDs�Ds��Ds�qDt}qDu  Du}qDu�qDv}qDv�qDw}qDw�qDx� Dx�qDy}qDz�Dz��Dz�qD{� D|  D|� D|�qD}� D~�D~}qD  D}qD�qD�AHD�~�D���D��qD�@ D�~�D���D�  D�AHD�~�D�� D��D�AHD�� D�D�  D�AHD�� D�D�  D�=qD��HD�� D�  D�>�D�� D���D�  D�AHD��HD��HD���D�AHD�� D���D�  D�AHD��HD�� D�  D�@ D��HD��HD��qD�B�D�� D��HD�  D�>�D�� D��HD�HD�@ D�� D���D���D�AHD�� D��HD�HD�@ D�� D�� D���D�@ D�� D�� D���D�AHD�� D�� D�  D�@ D�� D��HD�  D�AHD�� D��HD���D�>�D�~�D�� D�  D�>�D��HD��HD�HD�@ D��HD��HD�  D�>�D��HD�D�  D�>�D�� D�� D�  D�@ D�� D���D���D�@ D���D�D�HD�@ D�� D�� D���D�@ D�~�D�� D�HD�@ D��HD��HD���D�>�D�~�D�� D�HD�AHD�� D���D���D�=qD�~�D��HD�HD�=qD�~�D�D��D�>�D�� D�� D���D�>�D�� D�� D���D�@ D��HD���D�  D�@ D�~�D�� D��D�AHD�� D�� D���D�>�D�}qD���D���D�AHD��HD�� D�  D�>�D�}qD���D�  D�=qD�� D�� D���D�>�D�}qD���D��qD�>�D�� D���D���D�>�D�}qD�� D�  D�AHD�� D���D�  D�AHD��HD��HD�HD�@ D��HD���D�  D�@ D�}qD��qD���D�>�D�� D��HD���D�AHD���D�D��D�AHD��HD�� D�  D�@ D�� D��HD�  D�B�D��HD���D�  D�>�D�� D���D�  D�B�D��HD�� D�HD�AHD�� D�D�HD�AHD��HD��HD���D�>�D�~�D�� D��D�AHD�� D�� D�HD�AHD���D��HD�HD�@ D�� D���D��qD�AHD�}qD��qD�  D�@ D�� D��qD��qD�=qD��HD�� D�  D�@ D�}qD�� D��D�@ D�~�D�� D�  D�@ DĂ�Dľ�D�HD�@ DŁHDž�D�HD�@ DƁHD��HD�HD�=qDǂ�DǽqD�  D�@ D�~�DȾ�D�  D�AHDɂ�D�D�HD�AHDʂ�D�� D�  D�>�Dˀ D��HD�HD�@ D�~�D��HD��D�B�D̀ D�� D�HD�>�D΁HD��HD���D�@ Dπ D��HD���D�=qDЀ D�D�  D�@ DсHD��HD���D�<)DҀ DҾ�D�HD�>�DӀ DӽqD��qD�@ DԁHD��HD�HD�@ D�}qDվ�D�  D�@ Dւ�D��HD�HD�>�D�~�D�� D��)D�>�D؀ D�� D���D�@ D�~�D��HD���D�@ Dڀ DڽqD��qD�@ DہHD�� D�HD�@ D�~�Dܾ�D�HD�>�D݀ D��HD��)D�@ DށHD��HD�HD�@ D߀ D��HD��D�AHD�}qD�� D��D�AHD�HD�D��D�@ D� D��HD�HD�=qD�~�D��HD��D�@ D� D�� D���D�@ D� D�� D�HD�AHD� D�� D�  D�AHD�~�D羸D�HD�=qD�}qD�� D�HD�AHD�~�D龸D�HD�>�D�HD��HD���D�>�D�~�D�� D�HD�B�D�HD�� D�  D�AHD� D���D���D�@ D� D�� D�  D�AHD�HD��HD�  D�@ D�� D�� D�  D�@ D� D�� D��D�@ D�HD�� D��qD�=qD� D�� D�  D�AHD� D�� D�  D�>�D�~�D�� D�HD�@ D�� D��HD�HD�AHD�� D���D���D�>�D�� D��HD��D�@ D�~�D���D���D�@ D�~�D��
?\)?��?L��?�  ?��R?�33?��?�@�\@\)@�R@+�@8Q�@E�@W
=@h��@p��@z�H@��@�\)@�Q�@�  @��@�{@�@�p�@�ff@�\)@�
=@�  @���@�\)@�
=A   Az�AQ�A(�A��Az�A��A��A ��A%�A(��A-p�A1�A6ffA9��A>{AB�\AG
=AJ�HAN�RAR�\AUAY��A^�RAb�\Ag
=Aj�HAo\)As�
AxQ�A|��A���A��\A�p�A��A��A�(�A�ffA�Q�A��\A���A�
=A���A�33A�p�A��A���A��A�p�A��A���A�(�A�ffA���A��HA�p�A�  A�=qA�(�A��RA���A�33A�p�AǮAə�A�(�A�{A�Q�Aҏ\A��A�\)Aٙ�A��
A�ffA�Q�A�\A���A�
=A�G�A�A�{A�  A��A��
A�ffA�Q�A��HA��A�\)B ��B=qB33BQ�BB33BQ�B	G�B
�RB�
B�BffB�B��B�B
=B(�Bp�B�\B�
B��B=qB\)B��BB
=B   B!p�B"�\B#�
B%�B&ffB'�B(��B)�B+33B,Q�B-B.�RB0  B1G�B2�\B3�B4��B6{B7\)B8z�B9B:�HB<(�B=p�B>�\B?�
BA�BBffBC�BD��BF{BG33BHz�BIBJ�HBL(�BMG�BN�\BO�BP��BR=qBS�BT��BUBW33BXQ�BY��BZ�\B[�
B\��B^{B_
=B`��Ba�Bb�RBc�
Be�BfffBg�Bh��Bj{Bk33Blz�BmBo
=Bp(�Bqp�Br�\Bs�
Bu�Bv=qBw\)Bx��By�B{33B|Q�B}�B
=B�(�B���B�\)B�  B�z�B�
=B���B�=qB��HB��B�(�B��RB�G�B��B�ffB�
=B���B�(�B��RB�G�B��
B�ffB���B��B�{B���B�G�B�B�(�B��HB�\)B��
B�z�B���B�\)B��B�ffB���B��B�  B��\B��B���B�{B���B�G�B��
B�Q�B��HB�\)B��B�z�B���B��B�  B��\B�
=B��B�{B��\B�
=B��B�  B��\B��B��B�  B�ffB��HB�\)B��
B�z�B�
=B���B�=qB��RB�33B�B�Q�B���B�\)B��B�ffB��HB�p�B��B�ffB��HB�\)B�  B�z�B�
=B���B�(�B���B��B�B�z�B���B�p�B�  B��\B�
=B��B�{B�z�B���B�p�B�  B�ffB���B��B�=qB���B�G�B��
B�=qB���B�33B�B�=qB���B�G�B��B\B��B�B�{Bď\B���Bř�B�  BƏ\B��B��
B�Q�B��HB�\)B�B�Q�BʸRB�G�B��B̏\B�33BͮB�=qB���B�G�BϮB�Q�B���B�\)B��
Bҏ\B�33B��
B�ffBԸRB�G�BծB�ffB�
=B׮B�=qB���B�\)B��
B�Q�B���B�\)B��
B܏\B�G�B��
B�ffB޸RB�\)B��
B��B�G�B��B�z�B���B�p�B�  B��B�B�{B�RB�33B�B�=qB��HB陚B�ffB���B뙚B�{B��B�G�B�{B���B�B�  B�z�B�33B��
B�RB�\)B�{B�z�B�G�B��
B���B�p�B�{B��\B�G�B�{B��HB��B�=qB��RB�p�B�{B���B��C 33C p�C ��C(�C��C��C\)C�C�CG�C��C{Cp�C�
C{Cp�C��C=qC��C��CG�C��C  Cz�C�
C	�C	z�C	�
C
G�C
�C{CQ�C�C�C�\C��C33C��C�Cp�C��C{Cp�C��CQ�C��C  C\)C�
C33Cz�C�HCQ�C�RC�C\)CC33C��C��C=qC�\C  C\)C�\C�
C�Cz�CC�HC
=CG�C�\CC��C��C{CQ�C�C�CC�HC
=CQ�Cp�C��C�C�
C��C=qCp�Cz�C��C��C
=C33CQ�CffC�\C��C  C�C33CQ�C��C��C��C��C(�CQ�C�C�C�C�
C 
=C =qC =qC \)C ��C C �HC ��C!{C!Q�C!ffC!�C!��C!�HC"
=C"{C"(�C"G�C"�\C"�RC"�C"��C#{C#33C#=qC#Q�C#��C#C#C#�C$
=C$=qC$Q�C$ffC$��C$C$�C$�C%
=C%33C%p�C%�\C%��C%�RC%��C&�C&�C&=qC&z�C&��C&�C&C&��C'(�C'G�C'Q�C'p�C'�C'�
C'�HC(  C(�C(\)C(p�C(�C(�RC(�C(�C){C)Q�C)p�C)z�C)�\C)�
C*
=C*  C*�C*ffC*�C*��C*C*�C*��C+{C+\)C+�C+�C+��C+�C,
=C,{C,=qC,\)C,��C,�RC,��C,��C-33C-\)C-\)C-�C-C-�C-��C.{C.\)C.�C.�C.�C.�C/�C/(�C/G�C/�\C/�RC/C/�HC0�C0G�C0Q�C0z�C0�RC0�C0�C1{C1Q�C1p�C1�C1�RC1�C1��C2�C2Q�C2�C2��C2�RC2�C3�C333C3Q�C3�\C3C3C3�C4(�C4Q�C4ffC4�\C4��C4�HC5  C5G�C5p�C5z�C5��C5�HC6  C6�C6ffC6�C6��C6C7  C733C7=qC7ffC7�C7C7�HC833C8G�C8ffC8�RC8�HC8�C9�C9\)C9�\C9��C9�HC:{C:�C:Q�C:�\C:��C:��C;{C;G�C;Q�C;p�C;�RC;�C;��C<(�C<ffC<z�C<��C<�HC=
=C={C==qC=�C=�\C=�C=��C>(�C>33C>p�C>��C>�C>�C?�C?33C?p�C?��C?�C?�
C@(�C@33C@z�C@�C@�RCA  CA�CA33CA�CA��CACB
=CB(�CB=qCB�\CB�CBCC{CC(�CCffCC��CC�RCC��CD�CD33CDz�CD��CDCE  CE(�CEG�CE�CE�CECF{CF(�CF\)CF��CF�CF��CG�CG33CGz�CG��CG�RCH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111411114111111111111111114111111141111111141111111141111111111111111111141111111411111111114111111111111411111114111111111111111411111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��TA�|�Aְ!Aգ�A���A�ffA���A��/AӾwAӬAӣ�AӓuAӃA�z�A�t�A�p�A�jA�hsA�ffA�dZA�bNA�^5A�\)A�ZA�VA�S�A�S�A�Q�A�Q�A�Q�A�M�A�M�A�M�A�C�A�/A��A�
=A��mA���AҸRAҬAҡ�A�A�A�C�A��#A�^5A͋DA̍PA�jAʴ9A���Aȣ�A�ƨA�M�A�v�Aĺ^A�ĜA�5?A��9A���A�-A�7LA�Q�A��PA��yA��A�ƨA���A��PA��A�VA�^5A��uA��A�p�A�hsA�jA�=qA��A��9A�r�A�hsA��A�7LA�33A�"�A��`A�oA��A��A�9XA�+A�+A���A��A���A�hsA��HA��RA�S�A���A�A�A�1A���A��A��`A���A���A�z�A���AoA}C�A|  Ax�9AsS�AodZAl��Aj=qAi�-Ahr�Af�yAf�Ad��Ab��A`ffA_?}A^bA]oAZ��AW|�AT�AO�mAM33AKdZAH��AE?}AC\)ABn�AA?}A>��A=�7A;O�A7hsA2��A0M�A.�A,��A*1'A(�A&�A&A%%A#\)A"jA ~�A�A�/A�RAp�A��AbA��A�AE�A�^A�`AA�HAz�A��AoA��A�DA�A33A�AO�AdZA��AA��A�A(�A��A%A^5A�AȴAv�A
��A
{AjAffA�^A?}A�DAƨAXAĜA�A�A�A^5A��A|�A�A�^A�\A�#A�Ap�A ȴA E�@�S�@���@���@�`B@���@���@��-@�J@�;d@�;d@�"�@�@���@�ff@�=q@�@�j@��u@�Q�@�@�S�@�;d@�+@�5?@��D@��@��@�J@�F@��y@�
=@�hs@�Ĝ@�D@�(�@�
=@�V@�J@���@�7@�h@��@��@�  @�+@ޏ\@��#@�?}@��@�S�@��H@��@ٙ�@�`B@���@�bN@�A�@��@��@��@�J@�`B@�%@�Ĝ@�I�@���@�l�@���@�-@ղ-@�`B@�&�@���@�r�@��@ӕ�@�+@�
=@��y@��y@ҟ�@���@ёh@��@�z�@��@ϕ�@�\)@�"�@�
=@���@�@�?}@��`@̛�@�1@˅@���@ʏ\@�V@��#@�X@���@ȴ9@�j@Ǯ@���@���@Ɨ�@�n�@��@���@�x�@ļj@�I�@�|�@�@�J@��#@���@�p�@�O�@���@�1'@��F@��@�t�@�\)@��@��@��R@�^5@��-@�S�@��h@��h@�p�@�?}@�V@��/@���@��@�A�@�|�@��R@�=q@���@�@�Z@��F@�C�@�ȴ@���@���@��j@�Ĝ@���@�j@���@���@�C�@�n�@�=q@��@��@��^@��@���@��u@��@�ƨ@���@�dZ@�
=@��@��H@��\@�{@��@���@��7@�p�@�?}@��@���@� �@�|�@�;d@���@�{@��^@��h@�G�@�7L@�O�@�?}@�G�@��@�r�@�9X@� �@��;@�o@��@��\@�@�@��@���@��D@�1@��@�  @�b@�  @�t�@�@��@���@�^5@�M�@�5?@�J@��T@�O�@�%@��/@� �@���@��@�"�@��H@���@���@�$�@��@�@���@��7@�G�@��j@��@�r�@�z�@�Q�@�I�@�A�@�I�@��@�I�@�  @��w@���@��P@�dZ@���@��\@�E�@��@���@�&�@���@��@���@�dZ@�@�v�@�M�@�J@��^@�O�@���@�Ĝ@�Z@�A�@�(�@���@��w@��P@��@��\@�M�@�$�@��T@��7@�G�@�V@��@��D@�1'@��@�ƨ@�t�@�K�@���@��+@�$�@�J@��^@�X@��@���@�r�@�Z@�1'@�b@�|�@�"�@��@��@�ȴ@���@�ff@��@��^@���@��h@�`B@���@�r�@��@�  @��w@�S�@��y@�ȴ@��R@���@�v�@�5?@��-@�?}@��@�Ĝ@���@�bN@�@��@|�@K�@
=@~�@~�+@~@}�@|�@|�@|�D@|j@|9X@{�
@{��@{t�@{"�@z�!@z~�@z-@zJ@y�^@y%@x��@x�u@xQ�@w�@w�P@wK�@v��@v@u��@u�@u�@t�@t�/@t��@t�D@tZ@s�m@sƨ@so@r=q@q��@qx�@qG�@q�@p��@p�9@p�u@p�@pQ�@p  @o�@o+@n�@n��@n$�@n{@m�T@m?}@l�@lI�@k�
@kt�@j�@j�\@j^5@i��@i�^@ix�@ihs@h��@h�9@hbN@h1'@h  @g�@gl�@gK�@g�@fȴ@fV@f@e��@e�-@e�@eO�@d��@d�j@d9X@d(�@c�
@c33@bn�@a��@aG�@`�`@`Q�@_�@_l�@_�@^��@^�y@^�y@^v�@]�-@]p�@]/@\�/@\I�@[�F@[C�@[o@Z��@Z�!@Z~�@Y��@Yx�@Yhs@YG�@Y&�@XĜ@X1'@W�@W�P@Wl�@W+@V��@VE�@V@U��@Up�@UV@T��@TI�@R��@RM�@Q��@Q%@P�`@P��@O�;@O�P@O\)@O+@O
=@N��@N��@N��@N�@Nff@M�@M`B@MO�@M?}@M�@L�/@Lz�@Lz�@Lj@L1@J�H@J�@I�@I�#@I�^@I��@I7L@H��@HĜ@HbN@HQ�@HQ�@H �@G�P@F��@F@EV@Dj@D�@Cƨ@Ct�@C"�@B�@B��@B~�@B^5@BJ@A��@AX@A&�@@��@@�@@Q�@@ �@@b@@b@?�@?�P@?�@>�@>5?@=�T@=�T@=��@=��@=�-@=`B@=O�@=?}@<��@<�@<(�@;�m@;��@;"�@:�@:�!@:=q@9��@9�@9�7@9G�@8��@8��@8Ĝ@8�9@8bN@8b@7�@7K�@7
=@6�@6�R@6E�@6$�@5�@5@5�h@5�@4��@4j@4Z@4(�@41@3��@3t�@3S�@3C�@2�@1�@1X@1X@1X@1X@1G�@17L@0��@0�@01'@/��@/�@/��@/l�@/
=@.�y@.ȴ@.��@.$�@-�T@-�h@-�@,�@,��@,�D@,j@,9X@+��@+�
@+ƨ@+�F@+��@+��@+��@+33@*�!@*^5@*-@)�@)�^@)��@)��@)x�@)hs@)G�@)�@(��@(�9@(��@(A�@'��@';d@'�@&ȴ@&��@&��@&5?@%�@%��@%��@%`B@%?}@%V@$��@$z�@$I�@$I�@$I�@$I�@$I�@$I�@$I�@$9X@$1@#ƨ@#��@#dZ@#C�@#33@#o@"�H@"n�@"=q@"J@!�@!��@!��@!��@!��@!x�@!G�@!7L@ �`@ ��@ �u@ �@ r�@ 1'@��@+@��@�-@O�@��@�/@�@Z@ƨ@dZ@�@~�@J@��@��@��@7L@��@�9@1'@�;@��@�w@\)@;d@�@
=@�R@��@��@��@v�@$�@@�h@`B@?}@��@��@�@��@�j@�@�@�j@��@z�@I�@��@�F@��@�@dZ@dZ@33@o@�H@n�@^5@J@�@�@�7@X@G�@�@��@��@r�@A�@1'@  @�@��@�P@|�@K�@V@{@@@�T@��@�T@��@��@@�h@O�@��@z�@9X@�m@��@t�@t�@t�@t�@dZ@S�@"�@@
��@
�!@
�\@
^5@
^5@
M�A�bA�oA�bA���A��AׅA�p�A׃A�z�Aם�A�l�A��A���Aְ!A�VA�S�A��Aա�A�9XA� �A��A���AԲ-Aԣ�AԓuA�x�A�M�A�9XA��A�A���A��mA��TA��;A��A���A�ƨAӸRAӴ9AӰ!AӬAӥ�Aӣ�Aӡ�Aӡ�Aӡ�Aӟ�Aӕ�AӑhAӍPAӍPAӉ7AӇ+AӇ+AӇ+AӇ+AӅAӃAӃAӃAӁAӁA�~�A�|�A�z�A�x�A�v�A�t�A�t�A�t�A�r�A�r�A�r�A�r�A�r�A�r�A�p�A�p�A�p�A�n�A�l�A�n�A�n�A�l�A�l�A�jA�jA�jA�jA�hsA�jA�ffA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�bNA�dZA�bNA�`BA�`BA�`BA�`BA�`BA�`BA�`BA�bNA�bNA�bNA�bNA�bNA�bNA�`BA�bNA�`BA�`BA�`BA�`BA�\)A�ZA�ZA�ZA�ZA�XA�XA�XA�XA�XA�XA�VA�VA�S�A�S�A�S�A�S�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�Q�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�M�A�O�A�O�A�Q�A�O�A�O�A�O�A�O�A�M�A�M�A�M�A�K�A�K�A�K�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�I�A�G�A�C�A�9XA�7LA�5?A�1'A�1'A�1'A�/A�/A�+A�(�A�$�A� �A��A��A��A��A��A�oA�VA�VA���A���A��A��A��yA��mA��mA��TA��A���A���A���A�ȴA�ĜA���A���AҾwAҸRAҶFAҲ-AҲ-AҰ!AҰ!AҮAҬAҩ�Aҧ�Aҧ�Aҧ�Aҥ�Aҥ�Aҡ�Aң�Aҡ�Aҝ�Aқ�AғuA҇+A�|�A�t�A�ZA�/A��A��mA���Aћ�AыDAч+A�x�A�C�A���AЃA�\)A�K�A�(�A��Aϙ�AϑhAϋDAω7Aω7AυAσA�~�A�n�A�E�A�
=A��AΗ�A��
A͏\A�Q�A�(�A�$�A�$�A��A��/A̡�A̙�ȂhÃA�`BA�?}A��A���A˧�A�bNA�A�A�{A���A���A�
=A�5?A�oA��A�x�A��A���A��A���AɶFA�A�ȴA��/A��TA���Aɇ+A���A�t�A�K�A�A�A�33A�(�A��A��A�oA�VA��A��A�;dA��AƩ�AƑhA�~�A�jA�M�A��A���A��mA��A���AŴ9AŁA�C�A�(�A��A��A���A��A�AĬAĕ�Aď\AąA�n�A�\)A�33A��HAÃA� �A��A���ADA�G�A�5?A�VA�A��A��#A���A���A�ƨA��RA���A��PA�z�A�S�A�?}A�
=A���A�x�A�G�A�&�A���A��A�t�A�ffA�  A���A��hA��A�^5A�VA�7LA� �A�JA�A���A��A�G�A�9XA�1'A��A�
=A���A��^A���A�n�A�G�A�+A�$�A��A�oA���A���A��DA�XA�;dA��A�bA�%A���A��mA��A���A�A���A��jA��^A��9A��A���A���A��7A�r�A�=qA��yA�\)A���A���A��\A�VA�?}A��A��
A���A��uA��+A�jA�E�A��A��A���A���A�VA��A�  A��TA��9A���A�|�A�l�A�\)A�I�A�1A�JA�ƨA��\A�^5A��;A��PA�;dA��RA�A�A�A���A��-A��;A�?}A���A���A�ZA��A��;A��FA���A�A�A�{A��A��/A���A��!A�S�A�G�A��;A�ȴA��RA���A���A���A��\A�v�A�l�A�n�A�dZA�XA�?}A��A��7A�+A�VA���A���A�|�A�=qA�A��
A��^A���A��DA�p�A�Q�A�33A�&�A�bA��A���A��A��+A�dZA�G�A�+A�JA��A���A���A�~�A�hsA�VA�E�A�9XA�1'A�+A�(�A�(�A�(�A�(�A�&�A�$�A�$�A�(�A�&�A�$�A�$�A�$�A�&�A�&�A�"�A� �A�"�A��A��A��A��A��A��A�oA�VA�VA�
=A���A��A��yA��;A��
A�ȴA��wA��A��PA�\)A�?}A�?}A�9XA�-A�"�A��A�oA�%A���A���A���A��A��A��yA��;A�ȴA���A��DA�t�A�jA�`BA�K�A��A��A��
A�ĜA��RA���A��hA�z�A�`BA�1'A� �A��A�{A�
=A���A���A��mA���A���A��DA�t�A�ZA�&�A��TA��-A���A��uA��DA�v�A�bNA�O�A�7LA���A���A��^A���A���A��DA�~�A�jA�`BA�S�A�G�A�A�A�;dA�33A��A�  A��A��`A��#A�ƨA��A���A�~�A�VA�1'A��A�bA�JA�%A���A��A��HA���A���A��!A��A���A���A���A���A���A���A���A���A���A���A���A���A��uA��PA��\A��DA��+A��+A��+A��A�~�A�|�A�z�A�\)A��A���A��A��A��#A�ƨA��jA��A���A�z�A�n�A�XA��A��TA���A��FA���A�~�A�`BA�XA�S�A�K�A�C�A�33A��yA��uA�`BA�VA�G�A�7LA�+A�A��^A��PA�n�A�S�A�G�A� �A�A��yA��;A���A��RA���A���A���A���A��\A��7A�x�A�jA�`BA�^5A�\)A�VA�XA�VA�G�A�E�A�E�A�33A�(�A�"�A��A��A�%A��A��
A��A���A�z�A��/A��
A�ƨA���A���A��-A�hsA���A��mA���A�n�A���A�|�A�l�A�XA�K�A�;dA�+A�+A�(�A��A�  A��A��/A�ĜA���A��\A��PA�jA�G�A�-A�JA��A���A��FA���A��7A�hsA�K�A�&�A�{A�  A��;A���A��jA��DA�x�A�n�A�K�A�/A��A��A�ȴA��FA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111144111111111111111111111111111111111111111111111                                                                                                                                                                                   A��TA�|�Aְ!Aգ�A���A�ffA���A��/AӾwAӬAӣ�AӓuAӃA�z�A�t�A�p�A�jA�hsA�ffA�dZA�bNA�^5A�\)A�ZA�VA�S�A�S�A�Q�A�Q�A�Q�A�M�A�M�A�M�A�C�A�/A��A�
=A��mA���AҸRAҬAҡ�A�A�A�C�A��#A�^5A͋DA̍PA�jAʴ9A���Aȣ�A�ƨA�M�A�v�Aĺ^A�ĜA�5?A��9A���A�-A�7LA�Q�A��PA��yA��A�ƨA���A��PA��A�VA�^5A��uA��A�p�A�hsA�jA�=qA��A��9A�r�A�hsA��A�7LA�33A�"�A��`A�oA��A��A�9XA�+A�+A���A��A���A�hsA��HA��RA�S�A���A�A�A�1A���A��A��`A���A���A�z�A���AoA}C�A|  Ax�9AsS�AodZAl��Aj=qAi�-Ahr�Af�yAf�Ad��Ab��A`ffA_?}A^bA]oAZ��AW|�AT�AO�mAM33AKdZAH��AE?}AC\)ABn�AA?}A>��A=�7A;O�A7hsA2��A0M�A.�A,��A*1'A(�A&�A&A%%A#\)A"jA ~�A�A�/A�RAp�A��AbA��A�AE�A�^A�`AA�HAz�A��AoA��A�DA�A33A�AO�AdZA��AA��A�A(�A��A%A^5A�AȴAv�A
��A
{AjAffA�^A?}A�DAƨAXAĜA�A�A�A^5A��A|�A�A�^A�\A�#A�Ap�A ȴA E�@�S�@���@���@�`B@���@���@��-@�J@�;d@�;d@�"�@�@���@�ff@�=q@�@�j@��u@�Q�@�@�S�@�;d@�+@�5?@��D@��@��@�J@�F@��y@�
=@�hs@�Ĝ@�D@�(�@�
=@�V@�J@���@�7@�h@��@��@�  @�+@ޏ\@��#@�?}@��@�S�@��H@��@ٙ�@�`B@���@�bN@�A�@��@��@��@�J@�`B@�%@�Ĝ@�I�@���@�l�@���@�-@ղ-@�`B@�&�@���@�r�@��@ӕ�@�+@�
=@��y@��y@ҟ�@���@ёh@��@�z�@��@ϕ�@�\)@�"�@�
=@���@�@�?}@��`@̛�@�1@˅@���@ʏ\@�V@��#@�X@���@ȴ9@�j@Ǯ@���@���@Ɨ�@�n�@��@���@�x�@ļj@�I�@�|�@�@�J@��#@���@�p�@�O�@���@�1'@��F@��@�t�@�\)@��@��@��R@�^5@��-@�S�@��h@��h@�p�@�?}@�V@��/@���@��@�A�@�|�@��R@�=q@���@�@�Z@��F@�C�@�ȴ@���@���@��j@�Ĝ@���@�j@���@���@�C�@�n�@�=q@��@��@��^@��@���@��u@��@�ƨ@���@�dZ@�
=@��@��H@��\@�{@��@���@��7@�p�@�?}@��@���@� �@�|�@�;d@���@�{@��^@��h@�G�@�7L@�O�@�?}@�G�@��@�r�@�9X@� �@��;@�o@��@��\@�@�@��@���@��D@�1@��@�  @�b@�  @�t�@�@��@���@�^5@�M�@�5?@�J@��T@�O�@�%@��/@� �@���@��@�"�@��H@���@���@�$�@��@�@���@��7@�G�@��j@��@�r�@�z�@�Q�@�I�@�A�@�I�@��@�I�@�  @��w@���@��P@�dZ@���@��\@�E�@��@���@�&�@���@��@���@�dZ@�@�v�@�M�@�J@��^@�O�@���@�Ĝ@�Z@�A�@�(�@���@��w@��P@��@��\@�M�@�$�@��T@��7@�G�@�V@��@��D@�1'@��@�ƨ@�t�@�K�@���@��+@�$�@�J@��^@�X@��@���@�r�@�Z@�1'@�b@�|�@�"�@��@��@�ȴ@���@�ff@��@��^@���@��h@�`B@���@�r�@��@�  @��w@�S�@��y@�ȴ@��R@���@�v�@�5?@��-@�?}@��@�Ĝ@���@�bN@�@��@|�@K�@
=@~�@~�+@~@}�@|�@|�@|�D@|j@|9X@{�
@{��@{t�@{"�@z�!@z~�@z-@zJ@y�^@y%@x��@x�u@xQ�@w�@w�P@wK�@v��@v@u��@u�@u�@t�@t�/@t��@t�D@tZ@s�m@sƨ@so@r=q@q��@qx�@qG�@q�@p��@p�9@p�u@p�@pQ�@p  @o�@o+@n�@n��@n$�@n{@m�T@m?}@l�@lI�@k�
@kt�@j�@j�\@j^5@i��@i�^@ix�@ihs@h��@h�9@hbN@h1'@h  @g�@gl�@gK�@g�@fȴ@fV@f@e��@e�-@e�@eO�@d��@d�j@d9X@d(�@c�
@c33@bn�@a��@aG�@`�`@`Q�@_�@_l�@_�@^��@^�y@^�y@^v�@]�-@]p�@]/@\�/@\I�@[�F@[C�@[o@Z��@Z�!@Z~�@Y��@Yx�@Yhs@YG�@Y&�@XĜ@X1'@W�@W�P@Wl�@W+@V��@VE�@V@U��@Up�@UV@T��@TI�@R��@RM�@Q��@Q%@P�`@P��@O�;@O�P@O\)@O+@O
=@N��@N��@N��@N�@Nff@M�@M`B@MO�@M?}@M�@L�/@Lz�@Lz�@Lj@L1@J�H@J�@I�@I�#@I�^@I��@I7L@H��@HĜ@HbN@HQ�@HQ�@H �@G�P@F��@F@EV@Dj@D�@Cƨ@Ct�@C"�@B�@B��@B~�@B^5@BJ@A��@AX@A&�@@��@@�@@Q�@@ �@@b@@b@?�@?�P@?�@>�@>5?@=�T@=�T@=��@=��@=�-@=`B@=O�@=?}@<��@<�@<(�@;�m@;��@;"�@:�@:�!@:=q@9��@9�@9�7@9G�@8��@8��@8Ĝ@8�9@8bN@8b@7�@7K�@7
=@6�@6�R@6E�@6$�@5�@5@5�h@5�@4��@4j@4Z@4(�@41@3��@3t�@3S�@3C�@2�@1�@1X@1X@1X@1X@1G�@17L@0��@0�@01'@/��@/�@/��@/l�@/
=@.�y@.ȴ@.��@.$�@-�T@-�h@-�@,�@,��@,�D@,j@,9X@+��@+�
@+ƨ@+�F@+��@+��@+��@+33@*�!@*^5@*-@)�@)�^@)��@)��@)x�@)hs@)G�@)�@(��@(�9@(��@(A�@'��@';d@'�@&ȴ@&��@&��@&5?@%�@%��@%��@%`B@%?}@%V@$��@$z�@$I�@$I�@$I�@$I�@$I�@$I�@$I�@$9X@$1@#ƨ@#��@#dZ@#C�@#33@#o@"�H@"n�@"=q@"J@!�@!��@!��@!��@!��@!x�@!G�@!7L@ �`@ ��@ �u@ �@ r�@ 1'@��@+@��@�-@O�@��@�/@�@Z@ƨ@dZ@�@~�@J@��@��@��@7L@��@�9@1'@�;@��@�w@\)@;d@�@
=@�R@��@��@��@v�@$�@@�h@`B@?}@��@��@�@��@�j@�@�@�j@��@z�@I�@��@�F@��@�@dZ@dZ@33@o@�H@n�@^5@J@�@�@�7@X@G�@�@��@��@r�@A�@1'@  @�@��@�P@|�@K�@V@{@@@�T@��@�T@��@��@@�h@O�@��@z�@9X@�m@��@t�@t�@t�@t�@dZ@S�@"�@@
��@
�!@
�\@
^5@
^5@
M�A�bA�oA�bA���A��AׅA�p�A׃A�z�Aם�A�l�A��A���Aְ!A�VA�S�A��Aա�A�9XA� �A��A���AԲ-Aԣ�AԓuA�x�A�M�A�9XA��A�A���A��mA��TA��;A��A���A�ƨAӸRAӴ9AӰ!AӬAӥ�Aӣ�Aӡ�Aӡ�Aӡ�Aӟ�Aӕ�AӑhAӍPAӍPAӉ7AӇ+AӇ+AӇ+AӇ+AӅAӃAӃAӃAӁAӁA�~�A�|�A�z�A�x�A�v�A�t�A�t�A�t�A�r�A�r�A�r�A�r�A�r�A�r�A�p�A�p�A�p�A�n�A�l�A�n�A�n�A�l�A�l�A�jA�jA�jA�jA�hsA�jA�ffA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�dZA�bNA�dZA�bNA�`BA�`BA�`BA�`BA�`BA�`BA�`BA�bNA�bNA�bNA�bNA�bNA�bNA�`BA�bNA�`BA�`BA�`BA�`BA�\)A�ZA�ZA�ZA�ZA�XA�XA�XA�XA�XA�XA�VA�VA�S�A�S�A�S�A�S�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�Q�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�O�A�M�A�O�A�O�A�Q�A�O�A�O�A�O�A�O�A�M�A�M�A�M�A�K�A�K�A�K�A�I�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�K�A�I�A�G�A�C�A�9XA�7LA�5?A�1'A�1'A�1'A�/A�/A�+A�(�A�$�A� �A��A��A��A��A��A�oA�VA�VA���A���A��A��A��yA��mA��mA��TA��A���A���A���A�ȴA�ĜA���A���AҾwAҸRAҶFAҲ-AҲ-AҰ!AҰ!AҮAҬAҩ�Aҧ�Aҧ�Aҧ�Aҥ�Aҥ�Aҡ�Aң�Aҡ�Aҝ�Aқ�AғuA҇+A�|�A�t�A�ZA�/A��A��mA���Aћ�AыDAч+A�x�A�C�A���AЃA�\)A�K�A�(�A��Aϙ�AϑhAϋDAω7Aω7AυAσA�~�A�n�A�E�A�
=A��AΗ�A��
A͏\A�Q�A�(�A�$�A�$�A��A��/A̡�A̙�ȂhÃA�`BA�?}A��A���A˧�A�bNA�A�A�{A���A���A�
=A�5?A�oA��A�x�A��A���A��A���AɶFA�A�ȴA��/A��TA���Aɇ+A���A�t�A�K�A�A�A�33A�(�A��A��A�oA�VA��A��A�;dA��AƩ�AƑhA�~�A�jA�M�A��A���A��mA��A���AŴ9AŁA�C�A�(�A��A��A���A��A�AĬAĕ�Aď\AąA�n�A�\)A�33A��HAÃA� �A��A���ADA�G�A�5?A�VA�A��A��#A���A���A�ƨA��RA���A��PA�z�A�S�A�?}A�
=A���A�x�A�G�A�&�A���A��A�t�A�ffA�  A���A��hA��A�^5A�VA�7LA� �A�JA�A���A��A�G�A�9XA�1'A��A�
=A���A��^A���A�n�A�G�A�+A�$�A��A�oA���A���A��DA�XA�;dA��A�bA�%A���A��mA��A���A�A���A��jA��^A��9A��A���A���A��7A�r�A�=qA��yA�\)A���A���A��\A�VA�?}A��A��
A���A��uA��+A�jA�E�A��A��A���A���A�VA��A�  A��TA��9A���A�|�A�l�A�\)A�I�A�1A�JA�ƨA��\A�^5A��;A��PA�;dA��RA�A�A�A���A��-A��;A�?}A���A���A�ZA��A��;A��FA���A�A�A�{A��A��/A���A��!A�S�A�G�A��;A�ȴA��RA���A���A���A��\A�v�A�l�A�n�A�dZA�XA�?}A��A��7A�+A�VA���A���A�|�A�=qA�A��
A��^A���A��DA�p�A�Q�A�33A�&�A�bA��A���A��A��+A�dZA�G�A�+A�JA��A���A���A�~�A�hsA�VA�E�A�9XA�1'A�+A�(�A�(�A�(�A�(�A�&�A�$�A�$�A�(�A�&�A�$�A�$�A�$�A�&�A�&�A�"�A� �A�"�A��A��A��A��A��A��A�oA�VA�VA�
=A���A��A��yA��;A��
A�ȴA��wA��A��PA�\)A�?}A�?}A�9XA�-A�"�A��A�oA�%A���A���A���A��A��A��yA��;A�ȴA���A��DA�t�A�jA�`BA�K�A��A��A��
A�ĜA��RA���A��hA�z�A�`BA�1'A� �A��A�{A�
=A���A���A��mA���A���A��DA�t�A�ZA�&�A��TA��-A���A��uA��DA�v�A�bNA�O�A�7LA���A���A��^A���A���A��DA�~�A�jA�`BA�S�A�G�A�A�A�;dA�33A��A�  A��A��`A��#A�ƨA��A���A�~�A�VA�1'A��A�bA�JA�%A���A��A��HA���A���A��!A��A���A���A���A���A���A���A���A���A���A���A���A���A��uA��PA��\A��DA��+A��+A��+A��A�~�A�|�A�z�A�\)A��A���A��A��A��#A�ƨA��jA��A���A�z�A�n�A�XA��A��TA���A��FA���A�~�A�`BA�XA�S�A�K�A�C�A�33A��yA��uA�`BA�VA�G�A�7LA�+A�A��^A��PA�n�A�S�A�G�A� �A�A��yA��;A���A��RA���A���A���A���A��\A��7A�x�A�jA�`BA�^5A�\)A�VA�XA�VA�G�A�E�A�E�A�33A�(�A�"�A��A��A�%A��A��
A��A���A�z�A��/A��
A�ƨA���A���A��-A�hsA���A��mA���A�n�A���A�|�A�l�A�XA�K�A�;dA�+A�+A�(�A��A�  A��A��/A�ĜA���A��\A��PA�jA�G�A�-A�JA��A���A��FA���A��7A�hsA�K�A�&�A�{A�  A��;A���A��jA��DA�x�A�n�A�K�A�/A��A��A�ȴA��FA���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111144111111111111111111111111111111111111111111111                                                                                                                                                                                   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BB�B�B�B�B4B�B�B�BBxB�BxBxBxB�BB~B�B�B�B�B�B�B�B�BBPBPB�B�B�B"B�B�B
�B1B�B
�VB
��B
�lB
��B
�B
�GB
�PB
�"BVB�BIBW�Bl�BncBe�Bo�BzDB�~B��B˒BںB��B�BB�B1�B>wBE�BE9BEmBVBdZBffBj�BhsBo�B\�BS�BJ�B2�B3�B!�B~B�B��B�sBچB�B�[B�B��B��B�JBs�BiDBe�BjKBe�BV�BFB5�B/�B'RB�B
��B
�,B
�sB
��B
�HB
��B
��B
y>B
I�B
B
oB	�B	�B	��B	��B	��B	��B	��B	��B	�uB	|B	t�B	l�B	_;B	VB	O�B	EmB	AUB	'�B	IB	�B�PB��B�BݘBٴB�[B�BBɺB��B��B�zB�B�9B��B�LB��B�aB�hB��B�9B��B�zB��B��B�XB��B��B�mB�KB�XB��B� B�[BרB�)B�pB�B��B�B�]B�iB�	B�B	�B		7B	�B	�B	#�B	$tB	-CB	J�B	L0B	B[B	E�B	CaB	A�B	IRB	<�B	;0B	33B	A�B	FB	GB	FB	GzB	HKB	R�B	MjB	U�B	YB	h�B	jKB	m]B	.B	��B	��B	��B	�_B	��B	��B	��B	�hB	�"B	��B	��B	�bB	��B	�DB	��B	�+B	�B	�B	��B	��B	��B	��B	��B	��B	�VB	��B	��B	�B	�wB	��B	�tB	��B	�B	��B	�-B	�-B	��B	�B	��B	��B	��B	��B	��B	�CB	�B	��B	��B	��B	��B	��B	��B	��B	�OB	��B	�[B	�tB	�9B	��B	��B	�B	�RB	��B	��B	�dB	�B	�UB	��B	�B	ԕB	�B	�
B	רB	�?B	�mB	՛B	��B	�9B	�mB	��B	�yB	�B	�B	�#B	��B	ٴB	ںB	�)B	�B	�B	�dB	ݘB	�5B	�jB	��B	��B	ߤB	�B	�|B	�B	��B	�B	�B	�B	�2B	��B	�B	��B	�B	�B	��B	�KB	��B	��B	�cB	��B	��B	�B	��B	�B	��B	��B	��B	��B	�B	�]B	��B	�B	� B	��B	�B	��B	�AB	�B	�B	�B	��B	�B	�ZB	�%B	�B	�B	�
B	�B	��B	�B	�yB	�DB	�B	�B	�B	�DB	�>B	�2B	�,B	�`B	�B	� B	�B	�B	�NB	�B	�B	�B	�B	��B	�B	�B	��B	�B	��B	�`B	��B	�B	�sB	�B	�yB	�B	�"B	��B	�"B	�)B	��B	��B	�oB	��B	��B	�B	��B	�lB	��B	�lB	��B	�lB	�ZB	�`B	�2B	�+B	�ZB	�ZB	��B	�	B	�xB	��B	��B	�]B	��B	��B	��B	�]B	�"B	�"B	��B	��B	�VB	��B	��B	�(B	�B	��B
B
B
�B
�B
�B
�B
�B
	lB
	7B
	�B

	B
	�B
DB
B
B
�B
B
�B
�B
�B
"B
"B
\B
�B
�B
�B
�B
.B
�B
B
B
{B
�B
�B
+B
B
�B
�B
VB
VB
VB
!B
�B
OB
~B
B
�B
�B
�B
�B
VB
�B
�B
 'B
!�B
"4B
"hB
#nB
%B
%FB
%�B
&LB
&LB
&�B
&�B
'B
'B
(XB
(�B
)*B
)*B
)�B
)�B
*0B
*eB
)�B
+6B
+B
+6B
+kB
,B
+kB
,=B
-CB
.B
-CB
.B
.�B
.IB
/OB
/OB
.�B
/OB
.�B
0UB
0UB
0�B
0�B
0UB
0�B
1'B
2aB
2�B
33B
2aB
33B
3�B
49B
4�B
4B
4�B
5�B
5�B
6B
5�B
6FB
6zB
7B
7�B
8B
8�B
8B
9$B
9$B
:�B
:�B
:^B
:^B
;0B
:�B
:�B
;dB
;�B
<6B
<6B
;�B
<jB
<6B
<�B
<�B
<jB
<�B
=<B
=B
=B
<�B
<�B
=�B
=qB
<�B
<�B
<�B
<�B
<�B
=qB
=�B
>BB
=�B
>B
=�B
=�B
=qB
=�B
>B
>�B
>�B
?}B
?}B
?�B
@B
@OB
@B
@B
?�B
@B
@B
@B
@�B
A B
AUB
AUB
A�B
A�B
A�B
A�B
C-B
CaB
C�B
C�B
DgB
E9B
EmB
EmB
E�B
E�B
F�B
FtB
F�B
GzB
GEB
GzB
HB
H�B
HKB
HKB
H�B
I�B
I�B
I�B
I�B
J#B
J#B
K^B
K^B
K^B
L0B
L0B
L�B
MjB
M�B
MB
M�B
M�B
NB
NB
NpB
N�B
N�B
N�B
N<B
OvB
O�B
PB
O�B
O�B
PHB
P�B
Q�B
Q�B
Q�B
P�B
Q�B
R�B
S&B
S&B
R�B
R�B
R�B
S�B
S[B
S&B
S&B
S�B
T,B
T,B
S�B
S�B
TaB
TaB
T�B
UgB
W�B
V�B
XyB
XyB
W�B
W�B
W�B
XB
W�B
W?B
WsB
W?B
W�B
XEB
X�B
W�B
W�B
X�B
Y�B
Z�B
Z�B
[�B
\)B
[�B
[�B
[�B
[�B
Z�B
Z�B
Z�B
Z�B
[�B
\]B
\]B
[�B
\]B
[�B
[#B
[#B
[�B
\)B
]�B
_pB
^5B
^�B
^�B
^�B
^�B
^jB
^�B
_B
_;B
_�B
_�B
_�B
_�B
_�B
`BB
`vB
`�B
`vB
`B
`B
`�B
aHB
aB
bNB
b�B
c�B
d�B
d�B
e,B
e�B
e`B
e`B
ffB
f�B
gB
g8B
g�B
hsB
h�B
h>B
hsB
h
B
g�B
h�B
iDB
i�B
i�B
i�B
iyB
jB
i�B
jB
jB
jKB
jB
jB
jKB
jKB
j�B
j�B
jKB
j�B
jKB
j�B
jB
j�B
jB
j�B
jB
j�B
jKB
j�B
k�B
l"B
k�B
l"B
k�B
k�B
k�B
k�B
l"B
lWB
l�B
l�B
l�B
l�B
m)B
l�B
m)B
m)B
m�B
m�B
m)B
m)B
l�B
l�B
m]B
l�B
m)B
m]B
m]B
m�B
m�B
m]B
m�B
m�B
ncB
o B
o5B
o�B
p;B
p;B
o�B
p;B
p;B
p;B
p;B
p�B
poB
qB
poB
qAB
q�B
rB
r|B
r�B
r�B
r|B
sMB
s�B
tB
t�B
t�B
tB
tTB
t�B
u%B
u%B
u%B
u%B
u%B
u%B
uZB
t�B
uZB
uZB
u�B
u�B
v`B
u�B
v+B
v+B
v�B
w2B
v�B
wfB
w�B
xB
w�B
x8B
w�B
xB
x8B
w�B
xlB
x�B
x�B
x�B
x�B
x�B
y>B
zDB
z�B
{B
{B
{�B
{B
{B
{�B
|B
|PB
|�B
|�B
|�B
|�B
}�B
}"B
}�B
}�B
}VB
~(B
~(B
~]B
~�B
~]B
~(B
~(B
~�B
cB
~�B
~�B
~�B
~�B
�B
�B
�B
�4B
� B
��B
�4B
��B
�B
��B
�oB
�;B
��B
��B
��B
�oB
�;B
��B
�B
�AB
�AB
��B
��B
�B
��B
��B
��B
�MB
�B
��B
�B
��B
�MB
��B
��B
��B
�MB
�MB
��B
��B
�B
��B
��B
�B
��B
�+B
��B
�%B
��B
��B
��B
�+B
�_B
��B
��B
�1B
��B
��B
�_B
��B
��B
�7B
�lB
�7B
�B
�B
��B
��B
�lB
�lB
�7B
�	B
��B
�rB
�rB
��B	�B	�B�B�BB	B�B%B	B
��BB&�BSB@BbB�BYB#B�BBMB�BB�BB�BFB:B4B B\B�B"B�B�BB.B�BB�B�B�BPB~B�B�B~B�BJB�B�B�BBJB�BBDBxBxBBBB
�BBxB�B�BJBJBBJBJBJB~BJB~B~B�B~B~BDBxB�BxBB�BJB~B�BB~BPB�B�B�B�B�B�B�B�B�B�B�B�BB�B�B�B�B�B�BB�B�B�B~BBB�B�BDB�B�B�BBBPBPB�BBPB�B�B�B�BPB"B�B�B�B�B�B�B�B�B�B�B�BPBB�B�B�B�B"B�B"B"B�B"BVB�B�BVB"B�B�B"B"B�BVB�B�B�B�B�B�B�B�B�B�B�B(B�B�B�B�B�B�B�B(B�B�B�B�B�B�B�B�B�B�B�B�BB�B�B�B�BJBB�BDB�B
�BDB	7B	B�B	B�B�B�BfBBABoBoB�B 4BB 4B
�(B
�"B
��B
�"B
�PB
�JB
�B
�PB
�B
��B
�B
��B
��B
�DB
�DB
�>B
�lB
��B
�8B
��B
��B
�8B
�B
��B
�`B
��B
�%B
�TB
�%B
�vB
�B
�B
�fB
�vB
�>B
�QB
��B
�KB
�B
�B
��B
�B
�JB
�TB
�`B
��B�B
��B
�B
��B
��B
�JB
�B
�JB
�JB
�(B
��BB
��B
rB%zB�B�BB�B+B
�BMB"B�B�B:B�B�B$BSB!�B�BB�B�B&�B@�B\�Bg8BaBs�BV�BL0B`BB`BYBgBjB~]B��B�uB��B�uBiDB`BBYBYKBU�BU2BTaBT�BU�BXBy�B�B|�Bx8BpoBoiBlWBm�Bn�Bm]BpBrBs�Bv+B{�B��B~�B~�B.B��B�lB�B��B��B��B�hB�:B�@B�FB�-B��B��B��B��BǮB��B�dB̘B�0BӏB�,B�2B�B�?B��B�WB�|B�B��B��B�cB�JB��B�xB�BSB��B��B��B@B�B�B_B1B�B�BDB�B�BeB�B�B�B~B"�B$@B.�B+�B2-B3�B9�B5�B7B8�B9�B=�BDgBJ�BGBG�BF�BE9BB�BD�BFBC�BE�BE�BEBEBC-BD3BD�BE�BCaBG�BE�BNpBWsB`vB`BU2B]dB_�B`BBe,Br|Ba|Ba�Bd�Bi�BiBiDBg�BjBi�Bl�Bl�Bh�BiBj�BgmBkBg8Be�Bc�Bl�Bu�B�_B^5B\)Bb�BVmBc�BR�BX�BL�BHB\�BjBK�Bg�BQ�B>�BFB<�B7�B8B.B1�B,=B(�B'B*�B8�BN<B(�B#�B#�B"�BVBOBOB �BCB7BB�B7B*�BeBYB�BFB�BMB�B{B �B�PB��B�8B��B��B�%B�B�|B�oB�cB�/B��B��B�B��B��B�2B�2B�NB�B��BޞBޞB�/B�BخBںBںB��B�EB��B��B�B��B�EB��B�KB��B��B�EB�yBٴB֡B�?B�?B�yB��B�B��B�mB��B��B��B՛BԕB��BѷB�}BѷB�NB�B�B�BȴB�B�tBƨB�B�zB��B�[B��B��B��B��B��B�UB��B�jB�B��B��B�RB��B��B�dB��B�!B��B�B��B��B�kB�CB��B��B�:B�tB�FB��B�nB��B��B��B��B��B��B��B�-B��B��B��B�4B�\B��B�xB�DB��B�oB�iB{�B{B{By>BxBu�Bv�BsMBsBsMBs�BtBr�Bp�Bo�Bn�Bo�BncBn/Bm�Bm�Bj�BiDBf�Bd�Be�Bf�Bg8Be,Bd�Bf�Be�Bc�Bd�Be�Bd�Bc�BdZBe�Be�BffBh
Bh�BhsBh�BiyBjKBh>Bh�BjKBi�BiDBi�BkQBjBiyBo�BqvBiyBe�Bf�Bg�Be�Bc�Be,Be�Bd&B`vBbNBf2Bc BZ�BYBX�B[WBV�BS�BQ�BR�BP}BR�BT�BR�BMBH�BGBF�BE�BJ�BL0BD�BA�B=qB=<BF�B7�B9�B6�B6�B49B4B2�B2�B3�B2-B1'B5B/�B2aB0!B1'B.�B.�B.B1'B-wB,�B.�B,qB+6B(�B)_B+kB+kB&�B&B%zB!�BQ�B1B�B�B�BB�B�BK�BYBfB�B�B
��B
�>B
��B
�MB
��B
��B
��B
�B
�B
�B
�B
��B
�)B
�B
��B
�B
�B
�B
��B
�B
�B
��B
��B
�jB
ܒB
��B
�dB
�B
�B
רB
�gB
�yB
�sB
�B
�TB
� B
�B
�vB
�HB
��B
ɺB
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111144111111111111111111111111111111111111111111111                                                                                                                                                                                   BB
��B�B
�B	�B	@B�B�B�B!B�B�B�B�B�B�B!B�B�B�B�B�B�B�B�B�B'B\B\B�B�B�B.B�B�B�B =B
��B
�bB
��B
�xB
�B
�B
�SB
�\B
�.BbB	�BUBO�Be BfoB^
Bg�BrPB��B�BÞB��B��B��BB�B)�B6�B=�B=EB=yBNB\fB^rBb�B`Bg�BUBK�BB�B*�B+�B�B�B�B�B�BҒB�B�gB�B��B��B�VBk�BaPB]�BbWB^
BN�B>B-�B'�B^B�B
��B
�8B
�B
��B
�TB
��B
�B
qJB
A�B

B	�{B	�B	��B	��B	��B	��B	��B	��B	��B	z�B	t(B	l�B	d�B	WGB	NB	G�B	=yB	9aB	�B	UB��B�\B��B�BդB��B�gB�NB��B�B��B��B�B�EB��B�XB��B�mB�tB��B�EB��B��B�B��B�dB��B�B�yB�WB�dB��B�,B�gBϴB�5B�|BضB��B�B�iB�uB�B�(B��B	CB	�B	�B	�B	�B	%OB	B�B	D<B	:gB	=�B	;mB	9�B	A^B	4�B	3<B	+?B	9�B	>B	?B	>B	?�B	@WB	J�B	EvB	M�B	Q�B	`�B	bWB	eiB	w:B	}�B	��B	�B	kB	��B	��B	��B	�tB	�.B	��B	��B	�nB	��B	�PB	��B	�7B	�!B	�'B	��B	��B	��B	��B	��B	��B	�bB	��B	��B	�B	��B	��B	��B	��B	�B	��B	�9B	�9B	��B	�B	��B	��B	��B	��B	��B	�OB	�!B	��B	��B	��B	��B	��B	��B	��B	�[B	��B	�gB	��B	�EB	��B	��B	�)B	�^B	�B	��B	�pB	�B	�aB	��B	� B	̡B	�B	�B	ϴB	�KB	�yB	ͧB	��B	�EB	�yB	��B	ЅB	�)B	�)B	�/B	��B	��B	��B	�5B	�B	�B	�pB	դB	�AB	�vB	��B	��B	װB	ضB	وB	�%B	��B	ڎB	ڎB	��B	�>B	��B	߭B	��B	�B	�B	��B	�WB	��B	�B	�oB	�B	��B	�B	�B	�B	��B	�B	� B	��B	�B	�iB	� B	�B	�B	��B	�B	��B	�MB	�B	�B	�%B	��B	�+B	�fB	�1B	�B	�B	�B	�B	��B	�B	�B	�PB	�B	�B	�B	�PB	�JB	�>B	�8B	�lB	ݡB	�,B	�%B	�B	�ZB	�%B	�B	ضB	۔B	��B	��B	ٽB	��B	۔B	��B	�lB	�
B	ާB	�B	�"B	�B	�"B	�.B	��B	�.B	�5B	��B	��B	�{B	��B	�B	�B	��B	�xB	�B	�xB	��B	�xB	�fB	�lB	�>B	�7B	�fB	�fB	��B	�B	�B	��B	�B	�iB	��B	��B	��B	�iB	�.B	�.B	��B	�B	�bB	��B	��B	�4B	�(B	��B	�B	�B	��B	��B	��B	��B	��B
xB
CB
�B
B
�B
PB
B
B
�B
'B
�B
�B
�B
.B
.B
hB
�B
�B
�B
�B
:B
	�B
B
B
�B
�B
�B
7B
B
�B
�B
bB
bB
bB
-B
�B
[B
�B
'B
�B
�B
�B
�B
bB
�B
�B
3B
B
@B
tB
zB
B
RB
�B
XB
XB
�B
�B
*B
*B
 dB
 �B
!6B
!6B
!�B
"B
"<B
"qB
"B
#BB
#B
#BB
#wB
$B
#wB
$IB
%OB
& B
%OB
& B
&�B
&UB
'[B
'[B
&�B
'[B
&�B
(aB
(aB
(�B
(�B
(aB
(�B
)3B
*mB
*�B
+?B
*mB
+?B
+�B
,EB
,�B
,B
,�B
-�B
-�B
.B
-�B
.RB
.�B
/#B
/�B
0)B
0�B
0)B
10B
10B
2�B
2�B
2jB
2jB
3<B
3B
2�B
3pB
3�B
4BB
4BB
3�B
4vB
4BB
4�B
4�B
4vB
4�B
5HB
5B
5B
4�B
4�B
5�B
5}B
4�B
4�B
4�B
4�B
4�B
5}B
5�B
6NB
5�B
6B
5�B
5�B
5}B
5�B
6B
6�B
6�B
7�B
7�B
7�B
8&B
8[B
8&B
8&B
7�B
8&B
8&B
8&B
8�B
9,B
9aB
9aB
9�B
9�B
9�B
9�B
;9B
;mB
;�B
<
B
<sB
=EB
=yB
=yB
=�B
=�B
>�B
>�B
>�B
?�B
?QB
?�B
@#B
@�B
@WB
@WB
@�B
A�B
A�B
A�B
A�B
B/B
B/B
CjB
CjB
CjB
D<B
D<B
D�B
EvB
E�B
EB
E�B
E�B
FB
FB
F|B
F�B
F�B
F�B
FHB
G�B
G�B
H B
G�B
G�B
HTB
H�B
I�B
I�B
I�B
H�B
I�B
J�B
K2B
K2B
J�B
J�B
J�B
K�B
KgB
K2B
K2B
K�B
L8B
L8B
K�B
K�B
LmB
LmB
L�B
MsB
O�B
N�B
P�B
P�B
O�B
O�B
O�B
PB
O�B
OKB
OB
OKB
O�B
PQB
P�B
O�B
O�B
P�B
Q�B
R�B
R�B
S�B
T5B
S�B
S�B
TB
TB
R�B
R�B
R�B
R�B
S�B
TiB
TiB
S�B
TiB
S�B
S/B
S/B
TB
T5B
U�B
W|B
VAB
V�B
V�B
V�B
V�B
VvB
V�B
WB
WGB
W�B
W�B
W�B
W�B
W�B
XNB
X�B
X�B
X�B
XB
XB
X�B
YTB
YB
ZZB
Z�B
[�B
\�B
\�B
]8B
^
B
]lB
]lB
^rB
^�B
_B
_DB
_�B
`B
`�B
`JB
`B
`B
_�B
`�B
aPB
a�B
a�B
a�B
a�B
b"B
a�B
b"B
b�B
bWB
b�B
b�B
bWB
bWB
b�B
b�B
bWB
b�B
bWB
b�B
b�B
b�B
b�B
b�B
b�B
b�B
bWB
b�B
c�B
d.B
c�B
d.B
c�B
c�B
c�B
c�B
d.B
dcB
d�B
d�B
d�B
e B
e5B
e B
e5B
e5B
e�B
e�B
e5B
e5B
e B
e B
eiB
e B
e5B
eiB
eiB
e�B
e�B
eiB
e�B
e�B
foB
gB
gAB
g�B
hGB
hGB
g�B
hGB
hGB
hGB
hGB
h�B
h{B
iB
h{B
iMB
i�B
jB
j�B
j�B
j�B
j�B
kYB
k�B
l+B
l�B
l�B
l+B
l`B
l�B
m1B
m1B
m1B
m1B
m1B
m1B
mfB
l�B
mfB
mfB
nB
m�B
nlB
nB
n7B
n7B
n�B
o>B
o	B
orB
o�B
pB
o�B
pDB
o�B
pB
pDB
o�B
pxB
p�B
p�B
p�B
p�B
p�B
qJB
rPB
r�B
s"B
s�B
s�B
s�B
s�B
s�B
t(B
t\B
t�B
t�B
t�B
t�B
u�B
u.B
u�B
u�B
ubB
v4B
v4B
viB
v�B
viB
v4B
v4B
v�B
woB
wB
wB
wB
wB
w�B
w�B
w�B
x@B
xB
x�B
x@B
x�B
yB
x�B
y{B
yGB
x�B
y�B
y�B
y{B
yGB
y�B
zB
zMB
zMB
y�B
y�B
zB
z�B
{�B
{�B
|YB
|%B
{�B
|%B
|�B
|YB
|�B
|�B
|�B
|YB
|YB
|�B
|�B
}+B
|�B
|�B
}+B
}�B
7B
~�B
~1B
~�B
�B
�B
7B
kB
�B
�	B
�=B
�B
�B
kB
�B
��B
�CB
�xB
�CB
�B
�B
��B
��B
�xB
�xB
�CB
�B
��B
�~B
�~B
��B�B�B �B�B
�BB�B
�1BB
��B
�B�B
�_BLBnB
�B
�eBB�B
BYB�BB�B
B�BRB
FB	@B	BhB�B.B�B�B'B:B�B'B�B�B�B\B�B�B�B�B�BVB�B�B�B!BVB�BBPB�B�BBBB�BB�B�B�BVBVB!BVBVBVB�BVB�B�B�B�B�BPB�B�B�BB�BVB�B�B'B�B\B�B�B�B�B�B�B�B�B�B�B�B�B'B�B�B�B�B�B�B'B�B�B�B�B!B!B�B�BPB�B�B�B!B'B\B\B�B'B\B�B�B�B�B\B.B�B�B�B�B�B�B�B�B�B�B�B\B'B�B�B�B�B.B�B.B.B�B.BbB�B�BbB.B�B�B.B.B�BbB�B�B�B�B�B�B�B�B�B�B�B4B�B�B�B�B�B�B�B4B�B�B�B�B�B�B�B�B�B�B�B�B'B�B�B�B�BVB!B�BPB�B�BPBCBB �BB
��B
��B
��B rB
�%B
�MB
�{B
�{B
��B
�@B
�B
�@B
�4B
�.B
��B
�.B
�\B
�VB
�"B
�\B
�(B
�B
�B
�B
�B
�PB
�PB
�JB
�xB
��B
�DB
��B
�B
�DB
�B
�B
�lB
�B
�1B
�`B
�1B
�B
�%B
�B
�rB
�B
�JB
�]B
��B
�WB
�B
��B
�B
�"B
�VB
�`B
�lB
��B
��B
��B
�(B
��B
�B
�VB
�(B
�VB
�VB
�4B
��B
�+B
��B~B�B�B�B'B �B
�7B�BYB.B�B�B
FB�B
�B0B_BB�B!B�B�B�B8�BUB_DBYBk�BN�BD<BXNBXBQ#B_Bb�BviB��B��B��B��BaPBXNBQ#BQWBM�BM>BLmBL�BM�BPBq�ByBt�BpDBh{BguBdcBe�Bf�BeiBhBjBk�Bn7Bs�Bx�Bv�BwBw:B~�B�xB�!B��B��B��B�tB�FB�LB�RB�9B��B�B��B��B��B��B�pBĤB�<B˛B�8B�>B�B�KB��B�cBوB߭B��B��B�oB�VB��B�B��B_B��B�B�BLB �B��B�kB =B��B�BPB�B�BqB�B�B�B�B�BLB&�B#�B*9B+�B1�B-�B/#B0�B1�B5�B<sBB�B?B?�B>�B=EB:�B<�B>B<
B=�B=�B=B=B;9B<?B<�B=�B;mB?�B=�BF|BOBX�BXBM>BUpBW�BXNB]8Bj�BY�BY�B\�Ba�BaBaPB_�Bb"Ba�Bd�Bd�B`�BaBb�B_yBc(B_DB]�B[�Bd�Bm�BkBVABT5BZ�BNyB[�BJ�BP�BD�B@#BT�Bb"BC�B_�BI�B6�B>B4�B/�B0)B& B)�B$IB �B*B"�B0�BFHB �B�B�B�BbB[B[BBOBCB!B�BCB"�BqBeB�BRB�B�YB �B��B��B�\B�B�DB�B�B�1B�B�B�{B�oB�;B��B��B�B��B��B�>B�>B�ZBضB��B֪B֪B�;BыBкB��B��B��B�QB��B��B�#B��B�QB��B�WB��B��B�QBЅB��BέB�KB�KBЅB��B�B��B�yB��B�B�
BͧB̡B��B��BȉB��B�ZB� B�&B�&B��B�B��B��B�)B��B�
B�gB�
B��B��B��B��B�aB��B�vB�B��B��B�^B��B��B�pB��B�-B��B�'B��B��B�wB�OB��B��B�FB��B�RB��B�zB��B��B��B��B��B�B�B�9B��B��B��B�@B�hB��B��B�PB��By{BxuBs�Bs�Bs"BqJBpBnBn�BkYBk%BkYBk�Bl+Bj�Bh�Bg�Bf�Bg�BfoBf;Be�Be�Bb�BaPB^�B]B^
B^�B_DB]8B]B^�B]�B[�B]B]�B\�B[�B\fB]�B^
B^rB`B`�B`B`�Ba�BbWB`JB`�BbWBa�BaPBa�Bc]Bb�Ba�Bg�Bi�Ba�B]�B^�B_�B]�B[�B]8B]�B\2BX�BZZB^>B[,BR�BQ�BP�BScBN�BK�BI�BJ�BH�BJ�BM
BJ�BEB@�B?B>�B=�BB�BD<B<�B9�B5}B5HB>�B/�B1�B.�B.�B,EB,B*�B*�B+�B*9B)3B-B'�B*mB(-B)3B&�B&�B& B)3B%�B$�B&�B$}B#BB �B!kB#wB#wB�B$B�BBI�B=B�B�BB*B�B�BDB
�eB rB	�B
��B
��B
�JB
�B
�YB
��B
��B
��B
�B
�B
�B
�B
��B
�5B
�(B
��B
�B
�B
߭B
��B
ݡB
�%B
��B
��B
�vB
ԞB
��B
�pB
�B
�)B
ϴB
�sB
ЅB
�B
�&B
�`B
�,B
�&B
ǂB
�TB
��B
��B
�)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114411111144111111111111111111111111111111111111111111111                                                                                                                                                                                   G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230730020102                            20230730020102AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023073002010220230730020102  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023073002010220230730020102QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023073002010220230730020102QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               