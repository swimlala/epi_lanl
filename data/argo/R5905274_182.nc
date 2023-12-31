CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:22Z creation      
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
resolution        =���   axis      Z        x  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [X   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  c8   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     x  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ɀ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  �`   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x 0   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 7�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x ?�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     x f�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �X   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �$   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �,   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230426223222  20230426223222  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @��C�-p@��C�-p11  @��ww� @��ww� @/�h	ԕ@/�h	ԕ�d.
�L/��d.
�L/�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AB  AB  ?��@   @=p�@�G�@��R@�  @�  A   A  A\)A*�HA@  A`  A~�RA�\)A�  A���A�  A�\)A�  A�  B (�BQ�BQ�BQ�B   B'�
B/�
B7�B?�BG�
BP  BX  B`  Bh  Bp(�Bx(�B�{B�{B�  B��B�  B�{B��B�  B�{B�{B�{B��B�  B�  B�{B��B��
B��
B��
B��B�  B�  B�{B��B��
B�{B�  B��
B�  B�{B��
B�  C   C  C  C��C
=C

=C(�C{C  C  C  C
=C
=C
=C  C  C 
=C"  C$  C%��C(  C*  C,  C-��C0  C2  C3��C6  C8
=C:  C;��C=��C?��CB  CD
=CF  CH
=CJ  CK��CN
=CP
=CR  CT
=CV
=CW��CY��C\  C^
=C`  Ca��Cd
=Cf  Cg��Ci�Ck��Cm��Co�Cq�Cs�Cu�HCw�Cz
=C|  C}�C�  C���C��C���C�C�  C���C�C�C���C���C���C�  C�C�  C���C�C�  C���C�
=C�  C�C�  C�C�  C���C�  C�C�C�C�  C���C�  C�  C���C�  C�C�C�C�
=C�C�C�  C�  C�
=C�
=C�C�  C�C�
=C�  C���C�  C�  C�  C���C��C���C���C���C���C���C���C���C���C���C�  C�C�  C�  C�C���C��C��C�  C�C���C�  C�
=C�C�
=C�  C���C�C�C�C�C�  C�C�C�  C���C�C�  C�  C���C���C�  C�  C�  C�  C���C���C�  C�
=C�  C���C���C���C�  C�  C�  C�  C�  C���C���C�  C�
=C���C�C�\C�C���C���C���C�  C�C�D �D }qD ��Dz�D�RD}qD�D� D  D� D  D��D�D�D�D�D  D��D	D	�D
  D
z�D
��D��D�D�D�D��DD� D�RDz�D  D��DD�D�D��D�D� D  D� D  D� DD�D  D� D�D� D  D�DD}qD�qD��D�Dz�D��D}qD�D��D��Dz�D��D z�D �qD!}qD"  D"��D#D#�D#�qD$}qD$��D%}qD&�D&��D'D'�D(  D(� D)  D)� D*�D*�D+D+�D,�D,� D,�qD-�D.D.}qD.��D/� D0�D0��D1�D1��D1�RD2}qD2�qD3z�D3�RD4� D5D5xRD6�D6��D6��D7}qD7��D8}qD9  D9��D9�qD:��D;�D;z�D;�qD<�D<�qD=}qD=��D>z�D?D?� D?��D@� D@�qDA��DB  DB��DB��DC}qDD  DD}qDE�DE��DF  DF}qDG�DG��DH  DH}qDI  DI� DI��DJ� DK�DK� DK�qDL� DM  DM� DN  DN��DO  DO}qDO�qDP� DQ  DQ}qDR  DR� DS  DS}qDT  DT��DT�qDUz�DU��DVxRDV��DW}qDW�qDX� DY  DY� DZ�DZ� DZ�qD[}qD\�D\��D]  D]� D^  D^� D^�qD_}qD`  D`}qD`�qDa}qDa�qDb� Dc�Dc�Dd�Dd��Dd�qDe}qDf�Df��Dg�Dg� Dh  Dh��Di�Di��Dj  Dj��Dk  Dk� Dl�Dl}qDm  Dm��DnDn��Do  Do}qDo��Dp� Dp�qDq}qDrDr��Dr��Ds��DtDt� Dt�RDu� Du��Dv}qDw  Dwz�Dx  Dx��Dy  Dy��Dz�Dz}qDz�qD{� D|�D|}qD}�D}� D}��D~�D  D�D�qD�B�D�� D�� D�  D�>�D�~�D��HD���D�@ D��HD��HD�  D�AHD�� D�� D�  D�@ D���D��HD��D�@ D��HD��HD�  D�AHD�� D���D�  D�@ D�� D�� D�  D�B�D��HD��HD��qD�@ D�� D�� D��qD�AHD�~�D��HD��D�B�D��HD���D�HD�>�D�~�D�� D��)D�>�D�~�D�D�  D�@ D��HD�� D���D�@ D�� D�� D�HD�@ D�~�D���D���D�AHD�� D��HD���D�>�D�~�D��HD�HD�@ D�~�D��qD���D�@ D�� D���D�HD�>�D��HD�� D���D�AHD���D��HD�  D�@ D�~�D�� D�  D�>�D�~�D��HD���D�=qD�� D��HD��D�>�D�� D�� D�HD�AHD�~�D�� D�  D�>�D�}qD�� D�  D�@ D�� D�� D���D�>�D��HD��HD�  D�B�D���D��HD���D�@ D�� D�� D�  D�@ D�~�D���D���D�>�D�� D�� D�  D�AHD�~�D��HD�HD�>�D�~�D�� D�  D�@ D��HD�� D���D�AHD�� D��HD�  D�@ D���D�� D���D�@ D��HD���D���D�>�D�� D�� D���D�@ D�� D��HD�HD�>�D�}qD��qD��qD�>�D�}qD�� D�  D�AHD�� D��HD��D�B�D��HD�� D�  D�>�D�~�D�� D�  D�@ D�~�D���D�  D�AHD��HD���D��qD�>�D�~�D�� D�  D�AHD�~�D���D�HD�@ D�~�D�� D���D�@ D�~�D�� D�HD�AHD�~�D�� D�  D�@ D�� D���D�  D�AHD��HD�� D���D�@ D��HD�D�HD�@ D��HD�� D���D�AHD�� D�� D�  D�B�D��HD�� D�HD�AHD���D��HD��qD�AHD��HD��HD��qD�@ D��HD�� D���D�@ D���D��HD�HD�@ D��HD��HD�  D�AHDHD�� D�HD�@ DÁHD��HD���D�>�DāHD�� D�  D�@ Dŀ Dž�D��qD�@ D�~�DƽqD�  D�>�D�~�D�� D�HD�AHD�~�DȽqD�  D�B�D�~�Dɾ�D�  D�@ Dʀ D��HD���D�>�Dˀ D��HD�HD�AHD�~�D��HD��qD�@ D̀ D�D���D�AHD�}qD��HD�HD�AHD�~�D�� D�HD�@ DЂ�D�� D�  D�@ Dр D�� D�HD�>�D�~�DҾ�D�  D�@ DӁHD��HD�HD�AHDԀ D�� D�HD�AHDՀ D�D�  D�@ Dւ�D�� D���D�AHDׁHD�� D�HD�@ D؁HD��HD���D�>�D�~�D�� D�  D�@ D�}qD�� D�  D�@ DہHD��HD��D�B�D܁HD�� D�  D�=qD݀ D��HD�HD�>�DށHD��HD�  D�AHD߁HD��HD�HD�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D�  D�@ D�HD��HD��D�@ D� D��HD��D�@ D� D�� D�  D�AHD悏D�D�  D�@ D� D羸D�  D�@ D�}qD�� D�  D�@ D� D�� D�HD�AHD�HD�� D�  D�@ D� D�� D�HD�AHD� D쾸D�  D�@ D� D�� D���D�>�D�HD�� D��D�@ D�~�D�� D�  D�@ D�~�D�D���D�@ D� D��HD�HD�@ D�~�D�D���D�@ D� D�D���D�>�D� D�� D�  D�@ D��HD�� D���D�>�D��HD�� D���D�AHD��HD��HD�  D�@ D��HD�� D�  D�@ D���D��HD�HD�>�?\)?W
=?��?��R?�Q�?�
=?��@�@�@!G�@333@=p�@J=q@^�R@k�@xQ�@�G�@���@�\)@�
=@�p�@��
@��@�33@��H@�  @���@У�@�
=@޸R@��@�{@�z�@�(�A�\AA	��A{A�AA��Ap�A!�A%�A)��A,��A0��A4z�A8Q�A;�A@  AC33AG
=AK�AN�RAR�\AVffAY��A^{A`��Ae�Ai��Amp�AqG�Au�Ax��A}p�A�Q�A�=qA�z�A�ffA�  A��HA�(�A�ffA���A�33A���A�ffA���A��\A���A��RA���A��HA��A�
=A���A��A�p�A��A���A��
A�A�  A���A��A��A�\)A���AÅA�p�A�  A�=qA�z�A�ffA�Q�Aҏ\A�z�AָRAأ�A��HA�z�A޸RA��A��HA���A�\)A���A�33A�p�A�\)A�G�A�33A���A��RA�Q�A�=qA�(�A�ffB (�BG�B=qB33B  B��BB�HB�
B��B	B
�HB�
B��B=qB�Bz�BB�RB�B��B�B
=B  B��BB�RB�
B��B{B
=B (�B!G�B"{B#\)B$Q�B%B&�RB'�
B(��B*{B+33B,z�B-��B.�\B/�B0��B1B3
=B4(�B4��B5B6�HB8  B9�B:{B;33B<Q�B=p�B>�RB@  B@��BB{BC33BDQ�BEG�BF=qBG
=BH  BI�BJffBK\)BLQ�BMp�BN�HBP(�BQG�BRffBS\)BT��BUBV�HBX  BY�BZ=qB[\)B\(�B]G�B^ffB_�B`��BaBb�HBd(�Be�Bf=qBg\)Bhz�Bi�Bk33Blz�Bmp�Bn�RBo�
Bq�Br{Br�HBt  Bu�Bv=qBw�By�Bz{B{\)B|z�B}��B~=qB�B�Q�B��HB�\)B�  B�z�B��B�B�ffB��B��B�Q�B��HB�p�B�  B�z�B���B��B�(�B���B�33B��
B�ffB�
=B��B�(�B��HB��B�{B��RB�G�B�B�=qB���B�p�B��B��\B��B�B�ffB���B�p�B�{B��RB�p�B�{B��RB�G�B�B�=qB���B��B�  B��\B��B��B�=qB��HB�p�B�=qB���B�G�B��B�z�B���B�\)B�  B��\B��B���B�Q�B�
=B���B�{B���B�G�B��
B�Q�B��RB�G�B�B�ffB��B�B�Q�B���B�p�B�B�=qB���B�\)B��B��RB�G�B�B�Q�B��RB�G�B��B�=qB���B�\)B��
B�z�B�33B��B�(�B��RB�33B���B�(�B���B��B��
B�z�B�
=B���B�{B�ffB�
=BÅB�(�B���B�p�B�{B�z�B�
=BǙ�B�{B���BɅB�{Bʣ�B�
=BˮB�=qB��BͮB�ffB��HB�p�B�{B���Bљ�B�Q�B���B�\)B�{B���Bՙ�B�Q�BָRB�p�B�{B���BٮB�(�B��HB�p�B�ffB��B�B�=qB��HBߙ�B�z�B�33B�B�ffB��B�  B�RB�33B��
B�RB�p�B�(�B�\B�G�B�{B���B�p�B��B�\B�B�(�B�\B�33B�{B�RB�\)B��
B�z�B�\)B�  B�ffB��B��
B���B�33B�B�Q�B�G�B�{B��\B�33B��
B���B��B��B���B���C �C \)C �RC�C�C�HC�Cp�C�CG�Cp�C��C=qC�\CC{C�C�HC{CffC�HC=qCz�C��CG�C�C�
C	33C	�C
  C
=qC
�\C  CG�Cp�CC{C=qCp�C�RC�C  C(�CffC�\C��C�CC  C(�C�C=qCp�C��C��C�RC�HC{C�C33CQ�C�C��C��CC��C{C{C(�CffC�C��C��C�RC��C
=C{C(�CffCz�Cz�C��C�C�C
=C
=C�C=qCp�C�\C�\C��C��C��C{C{C�CffC�Cz�C��CC�C�C  C33C\)CffCz�C��CC�HC�HC  C33CQ�C\)Cp�C�C�
C�
C��C(�CG�CG�CffC��C��C��C�C�C=qCG�C\)C��CCC�HC�C33CG�CffC��C��CC��C�C�CG�C�C��C�CC
=C=qC33C\)C��C��C��C�C�C(�CG�C�\C�C�RC�
C�C33CG�C�C�C�RC�
C{C33CG�C�\C�RC�RC�HC(�CQ�CQ�Cp�C�RC��C�C33CQ�CffC�C�HC�HC {C Q�C \)C �C C ��C ��C!=qC!G�C!p�C!�C!�RC!�C"(�C"33C"\)C"��C"�C"�
C#�C#=qC#Q�C#�\C#�RC#��C${C$33C$G�C$�\C$�C$C%
=C%33C%G�C%�C%��C%C&
=C&�C&=qC&�C&�\C&C'
=C'{C'G�C'�\C'�\C'��C({C({C(\)C(�\C(��C(�HC){C)�C)p�C)�\C)�C)��C*(�C*33C*z�C*�C*�RC+  C+33C+=qC+�C+C+��C,{C,=qC,Q�C,��C,C,�
C-{C-G�C-Q�C-��C-C-��C.�C.=qC.Q�C.��C.�RC.�HC/�C/=qC/Q�C/��C/�C/�
C0�C033C0ffC0��C0�C0��C1(�C133C1z�C1�C1C2  C2
=C2Q�C2ffC2�C2��C2�HC3(�C3G�C3\)C3��C3�RC3�C433C4G�C4�C4��C4C5{C5(�C5p�C5��C5�C5��C6
=C6=qC6�C6��C6�
C6��C7{C7ffC7z�C7C7�HC8  C8Q�C8\)C8��C8�RC9  C9{C9G�C9�\C9��C9�HC9��C:(�C:p�C:z�C:C:�
C;�C;Q�C;\)C;�C;��C<{C<(�C<\)C<�\C<�RC=  C=
=C=\)C=p�C=�RC=�
C>�C>=qC>�C>��C>�
C?{C?33C?z�C?��C?�HC@  C@G�C@\)C@�C@CA
=CA=qCA\)CA�CACB
=CB(�CBp�CB�CB��CB�CC�CCQ�CC�CCCC��CD�CD33CDz�CD�\CD�
CD�CE33CEG�CE��CE��CE�HCF{CF33CFz�CF�\CF�HCF�CGG�CGG�CG��CG�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111411141111111111114111141111411114111411141111111141111111411141114111111141111111111411111114111111111111111111111111411141111111114111111111111111111111111111111111111111111111114114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111                                                                                                                                ?��@   @=p�@�G�@��R@�  @�  A   A  A\)A*�HA@  A`  A~�RA�\)A�  A���A�  A�\)A�  A�  B (�BQ�BQ�BQ�B   B'�
B/�
B7�B?�BG�
BP  BX  B`  Bh  Bp(�Bx(�B�{B�{B�  B��B�  B�{B��B�  B�{B�{B�{B��B�  B�  B�{B��B��
B��
B��
B��B�  B�  B�{B��B��
B�{B�  B��
B�  B�{B��
B�  C   C  C  C��C
=C

=C(�C{C  C  C  C
=C
=C
=C  C  C 
=C"  C$  C%��C(  C*  C,  C-��C0  C2  C3��C6  C8
=C:  C;��C=��C?��CB  CD
=CF  CH
=CJ  CK��CN
=CP
=CR  CT
=CV
=CW��CY��C\  C^
=C`  Ca��Cd
=Cf  Cg��Ci�Ck��Cm��Co�Cq�Cs�Cu�HCw�Cz
=C|  C}�C�  C���C��C���C�C�  C���C�C�C���C���C���C�  C�C�  C���C�C�  C���C�
=C�  C�C�  C�C�  C���C�  C�C�C�C�  C���C�  C�  C���C�  C�C�C�C�
=C�C�C�  C�  C�
=C�
=C�C�  C�C�
=C�  C���C�  C�  C�  C���C��C���C���C���C���C���C���C���C���C���C�  C�C�  C�  C�C���C��C��C�  C�C���C�  C�
=C�C�
=C�  C���C�C�C�C�C�  C�C�C�  C���C�C�  C�  C���C���C�  C�  C�  C�  C���C���C�  C�
=C�  C���C���C���C�  C�  C�  C�  C�  C���C���C�  C�
=C���C�C�\C�C���C���C���C�  C�C�D �D }qD ��Dz�D�RD}qD�D� D  D� D  D��D�D�D�D�D  D��D	D	�D
  D
z�D
��D��D�D�D�D��DD� D�RDz�D  D��DD�D�D��D�D� D  D� D  D� DD�D  D� D�D� D  D�DD}qD�qD��D�Dz�D��D}qD�D��D��Dz�D��D z�D �qD!}qD"  D"��D#D#�D#�qD$}qD$��D%}qD&�D&��D'D'�D(  D(� D)  D)� D*�D*�D+D+�D,�D,� D,�qD-�D.D.}qD.��D/� D0�D0��D1�D1��D1�RD2}qD2�qD3z�D3�RD4� D5D5xRD6�D6��D6��D7}qD7��D8}qD9  D9��D9�qD:��D;�D;z�D;�qD<�D<�qD=}qD=��D>z�D?D?� D?��D@� D@�qDA��DB  DB��DB��DC}qDD  DD}qDE�DE��DF  DF}qDG�DG��DH  DH}qDI  DI� DI��DJ� DK�DK� DK�qDL� DM  DM� DN  DN��DO  DO}qDO�qDP� DQ  DQ}qDR  DR� DS  DS}qDT  DT��DT�qDUz�DU��DVxRDV��DW}qDW�qDX� DY  DY� DZ�DZ� DZ�qD[}qD\�D\��D]  D]� D^  D^� D^�qD_}qD`  D`}qD`�qDa}qDa�qDb� Dc�Dc�Dd�Dd��Dd�qDe}qDf�Df��Dg�Dg� Dh  Dh��Di�Di��Dj  Dj��Dk  Dk� Dl�Dl}qDm  Dm��DnDn��Do  Do}qDo��Dp� Dp�qDq}qDrDr��Dr��Ds��DtDt� Dt�RDu� Du��Dv}qDw  Dwz�Dx  Dx��Dy  Dy��Dz�Dz}qDz�qD{� D|�D|}qD}�D}� D}��D~�D  D�D�qD�B�D�� D�� D�  D�>�D�~�D��HD���D�@ D��HD��HD�  D�AHD�� D�� D�  D�@ D���D��HD��D�@ D��HD��HD�  D�AHD�� D���D�  D�@ D�� D�� D�  D�B�D��HD��HD��qD�@ D�� D�� D��qD�AHD�~�D��HD��D�B�D��HD���D�HD�>�D�~�D�� D��)D�>�D�~�D�D�  D�@ D��HD�� D���D�@ D�� D�� D�HD�@ D�~�D���D���D�AHD�� D��HD���D�>�D�~�D��HD�HD�@ D�~�D��qD���D�@ D�� D���D�HD�>�D��HD�� D���D�AHD���D��HD�  D�@ D�~�D�� D�  D�>�D�~�D��HD���D�=qD�� D��HD��D�>�D�� D�� D�HD�AHD�~�D�� D�  D�>�D�}qD�� D�  D�@ D�� D�� D���D�>�D��HD��HD�  D�B�D���D��HD���D�@ D�� D�� D�  D�@ D�~�D���D���D�>�D�� D�� D�  D�AHD�~�D��HD�HD�>�D�~�D�� D�  D�@ D��HD�� D���D�AHD�� D��HD�  D�@ D���D�� D���D�@ D��HD���D���D�>�D�� D�� D���D�@ D�� D��HD�HD�>�D�}qD��qD��qD�>�D�}qD�� D�  D�AHD�� D��HD��D�B�D��HD�� D�  D�>�D�~�D�� D�  D�@ D�~�D���D�  D�AHD��HD���D��qD�>�D�~�D�� D�  D�AHD�~�D���D�HD�@ D�~�D�� D���D�@ D�~�D�� D�HD�AHD�~�D�� D�  D�@ D�� D���D�  D�AHD��HD�� D���D�@ D��HD�D�HD�@ D��HD�� D���D�AHD�� D�� D�  D�B�D��HD�� D�HD�AHD���D��HD��qD�AHD��HD��HD��qD�@ D��HD�� D���D�@ D���D��HD�HD�@ D��HD��HD�  D�AHDHD�� D�HD�@ DÁHD��HD���D�>�DāHD�� D�  D�@ Dŀ Dž�D��qD�@ D�~�DƽqD�  D�>�D�~�D�� D�HD�AHD�~�DȽqD�  D�B�D�~�Dɾ�D�  D�@ Dʀ D��HD���D�>�Dˀ D��HD�HD�AHD�~�D��HD��qD�@ D̀ D�D���D�AHD�}qD��HD�HD�AHD�~�D�� D�HD�@ DЂ�D�� D�  D�@ Dр D�� D�HD�>�D�~�DҾ�D�  D�@ DӁHD��HD�HD�AHDԀ D�� D�HD�AHDՀ D�D�  D�@ Dւ�D�� D���D�AHDׁHD�� D�HD�@ D؁HD��HD���D�>�D�~�D�� D�  D�@ D�}qD�� D�  D�@ DہHD��HD��D�B�D܁HD�� D�  D�=qD݀ D��HD�HD�>�DށHD��HD�  D�AHD߁HD��HD�HD�>�D�~�DྸD���D�>�D�~�DᾸD���D�>�D�~�D⾸D�  D�@ D�HD��HD��D�@ D� D��HD��D�@ D� D�� D�  D�AHD悏D�D�  D�@ D� D羸D�  D�@ D�}qD�� D�  D�@ D� D�� D�HD�AHD�HD�� D�  D�@ D� D�� D�HD�AHD� D쾸D�  D�@ D� D�� D���D�>�D�HD�� D��D�@ D�~�D�� D�  D�@ D�~�D�D���D�@ D� D��HD�HD�@ D�~�D�D���D�@ D� D�D���D�>�D� D�� D�  D�@ D��HD�� D���D�>�D��HD�� D���D�AHD��HD��HD�  D�@ D��HD�� D�  D�@ D���D��HD�HD�>�?\)?W
=?��?��R?�Q�?�
=?��@�@�@!G�@333@=p�@J=q@^�R@k�@xQ�@�G�@���@�\)@�
=@�p�@��
@��@�33@��H@�  @���@У�@�
=@޸R@��@�{@�z�@�(�A�\AA	��A{A�AA��Ap�A!�A%�A)��A,��A0��A4z�A8Q�A;�A@  AC33AG
=AK�AN�RAR�\AVffAY��A^{A`��Ae�Ai��Amp�AqG�Au�Ax��A}p�A�Q�A�=qA�z�A�ffA�  A��HA�(�A�ffA���A�33A���A�ffA���A��\A���A��RA���A��HA��A�
=A���A��A�p�A��A���A��
A�A�  A���A��A��A�\)A���AÅA�p�A�  A�=qA�z�A�ffA�Q�Aҏ\A�z�AָRAأ�A��HA�z�A޸RA��A��HA���A�\)A���A�33A�p�A�\)A�G�A�33A���A��RA�Q�A�=qA�(�A�ffB (�BG�B=qB33B  B��BB�HB�
B��B	B
�HB�
B��B=qB�Bz�BB�RB�B��B�B
=B  B��BB�RB�
B��B{B
=B (�B!G�B"{B#\)B$Q�B%B&�RB'�
B(��B*{B+33B,z�B-��B.�\B/�B0��B1B3
=B4(�B4��B5B6�HB8  B9�B:{B;33B<Q�B=p�B>�RB@  B@��BB{BC33BDQ�BEG�BF=qBG
=BH  BI�BJffBK\)BLQ�BMp�BN�HBP(�BQG�BRffBS\)BT��BUBV�HBX  BY�BZ=qB[\)B\(�B]G�B^ffB_�B`��BaBb�HBd(�Be�Bf=qBg\)Bhz�Bi�Bk33Blz�Bmp�Bn�RBo�
Bq�Br{Br�HBt  Bu�Bv=qBw�By�Bz{B{\)B|z�B}��B~=qB�B�Q�B��HB�\)B�  B�z�B��B�B�ffB��B��B�Q�B��HB�p�B�  B�z�B���B��B�(�B���B�33B��
B�ffB�
=B��B�(�B��HB��B�{B��RB�G�B�B�=qB���B�p�B��B��\B��B�B�ffB���B�p�B�{B��RB�p�B�{B��RB�G�B�B�=qB���B��B�  B��\B��B��B�=qB��HB�p�B�=qB���B�G�B��B�z�B���B�\)B�  B��\B��B���B�Q�B�
=B���B�{B���B�G�B��
B�Q�B��RB�G�B�B�ffB��B�B�Q�B���B�p�B�B�=qB���B�\)B��B��RB�G�B�B�Q�B��RB�G�B��B�=qB���B�\)B��
B�z�B�33B��B�(�B��RB�33B���B�(�B���B��B��
B�z�B�
=B���B�{B�ffB�
=BÅB�(�B���B�p�B�{B�z�B�
=BǙ�B�{B���BɅB�{Bʣ�B�
=BˮB�=qB��BͮB�ffB��HB�p�B�{B���Bљ�B�Q�B���B�\)B�{B���Bՙ�B�Q�BָRB�p�B�{B���BٮB�(�B��HB�p�B�ffB��B�B�=qB��HBߙ�B�z�B�33B�B�ffB��B�  B�RB�33B��
B�RB�p�B�(�B�\B�G�B�{B���B�p�B��B�\B�B�(�B�\B�33B�{B�RB�\)B��
B�z�B�\)B�  B�ffB��B��
B���B�33B�B�Q�B�G�B�{B��\B�33B��
B���B��B��B���B���C �C \)C �RC�C�C�HC�Cp�C�CG�Cp�C��C=qC�\CC{C�C�HC{CffC�HC=qCz�C��CG�C�C�
C	33C	�C
  C
=qC
�\C  CG�Cp�CC{C=qCp�C�RC�C  C(�CffC�\C��C�CC  C(�C�C=qCp�C��C��C�RC�HC{C�C33CQ�C�C��C��CC��C{C{C(�CffC�C��C��C�RC��C
=C{C(�CffCz�Cz�C��C�C�C
=C
=C�C=qCp�C�\C�\C��C��C��C{C{C�CffC�Cz�C��CC�C�C  C33C\)CffCz�C��CC�HC�HC  C33CQ�C\)Cp�C�C�
C�
C��C(�CG�CG�CffC��C��C��C�C�C=qCG�C\)C��CCC�HC�C33CG�CffC��C��CC��C�C�CG�C�C��C�CC
=C=qC33C\)C��C��C��C�C�C(�CG�C�\C�C�RC�
C�C33CG�C�C�C�RC�
C{C33CG�C�\C�RC�RC�HC(�CQ�CQ�Cp�C�RC��C�C33CQ�CffC�C�HC�HC {C Q�C \)C �C C ��C ��C!=qC!G�C!p�C!�C!�RC!�C"(�C"33C"\)C"��C"�C"�
C#�C#=qC#Q�C#�\C#�RC#��C${C$33C$G�C$�\C$�C$C%
=C%33C%G�C%�C%��C%C&
=C&�C&=qC&�C&�\C&C'
=C'{C'G�C'�\C'�\C'��C({C({C(\)C(�\C(��C(�HC){C)�C)p�C)�\C)�C)��C*(�C*33C*z�C*�C*�RC+  C+33C+=qC+�C+C+��C,{C,=qC,Q�C,��C,C,�
C-{C-G�C-Q�C-��C-C-��C.�C.=qC.Q�C.��C.�RC.�HC/�C/=qC/Q�C/��C/�C/�
C0�C033C0ffC0��C0�C0��C1(�C133C1z�C1�C1C2  C2
=C2Q�C2ffC2�C2��C2�HC3(�C3G�C3\)C3��C3�RC3�C433C4G�C4�C4��C4C5{C5(�C5p�C5��C5�C5��C6
=C6=qC6�C6��C6�
C6��C7{C7ffC7z�C7C7�HC8  C8Q�C8\)C8��C8�RC9  C9{C9G�C9�\C9��C9�HC9��C:(�C:p�C:z�C:C:�
C;�C;Q�C;\)C;�C;��C<{C<(�C<\)C<�\C<�RC=  C=
=C=\)C=p�C=�RC=�
C>�C>=qC>�C>��C>�
C?{C?33C?z�C?��C?�HC@  C@G�C@\)C@�C@CA
=CA=qCA\)CA�CACB
=CB(�CBp�CB�CB��CB�CC�CCQ�CC�CCCC��CD�CD33CDz�CD�\CD�
CD�CE33CEG�CE��CE��CE�HCF{CF33CFz�CF�\CF�HCF�CGG�CGG�CG��CG�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111411141111111111114111141111411114111411141111111141111111411141114111111141111111111411111114111111111111111111111111411141111111114111111111111111111111111111111111111111111111114114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�x�A�v�A�x�A�|�A�t�A�v�A�|�A҃A҉7A҇+A҇+A҇+A҉7A҇+A҅A҅A҇+AҋDAҋDA҉7A҇+A҇+A�z�A�O�A�K�A�I�A�I�A�M�A�Q�A�VA�\)A�^5A�bNA�hsA�^5A�^5A�^5A�^5A�^5A�^5A�bNA�dZA�VA�M�A�M�A�O�A�O�A�K�A�E�A�7LA�{A���A��A�  A̲-A�1A�  A��A�E�A��yA��A� �A���A�(�A�?}A�p�A�(�A��RA�bNA�$�A�JA�1A�ƨA�p�A�K�A���A���A�A��A�^5A���A��wA�JA��A��#A�jA�l�A��hA���A�^5A�S�A�ZA��FA��/A��A���A�ȴA���A�?}A�ĜA��A�Q�A~n�A}&�AzffAs+Amp�Aj5?Ai
=Ag�Aa��A^�A\��AZ  AVz�AT  AOXAL�AK|�AE�
ABJA@�9A?�A<M�A9p�A6�9A6A3�A1�A0Q�A.��A-t�A,�RA+�-A*�9A*1'A(5?A%��A$��A$E�A#A!�
A ��AQ�AK�AjA�7A^5A�A��AbNA{A��AdZA?}A"�A�A��AVA��A=qA�Al�A��A�A�Ap�A33A�A��A�+A�Av�A�-A7LA�/A�+A�AVA"�A��AA�A�AA�A^5AffAVA�PA	�7A	�PA	t�A	t�A	"�A��A1'A?}A�+A��A�/AI�A�7A�HA�A(�AbA�Ax�A+A �@�\)@�5?@��^@��@�7L@��j@��m@���@��y@���@�/@���@�v�@��@��/@��j@�@��H@���@��@�^@��T@�$�@���@�7L@�Ĝ@�z�@�
=@�@�1'@�@�@ꟾ@��@�D@�I�@�l�@柾@�n�@���@�G�@�bN@㕁@�+@�=q@��/@�1@߮@�33@���@��#@ݑh@�x�@ܼj@�C�@ڟ�@ف@���@أ�@�Q�@�b@���@׮@�t�@�C�@��y@�v�@�=q@��@�@պ^@�hs@�7L@�/@�V@ԋD@�(�@��
@�l�@�J@ѩ�@љ�@ёh@ёh@�O�@�j@��m@�t�@�dZ@��@ΰ!@�V@ͩ�@�r�@� �@�l�@��@�ff@�E�@�{@ɺ^@ț�@Ǿw@ǅ@�\)@�33@��H@Ƨ�@�J@ũ�@�&�@��@�C�@�~�@�5?@���@�hs@�/@��@�z�@�  @�ƨ@��F@���@��@�v�@��@�G�@���@���@�ƨ@��F@���@�+@��@��\@�@�p�@��`@�j@�9X@�  @��
@�\)@���@�V@��#@�G�@���@��@� �@���@�33@��@��!@�^5@�J@��#@�@��7@�O�@���@��D@�Q�@��@��@�33@���@�@���@�V@�@�G�@�A�@�33@��y@��R@�n�@�=q@���@��@��@���@��u@�b@�;d@��@�ȴ@�v�@�=q@��#@�`B@�%@�Z@��@�33@�ȴ@���@�ff@���@�G�@�%@�Ĝ@�9X@��P@��y@��R@��!@��+@�M�@���@�`B@�/@���@��@���@��w@��@���@��H@��@�ȴ@���@�v�@�{@���@��@�G�@��@�Ĝ@��u@�bN@�1'@�b@�  @��
@��@�@��+@�ff@�M�@�E�@�=q@�$�@��#@��^@���@�&�@���@���@�Z@��@��;@��w@�\)@��@�@��y@�v�@��@��@���@���@��7@�&�@�Ĝ@���@��m@�|�@�S�@�33@�@��@���@�~�@�^5@���@��@��@��^@��@�?}@�&�@�%@���@��@��@��@��
@���@���@�l�@�C�@�
=@��@��!@�ff@�-@���@�7L@���@���@��j@���@���@�z�@�bN@�(�@�1@��@�l�@�+@�"�@�@��!@�=q@���@��T@���@���@���@�x�@�hs@�?}@���@���@��@�Q�@��@��
@���@���@�\)@��y@��\@��\@�~�@�v�@�v�@�^5@�5?@���@�`B@�/@���@��`@���@�I�@�w@\)@�@~�y@~�R@~�+@~v�@~v�@~E�@}�@}��@}`B@|�@{�m@{��@{o@z��@z~�@z^5@zM�@z-@y��@y%@xĜ@w�@v��@v�+@v$�@u@u?}@t��@tz�@sdZ@r��@r=q@rJ@qx�@p�@pA�@p  @o�@ol�@n�+@m�@m@m��@m�@m`B@m�@lz�@k�F@k��@k��@kdZ@k33@j�\@i��@i&�@h��@hr�@hQ�@h �@h  @g|�@g;d@f��@f�+@fE�@e�T@ep�@e�@d�j@d�D@dI�@c�m@cC�@b��@b^5@b=q@bJ@aX@`Ĝ@`bN@_;d@^�@^�R@^�+@^V@^{@]p�@]O�@]/@\�@\j@\I�@[ƨ@[t�@[33@Z�!@Z^5@Z�@Y�@Y�^@Y�7@Y�@XĜ@X1'@W|�@W;d@W�@V�y@V�R@V5?@U�-@U�@U?}@UV@T�@S�m@St�@R~�@RM�@RM�@RM�@R-@Qx�@P��@P�u@O��@O;d@Nff@M�-@M�@L�D@L1@K��@Ko@J�!@J~�@J^5@J^5@J�@I7L@H�@HA�@H  @G��@G|�@G+@Fȴ@FV@F{@E@E�h@E?}@DI�@C�F@Ct�@CS�@C33@Co@Co@B�!@A��@A�^@Ahs@A7L@@�`@@��@@1'@?�P@>��@>��@>v�@>v�@=�@=?}@<�/@<�D@<j@;�m@;dZ@;@:��@:~�@:~�@:^5@:^5@:=q@:J@9�#@9�7@9X@9&�@8��@8�9@8��@8�@8bN@7�@7|�@7+@6��@6ȴ@6��@6�+@6$�@5��@5`B@4��@4��@4�D@4j@4�@3�
@3��@3dZ@333@3o@2�@2�!@2�\@2~�@2=q@1�#@1��@17L@1%@0Ĝ@0�9@0�9@0�@0A�@/�@/�@/|�@/;d@/�@.��@.��@.�R@.v�@.$�@-��@-`B@-O�@-/@-�@,��@,�@,�D@,j@,I�@,1@+��@+C�@+"�@*�H@*-@*�@)�#@)G�@)%@(Ĝ@(Ĝ@(�9@(r�@( �@'�;@'��@'\)@'+@&�@&��@&ff@&5?@%�@%p�@%�@$��@$�@$��@$z�@$(�@#�m@#�
@#��@#C�@"��@"�\@"M�@!��@!��@!�7@!x�@!X@!7L@!7L@!�@ �`@ �9@ r�@ A�@   @�@�@l�@\)@+@�y@ȴ@��@E�@E�@$�@��@@p�@?}@/@�@�@��@�j@j@�@��@�m@��@�@t�@dZ@C�@o@��@n�@^5@M�@��@x�@%@�`@Ĝ@��@r�@1'@  @��@�P@|�@l�@+@�@ȴ@�R@v�@V@E�@5?@�T@��@�@`B@?}@�@�/@�D@�D@z�@j@(�@��@�F@S�@S�@C�@33@@�H@��@�!@�\@^5@-@��@�@��@��@�7@hs@X@7L@7L@%@Ĝ@�@1'@��@��@|�@|�@K�@�@��@�y@�y@�@v�@$�@@��@p�@?}@�@V@��@�D@Z@I�@(�@1@1@�m@ƨ@��@t�@dZ@33@"�@o@@
�@
��@
~�@
M�@
-@
�@
J@	�#@	��@	X@	7L@	�@�`@Ĝ@�9@�u@Q�@A�@bA�|�A�x�A�z�A�z�A�z�A�z�A�x�A�x�A�x�A�z�A�v�A�x�A�v�A�v�A҅A҇+A�|�A�t�A�r�A�p�A�t�A�x�A�p�A�t�A�v�A�r�A�r�A�r�A�x�A�|�A�~�A�|�A�|�AҁA҃A҅A҇+A҇+A҇+A҇+A҇+A҅A҅A҃A�~�A҅A҇+A҇+A҇+A҅A҃AҁAҁA҃A҇+A҉7A҇+A҇+A҇+A҇+A҇+A҉7A҉7A҉7A҉7A҉7A҉7A҇+A҉7AҋDAҋDA҉7A҃A҅A҅A҅A҃A҅A҅A҃A҇+A҉7A҉7A҅A҃A҅A҇+A҇+A҅A҃A҅A҇+A҇+AҋDAҋDAҋDAҍPAҍPAҍPAҋDAҋDAҋDAҋDAҋDAҋDAҋDAҋDAҋDAҋDA҉7A҉7A҉7A҉7A҉7A҉7A҇+A҇+A҇+A҅A҅A҅A҅A҇+A҇+A҇+A҅A҇+A҇+A҇+A҇+A҇+A҅A҇+AҁA҅A҃A҃AҁA҃A҃AҁA�x�A�^5A�\)A�S�A�M�A�K�A�K�A�K�A�K�A�I�A�G�A�G�A�I�A�I�A�K�A�K�A�K�A�K�A�K�A�M�A�M�A�M�A�M�A�M�A�K�A�K�A�K�A�K�A�K�A�K�A�I�A�I�A�I�A�I�A�I�A�I�A�M�A�M�A�M�A�O�A�S�A�Q�A�S�A�VA�VA�XA�VA�VA�VA�VA�VA�VA�VA�VA�XA�ZA�ZA�`BA�`BA�`BA�bNA�bNA�bNA�`BA�`BA�\)A�ZA�ZA�XA�XA�^5A�^5A�`BA�dZA�hsA�p�A�r�A�p�A�n�A�bNA�`BA�bNA�dZA�bNA�^5A�^5A�^5A�^5A�^5A�^5A�\)A�\)A�ZA�ZA�ZA�^5A�^5A�`BA�`BA�`BA�^5A�\)A�\)A�\)A�^5A�^5A�`BA�bNA�bNA�bNA�bNA�bNA�bNA�`BA�^5A�ZA�XA�ZA�ZA�ZA�\)A�`BA�`BA�bNA�`BA�bNA�bNA�bNA�dZA�dZA�ffA�ffA�ffA�ffA�ffA�dZA�bNA�^5A�\)A�ZA�XA�S�A�S�A�S�A�Q�A�Q�A�Q�A�Q�A�O�A�M�A�K�A�K�A�I�A�K�A�K�A�M�A�M�A�M�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�M�A�M�A�M�A�M�A�O�A�O�A�Q�A�Q�A�Q�A�M�A�K�A�G�A�E�A�C�A�C�A�A�A�A�A�C�A�E�A�G�A�E�A�C�A�?}A�;dA�5?A�/A�-A�(�A�&�A�$�A�$�A��A�
=A�%A���A��A��`A��HA��HA��HAѺ^Aћ�A�x�A�`BA�+A���A�ĜAк^Aа!A�~�A�Q�A��A�n�A�1A�^5A�bA���A�XA��A��A�A��yA�?}A˴9A�bNA�=qA�9XA�=qA�"�A��A�x�A���A�dZA��A�t�A�;dAƝ�A�`BA��A�n�A�-A�9XA��yAº^A¥�APA�dZA�E�A��A��A��^A���A�jA��PA��A��uA�Q�A�bA��
A��^A���A��+A�l�A�K�A� �A�1A��A�ȴA���A��-A���A���A��uA��7A�v�A�ZA�-A���A��A��jA�l�A�ZA�G�A�-A���A��7A�z�A�|�A�jA�bNA�dZA�ffA�VA�O�A�A�A�5?A�{A�z�A��9A�{A��A�C�A��RA��\A�~�A�jA�dZA�O�A�C�A�A�A�5?A�$�A��A��A��A�{A�
=A�
=A�
=A�JA�
=A�1A�JA�JA�
=A�A�A�1A���A��A��`A���A�VA� �A�-A�M�A�%A���A�z�A�JA���A�hsA�bA�ȴA�C�A��A���A�^5A�{A���A�33A��PA�S�A�+A� �A�{A�bA�
=A�1A���A��yA��
A���A�A��-A���A��7A�z�A�ffA�S�A�O�A�I�A�;dA�"�A�oA�oA�VA�%A���A���A��yA��TA��A���A���A���A���A�ȴA���A�ȴA�ĜA���A��wA��jA��FA���A��A�r�A�jA�dZA�ZA�M�A�C�A�9XA�-A��A�%A���A��yA��A��RA���A��uA��+A�t�A�hsA�\)A�I�A�33A�bA��A��`A��#A���A���A��RA���A��A�x�A�r�A�VA��mA�r�A��\A�t�A�&�A��A�{A�1A���A���A��A��mA��/A��/A��/A��A��A��
A���A���A���A�ȴA�A��FA���A�t�A�I�A�5?A��A�  A�ƨA��\A�~�A�hsA�M�A��A��/A���A��A���A��`A��/A���A���A�A��^A��!A��uA�-A��`A��HA���A���A��A�^5A�/A���A���A��A��uA�v�A�S�A�/A���A�p�A�;dA�oA��`A���A��RA���A���A��+A�p�A�\)A�S�A�VA�S�A�ZA�ZA�XA�`BA�bNA�bNA�dZA�dZA�dZA�bNA�\)A�S�A�ZA�dZA�|�A��DA���A���A��PA�n�A�E�A��A�ȴA�n�A�C�A�+A���A���A�n�A�Q�A�?}A��TA��wA��FA��RA��A��!A��uA�l�A�I�A�/A��A�
=A���A��A�ȴA���A�XA��A���A���A��RA��!A���A���A�p�A�7LA���A��uA�bA��#A��A�=qA�A��mA��-A��A�l�A�XA�S�A�K�A�G�A�A�A�1'A�$�A��A�bA�  A��yA���A�A�ĜA��9A���A���A�r�A�ZA�K�A�?}A�?}A�=qA�-A��A�  A��yA���A���A�z�A�ffA�G�A��A���A��A���A�+A���A�ȴA���A�dZA�9XA��A���A��TA���A���A��DA�S�A�VA��A��\A�\)A��A�ĜA��\A�(�A���A�hsA�VA�1'A��
A�XA��9A��A�jA�`BA�VA�M�A�G�A�=qA�1'A��A��`A���A���A��A�l�A�\)A�bA�dZA�Q�A�=qA�/A��A�1A���A��jA�p�A���A�A�jA��A��A���A�I�A��A��yA��`A��#A���A���A�C�A�33A���A�XA��A���A���A���A��A�|�A�n�A�dZA�^5A�Q�A�M�A�M�A� �A�JA��A�^Al�AA~~�A}�A}�;A}�wA}��A}�hA}�7A}x�A}dZA}XA}K�A}?}A}&�A}�A|��A|�A|�/A|��A|z�A|bA{�wA{��A{�A{`BAzĜAydZAx�AwO�Au�#AuVAt{As��As�hG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                A�x�A�v�A�x�A�|�A�t�A�v�A�|�A҃A҉7A҇+A҇+A҇+A҉7A҇+A҅A҅A҇+AҋDAҋDA҉7A҇+A҇+A�z�A�O�A�K�A�I�A�I�A�M�A�Q�A�VA�\)A�^5A�bNA�hsA�^5A�^5A�^5A�^5A�^5A�^5A�bNA�dZA�VA�M�A�M�A�O�A�O�A�K�A�E�A�7LA�{A���A��A�  A̲-A�1A�  A��A�E�A��yA��A� �A���A�(�A�?}A�p�A�(�A��RA�bNA�$�A�JA�1A�ƨA�p�A�K�A���A���A�A��A�^5A���A��wA�JA��A��#A�jA�l�A��hA���A�^5A�S�A�ZA��FA��/A��A���A�ȴA���A�?}A�ĜA��A�Q�A~n�A}&�AzffAs+Amp�Aj5?Ai
=Ag�Aa��A^�A\��AZ  AVz�AT  AOXAL�AK|�AE�
ABJA@�9A?�A<M�A9p�A6�9A6A3�A1�A0Q�A.��A-t�A,�RA+�-A*�9A*1'A(5?A%��A$��A$E�A#A!�
A ��AQ�AK�AjA�7A^5A�A��AbNA{A��AdZA?}A"�A�A��AVA��A=qA�Al�A��A�A�Ap�A33A�A��A�+A�Av�A�-A7LA�/A�+A�AVA"�A��AA�A�AA�A^5AffAVA�PA	�7A	�PA	t�A	t�A	"�A��A1'A?}A�+A��A�/AI�A�7A�HA�A(�AbA�Ax�A+A �@�\)@�5?@��^@��@�7L@��j@��m@���@��y@���@�/@���@�v�@��@��/@��j@�@��H@���@��@�^@��T@�$�@���@�7L@�Ĝ@�z�@�
=@�@�1'@�@�@ꟾ@��@�D@�I�@�l�@柾@�n�@���@�G�@�bN@㕁@�+@�=q@��/@�1@߮@�33@���@��#@ݑh@�x�@ܼj@�C�@ڟ�@ف@���@أ�@�Q�@�b@���@׮@�t�@�C�@��y@�v�@�=q@��@�@պ^@�hs@�7L@�/@�V@ԋD@�(�@��
@�l�@�J@ѩ�@љ�@ёh@ёh@�O�@�j@��m@�t�@�dZ@��@ΰ!@�V@ͩ�@�r�@� �@�l�@��@�ff@�E�@�{@ɺ^@ț�@Ǿw@ǅ@�\)@�33@��H@Ƨ�@�J@ũ�@�&�@��@�C�@�~�@�5?@���@�hs@�/@��@�z�@�  @�ƨ@��F@���@��@�v�@��@�G�@���@���@�ƨ@��F@���@�+@��@��\@�@�p�@��`@�j@�9X@�  @��
@�\)@���@�V@��#@�G�@���@��@� �@���@�33@��@��!@�^5@�J@��#@�@��7@�O�@���@��D@�Q�@��@��@�33@���@�@���@�V@�@�G�@�A�@�33@��y@��R@�n�@�=q@���@��@��@���@��u@�b@�;d@��@�ȴ@�v�@�=q@��#@�`B@�%@�Z@��@�33@�ȴ@���@�ff@���@�G�@�%@�Ĝ@�9X@��P@��y@��R@��!@��+@�M�@���@�`B@�/@���@��@���@��w@��@���@��H@��@�ȴ@���@�v�@�{@���@��@�G�@��@�Ĝ@��u@�bN@�1'@�b@�  @��
@��@�@��+@�ff@�M�@�E�@�=q@�$�@��#@��^@���@�&�@���@���@�Z@��@��;@��w@�\)@��@�@��y@�v�@��@��@���@���@��7@�&�@�Ĝ@���@��m@�|�@�S�@�33@�@��@���@�~�@�^5@���@��@��@��^@��@�?}@�&�@�%@���@��@��@��@��
@���@���@�l�@�C�@�
=@��@��!@�ff@�-@���@�7L@���@���@��j@���@���@�z�@�bN@�(�@�1@��@�l�@�+@�"�@�@��!@�=q@���@��T@���@���@���@�x�@�hs@�?}@���@���@��@�Q�@��@��
@���@���@�\)@��y@��\@��\@�~�@�v�@�v�@�^5@�5?@���@�`B@�/@���@��`@���@�I�@�w@\)@�@~�y@~�R@~�+@~v�@~v�@~E�@}�@}��@}`B@|�@{�m@{��@{o@z��@z~�@z^5@zM�@z-@y��@y%@xĜ@w�@v��@v�+@v$�@u@u?}@t��@tz�@sdZ@r��@r=q@rJ@qx�@p�@pA�@p  @o�@ol�@n�+@m�@m@m��@m�@m`B@m�@lz�@k�F@k��@k��@kdZ@k33@j�\@i��@i&�@h��@hr�@hQ�@h �@h  @g|�@g;d@f��@f�+@fE�@e�T@ep�@e�@d�j@d�D@dI�@c�m@cC�@b��@b^5@b=q@bJ@aX@`Ĝ@`bN@_;d@^�@^�R@^�+@^V@^{@]p�@]O�@]/@\�@\j@\I�@[ƨ@[t�@[33@Z�!@Z^5@Z�@Y�@Y�^@Y�7@Y�@XĜ@X1'@W|�@W;d@W�@V�y@V�R@V5?@U�-@U�@U?}@UV@T�@S�m@St�@R~�@RM�@RM�@RM�@R-@Qx�@P��@P�u@O��@O;d@Nff@M�-@M�@L�D@L1@K��@Ko@J�!@J~�@J^5@J^5@J�@I7L@H�@HA�@H  @G��@G|�@G+@Fȴ@FV@F{@E@E�h@E?}@DI�@C�F@Ct�@CS�@C33@Co@Co@B�!@A��@A�^@Ahs@A7L@@�`@@��@@1'@?�P@>��@>��@>v�@>v�@=�@=?}@<�/@<�D@<j@;�m@;dZ@;@:��@:~�@:~�@:^5@:^5@:=q@:J@9�#@9�7@9X@9&�@8��@8�9@8��@8�@8bN@7�@7|�@7+@6��@6ȴ@6��@6�+@6$�@5��@5`B@4��@4��@4�D@4j@4�@3�
@3��@3dZ@333@3o@2�@2�!@2�\@2~�@2=q@1�#@1��@17L@1%@0Ĝ@0�9@0�9@0�@0A�@/�@/�@/|�@/;d@/�@.��@.��@.�R@.v�@.$�@-��@-`B@-O�@-/@-�@,��@,�@,�D@,j@,I�@,1@+��@+C�@+"�@*�H@*-@*�@)�#@)G�@)%@(Ĝ@(Ĝ@(�9@(r�@( �@'�;@'��@'\)@'+@&�@&��@&ff@&5?@%�@%p�@%�@$��@$�@$��@$z�@$(�@#�m@#�
@#��@#C�@"��@"�\@"M�@!��@!��@!�7@!x�@!X@!7L@!7L@!�@ �`@ �9@ r�@ A�@   @�@�@l�@\)@+@�y@ȴ@��@E�@E�@$�@��@@p�@?}@/@�@�@��@�j@j@�@��@�m@��@�@t�@dZ@C�@o@��@n�@^5@M�@��@x�@%@�`@Ĝ@��@r�@1'@  @��@�P@|�@l�@+@�@ȴ@�R@v�@V@E�@5?@�T@��@�@`B@?}@�@�/@�D@�D@z�@j@(�@��@�F@S�@S�@C�@33@@�H@��@�!@�\@^5@-@��@�@��@��@�7@hs@X@7L@7L@%@Ĝ@�@1'@��@��@|�@|�@K�@�@��@�y@�y@�@v�@$�@@��@p�@?}@�@V@��@�D@Z@I�@(�@1@1@�m@ƨ@��@t�@dZ@33@"�@o@@
�@
��@
~�@
M�@
-@
�@
J@	�#@	��@	X@	7L@	�@�`@Ĝ@�9@�u@Q�@A�@bA�|�A�x�A�z�A�z�A�z�A�z�A�x�A�x�A�x�A�z�A�v�A�x�A�v�A�v�A҅A҇+A�|�A�t�A�r�A�p�A�t�A�x�A�p�A�t�A�v�A�r�A�r�A�r�A�x�A�|�A�~�A�|�A�|�AҁA҃A҅A҇+A҇+A҇+A҇+A҇+A҅A҅A҃A�~�A҅A҇+A҇+A҇+A҅A҃AҁAҁA҃A҇+A҉7A҇+A҇+A҇+A҇+A҇+A҉7A҉7A҉7A҉7A҉7A҉7A҇+A҉7AҋDAҋDA҉7A҃A҅A҅A҅A҃A҅A҅A҃A҇+A҉7A҉7A҅A҃A҅A҇+A҇+A҅A҃A҅A҇+A҇+AҋDAҋDAҋDAҍPAҍPAҍPAҋDAҋDAҋDAҋDAҋDAҋDAҋDAҋDAҋDAҋDA҉7A҉7A҉7A҉7A҉7A҉7A҇+A҇+A҇+A҅A҅A҅A҅A҇+A҇+A҇+A҅A҇+A҇+A҇+A҇+A҇+A҅A҇+AҁA҅A҃A҃AҁA҃A҃AҁA�x�A�^5A�\)A�S�A�M�A�K�A�K�A�K�A�K�A�I�A�G�A�G�A�I�A�I�A�K�A�K�A�K�A�K�A�K�A�M�A�M�A�M�A�M�A�M�A�K�A�K�A�K�A�K�A�K�A�K�A�I�A�I�A�I�A�I�A�I�A�I�A�M�A�M�A�M�A�O�A�S�A�Q�A�S�A�VA�VA�XA�VA�VA�VA�VA�VA�VA�VA�VA�XA�ZA�ZA�`BA�`BA�`BA�bNA�bNA�bNA�`BA�`BA�\)A�ZA�ZA�XA�XA�^5A�^5A�`BA�dZA�hsA�p�A�r�A�p�A�n�A�bNA�`BA�bNA�dZA�bNA�^5A�^5A�^5A�^5A�^5A�^5A�\)A�\)A�ZA�ZA�ZA�^5A�^5A�`BA�`BA�`BA�^5A�\)A�\)A�\)A�^5A�^5A�`BA�bNA�bNA�bNA�bNA�bNA�bNA�`BA�^5A�ZA�XA�ZA�ZA�ZA�\)A�`BA�`BA�bNA�`BA�bNA�bNA�bNA�dZA�dZA�ffA�ffA�ffA�ffA�ffA�dZA�bNA�^5A�\)A�ZA�XA�S�A�S�A�S�A�Q�A�Q�A�Q�A�Q�A�O�A�M�A�K�A�K�A�I�A�K�A�K�A�M�A�M�A�M�A�O�A�O�A�Q�A�Q�A�Q�A�Q�A�Q�A�O�A�M�A�M�A�M�A�M�A�O�A�O�A�Q�A�Q�A�Q�A�M�A�K�A�G�A�E�A�C�A�C�A�A�A�A�A�C�A�E�A�G�A�E�A�C�A�?}A�;dA�5?A�/A�-A�(�A�&�A�$�A�$�A��A�
=A�%A���A��A��`A��HA��HA��HAѺ^Aћ�A�x�A�`BA�+A���A�ĜAк^Aа!A�~�A�Q�A��A�n�A�1A�^5A�bA���A�XA��A��A�A��yA�?}A˴9A�bNA�=qA�9XA�=qA�"�A��A�x�A���A�dZA��A�t�A�;dAƝ�A�`BA��A�n�A�-A�9XA��yAº^A¥�APA�dZA�E�A��A��A��^A���A�jA��PA��A��uA�Q�A�bA��
A��^A���A��+A�l�A�K�A� �A�1A��A�ȴA���A��-A���A���A��uA��7A�v�A�ZA�-A���A��A��jA�l�A�ZA�G�A�-A���A��7A�z�A�|�A�jA�bNA�dZA�ffA�VA�O�A�A�A�5?A�{A�z�A��9A�{A��A�C�A��RA��\A�~�A�jA�dZA�O�A�C�A�A�A�5?A�$�A��A��A��A�{A�
=A�
=A�
=A�JA�
=A�1A�JA�JA�
=A�A�A�1A���A��A��`A���A�VA� �A�-A�M�A�%A���A�z�A�JA���A�hsA�bA�ȴA�C�A��A���A�^5A�{A���A�33A��PA�S�A�+A� �A�{A�bA�
=A�1A���A��yA��
A���A�A��-A���A��7A�z�A�ffA�S�A�O�A�I�A�;dA�"�A�oA�oA�VA�%A���A���A��yA��TA��A���A���A���A���A�ȴA���A�ȴA�ĜA���A��wA��jA��FA���A��A�r�A�jA�dZA�ZA�M�A�C�A�9XA�-A��A�%A���A��yA��A��RA���A��uA��+A�t�A�hsA�\)A�I�A�33A�bA��A��`A��#A���A���A��RA���A��A�x�A�r�A�VA��mA�r�A��\A�t�A�&�A��A�{A�1A���A���A��A��mA��/A��/A��/A��A��A��
A���A���A���A�ȴA�A��FA���A�t�A�I�A�5?A��A�  A�ƨA��\A�~�A�hsA�M�A��A��/A���A��A���A��`A��/A���A���A�A��^A��!A��uA�-A��`A��HA���A���A��A�^5A�/A���A���A��A��uA�v�A�S�A�/A���A�p�A�;dA�oA��`A���A��RA���A���A��+A�p�A�\)A�S�A�VA�S�A�ZA�ZA�XA�`BA�bNA�bNA�dZA�dZA�dZA�bNA�\)A�S�A�ZA�dZA�|�A��DA���A���A��PA�n�A�E�A��A�ȴA�n�A�C�A�+A���A���A�n�A�Q�A�?}A��TA��wA��FA��RA��A��!A��uA�l�A�I�A�/A��A�
=A���A��A�ȴA���A�XA��A���A���A��RA��!A���A���A�p�A�7LA���A��uA�bA��#A��A�=qA�A��mA��-A��A�l�A�XA�S�A�K�A�G�A�A�A�1'A�$�A��A�bA�  A��yA���A�A�ĜA��9A���A���A�r�A�ZA�K�A�?}A�?}A�=qA�-A��A�  A��yA���A���A�z�A�ffA�G�A��A���A��A���A�+A���A�ȴA���A�dZA�9XA��A���A��TA���A���A��DA�S�A�VA��A��\A�\)A��A�ĜA��\A�(�A���A�hsA�VA�1'A��
A�XA��9A��A�jA�`BA�VA�M�A�G�A�=qA�1'A��A��`A���A���A��A�l�A�\)A�bA�dZA�Q�A�=qA�/A��A�1A���A��jA�p�A���A�A�jA��A��A���A�I�A��A��yA��`A��#A���A���A�C�A�33A���A�XA��A���A���A���A��A�|�A�n�A�dZA�^5A�Q�A�M�A�M�A� �A�JA��A�^Al�AA~~�A}�A}�;A}�wA}��A}�hA}�7A}x�A}dZA}XA}K�A}?}A}&�A}�A|��A|�A|�/A|��A|z�A|bA{�wA{��A{�A{`BAzĜAydZAx�AwO�Au�#AuVAt{As��As�hG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�
B
�>B
�>B
�>B
�
B
�>B
�
B
�mB
�B
�
B
�B
��B
��B
��B
�
B
�
B
�
B
��B
��B
��B
��B
�B
��B
��B
��B
�VB
��BBB�B�B�BeB!�B$B)�B,qB-�B/OB1'B2�B4�B4B3hB49B4�B4�B4�B5B4B.}B3�BEBOvBYKBv�B�*B�JBB1�B5tBGzBR�BV�BS�BNBI�BIBD�BH�BJ�BJ�BK�BS&B;�B-CB�B�B�BhB�rB�B��B��B��B�MB��B��B�*B��B|�B_�BVBD�B�B
��B
��B
��B
�;B
\�B
9�B
�B
�B	��B	��B	҉B	��B	�:B	�B	��B	��B	rB	n�B	h>B	[�B	TaB	XyB	Q�B	N�B	D�B	2�B	)�B	%zB	~B	�B�B�B��B��BںB��B�BیBٴB�B�
B�B��B��B��B�B�pB��B�EB�,BбB͟B�jB��B�B�]B��B�TB�B��B�B�ZB��B�2B�
B�oB� B�iB�PB��B�.B�(B	�B	�B	!B	#�B	'�B	:*B	A B	@�B	D3B	D�B	L�B	Z�B	^5B	^�B	a|B	h�B	x�B	}�B	�B	�;B	�oB	tB	t�B	u�B	|�B	��B	��B	��B	��B	�fB	��B	�7B	��B	�JB	�PB	�VB	�"B	��B	�"B	��B	�VB	� B	��B	�1B	�B	��B	�7B	�!B	�4B	��B	��B	��B	��B	��B	��B	�CB	�1B	�kB	�B	��B	��B	�XB	��B	��B	��B	�B	�'B	�-B	�[B	�3B	�B	�9B	�nB	��B	��B	�RB	�XB	�$B	��B	�B	��B	�}B	��B	B	ÖB	�-B	��B	�B	�B	ǮB	ǮB	�B	ȀB	ǮB	ǮB	�RB	�dB	��B	ϫB	ΥB	�vB	�BB	ϫB	�vB	�vB	ϫB	ϫB	бB	уB	уB	ѷB	��B	҉B	ҽB	҉B	� B	� B	�&B	҉B	҉B	��B	�2B	ӏB	�[B	�&B	ҽB	ӏB	ԕB	��B	ԕB	��B	ԕB	��B	��B	�9B	�B	��B	�mB	��B	֡B	�mB	�9B	֡B	چB	�B	ںB	�#B	�#B	�)B	��B	ݘB	�]B	ݘB	�;B	�vB	�pB	�B	�vB	��B	�HB	��B	� B	��B	�mB	�sB	�B	�yB	�/B	��B	��B	��B	�cB	�B	�/B	�B	�cB	�cB	� B	��B	�oB	�iB	�B	�B	�B	��B	�MB	�B	�B	�TB	�TB	��B	��B	�`B	��B	��B	�fB	�lB	��B	�lB	�lB	�	B	�lB	��B	�xB	��B	��B	��B	�JB	�xB	�B	��B	��B	��B	�.B
 4B
 4B
 iB	�cB	��B
 4B
oB
B
 �B
�B
;B
B
�B
MB
GB
B
�B
B
B
B
�B
�B
�B
%B
YB
�B
YB
�B
�B
�B
�B

=B

�B
B
B
B
B
�B
VB
�B
�B
�B
�B
�B
�B
.B
�B
hB
4B
hB
�B
�B
B
B
B
�B
�B
�B
�B
�B
�B
+B
�B
�B
�B
_B
1B
eB
�B
�B
B
7B
kB
kB
kB
�B
=B
qB
�B
IB
�B
�B
VB
�B
�B
�B
 'B
 �B
 �B
 'B
 \B
 \B
 �B
 �B
 �B
"hB
"�B
#B
$B
$�B
$�B
$�B
$B
#�B
&B
&�B
&�B
'�B
($B
(�B
(XB
(�B
(�B
)*B
(�B
*0B
*eB
*eB
*�B
*�B
*�B
+�B
+kB
+kB
,�B
,=B
,qB
.�B
.�B
.�B
/OB
/�B
/B
/�B
/B
0!B
/�B
0�B
0�B
1[B
1'B
0�B
2-B
2�B
1�B
1�B
1�B
1�B
1�B
2�B
2�B
3�B
3�B
3�B
3�B
33B
4�B
3�B
4B
3�B
4B
5tB
5�B
5tB
6zB
6zB
6�B
6�B
6�B
8RB
7�B
8�B
7�B
7�B
7�B
7�B
8RB
9XB
8�B
9$B
9XB
9�B
9XB
9$B
9XB
9XB
8�B
:^B
:�B
;0B
:�B
<6B
<jB
<�B
<B
;�B
<6B
=<B
<�B
<�B
>�B
=�B
>�B
>�B
?B
?�B
?HB
?�B
@�B
@OB
@OB
@B
@�B
AUB
AUB
@�B
A�B
AUB
B[B
B'B
B�B
B'B
B�B
B[B
B[B
C�B
D3B
CaB
B�B
C-B
C�B
D3B
D�B
D�B
EmB
E�B
E9B
E�B
E�B
F?B
F�B
F�B
GEB
F�B
GEB
HB
H�B
IB
IB
I�B
I�B
I�B
J#B
I�B
I�B
I�B
J�B
J�B
J�B
LdB
K�B
L�B
MB
L0B
LdB
M�B
M�B
MB
N<B
NpB
M�B
N�B
N�B
N<B
O�B
O�B
OBB
O�B
OvB
O�B
P}B
P}B
P�B
Q�B
Q�B
Q�B
QB
Q�B
R�B
R�B
RTB
RTB
R�B
S&B
T�B
T�B
U�B
UgB
T�B
T�B
UgB
VB
VB
UgB
V�B
V9B
W�B
W?B
V�B
V�B
V�B
W�B
Y�B
Y�B
YB
Y�B
YKB
YKB
Z�B
Z�B
[#B
Z�B
ZQB
Y�B
Z�B
[WB
[�B
\]B
\]B
\]B
\]B
]�B
]�B
]�B
]/B
]/B
]dB
]�B
^5B
_;B
_;B
_pB
_�B
_�B
_�B
_;B
`B
`�B
a|B
a|B
aB
bNB
b�B
b�B
b�B
b�B
c�B
cTB
c�B
c�B
c�B
c�B
c�B
d&B
dZB
e,B
e,B
e`B
e,B
e,B
d�B
d�B
d�B
d�B
d�B
e`B
e�B
ffB
g8B
g8B
f�B
f�B
g�B
g�B
hsB
iB
h�B
iyB
iB
i�B
jB
i�B
i�B
jB
j�B
jKB
kB
j�B
jB
kB
k�B
kQB
k�B
lWB
l�B
l"B
k�B
k�B
k�B
l�B
l�B
l"B
lWB
l�B
l�B
l�B
l�B
m)B
m]B
m)B
ncB
m�B
ncB
n�B
ncB
o5B
o5B
o B
o�B
oiB
o�B
pB
poB
p�B
qAB
p�B
p�B
rB
rB
r|B
rB
rGB
rB
rB
q�B
rB
q�B
rGB
r|B
sB
r�B
sMB
sB
tTB
s�B
tB
t�B
t�B
t�B
t�B
t�B
tB
tB
t�B
u�B
u%B
u%B
u�B
v�B
u�B
v`B
v�B
v�B
v`B
u�B
v`B
w2B
v�B
v�B
w�B
wfB
w�B
x8B
w�B
x8B
x�B
xB
xlB
y>B
x�B
x�B
y�B
yrB
y�B
zDB
z�B
z�B
z�B
zxB
zxB
{B
{B
{JB
{JB
|B
{�B
{�B
|PB
|PB
{�B
|B
|�B
|�B
{�B
|�B
}"B
}"B
}�B
~(B
~(B
~(B
}�B
~(B
~�B
~�B
~]B
~]B
~�B
.B
~�B
.B
�B
�B
�B
�B
�iB
��B
�;B
�;B
�;B
��B
��B
��B
��B
�B
��B
�B
�B
�GB
�{B
�GB
�GB
��B
�MB
�MB
�B
��B
�MB
��B
�SB
��B
��B
��B
��B
�%B
��B
��B
��B
��B
�_B
��B
��B
�1B
��B
�1B
�1B
��B
�fB
��B
��B
�B
�B
��B
��B
�7B
��B
�rB
��B
�B
�B
��B
�B
�xB
�B
��B
�xB
�B
�xB
��B
��B
�JB
�JB
�JB
�B
��B
�~B
�B
�B
�~B
�~B
��B
�B
��B
��B
�"B
��B
��B
��B
�VB
�VB
��B
��B
�(B
�\B
��B
��B
�B
�8B
�B
�B
�fB
�B
��B
�B
�2B
�8B
�
B
�>B
��B
�DB
�B
��B
�B
��B
�yB
�B
��B
�B
�yB
�yB
�B
��B
�yB
�DB
�sB
�B
�sB
�B
�sB
��B
�B
�fB
�
B
�B
�sB
�B
��B
�DB
�sB
�B
�DB
�DB
�B
�sB
�B
�DB
�>B
�DB
�>B
�
B
�>B
�>B
�>B
�
B
�
B
��B
��B
�8B
�8B
�B
�B
�8B
��B
�
B
�B
��B
��B
��B
�sB
��B
�8B
�
B
�
B
�8B
��B
�B
�B
��B
�B
�>B
��B
�8B
�
B
�>B
��B
��B
�B
�B
�B
�>B
�
B
�B
�mB
�mB
�B
�mB
�B
�8B
��B
��B
�
B
��B
��B
�8B
�>B
��B
�
B
�>B
�>B
�
B
�
B
�B
�B
�B
��B
�sB
��B
�B
�B
�B
�B
�>B
�sB
��B
�
B
�yB
�yB
�>B
�sB
��B
�B
�QB
��B
��B
�QB
�QB
�B
��B
�B
�+B
��B
�	B
�>B
��B
�DB
�DB
��B
��B
��B
��B
�PB
�B
�B
��B
��B
��B
�PB
�PB
�B
�PB
��B
��B
�"B
�VB
��B
��B
��B
��B
�.B 4BoB�B�B�BGB{BBB{B�B�BBB�B�B�B%B�B�B�B�B�B�B�B_B1B1B�B�B	7B
�BJBVB�BhB@B�B	B	BB�BIB \B!bB!�B �B!�B�B!�B!�B#B!�B!-B#�B$tB%zB'RB(XB(�B*�B+�B+�B+B+kB+B+B,B,�B-CB-�B-�B-wB-wB,�B,�B,�B-B-wB-�B-�B.�B/B0!B1'B0�B1[B1[B0�B2-B2�B2aB2�B2-B2aB2�B1�B2-B1�B33B4B5?B5?B6B5�B5�B4�B4nB4B2�B3�B2�B3�B2-B2-B2�B2�B2�B3�B4�B5tB5B4�B49B4nB4�B3�B4B3�B3�B3hB49B4B4�B5�B5�B5�B5�B5?B4�B4nB49B4B49B4�B4�B5?B5tB5�B5�B6B5�B4�B4�B49B4�B5B5?B5B4B3hB2aB1'B/�B/B/�B,=B+�B,qB,qB)*B&�B&LB,�BC�BGB@OBF?BE�BM�BA�B?HB>�BK)BD3BMjBT,BO�BY�BS�BI�B^5BXyBH�BNBNpBjBe�Bn�BoiBlWBh>Bo�ByrB��B��B��B�bB��BچB��B�^B�&B�vB iB�B 4BB+BB�B	lB�B�BkB2�B0�B:�B1'B4nB-B3hB5?B4�B8�B>wB?HBA�BG�BI�BN�BP}BO�BR BS�BR�BR�BS&BU�BT�B[WBYKBT�BT�BZ�BN�BOvBPHBZ�BW?BO�BM�BQBL0BK�BJ�BK)BGEBH�BD�BDgBT�BQ�BM�BE�BC�BD�BB[BC�B@�BB�BHKBE9BGEBG�BJ#BH�BJXBG�BI�BK)BK)BJ�BI�BJXBJ�BK^BI�BJ�BK�BJ�BF�BJ�BHBC�BE9BP�B�XB\)BC�B:*B>wB<jB=B=�B8B<6B5�BA B.}B0�B-wB-CB,B6zB&�B(�B�BqB�B�B=B�B	BCB�B�B�BeB�B�B�B1B_BYB�B�BeBeBB�BSB$BYB�BMB�BBFB�BB{BuB�B�BFB@BoB�B�B�BoB B�B:B�B4B.BbB�B:B:B�BB1B�B�B�B�B�B�B�B B4B�B�B�B�B�B�B�BB+BSB	lB�BB �BB�B�B�B�BޞB�)BݘB��BܒB�B�BںB�QB�B�yB�B�B�
B�B��B�EB�KBҽB��B҉BϫB�9B�B�6BʌB��B�B�^B͟B�<B��B�B�B�B��B��B�RB�B�*BÖB�B��B�3B��B�B�UB�=B�B�0B��B�tB��B�nB��B��B��B�=B��B�~B��B�B�B�B�{B�FB�B��B�B�FB�B��B��B�+B��B�B�!B��B��B�RB�B��B��B��B�wB��BÖB�?B�BʌB��BӏB��B�aB�^B��B�jB��B�^B��B�B�B�B��B��B��B�FB��B��B�B��B�+B��B��B�B�$B�oB�oB�7B�%B�DB��B�AB��B��B��B�_B��B~(B�+Bx�Bw�Bv�Bi�Bh�Bl�Bd�BaB_B\�B]�B]�B]/B\�B\�BZ�BZ�BZ�BZ�BXyBU�BTaBW?BS�BS�BT,BP}BMjBL0BJ�BIRBJ�BI�BH�BEmBC�BA�B>B:�B5?B7�B6�B49B/OB~BB�B B�B�B�B
�cB
��B
�xB
��B
�8B
�vB
�B
�B
�B
�BB
�&B
�B
�,B
��B
�B
�?B
��B
��B
�tB
ǮB
�B
��B
�B
��B
��B
�CB
�	B
�7B
��B
�CB
��B
��B
��B
�PB
��B
��B
�B
�	B
}�B
}VB
{JB
zxB
x�B
xB
~�B
x�B
p;B
o5B
g8B
d�B
_�B
\�B
VB
R�B
JXB
FB
F�B
C�B
D�B
P}B
\]B
9�B
33B
*eB
,�B
#B
�B
 \B
B
�B
�B
�B
�B
_B
�B
B
�B
�B
$B
�B
�B
FB
.B
�B
�B
�B
B
{B
B
GB
oB
;B	��B
 iB	��B	�]B	��B	�PB	�B	�JB	�]B	��B	�|B	�iB	��B	�`B	��B	��B	��B	�fB	�vB	�[B	�,B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                B
�B
�JB
�JB
�JB
�B
�JB
�B
�yB
߭B
�B
߭B
��B
��B
��B
�B
�B
�B
��B
��B
��B
��B
�B
��B
�B
��B
�bB
��B
�B
�%B
��B 	B�BqB�BB!�B$}B%�B'[B)3B*�B,�B,B+tB,EB,�B,�B,�B-B,B&�B+�B=BG�BQWBn�B�6B�VBB)�B-�B?�BJ�BN�BK�BFBA�BA)B<�B@�BB�BB�BC�BK2B3�B%OB�B�B
�B	tB�~B�#B��B��B�B�YB��B�B�6B��Bt�BW�BNB<�B�B
��B
��B
��B
yGB
UB
1�B
�B
�B	��B	��B	ʕB	�B	�FB	�B	��B	|�B	jB	f�B	`JB	S�B	LmB	P�B	I�B	F�B	<�B	*�B	"B	�B	�B	 �B�B��B��B��B��B�B�)BӘB��B�B�BضB�
B�
B��BܛB�|B��B�QB�8BȽBūB�vB��B�B�iB��B�`B۔B��B��B�fB�B�>B�B�{B�B�uB�\B��B�:B�4B��B	�B	-B	�B	�B	26B	9,B	8�B	<?B	<�B	D�B	R�B	VAB	V�B	Y�B	`�B	p�B	u�B	w�B	yGB	y{B	l+B	l�B	m�B	t�B	B	�B	��B	��B	�rB	��B	�CB	��B	�VB	�\B	�bB	�.B	��B	�.B	��B	�bB	�B	��B	�=B	�B	��B	�CB	�-B	�@B	��B	��B	��B	��B	��B	��B	�OB	�=B	�wB	�B	��B	��B	�dB	��B	��B	��B	�'B	�3B	�9B	�gB	�?B	�B	�EB	�zB	��B	��B	�^B	�dB	�0B	��B	�B	��B	��B	��B	��B	��B	�9B	��B	�)B	�B	��B	��B	�#B	��B	��B	��B	�^B	�pB	��B	ǷB	ƱB	ǂB	�NB	ǷB	ǂB	ǂB	ǷB	ǷB	ȽB	ɏB	ɏB	��B	��B	ʕB	��B	ʕB	�,B	�,B	�2B	ʕB	ʕB	��B	�>B	˛B	�gB	�2B	��B	˛B	̡B	��B	̡B	��B	̡B	��B	��B	�EB	�B	��B	�yB	��B	έB	�yB	�EB	έB	ҒB	�)B	��B	�/B	�/B	�5B	��B	դB	�iB	դB	�GB	؂B	�|B	�B	؂B	��B	�TB	��B	�,B	�
B	�yB	�B	�B	�B	�;B	� B	� B	��B	�oB	�B	�;B	�B	�oB	�oB	�B	��B	�{B	�uB	�B	�B	�B	��B	�YB	�B	�B	�`B	�`B	�B	��B	�lB	�B	�B	�rB	�xB	�B	�xB	�xB	�B	�xB	�B	�B	�B	��B	��B	�VB	�B	�(B	��B	�B	��B	�:B	�@B	�@B	�uB	�oB	��B	�@B	�{B	�B	��B	��B	�GB	�B	��B	�YB	�SB	�%B	��B	�%B	�+B	�+B	��B	��B	��B	�1B	�eB	��B	�eB	��B
 �B
 �B
 �B
IB
�B
!B
!B
'B
'B
�B
bB
�B
�B
�B
�B
B
�B
:B
	�B
	tB
	@B
	tB
	�B
	�B
B
B
B
�B
�B
�B
�B
B
�B
7B
�B
�B
�B
kB
=B
qB
�B
�B
B
CB
wB
wB
wB
�B
IB
}B
�B
UB
�B
�B
bB
�B
�B
�B
3B
�B
�B
3B
hB
hB
�B
B
�B
tB
�B
B
B
�B
�B
�B
B
�B
$B
�B
�B
�B
 0B
 �B
 dB
 �B
!B
!6B
!B
"<B
"qB
"qB
"�B
"�B
"�B
#�B
#wB
#wB
$�B
$IB
$}B
&�B
&�B
&�B
'[B
'�B
''B
'�B
''B
(-B
'�B
(�B
(�B
)gB
)3B
(�B
*9B
*�B
)�B
)�B
)�B
)�B
*B
*�B
*�B
+�B
+�B
+�B
+�B
+?B
,�B
+�B
,B
+�B
,B
-�B
-�B
-�B
.�B
.�B
.�B
.�B
.�B
0^B
/�B
0�B
/�B
/�B
/�B
/�B
0^B
1dB
0�B
10B
1dB
1�B
1dB
10B
1dB
1dB
0�B
2jB
2�B
3<B
2�B
4BB
4vB
4�B
4B
3�B
4BB
5HB
4�B
4�B
6�B
5�B
6�B
6�B
7 B
7�B
7TB
7�B
8�B
8[B
8[B
8&B
8�B
9aB
9aB
8�B
9�B
9aB
:gB
:3B
:�B
:3B
:�B
:gB
:gB
;�B
<?B
;mB
;B
;9B
;�B
<?B
<�B
<�B
=yB
=�B
=EB
=�B
=�B
>KB
>�B
>�B
?QB
>�B
?QB
@#B
@�B
A)B
A)B
A�B
A�B
A�B
B/B
A�B
A�B
A�B
B�B
B�B
CB
DpB
DB
D�B
EB
D<B
DpB
E�B
E�B
EB
FHB
F|B
E�B
F�B
F�B
FHB
G�B
G�B
GNB
G�B
G�B
G�B
H�B
H�B
H�B
I�B
I�B
I�B
I&B
I�B
J�B
J�B
J`B
J`B
J�B
K2B
L�B
M
B
M�B
MsB
M
B
L�B
MsB
NB
NB
MsB
N�B
NEB
O�B
OKB
N�B
N�B
N�B
O�B
Q�B
Q�B
Q�B
Q�B
QWB
QWB
R�B
R�B
S/B
R�B
R]B
Q�B
R�B
ScB
TB
TiB
TiB
TiB
TiB
U�B
U�B
U�B
U;B
U;B
UpB
U�B
VAB
WGB
WGB
W|B
W�B
W�B
W�B
WGB
XB
X�B
Y�B
Y�B
YB
ZZB
Z�B
Z�B
Z�B
Z�B
[�B
[`B
[�B
[�B
[�B
[�B
[�B
\2B
\fB
]8B
]8B
]lB
]8B
]8B
\�B
\�B
]B
]B
\�B
]lB
^
B
^rB
_DB
_DB
^�B
^�B
_�B
_�B
`B
aB
`�B
a�B
aB
a�B
b"B
a�B
a�B
b�B
b�B
bWB
c(B
b�B
b�B
c(B
c�B
c]B
c�B
dcB
d�B
d.B
c�B
c�B
c�B
d�B
d�B
d.B
dcB
d�B
e B
d�B
d�B
e5B
eiB
e5B
foB
fB
foB
f�B
foB
gAB
gAB
gB
g�B
guB
g�B
hB
h{B
h�B
iMB
h�B
h�B
jB
jB
j�B
jB
jSB
jB
jB
i�B
jB
i�B
jSB
j�B
k%B
j�B
kYB
k%B
l`B
k�B
l+B
l�B
l�B
l�B
l�B
l�B
l+B
l+B
l�B
m�B
m1B
m1B
nB
n�B
m�B
nlB
n�B
n�B
nlB
nB
nlB
o>B
o	B
o	B
o�B
orB
o�B
pDB
o�B
pDB
p�B
pB
pxB
qJB
p�B
p�B
q�B
q~B
q�B
rPB
r�B
r�B
r�B
r�B
r�B
s�B
s�B
sVB
sVB
t(B
s�B
s�B
t\B
t\B
s�B
t(B
t�B
t�B
s�B
t�B
u.B
u.B
u�B
v4B
v4B
v4B
v B
v4B
v�B
v�B
viB
viB
v�B
w:B
wB
w:B
w�B
w�B
w�B
w�B
xuB
x�B
yGB
yGB
yGB
y�B
y�B
y�B
y�B
zB
z�B
{B
{B
{SB
{�B
{SB
{SB
{�B
|YB
|YB
|%B
{�B
|YB
|�B
}_B
}�B
}�B
}�B
}�B
~1B
~�B
B
B
B
kB
�B
�B
�=B
��B
�=B
�=B
�	B
�rB
��B
��B
�B
�B
��B
��B
�CB
��B
�~B
��B
�B
�B
��B
�B
��B
�B
��B
��B
�!B
��B
��B
��B
�VB
�VB
�VB
�'B
��B
��B
�!B
�!B
��B
��B
��B
�'B
��B
��B
�.B
��B
��B
��B
�bB
�bB
��B
��B
�4B
�hB
��B
��B
�B
�DB
�B
߭B
�rB
�B
��B
߭B
�>B
�DB
�B
�JB
��B
�PB
ݡB
��B
�(B
��B
�B
�B
��B
�B
�B
�B
�B
��B
�B
�PB
�B
�B
�B
�B
�B
��B
�B
�rB
�B
�B
�B
�B
��B
�PB
�B
�B
�PB
�PB
�B
�B
�B
�PB
�JB
�PB
�JB
�B
�JB
�JB
�JB
�B
�B
��B
��B
�DB
�DB
�B
�B
�DB
��B
�B
�B
��B
��B
��B
�B
��B
�DB
�B
�B
�DB
��B
߭B
ާB
��B
�B
�JB
��B
�DB
�B
�JB
��B
��B
ާB
�B
�B
�JB
�B
߭B
�yB
�yB
�B
�yB
߭B
�DB
��B
��B
�B
��B
��B
�DB
�JB
��B
�B
�JB
�JB
�B
�B
�B
�B
�B
��B
�B
��B
�B
�B
�B
�B
�JB
�B
��B
�B
�B
�B
�JB
�B
��B
�B
�]B
��B
��B
�]B
�]B
��B
��B
�B
�7B
��B
�B
�JB
�B
�PB
�PB
��B
��B
��B
��B
�\B
�(B
�(B
��B
��B
��B
�\B
�\B
�(B
�\B
��B
��B
�.B
�bB
� B
��B
��B
�B
�:B
�@B
�{B
��B
��B
��B
�SB
��B
�%B
�%B
��B
��B
��B
�%B
�%B
��B
��B
��B
�1B
��B
��B
��B 	B
��B 	B 	B
�kB =B =B 	B �BCB�BVBbB�B	tBLB�BBBB�BUBhBnB�B�B�B�BB�BB�B9B�B�B�B^B dB �B"�B#�B#�B#B#wB#B#B$B$�B%OB%�B%�B%�B%�B$�B$�B$�B%B%�B%�B%�B&�B''B(-B)3B(�B)gB)gB(�B*9B*�B*mB*�B*9B*mB*�B*B*9B*B+?B,B-KB-KB.B-�B-�B,�B,zB,B+B+�B*�B+�B*9B*9B*�B*�B+B+�B,�B-�B-B,�B,EB,zB,�B+�B,B+�B+�B+tB,EB,B,�B-�B-�B-�B-�B-KB,�B,zB,EB,B,EB,�B,�B-KB-�B-�B-�B.B-�B,�B,�B,EB,�B-B-KB-B,B+tB*mB)3B'�B''B'�B$IB#�B$}B$}B!6B�BXB$�B<
B?B8[B>KB=�BE�B9�B7TB6�BC5B<?BEvBL8BG�BQ�BLBA�BVABP�B@�BFBF|Bb"B^
Bf�BguBdcB`JBg�Bq~B��B��B��B�nB��BҒB��B�jB�2B؂B�uB�B�@B�+B�7B�+B��BxB�B�BwB*�B(�B3B)3B,zB%B+tB-KB,�B0�B6�B7TB9�B?�BA�BF�BH�BG�BJ,BLBJ�BJ�BK2BM�BM
BScBQWBL�BL�BR�BF�BG�BHTBR�BOKBG�BE�BI&BD<BDBB�BC5B?QB@�B<�B<sBL�BI�BE�B=�B<
B<�B:gB;�B8�B:�B@WB=EB?QB?�BB/B@�BBdB?�BA�BC5BC5BB�BA�BBdBCBCjBA�BB�BC�BB�B>�BB�B@#B;�B=EBH�B�dBT5B<
B26B6�B4vB5B5�B0)B4BB-�B9,B&�B(�B%�B%OB$B.�B�B �B�B}B�B�BIB�BBOB�B�BBqB�B�B�B=BkBeB�B�BqBqB*B�B_B0BeB�BYB�B*BRB�B$B�B�B
�B�BRBLB
{B	�B�B�B
{B	B�B
FB
�B	@B:BnB
�B
FB
FB�BB=B�B
�B	�B�B	�B�B�B	B	@B�B�B�B�B�B�B�B!B�7B�_BxB�B�+B�B�+B߭B�B�B�B֪B�5BդB�BԞB�)BыB��B�]BыBЅBыBыB�B�B��B�QB�WB��B��BʕBǷB�EB�&B�BBB��B�&B�jBūB�HB��B�B�B�B�B��B�^B�)B�6B��B�#B��B�?B��B� B�aB�IB�B�<B��B��B��B�zB��B��B��B�IB��B��B��B�$B�*B�*B��B�RB�$B��B�B�RB�$B��B��B�7B��B�B�-B��B��B�^B�B��B�B��B��B��B��B�KB�#BB��B˛B�B�mB�jB��B�vB�B�jB��B�)B�B� B�B��B�B�RB��B��B�B��B�7B�B��B�$B�0B�{B�{B�CB~1B�PBy�BzMBx�Bx�B~�BkB~�Bv4B7Bp�Bo�Bo	Ba�B`�Bd�B]BYBWBT�BU�BU�BU;BUBT�BR�BR�BR�BR�BP�BM�BLmBOKBLBK�BL8BH�BEvBD<BB�BA^BCBA�B@�B=yB<
B9�B6B2�B-KB/�B.�B,EB'[B�B$B�B	B�B
��B
��B
�oB
��B
�B
��B
�DB
�B
�B
�B
ާB
�NB
�2B
ыB
�8B
��B
� B
�KB
��B
��B
��B
��B
�B
��B
�$B
��B
��B
�OB
�B
�CB
��B
�OB
��B
��B
��B
�\B
��B
��B
�B
�B
u�B
ubB
sVB
r�B
p�B
pB
wB
p�B
hGB
gAB
_DB
]B
W�B
T�B
NB
J�B
BdB
>B
>�B
;�B
<�B
H�B
TiB
1�B
+?B
"qB
$�B
B
�B
hB
B
�B
�B
�B
�B
kB
�B
'B
�B
�B
0B
�B

�B
RB
:B	��B	��B	��B	�%B	��B	�B	�SB	�{B	�GB	��B	�uB	��B	�iB	��B	�\B	�B	�VB	�iB	�B	�B	�uB	�B	�lB	�B	��B	��B	�rB	�B	�gB	�8B	��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223222                            20230426223222AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622322220230426223222  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622322220230426223222QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622322220230426223222QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               