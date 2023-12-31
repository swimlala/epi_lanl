CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-21T22:50:10Z creation      
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
resolution        =���   axis      Z           ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [    PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        b�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���        ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ϸ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o        ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 4�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       <�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � [�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o       c�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �d   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �l   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �t   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �|   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �    HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �(   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �H   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �P   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �X   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �`Argo profile    3.1 1.2 19500101000000  20230721225010  20230721225010  5902511 5902511 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL              	  	AA  AOAO6810                            6810                            2B  2B  AA  SOLO_II                         SOLO_II                         8521                            8521                            SBE602 27Jul16                  SBE602 27Jul16                  853 853 @�3���d@�3���d11  @�3�q�p@�3�q�p@2d~($x@2d~($x�d��~��M�d��~��M11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  FF  ?�  ?��H@B�\@�G�@��R@�p�@޸RA   A\)A   A,(�A@  A`  A�Q�A�Q�A�Q�A��A��A�  A߮A�A��B  B  B  B (�B(  B/�
B7�B?�
BH  BO�
BW�
B`  BhQ�Bp  Bx  B�  B��B�  B�{B�  B�  B�  B�  B�(�B�{B�{B�  B�  B��B�{B�{B�  B�  B�{B�{B��B��B��B��B�  B�  B�  B�  B��B��
B��
B��B��C��C  C
=C
=C

=C��C  C  C��C  C��C��C  C  C  C 
=C"
=C$
=C&
=C'��C*  C,  C-��C/��C2  C4
=C5��C7��C:  C;��C>  C@
=CB  CD
=CF
=CH
=CJ
=CK��CM��CP
=CR
=CT  CU��CW��CY�C[�C]��C`
=Cb{Cc��Cf  Ch
=Ci��Cl  Cm�Cp
=Cr
=Ct  Cu��Cx  Cz
=C|{C~  C�  C�  C�  C�  C���C���C���C�  C�  C���C���C�  C�C�  C���C�  C�  C�C�C�  C�C�C�  C�C�  C���C���C�  C�  C���C���C�  C�
=C���C�  C�  C�  C�C���C�  C�  C�  C���C�  C���C���C�C�
=C���C���C���C�C�C�  C���C���C���C�  C�C�  C���C�  C���C�  C�C�C�  C�C�  C���C�  C�C�C�  C���C���C���C�C�C�  C���C��C���C���C���C�  C�
=C�C���C�  C�C�C�  C���C���C���C�  C�  C�  C���C���C���C���C�  C���C���C�C�
=C�C�
=C�  C���C���C���C���C�  C�C�C�
=C�
=C�  C���C�  C�C�  C���C�C�D   D � D �qD}qD�D��D�qD� D�qD� D�D��D�D}qD��Dz�D�RDz�D	  D	��D
  D
� D  D� D�qD}qD  D� DD��D  D��D�D��D  D� DD��DD��D�D}qD�qD� D  D}qD  D��D�D� D  D� D��D� D  D� D�D��D  D� D  D� D�qD� D   D z�D �qD!}qD!��D"}qD"�qD#z�D#��D$}qD%  D%� D&  D&}qD&�qD'}qD(  D(� D)�D)}qD)��D*}qD*�qD+}qD,  D,��D-D-�D.D.� D.�qD/� D/�qD0� D1D1��D2�D2�D3  D3� D3�qD4� D5  D5��D6  D6}qD7  D7}qD7��D8}qD9  D9� D:�D:��D;  D;}qD<  D<��D=�D=� D>  D>��D?�D?�D@�D@� DA  DA��DA�qDB� DC�DC� DD�DD�DEDE}qDE�qDF� DG�DG}qDG�qDH� DH�qDI}qDI�qDJ� DJ�qDKz�DL  DL��DL�qDMz�DM�qDN� DO  DO��DP�DP� DP�qDQ}qDR  DR��DSDS��DS�qDT}qDU  DU� DV  DV��DW  DW��DX  DX}qDY  DY}qDY�qDZ� D[�D[��D[�qD\}qD]  D]��D^�D^� D^�qD_� D_�qD`}qD`�qDa� DbDb� Dc�Dc�Dd  Dd��De  De� Df�Df� Dg  Dg��Dh  Dh� Dh�qDi}qDj  Dj��Dk  Dk}qDk��Dl}qDl��Dm� Dn�Dn��Do  Do��Dp  Dp� Dq  Dq� Dr�Dr� Ds  Ds}qDs�qDt}qDu  Du� Dv  Dv� Dw  Dw�Dx�Dx��DyDy� Dy�qDz� D{�D{}qD{��D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D��HD��HD�HD�@ D�� D�� D���D�@ D�� D�� D���D�>�D�~�D��qD�  D�>�D�~�D��qD��qD�>�D�� D�D�  D�@ D�� D���D�HD�B�D�� D���D���D�AHD���D��HD�HD�AHD�� D��HD�HD�B�D�� D���D�  D�@ D��HD�� D�  D�@ D��HD�D�  D�=qD�~�D��qD��)D�>�D�~�D�� D�HD�AHD��HD��HD�  D�@ D��HD�� D���D�>�D��HD�� D���D�@ D�~�D�� D�  D�@ D��HD�� D��qD�>�D�~�D���D���D�@ D�� D��HD�HD�@ D��HD��HD�  D�@ D��HD�� D��qD�>�D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�HD�>�D�}qD�� D�  D�@ D��HD�D�HD�@ D�� D�� D��D�C�D��HD��HD��D�@ D�� D��qD���D�>�D��HD��HD�  D�AHD�� D�� D��D�@ D�}qD��qD��qD�@ D��HD��qD�  D�AHD��HD�� D���D�@ D�� D��HD�HD�@ D��HD�� D���D�>�D�~�D�� D���D�>�D�~�D�� D��qD�<)D�|)D�� D�HD�=qD�}qD��HD�  D�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D���D�� D�  D�@ D�~�D��HD�  D�>�D��HD�� D���D�AHD�� D�� D�HD�B�D��HD�� D�  D�@ D�� D��HD�  D�=qD�~�D��HD�  D�AHD�� D�� D�  D�@ D�� D���D���D�@ D��HD�� D���D�AHD�� D���D���D�>�D�}qD�� D��D�AHD��HD��HD�  D�>�D�� D�D��D�@ D�� D��HD�HD�@ D�� D��HD�HD�C�D���D��HD���D�>�D�� D��qD���D�>�D�~�D��HD�  D�@ D���D��HD�HD�AHD�D�D�  D�>�DÀ Dþ�D���D�@ DĀ D�� D���D�>�DŁHD��HD�HD�@ D�~�Dƾ�D�  D�AHDǂ�D��HD�HD�@ D�}qDȾ�D�HD�AHDɀ D��HD�HD�>�D�~�Dʾ�D���D�>�D�~�D�� D�HD�@ D�~�D�� D�HD�B�D́HD�� D�  D�@ D�~�Dξ�D���D�@ Dπ D��HD���D�>�D�~�Dо�D���D�>�D�~�D�� D�HD�@ D�~�D�� D�HD�@ DӁHDӾ�D��qD�>�DԀ DԾ�D�HD�AHD�~�D�� D�HD�>�D�}qD�� D�HD�@ DׁHD��HD�HD�AHD؁HDؾ�D�  D�@ DفHD��HD�HD�@ D�~�Dھ�D�  D�B�DہHD۾�D�  D�AHD܁HDܾ�D���D�@ D݀ Dݾ�D�HD�AHDށHD�� D���D�>�D߀ D߾�D��)D�=qD�~�D��HD�  D�@ D� D�� D�HD�@ D�~�D�qD��qD�>�D�~�D�� D�  D�AHD��D���D�HD�@ D� D�� D���D�@ D悏D��HD�  D�>�D�|)D�)D��qD�>�D�~�D�� D�  D�>�D�HD��HD�  D�@ D� D��HD�  D�=qD�}qD�qD�  D�AHD� D��HD�  D�>�D� D��HD��D�B�D� D�� D�HD�AHD�~�DﾸD���D�=qD�~�D�� D��D�B�D� D�D�HD�>�D�}qD��HD�  D�>�D�|)D�qD���D�>�D�HD���D���D�@ D��HD�� D�  D�B�D���D�D�HD�@ D�p�>�G�?#�
?W
=?��
?\?�@�@!G�@8Q�@O\)@\(�@s33@��@���@�Q�@�  @��@�
=@��H@Ǯ@��@ٙ�@��@���@���AG�AA(�A��A�A�HA\)A$z�A*=qA.{A4z�A:�HA>�RAC33AJ=qAMp�ATz�AX��A^{Ae�Ah��Ao\)Atz�Ax��A�  A��\A��A�\)A��\A���A�\)A��HA���A��A��\A�z�A�  A�=qA�(�A�\)A��A�(�A�\)A���A��
A�
=A���A�(�A�ffA���A�(�A�Aȣ�A��
A�p�A���A��HA�Aأ�A��HA�ffA�  A�33A�{A�Q�A�A�A�Q�A�A�p�A�Q�A��A��B Q�B��B�\B(�Bp�BffB(�B	G�B
ffB  BG�B=qB�
BG�B{B�B�B�B33B��B�B33B��B�B�HB ��B!B#
=B$z�B%B&�RB(z�B)B*�RB,z�B-B.�RB0(�B1B2�\B4z�B5G�B6�\B8(�B9�B:ffB<  B<��B>�\B@  B@��BB�RBC�BE�BF�RBG�BH��BJ�\BK�BL��BNffBO\)BP��BR=qBS33BT��BU�BW
=BX��BYp�B[
=B\(�B]�B^�RB_�B`��BbffBc33Bd��Bf{Bf�HBh(�Bi��Bj�\Bk�
Bmp�Bn=qBo�
BqG�Br=qBs\)Bt��Bv{Bw33Bx��By�B{33B|��B}��B~�HB�=qB��RB�G�B�{B��RB�33B��B��\B��B��B�z�B��B��B���B��B��B���B�33B��B���B��B��B���B��B��
B���B��B��
B��\B�
=B��
B���B��B��
B���B��B�B���B�33B�B��\B�G�B��
B�ffB�\)B��
B�z�B�G�B��
B�ffB�33B��B�(�B��HB��B��B��\B�
=B�p�B�(�B��RB���B��B�(�B�ffB��HB�p�B�B�  B���B�
=B�33B�B�{B�Q�B���B�G�B��B�B�Q�B��RB��HB�p�B��
B�{B��\B���B�G�B���B�{B��\B��RB��B���B��
B�Q�B���B�
=B�G�B��B�Q�B��\B���B�G�B��
B�{B�Q�B���B�G�B��B�B�Q�B���B���B�G�B�B�  B�=qB��RB�
=B�G�B���B�{B�z�B��\B�
=B�p�B��B�{B�z�B��RB�
=B��B��
B�  B�ffB��HB��B�\)B��
B�Q�Bģ�B��HB�33BŮB��
B�(�Bƣ�B�
=B�G�BǅB�  B�ffBȣ�B���B�p�B�B��B�Q�B���B�
=B�G�BˮB�(�B�Q�Ḅ�B�
=B�p�BͅB��B�Q�BΣ�B���B�33Bϙ�B��
B�  B�Q�B���B���B�33Bљ�B�{B�=qB�z�B���B�33B�\)B�B�=qB�z�Bԣ�B�
=B�p�Bՙ�B��
B�=qB֣�B���B�G�Bי�B׮B�  B�z�B���B���B�33BمB�  B�Q�B�z�B��HB�G�BۅB�B�=qB܏\BܸRB��B݅B�B�  B�z�B��HB�
=B�\)B�B�(�B�Q�B��B�33B�p�BᙚB�  B�z�B���B���B�G�B�B�  B�=qB��B�
=B�\)B�B�  B�ffB�\B��HB�G�B�B�{B�(�B�z�B�
=B�G�B�B��
B�Q�B��B��HB��B�p�B�  B�{B�ffB��HB�33B�\)B�B�(�B�z�B�RB�
=B�B��
B�  B�z�B��HB��B�p�B��B�(�B�ffB���B�G�B�B�B�=qB��RB���B�G�B�B�=qB��\B���B�\)B���B��B�ffB��HB�33B�p�B�  B�=qB��\B�33B��B�B�{B��\B���B�G�B���B�(�B�ffB���B�G�B��B��C =qC p�C ��C ��C{C33Cp�C�C��C
=CQ�C�C��C�C33C\)C�C��C  C(�Cz�C�RC�
C(�CffC�C��C
=C33CffC�RC�
C
=C\)Cz�C�RC  C�C\)C��CC	  C	G�C	p�C	��C	�C
�C
=qC
�C
��C
�C(�Cp�C��C��C�CQ�Cz�C�
C
=C33C�C�RC�HC(�Cp�C��C��C{C\)C�C�C
=C33C\)C�C��C{C=qC��C��C�C33Cz�C��C��C{CffC�\C�RC  C=qCz�C��C�C33Cp�C�\C�
C�CQ�C�C�RC
=C=qCffC�C��C�CG�C��C�HC  C=qCz�CC��C�Cp�C�RC�HC{CG�C�C�
C{C33Cz�CC�C�CffC�C��C
=CG�C��CC�C33Cz�C�C�
C {C \)C ��C ��C �C!33C!z�C!��C!�
C"
=C"\)C"��C"C"�C#(�C#p�C#�C#�HC$
=C$Q�C$��C$C$�C%33C%z�C%�C%�
C&{C&Q�C&��C&��C&��C'(�C'p�C'�RC'�HC({C(=qC(�C(��C)
=C)=qC)ffC)��C)�
C*(�C*ffC*��C*��C*��C+=qC+z�C+C,  C,�C,\)C,��C,�HC-(�C-G�C-z�C-�RC-��C.G�C.�C.�RC.�HC/�C/ffC/��C/�HC0{C0=qC0z�C0�RC1
=C133C1\)C1��C1�C2(�C2\)C2�C2�RC2��C3G�C3�C3�RC3�HC4�C4p�C4�C4�
C5
=C5\)C5��C5��C5��C633C6p�C6C7  C733C7ffC7��C7�
C8�C8p�C8��C8�
C9
=C9G�C9��C9�HC:{C:=qC:�\C:�
C;  C;=qC;�\C;�
C<  C<=qC<z�C<��C=
=C=33C=z�C=C>
=C>G�C>�C>�RC>��C?G�C?�C?C?�C@33C@�C@C@��CA(�CAffCA�RCB  CB=qCBffCB��CB�CC33CCz�CC��CC�
CD33CDz�CD�CD�HCE�CEffCE�CE��CF(�CFffCF��CF�CG=qCGffCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                     ?�  ?��H@B�\@�G�@��R@�p�@޸RA   A\)A   A,(�A@  A`  A�Q�A�Q�A�Q�A��A��A�  A߮A�A��B  B  B  B (�B(  B/�
B7�B?�
BH  BO�
BW�
B`  BhQ�Bp  Bx  B�  B��B�  B�{B�  B�  B�  B�  B�(�B�{B�{B�  B�  B��B�{B�{B�  B�  B�{B�{B��B��B��B��B�  B�  B�  B�  B��B��
B��
B��B��C��C  C
=C
=C

=C��C  C  C��C  C��C��C  C  C  C 
=C"
=C$
=C&
=C'��C*  C,  C-��C/��C2  C4
=C5��C7��C:  C;��C>  C@
=CB  CD
=CF
=CH
=CJ
=CK��CM��CP
=CR
=CT  CU��CW��CY�C[�C]��C`
=Cb{Cc��Cf  Ch
=Ci��Cl  Cm�Cp
=Cr
=Ct  Cu��Cx  Cz
=C|{C~  C�  C�  C�  C�  C���C���C���C�  C�  C���C���C�  C�C�  C���C�  C�  C�C�C�  C�C�C�  C�C�  C���C���C�  C�  C���C���C�  C�
=C���C�  C�  C�  C�C���C�  C�  C�  C���C�  C���C���C�C�
=C���C���C���C�C�C�  C���C���C���C�  C�C�  C���C�  C���C�  C�C�C�  C�C�  C���C�  C�C�C�  C���C���C���C�C�C�  C���C��C���C���C���C�  C�
=C�C���C�  C�C�C�  C���C���C���C�  C�  C�  C���C���C���C���C�  C���C���C�C�
=C�C�
=C�  C���C���C���C���C�  C�C�C�
=C�
=C�  C���C�  C�C�  C���C�C�D   D � D �qD}qD�D��D�qD� D�qD� D�D��D�D}qD��Dz�D�RDz�D	  D	��D
  D
� D  D� D�qD}qD  D� DD��D  D��D�D��D  D� DD��DD��D�D}qD�qD� D  D}qD  D��D�D� D  D� D��D� D  D� D�D��D  D� D  D� D�qD� D   D z�D �qD!}qD!��D"}qD"�qD#z�D#��D$}qD%  D%� D&  D&}qD&�qD'}qD(  D(� D)�D)}qD)��D*}qD*�qD+}qD,  D,��D-D-�D.D.� D.�qD/� D/�qD0� D1D1��D2�D2�D3  D3� D3�qD4� D5  D5��D6  D6}qD7  D7}qD7��D8}qD9  D9� D:�D:��D;  D;}qD<  D<��D=�D=� D>  D>��D?�D?�D@�D@� DA  DA��DA�qDB� DC�DC� DD�DD�DEDE}qDE�qDF� DG�DG}qDG�qDH� DH�qDI}qDI�qDJ� DJ�qDKz�DL  DL��DL�qDMz�DM�qDN� DO  DO��DP�DP� DP�qDQ}qDR  DR��DSDS��DS�qDT}qDU  DU� DV  DV��DW  DW��DX  DX}qDY  DY}qDY�qDZ� D[�D[��D[�qD\}qD]  D]��D^�D^� D^�qD_� D_�qD`}qD`�qDa� DbDb� Dc�Dc�Dd  Dd��De  De� Df�Df� Dg  Dg��Dh  Dh� Dh�qDi}qDj  Dj��Dk  Dk}qDk��Dl}qDl��Dm� Dn�Dn��Do  Do��Dp  Dp� Dq  Dq� Dr�Dr� Ds  Ds}qDs�qDt}qDu  Du� Dv  Dv� Dw  Dw�Dx�Dx��DyDy� Dy�qDz� D{�D{}qD{��D|� D}  D}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�@ D��HD��HD�HD�@ D�� D�� D���D�@ D�� D�� D���D�>�D�~�D��qD�  D�>�D�~�D��qD��qD�>�D�� D�D�  D�@ D�� D���D�HD�B�D�� D���D���D�AHD���D��HD�HD�AHD�� D��HD�HD�B�D�� D���D�  D�@ D��HD�� D�  D�@ D��HD�D�  D�=qD�~�D��qD��)D�>�D�~�D�� D�HD�AHD��HD��HD�  D�@ D��HD�� D���D�>�D��HD�� D���D�@ D�~�D�� D�  D�@ D��HD�� D��qD�>�D�~�D���D���D�@ D�� D��HD�HD�@ D��HD��HD�  D�@ D��HD�� D��qD�>�D�� D�� D�  D�@ D�� D�� D���D�@ D�� D�� D�HD�>�D�}qD�� D�  D�@ D��HD�D�HD�@ D�� D�� D��D�C�D��HD��HD��D�@ D�� D��qD���D�>�D��HD��HD�  D�AHD�� D�� D��D�@ D�}qD��qD��qD�@ D��HD��qD�  D�AHD��HD�� D���D�@ D�� D��HD�HD�@ D��HD�� D���D�>�D�~�D�� D���D�>�D�~�D�� D��qD�<)D�|)D�� D�HD�=qD�}qD��HD�  D�@ D��HD��HD�  D�@ D�� D�� D�  D�@ D���D�� D�  D�@ D�~�D��HD�  D�>�D��HD�� D���D�AHD�� D�� D�HD�B�D��HD�� D�  D�@ D�� D��HD�  D�=qD�~�D��HD�  D�AHD�� D�� D�  D�@ D�� D���D���D�@ D��HD�� D���D�AHD�� D���D���D�>�D�}qD�� D��D�AHD��HD��HD�  D�>�D�� D�D��D�@ D�� D��HD�HD�@ D�� D��HD�HD�C�D���D��HD���D�>�D�� D��qD���D�>�D�~�D��HD�  D�@ D���D��HD�HD�AHD�D�D�  D�>�DÀ Dþ�D���D�@ DĀ D�� D���D�>�DŁHD��HD�HD�@ D�~�Dƾ�D�  D�AHDǂ�D��HD�HD�@ D�}qDȾ�D�HD�AHDɀ D��HD�HD�>�D�~�Dʾ�D���D�>�D�~�D�� D�HD�@ D�~�D�� D�HD�B�D́HD�� D�  D�@ D�~�Dξ�D���D�@ Dπ D��HD���D�>�D�~�Dо�D���D�>�D�~�D�� D�HD�@ D�~�D�� D�HD�@ DӁHDӾ�D��qD�>�DԀ DԾ�D�HD�AHD�~�D�� D�HD�>�D�}qD�� D�HD�@ DׁHD��HD�HD�AHD؁HDؾ�D�  D�@ DفHD��HD�HD�@ D�~�Dھ�D�  D�B�DہHD۾�D�  D�AHD܁HDܾ�D���D�@ D݀ Dݾ�D�HD�AHDށHD�� D���D�>�D߀ D߾�D��)D�=qD�~�D��HD�  D�@ D� D�� D�HD�@ D�~�D�qD��qD�>�D�~�D�� D�  D�AHD��D���D�HD�@ D� D�� D���D�@ D悏D��HD�  D�>�D�|)D�)D��qD�>�D�~�D�� D�  D�>�D�HD��HD�  D�@ D� D��HD�  D�=qD�}qD�qD�  D�AHD� D��HD�  D�>�D� D��HD��D�B�D� D�� D�HD�AHD�~�DﾸD���D�=qD�~�D�� D��D�B�D� D�D�HD�>�D�}qD��HD�  D�>�D�|)D�qD���D�>�D�HD���D���D�@ D��HD�� D�  D�B�D���D�D�HD�@ D�p�>�G�?#�
?W
=?��
?\?�@�@!G�@8Q�@O\)@\(�@s33@��@���@�Q�@�  @��@�
=@��H@Ǯ@��@ٙ�@��@���@���AG�AA(�A��A�A�HA\)A$z�A*=qA.{A4z�A:�HA>�RAC33AJ=qAMp�ATz�AX��A^{Ae�Ah��Ao\)Atz�Ax��A�  A��\A��A�\)A��\A���A�\)A��HA���A��A��\A�z�A�  A�=qA�(�A�\)A��A�(�A�\)A���A��
A�
=A���A�(�A�ffA���A�(�A�Aȣ�A��
A�p�A���A��HA�Aأ�A��HA�ffA�  A�33A�{A�Q�A�A�A�Q�A�A�p�A�Q�A��A��B Q�B��B�\B(�Bp�BffB(�B	G�B
ffB  BG�B=qB�
BG�B{B�B�B�B33B��B�B33B��B�B�HB ��B!B#
=B$z�B%B&�RB(z�B)B*�RB,z�B-B.�RB0(�B1B2�\B4z�B5G�B6�\B8(�B9�B:ffB<  B<��B>�\B@  B@��BB�RBC�BE�BF�RBG�BH��BJ�\BK�BL��BNffBO\)BP��BR=qBS33BT��BU�BW
=BX��BYp�B[
=B\(�B]�B^�RB_�B`��BbffBc33Bd��Bf{Bf�HBh(�Bi��Bj�\Bk�
Bmp�Bn=qBo�
BqG�Br=qBs\)Bt��Bv{Bw33Bx��By�B{33B|��B}��B~�HB�=qB��RB�G�B�{B��RB�33B��B��\B��B��B�z�B��B��B���B��B��B���B�33B��B���B��B��B���B��B��
B���B��B��
B��\B�
=B��
B���B��B��
B���B��B�B���B�33B�B��\B�G�B��
B�ffB�\)B��
B�z�B�G�B��
B�ffB�33B��B�(�B��HB��B��B��\B�
=B�p�B�(�B��RB���B��B�(�B�ffB��HB�p�B�B�  B���B�
=B�33B�B�{B�Q�B���B�G�B��B�B�Q�B��RB��HB�p�B��
B�{B��\B���B�G�B���B�{B��\B��RB��B���B��
B�Q�B���B�
=B�G�B��B�Q�B��\B���B�G�B��
B�{B�Q�B���B�G�B��B�B�Q�B���B���B�G�B�B�  B�=qB��RB�
=B�G�B���B�{B�z�B��\B�
=B�p�B��B�{B�z�B��RB�
=B��B��
B�  B�ffB��HB��B�\)B��
B�Q�Bģ�B��HB�33BŮB��
B�(�Bƣ�B�
=B�G�BǅB�  B�ffBȣ�B���B�p�B�B��B�Q�B���B�
=B�G�BˮB�(�B�Q�Ḅ�B�
=B�p�BͅB��B�Q�BΣ�B���B�33Bϙ�B��
B�  B�Q�B���B���B�33Bљ�B�{B�=qB�z�B���B�33B�\)B�B�=qB�z�Bԣ�B�
=B�p�Bՙ�B��
B�=qB֣�B���B�G�Bי�B׮B�  B�z�B���B���B�33BمB�  B�Q�B�z�B��HB�G�BۅB�B�=qB܏\BܸRB��B݅B�B�  B�z�B��HB�
=B�\)B�B�(�B�Q�B��B�33B�p�BᙚB�  B�z�B���B���B�G�B�B�  B�=qB��B�
=B�\)B�B�  B�ffB�\B��HB�G�B�B�{B�(�B�z�B�
=B�G�B�B��
B�Q�B��B��HB��B�p�B�  B�{B�ffB��HB�33B�\)B�B�(�B�z�B�RB�
=B�B��
B�  B�z�B��HB��B�p�B��B�(�B�ffB���B�G�B�B�B�=qB��RB���B�G�B�B�=qB��\B���B�\)B���B��B�ffB��HB�33B�p�B�  B�=qB��\B�33B��B�B�{B��\B���B�G�B���B�(�B�ffB���B�G�B��B��C =qC p�C ��C ��C{C33Cp�C�C��C
=CQ�C�C��C�C33C\)C�C��C  C(�Cz�C�RC�
C(�CffC�C��C
=C33CffC�RC�
C
=C\)Cz�C�RC  C�C\)C��CC	  C	G�C	p�C	��C	�C
�C
=qC
�C
��C
�C(�Cp�C��C��C�CQ�Cz�C�
C
=C33C�C�RC�HC(�Cp�C��C��C{C\)C�C�C
=C33C\)C�C��C{C=qC��C��C�C33Cz�C��C��C{CffC�\C�RC  C=qCz�C��C�C33Cp�C�\C�
C�CQ�C�C�RC
=C=qCffC�C��C�CG�C��C�HC  C=qCz�CC��C�Cp�C�RC�HC{CG�C�C�
C{C33Cz�CC�C�CffC�C��C
=CG�C��CC�C33Cz�C�C�
C {C \)C ��C ��C �C!33C!z�C!��C!�
C"
=C"\)C"��C"C"�C#(�C#p�C#�C#�HC$
=C$Q�C$��C$C$�C%33C%z�C%�C%�
C&{C&Q�C&��C&��C&��C'(�C'p�C'�RC'�HC({C(=qC(�C(��C)
=C)=qC)ffC)��C)�
C*(�C*ffC*��C*��C*��C+=qC+z�C+C,  C,�C,\)C,��C,�HC-(�C-G�C-z�C-�RC-��C.G�C.�C.�RC.�HC/�C/ffC/��C/�HC0{C0=qC0z�C0�RC1
=C133C1\)C1��C1�C2(�C2\)C2�C2�RC2��C3G�C3�C3�RC3�HC4�C4p�C4�C4�
C5
=C5\)C5��C5��C5��C633C6p�C6C7  C733C7ffC7��C7�
C8�C8p�C8��C8�
C9
=C9G�C9��C9�HC:{C:=qC:�\C:�
C;  C;=qC;�\C;�
C<  C<=qC<z�C<��C=
=C=33C=z�C=C>
=C>G�C>�C>�RC>��C?G�C?�C?C?�C@33C@�C@C@��CA(�CAffCA�RCB  CB=qCBffCB��CB�CC33CCz�CC��CC�
CD33CDz�CD�CD�HCE�CEffCE�CE��CF(�CFffCF��CF�CG=qCGffCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                     G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�9XA�?}A�9XA�33A��A�{A���Aܙ�A�x�A�G�A��A��mAۛ�A�\)A�M�A�E�A�?}A�7LA�33A�1'A�1'A�1'A�/A�-A�(�A�(�A�/A�/A�-A�5?A�/A�33A�?}A�K�A�9XAڏ\A�jA���A��#A�l�A��A�A�oAԴ9A�?}A��mA� �AЍPA�$�Aκ^A�1'A�dZA�XA�dZAʝ�Aɝ�A�|�Aǥ�Aƥ�AŃAğ�A��
A�E�A���Aº^A���A�M�A�/A��
A��A�JA���A�&�A��PA��A���A��\A���A�l�A�O�A� �A�C�A�S�A�+A���A���A�t�A���A��A��A���A���A���A��A��^A��7A��A��wA�n�A�^5A��!A�p�A���A�hsA�(�A���A��/A���A�l�A�G�A�VA�l�A�ZA���A���A��/A�%A}Ax�`AwdZAv�/As\)Anr�AljAh��Adr�A`��A]��A[�hAY�TAY`BAW�AU��AS��AQ�AO33AJ�AJ  AI�AH^5AG�AF~�AE�^AD�AD  AB=qA>z�A<��A:�uA7ƨA4~�A3�7A2JA0ĜA/&�A.I�A,ĜA+;dA)�#A(�9A&��A$M�A#�^A#x�A"�A!�;AS�AI�AA$�AC�Ar�A^5AE�A�wAVA\)A+AVA�HA-A�Ap�A�\A=qA�A��A1'A
��A
I�A	��A	;dA	%A�\A
=A�A��A�!A�#A��A��A
=A 9X@�;d@��@��@���@��h@��@�@�n�@��-@��@��^@�I�@�o@�7L@�w@�E�@�@�^5@���@�?}@�Z@�1@�"�@�`B@��H@�J@䛦@�~�@�@�bN@�j@�?}@�;d@���@�M�@�t�@߅@�@�O�@�A�@�(�@ۅ@�33@�;d@���@�M�@�/@�bN@� �@��
@��@֏\@ՙ�@�A�@��m@��@��T@щ7@�`B@��@�Q�@��@Ͼw@ϝ�@�"�@�ȴ@�v�@̃@˥�@˶F@ˍP@�;d@�C�@ɲ-@ƸR@�`B@�?}@Ł@�p�@��T@�dZ@��@�M�@�5?@ɺ^@���@���@�
=@�Q�@� �@� �@�K�@�l�@���@�Q�@��j@���@�&�@���@���@�bN@��w@�33@���@���@��@�ƨ@��@�V@��9@�J@��@�7L@��j@��@�j@�Q�@� �@���@�S�@�33@��@���@���@�Q�@��P@��@�S�@��@�~�@��+@��@���@�V@�J@�J@���@���@��@�dZ@�dZ@�33@�o@�"�@�S�@��F@�j@��`@��/@��9@���@�G�@���@��u@�Z@� �@��m@���@��w@�;d@�ff@��h@��@�Ĝ@��u@�I�@��@���@�\)@�o@��!@�ff@�=q@��@��@�@���@�x�@��@��D@�1'@��@��
@�33@���@��+@�ff@�5?@�J@��@�@��7@�G�@�V@��@��/@�Ĝ@��9@�Ĝ@���@��`@��@��`@��/@�Z@�t�@���@�~�@�V@�$�@���@�`B@�/@��@��@���@�ƨ@���@��@�"�@�
=@��@��!@��@��^@�x�@�O�@�V@���@���@��/@�z�@�9X@��
@�\)@�
=@��R@�M�@���@��-@�x�@�&�@��u@�I�@� �@�1@��;@��F@��@�@��-@���@��7@�`B@���@��u@�bN@� �@�1@��@��
@��@�\)@�"�@�ȴ@�$�@���@��@���@��-@�x�@�/@���@���@�1'@��
@��F@�t�@�"�@�@��y@�n�@���@��-@��@�7L@���@�r�@�A�@�b@�t�@��@��R@���@�v�@�^5@�{@��-@�x�@�`B@�7L@��@��/@��9@��D@�bN@�9X@�1'@�(�@�b@��
@��@���@��@�33@��!@�~�@�v�@�5?@�{@�@��7@�?}@��/@��j@��u@�r�@�A�@�b@��m@��w@�33@�
=@��@���@�M�@�=q@���@���@��7@�?}@��/@��@��D@�bN@�9X@� �@��@|�@+@~��@~�R@~E�@}��@}�h@}`B@}O�@}?}@|��@{�m@{�@{33@z�H@z�H@zn�@y��@yx�@yG�@y�@x�`@xr�@xA�@w��@w;d@v��@v�y@v�R@vV@u�T@t�@t��@tz�@tz�@t9X@s�F@sS�@r��@q�@pĜ@p1'@o��@oK�@n�y@nE�@m�-@l��@l(�@kƨ@kt�@k@j~�@j-@i�#@i��@i��@ix�@i7L@hĜ@hĜ@h��@hr�@hQ�@g�@g�@gK�@f��@eO�@e�@d��@d��@d��@dZ@d�@c�
@c��@c�@cdZ@co@b�\@bM�@bJ@a�7@`��@`�9@`��@`bN@`A�@_��@_|�@^�y@^v�@^5?@^@]@]�h@]p�@]O�@]V@\z�@[�
@[S�@Z��@Z�@Y��@Y�@Y�#@Y7L@X�`@Xr�@W�@Wl�@W�@V�R@U�@U/@T�j@TZ@T(�@T(�@T1@S�F@So@R��@R~�@R^5@R�@Q�^@QG�@Q%@PĜ@P��@P �@O��@O+@O
=@N��@N$�@Mp�@L��@LI�@K��@K33@Jn�@J�@I�#@I�7@Ihs@I�@H��@H�u@HbN@H1'@H �@G�@G\)@F��@F@E�-@E�h@Ep�@E�@D�/@D��@D�@Dj@DZ@D9X@C�F@C�@C@B�@B�@B��@B�!@B��@B~�@B^5@B=q@BJ@BJ@A��@A��@A��@A��@Ahs@A7L@A�@A%@@r�@@A�@?�w@?;d@?
=@>ff@=@=V@<��@<�j@<�D@<Z@<(�@;��@;�@;@:^5@9��@9�@8��@8�@8 �@7�;@7�w@7�@7��@7|�@6��@6��@6v�@65?@6@6@5��@5��@5p�@5?}@4�@3�m@3S�@2��@2n�@2-@1�@1�^@1x�@1X@1G�@17L@1%@0Ĝ@0Q�@01'@0 �@0 �@/�;@/�w@/�@/|�@/;d@/�@.��@.ff@.E�@.5?@.@-��@-�h@-`B@-�@,�j@,z�@,(�@+�F@+S�@*��@*~�@*J@)�^@)��@)�7@)hs@)&�@(r�@(A�@( �@'�@'�@'�P@'\)@'�@&�y@&v�@%�@%�-@%p�@$��@$9X@#��@#�
@#�
@#ƨ@#ƨ@#�F@#��@#��@#"�@"�H@"��@"��@"��@"^5@!��@!��@!hs@!X@!X@!&�@ �`@ �9@ �@ r�@�;@K�@
=@��@��@�y@�@�R@��@��@�+@V@{@�T@��@@�h@`B@/@V@��@�@�/@�j@��@�D@j@1@��@o@�!@�\@~�@n�@M�@=q@-@J@�#@��@G�@��@��@��@Q�@�@��@�w@�@�P@�P@|�@|�@\)@��@��@E�@��@��@�h@p�@`B@/@V@�@�/@z�@9X@1@��@S�@C�@C�@o@�@�!@^5@M�@�@J@��@�@�#@��@X@G�@&�@��@�`@�9@r�@�@�@��@�w@�@�P@\)@;d@+@��@�R@��@v�@5?@{@�@�-@�@O�@/@/@/@�@V@��@�@�j@�D@I�@(�@1@�
@ƨ@��@�@S�@S�@33@o@o@
��@
�!A�-A�1'A�9XA�?}A�9XA�;dA�K�A�C�A�C�A�1'A�33A�A�A�-A�(�A� �A�$�A�oA�"�A�1A�oA�A�A��A��HAܡ�AܓuA܋DA܉7A�r�A�ffA�bNA�A�A�/A�"�A�VA�%A�A��mA��#A���A���A۾wA۰!A۝�Aۏ\A�~�A�v�A�hsA�`BA�^5A�VA�O�A�M�A�Q�A�M�A�K�A�O�A�I�A�K�A�K�A�C�A�G�A�E�A�?}A�C�A�A�A�?}A�C�A�=qA�;dA�=qA�5?A�;dA�9XA�7LA�9XA�33A�5?A�5?A�/A�5?A�-A�1'A�1'A�/A�5?A�/A�1'A�33A�/A�33A�1'A�1'A�5?A�/A�1'A�33A�-A�1'A�1'A�-A�1'A�/A�+A�1'A�/A�+A�1'A�1'A�+A�/A�-A�(�A�-A�-A�(�A�&�A�+A�&�A�&�A�+A�(�A�$�A�+A�&�A�-A�1'A�1'A�-A�33A�1'A�-A�1'A�/A�-A�/A�1'A�+A�/A�(�A�(�A�1'A�-A�33A�7LA�1'A�7LA�5?A�1'A�7LA�1'A�-A�/A�(�A�/A�5?A�1'A�33A�5?A�/A�5?A�7LA�5?A�9XA�?}A�A�A�E�A�=qA�C�A�G�A�E�A�O�A�M�A�I�A�O�A�M�A�VA�VA�E�A�33A�1'A�"�A��A�A���Aڧ�A�jA�"�A�1Aٺ^Aٙ�A�t�A�XA�+A��A�JA��yA��#A���Aأ�A؝�A؝�A�x�A�"�A״9AׅA�ffA�$�A���A֩�A�jA�?}A��A�VA�A���A��A��A��TA��`A��HA��
A���A�Aէ�AՁA�M�A�&�A�$�A�%A���A���A�AԸRAԴ9AԴ9AԬAԟ�Aԗ�AԑhA�|�A�VA��
A�ZA�-A��A���A��/AҸRAҕ�AҁA�ffA�?}A�33A�JA��A�Aѩ�AѓuA�Q�A���AиRA�VA��HAϣ�A�p�A�ZA�A�A�5?A�&�A�(�A�"�A��A��A��A�JA�A���A��A��`A��/A���Aκ^AΙ�AΉ7A�z�A�r�A�ffA�VA�I�A�A�A�9XA�+A�+A�(�A��A�VA���A��`A;wA͓uA�p�A�XA�G�A�;dA�7LA�5?A�1'A�&�A�"�A��A̟�A̙�A�~�A�XA�/A��A�  A��`A���A˼jA˥�Aˏ\AˋDA˅A�t�A�bNA�ZA�A�A�7LA�&�A�JA���A��A��A��`A��;A��A�ĜAʩ�A�jA�/A��A��/A��A���A���A�AɶFAɩ�Aɥ�Aə�Aɇ+A�jA�A�A��A���A��HA�Aȕ�A�t�A�`BA�O�A�K�A�K�A�I�A�=qA�;dA�7LA�-A�bA��A��TAǬAǏ\A�ZA�Q�A�;dA�1'A�/A�&�A�
=A��A��/A�AƲ-Aƣ�Aƛ�AƏ\A�~�A�n�A�bNA�M�A�9XA�&�A�{A��mAź^AŃA�hsA�VA�C�A�;dA�(�A� �A� �A��A�{A�1A���A��
AĶFAě�AăA�p�A�\)A�XA�I�A�VA�1A��A��A��`A��/A��HA��/A���A���A�ȴAú^Aã�A×�AÏ\AÃA�p�A�ZA�C�A�7LA�+A�$�A��A��A�VA�bA�VA�1A�A�A�  A���A��A��A��A��A��`A��;A��HA��/A���A�ȴA�ĜA�A²-A®A©�A¥�A�AhA\APA�r�A�VA��A���A�ȴA���A�bNA�;dA��A�t�A��
A��uA�p�A�l�A�bNA�Q�A�;dA�+A�VA��#A��RA���A�t�A�\)A�M�A�1'A�oA�A���A��A��mA��mA��HA��;A��HA��/A��A��A��A���A�ƨA���A��RA���A���A���A�~�A�5?A�ȴA�?}A���A��jA�v�A�M�A��A��A��A��A��A���A���A��A�\)A��A��A���A���A���A��\A��7A�|�A�p�A�dZA�XA�K�A�7LA�VA���A��A��/A���A�A��9A��A���A��A�t�A�dZA�Q�A�C�A�&�A�bA�A��A��;A��A���A�ȴA��!A�5?A���A��#A� �A��A��jA�S�A�(�A���A��A���A��uA��A�z�A�z�A�t�A�n�A�dZA�M�A�&�A� �A��A�A��
A��DA�bNA�+A���A���A���A�~�A�Q�A��A�A���A��+A�ZA��uA�"�A�JA�A���A��wA��\A��A�^5A�E�A� �A�
=A��A��yA���A���A��FA���A���A���A��A��A��+A�O�A��A�t�A��-A�XA�33A�%A��
A���A��A�l�A�ZA�M�A�E�A�9XA�"�A�JA��A���A�A��jA��!A���A���A��7A��A�jA�O�A�&�A��A�1A���A��A��A��A��TA�ƨA��9A���A�v�A�9XA��;A�A�^5A�  A�{A��#A�A��RA���A���A���A�t�A�\)A�I�A�5?A�+A�%A���A��A��TA���A���A�XA��A���A��/A���A�jA�A�A�{A���A�XA���A��mA���A��A���A���A��uA��\A��DA��DA�XA���A�r�A�S�A�?}A�-A�A���A���A�t�A�K�A��wA��A�JA��uA�M�A�33A��A��A��A�{A�oA�VA�VA�bA�JA�VA�VA�
=A��A���A�G�A��A�A���A��mA���A��RA��hA�x�A�9XA���A�p�A�1'A�(�A�bA��#A��A���A���A���A�^5A�A��
A��A���A��\A�l�A�
=A��mA��-A���A��A�`BA�E�A�-A��A���A��!A�~�A�`BA�E�A�/A�"�A�bA��TA��jA��-A��!A��!A��A���A���A���A���A���A���A��\A�n�A�I�A�7LA�&�A��A�%A���A���A���A��A��mA��/A���A�A���A�n�A�E�A�9XA�$�A�  A��wA��A�hsA�7LA�A��A���A���A�ȴA��^A��-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                     A�9XA�?}A�9XA�33A��A�{A���Aܙ�A�x�A�G�A��A��mAۛ�A�\)A�M�A�E�A�?}A�7LA�33A�1'A�1'A�1'A�/A�-A�(�A�(�A�/A�/A�-A�5?A�/A�33A�?}A�K�A�9XAڏ\A�jA���A��#A�l�A��A�A�oAԴ9A�?}A��mA� �AЍPA�$�Aκ^A�1'A�dZA�XA�dZAʝ�Aɝ�A�|�Aǥ�Aƥ�AŃAğ�A��
A�E�A���Aº^A���A�M�A�/A��
A��A�JA���A�&�A��PA��A���A��\A���A�l�A�O�A� �A�C�A�S�A�+A���A���A�t�A���A��A��A���A���A���A��A��^A��7A��A��wA�n�A�^5A��!A�p�A���A�hsA�(�A���A��/A���A�l�A�G�A�VA�l�A�ZA���A���A��/A�%A}Ax�`AwdZAv�/As\)Anr�AljAh��Adr�A`��A]��A[�hAY�TAY`BAW�AU��AS��AQ�AO33AJ�AJ  AI�AH^5AG�AF~�AE�^AD�AD  AB=qA>z�A<��A:�uA7ƨA4~�A3�7A2JA0ĜA/&�A.I�A,ĜA+;dA)�#A(�9A&��A$M�A#�^A#x�A"�A!�;AS�AI�AA$�AC�Ar�A^5AE�A�wAVA\)A+AVA�HA-A�Ap�A�\A=qA�A��A1'A
��A
I�A	��A	;dA	%A�\A
=A�A��A�!A�#A��A��A
=A 9X@�;d@��@��@���@��h@��@�@�n�@��-@��@��^@�I�@�o@�7L@�w@�E�@�@�^5@���@�?}@�Z@�1@�"�@�`B@��H@�J@䛦@�~�@�@�bN@�j@�?}@�;d@���@�M�@�t�@߅@�@�O�@�A�@�(�@ۅ@�33@�;d@���@�M�@�/@�bN@� �@��
@��@֏\@ՙ�@�A�@��m@��@��T@щ7@�`B@��@�Q�@��@Ͼw@ϝ�@�"�@�ȴ@�v�@̃@˥�@˶F@ˍP@�;d@�C�@ɲ-@ƸR@�`B@�?}@Ł@�p�@��T@�dZ@��@�M�@�5?@ɺ^@���@���@�
=@�Q�@� �@� �@�K�@�l�@���@�Q�@��j@���@�&�@���@���@�bN@��w@�33@���@���@��@�ƨ@��@�V@��9@�J@��@�7L@��j@��@�j@�Q�@� �@���@�S�@�33@��@���@���@�Q�@��P@��@�S�@��@�~�@��+@��@���@�V@�J@�J@���@���@��@�dZ@�dZ@�33@�o@�"�@�S�@��F@�j@��`@��/@��9@���@�G�@���@��u@�Z@� �@��m@���@��w@�;d@�ff@��h@��@�Ĝ@��u@�I�@��@���@�\)@�o@��!@�ff@�=q@��@��@�@���@�x�@��@��D@�1'@��@��
@�33@���@��+@�ff@�5?@�J@��@�@��7@�G�@�V@��@��/@�Ĝ@��9@�Ĝ@���@��`@��@��`@��/@�Z@�t�@���@�~�@�V@�$�@���@�`B@�/@��@��@���@�ƨ@���@��@�"�@�
=@��@��!@��@��^@�x�@�O�@�V@���@���@��/@�z�@�9X@��
@�\)@�
=@��R@�M�@���@��-@�x�@�&�@��u@�I�@� �@�1@��;@��F@��@�@��-@���@��7@�`B@���@��u@�bN@� �@�1@��@��
@��@�\)@�"�@�ȴ@�$�@���@��@���@��-@�x�@�/@���@���@�1'@��
@��F@�t�@�"�@�@��y@�n�@���@��-@��@�7L@���@�r�@�A�@�b@�t�@��@��R@���@�v�@�^5@�{@��-@�x�@�`B@�7L@��@��/@��9@��D@�bN@�9X@�1'@�(�@�b@��
@��@���@��@�33@��!@�~�@�v�@�5?@�{@�@��7@�?}@��/@��j@��u@�r�@�A�@�b@��m@��w@�33@�
=@��@���@�M�@�=q@���@���@��7@�?}@��/@��@��D@�bN@�9X@� �@��@|�@+@~��@~�R@~E�@}��@}�h@}`B@}O�@}?}@|��@{�m@{�@{33@z�H@z�H@zn�@y��@yx�@yG�@y�@x�`@xr�@xA�@w��@w;d@v��@v�y@v�R@vV@u�T@t�@t��@tz�@tz�@t9X@s�F@sS�@r��@q�@pĜ@p1'@o��@oK�@n�y@nE�@m�-@l��@l(�@kƨ@kt�@k@j~�@j-@i�#@i��@i��@ix�@i7L@hĜ@hĜ@h��@hr�@hQ�@g�@g�@gK�@f��@eO�@e�@d��@d��@d��@dZ@d�@c�
@c��@c�@cdZ@co@b�\@bM�@bJ@a�7@`��@`�9@`��@`bN@`A�@_��@_|�@^�y@^v�@^5?@^@]@]�h@]p�@]O�@]V@\z�@[�
@[S�@Z��@Z�@Y��@Y�@Y�#@Y7L@X�`@Xr�@W�@Wl�@W�@V�R@U�@U/@T�j@TZ@T(�@T(�@T1@S�F@So@R��@R~�@R^5@R�@Q�^@QG�@Q%@PĜ@P��@P �@O��@O+@O
=@N��@N$�@Mp�@L��@LI�@K��@K33@Jn�@J�@I�#@I�7@Ihs@I�@H��@H�u@HbN@H1'@H �@G�@G\)@F��@F@E�-@E�h@Ep�@E�@D�/@D��@D�@Dj@DZ@D9X@C�F@C�@C@B�@B�@B��@B�!@B��@B~�@B^5@B=q@BJ@BJ@A��@A��@A��@A��@Ahs@A7L@A�@A%@@r�@@A�@?�w@?;d@?
=@>ff@=@=V@<��@<�j@<�D@<Z@<(�@;��@;�@;@:^5@9��@9�@8��@8�@8 �@7�;@7�w@7�@7��@7|�@6��@6��@6v�@65?@6@6@5��@5��@5p�@5?}@4�@3�m@3S�@2��@2n�@2-@1�@1�^@1x�@1X@1G�@17L@1%@0Ĝ@0Q�@01'@0 �@0 �@/�;@/�w@/�@/|�@/;d@/�@.��@.ff@.E�@.5?@.@-��@-�h@-`B@-�@,�j@,z�@,(�@+�F@+S�@*��@*~�@*J@)�^@)��@)�7@)hs@)&�@(r�@(A�@( �@'�@'�@'�P@'\)@'�@&�y@&v�@%�@%�-@%p�@$��@$9X@#��@#�
@#�
@#ƨ@#ƨ@#�F@#��@#��@#"�@"�H@"��@"��@"��@"^5@!��@!��@!hs@!X@!X@!&�@ �`@ �9@ �@ r�@�;@K�@
=@��@��@�y@�@�R@��@��@�+@V@{@�T@��@@�h@`B@/@V@��@�@�/@�j@��@�D@j@1@��@o@�!@�\@~�@n�@M�@=q@-@J@�#@��@G�@��@��@��@Q�@�@��@�w@�@�P@�P@|�@|�@\)@��@��@E�@��@��@�h@p�@`B@/@V@�@�/@z�@9X@1@��@S�@C�@C�@o@�@�!@^5@M�@�@J@��@�@�#@��@X@G�@&�@��@�`@�9@r�@�@�@��@�w@�@�P@\)@;d@+@��@�R@��@v�@5?@{@�@�-@�@O�@/@/@/@�@V@��@�@�j@�D@I�@(�@1@�
@ƨ@��@�@S�@S�@33@o@o@
��@
�!A�-A�1'A�9XA�?}A�9XA�;dA�K�A�C�A�C�A�1'A�33A�A�A�-A�(�A� �A�$�A�oA�"�A�1A�oA�A�A��A��HAܡ�AܓuA܋DA܉7A�r�A�ffA�bNA�A�A�/A�"�A�VA�%A�A��mA��#A���A���A۾wA۰!A۝�Aۏ\A�~�A�v�A�hsA�`BA�^5A�VA�O�A�M�A�Q�A�M�A�K�A�O�A�I�A�K�A�K�A�C�A�G�A�E�A�?}A�C�A�A�A�?}A�C�A�=qA�;dA�=qA�5?A�;dA�9XA�7LA�9XA�33A�5?A�5?A�/A�5?A�-A�1'A�1'A�/A�5?A�/A�1'A�33A�/A�33A�1'A�1'A�5?A�/A�1'A�33A�-A�1'A�1'A�-A�1'A�/A�+A�1'A�/A�+A�1'A�1'A�+A�/A�-A�(�A�-A�-A�(�A�&�A�+A�&�A�&�A�+A�(�A�$�A�+A�&�A�-A�1'A�1'A�-A�33A�1'A�-A�1'A�/A�-A�/A�1'A�+A�/A�(�A�(�A�1'A�-A�33A�7LA�1'A�7LA�5?A�1'A�7LA�1'A�-A�/A�(�A�/A�5?A�1'A�33A�5?A�/A�5?A�7LA�5?A�9XA�?}A�A�A�E�A�=qA�C�A�G�A�E�A�O�A�M�A�I�A�O�A�M�A�VA�VA�E�A�33A�1'A�"�A��A�A���Aڧ�A�jA�"�A�1Aٺ^Aٙ�A�t�A�XA�+A��A�JA��yA��#A���Aأ�A؝�A؝�A�x�A�"�A״9AׅA�ffA�$�A���A֩�A�jA�?}A��A�VA�A���A��A��A��TA��`A��HA��
A���A�Aէ�AՁA�M�A�&�A�$�A�%A���A���A�AԸRAԴ9AԴ9AԬAԟ�Aԗ�AԑhA�|�A�VA��
A�ZA�-A��A���A��/AҸRAҕ�AҁA�ffA�?}A�33A�JA��A�Aѩ�AѓuA�Q�A���AиRA�VA��HAϣ�A�p�A�ZA�A�A�5?A�&�A�(�A�"�A��A��A��A�JA�A���A��A��`A��/A���Aκ^AΙ�AΉ7A�z�A�r�A�ffA�VA�I�A�A�A�9XA�+A�+A�(�A��A�VA���A��`A;wA͓uA�p�A�XA�G�A�;dA�7LA�5?A�1'A�&�A�"�A��A̟�A̙�A�~�A�XA�/A��A�  A��`A���A˼jA˥�Aˏ\AˋDA˅A�t�A�bNA�ZA�A�A�7LA�&�A�JA���A��A��A��`A��;A��A�ĜAʩ�A�jA�/A��A��/A��A���A���A�AɶFAɩ�Aɥ�Aə�Aɇ+A�jA�A�A��A���A��HA�Aȕ�A�t�A�`BA�O�A�K�A�K�A�I�A�=qA�;dA�7LA�-A�bA��A��TAǬAǏ\A�ZA�Q�A�;dA�1'A�/A�&�A�
=A��A��/A�AƲ-Aƣ�Aƛ�AƏ\A�~�A�n�A�bNA�M�A�9XA�&�A�{A��mAź^AŃA�hsA�VA�C�A�;dA�(�A� �A� �A��A�{A�1A���A��
AĶFAě�AăA�p�A�\)A�XA�I�A�VA�1A��A��A��`A��/A��HA��/A���A���A�ȴAú^Aã�A×�AÏ\AÃA�p�A�ZA�C�A�7LA�+A�$�A��A��A�VA�bA�VA�1A�A�A�  A���A��A��A��A��A��`A��;A��HA��/A���A�ȴA�ĜA�A²-A®A©�A¥�A�AhA\APA�r�A�VA��A���A�ȴA���A�bNA�;dA��A�t�A��
A��uA�p�A�l�A�bNA�Q�A�;dA�+A�VA��#A��RA���A�t�A�\)A�M�A�1'A�oA�A���A��A��mA��mA��HA��;A��HA��/A��A��A��A���A�ƨA���A��RA���A���A���A�~�A�5?A�ȴA�?}A���A��jA�v�A�M�A��A��A��A��A��A���A���A��A�\)A��A��A���A���A���A��\A��7A�|�A�p�A�dZA�XA�K�A�7LA�VA���A��A��/A���A�A��9A��A���A��A�t�A�dZA�Q�A�C�A�&�A�bA�A��A��;A��A���A�ȴA��!A�5?A���A��#A� �A��A��jA�S�A�(�A���A��A���A��uA��A�z�A�z�A�t�A�n�A�dZA�M�A�&�A� �A��A�A��
A��DA�bNA�+A���A���A���A�~�A�Q�A��A�A���A��+A�ZA��uA�"�A�JA�A���A��wA��\A��A�^5A�E�A� �A�
=A��A��yA���A���A��FA���A���A���A��A��A��+A�O�A��A�t�A��-A�XA�33A�%A��
A���A��A�l�A�ZA�M�A�E�A�9XA�"�A�JA��A���A�A��jA��!A���A���A��7A��A�jA�O�A�&�A��A�1A���A��A��A��A��TA�ƨA��9A���A�v�A�9XA��;A�A�^5A�  A�{A��#A�A��RA���A���A���A�t�A�\)A�I�A�5?A�+A�%A���A��A��TA���A���A�XA��A���A��/A���A�jA�A�A�{A���A�XA���A��mA���A��A���A���A��uA��\A��DA��DA�XA���A�r�A�S�A�?}A�-A�A���A���A�t�A�K�A��wA��A�JA��uA�M�A�33A��A��A��A�{A�oA�VA�VA�bA�JA�VA�VA�
=A��A���A�G�A��A�A���A��mA���A��RA��hA�x�A�9XA���A�p�A�1'A�(�A�bA��#A��A���A���A���A�^5A�A��
A��A���A��\A�l�A�
=A��mA��-A���A��A�`BA�E�A�-A��A���A��!A�~�A�`BA�E�A�/A�"�A�bA��TA��jA��-A��!A��!A��A���A���A���A���A���A���A��\A�n�A�I�A�7LA�&�A��A�%A���A���A���A��A��mA��/A���A�A���A�n�A�E�A�9XA�$�A�  A��wA��A�hsA�7LA�A��A���A���A�ȴA��^A��-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                     G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
��B
��B
�qB
��B
�kB
�CB
�'B
�xB
�CB
��B
�kB
��B
��B
�1B
�eB
�eB
��B
�eB
�B
��B
��B
��B
�	B
��B
��B
��B
�B
��B
�\B
�'B
��B
�B
��B
��B.}B\�Bp;B��B��B�B�mB��B	�B	B@�BN<BncB��B��B��B��B��B��B��BΥB��BخB��B�B�	B��B��B�rB��B�oB�2B�TBߤB�)B˒B�[B��B�3B�*B�B�lB}�Bo Bb�B[#BS[B;�B&�BOB�B�B��B�gB��B��B��B�BgBgmB`�BOvB@�B-CB!�BFB�B�B iB
�B
�B
�B
ѷB
�^B
�BB
�eB
�VB
��B
yrB
bNB
7�B
+B
\B	�AB	�BB	��B	��B	�B	�	B	��B	�%B	s�B	g8B	WsB	OB	HKB	D�B	<B	1'B	#B	�B	  B�>B��B�vB�5B�B�fB� B�jB��B�jB� B�B��B��B�:B��B�	B��B�FB�\B��B��B��B��B�MB�B�;B�iB�AB.B{JBz�Bx8Bu�Bq�Bu�BsMBqABu�Bq�BqABpBo5Bp�Bn�BkQBg8Bf�Be�BdZBe�Bc�Ba|B`BB^�B]�B\)B]dBZQB[#B\�B^�B^5BZQBW�BWsBVBV�BW�BV�BXB^B^B`�Bb�Bk�Br�BwfBu%B�B��B��B�B�VB��B�MB��B��B��B��B��B��B��B~�B��B��B�1B�$B�MB�_B��B��B��B��B��B�-B�zB��B�B��B�*B�B�aBĜB�gBĜB�B�tB�^BѷB�NB�aB��B�9B��BݘB�HB�B�B�B�B�B�B�/B��B��B�B�ZB�B	 �B��B	uB	�B	 B	SB	�B	*�B	8�B	?�B	JXB	I�B	M6B	EB	E�B	XyB	[�B	Z�B	W�B	W?B	C-B	J#B	K)B	VB	\�B	]�B	^jB	_�B	b�B	e`B	hsB	i�B	tB	zDB	��B	��B	|B	��B	��B	�.B	�hB	�B	��B	�oB	�@B	��B	��B	�YB	��B	��B	�CB	��B	�$B	�YB	�eB	�qB	��B	�!B	��B	�_B	��B	�kB	�xB	�B	�B	��B	��B	��B	��B	�nB	��B	�_B	��B	�-B	��B	�RB	��B	��B	�[B	�[B	�[B	�UB	�[B	B	�'B	��B	��B	�-B	�HB	�wB	�}B	��B	� B	B	��B	�mB	�B	ȀB	ɆB	�XB	��B	˒B	��B	�jB	��B	ϫB	�B	ӏB	ԕB	�gB	�B	ٴB	ںB	�WB	�)B	��B	ݘB	�B	�B	��B	ߤB	��B	�B	�HB	��B	�fB	�sB	�B	�DB	�B	��B	�B	�B	�B	�B	�B	��B	� B	��B	�iB	�B	�B	�ZB	��B	�ZB	��B	�B	�B	��B	�B	�B	��B	��B	�B	�B	�xB	�B	��B	��B	�JB	�JB	�PB	��B	�"B	�(B	��B	��B	��B	��B
 iB
 iB
 �B
 �B
 �B
 �B
{B
%B
_B
_B
_B
�B
	�B

�B
xB
�B
~B
JB
~B
�B
PB
PB
�B
�B
�B
�B
�B
VB
�B
�B
�B
�B
.B
.B
�B
�B
4B
 B
�B
 B
 B
�B
�B
�B
�B
uB
FB
B
B
�B
$B
�B
_B
+B
1B
�B
7B
kB
kB
	B
qB
qB
qB
�B
B
B
B
CB
CB
B
CB
xB
B
!B
VB
!B
 �B
 �B
!�B
!-B
!-B
!bB
!�B
!bB
!bB
!�B
"hB
"�B
#B
$tB
$B
$@B
$�B
$tB
$�B
$�B
$�B
$@B
#�B
$@B
#�B
#�B
#�B
$@B
$�B
$�B
$B
%B
%FB
%B
$�B
%�B
%�B
%�B
%�B
%FB
&B
&�B
&�B
'B
'�B
(XB
(�B
(�B
)�B
)�B
*eB
+B
+kB
*�B
*�B
*0B
)�B
)�B
)�B
+6B
,�B
,�B
,=B
.IB
/OB
/�B
/�B
/�B
/OB
/�B
/OB
.�B
/B
0�B
1�B
2�B
2�B
2�B
2�B
33B
33B
3hB
4nB
4�B
5�B
6B
6B
5�B
6B
6zB
7�B
8B
8B
8B
8�B
9$B
9�B
9�B
8B
8B
8RB
8RB
8�B
9$B
9�B
9�B
9�B
9�B
9�B
:*B
:^B
:�B
:�B
;dB
;�B
;�B
;�B
<6B
<6B
<�B
<�B
=�B
>B
>BB
>wB
>�B
>wB
>�B
>�B
?B
?HB
?�B
@B
A B
AUB
@�B
@�B
@�B
@�B
A B
AUB
A�B
A�B
A�B
B'B
B�B
CaB
C�B
D3B
C�B
D3B
C�B
DgB
EB
EmB
E9B
E9B
EmB
FB
F?B
F�B
F�B
F�B
GEB
GzB
HKB
H�B
IB
J#B
K^B
K�B
L0B
L�B
M6B
N<B
N�B
N�B
OB
OBB
OvB
O�B
PB
PB
PB
O�B
P�B
PB
PHB
O�B
O�B
OvB
OvB
OvB
OvB
O�B
R B
RTB
RTB
RTB
RTB
Q�B
Q�B
RTB
RTB
RTB
RTB
RTB
RTB
R�B
R�B
R�B
S[B
S[B
S[B
S[B
S�B
S�B
S�B
T,B
T,B
TaB
S�B
TaB
T�B
T�B
VmB
VmB
VB
W
B
WsB
W�B
W�B
W�B
W�B
XB
XB
XyB
X�B
YB
YKB
Y�B
Y�B
ZB
ZQB
Z�B
Z�B
Z�B
[�B
[�B
[�B
\)B
\]B
\)B
\�B
\�B
\�B
\�B
]�B
^B
_B
_;B
_pB
_�B
_�B
_�B
`B
`B
`B
`B
`vB
`�B
aHB
a|B
aHB
aHB
a�B
a�B
bB
bB
bNB
bB
c B
c B
cTB
c B
c�B
c�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
gmB
g�B
g�B
g�B
h
B
h>B
h
B
h>B
h�B
h�B
iDB
iyB
iyB
i�B
jB
j�B
j�B
kB
j�B
kB
kB
kB
kB
kB
k�B
k�B
k�B
k�B
k�B
k�B
lWB
l�B
l�B
l�B
l�B
m)B
m)B
m]B
m�B
m]B
n�B
n�B
n�B
n�B
o B
o B
o B
o5B
o5B
o5B
oiB
o�B
o�B
pB
pB
pB
p;B
poB
p�B
p�B
p�B
p�B
p�B
qAB
qB
qB
qB
q�B
rB
r�B
sMB
s�B
sMB
s�B
s�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
t�B
u%B
u�B
u�B
v+B
v+B
v+B
v`B
v`B
v`B
v+B
v`B
v�B
w2B
wfB
x8B
x8B
x8B
xlB
x8B
x�B
x�B
x�B
x�B
y�B
y�B
y�B
zxB
z�B
zxB
z�B
z�B
z�B
{JB
{�B
{B
|B
{�B
{�B
{�B
{�B
|PB
|�B
|�B
|�B
|�B
|�B
}"B
}�B
}�B
}�B
~(B
~(B
~(B
~�B
~�B
~�B
~�B
~�B
.B
.B
�B
�B
�B
� B
�4B
��B
��B
�B
��B
��B
��B
�B
�;B
�B
�oB
�oB
��B
��B
�B
�AB
�AB
�uB
�uB
��B
�uB
��B
��B
��B
��B
��B
�1B
�!B
��B
�=B
�xB
��B
�YB
�qB
��B
��B
�	B
�eB
��B
�B
��B
�CB
�7B
��B
�=B
��B
�xB
�B
��B
�B
��B
��B
�CB
�=B
�xB
�IB
�B
��B
��B
��B
��B
��B
�B
��B
�B
�B
��B
�qB
��B
��B
��B
�7B
�eB
�1B
�kB
�_B
�1B
��B
�7B
��B
��B
�B
�_B
�kB
�1B
��B
�=B
�+B
�1B
�kB
��B
�eB
�7B
�_B
�B
�7B
�+B
�kB
��B
��B
�B
�_B
�7B
��B
�1B
��B
�+B
�kB
�1B
�eB
�kB
��B
�=B
�7B
��B
�qB
��B
��B
�=B
��B
�B
��B
�B
�CB
�7B
�kB
��B
�B
��B
�qB
�eB
�kB
�B
��B
�7B
�B
��B
��B
��B
��B
��B
�	B
��B
�1B
�	B
�	B
�1B
��B
�qB
�eB
�=B
��B
��B
�CB
��B
��B
�xB
��B
�B
��B
�B
��B
��B
�OB
�xB
�B
�~B
�B
�OB
��B
��B
��B
��B
��B
��B
�VB
��B
�'B
��B
��B
�\B
�VB
��B
��B
�'B
��B
��B
��B
��B
��B
�hB
�B
��B
�_B
�B
�B
�qB
��B
��B
��B
�-B
�aB
یB
�B
��BBAB+B	�B.BB)_B9XBI�BK)BXBXEB\�B]�BaHBb�Bd�BjBk�Bm]BzxBz�ByrB��B��B��B��B��B��B�B�$B� B��B�^B�0B�6B�HB�HB�HB��B��B�NB�&B��B��B�WB��B�B��B��B��B�BB�B	lB	�B�B
�B�BBB BB1[B8�B:*B9�B>BBAUBEmBHBGBN<BK�BJXBUgBRTBS&BT�BS�B^BiBg�B|B~(B��B��B�MB��B��B��B�MB��B��B�SB�SB��B��B�_B�_B�=B�fB�B�"B�\B��B��B��B� B��B�B�B��B�uB�B��B�@B�B��B��B��B��B�4B�bB��B�B�:B��B��B��B�B�OB�[B��B��B��B�9B��B�B��B�XB�*B��B��B�^B��B�<B�6B�<B�UB��B��B��B��B�'B��B��B��B��B��B�tB�0B�B��B�0B��B�dB��B̘B�B�BʌB�<B�TB�TB�[B�2B֡B��B��B��BרBӏB�aB�,BҽB��B��BӏB� BҽB�mB�?B�2B��B�B��B�?B��BܒBںB�WB�vB�B�pB�B��B�&B�B�NB�ZB�2B�B�2B��B�mB�B�5B�B�B�B�MB�AB�;B��B�B��B�GB�B�TB�`B��B��B��B��B�xB��B�DB�"BMB��B�PB�JB��B�VB�xB�B�B�B�B��B�VB�PB��B��B�B�VB��B��B��B�B�rB��B�xB�>B��B�B��B�rB��B��B�DB��B��B��B��B��B��B��B��B��B��B��B��B��B��B�B��B�+B�MB�B��B�;B��B�;B�B�]B�B�>B�"B�GB��B�B�B�vB�&B�B��B��B��B�mB�B��B�`B��B��B�2B��B�B��B��B�|B�pB��B��B��BߤB�BB�B�;B�jB�vBޞB�5B�B��BیB��B��B�yB��B�QB�gB͟B�BB��BB˒B�jBΥB��BרB�mBٴB�B՛B�2B��BбB�B�B�BBΥB̘B͟B̘B�jB��BʌB�B��B�zB��B�?B�EBÖB�?B�OB�OB��B�OB��B��B�<B�0B�^B�RB�LB��B�B�B��B�BB��B��B�B�_B��B�hB��B��B��B�	B��B��B��B�MB�B�uB�B}�Bz�B}�B~]Bz�Bt�Bv�Bs�Bq�Bm)BpoBm)Bk�Bn�Be�BbNBl�B�;Bd�BZ�BW�BXEB`�BXyBZ�B\�B^5Bb�BYBX�BYBY�BTaBR�BTaBQ�BPHBS�Bd�BR�BH�BOvBOBQB<�B3�B6FB2aB-wB,�B*�B+�B'�B&LB$@B'�B%�B$B"�B!�B�BVBVB�B�BkBVB~B�B�B�B�B�B�B�B.BuBVBVB�B(BMB($B�>BuBDB�BB�BݘB�B��B��BںB��B�KB�)B�?B�2B��B��BбB��B�&B�WB�jBȀBƨB��B�EB��B��B�dB��B��B�[B��B��B��B��B�B��B�B�'B�UB��B��B�B�B�	B�xB��B��B��B��B�FB��B��B�BkBlWBh�Bd&Bf�Bf2BffBgmBe�Bd�Bd&Be�Be,Bd�BiDBz�Bi�Bf2B`vBa�Bb�Bc�BaB`BB]�B_�B\]B]dBT�BMBQBQNBOBH�BF?BEBN�BE�BB�B?�B8�B7�B8RBA�B2�B6�B,B-�B-CB+�B+B%�B)�B/�B#:B!bB�B�B�B�BVB�B�BB4BB:B�B�B�B(B�B.B�B\B�BBBB�B�B�B�B�BMB�B�B{B
rB
�(B
�B
��B
�cB�B
�B
��B
��B
�+B
��B
��B
�B
�,B
�B
�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                     B
��B
��B
��B
��B
�0B
��B
��B
�wB
��B
��B
��B
��B
�$B
��B
��B
��B
��B
��B
��B
�RB
�$B
�$B
��B
�YB
��B
��B
��B
�eB
�7B
��B
�wB
��B
�hB
�OB
��B*�BX�Bl�B�'B�<B�dBҽB��B�BYB<�BJ�Bj�B�B��B�'B�B�B��B�,B��B�B��B�;B��B�YB�	B�1B��B�B�B�BߤB��B�yB��BϫB�KB��B�zB�kB��BzDBkPB_;BWsBO�B7�B#9B�B�B
=B�#BѷB�B��B�LB�_BcTBc�B\�BK�B<�B)�BB�B�B�B
��B
�iB
�TB
�]B
�B
ǮB
��B
��B
��B
�B
u�B
^�B
49B
'RB
�B	�B	ܒB	�2B	�/B	�XB	�YB	�B	�uB	p;B	c�B	S�B	K^B	D�B	@�B	8RB	-wB	UB	*B�PB��B�GB��B�B�
B�B�pBںB�BɺB�pB�^B��B�3B��B�CB�YB�B��B��B�4B��B�B��B��B~\B}�B|�B~�B{~Bw�Bw1Bt�Bq�Bn.Bq�Bo�Bm�Bq�Bm�Bm�BlWBk�Bl�Bj�Bg�Bc�BcBa�B`�Ba�B`B]�B\�BZ�BY�BXyBY�BV�BWsBYB[#BZ�BV�BT,BS�BRTBR�BT,BS&BT`BZQBZQB]/B_;Bh>Bo Bs�BquB}VB~(B�B�eB��B�B��B��B��B�1B��B�@B��B�4B{B� B��B��B�tB��B��B��B��B�<B��B�'B�}B��B�3B�mB�EB�zB�dB��B��B��B��B�[B��BǮB�B͞BбB� B҉B�2B��BݘB��B��B�TB�`B�`B�lB�B�8B�5B��B�B�lB��B�JB��B	B	PB	�B	0B	&�B	4�B	<6B	F�B	F
B	I�B	AUB	B&B	T�B	XB	W
B	T,B	S�B	?}B	FsB	GyB	RTB	X�B	ZB	Z�B	[�B	_B	a�B	d�B	e�B	poB	v�B	��B	�B	xlB	��B	�1B	�~B	��B	�VB	��B	��B	��B	��B	�@B	��B	�LB	�*B	��B	��B	�tB	��B	��B	��B	�0B	�qB	�B	��B	�LB	��B	��B	�eB	�kB	�B	�*B	�B	�OB	��B	��B	��B	�0B	�}B	�9B	��B	�B	�#B	��B	��B	��B	��B	��B	��B	�wB	�B	�B	�}B	��B	��B	��B	�6B	�pB	��B	�B	��B	�[B	��B	��B	ƨB	�B	��B	�B	ɺB	�#B	��B	�jB	��B	��B	ѷB	�`B	�B	�
B	קB	�yB	�B	��B	�QB	�QB	�#B	��B	�)B	��B	ݘB	�B	�B	��B	�`B	�B	��B	�2B	��B	��B	��B	��B	��B	�JB	�PB	�B	�B	�WB	��B	�B	��B	�B	��B	�SB	�SB	�B	�SB	�SB	��B	��B	�`B	�fB	��B	�`B	��B	�1B	��B	��B	��B	��B	�rB	�xB	�JB	�B	�JB	�JB	��B	��B	��B	��B	�"B	�"B	��B
uB
�B
�B
�B
MB
%B
+B
�B
1B
�B
�B
�B
	7B
	�B
	�B

=B
CB

�B
B

�B

�B

�B

=B

	B

�B
~B
~B
�B
B
�B
PB
B
PB
PB
�B
!B
�B
'B
�B
�B
hB
nB
B
tB
B
�B
{B
�B
B
�B
�B
�B
YB
�B
�B
�B
�B
_B
_B
_B
�B
�B
_B
�B
�B
kB
qB
�B
qB
�B
B
�B
}B
}B
�B
B
�B
�B
OB
�B
�B
UB
 �B
 [B
 �B
 �B
 �B
 �B
 �B
!-B
 �B
 'B
 �B
�B
�B
 'B
 �B
 �B
!-B
 [B
!bB
!�B
!bB
!-B
!�B
"3B
!�B
!�B
!�B
"hB
"�B
#B
#nB
#�B
$�B
%B
%FB
%�B
%�B
&�B
'RB
'�B
'B
'B
&�B
&LB
&LB
&LB
'�B
(�B
(�B
(�B
*�B
+�B
+�B
,<B
,B
+�B
+�B
+�B
+6B
+kB
,�B
-�B
/B
.�B
/B
/OB
/�B
/�B
/�B
0�B
1'B
1�B
2aB
2aB
2-B
2aB
2�B
49B
4mB
4mB
4mB
4�B
5tB
5�B
6B
4mB
4mB
4�B
4�B
4�B
5tB
5�B
5�B
6B
6B
6B
6zB
6�B
6�B
7B
7�B
8B
8B
7�B
8�B
8�B
8�B
9#B
9�B
:^B
:�B
:�B
:�B
:�B
;0B
:�B
;dB
;�B
<6B
<jB
=pB
=�B
=B
=B
<�B
=<B
=pB
=�B
=�B
>B
>B
>wB
?HB
?�B
@B
@�B
@NB
@�B
@NB
@�B
AUB
A�B
A�B
A�B
A�B
B[B
B�B
B�B
B�B
C,B
C�B
C�B
D�B
D�B
EmB
FsB
G�B
HB
H�B
IB
I�B
J�B
J�B
J�B
K^B
K�B
K�B
L/B
LdB
LdB
LdB
L/B
MB
LdB
L�B
L/B
K�B
K�B
K�B
K�B
K�B
L/B
NpB
N�B
N�B
N�B
N�B
N<B
N<B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
N�B
OBB
O�B
O�B
O�B
O�B
O�B
PB
PB
P|B
P|B
P�B
PB
P�B
P�B
QB
R�B
R�B
RTB
SZB
S�B
S�B
S�B
S�B
T,B
T`B
T`B
T�B
T�B
U�B
U�B
VB
V8B
VmB
V�B
W>B
W>B
W>B
W�B
XB
XB
XyB
X�B
XyB
X�B
X�B
X�B
X�B
Y�B
ZQB
[WB
[�B
[�B
[�B
[�B
\)B
\]B
\]B
\]B
\]B
\�B
]/B
]�B
]�B
]�B
]�B
^5B
^5B
^iB
^iB
^�B
^iB
_pB
_pB
_�B
_pB
_�B
_�B
_�B
`AB
`AB
`�B
aB
aB
aGB
a�B
bB
bB
b�B
b�B
b�B
b�B
cB
c�B
d%B
d%B
d%B
dZB
d�B
dZB
d�B
d�B
d�B
e�B
e�B
e�B
e�B
f�B
gB
g8B
glB
g8B
glB
glB
glB
glB
glB
h
B
h
B
h
B
h
B
g�B
h>B
h�B
iB
iDB
iDB
iB
iyB
iyB
i�B
i�B
i�B
j�B
kB
kB
kB
kPB
kPB
kPB
k�B
k�B
k�B
k�B
k�B
l"B
lWB
lWB
lWB
l�B
l�B
m(B
m(B
m(B
m(B
m(B
m�B
m]B
m]B
m]B
n.B
ncB
o5B
o�B
o�B
o�B
o�B
pB
o�B
pB
pB
p;B
poB
qB
qAB
qAB
quB
rB
rGB
r{B
r{B
r{B
r�B
r�B
r�B
r{B
r�B
sB
s�B
s�B
t�B
t�B
t�B
t�B
t�B
t�B
u%B
u%B
u%B
u�B
u�B
u�B
v�B
v�B
v�B
v�B
v�B
w1B
w�B
xB
w�B
xlB
xB
x7B
x7B
x7B
x�B
y	B
x�B
y>B
y>B
y>B
yrB
zB
zDB
zDB
zxB
zxB
zxB
z�B
z�B
z�B
z�B
{JB
{~B
{~B
{�B
|B
|B
|PB
|�B
|�B
}"B
}VB
}"B
}"B
}"B
}VB
}�B
}VB
}�B
}�B
~(B
~(B
~\B
~�B
~�B
~�B
~�B
.B
~�B
.B
.B
.B
� B
.B
��B
�qB
��B
��B
��B
�0B
��B
��B
�@B
�$B
�YB
��B
�B
�eB
�0B
��B
��B
��B
��B
�B
��B
�hB
��B
�hB
��B
��B
��B
��B
��B
��B
�_B
�CB
�=B
��B
�*B
�FB
�nB
�B
�_B
�eB
��B
��B
�$B
�$B
�$B
��B
��B
��B
��B
��B
��B
��B
��B
�FB
��B
�RB
��B
��B
��B
��B
��B
�{B
��B
��B
��B
��B
��B
��B
�RB
��B
�{B
��B
�B
�LB
�RB
��B
��B
��B
��B
��B
�{B
��B
��B
��B
��B
�B
��B
��B
�B
��B
��B
�$B
��B
��B
�_B
�$B
�RB
��B
��B
��B
�*B
�RB
�$B
��B
��B
��B
�_B
�B
��B
�_B
��B
��B
�*B
��B
�B
�YB
�*B
��B
�YB
�YB
��B
�B
��B
��B
��B
��B
�*B
��B
�7B
��B
��B
�7B
�_B
��B
�kB
�0B
��B
��B
��B
�kB
��B
�eB
��B
�CB
�=B
��B
��B
�CB
��B
��B
�OB
�wB
�B
��B
��B
��B
��B
�IB
�wB
��B
�B
�OB
��B
�B
��B
�nB
�B
��B
�nB
�RB
��B
�B
��B
��B
�}B
��B
��B
�fB
�B
�\B
��B{B%B~BeB%�B5�BF?BGyBT`BT�BYKBY�B]�B^�B`�BffBh
Bi�Bv�Bv�Bu�B|�B�IB�*B�LB�7B��B�[B�tB�pB�&BǮBȀBɆB̘B̘B̘B�BB�5B͞B�vB�B�BקB�/B�fB�.B�(B��B �BoBB�B�BB+B	7B	kBeBPBnB-�B5?B6zB6B:�B=�BA�BDgBCaBJ�BHKBF�BQ�BN�BOvBQBPHBZQBe`Bd%BxlBzxB~(B~�B��B��B�:B�B��B�:B��B��B��B�GB�B��B��B��B��B�SB�rB��B��B�CB�B�PB��B�VB�VB�!B��B�VB�!B��B�nB��B�@B�0B�IB��B��B��B�[B��B�B�!B�-B�UB��B��B��B�BB� B��B�'B�gB�B��B�zB�EB��B��B�KB��B��B��B��B�6B��B��B�NB�wB��B�B�BB��B�B��BȀB�^B�<BȀB�BȴB�B��B�QB�dB��BʌBΤBΤBϫBтB��B�5B�B�8B��B��BбB�|B�B�<B�HB��B�pB�BҽBӏBтB�,B��B�>BӏB�>B��B�
BקB��B��B��B��B�5B�vB�iBޞB�B�B��B�B�%B�B�`B�B��B�cB�WB�B�B�B�.B� B�.B�B�cB�B�B��B�+B��B�7B��B�B��B�rB �B�B��B��B��B��B��B�fB�lB�lB��B��B��B��B�1B��B�lB��B�1B��B�B�`B��B��B��B��B�%B�`B��B��B�MB��B��B�+B��B��B�+B��B��B�B�B�%B��B��B�MB�B�AB�oB�B�{B�B� B�B�B�B�B��B�B�lB�B�rB�B�B��B�iB��B�vB��B�NB�5B�NB�B�TB�GB�B�B�5B�B�GB��B�)B�5B��B��B�/B�/B�#B��BܒB�]BۋBںB��B��BڅB�WB�KB��B�B�B��B�>B֡B��B��B��B��B��B��BɺB��B�B��BҽB�B��B��BтB�BB�B�WB�WB˒B��B��B��B��BɺB�)B��B�gB�B��B�&BBÕB��BB��B��B�<B��B�HB�)B��B��B��B��B��B�'B�[B�[B��B��B�9B��B�^B��B��B��B�'B��B��B�YB��B�4B�B��BbB~�B|By�Bw1By�Bz�Bv�BqBsBo�Bm�BiyBl�BiyBg�BkBbB^�Bh�B}�BaGBW
BT,BT�B]/BT�BW
BYKBZ�B_;BU�BT�BU�BV8BP�BOBBP�BNBL�BO�BaBOBBEBK�BK^BMjB8�B/�B2�B.�B)�B)*B&�B($B#�B"�B �B#�B!�B [B�BB=B�B�BB�B�B�B�B�B�B�B�B�BBIB~B�B
�B
�BBxB�B$tB��B��B�BܒB��B��B��B�B� B�
B�8B՛B�yBӏBтB�HB�HB�B�5B�vBקBɺB��B��B�KBÕB�3B�9BȴB�B��B��B�#B�<B�B�-B�[B�'B�UB�wB��B�EB��B�UB�_B�YB��B��B��B�B�B��B�B�BbBglBh�Be,B`vBb�Bb�Bb�Bc�BbBaB`vBa�Ba|BaGBe�Bw1Bf2Bb�B\�B^B^�B_�B]cB\�BY�B[�BX�BY�BQBIQBMjBM�BK^BEBB�BAUBK)BB&B>�B<6B4�B49B4�B=�B/OB33B(XB*0B)�B($B'RB!�B%�B,B�B�BB�B�B�B�BB�B\B�BVB�B�B�B�BxBCB~BB�B1BeBeBeBMB@B:BBB �B
�(B  B
��B�B
�xB
�`B
��B
��B
��B
�`B
�5B
�(B
�{B
�>B
�%B
�lB
�|B
�B
�ZG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                     G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230721225010                            20230721225010AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072122501020230721225010  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122501020230721225010QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122501020230721225010QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               