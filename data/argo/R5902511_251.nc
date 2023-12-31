CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-21T22:49:52Z creation      
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
resolution        =���   axis      Z        `  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  S@   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  Y   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  px   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     `  vP   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �H   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  ߀   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     `  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �    PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     ` �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` 1P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   1�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   7�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   =�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T C�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   D   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   D   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � D$   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   D�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   D�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    D�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        D�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        D�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       D�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    E Argo profile    3.1 1.2 19500101000000  20230721224952  20230721224952  5902511 5902511 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810                            6810                            2B  2B  AA  SOLO_II                         SOLO_II                         8521                            8521                            SBE602 27Jul16                  SBE602 27Jul16                  853 853 @��V��@��V��11  @���J�@���J�@2(�z@2(�z�d�O�M�d�O�M11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 BA  BA  FF  ?��@   @=p�@}p�@�  @��R@޸R@��RA\)A\)A,��A?\)A^�RA�Q�A�Q�A��A�  A��AϮA�  A�  A��B�
B  B�
B�
B'�
B/�
B7�
B?�
BH  BP(�BX(�B`(�Bg�
Bp(�Bx(�B�  B��B�{B�  B��B��B�  B��B�  B�  B�  B�{B�  B��B��B��B��B�  B�  B�{B�  B��
B�  B��B�  B�{B�  B�  B��B�  B�  B�  C   C{C��C��C
=C
  C�C
=C��C��C��C
=C��C��C��C��C {C"{C$
=C&
=C(  C*
=C,  C.  C0  C2  C3��C6  C8
=C:  C<  C>  C?��CA�CD
=CF
=CG�CJ  CK��CN  CP  CQ��CS��CU��CX  CZ{C[��C^
=C`  Cb  Cd
=Cf
=Ch  Ci��Cl
=Cn{Cp{Cr
=Ct  Cv
=Cx
=Cz
=C|
=C~{C�  C���C���C���C���C���C�  C�C�  C���C�  C���C���C���C�  C�  C���C���C�  C���C���C���C���C�  C�  C�C�  C�  C�  C�  C�  C���C���C�  C�  C�  C�C�C�  C���C���C���C���C�  C�  C�  C�  C���C���C�  C�C�  C���C�  C���C���C���C�  C���C�  C�  C�  C�  C���C�C�  C���C�C�C�  C�
=C�
=C�C�C���C���C���C�C�
=C�C�  C���C���C���C���C���C���C���C�  C�C�C�  C�  C�C�  C�C�C�  C�  C�C�  C�  C�C�C�  C���C�  C���C�  C�C�C�  C���C���C�C���C���C�C�
=C�  C�  C�  C�C�  C���C���C�  C�D   D }qD �qD}qD�qD� D�D}qD  D�D�D� D�qD� DD� D��D}qD	  D	� D	�qD
}qD  D��D  Dz�D��D��D  D}qD�qD��DD��D�D��D  D��D  D}qD�qDz�D��D}qD  D��D  D}qD  D� D  D�D�D��D�D��D  D� D�qD� D  D� D  D}qD   D � D!�D!��D"�D"}qD"�qD#��D$�D$� D%�D%}qD&  D&� D'  D'��D(�D(��D)  D)��D*�D*z�D*��D+��D,  D,� D,�qD-}qD-��D.� D/�D/��D0�D0� D1  D1��D2�D2}qD3  D3��D3�qD4z�D5  D5��D6�D6}qD6�qD7� D7�qD8}qD8�qD9� D:  D:}qD:�qD;}qD<�D<� D<�qD=� D=�qD>� D?�D?}qD@  D@��DADA��DB  DB� DB�qDC}qDC��DDz�DD��DE� DF�DF��DG  DG� DH�DH� DI  DI� DJ�DJ� DK�DK� DL  DL� DM  DM� DM��DN}qDO�DO� DO�qDP� DQ�DQ��DR  DR}qDR�qDS}qDS�qDT� DU  DU� DV  DV� DWDW�DX  DX��DYDY��DZ�DZ��DZ�qD[}qD\D\��D\��D]z�D^  D^��D_  D_� D_�qD`��Da�Daz�Da�qDb� Dc�Dc�DdDd}qDd�qDe�DfDf�Dg�Dg��Dh�Dh��Dh��Di� Dj�Dj�Dk  Dk}qDl�Dl��Dm  Dm� Dm�qDn��Do�Do}qDo�qDp� Dq  Dq}qDq�qDr� Ds�Ds��Dt  Dtz�Du  Du}qDu�qDv��Dw  Dw� DxDx��Dx�qDy}qDz  Dz}qDz�qD{� D|  D|�D}D}� D~  D~��D  D��D��D�@ D�� D��HD��D�AHD�~�D���D�  D�@ D�� D��HD���D�>�D�}qD��qD��qD�=qD�}qD���D���D�@ D���D��HD�  D�@ D��HD��HD�HD�AHD�� D�� D�  D�@ D��HD�� D��qD�>�D��HD��HD�  D�AHD�� D��qD���D�@ D�~�D�� D�HD�AHD��HD��HD�  D�AHD��HD��HD�fD�>�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��HD�HD�AHD�� D��HD�HD�>�D�� D��HD�  D�@ D��HD�� D�  D�AHD�� D�� D�HD�AHD�� D�� D�  D�@ D�� D��HD�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�?L��?��?���?���?��H@�@z�@.{@@  @L��@aG�@u@�  @��@�33@��H@��
@�{@�@�p�@Ǯ@�33@��H@�\@���@�
=@��RA33A	��Ap�AG�AffA�A\)A#33A(��A.{A1G�A5A;�A?\)AB�\AHQ�AN{AQG�AU�AZ�HA_\)Ab�\Ag
=Amp�Ap��Atz�Az�HA~{A�G�A�(�A�ffA�  A��\A��A�\)A�G�A�33A�{A���A��\A���A��A���A��A�{A���A��\A���A�\)A���A��A�ffA���A��\A��A��A\A�(�AƸRAə�A˅A�AУ�A��HA���A׮A�=qA��
A�{A���A�33A���A�A�\A�(�A�{A���A��HA�z�A��A���A��A�{B Q�B ��BffB�
B��B��B
=B(�B	�B
�RB�
B��BB\)Bz�BG�BffB  B�B{B33B��B��B�\B(�BG�B=qB\)B ��B"=qB#33B$(�B%G�B&�HB(  B(��B*{B+�B,��B-��B.�RB0(�B1p�B2�\B3�B4z�B5�B733B8(�B8��B:�\B;�
B<z�B=��B?
=B@(�B@��BB=qBC�BD��BEBF�HBH(�BIp�BJffBK33BLz�BM�BN�RBO�BQ�BR=qBS
=BTQ�BU��BV�\BW�BX��BZ{BZ�HB[�
B]�B^ffB_\)B`(�BaG�Bb�\Bc�Bdz�Be��Bg
=Bg�
Bh��BjffBk
=Bl(�Bm��Bn�\Bo\)Bpz�BqBs
=Bs�
Bt��Bv{Bw\)Bx(�By�Bz�\B{�B|Q�B}��B~�HB�  B�ffB��HB�p�B�(�B���B���B���B�Q�B��RB�33B�B�z�B�
=B��B�  B���B�G�B�B�=qB���B��B�{B��\B�
=B��B�Q�B��RB�G�B��B��\B�
=B�p�B�(�B��RB��B�B�ffB��HB�\)B�(�B��RB��B��B�ffB��B���B�{B���B�p�B�  B�z�B�
=B��B�ffB��HB�p�B�(�B��HB�\)B��B��\B�G�B��B�z�B�
=B�B�z�B��B���B�=qB���B��B�(�B���B�p�B�{B��\B�\)B�  B�ffB�
=B��
B�ffB���B��B�(�B���B�\)B��
B�ffB��B�B�=qB���B�p�B�(�B���B�33B��
B��\B�G�B�B�Q�B�
=B��
B�=qB���B�B�=qB���B���B�Q�B���B�\)B�=qB��HB�p�B�  Bƣ�B�p�B�{Bȣ�B��B�  Bʣ�B��BˮB�z�B�
=BͅB�(�B��HBϮB�(�B���B�G�B�  B���B�G�B�B�ffB�
=B�B�z�B��HB�p�B�{BظRB�\)B��B�Q�B��HB�\)B�  Bܣ�B�G�B�B�=qB޸RB�G�B��B�z�B��BᙚB�  B�z�B�33B�B�=qB���B�G�B�B�Q�B���B�B�=qB��HB�\)B��
B�ffB���B�B�(�B�RB�G�B�B�Q�B��HB�\)B�  B��B�33B��
B�z�B�
=B�B�Q�B���B��B�=qB���B�G�B��
B�Q�B���B��B�(�B���B�\)B�  B��RB�G�B��
B�ffB��HB�p�C   C G�C �C ��C�Cp�CC
=C\)C�\C�
C{C\)C�C  C\)C�RC  CG�C�C�
C33C�C�
C(�Cz�CC{CQ�C��C��C	G�C	��C
  C
\)C
�C  CG�C��C�C=qC�\C�CG�C��C��CG�C��C�HC33C�C�
C33C�\C�HC=qC��C��CQ�C�C
=CQ�C�C  CffCC(�C�C�
C33C�\C�HC33C�\C�HC33C�C�HC33C�\C�HC=qC�\C�HCG�C��C��CQ�C�C  C\)C�C
=Cp�CC(�C�C�
C (�C z�C �
C!�C!p�C!�RC"  C"G�C"��C"�
C#�C#\)C#��C#�HC$�C$ffC$��C$�
C%
=C%=qC%p�C%��C%��C&
=C&33C&ffC&�C&�C&�HC'{C'G�C'p�C'��C'C'�C({C(G�C(z�C(��C(�
C)
=C)=qC)p�C)��C)��C*  C*33C*ffC*��C*��C+  C+(�C+ffC+�\C+C+��C,�C,Q�C,�\C,C,��C-(�C-ffC-�\C-�RC-�C.�C.Q�C.�\C.C.�C/�C/Q�C/�C/�C/�HC0{C0=qC0p�C0��C0�
C1  C133C1\)C1�C1�C1�
C2
=C2(�C2Q�C2�C2�C2��C3  C3(�C3Q�C3�C3�C3��C4  C4(�C4Q�C4�C4�C4�
C5
=C533C5ffC5�\C5C5�C6{C6=qC6z�C6��C6��C7  C733C7ffC7�\C7�C7�HC8{C8=qC8p�C8��C8C8��C9(�C9Q�C9�C9�C9�HC:
=C:33C:ffC:��C:C:��C;�C;Q�C;�C;�RC;�C<�C<\)C<�\C<�RC<�C=�C=G�C=z�C=�C=�HC>
=C>33C>ffC>�\C>C>��C?(�C?Q�C?z�C?��C?C?��C@(�C@\)C@�\C@�RC@�CA�CAQ�CAz�CA��CA��CB  CB(�CB\)CB�\CBCC  CC=qCCp�CC��CC��CD  CD(�CDQ�CD�CD�RCD�CE{CE=qCEz�CE�CE�HCF�CFQ�CFz�CF�CF�HCG
=CG(�CG\)CG�CG�CG�HCH{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113333                                                                                                                                          1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?��@   @=p�@}p�@�  @��R@޸R@��RA\)A\)A,��A?\)A^�RA�Q�A�Q�A��A�  A��AϮA�  A�  A��B�
B  B�
B�
B'�
B/�
B7�
B?�
BH  BP(�BX(�B`(�Bg�
Bp(�Bx(�B�  B��B�{B�  B��B��B�  B��B�  B�  B�  B�{B�  B��B��B��B��B�  B�  B�{B�  B��
B�  B��B�  B�{B�  B�  B��B�  B�  B�  C   C{C��C��C
=C
  C�C
=C��C��C��C
=C��C��C��C��C {C"{C$
=C&
=C(  C*
=C,  C.  C0  C2  C3��C6  C8
=C:  C<  C>  C?��CA�CD
=CF
=CG�CJ  CK��CN  CP  CQ��CS��CU��CX  CZ{C[��C^
=C`  Cb  Cd
=Cf
=Ch  Ci��Cl
=Cn{Cp{Cr
=Ct  Cv
=Cx
=Cz
=C|
=C~{C�  C���C���C���C���C���C�  C�C�  C���C�  C���C���C���C�  C�  C���C���C�  C���C���C���C���C�  C�  C�C�  C�  C�  C�  C�  C���C���C�  C�  C�  C�C�C�  C���C���C���C���C�  C�  C�  C�  C���C���C�  C�C�  C���C�  C���C���C���C�  C���C�  C�  C�  C�  C���C�C�  C���C�C�C�  C�
=C�
=C�C�C���C���C���C�C�
=C�C�  C���C���C���C���C���C���C���C�  C�C�C�  C�  C�C�  C�C�C�  C�  C�C�  C�  C�C�C�  C���C�  C���C�  C�C�C�  C���C���C�C���C���C�C�
=C�  C�  C�  C�C�  C���C���C�  C�D   D }qD �qD}qD�qD� D�D}qD  D�D�D� D�qD� DD� D��D}qD	  D	� D	�qD
}qD  D��D  Dz�D��D��D  D}qD�qD��DD��D�D��D  D��D  D}qD�qDz�D��D}qD  D��D  D}qD  D� D  D�D�D��D�D��D  D� D�qD� D  D� D  D}qD   D � D!�D!��D"�D"}qD"�qD#��D$�D$� D%�D%}qD&  D&� D'  D'��D(�D(��D)  D)��D*�D*z�D*��D+��D,  D,� D,�qD-}qD-��D.� D/�D/��D0�D0� D1  D1��D2�D2}qD3  D3��D3�qD4z�D5  D5��D6�D6}qD6�qD7� D7�qD8}qD8�qD9� D:  D:}qD:�qD;}qD<�D<� D<�qD=� D=�qD>� D?�D?}qD@  D@��DADA��DB  DB� DB�qDC}qDC��DDz�DD��DE� DF�DF��DG  DG� DH�DH� DI  DI� DJ�DJ� DK�DK� DL  DL� DM  DM� DM��DN}qDO�DO� DO�qDP� DQ�DQ��DR  DR}qDR�qDS}qDS�qDT� DU  DU� DV  DV� DWDW�DX  DX��DYDY��DZ�DZ��DZ�qD[}qD\D\��D\��D]z�D^  D^��D_  D_� D_�qD`��Da�Daz�Da�qDb� Dc�Dc�DdDd}qDd�qDe�DfDf�Dg�Dg��Dh�Dh��Dh��Di� Dj�Dj�Dk  Dk}qDl�Dl��Dm  Dm� Dm�qDn��Do�Do}qDo�qDp� Dq  Dq}qDq�qDr� Ds�Ds��Dt  Dtz�Du  Du}qDu�qDv��Dw  Dw� DxDx��Dx�qDy}qDz  Dz}qDz�qD{� D|  D|�D}D}� D~  D~��D  D��D��D�@ D�� D��HD��D�AHD�~�D���D�  D�@ D�� D��HD���D�>�D�}qD��qD��qD�=qD�}qD���D���D�@ D���D��HD�  D�@ D��HD��HD�HD�AHD�� D�� D�  D�@ D��HD�� D��qD�>�D��HD��HD�  D�AHD�� D��qD���D�@ D�~�D�� D�HD�AHD��HD��HD�  D�AHD��HD��HD�fD�>�D�� D�� D�  D�@ D�� D�� D�  D�@ D�� D��HD�HD�AHD�� D��HD�HD�>�D�� D��HD�  D�@ D��HD�� D�  D�AHD�� D�� D�HD�AHD�� D�� D�  D�@ D�� D��HD�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�>�?L��?��?���?���?��H@�@z�@.{@@  @L��@aG�@u@�  @��@�33@��H@��
@�{@�@�p�@Ǯ@�33@��H@�\@���@�
=@��RA33A	��Ap�AG�AffA�A\)A#33A(��A.{A1G�A5A;�A?\)AB�\AHQ�AN{AQG�AU�AZ�HA_\)Ab�\Ag
=Amp�Ap��Atz�Az�HA~{A�G�A�(�A�ffA�  A��\A��A�\)A�G�A�33A�{A���A��\A���A��A���A��A�{A���A��\A���A�\)A���A��A�ffA���A��\A��A��A\A�(�AƸRAə�A˅A�AУ�A��HA���A׮A�=qA��
A�{A���A�33A���A�A�\A�(�A�{A���A��HA�z�A��A���A��A�{B Q�B ��BffB�
B��B��B
=B(�B	�B
�RB�
B��BB\)Bz�BG�BffB  B�B{B33B��B��B�\B(�BG�B=qB\)B ��B"=qB#33B$(�B%G�B&�HB(  B(��B*{B+�B,��B-��B.�RB0(�B1p�B2�\B3�B4z�B5�B733B8(�B8��B:�\B;�
B<z�B=��B?
=B@(�B@��BB=qBC�BD��BEBF�HBH(�BIp�BJffBK33BLz�BM�BN�RBO�BQ�BR=qBS
=BTQ�BU��BV�\BW�BX��BZ{BZ�HB[�
B]�B^ffB_\)B`(�BaG�Bb�\Bc�Bdz�Be��Bg
=Bg�
Bh��BjffBk
=Bl(�Bm��Bn�\Bo\)Bpz�BqBs
=Bs�
Bt��Bv{Bw\)Bx(�By�Bz�\B{�B|Q�B}��B~�HB�  B�ffB��HB�p�B�(�B���B���B���B�Q�B��RB�33B�B�z�B�
=B��B�  B���B�G�B�B�=qB���B��B�{B��\B�
=B��B�Q�B��RB�G�B��B��\B�
=B�p�B�(�B��RB��B�B�ffB��HB�\)B�(�B��RB��B��B�ffB��B���B�{B���B�p�B�  B�z�B�
=B��B�ffB��HB�p�B�(�B��HB�\)B��B��\B�G�B��B�z�B�
=B�B�z�B��B���B�=qB���B��B�(�B���B�p�B�{B��\B�\)B�  B�ffB�
=B��
B�ffB���B��B�(�B���B�\)B��
B�ffB��B�B�=qB���B�p�B�(�B���B�33B��
B��\B�G�B�B�Q�B�
=B��
B�=qB���B�B�=qB���B���B�Q�B���B�\)B�=qB��HB�p�B�  Bƣ�B�p�B�{Bȣ�B��B�  Bʣ�B��BˮB�z�B�
=BͅB�(�B��HBϮB�(�B���B�G�B�  B���B�G�B�B�ffB�
=B�B�z�B��HB�p�B�{BظRB�\)B��B�Q�B��HB�\)B�  Bܣ�B�G�B�B�=qB޸RB�G�B��B�z�B��BᙚB�  B�z�B�33B�B�=qB���B�G�B�B�Q�B���B�B�=qB��HB�\)B��
B�ffB���B�B�(�B�RB�G�B�B�Q�B��HB�\)B�  B��B�33B��
B�z�B�
=B�B�Q�B���B��B�=qB���B�G�B��
B�Q�B���B��B�(�B���B�\)B�  B��RB�G�B��
B�ffB��HB�p�C   C G�C �C ��C�Cp�CC
=C\)C�\C�
C{C\)C�C  C\)C�RC  CG�C�C�
C33C�C�
C(�Cz�CC{CQ�C��C��C	G�C	��C
  C
\)C
�C  CG�C��C�C=qC�\C�CG�C��C��CG�C��C�HC33C�C�
C33C�\C�HC=qC��C��CQ�C�C
=CQ�C�C  CffCC(�C�C�
C33C�\C�HC33C�\C�HC33C�C�HC33C�\C�HC=qC�\C�HCG�C��C��CQ�C�C  C\)C�C
=Cp�CC(�C�C�
C (�C z�C �
C!�C!p�C!�RC"  C"G�C"��C"�
C#�C#\)C#��C#�HC$�C$ffC$��C$�
C%
=C%=qC%p�C%��C%��C&
=C&33C&ffC&�C&�C&�HC'{C'G�C'p�C'��C'C'�C({C(G�C(z�C(��C(�
C)
=C)=qC)p�C)��C)��C*  C*33C*ffC*��C*��C+  C+(�C+ffC+�\C+C+��C,�C,Q�C,�\C,C,��C-(�C-ffC-�\C-�RC-�C.�C.Q�C.�\C.C.�C/�C/Q�C/�C/�C/�HC0{C0=qC0p�C0��C0�
C1  C133C1\)C1�C1�C1�
C2
=C2(�C2Q�C2�C2�C2��C3  C3(�C3Q�C3�C3�C3��C4  C4(�C4Q�C4�C4�C4�
C5
=C533C5ffC5�\C5C5�C6{C6=qC6z�C6��C6��C7  C733C7ffC7�\C7�C7�HC8{C8=qC8p�C8��C8C8��C9(�C9Q�C9�C9�C9�HC:
=C:33C:ffC:��C:C:��C;�C;Q�C;�C;�RC;�C<�C<\)C<�\C<�RC<�C=�C=G�C=z�C=�C=�HC>
=C>33C>ffC>�\C>C>��C?(�C?Q�C?z�C?��C?C?��C@(�C@\)C@�\C@�RC@�CA�CAQ�CAz�CA��CA��CB  CB(�CB\)CB�\CBCC  CC=qCCp�CC��CC��CD  CD(�CDQ�CD�CD�RCD�CE{CE=qCEz�CE�CE�HCF�CFQ�CFz�CF�CF�HCG
=CG(�CG\)CG�CG�CG�HCH{1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113333                                                                                                                                          1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A��A�  A��A˴9AˬAˮA˰!AˬAˮA˶FA˶FA˺^A�ƨA�ȴA˼jA˙�A�p�A�^5A�XA�VA�Q�A�VA�VA�K�A�K�A�G�A�?}A�9XA�9XA�1'A�-A� �A�{A�bA�A���A���AʸRAʣ�Aʛ�Aʡ�AʬAʴ9Aʏ\A�"�A� �A�bA���Aț�Aǡ�A�E�A�JA��uA�9XA�&�A���A��
A��PA�I�A���A���A�/A��mA�?}A���A�
=A�jA��A�ĜA��7A���A���A��A�C�A�ƨA�$�A�+AVA~��A~A}+A{?}Ay�TAy��Ax�Avv�At�jAq�Aml�Ai��Ae
=AdȴAd�\AcAb��A^jAZZAW��AS�AR1AQ�AM�AH�!AHZAG��AF�ADQ�AC
=AA��A@=qA?p�A>ȴA>n�A=7LA<ffA;K�A:z�A9?}A7�A6�RA5�;A5��A5`BA4��A3�A3/A2��A2 �A0A.�DA-�wA-&�A,�\A+��A+A*-A'�hA'A&�!A%XA$�yA$�RA$I�A#\)A"��A"~�A!��A �A�#A�PA+A�A�;A��A  A\)A�A��A5?A��A��A��A(�A��AO�Al�AE�A�A{A�A�RA$�A
=A	ƨA	dZA	oA�/A�AA��AA�A\)A�uAjA|�A��A7LA �@�M�@��H@�=q@�p�@�\)@��R@�{@�hs@�bN@�@���@��@�@�\@�=q@�G�@�C�@�hs@��@�F@�R@�9@��@�w@�"�@��@�~�@�E�@�{@��#@�@�%@��@�r�@��@�@�@�P@��@�~�@�E�@��@��@�&�@��u@�33@�~�@�5?@�@ݙ�@��@��`@��/@���@ܛ�@ܼj@�&�@݁@�X@�&�@�Z@�V@�/@ܬ@�1@ە�@��@��@���@�{@ٲ-@�r�@�J@Ұ!@�O�@д9@Л�@�z�@�Z@��
@��@�=q@�x�@�bN@�1'@�9X@�b@�l�@ʏ\@�J@���@�7L@ȋD@�(�@�|�@�;d@���@�V@�`B@���@��@��`@���@�V@Ĭ@�33@�@�5?@�`B@��/@���@�z�@�Z@�Z@�bN@�A�@� �@���@��@��P@�|�@��@���@��@���@�~�@�M�@��h@���@�(�@��@���@��@�p�@��7@�hs@�%@��j@�Z@�1@��m@���@�ƨ@��@�\)@�"�@��@�n�@�{@�x�@�/@��j@��m@���@�C�@���@�{@��-@�x�@�z�@�b@��m@��@�\)@�"�@�v�@�=q@��@���@�hs@�7L@���@���@��j@��@�(�@��;@��;@��m@��@�  @���@��
@���@���@�t�@��@��H@��@���@�-@�G�@���@���@�Ĝ@��9@���@�z�@�r�@��D@��@���@��@�bN@�ƨ@���@�+@�ȴ@��!@��\@�-@��-@�O�@�V@��9@�z�@�A�@��@���@�dZ@�"�@��@��+@��^@��7@�`B@���@��@�9X@��w@��H@�v�@�=q@���@�/@���@��`@���@�bN@���@���@�S�@�+@��@�ȴ@���@�^5@��^@��@�Ĝ@��u@��u@��@�9X@�1@�l�@�\)@�"�@�@��H@��\@�V@��@��-@�x�@�G�@��@���@���@��D@� �@��
@�S�@�"�@��R@�^5@���@�@�p�@�&�@��/@���@��@��u@�r�@�Z@�Q�@�I�@�1'@��@�t�@���@���@���@�ȴ@�ȴ@�V@��^@��-@���@�7L@�%@��j@�(�@���@�C�@�C�@�K�@�+@��y@���@�V@��@�x�@�&�@���@���@���@��D@�j@�I�@��@��;@��F@���@��P@��@�|�@�t�@�l�@�S�@���@��@�x�@�&�@���@��`@���@���@��u@��u@���@���@��u@��D@�z�@� �@��@�K�@���@��@�~�@�-@�@��#@�@��h@�p�@�`B@�&�@��/@���@��@�z�@�r�@�bN@� �@��@\)@~��@~��@~{@}�@|Z@|�@{�F@{t�@z��@z=q@y��@y%@xbN@xb@x  @xQ�@x1'@x  @w��@wK�@v�y@v�+@v$�@u�-@up�@t��@tI�@s�m@s�F@s"�@r=q@q��@qG�@p�9@p1'@o�@o�@o�P@o;d@n�R@n5?@m�h@l��@lj@k��@k��@kdZ@k"�@k@j�H@j��@j-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�"�A��A��A�{A�bA��A��A�"�A��A���A��mA��A��A��A��yA���A˴9A˰!A˲-AˮA˧�AˬA˰!A˰!AˮA˰!A˲-A˰!AˮA˰!AˬA˩�AˬAˮAˬA˰!A˰!A˺^A˸RA˴9A˸RA˶FA˴9A˸RA˼jA˺^A˸RA˺^A˼jA˸RA˺^A˾wA˼jA˾wA�ƨA�ƨA�ƨA���A���A�ȴA�ȴA���A���A�ȴA�ȴA�ƨA�ȴA�A���A�A˺^A˲-A˲-A˰!A˧�A˛�A˝�A˙�A˓uA˅A˃A�x�A�r�A�p�A�n�A�hsA�dZA�hsA�^5A�^5A�^5A�\)A�VA�XA�\)A�XA�VA�ZA�VA�S�A�VA�XA�S�A�Q�A�VA�VA�S�A�VA�VA�O�A�Q�A�S�A�O�A�O�A�S�A�O�A�O�A�VA�VA�S�A�ZA�\)A�\)A�ZA�\)A�XA�M�A�K�A�M�A�M�A�I�A�I�A�M�A�I�A�I�A�M�A�M�A�K�A�I�A�K�A�M�A�K�A�G�A�I�A�K�A�I�A�G�A�E�A�E�A�G�A�A�A�A�A�A�A�?}A�;dA�5?A�5?A�;dA�=qA�9XA�7LA�;dA�=qA�9XA�9XA�;dA�;dA�7LA�5?A�7LA�5?A�-A�/A�1'A�1'A�/A�-A�/A�1'A�-A�-A�1'A�-A�(�A�(�A�(�A�"�A��A� �A��A��A�{A��A��A�{A�bA�oA�oA�oA�VA�bA�{A�bA�VA�oA�JA�A�%A�%A�A�A�%A�%A�  A�  A�A�A���A���A���A���A��A��`A��
A���A���A�ƨA�A�ĜA���Aʺ^AʸRAʺ^Aʲ-AʮAʬAʬAʬAʧ�Aʝ�Aʛ�Aʝ�Aʛ�Aʗ�Aʙ�Aʝ�Aʝ�Aʙ�Aʗ�Aʟ�Aʣ�Aʟ�Aʝ�Aʡ�Aʥ�Aʡ�Aʟ�Aʣ�Aʧ�Aʧ�AʬAʰ!AʮAʮAʸRA���Aʺ^Aʲ-Aʴ9Aʰ!Aʧ�Aʣ�Aʡ�Aʛ�AʍPAʇ+A�~�A�z�A�|�A�jA�I�A�C�A��AɶFA�\)A�-A�&�A�"�A��A��A��A��A��A�bA�JA�JA�JA�A�  A�A���A��A��mA���A���A�AȸRAȩ�A�z�A�I�A�9XA�  A��
Aǝ�A�ZA�C�A�"�A���AƓuA�jA�?}A�oA��TA�l�A�;dAÕ�A�Q�A¬A�A���A���A��FA��DA�?}A�VA���A��A��jA�hsA�1A��
A���A���A��+A�ZA��A��yA���A��A��A�bNA�S�A��A��#A��jA�|�A�ffA�C�A���A���A���A�p�A�E�A�A��hA��A���A���A��A���A�n�A�5?A���A���A��A�G�A��A���A�x�A�\)A�C�A�+A�JA���A��A�r�A���A���A��A���A�33A���A���A�K�A���A�E�A�A�A�t�A��uA���A���A�9XA���A��A�~�A�S�A�C�A�5?A�/A��A�  A���A�A�hsA�t�A���A��7A�?}A��A�bA�JA�A�  A���A��mA��
A��!A��A�l�A��A�ȴA��-A���A���A���A��A�ffA��A�"�A��yA���A���A�E�A���A�%A���A�dZA��A�v�A�&�A��A��!A��DA� �A�$�A��!A�/A���A��-A�^5A��A�JA�A�O�A���A��A�n�A��FA��A��PA�A�A�A��PA�ƨA�33A��DA�1'A�p�A��TA���A��jA��^A��A�VA���A�\)A�VA��A���A���A�x�A�$�A�=qA��A��A��AdZA"�A
=A~�A~��A~��A~�9A~�9A~�A~��A~�uA~�A~ �A~�A}�A}��A}�TA}�;A}�wA}ƨA}XA{�TA{�mA{��A{��A{��Az�yAzjAzA�Az �Ay�Ay��AyƨAy�wAy�Ay��Ay��Ay��Ay�PAyl�Ay?}AxȴAx9XAw�mAw��AwdZAw/Av�AvĜAvjAv1'Av�Av  Au��Au�Au;dAuAt��At�\At1As�7AsK�As+Ar��Ar��Arn�Ar(�Ap�\Ap��ApZAp$�AoG�An�9AnffAn=qAm�Am�7AmO�Am�Al�Al�yAlȴAl�uAl=qAk�TAk�AkO�Aj��AjbNAi��AhM�Ag��Ag
=AfE�Ae�hAeC�Ae%Ad��Ad�Ad�HAd�HAd�/Ad�Ad�Ad��Ad�Ad�Ad��Ad��AdĜAd��Ad�jAd�RAd�RAd�9Ad�9Ad�9Ad�9Ad��Ad��Ad��Ad�\Adr�Ad=qAd$�Ad{Ac��Ac�;Ac��Ac�wAc�-Ac��Ac��Ac��Ac��Ac��Ac��Ac��Ac��Ac�hAc�Ac�Ac\)Ab�yAb�\AbM�AbJAa��Aax�A`�`A_�;A_�FA_VA^�9A^�DA]�
A]�A\1'A[�A[�#A[��A[�wA[��A[O�AZ�AZjAY�
AY33AX��AX��AX��AX��AX�RAX�9AX�!AX��AXn�AX$�AW�#AV�/AU��ATr�AT1AS��AS\)AS&�AR�AR�AR��AR�!AR��AR��AR�DARv�ARffARQ�AR5?AR1AQ��AQ�AQ�hAQ�AQt�AQdZAQK�AQG�AQ?}AQ+AQoAQVAQ
=AP��AP�AP��APz�APA�AO��AO+AN �AMx�AL��AK�#AKAJAIS�AI&�AH�AHĜAH��AH�\AH�AH~�AHv�AHn�AHn�AHn�AHjAHjAHjAHjAHffAHbNAHZAH=qAH$�AG��AG��AG��AG��AG��AG��AG��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113333                                                                                                                                          1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A��A��A�  A��A˴9AˬAˮA˰!AˬAˮA˶FA˶FA˺^A�ƨA�ȴA˼jA˙�A�p�A�^5A�XA�VA�Q�A�VA�VA�K�A�K�A�G�A�?}A�9XA�9XA�1'A�-A� �A�{A�bA�A���A���AʸRAʣ�Aʛ�Aʡ�AʬAʴ9Aʏ\A�"�A� �A�bA���Aț�Aǡ�A�E�A�JA��uA�9XA�&�A���A��
A��PA�I�A���A���A�/A��mA�?}A���A�
=A�jA��A�ĜA��7A���A���A��A�C�A�ƨA�$�A�+AVA~��A~A}+A{?}Ay�TAy��Ax�Avv�At�jAq�Aml�Ai��Ae
=AdȴAd�\AcAb��A^jAZZAW��AS�AR1AQ�AM�AH�!AHZAG��AF�ADQ�AC
=AA��A@=qA?p�A>ȴA>n�A=7LA<ffA;K�A:z�A9?}A7�A6�RA5�;A5��A5`BA4��A3�A3/A2��A2 �A0A.�DA-�wA-&�A,�\A+��A+A*-A'�hA'A&�!A%XA$�yA$�RA$I�A#\)A"��A"~�A!��A �A�#A�PA+A�A�;A��A  A\)A�A��A5?A��A��A��A(�A��AO�Al�AE�A�A{A�A�RA$�A
=A	ƨA	dZA	oA�/A�AA��AA�A\)A�uAjA|�A��A7LA �@�M�@��H@�=q@�p�@�\)@��R@�{@�hs@�bN@�@���@��@�@�\@�=q@�G�@�C�@�hs@��@�F@�R@�9@��@�w@�"�@��@�~�@�E�@�{@��#@�@�%@��@�r�@��@�@�@�P@��@�~�@�E�@��@��@�&�@��u@�33@�~�@�5?@�@ݙ�@��@��`@��/@���@ܛ�@ܼj@�&�@݁@�X@�&�@�Z@�V@�/@ܬ@�1@ە�@��@��@���@�{@ٲ-@�r�@�J@Ұ!@�O�@д9@Л�@�z�@�Z@��
@��@�=q@�x�@�bN@�1'@�9X@�b@�l�@ʏ\@�J@���@�7L@ȋD@�(�@�|�@�;d@���@�V@�`B@���@��@��`@���@�V@Ĭ@�33@�@�5?@�`B@��/@���@�z�@�Z@�Z@�bN@�A�@� �@���@��@��P@�|�@��@���@��@���@�~�@�M�@��h@���@�(�@��@���@��@�p�@��7@�hs@�%@��j@�Z@�1@��m@���@�ƨ@��@�\)@�"�@��@�n�@�{@�x�@�/@��j@��m@���@�C�@���@�{@��-@�x�@�z�@�b@��m@��@�\)@�"�@�v�@�=q@��@���@�hs@�7L@���@���@��j@��@�(�@��;@��;@��m@��@�  @���@��
@���@���@�t�@��@��H@��@���@�-@�G�@���@���@�Ĝ@��9@���@�z�@�r�@��D@��@���@��@�bN@�ƨ@���@�+@�ȴ@��!@��\@�-@��-@�O�@�V@��9@�z�@�A�@��@���@�dZ@�"�@��@��+@��^@��7@�`B@���@��@�9X@��w@��H@�v�@�=q@���@�/@���@��`@���@�bN@���@���@�S�@�+@��@�ȴ@���@�^5@��^@��@�Ĝ@��u@��u@��@�9X@�1@�l�@�\)@�"�@�@��H@��\@�V@��@��-@�x�@�G�@��@���@���@��D@� �@��
@�S�@�"�@��R@�^5@���@�@�p�@�&�@��/@���@��@��u@�r�@�Z@�Q�@�I�@�1'@��@�t�@���@���@���@�ȴ@�ȴ@�V@��^@��-@���@�7L@�%@��j@�(�@���@�C�@�C�@�K�@�+@��y@���@�V@��@�x�@�&�@���@���@���@��D@�j@�I�@��@��;@��F@���@��P@��@�|�@�t�@�l�@�S�@���@��@�x�@�&�@���@��`@���@���@��u@��u@���@���@��u@��D@�z�@� �@��@�K�@���@��@�~�@�-@�@��#@�@��h@�p�@�`B@�&�@��/@���@��@�z�@�r�@�bN@� �@��@\)@~��@~��@~{@}�@|Z@|�@{�F@{t�@z��@z=q@y��@y%@xbN@xb@x  @xQ�@x1'@x  @w��@wK�@v�y@v�+@v$�@u�-@up�@t��@tI�@s�m@s�F@s"�@r=q@q��@qG�@p�9@p1'@o�@o�@o�P@o;d@n�R@n5?@m�h@l��@lj@k��@k��@kdZ@k"�@k@j�H@j��@j-G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�"�A��A��A�{A�bA��A��A�"�A��A���A��mA��A��A��A��yA���A˴9A˰!A˲-AˮA˧�AˬA˰!A˰!AˮA˰!A˲-A˰!AˮA˰!AˬA˩�AˬAˮAˬA˰!A˰!A˺^A˸RA˴9A˸RA˶FA˴9A˸RA˼jA˺^A˸RA˺^A˼jA˸RA˺^A˾wA˼jA˾wA�ƨA�ƨA�ƨA���A���A�ȴA�ȴA���A���A�ȴA�ȴA�ƨA�ȴA�A���A�A˺^A˲-A˲-A˰!A˧�A˛�A˝�A˙�A˓uA˅A˃A�x�A�r�A�p�A�n�A�hsA�dZA�hsA�^5A�^5A�^5A�\)A�VA�XA�\)A�XA�VA�ZA�VA�S�A�VA�XA�S�A�Q�A�VA�VA�S�A�VA�VA�O�A�Q�A�S�A�O�A�O�A�S�A�O�A�O�A�VA�VA�S�A�ZA�\)A�\)A�ZA�\)A�XA�M�A�K�A�M�A�M�A�I�A�I�A�M�A�I�A�I�A�M�A�M�A�K�A�I�A�K�A�M�A�K�A�G�A�I�A�K�A�I�A�G�A�E�A�E�A�G�A�A�A�A�A�A�A�?}A�;dA�5?A�5?A�;dA�=qA�9XA�7LA�;dA�=qA�9XA�9XA�;dA�;dA�7LA�5?A�7LA�5?A�-A�/A�1'A�1'A�/A�-A�/A�1'A�-A�-A�1'A�-A�(�A�(�A�(�A�"�A��A� �A��A��A�{A��A��A�{A�bA�oA�oA�oA�VA�bA�{A�bA�VA�oA�JA�A�%A�%A�A�A�%A�%A�  A�  A�A�A���A���A���A���A��A��`A��
A���A���A�ƨA�A�ĜA���Aʺ^AʸRAʺ^Aʲ-AʮAʬAʬAʬAʧ�Aʝ�Aʛ�Aʝ�Aʛ�Aʗ�Aʙ�Aʝ�Aʝ�Aʙ�Aʗ�Aʟ�Aʣ�Aʟ�Aʝ�Aʡ�Aʥ�Aʡ�Aʟ�Aʣ�Aʧ�Aʧ�AʬAʰ!AʮAʮAʸRA���Aʺ^Aʲ-Aʴ9Aʰ!Aʧ�Aʣ�Aʡ�Aʛ�AʍPAʇ+A�~�A�z�A�|�A�jA�I�A�C�A��AɶFA�\)A�-A�&�A�"�A��A��A��A��A��A�bA�JA�JA�JA�A�  A�A���A��A��mA���A���A�AȸRAȩ�A�z�A�I�A�9XA�  A��
Aǝ�A�ZA�C�A�"�A���AƓuA�jA�?}A�oA��TA�l�A�;dAÕ�A�Q�A¬A�A���A���A��FA��DA�?}A�VA���A��A��jA�hsA�1A��
A���A���A��+A�ZA��A��yA���A��A��A�bNA�S�A��A��#A��jA�|�A�ffA�C�A���A���A���A�p�A�E�A�A��hA��A���A���A��A���A�n�A�5?A���A���A��A�G�A��A���A�x�A�\)A�C�A�+A�JA���A��A�r�A���A���A��A���A�33A���A���A�K�A���A�E�A�A�A�t�A��uA���A���A�9XA���A��A�~�A�S�A�C�A�5?A�/A��A�  A���A�A�hsA�t�A���A��7A�?}A��A�bA�JA�A�  A���A��mA��
A��!A��A�l�A��A�ȴA��-A���A���A���A��A�ffA��A�"�A��yA���A���A�E�A���A�%A���A�dZA��A�v�A�&�A��A��!A��DA� �A�$�A��!A�/A���A��-A�^5A��A�JA�A�O�A���A��A�n�A��FA��A��PA�A�A�A��PA�ƨA�33A��DA�1'A�p�A��TA���A��jA��^A��A�VA���A�\)A�VA��A���A���A�x�A�$�A�=qA��A��A��AdZA"�A
=A~�A~��A~��A~�9A~�9A~�A~��A~�uA~�A~ �A~�A}�A}��A}�TA}�;A}�wA}ƨA}XA{�TA{�mA{��A{��A{��Az�yAzjAzA�Az �Ay�Ay��AyƨAy�wAy�Ay��Ay��Ay��Ay�PAyl�Ay?}AxȴAx9XAw�mAw��AwdZAw/Av�AvĜAvjAv1'Av�Av  Au��Au�Au;dAuAt��At�\At1As�7AsK�As+Ar��Ar��Arn�Ar(�Ap�\Ap��ApZAp$�AoG�An�9AnffAn=qAm�Am�7AmO�Am�Al�Al�yAlȴAl�uAl=qAk�TAk�AkO�Aj��AjbNAi��AhM�Ag��Ag
=AfE�Ae�hAeC�Ae%Ad��Ad�Ad�HAd�HAd�/Ad�Ad�Ad��Ad�Ad�Ad��Ad��AdĜAd��Ad�jAd�RAd�RAd�9Ad�9Ad�9Ad�9Ad��Ad��Ad��Ad�\Adr�Ad=qAd$�Ad{Ac��Ac�;Ac��Ac�wAc�-Ac��Ac��Ac��Ac��Ac��Ac��Ac��Ac��Ac�hAc�Ac�Ac\)Ab�yAb�\AbM�AbJAa��Aax�A`�`A_�;A_�FA_VA^�9A^�DA]�
A]�A\1'A[�A[�#A[��A[�wA[��A[O�AZ�AZjAY�
AY33AX��AX��AX��AX��AX�RAX�9AX�!AX��AXn�AX$�AW�#AV�/AU��ATr�AT1AS��AS\)AS&�AR�AR�AR��AR�!AR��AR��AR�DARv�ARffARQ�AR5?AR1AQ��AQ�AQ�hAQ�AQt�AQdZAQK�AQG�AQ?}AQ+AQoAQVAQ
=AP��AP�AP��APz�APA�AO��AO+AN �AMx�AL��AK�#AKAJAIS�AI&�AH�AHĜAH��AH�\AH�AH~�AHv�AHn�AHn�AHn�AHjAHjAHjAHjAHffAHbNAHZAH=qAH$�AG��AG��AG��AG��AG��AG��AG��1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111113333                                                                                                                                          1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
��B
�B
��B
�hB
��B
�hB
��B
��B
��B
��B
�$B
�jB
�'B
�EB
�BB
�gB
یB
�5B
�B
��B
�BB'RB9XB;dB>�B@BA�BCaBD�BEmBF�BF�BG�BG�BH�BK^BL�BN�BT�BncB��B�dB��B�+B!�B"�B#nB&B.B+B)*B�B��B�ZB�5B�vB�CB�VBp�BT�BMB
�AB
ϫB
�yB
��B
B
�B
��B
�JB
��B
gmB
^B
GEB
&LB
�B
�B	��B	��B	�B	�B	�B	��B	یB	�KB	�vB	ȀB	��B	��B	�1B	}�B	x8B	u�B	poB	j�B	\]B	NB	IB	0!B	(�B	!�B	�B	�B	oB�.B�VB�>B�MB�B�)B�KB��B��B��B��B�DB�B�B�]B�B�B�&B��B�B�B��BȴBŢB��B��B��B�B��B��B��B��B��B�eB�6B��B��B�LB��B��B�nB�B�4B�bB��B�IB��B�	B�=B��B��B��B��B�$B�MB��B�B�B��B�(B��B��B�lB��B�lB�fB�1B�fB��B�\B�bB��B��B�(B�hB�B��B�hB��B�VB�B�~B��B��B�xB�7B��B�B��B�B��B�bB��B�6B��B�B��B�kB�qB��B�$B�B�wB��BǮB�NBӏB�2B�QBܒB�pB�HB��B��B�B	 iB	�B	�B	B	�B	1B	qB	 �B	#:B	'�B	+�B	,�B	.}B	0!B	7�B	;�B	=�B	>BB	@�B	?}B	@�B	B[B	C�B	DgB	G�B	R B	[�B	]dB	^B	`BB	h�B	qvB	q�B	t�B	tB	tB	qAB	yrB	{�B	~(B	~�B	|�B	y>B	y>B	zDB	zxB	zDB	zB	z�B	{�B	|PB	}�B	�B	��B	�AB	��B	��B	�DB	��B	��B	��B	�B	�B	�"B	�"B	��B	�.B	��B	��B	��B	�uB	��B	��B	��B	��B	��B	�OB	��B	�:B	�tB	��B	�_B	��B	��B	�=B	��B	�CB	��B	��B	�'B	�3B	�B	��B	�B	��B	��B	�RB	��B	�6B	��B	�wB	�B	��B	��B	�B	��B	�-B	�gB	�gB	�3B	�B	��B	�B	ǮB	�B	ŢB	�tB	�?B	��B	�B	ȀB	�#B	�0B	�6B	��B	�<B	�<B	�BB	ҽB	ҽB	�TB	�&B	��B	��B	�B	�B	ںB	��B	��B	ݘB	�B	��B	ݘB	�dB	�BB	�B	�B	��B	� B	�B	�B	��B	�B	�B	�B	�`B	��B	�,B	�B	�B	�B	�>B	�B	�B	�KB	�B	�B	��B	�)B	��B	�B	�B	�GB	�B	�B	�ZB	��B	��B	��B	�`B	��B	��B	�fB	�lB	�lB	��B	�lB	�	B	��B	�B	�B	��B	�B	��B	�B	��B	�PB	��B	��B	�]B	��B	��B	��B	��B	��B	��B	��B
 4B
 �B
�B
�B
GB
B
�B
�B
�B
�B
�B
�B
1B
�B
1B
�B
�B

	B

	B

	B
	�B

=B

=B

rB
�B
�B
~B
�B
�B
"B
�B
\B
bB
�B
�B
hB
 B
�B
�B
:B
�B
B
uB
uB
�B
B
FB
{B
�B
MB
�B
_B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
qB
�B
�B
�B
�B
B
IB
~B
�B
!B
 'B
 �B
!�B
"4B
"4B
"4B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"�B
"hB
"hB
"hB
!�B
!�B
!�B
"hB
%�B
&B
%zB
%zB
%�B
%�B
%�B
%�B
&B
&LB
&LB
&LB
&B
%�B
&�B
'�B
'�B
(�B
(XB
)�B
*�B
*�B
*�B
*eB
*�B
+B
+B
,=B
-CB
.}B
.�B
/B
/OB
/�B
/�B
/OB
/�B
0!B
0UB
0UB
0�B
1[B
1'B
1�B
1�B
2�B
2�B
33B
3�B
4B
49B
4nB
5B
4�B
4�B
5B
5tB
5�B
6FB
6�B
6�B
7�B
7�B
8B
8B
8�B
9XB
9$B
9$B
9�B
:^B
:�B
:�B
:�B
;0B
;dB
;�B
;�B
<B
<B
<6B
<�B
=B
=<B
=qB
=�B
>B
>B
>BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
�B
�?B
��B
��B
��B
�-B
�LB
��B
��B
��B
��B
��B
��B
�3B
��B
�[B
�aB
��B
�UB
��B
��B
�aB
��B
��B
��B
�aB
�9B
��B
�nB
��B
�?B
�nB
�B
��B
�LB
�tB
�B
�XB
��B
�RB
��B
�*B
��B
�B
��B
�jB
�0B
�dB
��B
��B
��B
��B
��B
��B
B
�gB
��B
��B
�?B
��B
�9B
�B
�B
ȀB
�RB
��B
�B
��B
�B
�BB
�TB
� B
ѷB
��B
�mB
ԕB
�2B
֡B
خB
רB
ںB
��B
یB
�WB
�dB
�dB
��B
�5B
�B
�/B
�5B
�B
�pB
ޞB
�HB
��B
�B
�B
�B
�HB
�B
��B
��B
�|B
�B
��B
�B
��B
��B
�`B
�]BBBYBDB�B�B�B�B�BqB \B!�B!�B$@B+kB1�B3�B5B9�B:�B8�B:�B;�B:*B:^B;�B<jB;dB:�B<B=�B=�B=�B>wB?�B@�B>�B>BB@�BA B@�B@OB@OBA BAUB@�B@�BB'BC�BB'BA�BC�BB�BB'BB�BD3BEBC�BC�BFBEmBD3BC�BD�BE�BEBD3BE�BF?BDgBE9BGBFBE�BF�BHKBE�BFBG�BG�BF�BE�BGEBHBGEBFtBF�BHKBHBFtBG�BH�BF�BH�BIRBGBGEBH�BH�BGzBF�BH�BIBG�BGEBH�BH�BHBHBK)BK)BL�BI�BK)BLdBK^BJXBK^BM6BL�BK^BN<BM�BN�BM�BLdBNpBPHBOBBNBQBR�BR�BQ�BTaBV�BW�BYKBdZBkBl"BkQBm�Bw2Bx�B}�B��B�CB��B��B�RB�aB��B��B�zB�0B�TB�mB��B�dB�B�,B�8B��B�>B��B��B�B�B�KB�+B�B#:B �B!B �B"�B#�B"�B!bB"hB#�B$B#nB"4B$tB#�B!�B#B#B%�B"hB#:B#nB!�B#�B,qB-B(�B0�B+�B9XB-�B+�B*0B2-B*eB&�B)_B%�B+6B4B6FB �BB,�B!�B	B:�B�B��BB�JB�B�TB�B��B�]B�B�&B�"B��B�fB�B��B�HBߤBچB��B�
B�B�B	B�BуB��B��B��B��B��B��B��B��B�'B��B��B��B��B��B�	B��B~�Bx�B|�B�7Bt�Bm]Bg8BdZB`vBYKB^jB�ZB?HBL0BB'B&B=B!bB�BJB�B�B\B<�B
��B�B�B
��B
�B
�WB
�KB
�EB
�yB
�B
͟B
�XB
�KB
ȀB
��B
��BSBFB
�2B
�B
�pB
�B
��B
��B
B
�B
��B
�B
��B
�UB
� B
��B
˒B
� B
��B
��B
�LB
��B
�B
�nB
ƨB
�6B
��B
��B�B
�@B
�B
�eB
��B
�FB
��B
��B
�B
��B
��B
zxB
��B
poB
�B
�rB
o�B
g�B
lWB
ffB
`BB
h�B
_�B
b�B
\]B
Y�B
qB
j�B
R�B
S�B
iB
O�B
K�B
W?B
=�B
8�B
C�B
0�B
�B
IB
B
~B
-B
6FB
�B
VB
�B
.B
�B
�B
�B
!-B
eB
+B
AB	��B	�>B	��B	��B	��B	�ZB	��B	��B	�TB	�B	��B	�B	��B	�B	�B	�B	�B	�B	�QB	�B	��B	�ZB	�B	��B	�`B	�yB	�]B	�8B	��B	�|B	�;B	��B	�B	�]B	��B	��B	یB	چB	�WB	�5B	֡B	�B	��B	�B	��B	��B	��B	�&B	�HB	уB	�<B	̘B	ɆB	ʌB	��B	��B	ǮB	�B	ȴB	��B	ŢB	�qB	��B	�wB	�6B	��B	�B	�jB	�eB	�qB	��B	��B	��B	�6B	�0B	��B	��B	�eB	��B	��B	�VB	��B	�:B	�hB	�xB	�eB	��B	�~B	�hB	��B	�IB	�(B	�:B	��B	�(B	~�B	~�B	}"B	zDB	z�B	yrB	zDB	zxB	x�B	x�B	x8B	x�B	w�B	y	B	x�B	x8B	wfB	w�B	v�B	u�B	v+B	v`B	u%B	y>B	tTB	u%B	w2B	v�B	p�B	t�B	r�B	xlB	qB	qAB	o5B	o5B	qvB	n�B	o B	n/B	m�B	n�B	m�B	l�B	l�B	m)B	j�B	l�B	u%B	h�B	h�B	gmB	c B	iB	\�B	c�B	W�B	gmB	W�B	T�B	e�B	Z�B	\)B	MjB	M�B	I�B	J�B	MB	HKB	W
B	M�B	U2B	Y�B	K)B	I�B	GzB	GB	F�B	F?B	E9B	C�B	IB	E�B	C�B	Z�B	PB	H�B	8B	<�B	/�B	2�B	/�B	+�B	-B	*0B	*�B	)*B	*�B	*�B	)_B	(�B	)_B	)*B	'RB	(�B	(XB	%B	%zB	%B	"hB	#:B	"�B	"4B	"4B	!�B	VB	!B	"4B	!B	IB	B	 \B	$tB	$�B	!-B	�B	!bB	~B	!�B	�B	
rB	�B	SB	SB	B	B	B	uB	�B	AB	AB	uB	�B	 �B	 �B	�B��B	 �B	�B	�B	uB	oB��B�cB�.B��B��4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                          4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B
�aB
��B
�gB
�B
��B
��B
��B
� B
�'B
��B
�-B
�tB
��B
�wB
ÕB
˒B
ѷB
��B
څB
�cB
�AB
�`B\B#�B5�B7�B:�B<jB=�B?�BA BA�BB�BC,BD3BD3BD�BG�BH�BJ�BQNBj�B��BȴB�B�{BOB!B�B"hB*dB'RB%zBB�AB�BڅB��B��B��Bm(BQB�B
�B
��B
��B
�BB
��B
�mB
��B
��B
� B
c�B
ZQB
C�B
"�B
�B
�B	�7B	�B	�WB	��B	��B	�)B	��B	՛B	��B	��B	�9B	�3B	��B	y�B	t�B	q�B	l�B	g8B	X�B	JWB	EmB	,qB	$�B	OB	�B	:B��B�~B��B��B�B�cB�yB�B�DB�;B�(B�B�B��B�BحB�`B�TB�vB�<B�dB�QB�B�B��B�&B�#B�B�mB��B��B�<B��B�$B��B��B�FB�B��B�B��B��B�UB��B��B��B��B�*B�YB��B�LB��B��B��B�tB��B��B�hB�VB�IB�xB�CB�=B��B��B��B��B��B��B�7B��B��B�B�B�xB��B�\B�B��B�B��B�nB��B�7B�0B��B��B�B�RB��B�kB��B��B�9B��B�$B�XB�$B��B��B��B�tB�^B��B�B��B͞B��BтB֡B��B��BݘB�;B�NB��B��B	�B		�B	\B	B	�B	�B	�B	�B	$@B	'�B	)*B	*�B	,qB	3�B	8B	9�B	:�B	=B	;�B	=B	>�B	?�B	@�B	D3B	NpB	W�B	Y�B	ZQB	\�B	e,B	m�B	m�B	qB	poB	poB	m�B	u�B	x7B	zxB	z�B	y	B	u�B	u�B	v�B	v�B	v�B	v`B	w1B	xB	x�B	zDB	{�B	}"B	~�B	�MB	��B	��B	�7B	�7B	��B	�kB	�kB	�rB	�rB	��B	�~B	��B	�B	�!B	��B	��B	�$B	��B	�B	�B	��B	�IB	��B	��B	�3B	��B	�B	�B	��B	��B	��B	�6B	��B	�wB	��B	�UB	��B	�aB	�3B	��B	��B	�?B	��B	��B	��B	�XB	�#B	�6B	�jB	�<B	�}B	��B	��B	��B	�UB	�&B	�aB	��B	�aB	��B	��B	B	�3B	�gB	��B	�sB	ȀB	ɆB	�#B	ʌB	ʌB	˒B	�B	�B	ΤB	�vB	�HB	�NB	�`B	��B	�
B	�B	�KB	��B	�QB	�B	��B	ٴB	ܒB	�iB	�B	�;B	�pB	�B	��B	�GB	��B	��B	��B	�B	�GB	�|B	��B	��B	��B	�B	��B	�B	�B	�B	�fB	�8B	�yB	�B	��B	�]B	�B	�iB	��B	�B	�B	��B	��B	�B	��B	�B	�B	��B	��B	��B	��B	�YB	�+B	�`B	�`B	��B	�fB	��B	�fB	�B	��B	�>B	�B	��B	��B	�B	��B	�JB	�B	�JB	�B	��B	��B	��B	��B	��B
 iB
 �B
B
�B
GB
MB
MB
�B
MB
�B
�B
B
YB
YB
YB
%B
�B
�B
�B
�B
�B
�B
	7B
	7B

rB

=B
�B
�B
�B
�B
�B
PB
�B
!B
�B
'B
\B
�B
�B
.B
bB
�B
�B
4B
�B
�B
�B
FB
FB
B
B
FB
B
�B
RB
B
�B
�B
*B
�B
�B
*B
eB
�B
�B
B
qB
wB
IB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
OB
B
B
�B
!�B
"hB
!�B
!�B
!�B
!�B
"3B
"3B
"hB
"�B
"�B
"�B
"hB
"3B
#B
#�B
$@B
$�B
$�B
&LB
'B
&�B
&�B
&�B
'B
'RB
'RB
(�B
)�B
*�B
+B
+kB
+�B
+�B
,B
+�B
,<B
,qB
,�B
,�B
-B
-�B
-wB
-�B
-�B
.�B
/B
/�B
/�B
0UB
0�B
0�B
1[B
1'B
1'B
1[B
1�B
2-B
2�B
2�B
33B
3�B
49B
4mB
4mB
5B
5�B
5tB
5tB
6B
6�B
6�B
6�B
7KB
7�B
7�B
8B
8B
8RB
8RB
8�B
9#B
9XB
9�B
9�B
9�B
:^B
:^B
:�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
�UB
��B
�B
�9B
��B
�}B
��B
�3B
�B
�-B
�B
��B
��B
��B
�0B
��B
��B
��B
��B
��B
�BB
��B
��B
�'B
�OB
��B
��B
�'B
��B
�'B
��B
��B
�UB
�-B
��B
��B
�aB
��B
�EB
��B
�B
�zB
�?B
�mB
�KB
��B
��B
��B
�)B
��B
�)B
��B
�B
�B
��B
��B
�BB
�B
B
�&B
��B
�[B
�gB
��B
ŢB
�?B
�QB
�)B
�WB
˒B
ΤB
�pB
�B
�B
ҽB
��B
тB
��B
��B
��B
�
B
�EB
��B
קB
ٴB
ٴB
�EB
څB
�QB
�B
څB
�]B
��B
��B
ݘB
�5B
��B
�cB
�B
ݘB
��B
�;B
�;B
��B
��B
�,B
��B
�2B
�(B
�B
��B
�bBoB�B�B
=BB�B�B�B�B�BOBB �B'�B.IB0 B1[B6B7KB5?B7B8B6zB6�B7�B8�B7�B7B8RB:)B9�B:)B:�B<B=<B:�B:�B<�B=pB=B<�B<�B=pB=�B<�B<�B>wB?�B>wB>B@B?B>wB>�B@�BAUB?�B@NBB[BA�B@�B@BA BA�BAUB@�BA�BB�B@�BA�BCaBB[BA�BC,BD�BB&BB[BC�BD3BB�BB&BC�BDgBC�BB�BC,BD�BDgBB�BD3BE9BC,BD�BE�BCaBC�BEBEBC�BC,BE9BEmBD3BC�BE9BEBDgBDgBGyBGyBIBF?BGyBH�BG�BF�BG�BI�BIBG�BJ�BJ#BJ�BI�BH�BJ�BL�BK�BJWBMjBOBOBBN<BP�BS&BT,BU�B`�BglBhrBg�BjJBs�Bu%BzDB��B��B�=B��B��B��B�9B�6B��BȀBΤBҽB�8BٴB�cB�|B�B�%B�B�%B�B��B�WB�B�{B	7B�BIBqBIB�B�B!B�B�B 'B [B�B�B �B 'BOBUBUB"3B�B�B�B�B�B(�B)^B%FB-BB($B5�B*0B($B&�B.}B&�B#B%�B!�B'�B0UB2�B�BeB(�B�BYB7KBB�7B�\B��B��B�B�lB�DB��B�fB�vB�rB�B�B�B�#BݘB��B��B�8B�ZB�iB�TBSB�jB��B�EB�B�&B�5B�B�<B� B�3B�wB�OB�$B�-B�:B�=B�YB�Bz�Bu%By>B��BqBi�Bc�B`�B\�BU�BZ�B�B;�BH�B>wB"hB�B�B�B�B:B�B�B9#B
�>B�B�B
�8B
�`B
קB
՛B
ԕB
��B
�jB
��B
ƨB
ěB
��B
�9B
�2B�B�B
�B
�WB
��B
�UB
�B
�B
��B
�jB
�6B
�jB
�B
��B
�pB
�BB
��B
�pB
�B
�EB
��B
��B
�UB
��B
��B
��B
��B
�B�B
��B
�aB
��B
��B
��B
��B
�=B
�SB
}�B
�@B
v�B
��B
l�B
�VB
��B
k�B
c�B
h�B
b�B
\�B
d�B
\)B
_B
X�B
VB
m]B
g8B
OB
PHB
e`B
L/B
G�B
S�B
9�B
4�B
@B
-B
�B
�B
_B
�B
)^B
2�B
�B
�B
�B
~B
CB

	B
0B
}B
�B
{B	��B	��B	��B	��B	�B	�B	�B	�+B	�;B	�B	��B	�GB	��B	�DB	��B	�WB	��B	��B	��B	�B	�B	��B	�B	�WB	�%B	�B	��B	�B	�B	�B	��B	ۋB	�)B	�QB	حB	�B	�B	��B	��B	קB	څB	��B	�fB	�,B	�`B	�,B	�HB	�BB	�vB	̘B	��B	ʌB	��B	��B	��B	�/B	�3B	��B	�UB	�B	�,B	��B	��B	�B	��B	��B	� B	�mB	��B	��B	��B	�6B	�*B	��B	��B	��B	�*B	��B	��B	�9B	�B	��B	�B	��B	��B	��B	��B	�$B	��B	��B	�@B	��B	�xB	��B	�7B	�xB	{B	z�B	yrB	v�B	v�B	u�B	v�B	v�B	t�B	u%B	t�B	t�B	tB	uYB	u%B	t�B	s�B	s�B	r�B	rGB	r{B	r�B	quB	u�B	p�B	quB	s�B	sMB	m(B	qAB	o B	t�B	m]B	m�B	k�B	k�B	m�B	kB	kPB	jB	jB	kB	jB	h�B	h�B	iyB	g8B	iDB	quB	e,B	d�B	c�B	_pB	e`B	X�B	_�B	S�B	c�B	T,B	P�B	bB	W>B	XyB	I�B	J#B	E�B	GEB	IQB	D�B	SZB	J#B	Q�B	VB	GyB	F
B	C�B	CaB	B�B	B�B	A�B	@B	EmB	B&B	@NB	V�B	LdB	E9B	4mB	9#B	,B	.�B	,B	($B	)^B	&�B	'B	%zB	&�B	'B	%�B	%FB	%�B	%zB	#�B	%B	$�B	!bB	!�B	!bB	�B	�B	!B	�B	�B	�B	�B	qB	�B	qB	�B	_B	�B	 �B	!-B	}B	�B	�B	�B	�B	B	�B	 4B	�B	�B	 iB�bB�bB��B�.B��B��B��B�(B�"B�"B�(B��B��B��B��B��B��B��B��B�~B�JB�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                          4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230721224952                            20230721224952AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072122495220230721224952  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122495220230721224952QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072122495220230721224952QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8800            800             