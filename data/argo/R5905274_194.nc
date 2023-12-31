CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-05-01T22:36:53Z creation      
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
_FillValue                    �Argo profile    3.1 1.2 19500101000000  20230501223653  20230501223653  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�%r���@�%r���11  @�%r�O�p@�%r�O�p@0ŬGH@0ŬGH�dE�m��dE�m�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  AA  ?�z�?��H@:�H@}p�@�  @�G�@�G�@��RA  A ��A,��A@  A_\)A�Q�A���A�  A�\)A�\)AϮA�Q�A�A�
=B�
B(�B(�B   B(  B/�
B8  B@  BG�
BO�
BW�
B`  Bh  Bp(�Bx  B�B��B�{B��B��B�{B�  B�  B�(�B�(�B�  B�  B�(�B�  B�  B�(�B�{B�{B��
B��B�  B��
B��B�  B��B��
B��B�  B��B�B�B�B��C��C�C�C��C

=C{C  C  C  C��C��C��C��C�C�C   C"  C$  C&�C(
=C)��C,  C.  C/��C1��C4
=C5�C7��C:  C<  C>  C?�CA��CD  CE��CH  CJ
=CL  CM��CP  CR
=CT  CV  CW��CZ  C\
=C^
=C`  Cb  Cc��Cf  Ch  Cj  Cl
=Cn  Co�Cq��Ct  Cv  Cx
=Cz
=C|  C~  C�C�C�C�
=C�C�  C�
=C�C�  C�C���C���C�  C�  C���C���C���C���C�  C�
=C�
=C�C�  C�  C�C�\C�C�
=C�C���C���C���C�  C�C�  C�  C�C�  C�  C�  C�  C���C���C�  C�  C�  C���C�  C�
=C���C�  C�C�  C���C���C���C�  C�
=C�
=C���C���C���C���C�  C���C���C���C���C�  C�  C�  C���C���C�  C�  C�C�
=C�
=C�C�  C���C���C�  C�  C�  C�  C�  C�  C�C�C���C�  C���C�C�\C�C���C�  C�
=C�
=C�
=C���C��C���C�C�C���C�  C�
=C�C�  C�  C���C�C�
=C�C���C�  C�\C�  C�C�\C�C�  C�C���C�  C�\D �D }qD  Dz�D  D��D�qD� D  D��D  Dz�D  D}qD��D}qD  D��D	  D	z�D
  D
}qD  D� D  D�DD��D�D� D  D��D  D}qD��D� D�D��D  D��D  D��D  D}qD  D��D  D� DD��D�D��D  D� D�D}qD��D� D�D� D�D�DDz�D   D ��D ��D!z�D"  D"� D"��D#z�D#��D$}qD%�D%� D&�D&��D'D'�D(  D(� D)�D)��D*  D*� D+D+�D+�qD,}qD,��D-}qD.�D.z�D.�RD/z�D/��D0z�D0��D1}qD2  D2��D3�D3��D4D4� D4�RD5z�D5��D6z�D6�qD7}qD8  D8}qD9D9��D:  D:xRD:�RD;xRD;�qD<��D<��D=u�D=�RD>xRD>�RD?xRD?�RD@u�D@��DA}qDA�qDB}qDC  DC��DDDD� DE�DE��DE�qDF}qDF��DGz�DH  DH� DH��DIz�DJ  DJ� DK  DK� DK�qDL}qDL��DMz�DN�DN�DN�qDO}qDP  DP� DP�qDQ��DRDR��DS  DS��DS�qDT� DU�DU� DV  DV� DW  DW� DW��DXz�DY  DY� DZ  DZ� D[  D[� D\  D\� D]�D]� D]�qD^��D^�qD_}qD`  D`z�Da  Da� Db  Db��Dc  Dc}qDc�qDd}qDe  De� Df  Df��Dg  Dg��Dh  DhxRDh�qDi� Dj  Dj� Dj�qDk� Dl�Dl��Dm�Dm� Dn�Dn� Do  Do}qDo�qDp� Dq  Dq� DrDr�Ds�Ds}qDs�qDt}qDt�qDu��Dv�Dv�Dw�Dw��Dx�Dx� Dy  Dy��Dz  Dz� Dz�qD{� D|�D|}qD}  D}� D~�D~� D~��D}qD��D�@ D�}qD��HD��D�B�D�~�D��)D���D�AHD��HD���D��)D�AHD��HD���D�  D�@ D�� D�� D�HD�AHD��HD���D�HD�>�D��HD��HD���D�=qD�� D��HD��qD�@ D��HD��HD���D�<)D��HD��HD���D�>�D�~�D��HD���D�B�D�}qD��HD�HD�C�D�� D��HD��D�>�D��HD�D��qD�B�D��HD��HD�HD�>�D��HD���D�HD�>�D�� D�� D��D�>�D�~�D�� D���D�@ D�~�D�� D��qD�>�D�~�D��qD�  D�AHD��HD���D��qD�@ D�~�D�� D�HD�@ D�~�D�� D�  D�@ D�~�D���D��qD�@ D�� D���D��D�AHD��HD��HD�HD�>�D�~�D�� D���D�@ D�~�D���D��qD�>�D��HD��HD�HD�@ D�� D��HD�HD�AHD�� D���D��qD�=qD�� D���D���D�>�D�� D���D��D�AHD��HD�D�  D�>�D�}qD�� D�  D�AHD�~�D�� D�HD�AHD��HD��HD�HD�B�D�� D�� D�  D�>�D�~�D���D��qD�>�D�� D��HD�  D�>�D�� D�� D�  D�@ D��HD�D�HD�@ D�~�D��HD�HD�AHD�� D�� D��D�AHD��HD��HD�HD�AHD�� D���D�HD�B�D���D��HD�HD�=qD�� D���D�  D�AHD�� D�� D�HD�@ D��HD��HD�  D�@ D�� D�� D���D�@ D�~�D�� D�  D�AHD���D��HD�  D�B�D��HD��HD�HD�AHD��HD��HD�  D�@ D�� D��HD�HD�>�D�� D��HD��D�@ D�� D��HD�HD�AHD��HD�� D���D�@ D�� D�� D�HD�@ D�� D�� D�HD�AHD�~�D�� D�  D�AHD�~�D�� D�  D�AHD��HD��HD�  D�@ D�� D�� D���D�AHD�~�D���D���D�@ D D��HD�  D�AHD�~�D�� D�  D�=qDĂ�D�� D�HD�@ D�~�Dž�D�HD�@ Dƀ DƽqD�  D�B�Dǀ D�� D���D�=qDȀ D��HD�  D�>�DɁHD�� D�  D�AHDʀ DʽqD�HD�AHDˁHD�� D�  D�@ D̀ D�D�  D�@ D́HD�� D�  D�@ D�~�D�D�  D�>�Dπ D�� D�HD�@ DЀ D�� D��qD�>�D�~�DѾ�D�  D�@ DҀ DҾ�D�  D�>�D�~�D�� D���D�=qDԀ D��HD�HD�@ DՁHD��HD�HD�@ D�~�D�� D�  D�@ D׀ D��HD�HD�@ D�~�D�� D�HD�AHDفHD�� D�HD�AHDځHD��HD�  D�@ D�~�D۾�D���D�@ D܀ D�� D���D�>�D�}qDݾ�D�HD�@ Dހ D�� D�  D�@ D߀ D߾�D�HD�@ D�~�D��HD�HD�@ D� D��HD�HD�AHD� D⾸D���D�AHDわD��HD�  D�>�D� D�qD��qD�@ D�~�D徸D�  D�@ D�HD�� D���D�AHD�~�D��HD���D�AHD� D辸D�HD�>�D�~�D���D�  D�AHD�HD�qD���D�AHD�HD�� D�  D�>�D� D쾸D��qD�>�D�HD�D�  D�AHD�HD�� D�  D�@ D� D�� D�  D�AHD�~�D�D�  D�@ D��D�D�  D�>�D� D�� D���D�>�D� D��HD���D�>�D�HD�D�HD�B�D�� D��qD���D�=qD�~�D�� D��D�B�D���D��HD���D�=qD�~�D��qD���D�>�D�~�D�� D�HD�/\?W
=?�=q?���?Ǯ?�ff@�@z�@#�
@5@E�@Tz�@fff@p��@��\@�=q@�33@��H@��
@��@�z�@�(�@��@���@�
=@޸R@��
@���@�33@��HA�AffA	��A{A�\AffA=qA�RA"�\A'
=A*�HA/\)A333A7
=A:�HA>�RAB�\AG�AK�AP  ATz�AXQ�A\(�AaG�Ae�Ai��An{AqG�Au�Ay��A~{A���A�33A�p�A��A��A��
A�{A�  A�=qA�(�A�ffA�  A��A��
A�{A���A��\A�z�A�ffA���A��A�A�  A��A�(�A�{A���A��\A���A��RA���A��HA��AƸRAȣ�A��HA�{AϮAљ�A��
A�{A�Q�A�=qA�z�A޸RA���A�33A��A�\)A陚A��
A�A�  A�=qA�(�A�ffA���A��HA��A�\)B ��BB�HB  B��B=qB�B��B	�B33Bz�BB�HB  B�BffB\)B��BB�RB�B��BB
=B(�BG�BffB�B ��B!�B#
=B$(�B%G�B&ffB'�B(��B)��B*�HB,  B-p�B.�\B/�
B1�B2=qB3\)B4Q�B5��B6�RB8  B9�B:=qB;�B<z�B=��B>�HB?�B@��BABC
=BD(�BEG�BF�\BG�BH��BI�BJ�HBL(�BM��BN�RBP  BQG�BR=qBS\)BT��BUBV�HBW�BX��BY�B[33B\Q�B]p�B^ffB_�BaG�Bb�\Bc�Bd��Bf{Bg
=Bh(�Bi�Bj=qBk\)Blz�Bmp�Bn�HBp(�Bqp�Br�\Bs�Bt��Bv{Bw
=Bx  Bx��Bz{B{33B|Q�B}B~�HB�{B��RB�33B��B�{B���B�33B��
B�Q�B��HB�p�B�(�B��RB�\)B��B�z�B�
=B�p�B�  B�ffB�
=B�p�B�(�B���B�p�B�  B��\B��B�B�{B���B�G�B��
B�Q�B���B�B�Q�B���B�p�B��
B�ffB���B���B�{B���B��B�(�B��RB�33B��B�(�B���B�\)B��
B���B�G�B��
B�ffB��RB�G�B��
B��\B�33B�B�ffB���B�G�B��B�z�B�\)B�  B��\B���B���B�=qB���B���B�=qB���B�p�B��B��\B�
=B��B�z�B�33B�B�z�B��HB�p�B�{B���B�p�B�{B��RB�G�B�B�ffB���B��
B�z�B��B��B�(�B��HB�\)B�{B���B��B�(�B���B�p�B��B\B��B��
Bģ�B�33B�  B�Q�B�
=BǙ�B�z�B��Bə�B�=qB���B˙�B�Q�B���B�B�(�BθRB�\)B�{B��HBхB�=qBң�B�G�B��Bԏ\B�G�B�{BָRB�p�B��B�z�B��B�Bڏ\B�\)B�  B܏\B�
=B�B�Q�B�
=B��
B�z�B�
=B�B�=qB���B�B�ffB�
=B�B�(�B���B癚B�Q�B���B�B�{B�RB�p�B�=qB��HB홚B�{B�RB�G�B��B���B�p�B�{B��B�33B��
B�\B�p�B�{B���B�33B��
B���B�p�B�{B���B�33B�B��RB�\)B�  B�z�B��B�C Q�C ��C ��C33Cz�C��CG�C�C�
C(�C�\C�C(�Cp�CC{C�C�
C  C\)C�C{CffC��C�C33C��C��C	G�C	�C	�
C
�C
�\C
�C{CffCC�Cz�C��C��C=qC��C  CQ�C�\C��C{Cz�C�
C�CQ�C��C  CQ�C��C�C�Cp�C��C{CQ�C�\C��CQ�Cz�CC33Cz�CC��CG�C��C��CG�Cz�C��C
=Cz�C��C
=CQ�C��C��CQ�C�\C�
C{C�C�
C
=CQ�C�RC
=C=qC�C�CG�C�\C��C{C�C�HC (�C p�C �C!(�C!z�C!�RC"
=C"ffC"C#  C#Q�C#��C$
=C$\)C$��C$�HC%Q�C%�C%�C&=qC&�C&��C'=qC'z�C'�
C(G�C(��C(�
C)�C)��C)��C*(�C*z�C*��C+Q�C+�C+�
C,Q�C,�C,�C-=qC-�RC.{C.\)C.��C/�C/z�C/�RC0{C0�\C0�C133C1�\C2
=C2\)C2�C333C3��C3�HC4Q�C4C5
=C5z�C5�C6(�C6�\C7
=C7\)C7�RC833C8�C8�HC9\)C9C:{C:p�C:��C;Q�C;��C<�C<�C<�
C=Q�C=C>
=C>p�C>�C?=qC?��C@{C@p�C@��CA=qCA��CA�CB\)CB��CC{CCp�CC��CDG�CD��CE(�CEp�CE�
CF\)CF��CG  CG�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                     ?�z�?��H@:�H@}p�@�  @�G�@�G�@��RA  A ��A,��A@  A_\)A�Q�A���A�  A�\)A�\)AϮA�Q�A�A�
=B�
B(�B(�B   B(  B/�
B8  B@  BG�
BO�
BW�
B`  Bh  Bp(�Bx  B�B��B�{B��B��B�{B�  B�  B�(�B�(�B�  B�  B�(�B�  B�  B�(�B�{B�{B��
B��B�  B��
B��B�  B��B��
B��B�  B��B�B�B�B��C��C�C�C��C

=C{C  C  C  C��C��C��C��C�C�C   C"  C$  C&�C(
=C)��C,  C.  C/��C1��C4
=C5�C7��C:  C<  C>  C?�CA��CD  CE��CH  CJ
=CL  CM��CP  CR
=CT  CV  CW��CZ  C\
=C^
=C`  Cb  Cc��Cf  Ch  Cj  Cl
=Cn  Co�Cq��Ct  Cv  Cx
=Cz
=C|  C~  C�C�C�C�
=C�C�  C�
=C�C�  C�C���C���C�  C�  C���C���C���C���C�  C�
=C�
=C�C�  C�  C�C�\C�C�
=C�C���C���C���C�  C�C�  C�  C�C�  C�  C�  C�  C���C���C�  C�  C�  C���C�  C�
=C���C�  C�C�  C���C���C���C�  C�
=C�
=C���C���C���C���C�  C���C���C���C���C�  C�  C�  C���C���C�  C�  C�C�
=C�
=C�C�  C���C���C�  C�  C�  C�  C�  C�  C�C�C���C�  C���C�C�\C�C���C�  C�
=C�
=C�
=C���C��C���C�C�C���C�  C�
=C�C�  C�  C���C�C�
=C�C���C�  C�\C�  C�C�\C�C�  C�C���C�  C�\D �D }qD  Dz�D  D��D�qD� D  D��D  Dz�D  D}qD��D}qD  D��D	  D	z�D
  D
}qD  D� D  D�DD��D�D� D  D��D  D}qD��D� D�D��D  D��D  D��D  D}qD  D��D  D� DD��D�D��D  D� D�D}qD��D� D�D� D�D�DDz�D   D ��D ��D!z�D"  D"� D"��D#z�D#��D$}qD%�D%� D&�D&��D'D'�D(  D(� D)�D)��D*  D*� D+D+�D+�qD,}qD,��D-}qD.�D.z�D.�RD/z�D/��D0z�D0��D1}qD2  D2��D3�D3��D4D4� D4�RD5z�D5��D6z�D6�qD7}qD8  D8}qD9D9��D:  D:xRD:�RD;xRD;�qD<��D<��D=u�D=�RD>xRD>�RD?xRD?�RD@u�D@��DA}qDA�qDB}qDC  DC��DDDD� DE�DE��DE�qDF}qDF��DGz�DH  DH� DH��DIz�DJ  DJ� DK  DK� DK�qDL}qDL��DMz�DN�DN�DN�qDO}qDP  DP� DP�qDQ��DRDR��DS  DS��DS�qDT� DU�DU� DV  DV� DW  DW� DW��DXz�DY  DY� DZ  DZ� D[  D[� D\  D\� D]�D]� D]�qD^��D^�qD_}qD`  D`z�Da  Da� Db  Db��Dc  Dc}qDc�qDd}qDe  De� Df  Df��Dg  Dg��Dh  DhxRDh�qDi� Dj  Dj� Dj�qDk� Dl�Dl��Dm�Dm� Dn�Dn� Do  Do}qDo�qDp� Dq  Dq� DrDr�Ds�Ds}qDs�qDt}qDt�qDu��Dv�Dv�Dw�Dw��Dx�Dx� Dy  Dy��Dz  Dz� Dz�qD{� D|�D|}qD}  D}� D~�D~� D~��D}qD��D�@ D�}qD��HD��D�B�D�~�D��)D���D�AHD��HD���D��)D�AHD��HD���D�  D�@ D�� D�� D�HD�AHD��HD���D�HD�>�D��HD��HD���D�=qD�� D��HD��qD�@ D��HD��HD���D�<)D��HD��HD���D�>�D�~�D��HD���D�B�D�}qD��HD�HD�C�D�� D��HD��D�>�D��HD�D��qD�B�D��HD��HD�HD�>�D��HD���D�HD�>�D�� D�� D��D�>�D�~�D�� D���D�@ D�~�D�� D��qD�>�D�~�D��qD�  D�AHD��HD���D��qD�@ D�~�D�� D�HD�@ D�~�D�� D�  D�@ D�~�D���D��qD�@ D�� D���D��D�AHD��HD��HD�HD�>�D�~�D�� D���D�@ D�~�D���D��qD�>�D��HD��HD�HD�@ D�� D��HD�HD�AHD�� D���D��qD�=qD�� D���D���D�>�D�� D���D��D�AHD��HD�D�  D�>�D�}qD�� D�  D�AHD�~�D�� D�HD�AHD��HD��HD�HD�B�D�� D�� D�  D�>�D�~�D���D��qD�>�D�� D��HD�  D�>�D�� D�� D�  D�@ D��HD�D�HD�@ D�~�D��HD�HD�AHD�� D�� D��D�AHD��HD��HD�HD�AHD�� D���D�HD�B�D���D��HD�HD�=qD�� D���D�  D�AHD�� D�� D�HD�@ D��HD��HD�  D�@ D�� D�� D���D�@ D�~�D�� D�  D�AHD���D��HD�  D�B�D��HD��HD�HD�AHD��HD��HD�  D�@ D�� D��HD�HD�>�D�� D��HD��D�@ D�� D��HD�HD�AHD��HD�� D���D�@ D�� D�� D�HD�@ D�� D�� D�HD�AHD�~�D�� D�  D�AHD�~�D�� D�  D�AHD��HD��HD�  D�@ D�� D�� D���D�AHD�~�D���D���D�@ D D��HD�  D�AHD�~�D�� D�  D�=qDĂ�D�� D�HD�@ D�~�Dž�D�HD�@ Dƀ DƽqD�  D�B�Dǀ D�� D���D�=qDȀ D��HD�  D�>�DɁHD�� D�  D�AHDʀ DʽqD�HD�AHDˁHD�� D�  D�@ D̀ D�D�  D�@ D́HD�� D�  D�@ D�~�D�D�  D�>�Dπ D�� D�HD�@ DЀ D�� D��qD�>�D�~�DѾ�D�  D�@ DҀ DҾ�D�  D�>�D�~�D�� D���D�=qDԀ D��HD�HD�@ DՁHD��HD�HD�@ D�~�D�� D�  D�@ D׀ D��HD�HD�@ D�~�D�� D�HD�AHDفHD�� D�HD�AHDځHD��HD�  D�@ D�~�D۾�D���D�@ D܀ D�� D���D�>�D�}qDݾ�D�HD�@ Dހ D�� D�  D�@ D߀ D߾�D�HD�@ D�~�D��HD�HD�@ D� D��HD�HD�AHD� D⾸D���D�AHDわD��HD�  D�>�D� D�qD��qD�@ D�~�D徸D�  D�@ D�HD�� D���D�AHD�~�D��HD���D�AHD� D辸D�HD�>�D�~�D���D�  D�AHD�HD�qD���D�AHD�HD�� D�  D�>�D� D쾸D��qD�>�D�HD�D�  D�AHD�HD�� D�  D�@ D� D�� D�  D�AHD�~�D�D�  D�@ D��D�D�  D�>�D� D�� D���D�>�D� D��HD���D�>�D�HD�D�HD�B�D�� D��qD���D�=qD�~�D�� D��D�B�D���D��HD���D�=qD�~�D��qD���D�>�D�~�D�� D�HD�/\?W
=?�=q?���?Ǯ?�ff@�@z�@#�
@5@E�@Tz�@fff@p��@��\@�=q@�33@��H@��
@��@�z�@�(�@��@���@�
=@޸R@��
@���@�33@��HA�AffA	��A{A�\AffA=qA�RA"�\A'
=A*�HA/\)A333A7
=A:�HA>�RAB�\AG�AK�AP  ATz�AXQ�A\(�AaG�Ae�Ai��An{AqG�Au�Ay��A~{A���A�33A�p�A��A��A��
A�{A�  A�=qA�(�A�ffA�  A��A��
A�{A���A��\A�z�A�ffA���A��A�A�  A��A�(�A�{A���A��\A���A��RA���A��HA��AƸRAȣ�A��HA�{AϮAљ�A��
A�{A�Q�A�=qA�z�A޸RA���A�33A��A�\)A陚A��
A�A�  A�=qA�(�A�ffA���A��HA��A�\)B ��BB�HB  B��B=qB�B��B	�B33Bz�BB�HB  B�BffB\)B��BB�RB�B��BB
=B(�BG�BffB�B ��B!�B#
=B$(�B%G�B&ffB'�B(��B)��B*�HB,  B-p�B.�\B/�
B1�B2=qB3\)B4Q�B5��B6�RB8  B9�B:=qB;�B<z�B=��B>�HB?�B@��BABC
=BD(�BEG�BF�\BG�BH��BI�BJ�HBL(�BM��BN�RBP  BQG�BR=qBS\)BT��BUBV�HBW�BX��BY�B[33B\Q�B]p�B^ffB_�BaG�Bb�\Bc�Bd��Bf{Bg
=Bh(�Bi�Bj=qBk\)Blz�Bmp�Bn�HBp(�Bqp�Br�\Bs�Bt��Bv{Bw
=Bx  Bx��Bz{B{33B|Q�B}B~�HB�{B��RB�33B��B�{B���B�33B��
B�Q�B��HB�p�B�(�B��RB�\)B��B�z�B�
=B�p�B�  B�ffB�
=B�p�B�(�B���B�p�B�  B��\B��B�B�{B���B�G�B��
B�Q�B���B�B�Q�B���B�p�B��
B�ffB���B���B�{B���B��B�(�B��RB�33B��B�(�B���B�\)B��
B���B�G�B��
B�ffB��RB�G�B��
B��\B�33B�B�ffB���B�G�B��B�z�B�\)B�  B��\B���B���B�=qB���B���B�=qB���B�p�B��B��\B�
=B��B�z�B�33B�B�z�B��HB�p�B�{B���B�p�B�{B��RB�G�B�B�ffB���B��
B�z�B��B��B�(�B��HB�\)B�{B���B��B�(�B���B�p�B��B\B��B��
Bģ�B�33B�  B�Q�B�
=BǙ�B�z�B��Bə�B�=qB���B˙�B�Q�B���B�B�(�BθRB�\)B�{B��HBхB�=qBң�B�G�B��Bԏ\B�G�B�{BָRB�p�B��B�z�B��B�Bڏ\B�\)B�  B܏\B�
=B�B�Q�B�
=B��
B�z�B�
=B�B�=qB���B�B�ffB�
=B�B�(�B���B癚B�Q�B���B�B�{B�RB�p�B�=qB��HB홚B�{B�RB�G�B��B���B�p�B�{B��B�33B��
B�\B�p�B�{B���B�33B��
B���B�p�B�{B���B�33B�B��RB�\)B�  B�z�B��B�C Q�C ��C ��C33Cz�C��CG�C�C�
C(�C�\C�C(�Cp�CC{C�C�
C  C\)C�C{CffC��C�C33C��C��C	G�C	�C	�
C
�C
�\C
�C{CffCC�Cz�C��C��C=qC��C  CQ�C�\C��C{Cz�C�
C�CQ�C��C  CQ�C��C�C�Cp�C��C{CQ�C�\C��CQ�Cz�CC33Cz�CC��CG�C��C��CG�Cz�C��C
=Cz�C��C
=CQ�C��C��CQ�C�\C�
C{C�C�
C
=CQ�C�RC
=C=qC�C�CG�C�\C��C{C�C�HC (�C p�C �C!(�C!z�C!�RC"
=C"ffC"C#  C#Q�C#��C$
=C$\)C$��C$�HC%Q�C%�C%�C&=qC&�C&��C'=qC'z�C'�
C(G�C(��C(�
C)�C)��C)��C*(�C*z�C*��C+Q�C+�C+�
C,Q�C,�C,�C-=qC-�RC.{C.\)C.��C/�C/z�C/�RC0{C0�\C0�C133C1�\C2
=C2\)C2�C333C3��C3�HC4Q�C4C5
=C5z�C5�C6(�C6�\C7
=C7\)C7�RC833C8�C8�HC9\)C9C:{C:p�C:��C;Q�C;��C<�C<�C<�
C=Q�C=C>
=C>p�C>�C?=qC?��C@{C@p�C@��CA=qCA��CA�CB\)CB��CC{CCp�CC��CDG�CD��CE(�CEp�CE�
CF\)CF��CG  CG�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                     G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�Aя\AѓuAѓuAёhAёhAч+AхAщ7AыDA�l�A�dZA�ZA�=qAд9A��A���Aϝ�A�?}A��AΩ�A�VA�|�A�7LA��A�ĜAˣ�A˛�A˓uA�O�A�7LA�"�A�
=A��A��
A�ĜAʲ-Aʝ�AʍPAʁA�n�A��A���A�\)AǛ�A�oAƅA�ĜA�-A�ƨAġ�Aĕ�A�z�A��A�%A��TAøRAå�AÕ�AÏ\A�x�A�K�A��A���A¼jA´9A¡�A�z�A�E�A���A�ƨA���A�n�A�^5A�I�A��A���A�bA��+A��
A�K�A�{A��9A�ZA���A��PA�-A���A� �A��+A���A���A�z�A��A���A��A�\)A���A��A��A�E�A�XA��PA�bA��A��RA��FA���A�ffA���A���A���A���A��A���A��hA�p�A��mA�E�A��yA���A���A}�
Ay�Au��Arn�Ao��Ah�yAc�A^ �A[�FAZ��AX�AWC�AT��AQ�7ALQ�AH�AHbNAGG�AFz�AFbNAE�
AB�/AA��AAG�A?XA:�A:A9�-A6�/A5+A4ZA3�;A3�PA3/A3��A41A3�#A3|�A1��A0ffA.E�A*��A( �A'oA%33A#l�A!A n�A^5A�!A�AoA��AM�A1'A�
A�A%Az�AVA9XA{A�FA\)A��AAM�A9XA�#A��A��Al�A	��A�AC�A�\A=qA(�A�-A|�AC�AZAl�A=qAp�AhsA ��A �A �A @��!@��/@��@�Q�@��@���@���@��9@�$�@��@���@�ƨ@�@�\)@�dZ@�l�@�!@�?}@��@�Ĝ@�w@�R@��@�Q�@��@��m@���@�+@柾@���@�j@�A�@�@�"�@�@��@�bN@�ƨ@�;d@ޏ\@ݡ�@�z�@� �@ۥ�@�33@���@ڏ\@�ff@�M�@�{@�@�G�@�%@��@��`@���@؋D@׶F@��@�^5@Ցh@�?}@���@�I�@Ӯ@�|�@�33@�@�p�@��@�A�@ύP@�\)@ΰ!@�^5@�V@ͺ^@���@�Q�@���@�t�@�C�@�+@�ȴ@ʇ+@�E�@ɡ�@���@Ȭ@�|�@ƸR@Ə\@�~�@�~�@�V@ŉ7@ģ�@�Z@�b@��@Ý�@���@���@�@�~�@�v�@�ff@�5?@�`B@�&�@�/@�/@��/@��F@��H@���@�v�@�=q@��-@�G�@�V@��9@��D@�r�@�bN@�A�@��@���@�o@��@��H@��H@��y@�M�@��^@��u@�I�@��@��@��y@�v�@�n�@�5?@�?}@�&�@�V@���@��@��/@���@��@��`@���@�Q�@� �@�1@��;@��@�S�@�@��!@��+@�ff@�J@���@��^@�p�@��@���@�z�@��@�ƨ@�l�@�"�@�ȴ@�~�@�M�@��@���@��7@��7@�`B@�7L@��@��j@�(�@�;d@��y@��!@��\@�$�@���@�V@��@��D@��@��@�z�@�r�@�A�@�1'@��@��m@��F@���@��P@�t�@�S�@�@�ȴ@��R@��!@���@�v�@�V@�J@���@���@��@��`@���@��u@�bN@�1@��F@�C�@�o@�+@�"�@��!@�E�@�$�@�{@���@�`B@��/@�Ĝ@��j@���@��@�j@�Z@�b@��@��@��!@���@�@�`B@��@�r�@��@�b@�b@��w@�33@��@��@���@�ȴ@��!@��+@�-@��@�V@�Ĝ@�Ĝ@�b@�dZ@�C�@�dZ@�+@��R@�v�@�^5@��@��@�Z@��@�ƨ@�S�@�
=@���@��!@��+@�{@��-@��@�G�@��@���@��D@�Z@�I�@���@���@�l�@�o@���@���@���@�ff@��@���@�x�@�X@�G�@�7L@��@��@���@��@���@���@��u@��D@��@�bN@��m@��P@�;d@��@���@�v�@�ff@�M�@��@��@�@���@��h@�p�@�O�@��@���@���@��u@�A�@�9X@�(�@���@�33@��H@�~�@�E�@�=q@�=q@�=q@�{@��T@���@�&�@��@��j@�j@�(�@��@���@�l�@�\)@��@�@�@��y@���@���@���@�v�@�V@�=q@�J@���@��/@�r�@�9X@�b@\)@~ȴ@~��@~{@}�h@|��@|�D@|I�@|9X@|(�@|�@{ƨ@{"�@z�@yhs@x�@x  @w��@w+@v�@vȴ@v5?@v{@u�T@u��@u�h@u`B@t�@tz�@s��@s�
@s�F@s�@r�@r��@rn�@r�@q�7@pĜ@p1'@p �@pb@o��@o\)@n�y@nE�@m�T@n@m`B@l��@lI�@kƨ@kt�@k33@j��@j�\@j=q@jJ@i��@iX@i7L@i�@hA�@g�;@f�R@f{@e@e`B@d�/@dz�@dI�@d(�@d�@c�
@c@a��@a%@`��@` �@_�@_�;@_l�@^�y@^�R@^�+@]�@]�@]O�@]/@]V@\�/@\��@[�m@[t�@["�@Z�!@ZJ@Y��@Yx�@YG�@Y&�@X�`@X��@Xb@W��@WK�@W+@VE�@V{@U@U�@UO�@T�@T��@T��@T�j@T�D@T9X@S�m@S�
@SS�@R��@R~�@Q��@Q��@Q��@Qhs@QG�@P��@P�@O�P@N��@Nv�@N{@M�-@Mp�@M?}@MV@L�j@L�D@K��@K��@J�@J��@Jn�@JM�@JM�@I�@I&�@H�@Hb@G�@G|�@Fȴ@Fff@Fv�@Fff@F@E�@E�@D��@D9X@D1@C��@C��@C�m@CS�@Co@B��@A��@A�#@A��@A%@@�u@@bN@@Q�@@ �@?�w@?
=@>��@>ff@>{@=�T@=@=�h@=`B@=O�@=V@=V@=V@<�@<��@<z�@<j@<�@<1@;�@:�H@:��@:��@:~�@9��@9%@8�u@8A�@81'@8  @7�;@7��@7l�@7\)@7+@6�R@6E�@6{@5�T@5�-@5�@5��@5��@5��@5O�@4�j@4I�@4(�@3�F@3C�@333@333@3"�@2�H@2�!@2�@1�@1��@1%@0bN@/��@.��@.��@.E�@.@-@-�-@-��@-`B@,�/@,j@,9X@+�
@+dZ@+S�@+C�@+o@*�@*��@*^5@*^5@*=q@*J@)�#@)�7@)7L@(�`@(��@(�u@(bN@(1'@'�;@'|�@'+@'+@&��@&�@&ȴ@&ȴ@&��@&v�@&E�@&5?@&{@%�-@%O�@%�@$�/@$�j@$(�@#�F@#dZ@#33@#"�@#@"��@"�!@"n�@"^5@"-@!��@!��@!G�@!G�@!7L@!7L@!&�@ ��@ �u@ r�@ Q�@ Q�@ A�@ 1'@ 1'@ 1'@  �@�@��@|�@;d@��@$�@��@�h@�@`B@O�@/@V@�@�/@�j@�D@I�@�@ƨ@��@dZ@C�@�\@~�@~�@~�@n�@n�@��@�^@�7@x�@G�@&�@&�@�@�@��@�u@��@�u@bN@b@�;@��@�;@�;@�w@��@K�@;d@+@
=@��@�R@�+@ff@V@E�@@�@��@O�@/@V@�@�@�D@Z@(�@(�@�@1@�m@�F@��@�@t�@S�@�@��@�!@��@~�@M�@�@��@��@��@7L@%@Ĝ@�9@�@A�@ �@  @�;@\)@
=@��@�y@ȴ@V@E�Aя\Aя\Aя\Aя\AёhAёhAёhAёhAя\AёhAя\Aя\AэPAэPAя\Aя\AёhAыDAя\AыDAч+AуAуAхAхAуAуAхAщ7Aя\Aя\AёhAя\AёhA�r�A�p�A�n�A�n�A�ffA�ffA�ffA�dZA�\)A�ZA�VA�ZA�ZA�S�A�Q�A�I�A�=qA�?}A�;dA�=qA�;dA�&�A�"�A��A���AЬAЍPA�|�A�l�A�hsA�ffA�bNA��A�VA���A���A��A��mA��TA��;A���A���A���A�ƨA�ĜA�AϾwAϴ9Aϟ�AυA�z�A�jA�ffA�dZA�^5A�I�A�1'A�(�A��A�VA�
=A�%A���A��A��A��;A���A���A�Aδ9Aΰ!AΩ�AΝ�AΕ�A΍PA·+A�|�A�jA�\)A�G�A�/A�  A��A��
AͰ!AͅA�`BA�JA���A̍PA�ffA�=qA��A�{A�bA�JA�JA�
=A�A��A��TA��HA��/A��/A��#A��#A�ȴA˸RA˲-AˬA˩�A˩�A˥�Aˣ�Aˣ�Aˣ�A˟�Aˡ�A˟�A˝�A˝�A˝�A˛�A˙�A˗�A˗�A˗�A˕�A˓uA˓uAˍPA�x�A�ZA�O�A�I�A�E�A�C�A�A�A�?}A�;dA�;dA�9XA�5?A�1'A�-A�+A�&�A�$�A�&�A�$�A��A��A�{A�bA�VA�JA�
=A�A���A���A���A��A��A��A��A��yA��mA��TA��;A��A���A���A�ƨA�ĜA�ƨA�ĜA�ĜA�A���A���AʼjAʸRAʴ9Aʰ!AʮAʧ�Aʧ�Aʥ�Aʣ�Aʝ�Aʙ�Aʙ�Aʛ�Aʙ�Aʗ�Aʕ�AʑhAʉ7AʅAʃAʃAʃAʅAʅAʃAʃAʃAʃAʁA�|�A�v�A�p�A�hsA�ffA�`BA�S�A�M�A�I�A�?}A� �A���A��#AɋDA�M�A�bA�  A��TA�ƨAȸRAȩ�AȑhAȅA�~�A�ffA�A�A� �A���A��
AǾwAǬAǓuA�x�A�\)A�A�A�1'A�(�A��A�bA�%A��HA���AƾwAƮAƝ�A�|�A�p�A�Q�A�33A�oA��mA�ƨAżjAŬAőhA�hsA�O�A�?}A�5?A�"�A�1A�  A��A��A�ȴAĺ^AĴ9Aİ!AĬAĩ�Aħ�Aĥ�Aġ�Aě�Aĕ�Aĕ�Aĕ�Aę�Aę�Aė�AđhAď\Aĉ7Ać+AăA�|�A�p�A�^5A�?}A�+A�$�A��A�{A�VA�JA�JA�
=A�1A�%A�  A��A��A��A��yA��HA��
A���AüjAú^AøRAöFAô9AöFAöFAîAá�AÛ�AÛ�AÙ�A×�AÕ�AÕ�AÕ�AÓuAÑhAÑhAÑhAÓuAÑhAÏ\AËDAÃA�z�A�v�A�t�A�t�A�x�A�t�A�bNA�Q�A�I�A�G�A�?}A�7LA�/A�$�A��A��A�1A��A��yA��
A���A�ȴA�ĜA�ĜA�ĜA���A¾wAº^A¸RA¸RA¸RAº^A¸RA´9A°!A¬A¬A®A¬A¥�A�A�A\AA�|�A�x�A�t�A�l�A�dZA�XA�K�A�C�A�;dA�(�A��A�JA�A���A��A��A���A���A�ȴA�ƨA�A���A���A�A�A��FA���A��PA��A�z�A�p�A�jA�jA�hsA�ffA�bNA�\)A�\)A�\)A�\)A�\)A�XA�XA�VA�Q�A�I�A�9XA�$�A��A���A��mA��;A��
A���A�A��-A���A��PA��7A��A�t�A�VA�&�A�VA���A��mA���A��9A���A��DA��A�t�A�hsA�S�A�G�A�;dA���A���A�p�A�dZA�ZA�ZA�S�A�K�A�G�A�=qA�5?A�+A� �A��A�oA�
=A�%A��A��
A���A�ĜA��-A���A�z�A�p�A�^5A�1A�9XA��/A�dZA�{A��TA��wA���A��7A�p�A�M�A�?}A�9XA�(�A�v�A�/A�ƨA��A��A�ZA��A�VA�A��A��TA���A���A���A��!A���A��uA��7A�l�A�E�A�-A��mA���A���A��9A��+A�r�A�Q�A�7LA�
=A�JA���A���A���A�l�A�Q�A�5?A�bA��;A��-A���A��uA���A��7A�v�A�\)A�C�A�/A� �A�{A�
=A�%A���A���A��RA���A��A��A���A���A�
=A�&�A���A��A���A�Q�A��A�%A���A��/A���A��A�r�A�M�A�+A�%A��/A��A�t�A�I�A��HA���A�E�A��A��RA�ffA��A���A��TA���A��+A�;dA�bA��HA��-A���A��A�t�A�bNA�?}A� �A��A�
=A��A�t�A���A�t�A��A�A��TA���A���A��!A�~�A�S�A� �A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                     Aя\AѓuAѓuAёhAёhAч+AхAщ7AыDA�l�A�dZA�ZA�=qAд9A��A���Aϝ�A�?}A��AΩ�A�VA�|�A�7LA��A�ĜAˣ�A˛�A˓uA�O�A�7LA�"�A�
=A��A��
A�ĜAʲ-Aʝ�AʍPAʁA�n�A��A���A�\)AǛ�A�oAƅA�ĜA�-A�ƨAġ�Aĕ�A�z�A��A�%A��TAøRAå�AÕ�AÏ\A�x�A�K�A��A���A¼jA´9A¡�A�z�A�E�A���A�ƨA���A�n�A�^5A�I�A��A���A�bA��+A��
A�K�A�{A��9A�ZA���A��PA�-A���A� �A��+A���A���A�z�A��A���A��A�\)A���A��A��A�E�A�XA��PA�bA��A��RA��FA���A�ffA���A���A���A���A��A���A��hA�p�A��mA�E�A��yA���A���A}�
Ay�Au��Arn�Ao��Ah�yAc�A^ �A[�FAZ��AX�AWC�AT��AQ�7ALQ�AH�AHbNAGG�AFz�AFbNAE�
AB�/AA��AAG�A?XA:�A:A9�-A6�/A5+A4ZA3�;A3�PA3/A3��A41A3�#A3|�A1��A0ffA.E�A*��A( �A'oA%33A#l�A!A n�A^5A�!A�AoA��AM�A1'A�
A�A%Az�AVA9XA{A�FA\)A��AAM�A9XA�#A��A��Al�A	��A�AC�A�\A=qA(�A�-A|�AC�AZAl�A=qAp�AhsA ��A �A �A @��!@��/@��@�Q�@��@���@���@��9@�$�@��@���@�ƨ@�@�\)@�dZ@�l�@�!@�?}@��@�Ĝ@�w@�R@��@�Q�@��@��m@���@�+@柾@���@�j@�A�@�@�"�@�@��@�bN@�ƨ@�;d@ޏ\@ݡ�@�z�@� �@ۥ�@�33@���@ڏ\@�ff@�M�@�{@�@�G�@�%@��@��`@���@؋D@׶F@��@�^5@Ցh@�?}@���@�I�@Ӯ@�|�@�33@�@�p�@��@�A�@ύP@�\)@ΰ!@�^5@�V@ͺ^@���@�Q�@���@�t�@�C�@�+@�ȴ@ʇ+@�E�@ɡ�@���@Ȭ@�|�@ƸR@Ə\@�~�@�~�@�V@ŉ7@ģ�@�Z@�b@��@Ý�@���@���@�@�~�@�v�@�ff@�5?@�`B@�&�@�/@�/@��/@��F@��H@���@�v�@�=q@��-@�G�@�V@��9@��D@�r�@�bN@�A�@��@���@�o@��@��H@��H@��y@�M�@��^@��u@�I�@��@��@��y@�v�@�n�@�5?@�?}@�&�@�V@���@��@��/@���@��@��`@���@�Q�@� �@�1@��;@��@�S�@�@��!@��+@�ff@�J@���@��^@�p�@��@���@�z�@��@�ƨ@�l�@�"�@�ȴ@�~�@�M�@��@���@��7@��7@�`B@�7L@��@��j@�(�@�;d@��y@��!@��\@�$�@���@�V@��@��D@��@��@�z�@�r�@�A�@�1'@��@��m@��F@���@��P@�t�@�S�@�@�ȴ@��R@��!@���@�v�@�V@�J@���@���@��@��`@���@��u@�bN@�1@��F@�C�@�o@�+@�"�@��!@�E�@�$�@�{@���@�`B@��/@�Ĝ@��j@���@��@�j@�Z@�b@��@��@��!@���@�@�`B@��@�r�@��@�b@�b@��w@�33@��@��@���@�ȴ@��!@��+@�-@��@�V@�Ĝ@�Ĝ@�b@�dZ@�C�@�dZ@�+@��R@�v�@�^5@��@��@�Z@��@�ƨ@�S�@�
=@���@��!@��+@�{@��-@��@�G�@��@���@��D@�Z@�I�@���@���@�l�@�o@���@���@���@�ff@��@���@�x�@�X@�G�@�7L@��@��@���@��@���@���@��u@��D@��@�bN@��m@��P@�;d@��@���@�v�@�ff@�M�@��@��@�@���@��h@�p�@�O�@��@���@���@��u@�A�@�9X@�(�@���@�33@��H@�~�@�E�@�=q@�=q@�=q@�{@��T@���@�&�@��@��j@�j@�(�@��@���@�l�@�\)@��@�@�@��y@���@���@���@�v�@�V@�=q@�J@���@��/@�r�@�9X@�b@\)@~ȴ@~��@~{@}�h@|��@|�D@|I�@|9X@|(�@|�@{ƨ@{"�@z�@yhs@x�@x  @w��@w+@v�@vȴ@v5?@v{@u�T@u��@u�h@u`B@t�@tz�@s��@s�
@s�F@s�@r�@r��@rn�@r�@q�7@pĜ@p1'@p �@pb@o��@o\)@n�y@nE�@m�T@n@m`B@l��@lI�@kƨ@kt�@k33@j��@j�\@j=q@jJ@i��@iX@i7L@i�@hA�@g�;@f�R@f{@e@e`B@d�/@dz�@dI�@d(�@d�@c�
@c@a��@a%@`��@` �@_�@_�;@_l�@^�y@^�R@^�+@]�@]�@]O�@]/@]V@\�/@\��@[�m@[t�@["�@Z�!@ZJ@Y��@Yx�@YG�@Y&�@X�`@X��@Xb@W��@WK�@W+@VE�@V{@U@U�@UO�@T�@T��@T��@T�j@T�D@T9X@S�m@S�
@SS�@R��@R~�@Q��@Q��@Q��@Qhs@QG�@P��@P�@O�P@N��@Nv�@N{@M�-@Mp�@M?}@MV@L�j@L�D@K��@K��@J�@J��@Jn�@JM�@JM�@I�@I&�@H�@Hb@G�@G|�@Fȴ@Fff@Fv�@Fff@F@E�@E�@D��@D9X@D1@C��@C��@C�m@CS�@Co@B��@A��@A�#@A��@A%@@�u@@bN@@Q�@@ �@?�w@?
=@>��@>ff@>{@=�T@=@=�h@=`B@=O�@=V@=V@=V@<�@<��@<z�@<j@<�@<1@;�@:�H@:��@:��@:~�@9��@9%@8�u@8A�@81'@8  @7�;@7��@7l�@7\)@7+@6�R@6E�@6{@5�T@5�-@5�@5��@5��@5��@5O�@4�j@4I�@4(�@3�F@3C�@333@333@3"�@2�H@2�!@2�@1�@1��@1%@0bN@/��@.��@.��@.E�@.@-@-�-@-��@-`B@,�/@,j@,9X@+�
@+dZ@+S�@+C�@+o@*�@*��@*^5@*^5@*=q@*J@)�#@)�7@)7L@(�`@(��@(�u@(bN@(1'@'�;@'|�@'+@'+@&��@&�@&ȴ@&ȴ@&��@&v�@&E�@&5?@&{@%�-@%O�@%�@$�/@$�j@$(�@#�F@#dZ@#33@#"�@#@"��@"�!@"n�@"^5@"-@!��@!��@!G�@!G�@!7L@!7L@!&�@ ��@ �u@ r�@ Q�@ Q�@ A�@ 1'@ 1'@ 1'@  �@�@��@|�@;d@��@$�@��@�h@�@`B@O�@/@V@�@�/@�j@�D@I�@�@ƨ@��@dZ@C�@�\@~�@~�@~�@n�@n�@��@�^@�7@x�@G�@&�@&�@�@�@��@�u@��@�u@bN@b@�;@��@�;@�;@�w@��@K�@;d@+@
=@��@�R@�+@ff@V@E�@@�@��@O�@/@V@�@�@�D@Z@(�@(�@�@1@�m@�F@��@�@t�@S�@�@��@�!@��@~�@M�@�@��@��@��@7L@%@Ĝ@�9@�@A�@ �@  @�;@\)@
=@��@�y@ȴ@V@E�Aя\Aя\Aя\Aя\AёhAёhAёhAёhAя\AёhAя\Aя\AэPAэPAя\Aя\AёhAыDAя\AыDAч+AуAуAхAхAуAуAхAщ7Aя\Aя\AёhAя\AёhA�r�A�p�A�n�A�n�A�ffA�ffA�ffA�dZA�\)A�ZA�VA�ZA�ZA�S�A�Q�A�I�A�=qA�?}A�;dA�=qA�;dA�&�A�"�A��A���AЬAЍPA�|�A�l�A�hsA�ffA�bNA��A�VA���A���A��A��mA��TA��;A���A���A���A�ƨA�ĜA�AϾwAϴ9Aϟ�AυA�z�A�jA�ffA�dZA�^5A�I�A�1'A�(�A��A�VA�
=A�%A���A��A��A��;A���A���A�Aδ9Aΰ!AΩ�AΝ�AΕ�A΍PA·+A�|�A�jA�\)A�G�A�/A�  A��A��
AͰ!AͅA�`BA�JA���A̍PA�ffA�=qA��A�{A�bA�JA�JA�
=A�A��A��TA��HA��/A��/A��#A��#A�ȴA˸RA˲-AˬA˩�A˩�A˥�Aˣ�Aˣ�Aˣ�A˟�Aˡ�A˟�A˝�A˝�A˝�A˛�A˙�A˗�A˗�A˗�A˕�A˓uA˓uAˍPA�x�A�ZA�O�A�I�A�E�A�C�A�A�A�?}A�;dA�;dA�9XA�5?A�1'A�-A�+A�&�A�$�A�&�A�$�A��A��A�{A�bA�VA�JA�
=A�A���A���A���A��A��A��A��A��yA��mA��TA��;A��A���A���A�ƨA�ĜA�ƨA�ĜA�ĜA�A���A���AʼjAʸRAʴ9Aʰ!AʮAʧ�Aʧ�Aʥ�Aʣ�Aʝ�Aʙ�Aʙ�Aʛ�Aʙ�Aʗ�Aʕ�AʑhAʉ7AʅAʃAʃAʃAʅAʅAʃAʃAʃAʃAʁA�|�A�v�A�p�A�hsA�ffA�`BA�S�A�M�A�I�A�?}A� �A���A��#AɋDA�M�A�bA�  A��TA�ƨAȸRAȩ�AȑhAȅA�~�A�ffA�A�A� �A���A��
AǾwAǬAǓuA�x�A�\)A�A�A�1'A�(�A��A�bA�%A��HA���AƾwAƮAƝ�A�|�A�p�A�Q�A�33A�oA��mA�ƨAżjAŬAőhA�hsA�O�A�?}A�5?A�"�A�1A�  A��A��A�ȴAĺ^AĴ9Aİ!AĬAĩ�Aħ�Aĥ�Aġ�Aě�Aĕ�Aĕ�Aĕ�Aę�Aę�Aė�AđhAď\Aĉ7Ać+AăA�|�A�p�A�^5A�?}A�+A�$�A��A�{A�VA�JA�JA�
=A�1A�%A�  A��A��A��A��yA��HA��
A���AüjAú^AøRAöFAô9AöFAöFAîAá�AÛ�AÛ�AÙ�A×�AÕ�AÕ�AÕ�AÓuAÑhAÑhAÑhAÓuAÑhAÏ\AËDAÃA�z�A�v�A�t�A�t�A�x�A�t�A�bNA�Q�A�I�A�G�A�?}A�7LA�/A�$�A��A��A�1A��A��yA��
A���A�ȴA�ĜA�ĜA�ĜA���A¾wAº^A¸RA¸RA¸RAº^A¸RA´9A°!A¬A¬A®A¬A¥�A�A�A\AA�|�A�x�A�t�A�l�A�dZA�XA�K�A�C�A�;dA�(�A��A�JA�A���A��A��A���A���A�ȴA�ƨA�A���A���A�A�A��FA���A��PA��A�z�A�p�A�jA�jA�hsA�ffA�bNA�\)A�\)A�\)A�\)A�\)A�XA�XA�VA�Q�A�I�A�9XA�$�A��A���A��mA��;A��
A���A�A��-A���A��PA��7A��A�t�A�VA�&�A�VA���A��mA���A��9A���A��DA��A�t�A�hsA�S�A�G�A�;dA���A���A�p�A�dZA�ZA�ZA�S�A�K�A�G�A�=qA�5?A�+A� �A��A�oA�
=A�%A��A��
A���A�ĜA��-A���A�z�A�p�A�^5A�1A�9XA��/A�dZA�{A��TA��wA���A��7A�p�A�M�A�?}A�9XA�(�A�v�A�/A�ƨA��A��A�ZA��A�VA�A��A��TA���A���A���A��!A���A��uA��7A�l�A�E�A�-A��mA���A���A��9A��+A�r�A�Q�A�7LA�
=A�JA���A���A���A�l�A�Q�A�5?A�bA��;A��-A���A��uA���A��7A�v�A�\)A�C�A�/A� �A�{A�
=A�%A���A���A��RA���A��A��A���A���A�
=A�&�A���A��A���A�Q�A��A�%A���A��/A���A��A�r�A�M�A�+A�%A��/A��A�t�A�I�A��HA���A�E�A��A��RA�ffA��A���A��TA���A��+A�;dA�bA��HA��-A���A��A�t�A�bNA�?}A� �A��A�
=A��A�t�A���A�t�A��A�A��TA���A���A��!A�~�A�S�A� �A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                     G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
|�B
}"B
|�B
|�B
|�B
|�B
{�B
{B
|�B
y�B
xB
v�B
sMB
c�B
W
B
YB
c�B
r|B
|PB
��B
�.B
��B
�?B
�B
ȀB
�B
�RB
�<B
�,B
ԕB
��B
֡B
�9B
��B
՛B
��B
��B
ҽB
��B
�BB
��B
��B
�0B
�pB
��B
�BB
�[B
�EB
�B
�pB
�BB
��B
�B
��B
��B
�B
�B
�B
��B
��B
�"B�BB�B�B:B�BVB'RB,�B0�B7�B9�B;�BHB[�BncB{�B��B��B�tB��BȴB�B��B�jB��BҽB��B� BӏB�HB�B�<B��B��B�>B�B��B�B�BB�B�B�NB�B�B��B��Bf2BJ�B,�B�B�B;B
�oB
��B
��B
�3B
�\B
d�B
,=B
FB	�vB	�vB	�)B	��B	��B	�B	kB	a�B	\]B	U�B	M6B	HKB	>BB	>B	2-B	0�B	3�B	0�B	0�B	/B	<�B	7B	33B	0UB	!�B	�B	/�B	.}B	VB	�B	$B	$@B	%zB	4�B	E9B	K�B	VmB	VmB	P�B	K�B	HKB	;dB	8�B	6�B	/OB	)_B	,B	6B	/�B	-wB	)�B	'�B	(�B	)�B	,qB	.}B	.�B	'�B	&�B	%�B	%zB	$@B	$tB	&�B	+�B	3�B	1�B	,�B	+6B	'�B	%�B	!�B	FB	�B	�B	
�B		�B	DB	�B	�B	fB	�B	�B	�B	�B	�B	:B	�B	�B	�B	�B	�B	$B	�B	MB	B	�B	B	7B	�B	B	�B	�B	~B	 �B	 �B	#�B	�B	+B	-B	0�B	49B	4B	4nB	4�B	4nB	8�B	:*B	@B	B�B	DgB	GB	G�B	QNB	U�B	W?B	ZB	[�B	_B	c�B	g8B	h�B	jKB	k�B	k�B	m�B	m�B	m�B	n�B	o5B	poB	qB	qB	qB	qAB	q�B	tTB	u�B	wfB	zB	y�B	z�B	}�B	}�B	~(B	�B	�1B	��B	��B	��B	��B	�B	�:B	� B	��B	�B	�B	�YB	�=B	�IB	��B	��B	�-B	�4B	�B	��B	�eB	��B	��B	��B	�hB	��B	��B	�B	��B	�^B	��B	�^B	��B	�dB	��B	�B	��B	�jB	�jB	�6B	��B	�}B	�OB	�OB	��B	B	�tB	�?B	�zB	ʌB	�6B	бB	� B	�[B	�2B	�mB	��B	خB	�KB	��B	ܒB	��B	�]B	ݘB	�B	�B	�|B	�vB	��B	�,B	�,B	�8B	�B	�B	��B	�)B	�]B	��B	��B	��B	��B	�/B	�B	� B	�B	��B	��B	��B	��B	��B	��B	�xB	�B	��B	��B	�PB	�"B	��B	��B	�"B	�VB	��B	�(B	��B	��B
 4B
 iB
;B
�B
�B
�B
�B
;B
 �B
oB
;B
�B
B
{B
B
SB
�B
�B
�B
�B
	�B

�B
B
DB
DB
�B
JB
(B
.B
�B
hB
�B
�B
�B
B
oB
B
B
B
�B
�B
�B
B
uB
B
�B
�B
�B
�B
uB
uB
�B
�B
B
B
B
�B
FB
�B
FB
�B
�B
�B
{B
B
�B
�B
$B
�B
�B
�B
�B
�B
+B
�B
�B
eB
_B
+B
�B
+B
_B
1B
�B
B
B
B
7B
�B
�B
=B
�B
~B
�B
B
~B
B
IB
!B
 �B
!bB
 �B
 �B
!bB
!�B
!-B
 \B
 \B
 \B
 �B
VB
VB
VB
 'B
VB
VB
�B
�B
 'B
!-B
 �B
 �B
!bB
!-B
"4B
!�B
#B
"4B
"4B
"�B
"�B
$@B
$B
$tB
$tB
$tB
$tB
$�B
%FB
%FB
%FB
%B
%B
%B
%B
$�B
&�B
&�B
'�B
'RB
(�B
(�B
(�B
(�B
)_B
)_B
)�B
*0B
*0B
)�B
*�B
+6B
+6B
+B
,B
,=B
+kB
+�B
-wB
-CB
.B
.�B
.�B
.�B
.�B
.�B
.}B
/B
/B
/�B
.�B
0�B
/�B
0�B
0�B
0�B
1'B
0�B
1[B
1[B
0�B
1[B
1[B
1[B
1�B
1'B
1�B
1'B
1'B
2-B
49B
3�B
3�B
3hB
4nB
5B
49B
5tB
5B
6B
5tB
6B
5�B
5B
4�B
5?B
6FB
5�B
5�B
6�B
6zB
7�B
7�B
7�B
8RB
:*B
9�B
:^B
:*B
9�B
:*B
:�B
;dB
<�B
=B
=�B
>wB
?�B
@OB
@B
@OB
AUB
AUB
@�B
@�B
A�B
A�B
A�B
B'B
AUB
@OB
B�B
C�B
CaB
D�B
D�B
DgB
D3B
EB
EmB
FB
E9B
E9B
E�B
E9B
E�B
F�B
F?B
G�B
G�B
GzB
GEB
G�B
G�B
G�B
G�B
G�B
HB
H�B
J#B
JXB
J�B
J�B
J�B
JXB
JXB
J�B
K^B
K)B
K�B
K�B
K�B
K�B
L0B
LdB
L�B
MjB
M6B
M6B
M�B
NB
M�B
N<B
N�B
N�B
PB
P}B
QB
QNB
P�B
QB
Q�B
Q�B
Q�B
P�B
P�B
QB
P�B
R�B
T,B
S�B
S�B
T�B
S�B
T�B
T�B
T�B
T�B
U2B
U�B
U2B
T�B
T�B
T�B
V9B
V�B
U�B
U�B
VB
VB
VB
VB
W?B
XB
X�B
X�B
X�B
XEB
X�B
X�B
XB
XyB
W�B
W�B
V�B
W?B
W�B
XEB
X�B
ZQB
[#B
Z�B
Z�B
[#B
[WB
[WB
[�B
[�B
[#B
Z�B
\]B
\)B
\�B
\�B
\�B
\�B
\�B
\�B
]dB
]�B
]dB
]�B
_B
^�B
^�B
_;B
^�B
_pB
_�B
_�B
_�B
_pB
^�B
^�B
_B
_�B
_pB
_�B
_pB
_B
_�B
`BB
`BB
`BB
_pB
`B
a�B
a�B
bB
aHB
a�B
a�B
a|B
a|B
a�B
a|B
a�B
bNB
b�B
b�B
b�B
c B
c�B
c�B
d&B
e�B
e`B
e,B
e,B
ffB
f�B
g8B
g�B
gB
gmB
g�B
g�B
gmB
g�B
hsB
h
B
g�B
h
B
h�B
h�B
hsB
h�B
h�B
h>B
iB
iDB
i�B
i�B
i�B
jKB
jB
kB
kB
j�B
k�B
k�B
kB
kQB
k�B
lWB
l"B
m)B
m]B
m)B
l�B
l�B
m)B
m)B
n/B
ncB
m�B
ncB
ncB
o B
n�B
n�B
o B
o�B
o�B
o�B
poB
p�B
qB
qAB
qB
q�B
rGB
sB
sB
sB
sB
sB
s�B
tTB
tB
s�B
s�B
tTB
tTB
s�B
tTB
t�B
tTB
t�B
u%B
uZB
u%B
t�B
u%B
uZB
u%B
u%B
uZB
u%B
t�B
uZB
u�B
v+B
w�B
v�B
w�B
w�B
w2B
wfB
wfB
w�B
xlB
x8B
xB
x�B
x�B
x�B
x�B
y>B
yrB
yrB
z�B
zDB
zDB
zxB
zDB
y�B
{B
{B
{�B
{B
{�B
{�B
{B
{�B
{�B
{�B
|�B
{�B
|PB
|�B
|�B
}�B
}�B
}"B
}VB
}�B
~�B
~�B
~(B
~(B
~�B
~�B
.B
.B
�B
�B
�B
�B
�B
.B
~�B
~�B
�B
��B
��B
��B
��B
��B
�;B
��B
�oB
�;B
�oB
��B
��B
��B
��B
�B
�B
��B
�AB
�B
�uB
��B
�AB
�AB
�uB
��B
��B
�B
��B
�{B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
�YB
|PB
}VB
|�B
}�B
}"B
~(B
}�B
~(B
~�B
}VB
}�B
}�B
~(B
~]B
}�B
}VB
}"B
}�B
}�B
}VB
}�B
|�B
}�B
|�B
~(B
zDB
~�B
z�B
wfB
{�B
{JB
zxB
{B
zB
�SB
y	B
y>B
xlB
zB
w2B
xB
w2B
xlB
u�B
v�B
t�B
zDB
v�B
x�B
u�B
qvB
sMB
tB
q�B
s�B
r|B
n/B
poB
ffB
y>B
[WB
]�B
[#B
X�B
W�B
XyB
VmB
UgB
VmB
U�B
V9B
VmB
VB
V�B
YKB
Z�B
ZB
]dB
^jB
^�B
_�B
`�B
e�B
f2B
h
B
lWB
k�B
k�B
m�B
q�B
u�B
v+B
zB
yrB
y�B
{JB
}"B
}�B
}�B
|�B
~(B
|B
~(B
�;B
�oB
�AB
�B
�%B
��B
�1B
��B
��B
��B
��B
�B
�eB
��B
��B
�4B
��B
��B
�zB
��B
�aB
��B
�zB
ƨB
�?B
�B
��B
ƨB
�B
�KB
��B
�XB
��B
��B
ȀB
ȀB
��B
��B
��B
ǮB
ǮB
ǮB
ǮB
�zB
ǮB
�B
�zB
�B
ǮB
��B
�B
�KB
�KB
ȴB
��B
��B
�)B
�B
�<B
��B
�B
�NB
�9B
�mB
��B
ԕB
�,B
�,B
�aB
�,B
ԕB
��B
��B
��B
�gB
��B
�gB
�2B
՛B
��B
՛B
��B
��B
��B
�B
�9B
�9B
��B
�?B
�?B
�sB
�
B
�
B
��B
�9B
��B
��B
�aB
�2B
��B
�gB
�B
�
B
֡B
�9B
�B
�9B
�B
��B
�gB
��B
ԕB
ԕB
��B
��B
��B
�gB
��B
�2B
�gB
��B
�aB
ӏB
�TB
�TB
�TB
��B
�&B
�,B
��B
�TB
уB
�B
бB
�}B
�B
��B
��B
�vB
�B
�HB
�HB
�B
�B
��B
�pB
��B
ʌB
ȴB
ɆB
�jB
ȀB
��B
�B
�}B
�B
��B
��B
ʌB
��B
��B
��B
ɆB
�B
ΥB
�pB
�jB
ΥB
�vB
�B
�0B
�B
��B
�B
ΥB
�pB
�jB
��B
�0B
�jB
бB
͟B
�B
�pB
�B
�B
�B
�B
��B
�B
֡B
ҽB
� B
��B
�mB
�B
�sB
�B
רB
ٴB
چB
�KB
�#B
�jB
ݘB
�;B
�pB
�pB
�jB
ޞB
�5B
ޞB
�;B
��B
�B
��B
�vB
�;B
�pB
ߤB
��B
�HB
�NB
��B
�NB
� B
�,B
�sB
�QB
�B
�B
�B
�B
�B
�QB
�QB
�"B
�B
��B
�/B
�cB
��B
�)B
��B
��B
�oB
�B
�vB
�B
��B
�B
��B
�B
�;B
�B
�MB
�B
�B
��B
�TB
�MB
�B
�B
��B
��B
��B
��B
�B
�B
�B
��B
��B
��B
��B
�fB
��B
��B
�8B
��B
��B
��B
�"B
��B
��B
�.B �B�BuB�B	lB�B�BB"B"B�B�BVB�B\B�B�B�B�B�B.B4B�B�BbB.B4BBFB�B_B_B+B1B	B�BOB!BOBVB#:B%B$�B%zB&�B(�B-B,�B,B,=B,B-CB.B-wB,qB,=B/�B4B49B5B5�B7LB8�B8B8�B7�B8�B:*B9�B9XB8�B8�B:*B:^B:^B:�B;dB>BA�BB�BH�BGEBH�BJ�BL0BO�BQ�BT�BZ�Ba|BdZBh�Bo5Bp�Bm]Bl�Bn�BpBt�By>Bz�B|�B.B.B�B�B�4B�VB��B�$B�YB��B�SB��B��B�eB�7B�=B��B��B�nB�zB�B�RB��B��B��B��B��B��B��B�zB��B��B��B��B��B�KB�tB�zB�tB�B�gBŢB�mB�'B��B�8BȴBϫB�)B�pBΥB�}B�dB�^B�0B˒B��B��BʌB�XB��B��BɺB��B��BɆB� BJB�gB��B҉B�vBѷB� B��B�#B��B��B�B�)BѷB�mB�6B��BбB��BϫB�B��B��B֡B�HBуB�}BбB�vB��B�jBбB�jBȴB�RB�5B�B�/B��B�B��B�cB�B�B�yB�B��B��B�B�`B��B�B��B�fB�B��B�|B��B�WB�#B� B��B�B� B�B�B�jBߤB�B�NB��B�jB��B��BخB��B��B�QB��B�gB՛B�B�KB�B�B�dB֡B�B��BҽB�BخB�gB��B�gG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                     B
t�B
u.B
t�B
t�B
t�B
t�B
s�B
s"B
t�B
q�B
pB
o	B
kYB
[�B
OB
Q�B
[�B
j�B
t\B
z�B
�:B
��B
�KB
�#B
��B
�#B
�^B
�HB
�8B
̡B
��B
έB
�EB
��B
ͧB
�
B
�B
��B
��B
�NB
�B
��B
�<B
�|B
��B
�NB
�gB
�QB
�B
�|B
�NB
��B
�(B
��B
��B
�B
�B
��B
��B
�	B
�.B
��B!B�BB
FB�BbB^B$�B(�B/�B1�B3�B@#BS�BfoBs�B��B��B��B�B��B�B��B�vB�B��B��B�,B˛B�TB�B�HB��B��B�JBݡB��B��B�NB�)BыB�ZB�B�&B�B��B^>BB�B$�B�B�B
�GB
�{B
��B
��B
�?B
�hB
]B
$IB
RB	�B	؂B	�5B	��B	��B	�B	c(B	Y�B	TiB	M�B	EBB	@WB	6NB	6B	*9B	(�B	+�B	(�B	(�B	''B	4�B	/#B	+?B	(aB	�B	�B	'�B	&�B	bB	�B	B	LB	�B	,�B	=EB	DB	NyB	NyB	H�B	C�B	@WB	3pB	0�B	.�B	'[B	!kB	$B	.B	'�B	%�B	"B	�B	!B	!�B	$}B	&�B	&�B	�B	�B	�B	�B	LB	�B	�B	#�B	+�B	)�B	$�B	#BB	�B	�B	�B	RB	�B	�B	�B	�B	PB��B	 	B	 rB��B	�B	�B	�B		�B	
FB	B	B	�B	�B	�B	0B	�B	YB	*B	�B	B	CB	�B	B	�B	�B	�B	�B	�B	�B	�B	#B	%B	(�B	,EB	,B	,zB	,�B	,zB	0�B	26B	8&B	;B	<sB	?B	?�B	IZB	M�B	OKB	R)B	S�B	WB	[�B	_DB	`�B	bWB	c�B	c�B	e�B	e�B	e�B	f�B	gAB	h{B	iB	iB	iB	iMB	i�B	l`B	m�B	orB	rB	q�B	r�B	u�B	v B	v4B	|%B	�=B	�	B	�	B	��B	��B	�!B	�FB	�B	��B	�$B	�*B	�eB	�IB	�UB	��B	��B	�9B	�@B	�B	��B	�qB	��B	��B	��B	�tB	��B	��B	�B	�B	�jB	��B	�jB	��B	�pB	��B	�B	��B	�vB	�vB	�BB	��B	��B	�[B	�[B	��B	��B	��B	�KB	��B	B	�BB	ȽB	�,B	�gB	�>B	�yB	��B	кB	�WB	��B	ԞB	�B	�iB	դB	�B	�B	وB	؂B	��B	�8B	�8B	�DB	�B	�B	��B	�5B	�iB	��B	�B	�B	�B	�;B	�B	�B	��B	��B	��B	��B	�	B	�B	��B	�B	�"B	��B	��B	�\B	�.B	��B	��B	�.B	�bB	��B	�4B	��B	��B	�@B	�uB	�GB	��B	��B	��B	��B	�GB	��B	�{B	�GB	��B	�B	��B	�+B	�_B	��B	��B	�B	��B
�B
�B
B
PB
PB
�B
VB
4B
:B
�B
	tB
	�B
	�B
	�B

B

{B
B
B
B

�B

�B

�B
B
�B
B
�B

�B

�B

�B
�B
�B
�B
�B
B
B
B
�B
RB
�B
RB
�B
�B
�B
�B
$B
�B
�B
0B
�B
�B
B
B
�B
7B
B
B
qB
kB
7B
�B
7B
kB
=B
�B
B
B
B
CB
�B
�B
IB
�B
�B
�B
B
�B
!B
UB
-B
�B
nB
�B
B
nB
�B
9B
hB
hB
hB
�B
bB
bB
bB
3B
bB
bB
�B
�B
3B
9B
�B
B
nB
9B
@B
�B
B
@B
@B
�B
�B
LB
B
�B
�B
�B
�B
�B
RB
RB
RB
B
B
B
B
�B
�B
�B
�B
^B
 �B
 �B
 �B
 �B
!kB
!kB
!�B
"<B
"<B
"B
"�B
#BB
#BB
#B
$B
$IB
#wB
#�B
%�B
%OB
& B
&�B
&�B
&�B
&�B
&�B
&�B
''B
''B
'�B
&�B
(�B
'�B
(�B
(�B
(�B
)3B
(�B
)gB
)gB
(�B
)gB
)gB
)gB
)�B
)3B
*B
)3B
)3B
*9B
,EB
+�B
+�B
+tB
,zB
-B
,EB
-�B
-B
.B
-�B
.B
-�B
-B
,�B
-KB
.RB
-�B
-�B
.�B
.�B
/�B
/�B
/�B
0^B
26B
2B
2jB
26B
2B
26B
2�B
3pB
4�B
5B
5�B
6�B
7�B
8[B
8&B
8[B
9aB
9aB
8�B
8�B
9�B
9�B
9�B
:3B
9aB
8[B
:�B
;�B
;mB
<�B
<�B
<sB
<?B
=B
=yB
>B
=EB
=EB
=�B
=EB
=�B
>�B
>KB
?�B
?�B
?�B
?QB
?�B
?�B
?�B
?�B
?�B
@#B
@�B
B/B
BdB
B�B
CB
B�B
BdB
BdB
CB
CjB
C5B
DB
C�B
C�B
DB
D<B
DpB
D�B
EvB
EBB
EBB
E�B
FB
E�B
FHB
F�B
F�B
H B
H�B
I&B
IZB
H�B
I&B
I�B
I�B
I�B
H�B
H�B
I&B
H�B
J�B
L8B
K�B
LB
L�B
LB
M
B
M
B
L�B
L�B
M>B
M�B
M>B
L�B
L�B
L�B
NEB
N�B
M�B
M�B
NB
NB
NB
NB
OKB
PB
P�B
P�B
P�B
PQB
P�B
P�B
PB
P�B
O�B
O�B
N�B
OKB
O�B
PQB
P�B
R]B
S/B
R�B
R�B
S/B
ScB
ScB
S�B
S�B
S/B
R�B
TiB
T5B
UB
T�B
T�B
T�B
T�B
UB
UpB
U�B
UpB
U�B
WB
V�B
V�B
WGB
V�B
W|B
W�B
W�B
W�B
W|B
V�B
V�B
WB
W�B
W|B
W�B
W|B
WB
W�B
XNB
XNB
XNB
W|B
XB
Y�B
Y�B
Z%B
YTB
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
Y�B
ZZB
Z�B
Z�B
Z�B
[,B
[�B
[�B
\2B
]�B
]lB
]8B
]8B
^rB
^�B
_DB
_�B
_B
_yB
_�B
_�B
_yB
_�B
`B
`B
_�B
`B
`�B
`�B
`B
`�B
`�B
`JB
aB
aPB
a�B
a�B
a�B
bWB
b�B
c(B
c(B
b�B
c�B
c�B
c(B
c]B
c�B
dcB
d.B
e5B
eiB
e5B
d�B
d�B
e5B
e5B
f;B
foB
e�B
foB
foB
gB
f�B
f�B
gB
g�B
g�B
g�B
h{B
h�B
iB
iMB
iB
i�B
jSB
k%B
k%B
k%B
k%B
k%B
k�B
l`B
l+B
k�B
k�B
l`B
l`B
k�B
l`B
l�B
l`B
l�B
m1B
mfB
m1B
l�B
m1B
mfB
m1B
m1B
mfB
m1B
l�B
mfB
m�B
n7B
o�B
o	B
o�B
o�B
o>B
orB
orB
o�B
pxB
pDB
pB
p�B
p�B
p�B
p�B
qJB
q~B
q~B
r�B
rPB
rPB
r�B
rPB
q�B
s�B
s"B
s�B
s"B
s�B
s�B
s�B
s�B
s�B
s�B
t�B
s�B
t\B
t�B
t�B
u�B
v B
u.B
ubB
u�B
v�B
v�B
v4B
v4B
v�B
v�B
w:B
w:B
w�B
w�B
w�B
w�B
w�B
w:B
wB
wB
w�B
x�B
x�B
x�B
x�B
y�B
yGB
y�B
y{B
yGB
y{B
x�B
x�B
y�B
z�B
{B
{B
z�B
zMB
zB
z�B
z�B
zMB
zMB
z�B
{�B
{�B
|%B
{�B
{�B
{�B
{�B
|%B
|�B
}�B
}�B
}�B
}�B
}�B
~�B
~eB
t\B
ubB
t�B
u�B
u.B
v4B
v B
v4B
v�B
ubB
v B
v B
v4B
viB
u�B
ubB
u.B
v B
v B
ubB
u�B
t�B
u�B
t�B
v4B
rPB
wB
r�B
orB
s�B
sVB
r�B
s�B
rB
}_B
qB
qJB
pxB
rB
o>B
pB
o>B
pxB
nB
o	B
l�B
rPB
o	B
p�B
m�B
i�B
kYB
l+B
i�B
k�B
j�B
f;B
h{B
^rB
qJB
ScB
U�B
S/B
P�B
O�B
P�B
NyB
MsB
NyB
M�B
NEB
NyB
NB
N�B
QWB
R�B
R)B
UpB
VvB
V�B
W�B
X�B
^
B
^>B
`B
dcB
c�B
c�B
e�B
i�B
m�B
n7B
rB
q~B
q�B
sVB
u.B
u�B
v B
t�B
v4B
t(B
v4B
yGB
y{B
zMB
}+B
~1B
�	B
�=B
��B
��B
��B
��B
�B
�qB
��B
��B
�@B
��B
��B
��B
��B
�mB
�
B
��B
��B
�KB
�B
��B
��B
�B
�WB
��B
�dB
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
�B
��B
�#B
��B
��B
�#B
�WB
�WB
��B
��B
��B
�5B
�B
�HB
��B
� B
�ZB
�EB
�yB
�
B
̡B
�8B
�8B
�mB
�8B
̡B
��B
��B
�
B
�sB
�
B
�sB
�>B
ͧB
�
B
ͧB
��B
��B
��B
�B
�EB
�EB
��B
�KB
�KB
�B
�B
�B
��B
�EB
��B
��B
�mB
�>B
��B
�sB
�B
�B
έB
�EB
�B
�EB
�B
��B
�sB
��B
̡B
̡B
��B
�
B
�
B
�sB
�
B
�>B
�sB
�
B
�mB
˛B
�`B
�`B
�`B
��B
�2B
�8B
��B
�`B
ɏB
�&B
ȽB
ȉB
� B
��B
��B
ǂB
� B
�TB
�TB
�&B
� B
��B
�|B
��B
B
��B
��B
�vB
��B
�B
� B
ȉB
�B
��B
��B
B
��B
��B
�B
��B
�)B
ƱB
�|B
�vB
ƱB
ǂB
�B
�<B
�B
��B
�B
ƱB
�|B
�vB
��B
�<B
�vB
ȽB
ūB
�B
�|B
�B
�&B
�B
�B
��B
� B
έB
��B
�,B
��B
�yB
�B
�B
�B
ϴB
��B
ҒB
�WB
�/B
�vB
դB
�GB
�|B
�|B
�vB
֪B
�AB
֪B
�GB
��B
ضB
��B
؂B
�GB
�|B
װB
��B
�TB
�ZB
��B
�ZB
�,B
�8B
�B
�]B
�B
�B
�"B
�B
�(B
�]B
�]B
�.B
�B
� B
�;B
�oB
��B
�5B
��B
��B
�{B
�B
�B
�B
��B
�B
��B
�B
�GB
�B
�YB
�B
�B
��B
�`B
�YB
�B
�B
��B
��B
��B
��B
�B
�B
�+B
��B
�B
��B
��B
�rB
��B
��B
�DB
��B
��B
��B
�.B
��B
��B
�:B
��B
��B
��B
��BxB 	B�B'B.B.B�B�BbB�BhBB�B�B�B�B:B	@B	�B�BnB:B	@BBRB�BkBkB7B=BB�B[B-B[BbBFBB�B�B�B �B%B$�B$B$IB$B%OB& B%�B$}B$IB'�B,B,EB-B-�B/XB0�B0)B0�B/�B0�B26B2B1dB0�B0�B26B2jB2jB2�B3pB6B9�B:�B@�B?QB@�BB�BD<BG�BI�BM
BR�BY�B\fB`�BgABh�BeiBe Bf�BhBl�BqJBr�Bt�Bw:Bw:BzByBx@B�bB��B�0B�eB��B�_B��B�B�qB�CB�IB��B��B�zB��B�$B�^B��B��B��B��B�B��B��B��B��B��B��B��B��B�WB��B��B��B�B�sB��B�yB�3B��B�DB��BǷB�5B�|BƱBȉB�pB�jB�<BÞB�B��BB�dB��B��B��B��B��B��B�,BVB�sB�BʕBǂB��B�,B��B�/B��B�B�&B�5B��B�yB�BB�
BȽB��BǷB�B��B��BέB�TBɏBȉBȽBǂB��B�vBȽB�vB��B�^B�AB �B�;B��B�B�B�oB�B�%B�B�B�
B��B�B�lB�B�B��B�rBܛB��BوB��B�cB�/B�,B��B��B�BضB۔B�vBװB۔B�ZB�B�vB��B��BкB��B��B�]B��B�sBͧB�B�WB�B۔B�pBέB�B��B��B�#BкB�sB�B�sG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                     G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230501223653                            20230501223653AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023050122365320230501223653  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023050122365320230501223653QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023050122365320230501223653QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               