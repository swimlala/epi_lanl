CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:32Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230426223232  20230426223232  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @���Z�H@���Z�H11  @��  �@��  �@/7�T7@/7�T7�dcm��8��dcm��8�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AB  AB  ?�=q@   @@  @�  @�  @��R@޸RA ��A��A\)A*�HA@  A`��A�  A�\)A�  A���A���A�Q�A�  A�  B (�B  B  B  B   B'�
B/�
B8  B@Q�BH  BO�
BW�B`  BhQ�Bp(�Bx  B�  B�{B��B��B�  B�  B��B��B�  B��B��B��B�{B�(�B�{B��
B��
B�{B�(�B�  B��
B��B��
B�{B�{B�  B��B�  B�{B�{B�=qB�{B��
C�C��C  C��C	�HC�C�HC�C  C
=C{C  C
=C
=C  C 
=C"  C#��C&  C(
=C*  C+�C-��C0  C1��C4  C6{C8
=C:
=C<
=C>
=C?��CB  CD  CE��CH
=CJ
=CL
=CN  CP  CQ��CS�CU�CX  CZ
=C\
=C^
=C`  Cb
=Cd  Cf  Cg��Cj  Ck��Cm��Co��Cq��Cs��Cv
=Cx
=Cz  C|
=C~{C�C�  C�  C���C���C���C�  C�  C�C���C��C���C�\C�
=C�  C�  C�  C���C�  C�  C���C���C�
=C�  C��C���C�C�\C�C���C���C���C���C�  C�  C�C�C�  C���C���C�C�  C�  C���C���C�
=C�  C�  C�
=C�  C�  C�  C�  C�  C�
=C�C�  C�
=C�  C���C�  C�  C�  C���C���C�C�  C���C�C�C�  C�C�  C���C���C�  C�C�  C���C���C���C���C�  C�  C�  C�  C�  C�  C�
=C�
=C���C���C���C���C�  C�C�
=C�C��C�  C�C�  C���C���C���C��C���C���C���C���C��C���C�  C�
=C�C�C�  C���C���C�  C�
=C�
=C�C�  C�  C�  C�  C���D �D � D�D��D  Dz�D�qD� D�D��D�D��D�D�D�D� D  D��D	�D	�D
D
�D�D��D�D}qD�qD��D�D� D�RDu�D�qD�D�qDz�D�Dz�D�qD��D  D}qD�D� D�RD}qD  Dz�D�qD�DD� D��Dz�D��D}qD  D� D  Dz�D��Dz�D�qDz�D�RD }qD ��D!� D"�D"� D"��D#}qD$  D$}qD%�D%� D%��D&� D&��D'� D'��D(}qD(�qD)� D)�qD*� D+�D+}qD,  D,}qD,��D-� D.D.� D/�D/��D0  D0��D1D1��D1�qD2}qD3  D3}qD4�D4�D5�D5� D5�qD6}qD6�qD7� D8�D8}qD9�D9}qD9��D:��D:�qD;��D<  D<��D=  D=�D>  D>}qD?�D?� D?�qD@�DA  DA}qDA�qDB� DC  DC��DD�DD� DE  DE� DF�DF��DF�qDG}qDH�DH� DI  DI� DI�qDJ�DJ�qDK}qDL  DL� DM  DM� DN  DN��DN�qDO��DP  DP� DQ  DQ}qDR�DR��DS�DS��DS�qDT}qDU  DU}qDU�qDV}qDW  DW��DW�qDX� DY�DY�DZ  DZ}qD[  D[}qD[�qD\� D]�D]� D]�qD^}qD_  D_z�D_��D`}qDa  Da}qDa�qDb��Dc  Dc}qDd�Dd� Dd�qDe� De�qDfz�Dg  Dg� Dg�qDh}qDi  Di��Dj�Dj�Dj��Dk� Dk�qDl}qDm�Dm��Dn�Dn��Do  Do� Do�qDp��Dq�Dq}qDr  Dr��Ds�Ds� Dt  Dt� Du�Du� Dv  Dv� Dw�Dw��Dx  Dx��Dx�qDy}qDzDz}qD{  D{}qD|D|� D}  D}}qD}�qD~��D  D� D�  D�=qD�� D�D�  D�AHD���D��HD�HD�AHD�� D��HD�  D�=qD�� D��HD�  D�C�D��HD��HD���D�@ D�� D�D�  D�AHD��D��HD�  D�@ D��HD��HD�  D�>�D�� D��HD�  D�@ D���D�� D���D�AHD��HD���D��qD�@ D�~�D���D���D�AHD��HD���D���D�=qD�}qD�� D��qD�B�D�� D�� D�  D�AHD�~�D�� D��qD�@ D�� D��qD�HD�AHD�~�D��qD�HD�@ D�� D�� D��qD�>�D�}qD�D���D�@ D���D�� D��qD�@ D�� D�� D�HD�>�D�~�D���D�  D�@ D�~�D��qD�  D�AHD�� D���D��qD�>�D�� D�� D�HD�AHD�� D��HD�  D�=qD�~�D���D��D�AHD��HD�� D�  D�@ D�~�D��HD��D�@ D�|)D�� D��D�B�D�~�D���D�  D�AHD�� D���D�  D�@ D��HD���D�  D�@ D�|)D���D���D�=qD�� D�� D�HD�B�D�� D���D���D�@ D��HD��HD��D�AHD��HD�D�  D�@ D�~�D���D��D�AHD���D�D���D�=qD�}qD�� D�HD�AHD�}qD�� D�HD�@ D��HD�D�HD�@ D�}qD��qD��qD�=qD�� D��HD���D�@ D��HD�� D��qD�=qD�� D�D�HD�AHD�� D�� D�  D�@ D�~�D�D��D�@ D��HD��HD�HD�AHD�� D���D���D�>�D�� D�� D�  D�@ D��HD�� D�  D�>�D�� D���D��D�>�D�~�D�� D�HD�@ D�~�D�� D���D�=qD��HD��HD���D�>�D���D�D�HD�AHD�� D���D�  D�>�D�~�D��HD�HD�>�D�}qD��HD��D�B�D��HD�� D��qD�>�D��HD��HD�HD�@ D�~�D���D�HD�AHD��HD��HD��D�B�D D¾�D��qD�=qD�~�D�� D�  D�>�D�~�D�� D�  D�AHD�~�Dž�D���D�@ DƂ�D��HD���D�=qD�}qD�� D�HD�@ DȀ DȾ�D���D�=qDɀ D�D�HD�>�D�~�D��HD���D�=qD�~�D�� D�  D�@ D́HD��HD�HD�@ D̀ D;�D�HD�AHD΀ Dξ�D���D�@ D�~�DϾ�D��qD�>�DЀ D�� D���D�>�Dр DѾ�D���D�@ DҀ D�� D�HD�B�Dӂ�DӾ�D�  D�B�D�~�D�� D�  D�>�DՂ�D��HD�HD�B�DցHD�� D�  D�AHD׀ D�D���D�>�D؀ D��HD�HD�@ Dق�D��HD���D�AHDځHDھ�D���D�=qDہHD۾�D�HD�B�D܀ D��HD��D�@ D�}qDݾ�D�HD�B�DށHD�� D���D�>�D߀ D�D���D�<)D�~�D�� D�HD�@ D�HDᾸD��qD�>�D� D�� D�HD�AHD�HD��HD�  D�>�D� D�� D�  D�>�D�~�D�qD�  D�AHD� D��HD�HD�@ D�~�D�qD�  D�@ D� D辸D���D�@ D�~�D��HD�HD�>�D�HD�� D�  D�B�D�~�D�� D�  D�>�D� D��HD�  D�B�D��D���D�HD�@ D� D�� D�  D�@ D�HD�� D���D�@ D�~�D�qD���D�@ D�HD�D�HD�@ D�D��HD�HD�B�D�D�� D�  D�>�D� D��HD�HD�AHD�~�D���D�HD�AHD�� D�� D�HD�AHD�� D���D�  D�>�D�� D��HD�  D�AHD�� D��qD��)D�>�D�~�D��=?8Q�?B�\?�  ?���?�{?���?��@
=q@
=@#�
@5@B�\@J=q@Y��@h��@s33@��\@��@�33@��H@�G�@���@���@�Q�@�(�@\@˅@�33@�Q�@�\@���@�z�@�(�A ��A�
A�A�A�RA�\A
=A(�A ��A#�
A(Q�A*�HA.{A1�A5A8��A=p�AC33AG
=AJ�HAN{AS33AUAX��A\��A`��Adz�Ai��An�RAr�\AvffAz�HA~{A�Q�A�=qA�z�A��RA���A��HA�p�A�\)A���A��A��A��RA���A��\A�z�A�
=A�G�A��A�A��A���A��HA�z�A��RA���A��\A��A��A��A��
A�A�  A���A�33A�p�A�\)A�G�A��
A�ffA�Q�Aҏ\A��
A�A׮A��A�(�A޸RA��A��HA���A�ffA�Q�A�=qA�(�A�RA�G�A�A�A��A�=qA�33A��A�
=B ��B=qB33BQ�Bp�B=qB33B(�B	�B
=qB\)Bz�B��B�HB�
B��B{B�HBQ�B��B�B�HB  Bp�B�\B�B��B��BffB�B z�B!��B#33B$(�B%p�B&ffB'\)B((�B)G�B*=qB+�B,z�B-�B/
=B0(�B1p�B2=qB333B4(�B5��B6�HB8  B9�B9�B:�HB<  B<��B>=qB?�B@��BABB�\BC�BD��BF{BG\)BHz�BIp�BJ=qBK\)BLz�BM��BO33BP(�BQG�BR{BS33BT(�BUBV�HBX(�BX��BY�BZ�HB\(�B]��B^�HB`  B`��BaBb�HBc�
Bd��Bf�\Bg�
Bh��Bi��Bj�RBk�
BmG�BnffBo�Bp��Bqp�BrffBs�Bu�Bv=qBw�Bx(�ByG�Bz=qB{\)B|��B~{B33B�(�B�z�B�
=B��B�ffB���B�p�B��
B�ffB�
=B��B�=qB��HB��B�B�=qB���B��B�(�B��\B���B��B�(�B���B�\)B��
B�=qB���B�G�B�  B���B�33B��B�  B��\B��B��
B�ffB���B�G�B��
B�Q�B��HB���B�{B���B�
=B��B�{B��RB�G�B��B�(�B��RB�\)B�  B��\B���B�\)B�  B���B�33B���B�  B��\B��B��
B�Q�B��RB�33B��B�ffB���B�p�B�B�Q�B���B��B�{B�z�B���B�\)B�(�B���B�G�B���B�  B��\B�G�B��B�{B���B��B��
B�Q�B��HB��B��B�(�B���B�p�B��B�Q�B��RB�33B��B�z�B��RB�33B��B�ffB���B��B��B�=qB���B�p�B�  B�Q�B��RB�33B��B�z�B��HB�G�B��B�ffB�
=B�\)B��
B�z�B�
=BŅB�  Bƣ�B�33BǙ�B�{Bȏ\B�\)B��B�=qBʸRB�33B�  B�z�B���B�\)B�{BΣ�B��BυB�  B���B�G�B��
B�Q�B��B�B�=qB���BՅB�(�B֣�B��B��B؏\B���Bٙ�B�ffB�
=BۅB�=qB���B�\)B��B���B�33B�B��\B�G�B�B�Q�B��B�B�(�B���B噚B�{B�RB�p�B��B�z�B�G�B��B�=qB���B�B�{B�RB�B�  B�\B�p�B��
B�z�B�\)B�B�z�B�\)B��
B���B�p�B��B���B�\)B��B��HB�\)B��B��HB�G�B�  B��HB�G�B�(�B���B�33C 
=C G�C ��C{CG�C�RC
=CG�C�RC
=CG�CC
=CffC�
C
=Cz�CC
=Cz�C�RC{C�C�RC(�C�CC	33C	z�C	�HC
Q�C
��C
=CQ�C��C
=CffC�HC(�C��C��Cp�CC�C��C�HCffC�C(�Cz�C��C=qC�RC{CffC�C33C�RC��Cz�CC=qCz�C�HC(�C�CC�CG�C��C�RC{C=qC\)C�C��C{C(�Cz�C�\C�HC��CG�C\)C�C��C{CG�Cp�C�C�HC{CQ�Cz�C�C��C
=CffC�C��C�HC33CQ�C��C�RC 
=C �C z�C �\C �HC ��C!Q�C!\)C!�C!��C"�C"=qC"p�C"�RC"�
C#(�C#=qC#�\C#��C#��C$
=C$\)C$z�C$C$�
C%(�C%G�C%�\C%��C%��C&
=C&\)C&ffC&�RC&��C'{C'=qC'\)C'�C'C({C((�C(p�C(�\C(�HC(�C)=qC)\)C)��C)C*
=C*�C*z�C*�C*��C+  C+�C+p�C+�C+�
C+�C,=qC,Q�C,��C,�RC-
=C-�C-p�C-�C-�
C-�C.(�C.ffC.�\C.�
C.�C/33C/\)C/��C/�RC0{C033C0ffC0��C0C1{C133C1�C1�\C1�C2
=C2=qC2�C2��C2�C3  C3\)C3p�C3�C3��C4{C4ffC4p�C4�RC5  C5�C5p�C5�C5C6  C6�C6ffC6�C6�
C6�C733C7ffC7�C7��C7�C8(�C8p�C8�C8�
C8��C9(�C9p�C9�\C9�HC9��C:=qC:z�C:��C:�C;
=C;\)C;p�C;�C;��C<{C<ffC<�C<�
C<�C=33C=p�C=�\C=�HC=��C>Q�C>p�C>��C>�C?
=C?\)C?p�C?�RC?�C@�C@p�C@�C@�RCA
=CA(�CAp�CA�CACB{CBG�CBp�CB�RCB��CC{CC\)CCz�CCCC��CD�CDffCD�CD�RCE  CE(�CEz�CE��CE��CF�CF33CFz�CFCF�CG=qCGz�CG��CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                              ?�=q@   @@  @�  @�  @��R@޸RA ��A��A\)A*�HA@  A`��A�  A�\)A�  A���A���A�Q�A�  A�  B (�B  B  B  B   B'�
B/�
B8  B@Q�BH  BO�
BW�B`  BhQ�Bp(�Bx  B�  B�{B��B��B�  B�  B��B��B�  B��B��B��B�{B�(�B�{B��
B��
B�{B�(�B�  B��
B��B��
B�{B�{B�  B��B�  B�{B�{B�=qB�{B��
C�C��C  C��C	�HC�C�HC�C  C
=C{C  C
=C
=C  C 
=C"  C#��C&  C(
=C*  C+�C-��C0  C1��C4  C6{C8
=C:
=C<
=C>
=C?��CB  CD  CE��CH
=CJ
=CL
=CN  CP  CQ��CS�CU�CX  CZ
=C\
=C^
=C`  Cb
=Cd  Cf  Cg��Cj  Ck��Cm��Co��Cq��Cs��Cv
=Cx
=Cz  C|
=C~{C�C�  C�  C���C���C���C�  C�  C�C���C��C���C�\C�
=C�  C�  C�  C���C�  C�  C���C���C�
=C�  C��C���C�C�\C�C���C���C���C���C�  C�  C�C�C�  C���C���C�C�  C�  C���C���C�
=C�  C�  C�
=C�  C�  C�  C�  C�  C�
=C�C�  C�
=C�  C���C�  C�  C�  C���C���C�C�  C���C�C�C�  C�C�  C���C���C�  C�C�  C���C���C���C���C�  C�  C�  C�  C�  C�  C�
=C�
=C���C���C���C���C�  C�C�
=C�C��C�  C�C�  C���C���C���C��C���C���C���C���C��C���C�  C�
=C�C�C�  C���C���C�  C�
=C�
=C�C�  C�  C�  C�  C���D �D � D�D��D  Dz�D�qD� D�D��D�D��D�D�D�D� D  D��D	�D	�D
D
�D�D��D�D}qD�qD��D�D� D�RDu�D�qD�D�qDz�D�Dz�D�qD��D  D}qD�D� D�RD}qD  Dz�D�qD�DD� D��Dz�D��D}qD  D� D  Dz�D��Dz�D�qDz�D�RD }qD ��D!� D"�D"� D"��D#}qD$  D$}qD%�D%� D%��D&� D&��D'� D'��D(}qD(�qD)� D)�qD*� D+�D+}qD,  D,}qD,��D-� D.D.� D/�D/��D0  D0��D1D1��D1�qD2}qD3  D3}qD4�D4�D5�D5� D5�qD6}qD6�qD7� D8�D8}qD9�D9}qD9��D:��D:�qD;��D<  D<��D=  D=�D>  D>}qD?�D?� D?�qD@�DA  DA}qDA�qDB� DC  DC��DD�DD� DE  DE� DF�DF��DF�qDG}qDH�DH� DI  DI� DI�qDJ�DJ�qDK}qDL  DL� DM  DM� DN  DN��DN�qDO��DP  DP� DQ  DQ}qDR�DR��DS�DS��DS�qDT}qDU  DU}qDU�qDV}qDW  DW��DW�qDX� DY�DY�DZ  DZ}qD[  D[}qD[�qD\� D]�D]� D]�qD^}qD_  D_z�D_��D`}qDa  Da}qDa�qDb��Dc  Dc}qDd�Dd� Dd�qDe� De�qDfz�Dg  Dg� Dg�qDh}qDi  Di��Dj�Dj�Dj��Dk� Dk�qDl}qDm�Dm��Dn�Dn��Do  Do� Do�qDp��Dq�Dq}qDr  Dr��Ds�Ds� Dt  Dt� Du�Du� Dv  Dv� Dw�Dw��Dx  Dx��Dx�qDy}qDzDz}qD{  D{}qD|D|� D}  D}}qD}�qD~��D  D� D�  D�=qD�� D�D�  D�AHD���D��HD�HD�AHD�� D��HD�  D�=qD�� D��HD�  D�C�D��HD��HD���D�@ D�� D�D�  D�AHD��D��HD�  D�@ D��HD��HD�  D�>�D�� D��HD�  D�@ D���D�� D���D�AHD��HD���D��qD�@ D�~�D���D���D�AHD��HD���D���D�=qD�}qD�� D��qD�B�D�� D�� D�  D�AHD�~�D�� D��qD�@ D�� D��qD�HD�AHD�~�D��qD�HD�@ D�� D�� D��qD�>�D�}qD�D���D�@ D���D�� D��qD�@ D�� D�� D�HD�>�D�~�D���D�  D�@ D�~�D��qD�  D�AHD�� D���D��qD�>�D�� D�� D�HD�AHD�� D��HD�  D�=qD�~�D���D��D�AHD��HD�� D�  D�@ D�~�D��HD��D�@ D�|)D�� D��D�B�D�~�D���D�  D�AHD�� D���D�  D�@ D��HD���D�  D�@ D�|)D���D���D�=qD�� D�� D�HD�B�D�� D���D���D�@ D��HD��HD��D�AHD��HD�D�  D�@ D�~�D���D��D�AHD���D�D���D�=qD�}qD�� D�HD�AHD�}qD�� D�HD�@ D��HD�D�HD�@ D�}qD��qD��qD�=qD�� D��HD���D�@ D��HD�� D��qD�=qD�� D�D�HD�AHD�� D�� D�  D�@ D�~�D�D��D�@ D��HD��HD�HD�AHD�� D���D���D�>�D�� D�� D�  D�@ D��HD�� D�  D�>�D�� D���D��D�>�D�~�D�� D�HD�@ D�~�D�� D���D�=qD��HD��HD���D�>�D���D�D�HD�AHD�� D���D�  D�>�D�~�D��HD�HD�>�D�}qD��HD��D�B�D��HD�� D��qD�>�D��HD��HD�HD�@ D�~�D���D�HD�AHD��HD��HD��D�B�D D¾�D��qD�=qD�~�D�� D�  D�>�D�~�D�� D�  D�AHD�~�Dž�D���D�@ DƂ�D��HD���D�=qD�}qD�� D�HD�@ DȀ DȾ�D���D�=qDɀ D�D�HD�>�D�~�D��HD���D�=qD�~�D�� D�  D�@ D́HD��HD�HD�@ D̀ D;�D�HD�AHD΀ Dξ�D���D�@ D�~�DϾ�D��qD�>�DЀ D�� D���D�>�Dр DѾ�D���D�@ DҀ D�� D�HD�B�Dӂ�DӾ�D�  D�B�D�~�D�� D�  D�>�DՂ�D��HD�HD�B�DցHD�� D�  D�AHD׀ D�D���D�>�D؀ D��HD�HD�@ Dق�D��HD���D�AHDځHDھ�D���D�=qDہHD۾�D�HD�B�D܀ D��HD��D�@ D�}qDݾ�D�HD�B�DށHD�� D���D�>�D߀ D�D���D�<)D�~�D�� D�HD�@ D�HDᾸD��qD�>�D� D�� D�HD�AHD�HD��HD�  D�>�D� D�� D�  D�>�D�~�D�qD�  D�AHD� D��HD�HD�@ D�~�D�qD�  D�@ D� D辸D���D�@ D�~�D��HD�HD�>�D�HD�� D�  D�B�D�~�D�� D�  D�>�D� D��HD�  D�B�D��D���D�HD�@ D� D�� D�  D�@ D�HD�� D���D�@ D�~�D�qD���D�@ D�HD�D�HD�@ D�D��HD�HD�B�D�D�� D�  D�>�D� D��HD�HD�AHD�~�D���D�HD�AHD�� D�� D�HD�AHD�� D���D�  D�>�D�� D��HD�  D�AHD�� D��qD��)D�>�D�~�D��=?8Q�?B�\?�  ?���?�{?���?��@
=q@
=@#�
@5@B�\@J=q@Y��@h��@s33@��\@��@�33@��H@�G�@���@���@�Q�@�(�@\@˅@�33@�Q�@�\@���@�z�@�(�A ��A�
A�A�A�RA�\A
=A(�A ��A#�
A(Q�A*�HA.{A1�A5A8��A=p�AC33AG
=AJ�HAN{AS33AUAX��A\��A`��Adz�Ai��An�RAr�\AvffAz�HA~{A�Q�A�=qA�z�A��RA���A��HA�p�A�\)A���A��A��A��RA���A��\A�z�A�
=A�G�A��A�A��A���A��HA�z�A��RA���A��\A��A��A��A��
A�A�  A���A�33A�p�A�\)A�G�A��
A�ffA�Q�Aҏ\A��
A�A׮A��A�(�A޸RA��A��HA���A�ffA�Q�A�=qA�(�A�RA�G�A�A�A��A�=qA�33A��A�
=B ��B=qB33BQ�Bp�B=qB33B(�B	�B
=qB\)Bz�B��B�HB�
B��B{B�HBQ�B��B�B�HB  Bp�B�\B�B��B��BffB�B z�B!��B#33B$(�B%p�B&ffB'\)B((�B)G�B*=qB+�B,z�B-�B/
=B0(�B1p�B2=qB333B4(�B5��B6�HB8  B9�B9�B:�HB<  B<��B>=qB?�B@��BABB�\BC�BD��BF{BG\)BHz�BIp�BJ=qBK\)BLz�BM��BO33BP(�BQG�BR{BS33BT(�BUBV�HBX(�BX��BY�BZ�HB\(�B]��B^�HB`  B`��BaBb�HBc�
Bd��Bf�\Bg�
Bh��Bi��Bj�RBk�
BmG�BnffBo�Bp��Bqp�BrffBs�Bu�Bv=qBw�Bx(�ByG�Bz=qB{\)B|��B~{B33B�(�B�z�B�
=B��B�ffB���B�p�B��
B�ffB�
=B��B�=qB��HB��B�B�=qB���B��B�(�B��\B���B��B�(�B���B�\)B��
B�=qB���B�G�B�  B���B�33B��B�  B��\B��B��
B�ffB���B�G�B��
B�Q�B��HB���B�{B���B�
=B��B�{B��RB�G�B��B�(�B��RB�\)B�  B��\B���B�\)B�  B���B�33B���B�  B��\B��B��
B�Q�B��RB�33B��B�ffB���B�p�B�B�Q�B���B��B�{B�z�B���B�\)B�(�B���B�G�B���B�  B��\B�G�B��B�{B���B��B��
B�Q�B��HB��B��B�(�B���B�p�B��B�Q�B��RB�33B��B�z�B��RB�33B��B�ffB���B��B��B�=qB���B�p�B�  B�Q�B��RB�33B��B�z�B��HB�G�B��B�ffB�
=B�\)B��
B�z�B�
=BŅB�  Bƣ�B�33BǙ�B�{Bȏ\B�\)B��B�=qBʸRB�33B�  B�z�B���B�\)B�{BΣ�B��BυB�  B���B�G�B��
B�Q�B��B�B�=qB���BՅB�(�B֣�B��B��B؏\B���Bٙ�B�ffB�
=BۅB�=qB���B�\)B��B���B�33B�B��\B�G�B�B�Q�B��B�B�(�B���B噚B�{B�RB�p�B��B�z�B�G�B��B�=qB���B�B�{B�RB�B�  B�\B�p�B��
B�z�B�\)B�B�z�B�\)B��
B���B�p�B��B���B�\)B��B��HB�\)B��B��HB�G�B�  B��HB�G�B�(�B���B�33C 
=C G�C ��C{CG�C�RC
=CG�C�RC
=CG�CC
=CffC�
C
=Cz�CC
=Cz�C�RC{C�C�RC(�C�CC	33C	z�C	�HC
Q�C
��C
=CQ�C��C
=CffC�HC(�C��C��Cp�CC�C��C�HCffC�C(�Cz�C��C=qC�RC{CffC�C33C�RC��Cz�CC=qCz�C�HC(�C�CC�CG�C��C�RC{C=qC\)C�C��C{C(�Cz�C�\C�HC��CG�C\)C�C��C{CG�Cp�C�C�HC{CQ�Cz�C�C��C
=CffC�C��C�HC33CQ�C��C�RC 
=C �C z�C �\C �HC ��C!Q�C!\)C!�C!��C"�C"=qC"p�C"�RC"�
C#(�C#=qC#�\C#��C#��C$
=C$\)C$z�C$C$�
C%(�C%G�C%�\C%��C%��C&
=C&\)C&ffC&�RC&��C'{C'=qC'\)C'�C'C({C((�C(p�C(�\C(�HC(�C)=qC)\)C)��C)C*
=C*�C*z�C*�C*��C+  C+�C+p�C+�C+�
C+�C,=qC,Q�C,��C,�RC-
=C-�C-p�C-�C-�
C-�C.(�C.ffC.�\C.�
C.�C/33C/\)C/��C/�RC0{C033C0ffC0��C0C1{C133C1�C1�\C1�C2
=C2=qC2�C2��C2�C3  C3\)C3p�C3�C3��C4{C4ffC4p�C4�RC5  C5�C5p�C5�C5C6  C6�C6ffC6�C6�
C6�C733C7ffC7�C7��C7�C8(�C8p�C8�C8�
C8��C9(�C9p�C9�\C9�HC9��C:=qC:z�C:��C:�C;
=C;\)C;p�C;�C;��C<{C<ffC<�C<�
C<�C=33C=p�C=�\C=�HC=��C>Q�C>p�C>��C>�C?
=C?\)C?p�C?�RC?�C@�C@p�C@�C@�RCA
=CA(�CAp�CA�CACB{CBG�CBp�CB�RCB��CC{CC\)CCz�CCCC��CD�CDffCD�CD�RCE  CE(�CEz�CE��CE��CF�CF33CFz�CFCF�CG=qCGz�CG��CG�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                              G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�z�A�|�A�|�A�9XA�{A�1A�9XA�ȴA�9XAΧ�A΍PA΃A�v�A�hsA�ZA�(�AͼjA�Q�A�1'A�&�A��A�A�
=A���A��HA��;A��mA��;A��
A���A���A�ĜA̬A̙�A̍PẢ7Ả7A̍PA̅ÃA̍PA̋DÃA�|�A�v�A�t�A�t�A�v�A�|�ÁÁA�M�A�1A˼jA��Aɺ^A��TA�ƨA�\)A�7LA�l�A�A��A���A��A��wA�G�A�K�A��FA���A�x�A��A���A��;A��`A�&�A�G�A��wA��yA�O�A�ȴA�z�A�ZA|M�Ax�9As�7Ap~�Al�HAk33Ai7LAc�A_oA\bAY�AWS�AVJARĜAQ;dAMG�AK��AKVAI��AE
=AC�A?
=A9A5?}A2��A29XA1�^A0A-�A,�!A,{A+\)A);dA'K�A&A$�\A#oA!��A   AXA�A%AI�AA��A�yAVA�A��At�AAr�A&�AĜAZA��AK�AS�A�A��A��A{A�A;dA&�AVA��AJA��AG�A�A�jA^5AA��A|�AhsAS�A"�A�DA�FA\)A\)A\)Al�AA�A��A�7AXA33AE�A
=A�A1A?}AoA
�+A�A��A��A�jA5?A��A�A ��A =qA b@��@��;@���@�S�@�@�(�@�ȴ@��9@��
@�\)@�@��@��R@��\@��@��@�&�@��@�+@���@��;@�w@@�C�@�5?@�&�@��`@�@� �@�P@�33@�!@�^@�D@� �@�@��@�9@�1'@�+@�ȴ@�R@�v�@�$�@��@�$�@�$�@�J@�-@���@��@��@�n�@���@���@�x�@�j@�  @��
@۝�@�l�@�33@���@���@١�@���@��@ו�@�;d@ָR@�ff@�M�@�=q@��@�z�@ӶF@ӕ�@�o@ҧ�@�n�@���@�p�@���@�1'@υ@��@�v�@�V@�-@�{@Ͳ-@��@�r�@˅@���@�~�@�-@ɡ�@��`@�1'@ǅ@�33@�
=@���@Ƈ+@���@�`B@Õ�@¸R@�E�@�@��@���@��-@�X@��/@���@��/@��@�9X@��
@�|�@�;d@��@��!@�v�@�5?@���@�`B@��j@�A�@��F@�
=@���@��+@�@��7@�p�@�`B@�O�@�O�@���@�j@�ƨ@���@��P@�t�@�33@���@��\@�-@��h@�`B@�?}@��@��u@�Z@��m@��P@�+@��@��+@�ff@�-@�x�@���@�1'@���@�\)@�33@�"�@�@�V@���@��@�&�@���@�z�@�1'@�  @��m@��;@���@���@���@��@���@��@���@��-@��7@�G�@��@��@�1'@��;@���@���@�=q@�@��@��#@��-@��h@�x�@�`B@�?}@��@���@��@���@�bN@� �@�t�@�@���@�V@��-@�hs@�&�@�V@��j@��D@�9X@��F@�C�@�ȴ@���@�M�@�J@���@��@�X@�/@�V@��u@���@��
@��F@�l�@�"�@��@��\@�ff@�E�@�$�@��@��@���@�V@���@�1'@��@�  @��m@���@�t�@�C�@�o@���@��y@���@�V@��@�@��#@���@�O�@��@��@���@�9X@�1@��
@���@��P@�l�@�l�@�dZ@�o@���@��R@��+@��^@�?}@�V@��9@���@���@���@���@���@��u@�z�@�Q�@���@�ƨ@���@��P@�\)@��@��@��H@��@��@�ȴ@���@�ff@�E�@�@���@��h@�x�@��@��u@�j@�  @���@�ȴ@��+@�V@�5?@�{@��-@��@�?}@���@��j@�I�@�1@��@��w@���@��P@�|�@�dZ@�\)@�C�@�
=@���@�v�@�M�@�=q@�{@���@��@��u@�r�@�Z@�b@�ƨ@���@�~�@�=q@�@��@�&�@���@��`@���@��9@��@���@�Z@�1@�;@�@�@�P@�@~v�@}�@}O�@|��@|9X@{ƨ@{o@y�@x��@xb@w��@w�@w|�@w
=@v�R@v5?@u��@u�-@u`B@t��@t�@s��@sdZ@s"�@r~�@q�#@q�@p��@o�w@o|�@n�y@n�+@m��@l��@l��@lZ@l(�@k��@k�
@k�F@k�@kS�@ko@j�H@jJ@i��@i%@hr�@h1'@g��@g�w@g�P@g�@f�R@f5?@f@f@e��@e�@dZ@d(�@d1@cS�@c"�@co@b�@b��@b�!@b=q@a��@a�^@a��@ahs@a7L@a%@`�u@`  @_��@_l�@_K�@_+@_�@^�@]`B@\(�@[�F@Z��@ZJ@Y�^@Y7L@X��@X �@W�P@W;d@V��@U�@UO�@T�@TI�@S��@S��@SdZ@R�@R��@R�\@R^5@RM�@R^5@R^5@RM�@R=q@Q�7@PA�@Ol�@N��@N��@N�+@Nv�@NV@N{@M�@L�/@L��@Kƨ@J~�@J�@I��@I�#@I��@Ix�@H�9@H��@H��@H��@H��@H�9@H��@H�u@HbN@G�@G��@G|�@G\)@G;d@G
=@F��@E�@EO�@EV@D�@D(�@C��@CC�@Bn�@A�@A��@Ahs@A7L@@��@@�@@b@?��@?K�@?
=@>�@>�+@>V@=@=?}@=V@<��@<�@;��@;ƨ@;��@;S�@;"�@:�H@:��@:��@:�@9�^@9hs@9G�@8Ĝ@8�@8Q�@81'@7�@7�P@7+@6��@6�R@6��@6v�@65?@5��@5�h@5�h@5�@5?}@5�@4�@4z�@3�m@3S�@2�H@2��@1�@1&�@1%@0�`@0Ĝ@0Ĝ@0Ĝ@0�9@0�9@0�@0  @/|�@.��@.V@-��@-��@-�@-`B@-/@,�@,z�@,I�@+��@+dZ@+S�@+S�@+C�@+C�@+C�@+"�@*�H@*��@*�\@*^5@*J@)�^@)�7@)X@(�u@(b@(  @'��@'\)@'+@'
=@&��@&ȴ@&��@&�+@&{@%�-@%�h@%p�@%V@$�j@$I�@#ƨ@#@"��@"��@"�\@"n�@"M�@"J@!�#@!��@!�7@!X@!7L@!�@ ��@ ��@ Ĝ@ Q�@ A�@ A�@   @��@�w@�@��@|�@K�@+@�@��@��@ff@{@�T@��@p�@O�@�@��@�/@�j@�D@��@ƨ@��@��@t�@C�@33@o@��@M�@�@�@��@�#@�#@��@x�@�@�u@�@bN@  @�;@�P@l�@\)@+@
=@�y@�R@��@{@�-@`B@��@I�@9X@(�@�@�m@�F@�@t�@S�@o@�@��@�\@M�@-@�#@��@x�@hs@&�@�@%@%@��@Ĝ@��@�u@r�@Q�@  @�;@�w@|�@l�@K�@;d@�y@E�@5?@$�@$�@$�@{@{@�T@��@O�@V@�j@�@��@�D@�@�
@ƨ@�@"�@
�H@
�!@
M�@
J@	��@	��@	�7@	&�@	�@��@�9@A�@1'@1'@b@�@l�@�@��@��@v�@v�@E�@@@��@�h@�@p�@O�@�@�@�@�@�@�@�/@�j@z�@Z@Z@9X@�@�@�@�@1@��@�m@�
A�`BA�l�Aч+Aч+Aч+AуA�~�A�z�A�x�A�z�A�~�AсA�z�A�r�A�t�A�O�A�oA��A�oA�oA�JA�JA�"�A�1'A�(�A��A�AЉ7A�=qA��A�
=A��mA��A��HAϣ�A�p�A�A�A�E�A�E�A�
=AήAΗ�AΓuAΏ\A΋DA΋DAΉ7A·+A΅AΉ7A·+A΃A�~�A�z�A�t�A�x�A�z�A�|�A�z�A�x�A�t�A�r�A�t�A�jA�ffA�dZA�ffA�hsA�ffA�dZA�bNA�ZA�XA�XA�XA�XA�XA�VA�E�A�A�A�7LA�+A�"�A��A�oA�1A�  A��A���AͼjA͸RAͰ!A͛�A͍PA�x�A�XA�VA�O�A�O�A�M�A�G�A�C�A�?}A�;dA�5?A�/A�-A�+A�&�A�&�A�(�A�+A�(�A�&�A�&�A�$�A�"�A�$�A�"�A� �A��A��A�oA�VA�1A�A���A�  A�A�
=A�
=A�
=A�JA�VA�VA�VA�JA�1A�A�A�A�A���A��A��A��A��A��mA��yA��`A��TA��HA��;A��A��A��A��#A��/A��;A��;A��;A��mA��A��A��yA��yA��yA��`A��TA��HA��HA��;A��;A��#A��#A��/A��;A��;A��#A��A��
A���A���A���A���A���A���A���A���A���A��
A��
A���A���A���A���A���A���A���A���A���A�ĜA�A���A̾wA̺^A̴9A̰!A̮A̩�A̩�A̩�A̧�Ạ�A̝�A̗�A̗�A̗�A̙�A̙�A̕�ȂhA̋DA̋DA̋DA̍PA̍PA̍PẢ7Ả7Ả7Ȧ+Ả7A̋DA̋DȦ+A̅Ȧ+Ả7A̍PA̍PA̍PA̋DẢ7Ȧ+A̍PȀ\ȂhȀ\A̋DẢ7A̅A̅A̅A̅ÃÁA�~�ÃA̅A̅ÁÁÁÃẢ7ȂhȂhȀ\Ȁ\ȂhȂhȂhȀ\A̍PȦ+A̅A̅A̅A̅A̅ÃÁÁA�~�ÁÁÁA�|�A�z�A�x�A�z�A�~�A�|�A�z�A�v�A�t�A�v�A�x�A�v�A�r�A�r�A�t�A�v�A�v�A�r�A�r�A�r�A�v�A�v�A�v�A�t�A�r�A�t�A�x�A�x�A�v�A�t�A�t�A�v�A�x�A�z�A�x�A�v�A�z�A�~�ÁA�~�A�z�A�|�A�~�ÁÁA�|�A�|�ÃÃÃÁA�~�A�~�ÁÃA̅A̅A�~�A�z�A�n�A�jA�`BA�I�A�7LA�;dA�?}A�=qA�-A��A�JA�A���A��A��A��A��A��A��/A˰!A˗�Aˏ\Aˏ\Aˉ7A�t�A�;dA��A�1A���AʶFAʃA�9XA�JA��A��yA��TA��
Aɉ7A�{Aȧ�A�jA�"�A���Aǥ�AǏ\A�^5A��#A��A�A�A�K�A��`Aã�A�A�VA���A��TA�?}A�A��9A�~�A�l�A�\)A�I�A�&�A�  A���A��uA�9XA�  A�ffA��!A�9XA���A���A���A���A���A��^A�hsA�1A���A�(�A��A�1'A���A�A�dZA��A��A���A�ffA�1A�ĜA��uA�`BA�K�A�/A�A��#A���A��
A��-A�E�A���A��uA�A���A��#A���A���A��A�jA�`BA�ZA�M�A�+A��A���A��9A���A���A��9A���A��hA�n�A� �A��A���A�M�A��A��A�{A��TA��A�n�A�G�A��A�A�  A���A���A���A���A�1A�  A��A��A��A��A��;A���A���A�bNA���A��A�G�A��A��RA�bA��jA�33A���A�5?A��A�?}A�bA�r�A��RA���A��HA�C�A� �A�bA��A��A���A���A�(�A�VA���A�;dA��;A��A��\A�A�A�dZA�r�A��^A�7LA��yA��!A���A�|�A�XA�;dA�bA�ĜA��7A�VA�(�A��TA��A�;dA���A�9XA��A��AA~n�A}�A{�mA{O�A{oAz�Az��Az�9Az�\Azv�AzQ�Az�Ay��AxȴAw�Av��AvZAu��Au�Au�At��AtffAs��As7LArjAr  Aq��Aql�AqG�Aq�Ap��Ap�RApz�ApQ�Ap�Ao��Aot�Ao�An�RAm�
AmK�Al��Al-Ak��Ak�7AkO�AkK�AkC�Ak?}Ak7LAk7LAk33Ak7LAk/Ak/Ak&�Ak+Ak�Aj��Aj��Aj��AjbNAi�Ai
=Ah1AgG�Af�!Af(�Ae�7AeVAd�HAd�RAd1'Acl�Ab�\Aa/A`ffA`JA_�A_��A_�FA_��A_x�A_
=A^Q�A]��A]��A]dZA]VA\�RA\�\A\E�A[�A[�wA[|�A[`BA[33A[�AZ�AZ��AZ��AZn�AZ=qAY�-AY\)AY
=AX��AXZAX�AW|�AW;dAW�AW�AWoAWVAW%AV�AV�HAVȴAV�AVz�AVA�AU��AU��AUp�AUK�AU�AT~�AS�ASC�AR�AR�AR��ARVAR5?ARJAQ��AQ�TAQ�wAQx�AQ?}AQ;dAQ/AQ�AP��AP�!AP=qAO�hAN�AM�^AMAL��ALz�ALbNALM�AL5?AL5?AL�ALbAK��AK�
AK�^AK��AK��AK�PAKx�AKdZAKC�AK;dAK/AK"�AJ��AJ�yAJ��AJ�uAJ�AJn�AJM�AJ{AI��AI�7AIC�AH��AHjAH-AG�AF�yAE�AD�9AC�AC�FAC�PACl�ACdZACS�ACC�AC;dAC+AC"�AC�ACVAC%AB�AB�+AB5?AA�FAA33A@ffA>9XA<bA<JA<JA<{A<A;��A:��A9��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111                                                                                                                                                                                                                                              A�z�A�|�A�|�A�9XA�{A�1A�9XA�ȴA�9XAΧ�A΍PA΃A�v�A�hsA�ZA�(�AͼjA�Q�A�1'A�&�A��A�A�
=A���A��HA��;A��mA��;A��
A���A���A�ĜA̬A̙�A̍PẢ7Ả7A̍PA̅ÃA̍PA̋DÃA�|�A�v�A�t�A�t�A�v�A�|�ÁÁA�M�A�1A˼jA��Aɺ^A��TA�ƨA�\)A�7LA�l�A�A��A���A��A��wA�G�A�K�A��FA���A�x�A��A���A��;A��`A�&�A�G�A��wA��yA�O�A�ȴA�z�A�ZA|M�Ax�9As�7Ap~�Al�HAk33Ai7LAc�A_oA\bAY�AWS�AVJARĜAQ;dAMG�AK��AKVAI��AE
=AC�A?
=A9A5?}A2��A29XA1�^A0A-�A,�!A,{A+\)A);dA'K�A&A$�\A#oA!��A   AXA�A%AI�AA��A�yAVA�A��At�AAr�A&�AĜAZA��AK�AS�A�A��A��A{A�A;dA&�AVA��AJA��AG�A�A�jA^5AA��A|�AhsAS�A"�A�DA�FA\)A\)A\)Al�AA�A��A�7AXA33AE�A
=A�A1A?}AoA
�+A�A��A��A�jA5?A��A�A ��A =qA b@��@��;@���@�S�@�@�(�@�ȴ@��9@��
@�\)@�@��@��R@��\@��@��@�&�@��@�+@���@��;@�w@@�C�@�5?@�&�@��`@�@� �@�P@�33@�!@�^@�D@� �@�@��@�9@�1'@�+@�ȴ@�R@�v�@�$�@��@�$�@�$�@�J@�-@���@��@��@�n�@���@���@�x�@�j@�  @��
@۝�@�l�@�33@���@���@١�@���@��@ו�@�;d@ָR@�ff@�M�@�=q@��@�z�@ӶF@ӕ�@�o@ҧ�@�n�@���@�p�@���@�1'@υ@��@�v�@�V@�-@�{@Ͳ-@��@�r�@˅@���@�~�@�-@ɡ�@��`@�1'@ǅ@�33@�
=@���@Ƈ+@���@�`B@Õ�@¸R@�E�@�@��@���@��-@�X@��/@���@��/@��@�9X@��
@�|�@�;d@��@��!@�v�@�5?@���@�`B@��j@�A�@��F@�
=@���@��+@�@��7@�p�@�`B@�O�@�O�@���@�j@�ƨ@���@��P@�t�@�33@���@��\@�-@��h@�`B@�?}@��@��u@�Z@��m@��P@�+@��@��+@�ff@�-@�x�@���@�1'@���@�\)@�33@�"�@�@�V@���@��@�&�@���@�z�@�1'@�  @��m@��;@���@���@���@��@���@��@���@��-@��7@�G�@��@��@�1'@��;@���@���@�=q@�@��@��#@��-@��h@�x�@�`B@�?}@��@���@��@���@�bN@� �@�t�@�@���@�V@��-@�hs@�&�@�V@��j@��D@�9X@��F@�C�@�ȴ@���@�M�@�J@���@��@�X@�/@�V@��u@���@��
@��F@�l�@�"�@��@��\@�ff@�E�@�$�@��@��@���@�V@���@�1'@��@�  @��m@���@�t�@�C�@�o@���@��y@���@�V@��@�@��#@���@�O�@��@��@���@�9X@�1@��
@���@��P@�l�@�l�@�dZ@�o@���@��R@��+@��^@�?}@�V@��9@���@���@���@���@���@��u@�z�@�Q�@���@�ƨ@���@��P@�\)@��@��@��H@��@��@�ȴ@���@�ff@�E�@�@���@��h@�x�@��@��u@�j@�  @���@�ȴ@��+@�V@�5?@�{@��-@��@�?}@���@��j@�I�@�1@��@��w@���@��P@�|�@�dZ@�\)@�C�@�
=@���@�v�@�M�@�=q@�{@���@��@��u@�r�@�Z@�b@�ƨ@���@�~�@�=q@�@��@�&�@���@��`@���@��9@��@���@�Z@�1@�;@�@�@�P@�@~v�@}�@}O�@|��@|9X@{ƨ@{o@y�@x��@xb@w��@w�@w|�@w
=@v�R@v5?@u��@u�-@u`B@t��@t�@s��@sdZ@s"�@r~�@q�#@q�@p��@o�w@o|�@n�y@n�+@m��@l��@l��@lZ@l(�@k��@k�
@k�F@k�@kS�@ko@j�H@jJ@i��@i%@hr�@h1'@g��@g�w@g�P@g�@f�R@f5?@f@f@e��@e�@dZ@d(�@d1@cS�@c"�@co@b�@b��@b�!@b=q@a��@a�^@a��@ahs@a7L@a%@`�u@`  @_��@_l�@_K�@_+@_�@^�@]`B@\(�@[�F@Z��@ZJ@Y�^@Y7L@X��@X �@W�P@W;d@V��@U�@UO�@T�@TI�@S��@S��@SdZ@R�@R��@R�\@R^5@RM�@R^5@R^5@RM�@R=q@Q�7@PA�@Ol�@N��@N��@N�+@Nv�@NV@N{@M�@L�/@L��@Kƨ@J~�@J�@I��@I�#@I��@Ix�@H�9@H��@H��@H��@H��@H�9@H��@H�u@HbN@G�@G��@G|�@G\)@G;d@G
=@F��@E�@EO�@EV@D�@D(�@C��@CC�@Bn�@A�@A��@Ahs@A7L@@��@@�@@b@?��@?K�@?
=@>�@>�+@>V@=@=?}@=V@<��@<�@;��@;ƨ@;��@;S�@;"�@:�H@:��@:��@:�@9�^@9hs@9G�@8Ĝ@8�@8Q�@81'@7�@7�P@7+@6��@6�R@6��@6v�@65?@5��@5�h@5�h@5�@5?}@5�@4�@4z�@3�m@3S�@2�H@2��@1�@1&�@1%@0�`@0Ĝ@0Ĝ@0Ĝ@0�9@0�9@0�@0  @/|�@.��@.V@-��@-��@-�@-`B@-/@,�@,z�@,I�@+��@+dZ@+S�@+S�@+C�@+C�@+C�@+"�@*�H@*��@*�\@*^5@*J@)�^@)�7@)X@(�u@(b@(  @'��@'\)@'+@'
=@&��@&ȴ@&��@&�+@&{@%�-@%�h@%p�@%V@$�j@$I�@#ƨ@#@"��@"��@"�\@"n�@"M�@"J@!�#@!��@!�7@!X@!7L@!�@ ��@ ��@ Ĝ@ Q�@ A�@ A�@   @��@�w@�@��@|�@K�@+@�@��@��@ff@{@�T@��@p�@O�@�@��@�/@�j@�D@��@ƨ@��@��@t�@C�@33@o@��@M�@�@�@��@�#@�#@��@x�@�@�u@�@bN@  @�;@�P@l�@\)@+@
=@�y@�R@��@{@�-@`B@��@I�@9X@(�@�@�m@�F@�@t�@S�@o@�@��@�\@M�@-@�#@��@x�@hs@&�@�@%@%@��@Ĝ@��@�u@r�@Q�@  @�;@�w@|�@l�@K�@;d@�y@E�@5?@$�@$�@$�@{@{@�T@��@O�@V@�j@�@��@�D@�@�
@ƨ@�@"�@
�H@
�!@
M�@
J@	��@	��@	�7@	&�@	�@��@�9@A�@1'@1'@b@�@l�@�@��@��@v�@v�@E�@@@��@�h@�@p�@O�@�@�@�@�@�@�@�/@�j@z�@Z@Z@9X@�@�@�@�@1@��@�m@�
A�`BA�l�Aч+Aч+Aч+AуA�~�A�z�A�x�A�z�A�~�AсA�z�A�r�A�t�A�O�A�oA��A�oA�oA�JA�JA�"�A�1'A�(�A��A�AЉ7A�=qA��A�
=A��mA��A��HAϣ�A�p�A�A�A�E�A�E�A�
=AήAΗ�AΓuAΏ\A΋DA΋DAΉ7A·+A΅AΉ7A·+A΃A�~�A�z�A�t�A�x�A�z�A�|�A�z�A�x�A�t�A�r�A�t�A�jA�ffA�dZA�ffA�hsA�ffA�dZA�bNA�ZA�XA�XA�XA�XA�XA�VA�E�A�A�A�7LA�+A�"�A��A�oA�1A�  A��A���AͼjA͸RAͰ!A͛�A͍PA�x�A�XA�VA�O�A�O�A�M�A�G�A�C�A�?}A�;dA�5?A�/A�-A�+A�&�A�&�A�(�A�+A�(�A�&�A�&�A�$�A�"�A�$�A�"�A� �A��A��A�oA�VA�1A�A���A�  A�A�
=A�
=A�
=A�JA�VA�VA�VA�JA�1A�A�A�A�A���A��A��A��A��A��mA��yA��`A��TA��HA��;A��A��A��A��#A��/A��;A��;A��;A��mA��A��A��yA��yA��yA��`A��TA��HA��HA��;A��;A��#A��#A��/A��;A��;A��#A��A��
A���A���A���A���A���A���A���A���A���A��
A��
A���A���A���A���A���A���A���A���A���A�ĜA�A���A̾wA̺^A̴9A̰!A̮A̩�A̩�A̩�A̧�Ạ�A̝�A̗�A̗�A̗�A̙�A̙�A̕�ȂhA̋DA̋DA̋DA̍PA̍PA̍PẢ7Ả7Ả7Ȧ+Ả7A̋DA̋DȦ+A̅Ȧ+Ả7A̍PA̍PA̍PA̋DẢ7Ȧ+A̍PȀ\ȂhȀ\A̋DẢ7A̅A̅A̅A̅ÃÁA�~�ÃA̅A̅ÁÁÁÃẢ7ȂhȂhȀ\Ȁ\ȂhȂhȂhȀ\A̍PȦ+A̅A̅A̅A̅A̅ÃÁÁA�~�ÁÁÁA�|�A�z�A�x�A�z�A�~�A�|�A�z�A�v�A�t�A�v�A�x�A�v�A�r�A�r�A�t�A�v�A�v�A�r�A�r�A�r�A�v�A�v�A�v�A�t�A�r�A�t�A�x�A�x�A�v�A�t�A�t�A�v�A�x�A�z�A�x�A�v�A�z�A�~�ÁA�~�A�z�A�|�A�~�ÁÁA�|�A�|�ÃÃÃÁA�~�A�~�ÁÃA̅A̅A�~�A�z�A�n�A�jA�`BA�I�A�7LA�;dA�?}A�=qA�-A��A�JA�A���A��A��A��A��A��A��/A˰!A˗�Aˏ\Aˏ\Aˉ7A�t�A�;dA��A�1A���AʶFAʃA�9XA�JA��A��yA��TA��
Aɉ7A�{Aȧ�A�jA�"�A���Aǥ�AǏ\A�^5A��#A��A�A�A�K�A��`Aã�A�A�VA���A��TA�?}A�A��9A�~�A�l�A�\)A�I�A�&�A�  A���A��uA�9XA�  A�ffA��!A�9XA���A���A���A���A���A��^A�hsA�1A���A�(�A��A�1'A���A�A�dZA��A��A���A�ffA�1A�ĜA��uA�`BA�K�A�/A�A��#A���A��
A��-A�E�A���A��uA�A���A��#A���A���A��A�jA�`BA�ZA�M�A�+A��A���A��9A���A���A��9A���A��hA�n�A� �A��A���A�M�A��A��A�{A��TA��A�n�A�G�A��A�A�  A���A���A���A���A�1A�  A��A��A��A��A��;A���A���A�bNA���A��A�G�A��A��RA�bA��jA�33A���A�5?A��A�?}A�bA�r�A��RA���A��HA�C�A� �A�bA��A��A���A���A�(�A�VA���A�;dA��;A��A��\A�A�A�dZA�r�A��^A�7LA��yA��!A���A�|�A�XA�;dA�bA�ĜA��7A�VA�(�A��TA��A�;dA���A�9XA��A��AA~n�A}�A{�mA{O�A{oAz�Az��Az�9Az�\Azv�AzQ�Az�Ay��AxȴAw�Av��AvZAu��Au�Au�At��AtffAs��As7LArjAr  Aq��Aql�AqG�Aq�Ap��Ap�RApz�ApQ�Ap�Ao��Aot�Ao�An�RAm�
AmK�Al��Al-Ak��Ak�7AkO�AkK�AkC�Ak?}Ak7LAk7LAk33Ak7LAk/Ak/Ak&�Ak+Ak�Aj��Aj��Aj��AjbNAi�Ai
=Ah1AgG�Af�!Af(�Ae�7AeVAd�HAd�RAd1'Acl�Ab�\Aa/A`ffA`JA_�A_��A_�FA_��A_x�A_
=A^Q�A]��A]��A]dZA]VA\�RA\�\A\E�A[�A[�wA[|�A[`BA[33A[�AZ�AZ��AZ��AZn�AZ=qAY�-AY\)AY
=AX��AXZAX�AW|�AW;dAW�AW�AWoAWVAW%AV�AV�HAVȴAV�AVz�AVA�AU��AU��AUp�AUK�AU�AT~�AS�ASC�AR�AR�AR��ARVAR5?ARJAQ��AQ�TAQ�wAQx�AQ?}AQ;dAQ/AQ�AP��AP�!AP=qAO�hAN�AM�^AMAL��ALz�ALbNALM�AL5?AL5?AL�ALbAK��AK�
AK�^AK��AK��AK�PAKx�AKdZAKC�AK;dAK/AK"�AJ��AJ�yAJ��AJ�uAJ�AJn�AJM�AJ{AI��AI�7AIC�AH��AHjAH-AG�AF�yAE�AD�9AC�AC�FAC�PACl�ACdZACS�ACC�AC;dAC+AC"�AC�ACVAC%AB�AB�+AB5?AA�FAA33A@ffA>9XA<bA<JA<JA<{A<A;��A:��A9��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111                                                                                                                                                                                                                                              G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�	B	�=B	��B	��B	�$B	�$B	�YB	�hB	�VB	��B	�~B	�VB	� B	��B	��B	��B	�lB	z�B	y>B	y�B	yrB	xlB	z�B	y	B	v`B	v`B	{B	{JB	zxB	y�B	{�B	{B	w�B	tB	r�B	sB	s�B	t�B	tB	s�B	u�B	v�B	u%B	t�B	tB	s�B	s�B	t�B	v�B	xlB	z�B	�MB	��B	��B
&�B
M�B
iyB
v�B
��B
��B
�NB
�,B
�8B
��B
��BB
�B
�)B
�NB
�"B
��B
}�B
z�B
~�B
�XB
��B
OBB
6�B
A B
/B
�B	��B	�ZB	�B	�6B	�	B	�B	��B	y�B	{B	t�B	a|B	Z�B	R�B	H�B	E9B	JXB	F�B	K�B	H�B	EmB	<�B	2�B	$B	$@B	$�B	�B	+B��B�	B�B�	B��B��B�xB�]B	�B	 �B	 �B�B�B��B��B��B	xB	VB	B	�B	'B	(�B	&�B	*�B	2aB	.IB	5tB	2�B	2-B	4�B	=B	B'B	FtB	J�B	GEB	CaB	R�B	a�B	jB	l�B	oiB	r�B	v+B	v�B	xB	x8B	zDB	|PB	�B	��B	��B	�xB	�PB	�~B	��B	��B	�	B	��B	��B	��B	�-B	�'B	��B	�B	��B	��B	��B	�KB	ݘB	�vB	�B	�#B	�yB	�^B	��B	��B	��B	�@B	�B	�hB	��B	��B	��B	��B	�B	��B	�!B	��B	��B	�B	��B	�$B	�$B	�$B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�kB	��B	��B	�}B	�OB	�!B	�UB	��B	��B	��B	�zB	��B	��B	�FB	��B	��B	��B	�jB	�0B	��B	��B	�0B	��B	��B	��B	�^B	�dB	�qB	��B	�wB	�wB	��B	��B	�HB	��B	�B	�B	��B	��B	��B	B	�'B	�'B	��B	�gB	�B	��B	�?B	�EB	ƨB	�?B	��B	�#B	�B	��B	�XB	�)B	�0B	��B	�B	�BB	�B	ϫB	�B	ΥB	�pB	�pB	�<B	�vB	ϫB	�BB	��B	��B	�}B	��B	� B	ѷB	҉B	҉B	��B	҉B	��B	��B	�[B	�&B	��B	ѷB	� B	� B	уB	� B	҉B	��B	��B	՛B	�mB	�EB	�B	�B	�yB	خB	�B	ٴB	�QB	�QB	یB	��B	�B	ߤB	�B	�NB	�B	��B	�ZB	�ZB	�ZB	�,B	�B	��B	��B	�B	�B	�B	�B	�QB	�"B	�B	��B	�B	�B	��B	�B	�B	��B	�B	�B	�vB	�AB	�B	�B	�B	�B	�B	�B	��B	��B	��B	�+B	�+B	�+B	�2B	�B	�B	��B	��B	�DB	��B	�B	�JB	�B	�B	��B	��B	��B	��B	�(B	��B	�]B	�]B	��B	��B	��B
 iB
 �B
B
�B
{B
MB
MB
MB
MB
�B
�B
�B
�B
�B
YB
�B
%B
�B
%B
1B
�B
�B
	�B
DB

�B
�B
DB
�B
B
�B
�B
�B
VB
(B
�B
.B
�B
�B
�B
�B
�B
hB
oB
B
�B
�B
@B
uB
FB
{B
�B
�B
{B
�B
B
�B
YB
YB
YB
�B
+B
+B
1B
_B
_B
�B
�B
1B
B
kB
kB
kB
�B
�B
qB
�B
�B
B
IB
�B
B
~B
�B
~B
IB
�B
�B
�B
�B
 �B
 \B
 �B
!�B
!-B
!�B
!�B
 �B
 �B
 �B
 �B
!-B
"4B
"�B
"hB
"�B
"4B
#B
#B
"�B
"hB
"4B
"4B
"�B
#�B
#nB
#B
#�B
#�B
#:B
$B
$�B
$�B
%zB
%B
'B
&LB
'RB
'RB
&�B
($B
'�B
(XB
)*B
)�B
*eB
+kB
+6B
+�B
,=B
+�B
,qB
,�B
,B
,qB
-wB
-B
-�B
-�B
-B
-�B
-�B
/B
/OB
/B
/B
/�B
/�B
1[B
1[B
1�B
2�B
3�B
4�B
4�B
4�B
4�B
4�B
5?B
4�B
5�B
6B
5�B
6FB
6B
6B
6FB
6zB
7B
7�B
7�B
7�B
8RB
8�B
:^B
<6B
<�B
<�B
=B
<�B
=qB
<�B
=qB
=qB
=qB
=�B
>wB
>BB
>�B
>�B
>�B
?�B
@OB
@�B
A B
B[B
B[B
B[B
B�B
B�B
C�B
C-B
C�B
C�B
C�B
D3B
C�B
D3B
DgB
DgB
DgB
E�B
EmB
FB
FtB
F�B
GB
F�B
GEB
G�B
HB
H�B
H�B
HKB
H�B
I�B
I�B
I�B
I�B
J�B
J�B
J�B
J�B
J�B
J�B
K�B
K�B
K�B
K�B
K�B
K�B
K�B
L�B
L�B
MB
L�B
L�B
LdB
K�B
K�B
MB
MjB
M�B
OvB
O�B
PHB
PB
P}B
QB
Q�B
Q�B
R�B
S�B
S&B
S�B
TaB
T,B
TaB
TaB
T�B
T�B
T�B
T�B
T�B
T�B
T�B
T�B
TaB
UgB
V�B
W
B
W
B
W
B
W�B
W?B
WsB
WsB
W�B
XyB
XyB
YB
Z�B
Z�B
Z�B
Z�B
Z�B
[WB
[�B
[�B
[�B
[�B
[�B
[�B
[WB
[#B
[�B
[�B
[�B
\)B
\)B
\)B
[�B
\�B
\�B
]�B
]/B
]�B
^B
^5B
^jB
_�B
_�B
_�B
_�B
`B
`BB
`BB
`�B
aHB
aHB
a|B
a�B
a�B
a�B
b�B
bNB
b�B
cTB
cTB
c�B
c�B
cTB
c�B
dZB
d&B
d&B
d&B
e,B
e`B
e,B
e`B
e�B
e�B
f2B
e�B
f2B
f�B
gB
gB
g8B
gmB
g8B
g�B
h>B
h>B
h
B
h
B
hsB
h>B
hsB
h�B
iB
i�B
i�B
i�B
kQB
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
k�B
lWB
l�B
l�B
m�B
m�B
m�B
m�B
n/B
n/B
ncB
n�B
n�B
o�B
o�B
oiB
o�B
o�B
o�B
o�B
o�B
pB
p;B
pB
p;B
p�B
p�B
p�B
p�B
q�B
rB
rB
rB
r�B
r�B
r�B
r�B
r�B
sB
r�B
s�B
s�B
s�B
s�B
tB
t�B
t�B
u%B
v+B
v+B
v+B
v`B
v`B
v�B
v�B
v�B
v�B
v�B
w2B
wfB
wfB
w�B
w�B
w�B
x8B
x8B
xB
x�B
xlB
xlB
xlB
x�B
x�B
x�B
x�B
yrB
yrB
yrB
y�B
zB
zDB
zxB
zxB
z�B
z�B
z�B
{B
{B
{B
{�B
{�B
{�B
{�B
|B
|B
|PB
|B
|�B
}"B
}VB
}�B
}�B
}�B
}�B
}VB
~(B
~(B
~�B
~�B
.B
cB
cB
�B
cB
�B
�B
�B
� B
� B
�B
��B
�B
�;B
�B
�uB
�uB
�AB
��B
��B
��B
��B
�B
�GB
�{B
�{B
��B
��B
�B
�B
��B
��B
��B
��B
��B
��B
��B
��B
�YB
��B
��B
��B
��B
�YB
��B
��B
�+B
�_B
�+B
�_B
�_B
�1B
��B
�fB
��B
�fB
�fB
�fB
�1B
��B
�B
�7B
��B
�	B
��B
��B
��B
�rB
��B
��B
�B
�xB
��B
��B
�~B
��B
��B
�B
�PB
��B
��B
��B
�"B
��B
��B
��B
��B
�(B
�\B
��B
��B
�bB
�bB
�bB
��B
� B
�4B
�hB
�hB
�hB
��B
��B
�:B
�:B
�:B
�:B
�:B
�:B
�:B
�:B
��B
��B
��B
�B
�@B
�@B
�@B
�@B
�uB
�uB
�uB
��B	��B	�eB	�eB	�B	�B	�kB	�	B	�qB	��B	��B	�kB	��B	�CB	�qB	�_B	��B	��B	�B	��B	��B	�YB	��B	��B	�MB	��B	�xB	�7B	��B	��B	��B	�hB	��B	��B	�.B	�B	�uB	�(B	�lB	�rB	�SB	��B	��B	��B	��B	��B	�JB	��B	��B	��B	�B	�(B	��B	� B	�bB	��B	�bB	��B	��B	��B	��B	��B	� B	�FB	�SB	�uB	�B	�:B	�B	��B	��B	�MB	��B	��B	�MB	��B	��B	��B	�$B	�$B	�FB	�{B	��B	�@B	�B	��B	��B	�4B	��B	��B	�+B	��B	��B	�_B	~(B	�B	{�B	y�B	y�B	y>B	yrB	y	B	xlB	y	B	x�B	y�B	zDB	zDB	yrB	x�B	wfB	v�B	yrB	zDB	zxB	z�B	z�B	zDB	y	B	x�B	x�B	y	B	yrB	y>B	y�B	zB	yrB	w2B	w2B	uZB	xlB	yrB	{JB	z�B	|PB	|B	{�B	{�B	z�B	y�B	x�B	y>B	y�B	{B	y	B	yrB	xlB	w�B	y>B	v�B	w�B	v`B	t�B	v�B	v+B	u�B	u�B	v+B	tB	u�B	u�B	v+B	t�B	{�B	|PB	|�B	|PB	{B	{B	zxB	zDB	z�B	{B	{B	|PB	{�B	{B	zxB	zB	z�B	{�B	{B	{�B	zxB	x�B	xlB	wfB	w�B	xB	{JB	{B	{�B	z�B	z�B	{B	{�B	|PB	|�B	|B	{B	{JB	{�B	{B	{�B	z�B	y�B	y�B	w�B	w�B	xlB	xB	v`B	v`B	u%B	uZB	t�B	u�B	t�B	s�B	r�B	r|B	r�B	rGB	sB	s�B	sMB	r|B	q�B	q�B	sB	s�B	s�B	tTB	s�B	rGB	rGB	r�B	s�B	s�B	sMB	s�B	r�B	sB	s�B	t�B	t�B	tB	t�B	tTB	uZB	u�B	v+B	uZB	s�B	sB	r�B	s�B	tTB	t�B	sB	r|B	r�B	s�B	tB	t�B	tB	rB	u�B	v+B	v`B	x8B	w�B	w2B	v�B	v+B	u�B	xB	w2B	u�B	t�B	tB	t�B	u�B	u�B	v`B	u�B	tB	s�B	tB	u%B	uZB	uZB	t�B	s�B	s�B	t�B	u%B	t�B	sMB	r|B	sB	tTB	tTB	sMB	r|B	sB	tTB	tTB	s�B	r�B	rGB	r�B	s�B	t�B	tB	sB	s�B	t�B	u�B	u�B	t�B	tB	tB	u%B	u�B	u�B	u�B	u�B	v�B	xlB	xlB	x8B	v�B	v�B	x�B	x�B	w�B	x8B	x�B	y�B	z�B	z�B	y�B	y>B	yrB	y�B	{�B	}�B	��B	� B	��B	�B	��B	�%B	��B	�B	��B	�hB	�oB	�eB	�xB	�wB	�nB	�$B	��B	�'B	��B	�B	�cB	�	B	��B	�xB
B
B
!�B
'B
)�B
:�B
:�B
D�B
GzB
H�B
G�B
E�B
FB
U2B
bNB
`�B
e,B
ffB
rGB
k�B
f�B
l�B
�4B
�{B
}�B
�B
h�B
bNB
X�B
��B
`BB
��B
�B
�B
�MB
�B
�LB
�B
�B
��B
�B
�qB
�B
��B
�B
��B
�B
��B
�B
�B
�2B
�B
�|B
�NB
��B
��B
��B
��B
��B�B
�JB
�TB
�2B
��B
�cB
��B
�%B
�B
��B
��B
�B
�iB
��B
�mB'�B
��B
�cB
�B�BVBkB�B
�>B�B
�>B
��B
��B
��B
�dB
�#B
�B
�B
�vB
�)B
�B
�B
��B
ޞB
��B
�B
�fB
�B
�>B
�B
��B�B
�B
��B
��B
��B
�=B
��B
��B
cB
zDB
x�B
z�B
y�B
{B
yrB
x�B
{JB
y�B
y	B
y>B
y�B
}�B
�iB
�hB
�B
��B
�B
�*B
��B
�qB
�~B
|�B
�4B
z�B
iDB
�B
'�B
-wB
J#B
U�B
K)B
5tB
bB
33B
3�B
9$B
;�B
M�B
5tB
@�B
\)B
GEB
+B
"hB
�B
�B
7B
/�B
&�B
 4B	��B	��B	�B	�B	�B	�GB	� B	�oB	��B	�)B	�B	�>B	�B	�B	��B	�5B	ҽB	�B	��B	�aB	�B	�BB	�aB	��B	��B	�UB	�B	�B	�wB	�qB	��B	�0B	�}B	�aB	�B	��B	��B	�-B	��B	�CB	��B	�B	��B	��B	�B	��B	��B	�(B	��B	��B	��B	�hB	�B	��B	�=B	�"B	��B	�DB	�=B	��B	��B	�+B	��B	� B	�B	~�B	{�B	{�B	{�B	{B	z�B	z�B	x�B	yrB	x8B	x�B	w�B	wfB	v�B	wfB	w�B	xlB	|�B	��B	��B	|B	y�B	y�B	z�B	r�B	m�B	oiB	xlB	u�B	y�B	|�B	o�B	c�B	`vB	_�B	^�B	_pB	]/B	f�B	iyB	`BB	]dB	_�B	c�B	_pB	ZB	^�B	[�B	V�B	Z�B	RTB	YKB	TaB	V9B	T�B	R�B	N�B	S[B	T�B	P}B	M�B	P}B	K�B	M�B	R B	G�B	GEB	FB	E9B	E9B	D�B	FB	C�B	C�B	C�B	E�B	E9B	E�B	GB	B�B	CaB	DgB	R�B	PB	G�B	M6B	GEB	F?B	J�B	FB	H�B	E�B	FtB	HKB	J�B	GEB	DgB	D�B	DgB	C�B	H�B	K�B	N�B	NB	\�B	R�B	GB	FtB	B�B	FB	EmB	D�B	E�B	HB	H�B	IRB	IRB	H�B	H�B	HKB	IRB	F�B	G�B	EB	FB	EB	EB	C�B	B�B	D�B	?HB	?HB	@�B	>B	<�B	9$B	:*B	:�B	6�B	1[B	2�B	?�B	:^B	A�B	3�B	)*B	-�B	(�B	'�B	'RB	&�B	%B	%FB	$tB	"hB	"�B	 �B	!�B	'�B	!bB	"�B	)�B	�B	k�B	qB	�B	�B	MB	�B	�B	A�B	($G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111                                                                                                                                                                                                                                              B	�B	�IB	��B	��B	�0B	�0B	�eB	�tB	�bB	��B	��B	�bB	�B	��B	��B	��B	�xB	r�B	qJB	q�B	q~B	pxB	r�B	qB	nlB	nlB	s�B	sVB	r�B	q�B	s�B	s"B	o�B	l+B	j�B	k%B	k�B	l�B	l+B	k�B	m�B	n�B	m1B	l�B	l+B	k�B	k�B	l�B	n�B	pxB	r�B	|YB	��B	��B
�B
E�B
a�B
n�B
��B
�B
�ZB
�8B
�DB
�B
�B!B
�B
�5B
�ZB
�.B
��B
v B
r�B
v�B
�dB
B
GNB
.�B
9,B
''B
�B	��B	�fB	�B	�BB	�B	�'B	}�B	q�B	s�B	l�B	Y�B	R�B	J�B	@�B	=EB	BdB	>�B	DB	@�B	=yB	4�B	+B	B	LB	�B	�B�7B��B�B�"B�B�B�	B�B�iB��B��B��B�(B�"B��B��B��B	�B	bB	
B	�B	*B	!B	�B	"�B	*mB	&UB	-�B	*�B	*9B	,�B	5B	:3B	>�B	B�B	?QB	;mB	J�B	Y�B	b"B	e B	guB	j�B	n7B	n�B	pB	pDB	rPB	t\B	|%B	��B	��B	��B	�\B	��B	��B	��B	�B	��B	��B	��B	�9B	�3B	��B	�'B	��B	��B	��B	�WB	դB	؂B	�B	�/B	ЅB	�jB	��B	��B	��B	�LB	�B	�tB	��B	��B	��B	��B	�'B	��B	�-B	��B	��B	�B	��B	�0B	�0B	�0B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	��B	�wB	��B	��B	��B	�[B	�-B	�aB	�B	��B	��B	��B	��B	��B	�RB	��B	��B	�B	�vB	�<B	�B	��B	�<B	�B	�B	��B	�jB	�pB	�}B	��B	��B	��B	��B	��B	�TB	��B	�&B	�&B	��B	��B	��B	��B	�3B	�3B	��B	�sB	�B	��B	�KB	�QB	��B	�KB	��B	�/B	�)B	��B	�dB	�5B	�<B	��B	�B	�NB	� B	ǷB	�B	ƱB	�|B	�|B	�HB	ǂB	ǷB	�NB	��B	��B	ȉB	��B	�,B	��B	ʕB	ʕB	��B	ʕB	��B	��B	�gB	�2B	��B	��B	�,B	�,B	ɏB	�,B	ʕB	��B	�
B	ͧB	�yB	�QB	�B	�B	ЅB	кB	ыB	��B	�]B	�]B	ӘB	�B	�B	װB	ضB	�ZB	ڎB	��B	�fB	�fB	�fB	�8B	ݡB	�B	��B	�B	��B	��B	�B	�]B	�.B	�B	��B	�B	�B	��B	�B	�B	��B	�B	�B	�B	�MB	�B	�B	�B	�%B	�B	��B	��B	�B	��B	�7B	�7B	�7B	�>B	�B	�B	��B	�B	�PB	��B	�"B	�VB	�B	�B	��B	��B	��B	��B	�4B	� B	�iB	�iB	�B	�B	��B	�uB	��B	�B	��B	��B	�YB	�YB	�YB	�YB	��B	��B	��B	��B	��B	�eB	��B	�1B	��B	�1B
 =B
 �B
 �B
�B
PB
�B
�B
PB
�B
!B
�B
�B
�B
bB
4B
�B
:B
B
�B
B
�B
�B
	tB

{B

B
	�B

�B
LB
�B
RB
�B
�B
�B
�B
�B
*B
�B
eB
eB
eB
B
7B
7B
=B
kB
kB
B
B
=B
B
wB
wB
wB
�B
�B
}B
�B
�B
!B
UB
�B
'B
�B
�B
�B
UB
�B
�B
�B
�B
�B
hB
B
�B
9B
�B
�B
B
�B
�B
�B
9B
@B
�B
tB
�B
@B
B
B
�B
tB
@B
@B
�B
�B
zB
B
�B
�B
FB
B
�B
�B
�B
B
*B
XB
^B
^B
�B
 0B
�B
 dB
!6B
!�B
"qB
#wB
#BB
#�B
$IB
#�B
$}B
$�B
$B
$}B
%�B
%B
%�B
%�B
%B
%�B
%�B
''B
'[B
''B
''B
'�B
'�B
)gB
)gB
*B
*�B
+�B
,�B
,�B
,�B
,�B
,�B
-KB
,�B
-�B
.B
-�B
.RB
.B
.B
.RB
.�B
/#B
/�B
/�B
/�B
0^B
0�B
2jB
4BB
4�B
4�B
5B
4�B
5}B
4�B
5}B
5}B
5}B
5�B
6�B
6NB
6�B
6�B
6�B
7�B
8[B
8�B
9,B
:gB
:gB
:gB
:�B
:�B
;�B
;9B
<
B
;�B
<
B
<?B
<
B
<?B
<sB
<sB
<sB
=�B
=yB
>B
>�B
>�B
?B
>�B
?QB
?�B
@#B
@�B
@�B
@WB
@�B
A�B
A�B
A�B
A�B
CB
B�B
B�B
B�B
CB
CB
C�B
C�B
C�B
C�B
C�B
C�B
C�B
D�B
D�B
EB
D�B
D�B
DpB
DB
C�B
EB
EvB
E�B
G�B
G�B
HTB
H B
H�B
I&B
I�B
I�B
J�B
K�B
K2B
K�B
LmB
L8B
LmB
LmB
L�B
L�B
L�B
M
B
L�B
L�B
L�B
L�B
LmB
MsB
N�B
OB
OB
OB
O�B
OKB
OB
OB
O�B
P�B
P�B
Q#B
R�B
R�B
R�B
R�B
R�B
ScB
S�B
S�B
S�B
S�B
S�B
S�B
ScB
S/B
S�B
TB
S�B
T5B
T5B
T5B
TB
T�B
T�B
U�B
U;B
U�B
VB
VAB
VvB
W�B
W�B
W�B
W�B
XB
XNB
XNB
X�B
YTB
YTB
Y�B
Y�B
Y�B
Y�B
Z�B
ZZB
Z�B
[`B
[`B
[�B
[�B
[`B
[�B
\fB
\2B
\2B
\2B
]8B
]lB
]8B
]lB
^
B
^
B
^>B
^
B
^>B
^�B
_B
_B
_DB
_yB
_DB
_�B
`JB
`JB
`B
`B
`B
`JB
`B
`�B
aB
a�B
a�B
a�B
c]B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
c�B
dcB
d�B
e B
e�B
e�B
e�B
fB
f;B
f;B
foB
f�B
f�B
g�B
g�B
guB
g�B
g�B
g�B
g�B
g�B
hB
hGB
hB
hGB
h�B
h�B
h�B
h�B
i�B
jB
jB
jB
j�B
j�B
j�B
j�B
j�B
k%B
j�B
k�B
k�B
k�B
k�B
l+B
l�B
l�B
m1B
n7B
n7B
n7B
nlB
nlB
n�B
n�B
n�B
o	B
o	B
o>B
orB
orB
o�B
o�B
o�B
pDB
pDB
pB
p�B
pxB
pxB
pxB
p�B
p�B
p�B
p�B
q~B
q~B
q~B
q�B
rB
rPB
r�B
r�B
r�B
r�B
r�B
s"B
s"B
s"B
s�B
s�B
s�B
s�B
t(B
t(B
t\B
t(B
t�B
u.B
ubB
u�B
u�B
u�B
u�B
ubB
v4B
v4B
v�B
v�B
w:B
woB
woB
w�B
woB
w�B
w�B
w�B
xB
xB
w�B
x�B
yB
yGB
zB
z�B
z�B
zMB
z�B
z�B
z�B
z�B
{B
{SB
{�B
{�B
{�B
{�B
|%B
|%B
|�B
|�B
|�B
|�B
}�B
}�B
}�B
}�B
~eB
~�B
~�B
~�B
~�B
~eB
B
B
7B
kB
7B
kB
kB
�=B
��B
�rB
��B
�rB
�rB
�rB
�=B
��B
�B
�CB
��B
�B
��B
��B
��B
�~B
��B
��B
�B
��B
��B
��B
��B
��B
��B
�'B
�\B
��B
��B
��B
�.B
��B
��B
��B
��B
�4B
�hB
�B
��B
�nB
�nB
�nB
��B
�B
�@B
�tB
�tB
�tB
��B
��B
�FB
�FB
�FB
�FB
�FB
�FB
�FB
�FB
��B
��B
��B
�B
�LB
�LB
�LB
�LB
��B
��B
��B
��B	��B	�qB	�qB	�B	�B	�wB	�B	�}B	��B	��B	�wB	��B	�OB	�}B	�kB	��B	��B	�*B	��B	��B	�eB	��B	��B	�YB	��B	��B	�CB	��B	��B	��B	�tB	��B	��B	�:B	�!B	��B	�4B	�xB	�~B	�_B	��B	��B	��B	��B	��B	�VB	��B	��B	��B	�'B	�4B	��B	�B	�nB	��B	�nB	��B	��B	��B	��B	��B	�B	�RB	�_B	��B	�B	�FB	�B	��B	��B	�YB	��B	��B	�YB	��B	��B	��B	�0B	�0B	�RB	��B	��B	�LB	�B	��B	��B	�@B	��B	��B	7B	|�B	�B	kB	v4B	yB	s�B	q�B	q�B	qJB	q~B	qB	pxB	qB	p�B	q�B	rPB	rPB	q~B	p�B	orB	o	B	q~B	rPB	r�B	r�B	r�B	rPB	qB	p�B	p�B	qB	q~B	qJB	q�B	rB	q~B	o>B	o>B	mfB	pxB	q~B	sVB	r�B	t\B	t(B	s�B	s�B	r�B	q�B	p�B	qJB	q�B	s�B	qB	q~B	pxB	o�B	qJB	n�B	o�B	nlB	l�B	o	B	n7B	m�B	m�B	n7B	l+B	m�B	m�B	n7B	l�B	s�B	t\B	t�B	t\B	s�B	s"B	r�B	rPB	r�B	s"B	s�B	t\B	s�B	s�B	r�B	rB	r�B	s�B	s"B	s�B	r�B	p�B	pxB	orB	o�B	pB	sVB	s"B	s�B	r�B	r�B	s"B	s�B	t\B	t�B	t(B	s"B	sVB	s�B	s�B	s�B	r�B	q�B	q�B	o�B	o�B	pxB	pB	nlB	nlB	m1B	mfB	l�B	m�B	l�B	k�B	j�B	j�B	j�B	jSB	k%B	k�B	kYB	j�B	i�B	i�B	k%B	k�B	k�B	l`B	k�B	jSB	jSB	j�B	k�B	k�B	kYB	k�B	j�B	k%B	k�B	l�B	l�B	l+B	l�B	l`B	mfB	nB	n7B	mfB	k�B	k%B	j�B	k�B	l`B	l�B	k%B	j�B	j�B	k�B	l+B	l�B	l+B	jB	m�B	n7B	nlB	pDB	o�B	o>B	n�B	n7B	nB	pB	o>B	m�B	l�B	l+B	l�B	m�B	nB	nlB	m�B	l+B	k�B	l+B	m1B	mfB	mfB	l�B	k�B	k�B	l�B	m1B	l�B	kYB	j�B	k%B	l`B	l`B	kYB	j�B	k%B	l`B	l`B	k�B	j�B	jSB	j�B	k�B	l�B	l+B	k%B	k�B	l�B	m�B	m�B	l�B	l+B	l+B	m1B	nB	m�B	nB	m�B	n�B	pxB	pxB	pDB	o	B	o	B	p�B	p�B	o�B	pDB	p�B	q�B	r�B	r�B	q�B	qJB	q~B	q�B	s�B	v B	x�B	xB	y�B	}+B	��B	~1B	|�B	}+B	��B	�tB	�{B	�qB	��B	��B	�zB	�0B	�B	�3B	��B	ыB	�oB	�B	��B	�B	�B
B
�B
*B
!�B
2�B
2�B
<�B
?�B
@�B
?�B
=�B
>B
M>B
ZZB
X�B
]8B
^rB
jSB
c�B
^�B
d�B
x@B
{�B
u�B
w�B
`�B
ZZB
P�B
��B
XNB
��B
�B
�'B
�YB
�B
�XB
�B
�B
��B
�B
�}B
�&B
��B
�&B
��B
�B
��B
ݡB
��B
�>B
��B
وB
�ZB
�B
�B
��B
�B
��B
�B
�VB
�`B
�>B
� B
�oB
��B
�1B
�B
��B
��B
�%B
�uB
��B
�yB�B
��B
�oB�B�BbBwB
��B
�JB�B
�JB
�B
��B
��B
�pB
�/B
�B
�#B
؂B
�5B
�B
ݡB
��B
֪B
��B
�B
�rB
�B
�JB
�(B
��B�B
�'B
��B
��B
��B
�IB
��B
|�B
woB
rPB
p�B
r�B
q�B
s"B
q~B
p�B
sVB
q�B
qB
qJB
q�B
v B
xuB
�tB
�B
��B
�B
�6B
��B
�}B
��B
t�B
�@B
r�B
aPB
�'B
�B
%�B
B/B
M�B
C5B
-�B
nB
+?B
+�B
10B
3�B
E�B
-�B
8�B
T5B
?QB
#B
tB
�B
�B
CB
'�B
�B	�@B	��B	��B	�B	�B	�+B	�SB	�B	�{B	��B	�5B	�B	�JB	�(B	�B	��B	�AB	��B	�&B	��B	�mB	� B	�NB	�mB	��B	��B	�aB	� B	�B	��B	�}B	��B	�<B	��B	�mB	�#B	��B	��B	�9B	��B	�OB	��B	�B	��B	��B	�B	��B	��B	�4B	��B	��B	��B	�tB	�!B	��B	�IB	�.B	|�B	�PB	�IB	��B	�	B	7B	~�B	xB	yB	wB	s�B	s�B	s�B	s"B	r�B	r�B	p�B	q~B	pDB	p�B	o�B	orB	o	B	orB	o�B	pxB	t�B	��B	}�B	t(B	q�B	q�B	r�B	j�B	e�B	guB	pxB	m�B	q�B	t�B	g�B	[�B	X�B	W�B	V�B	W|B	U;B	^�B	a�B	XNB	UpB	W�B	[�B	W|B	R)B	V�B	S�B	N�B	R�B	J`B	QWB	LmB	NEB	L�B	J�B	F�B	KgB	M
B	H�B	E�B	H�B	C�B	E�B	J,B	?�B	?QB	>B	=EB	=EB	<�B	>B	;�B	<
B	<
B	=�B	=EB	=�B	?B	:�B	;mB	<sB	J�B	H B	?�B	EBB	?QB	>KB	B�B	>B	@�B	=�B	>�B	@WB	B�B	?QB	<sB	<�B	<sB	<
B	@�B	C�B	F�B	FB	T�B	J�B	?B	>�B	:�B	>B	=yB	<�B	=�B	@#B	@�B	A^B	A^B	@�B	@�B	@WB	A^B	>�B	?�B	=B	>B	=B	=B	;�B	;B	<�B	7TB	7TB	8�B	6B	4�B	10B	26B	2�B	.�B	)gB	+B	7�B	2jB	9�B	+�B	!6B	%�B	!B	�B	^B	�B	B	RB	�B	tB	�B	�B	�B	�B	nB	�B	!�B	�B	c�B	}B		�B	�B	YB	�B	�B	9�B	 0G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111441111111                                                                                                                                                                                                                                              G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223232                            20230426223232AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622323220230426223232  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622323220230426223232QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622323220230426223232QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               