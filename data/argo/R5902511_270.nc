CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-07-27T09:00:48Z creation      
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
resolution        =���   axis      Z        �  ;�   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  R`   PRES_ADJUSTED            
      	   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     
_FillValue        G�O�   units         decibar    	valid_min                	valid_max         F;�    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  X    PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  n�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         decibar    C_format      %7.1f      FORTRAN_format        F7.1   
resolution        =���     �  t    TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �    TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      
_FillValue        G�O�   units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  �`   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     
_FillValue        G�O�   units         psu    	valid_min         @      	valid_max         B$     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     �  ��   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �     PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     
_FillValue        G�O�   units         psu    C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o     � �   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` (    SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   (�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   .�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   4�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T :�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   :�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   :�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   :�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   :�   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � :�   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ;t   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ;�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ;�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ;�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ;�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ;�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ;�Argo profile    3.1 1.2 19500101000000  20230727090048  20230727090048  5902511 5902511 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                AA  AOAO6810                            6810                            2B  2B  AA  SOLO_II                         SOLO_II                         8521                            8521                            SBE602 27Jul16                  SBE602 27Jul16                  853 853 @�=��\�@@�=��\�@11  @�=��i @�=��i @1�s��@1�s���d������d�����11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AA  AA  FF  ?u?�@:�H@�  @�  @�  @�\@��RA\)A\)A+�A@  A_\)A~�RA��A�  A�  A���AУ�A�  A�Q�B   B  BQ�B(�B�
B(  B0  B8(�B@(�BH  BO�
BW�
B_�Bg�Bo�
Bx  B�  B�  B�  B��B�  B��B��B�  B�  B��B��B��B�{B�(�B�  B�  B�  B�  B��
B��
B��B�{B��B�  B�{B�  B��B��B�  B�(�B�=qB�{C   C  C
=C��C  C

=C{C{C��C  C
=C  C
=C
=C��C��C 
=C"  C$  C&
=C'��C)��C,  C-��C0  C2{C4  C6  C8{C:{C<
=C>
=C@{CB
=CC��CE�CG�CI��CK�CM�CO��CR  CS��CV
=CW��CZ
=C[��C^  C`  Cb
=Cd  Ce�Cg��Cj
=Cl  Cm�Cp  Cr  Ct
=Cv{Cw��Cy��C|  C~{C��C�  C�C�C�C�  C�  C�C�  C�  C�  C�  C���C���C�  C�  C�  C���C�  C���C�  C�  C���C�  C�  C�C�  C�  C���C�  C�  C���C�  C�C�C���C���C�  C�  C�C�  C�  C�  C�  C�C�C�
=C�  C�C�
=C�C�
=C�  C���C�C�C�  C�  C�C�  C���C���C�  C�C�  C�  C�C�  C�C���C�  C�  C���C���C���C���C�  C�  C�  C���C���C���C�C�
=C�C�C�C�C�  C�  C�C�C���C�C�
=C�  C�C�  C�  C�  C���C�C���C�  C�C���C���C���C�  C�  C���C���C���C���C�  C�  C���C�  C�  C���C�  C�  C�C�  C���C�C�  C�  D �D � D  D��DD��D  D��D�D� D  D��D  D��D�D� D  D}qD	  D	��D
�D
� D�D� D�qD��DD��D�D��D  D�D�D��DD��D�D��D�D��D�D�D�D� D�qD}qD�qD��D�D� D�qDz�D�qD��D�qD}qD�D}qD��D� D�D}qD�qD}qD�qD � D!  D!� D"  D"� D#�D#��D#�qD$� D%  D%� D&�D&� D'  D'�D(�D(}qD(�qD)}qD*  D*� D+�D+� D,  D,��D,�qD-� D.  D.��D/D/��D0  D0� D1D1�D2�D2� D2�qD3}qD3�qD4� D5D5��D5�qD6}qD7  D7� D8D8��D9�D9� D9�qD:z�D:��D;}qD<�D<�D=  D=}qD=��D>}qD>�qD?}qD?�qD@}qD@�qDA� DB  DB}qDB�qDC� DD  DD� DD�qDE� DF  DF}qDF��DG}qDH  DH��DI  DI� DJ�DJ}qDJ�qDK� DL  DL� DL�qDM� DM�qDNz�DO  DO}qDO�qDP� DQ  DQ��DR  DR� DS  DS� DT�DT�DUDU��DU�qDV}qDV��DW� DX  DXxRDX��DY}qDZ  DZ��D[�D[� D[�qD\� D]  D]}qD]��D^z�D^�qD_� D`  D`��DaDa� Db  Db��Dc  Dc� Dc�qDd}qDe  De� Df�Df� Dg  Dg}qDg�qDh��Di�Di}qDi�qDj}qDj�qDk}qDk�qDl}qDm  Dm� Dm��DnxRDn�qDo� Dp  Dp�Dq  Dq}qDr  Dr�Ds�Ds��Ds�qDt� Dt�qDu}qDv�Dv��Dv�qDw}qDx�Dx��Dy  Dy�Dz�Dz�D{�D{�D|�D|� D}  D}� D~  D~}qD~�qD��D�  D�>�D�~�D�� D�HD�@ D�~�D�� D�  D�@ D�~�D���D���D�>�D�� D���D�  D�@ D�� D�� D�  D�>�D�� D�� D�  D�@ D�~�D�� D�HD�AHD�� D���D���D�AHD��HD�� D�  D�@ D��HD�� D�  D�AHD�� D�� D�  D�>�D�~�D���D��qD�@ D�~�D�� D�HD�AHD�h�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?#�
?.{?aG�?�z�?��
?�
=?�(�@�\@�@(�@#�
@0��@@  @J=q@\(�@c�
@u@�  @��
@�{@��@��H@��
@��@��@���@�G�@�{@�33@�  @�ff@�\)@���A   A�A
�HA��A�\AffA=qA ��A#33A(��A,��A1G�A6ffA8��A>{AB�\AEAK�AP  AS33AY��A\��Ab�\AfffAj=qAp��As�
Ax��A}p�A���A��A��A�Q�A���A�z�A�{A���A�33A���A��A�G�A��
A�{A��A��\A�(�A�{A���A�=qA���A��RA���A��A��A�\)A�=qA��
A�ffA���A�=qA�p�AƸRAə�A�33A�A�  A�G�A�(�A�A׮A�=qAۅA�ffA��A��A�z�A�A�Q�A�=qA�(�A�RA�  A�\A�z�A�{A�  A�=qA��A�A��B z�BB�RB\)B��B��BffB�Bz�B	p�B
�\B33Bz�Bp�B=qB�B(�BG�BffB
=B  Bp�B=qB�HBQ�B�B�B\)B(�B��B=qB�HB   B!G�B"{B#
=B$Q�B%G�B%�B'\)B((�B)G�B*ffB+
=B,z�B-�B.=qB/�B0(�B1�B2�\B333B4Q�B5p�B6=qB7�B8Q�B9��B:ffB;33B<��B=p�B>�\B?�
B@z�BABB�HBC�BD��BF{BF�RBH(�BI�BI�BK\)BL(�BM�BN�\BO\)BP(�BQp�BRffBS\)BT��BUp�BV�\BW�BXz�BYp�BZ�HB[�B\z�B]B^ffB_�B`��Ba��Bb�\Bd  Bd��Be��Bg
=Bg�
Bh��Bj=qBj�HBlQ�Bl��Bn=qBo�Bp(�Bqp�BrffBs\)Bt��Bup�Bv�HBw�Bx��By�Bz�RB{�
B}�B}B
=B�{B�ffB��B��B�  B���B���B���B�=qB��\B�33B��B�{B���B�33B���B�ffB��HB�33B��B�Q�B���B��B��
B�z�B�
=B�p�B�{B��\B���B���B�(�B�z�B�
=B�B�{B��\B�G�B���B�(�B���B��B���B�Q�B��\B�33B�B�(�B���B�G�B���B�{B��RB���B���B�{B�ffB�
=B��B��
B�ffB���B�\)B�B�ffB��HB�33B��B�ffB���B�33B�B�(�B��\B�33B���B�  B��RB�33B���B�(�B���B��B��B�ffB��RB�33B��B�z�B��RB�G�B��B�Q�B��RB�\)B��
B�=qB���B�p�B�B�=qB���B�\)B�B�Q�B���B�p�B��
B�ffB��B��B��B��\B��B��B�(�B���B�33B��B�Q�B���B�33B��
B�z�B���B�33B��
B��\B���B�G�B��B\B��HB�\)B�{Bď\B���B�p�B�(�B�z�B���BǙ�B�=qBȣ�B�
=B�B�ffBʸRB�33B��B�ffB���B�G�B�  BΏ\B�
=Bϙ�B�=qB��HB�G�B��
B�z�B�33BӮB�{B��HBՅB�  B�ffB��B��
B�=qBظRB�\)B�{Bڣ�B�33BۮB�=qB���B�p�B��Bޣ�B�G�B߮B�=qB�
=BᙚB�{B�RB�p�B��B�z�B�33B�B�Q�B���B�B�(�B���B陚B�{B�\B�33B��
B�=qB��HB�B�=qB��B�33B�B�z�B�
=B�B�{B��HB�\)B��B���B�33B��B�Q�B��B���B�(�B��HB�p�B��B���B�\)B��B�z�B�33B��B�ffB�33B��C =qC �C �C=qCz�C��C33C�C��C�Cz�C�
C{CffC�
C{C\)CC{C\)C��C
=C\)C��C��C\)C��C�C	Q�C	�C	�C
=qC
��C
��C=qC�C�CG�C��C�
C(�C�\C�HC�CffC��C�C\)C�C{CffC��C  CffC�C��CQ�C�RC��C=qC��C  C=qC�\C��CG�C�\C�HCG�C��C�HC33C��C�C(�C�C��C(�C�C��C{Cz�C�
C
=CffCC
=CG�C�C
=C=qC�C�HC33Cp�C��C(�C\)C�RC 
=C G�C ��C!  C!Q�C!�\C!�HC"G�C"�C"��C#33C#z�C#�RC$
=C$p�C$��C$�C%Q�C%��C%��C&�C&�C&��C'
=C'Q�C'�C(  C(G�C(z�C(�
C)(�C)ffC)��C*  C*Q�C*�\C*C+
=C+ffC+�C+�HC,(�C,�C,��C-  C-G�C-��C-��C.�C.ffC.C/
=C/=qC/z�C/��C0�C0ffC0��C0�
C1(�C1�C1C2  C2\)C2�C2�C3(�C3p�C3��C4�C4ffC4�\C4�HC533C5�C5�
C6�C6ffC6��C6�C7=qC7�\C7�HC8(�C8p�C8�C9  C9\)C9�RC:
=C:G�C:�\C:�
C;33C;�\C;�HC<�C<\)C<��C<��C=Q�C=��C=��C>33C>z�C>C?�C?p�C?��C@{C@ffC@�RC@��CA=qCA��CA��CBQ�CB��CB��CC33CCz�CC�HCD=qCD�\CD�CE=qCE�CE��CF�CFz�CF�
CG33CG�\CG��CH�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                    111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111?u?�@:�H@�  @�  @�  @�\@��RA\)A\)A+�A@  A_\)A~�RA��A�  A�  A���AУ�A�  A�Q�B   B  BQ�B(�B�
B(  B0  B8(�B@(�BH  BO�
BW�
B_�Bg�Bo�
Bx  B�  B�  B�  B��B�  B��B��B�  B�  B��B��B��B�{B�(�B�  B�  B�  B�  B��
B��
B��B�{B��B�  B�{B�  B��B��B�  B�(�B�=qB�{C   C  C
=C��C  C

=C{C{C��C  C
=C  C
=C
=C��C��C 
=C"  C$  C&
=C'��C)��C,  C-��C0  C2{C4  C6  C8{C:{C<
=C>
=C@{CB
=CC��CE�CG�CI��CK�CM�CO��CR  CS��CV
=CW��CZ
=C[��C^  C`  Cb
=Cd  Ce�Cg��Cj
=Cl  Cm�Cp  Cr  Ct
=Cv{Cw��Cy��C|  C~{C��C�  C�C�C�C�  C�  C�C�  C�  C�  C�  C���C���C�  C�  C�  C���C�  C���C�  C�  C���C�  C�  C�C�  C�  C���C�  C�  C���C�  C�C�C���C���C�  C�  C�C�  C�  C�  C�  C�C�C�
=C�  C�C�
=C�C�
=C�  C���C�C�C�  C�  C�C�  C���C���C�  C�C�  C�  C�C�  C�C���C�  C�  C���C���C���C���C�  C�  C�  C���C���C���C�C�
=C�C�C�C�C�  C�  C�C�C���C�C�
=C�  C�C�  C�  C�  C���C�C���C�  C�C���C���C���C�  C�  C���C���C���C���C�  C�  C���C�  C�  C���C�  C�  C�C�  C���C�C�  C�  D �D � D  D��DD��D  D��D�D� D  D��D  D��D�D� D  D}qD	  D	��D
�D
� D�D� D�qD��DD��D�D��D  D�D�D��DD��D�D��D�D��D�D�D�D� D�qD}qD�qD��D�D� D�qDz�D�qD��D�qD}qD�D}qD��D� D�D}qD�qD}qD�qD � D!  D!� D"  D"� D#�D#��D#�qD$� D%  D%� D&�D&� D'  D'�D(�D(}qD(�qD)}qD*  D*� D+�D+� D,  D,��D,�qD-� D.  D.��D/D/��D0  D0� D1D1�D2�D2� D2�qD3}qD3�qD4� D5D5��D5�qD6}qD7  D7� D8D8��D9�D9� D9�qD:z�D:��D;}qD<�D<�D=  D=}qD=��D>}qD>�qD?}qD?�qD@}qD@�qDA� DB  DB}qDB�qDC� DD  DD� DD�qDE� DF  DF}qDF��DG}qDH  DH��DI  DI� DJ�DJ}qDJ�qDK� DL  DL� DL�qDM� DM�qDNz�DO  DO}qDO�qDP� DQ  DQ��DR  DR� DS  DS� DT�DT�DUDU��DU�qDV}qDV��DW� DX  DXxRDX��DY}qDZ  DZ��D[�D[� D[�qD\� D]  D]}qD]��D^z�D^�qD_� D`  D`��DaDa� Db  Db��Dc  Dc� Dc�qDd}qDe  De� Df�Df� Dg  Dg}qDg�qDh��Di�Di}qDi�qDj}qDj�qDk}qDk�qDl}qDm  Dm� Dm��DnxRDn�qDo� Dp  Dp�Dq  Dq}qDr  Dr�Ds�Ds��Ds�qDt� Dt�qDu}qDv�Dv��Dv�qDw}qDx�Dx��Dy  Dy�Dz�Dz�D{�D{�D|�D|� D}  D}� D~  D~}qD~�qD��D�  D�>�D�~�D�� D�HD�@ D�~�D�� D�  D�@ D�~�D���D���D�>�D�� D���D�  D�@ D�� D�� D�  D�>�D�� D�� D�  D�@ D�~�D�� D�HD�AHD�� D���D���D�AHD��HD�� D�  D�@ D��HD�� D�  D�AHD�� D�� D�  D�>�D�~�D���D��qD�@ D�~�D�� D�HD�AHD�h�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�?#�
?.{?aG�?�z�?��
?�
=?�(�@�\@�@(�@#�
@0��@@  @J=q@\(�@c�
@u@�  @��
@�{@��@��H@��
@��@��@���@�G�@�{@�33@�  @�ff@�\)@���A   A�A
�HA��A�\AffA=qA ��A#33A(��A,��A1G�A6ffA8��A>{AB�\AEAK�AP  AS33AY��A\��Ab�\AfffAj=qAp��As�
Ax��A}p�A���A��A��A�Q�A���A�z�A�{A���A�33A���A��A�G�A��
A�{A��A��\A�(�A�{A���A�=qA���A��RA���A��A��A�\)A�=qA��
A�ffA���A�=qA�p�AƸRAə�A�33A�A�  A�G�A�(�A�A׮A�=qAۅA�ffA��A��A�z�A�A�Q�A�=qA�(�A�RA�  A�\A�z�A�{A�  A�=qA��A�A��B z�BB�RB\)B��B��BffB�Bz�B	p�B
�\B33Bz�Bp�B=qB�B(�BG�BffB
=B  Bp�B=qB�HBQ�B�B�B\)B(�B��B=qB�HB   B!G�B"{B#
=B$Q�B%G�B%�B'\)B((�B)G�B*ffB+
=B,z�B-�B.=qB/�B0(�B1�B2�\B333B4Q�B5p�B6=qB7�B8Q�B9��B:ffB;33B<��B=p�B>�\B?�
B@z�BABB�HBC�BD��BF{BF�RBH(�BI�BI�BK\)BL(�BM�BN�\BO\)BP(�BQp�BRffBS\)BT��BUp�BV�\BW�BXz�BYp�BZ�HB[�B\z�B]B^ffB_�B`��Ba��Bb�\Bd  Bd��Be��Bg
=Bg�
Bh��Bj=qBj�HBlQ�Bl��Bn=qBo�Bp(�Bqp�BrffBs\)Bt��Bup�Bv�HBw�Bx��By�Bz�RB{�
B}�B}B
=B�{B�ffB��B��B�  B���B���B���B�=qB��\B�33B��B�{B���B�33B���B�ffB��HB�33B��B�Q�B���B��B��
B�z�B�
=B�p�B�{B��\B���B���B�(�B�z�B�
=B�B�{B��\B�G�B���B�(�B���B��B���B�Q�B��\B�33B�B�(�B���B�G�B���B�{B��RB���B���B�{B�ffB�
=B��B��
B�ffB���B�\)B�B�ffB��HB�33B��B�ffB���B�33B�B�(�B��\B�33B���B�  B��RB�33B���B�(�B���B��B��B�ffB��RB�33B��B�z�B��RB�G�B��B�Q�B��RB�\)B��
B�=qB���B�p�B�B�=qB���B�\)B�B�Q�B���B�p�B��
B�ffB��B��B��B��\B��B��B�(�B���B�33B��B�Q�B���B�33B��
B�z�B���B�33B��
B��\B���B�G�B��B\B��HB�\)B�{Bď\B���B�p�B�(�B�z�B���BǙ�B�=qBȣ�B�
=B�B�ffBʸRB�33B��B�ffB���B�G�B�  BΏ\B�
=Bϙ�B�=qB��HB�G�B��
B�z�B�33BӮB�{B��HBՅB�  B�ffB��B��
B�=qBظRB�\)B�{Bڣ�B�33BۮB�=qB���B�p�B��Bޣ�B�G�B߮B�=qB�
=BᙚB�{B�RB�p�B��B�z�B�33B�B�Q�B���B�B�(�B���B陚B�{B�\B�33B��
B�=qB��HB�B�=qB��B�33B�B�z�B�
=B�B�{B��HB�\)B��B���B�33B��B�Q�B��B���B�(�B��HB�p�B��B���B�\)B��B�z�B�33B��B�ffB�33B��C =qC �C �C=qCz�C��C33C�C��C�Cz�C�
C{CffC�
C{C\)CC{C\)C��C
=C\)C��C��C\)C��C�C	Q�C	�C	�C
=qC
��C
��C=qC�C�CG�C��C�
C(�C�\C�HC�CffC��C�C\)C�C{CffC��C  CffC�C��CQ�C�RC��C=qC��C  C=qC�\C��CG�C�\C�HCG�C��C�HC33C��C�C(�C�C��C(�C�C��C{Cz�C�
C
=CffCC
=CG�C�C
=C=qC�C�HC33Cp�C��C(�C\)C�RC 
=C G�C ��C!  C!Q�C!�\C!�HC"G�C"�C"��C#33C#z�C#�RC$
=C$p�C$��C$�C%Q�C%��C%��C&�C&�C&��C'
=C'Q�C'�C(  C(G�C(z�C(�
C)(�C)ffC)��C*  C*Q�C*�\C*C+
=C+ffC+�C+�HC,(�C,�C,��C-  C-G�C-��C-��C.�C.ffC.C/
=C/=qC/z�C/��C0�C0ffC0��C0�
C1(�C1�C1C2  C2\)C2�C2�C3(�C3p�C3��C4�C4ffC4�\C4�HC533C5�C5�
C6�C6ffC6��C6�C7=qC7�\C7�HC8(�C8p�C8�C9  C9\)C9�RC:
=C:G�C:�\C:�
C;33C;�\C;�HC<�C<\)C<��C<��C=Q�C=��C=��C>33C>z�C>C?�C?p�C?��C@{C@ffC@�RC@��CA=qCA��CA��CBQ�CB��CB��CC33CCz�CC�HCD=qCD�\CD�CE=qCE�CE��CF�CFz�CF�
CG33CG�\CG��CH�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                    111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�XA�dZA�dZA�ffA�hsA�n�A�n�A�p�A�v�A�t�A�v�A�v�A�x�A�v�A�ffA�dZA�l�A�r�A�x�A�z�A�v�A�t�A�jA�hsA�&�A�"�A�"�A� �A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A�
=A��A�\)A��yA��/A֡�Aէ�A�"�A�(�Aҡ�AѸRA�ffA�r�AΗ�A���A�bA���A��`A�l�Aǣ�A�ZA�(�A�r�A�JA�33A�S�A�&�A�A��A���A�%A�bA��
A��RA�x�A�/A�O�A��
A���A�^5A���A���A�I�A��A�ZA���A�+A�x�A���A���A�ƨA��yA�5?A���A�1A�oA��A��A�ȴA���A�~�A��A�E�A�x�A��hA�=qA��\A���A��\A��/A��DA�=qA���A��A��A��RA}G�AzȴAzn�Ax�+Au��Arn�Aop�Ak�^Ai�AfĜAcK�A`ZA_33A^JA]%A[�AYAW�AT~�AQ��AN�/AL��AJ�/AI��AG33AE&�ACC�A@=qA> �A<bA:�A9�hA7hsA5��A4��A3
=A1��A1�hA0�jA.n�A-p�A+C�A(��A'l�A&�A%7LA#+A"�A"�A"bNA"-A!oAK�A�A��A��A�A-A�PA�A�A��A��A�wA��Ar�A��AG�A��AS�A�yAĜA��AG�A��A{AbA�;AC�A�!A33A�
A"�A
�!A	ƨA	dZA��Az�A��A33AM�A��AC�A"�A�9A��A��AdZ@��@�X@��/@���@�{@��u@�1'@�b@�;d@��@�p�@�O�@��`@���@�r�@��y@�@���@�9@��@�C�@�F@�  @��@�j@���@��@�+@��@�9X@�"�@�~�@�X@�9X@�(�@� �@�(�@���@�@�ȴ@�^5@�7L@�Ĝ@�9X@���@���@���@�`B@��@��m@�\)@�G�@��m@��
@�"�@ڇ+@�-@١�@أ�@�l�@ָR@֏\@և+@և+@�E�@�V@�bN@��;@�;d@�{@�x�@�p�@�I�@�ȴ@́@��`@�Q�@�dZ@�ȴ@ʏ\@ɺ^@��`@�(�@�+@Ɵ�@�E�@��@�r�@��m@öF@�K�@��H@�ff@�hs@��@�Ĝ@�1'@��m@�;d@�
=@�o@�33@�o@�E�@��T@��-@�G�@�V@���@��@��P@���@�ff@�{@���@��@�?}@�7L@��@��@�z�@��@���@�;d@�n�@�-@���@��-@�O�@��9@�Z@��m@�33@�@���@�n�@�E�@�$�@��T@�@���@�hs@�/@���@��u@��
@���@��
@��@�
=@�v�@��@��@��^@�?}@��j@�Z@���@�;d@�"�@��R@�=q@�J@���@�7L@�Ĝ@�z�@�ƨ@�33@���@�@�@��h@���@�x�@��@�r�@�A�@� �@���@���@�
=@���@�ȴ@���@�^5@��@��T@���@�&�@���@�bN@�A�@� �@��@�|�@�o@��y@���@�5?@��7@�G�@��@��@���@�Z@� �@���@�S�@�
=@��y@���@�V@�-@���@���@���@�hs@�?}@���@���@���@�bN@� �@��@��;@�ƨ@���@�;d@���@��+@�V@�=q@�-@�{@��@���@��-@���@���@�X@��/@��u@� �@��m@��P@�dZ@�\)@�@���@��!@�V@�E�@�$�@�{@�{@��@���@�@���@��7@�`B@�&�@���@��j@���@�r�@�(�@�1@�ƨ@�C�@�
=@��!@�v�@�V@�{@��T@�@���@��h@�p�@�G�@�7L@���@�Ĝ@��j@��@��u@�j@�9X@�  @��@��R@�$�@��T@��^@�x�@�p�@�hs@�`B@�7L@�&�@��@��@��@��@�V@���@��`@���@���@��D@�z�@�r�@�Z@�9X@�(�@���@�\)@�o@���@�v�@�$�@���@��-@��h@�hs@�O�@�&�@�%@���@���@��D@��@�r�@�j@�Z@�9X@��
@��!@�n�@�$�@��T@���@�?}@��/@�Z@��@K�@~��@}�-@}/@{�m@{C�@{o@z��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�\)A�XA�XA�ZA�VA�ffA�dZA�hsA�bNA�ffA�dZA�dZA�ffA�bNA�hsA�bNA�dZA�bNA�hsA�hsA�ffA�jA�hsA�ffA�jA�ffA�t�A�r�A�n�A�p�A�l�A�jA�p�A�l�A�v�A�z�A�t�A�x�A�t�A�r�A�x�A�t�A�x�A�t�A�t�A�v�A�r�A�v�A�v�A�t�A�x�A�v�A�v�A�|�A�t�A�z�A�x�A�x�A܁A�z�A�x�A�t�A�r�A�t�A�l�A�l�A�hsA�n�A�ffA�dZA�dZA�`BA�ffA�dZA�ffA�dZA�`BA�hsA�dZA�dZA�ffA�bNA�hsA�l�A�n�A�t�A�r�A�r�A�x�A�hsA�jA�p�A�r�A�z�A�v�A�z�A�v�A�x�A�z�A�t�A�|�A�x�A�x�A�~�A�x�A�z�A�z�A�v�A�~�A�x�A�z�A�x�A�v�A�|�A�x�A�z�A�r�A�n�A�v�A�x�A�x�A�|�A�~�A�v�A�n�A�jA�dZA�jA�jA�jA�l�A�ffA�jA�jA�ffA�l�A�jA�ffA�n�A�ffA�hsA�l�A�`BA�VA�&�A�&�A�"�A�&�A�$�A�"�A�&�A�$�A� �A�&�A� �A�"�A�&�A� �A�"�A�&�A�$�A� �A�$�A� �A�"�A�"�A��A�"�A��A� �A�$�A� �A��A�$�A��A� �A� �A��A�"�A��A�"�A��A��A�"�A��A��A�"�A��A��A� �A��A��A��A��A� �A��A��A� �A��A��A��A��A��A��A��A��A� �A��A��A� �A��A��A� �A��A��A� �A��A��A��A��A��A� �A��A��A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A��A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A��A��A�{A��A��A�oA��A��A�oA�{A��A�oA�bA�bA�
=A�1A�
=A�A�  A�A���A���A���A��A��TA��HA۸RAۗ�Aۉ7A�|�A�p�A�O�A�A�A�A�A�/A�-A�33A�=qA�-A�
=A���Aں^A�|�A�ZA�A�A�(�A��A�z�A���Aׇ+A�VA�G�A�9XA�1'A��A־wA։7A�9XA�JA���A��A��#AոRAե�AՋDA�z�A�t�A�l�A�\)A�K�A�?}A�1'A��A�VA���A��TAԾwAԬAԁA�$�A��`A��;A���AӶFA�r�A�9XAҾwA�bNA�K�A�1'A�VA��mA���AѼjAѶFAѰ!Aѩ�Aѣ�Aѥ�Aѣ�AѓuAсA�r�A�ZA�?}A�(�A� �A�
=A��A��
AЬA�`BA���AϋDA�jA��A���A�ĜAΟ�A�\)A�bAͧ�A�p�A�E�A�9XA�(�A�VA̰!A�bA˟�A�O�A��A�  A���A��mA��/A��A���A�~�A�AɁA�\)A�A�A�33A�&�A�oA��A���AȺ^Aȴ9AȮAȡ�AȑhA�~�A�n�A�=qA�+A��A�bA�  A���AǗ�A�O�A�oA��A���Aƛ�A�`BA��A��/AžwAŅA�M�A�&�A�
=A��A��mA���AļjAę�A�l�A�G�A�;dA�9XA�+A�"�A��A�VA�VA���A��yA��mA��yA��
A�n�A�oAA�9XA�VA��;A���A�r�A�I�A���A�\)A� �A���A��A��yA��A��jA�C�A��`A��RA���A�l�A�hsA�`BA�A�A� �A��mA��!A�^5A��A��A��TA��^A�O�A��;A���A�C�A�oA�
=A��`A��PA�`BA�9XA�JA�%A��A��A���A��;A���A���A��
A���A���A�ƨA�ȴA�ȴA��A��!A���A���A��\A���A��7A�bNA�O�A�K�A�G�A�C�A�G�A�?}A��A�A��9A�~�A�Q�A��A��A���A�A�A��A�A��^A�r�A�$�A���A���A���A���A�ƨA��wA���A��hA�r�A�K�A�-A��A�VA�%A���A���A���A��A��mA��;A��A���A��jA��-A���A��DA�x�A�^5A�C�A��A��yA���A���A���A�%A���A��-A�{A��^A�~�A�E�A�+A��A��A��A�{A��A�ƨA��A�ZA�1A���A���A�M�A�VA��^A��A�XA�-A��yA�p�A�  A���A��A�hsA�(�A�ĜA��\A�-A��A�
=A�1A�
=A�A���A��TA���A�O�A�A��A�S�A���A��DA��yA���A�VA�$�A�1A��A��`A���A��^A���A��PA�r�A���A���A��DA��HA�M�A���A���A���A��A�n�A�ZA�I�A�;dA�$�A�{A�JA�  A��HA��^A���A���A�/A��A�~�A��A���A���A�ffA���A��#A���A���A��wA��hA�A��A���A�XA�bA��A�(�A��A�n�A�1'A�"�A�1A��;A��FA��A�bNA�XA�K�A�1'A�l�A�"�A���A�\)A�-A�JA��TA��\A�I�A�{A��A�t�A�A�A�;dA�5?A�+A�&�A�JA��
A��PA�E�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                    111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111A�XA�dZA�dZA�ffA�hsA�n�A�n�A�p�A�v�A�t�A�v�A�v�A�x�A�v�A�ffA�dZA�l�A�r�A�x�A�z�A�v�A�t�A�jA�hsA�&�A�"�A�"�A� �A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A�
=A��A�\)A��yA��/A֡�Aէ�A�"�A�(�Aҡ�AѸRA�ffA�r�AΗ�A���A�bA���A��`A�l�Aǣ�A�ZA�(�A�r�A�JA�33A�S�A�&�A�A��A���A�%A�bA��
A��RA�x�A�/A�O�A��
A���A�^5A���A���A�I�A��A�ZA���A�+A�x�A���A���A�ƨA��yA�5?A���A�1A�oA��A��A�ȴA���A�~�A��A�E�A�x�A��hA�=qA��\A���A��\A��/A��DA�=qA���A��A��A��RA}G�AzȴAzn�Ax�+Au��Arn�Aop�Ak�^Ai�AfĜAcK�A`ZA_33A^JA]%A[�AYAW�AT~�AQ��AN�/AL��AJ�/AI��AG33AE&�ACC�A@=qA> �A<bA:�A9�hA7hsA5��A4��A3
=A1��A1�hA0�jA.n�A-p�A+C�A(��A'l�A&�A%7LA#+A"�A"�A"bNA"-A!oAK�A�A��A��A�A-A�PA�A�A��A��A�wA��Ar�A��AG�A��AS�A�yAĜA��AG�A��A{AbA�;AC�A�!A33A�
A"�A
�!A	ƨA	dZA��Az�A��A33AM�A��AC�A"�A�9A��A��AdZ@��@�X@��/@���@�{@��u@�1'@�b@�;d@��@�p�@�O�@��`@���@�r�@��y@�@���@�9@��@�C�@�F@�  @��@�j@���@��@�+@��@�9X@�"�@�~�@�X@�9X@�(�@� �@�(�@���@�@�ȴ@�^5@�7L@�Ĝ@�9X@���@���@���@�`B@��@��m@�\)@�G�@��m@��
@�"�@ڇ+@�-@١�@أ�@�l�@ָR@֏\@և+@և+@�E�@�V@�bN@��;@�;d@�{@�x�@�p�@�I�@�ȴ@́@��`@�Q�@�dZ@�ȴ@ʏ\@ɺ^@��`@�(�@�+@Ɵ�@�E�@��@�r�@��m@öF@�K�@��H@�ff@�hs@��@�Ĝ@�1'@��m@�;d@�
=@�o@�33@�o@�E�@��T@��-@�G�@�V@���@��@��P@���@�ff@�{@���@��@�?}@�7L@��@��@�z�@��@���@�;d@�n�@�-@���@��-@�O�@��9@�Z@��m@�33@�@���@�n�@�E�@�$�@��T@�@���@�hs@�/@���@��u@��
@���@��
@��@�
=@�v�@��@��@��^@�?}@��j@�Z@���@�;d@�"�@��R@�=q@�J@���@�7L@�Ĝ@�z�@�ƨ@�33@���@�@�@��h@���@�x�@��@�r�@�A�@� �@���@���@�
=@���@�ȴ@���@�^5@��@��T@���@�&�@���@�bN@�A�@� �@��@�|�@�o@��y@���@�5?@��7@�G�@��@��@���@�Z@� �@���@�S�@�
=@��y@���@�V@�-@���@���@���@�hs@�?}@���@���@���@�bN@� �@��@��;@�ƨ@���@�;d@���@��+@�V@�=q@�-@�{@��@���@��-@���@���@�X@��/@��u@� �@��m@��P@�dZ@�\)@�@���@��!@�V@�E�@�$�@�{@�{@��@���@�@���@��7@�`B@�&�@���@��j@���@�r�@�(�@�1@�ƨ@�C�@�
=@��!@�v�@�V@�{@��T@�@���@��h@�p�@�G�@�7L@���@�Ĝ@��j@��@��u@�j@�9X@�  @��@��R@�$�@��T@��^@�x�@�p�@�hs@�`B@�7L@�&�@��@��@��@��@�V@���@��`@���@���@��D@�z�@�r�@�Z@�9X@�(�@���@�\)@�o@���@�v�@�$�@���@��-@��h@�hs@�O�@�&�@�%@���@���@��D@��@�r�@�j@�Z@�9X@��
@��!@�n�@�$�@��T@���@�?}@��/@�Z@��@K�@~��@}�-@}/@{�m@{C�@{o@z��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�\)A�XA�XA�ZA�VA�ffA�dZA�hsA�bNA�ffA�dZA�dZA�ffA�bNA�hsA�bNA�dZA�bNA�hsA�hsA�ffA�jA�hsA�ffA�jA�ffA�t�A�r�A�n�A�p�A�l�A�jA�p�A�l�A�v�A�z�A�t�A�x�A�t�A�r�A�x�A�t�A�x�A�t�A�t�A�v�A�r�A�v�A�v�A�t�A�x�A�v�A�v�A�|�A�t�A�z�A�x�A�x�A܁A�z�A�x�A�t�A�r�A�t�A�l�A�l�A�hsA�n�A�ffA�dZA�dZA�`BA�ffA�dZA�ffA�dZA�`BA�hsA�dZA�dZA�ffA�bNA�hsA�l�A�n�A�t�A�r�A�r�A�x�A�hsA�jA�p�A�r�A�z�A�v�A�z�A�v�A�x�A�z�A�t�A�|�A�x�A�x�A�~�A�x�A�z�A�z�A�v�A�~�A�x�A�z�A�x�A�v�A�|�A�x�A�z�A�r�A�n�A�v�A�x�A�x�A�|�A�~�A�v�A�n�A�jA�dZA�jA�jA�jA�l�A�ffA�jA�jA�ffA�l�A�jA�ffA�n�A�ffA�hsA�l�A�`BA�VA�&�A�&�A�"�A�&�A�$�A�"�A�&�A�$�A� �A�&�A� �A�"�A�&�A� �A�"�A�&�A�$�A� �A�$�A� �A�"�A�"�A��A�"�A��A� �A�$�A� �A��A�$�A��A� �A� �A��A�"�A��A�"�A��A��A�"�A��A��A�"�A��A��A� �A��A��A��A��A� �A��A��A� �A��A��A��A��A��A��A��A��A� �A��A��A� �A��A��A� �A��A��A� �A��A��A��A��A��A� �A��A��A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A��A�{A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A��A��A�{A��A��A�oA��A��A�oA�{A��A�oA�bA�bA�
=A�1A�
=A�A�  A�A���A���A���A��A��TA��HA۸RAۗ�Aۉ7A�|�A�p�A�O�A�A�A�A�A�/A�-A�33A�=qA�-A�
=A���Aں^A�|�A�ZA�A�A�(�A��A�z�A���Aׇ+A�VA�G�A�9XA�1'A��A־wA։7A�9XA�JA���A��A��#AոRAե�AՋDA�z�A�t�A�l�A�\)A�K�A�?}A�1'A��A�VA���A��TAԾwAԬAԁA�$�A��`A��;A���AӶFA�r�A�9XAҾwA�bNA�K�A�1'A�VA��mA���AѼjAѶFAѰ!Aѩ�Aѣ�Aѥ�Aѣ�AѓuAсA�r�A�ZA�?}A�(�A� �A�
=A��A��
AЬA�`BA���AϋDA�jA��A���A�ĜAΟ�A�\)A�bAͧ�A�p�A�E�A�9XA�(�A�VA̰!A�bA˟�A�O�A��A�  A���A��mA��/A��A���A�~�A�AɁA�\)A�A�A�33A�&�A�oA��A���AȺ^Aȴ9AȮAȡ�AȑhA�~�A�n�A�=qA�+A��A�bA�  A���AǗ�A�O�A�oA��A���Aƛ�A�`BA��A��/AžwAŅA�M�A�&�A�
=A��A��mA���AļjAę�A�l�A�G�A�;dA�9XA�+A�"�A��A�VA�VA���A��yA��mA��yA��
A�n�A�oAA�9XA�VA��;A���A�r�A�I�A���A�\)A� �A���A��A��yA��A��jA�C�A��`A��RA���A�l�A�hsA�`BA�A�A� �A��mA��!A�^5A��A��A��TA��^A�O�A��;A���A�C�A�oA�
=A��`A��PA�`BA�9XA�JA�%A��A��A���A��;A���A���A��
A���A���A�ƨA�ȴA�ȴA��A��!A���A���A��\A���A��7A�bNA�O�A�K�A�G�A�C�A�G�A�?}A��A�A��9A�~�A�Q�A��A��A���A�A�A��A�A��^A�r�A�$�A���A���A���A���A�ƨA��wA���A��hA�r�A�K�A�-A��A�VA�%A���A���A���A��A��mA��;A��A���A��jA��-A���A��DA�x�A�^5A�C�A��A��yA���A���A���A�%A���A��-A�{A��^A�~�A�E�A�+A��A��A��A�{A��A�ƨA��A�ZA�1A���A���A�M�A�VA��^A��A�XA�-A��yA�p�A�  A���A��A�hsA�(�A�ĜA��\A�-A��A�
=A�1A�
=A�A���A��TA���A�O�A�A��A�S�A���A��DA��yA���A�VA�$�A�1A��A��`A���A��^A���A��PA�r�A���A���A��DA��HA�M�A���A���A���A��A�n�A�ZA�I�A�;dA�$�A�{A�JA�  A��HA��^A���A���A�/A��A�~�A��A���A���A�ffA���A��#A���A���A��wA��hA�A��A���A�XA�bA��A�(�A��A�n�A�1'A�"�A�1A��;A��FA��A�bNA�XA�K�A�1'A�l�A�"�A���A�\)A�-A�JA��TA��\A�I�A�{A��A�t�A�A�A�;dA�5?A�+A�&�A�JA��
A��PA�E�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                    111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�B B B�B4B�B B.B�B�B�B�B�B�B�B\B�B�B�B�BbBbB(B(BPB~BJBJBBBBJB�BB�BDBDBxB�B�BVB.B�BB:�B`B� B'RBQNBb�Bo�Bv�B}�B�B�FB��B��B�EBںB�B��B�yB�vB��B��B� B�B�tB�hB��B�-B�FB��B�CB�eB��B�@B� B��BzxBs�B�;B�oB�4B~(By>Bh
BbBS�BE�B5B%B=B�B�B�B��B�-B�bB�ABh
BQ�BN�B1'BVB4B
�B
�5B
��B
��B
��B
�UB
�VB
u�B
m]B
]�B
G�B
9XB
.}B
B
�B
+B
 �B	�pB	��B	��B	��B	�\B	�lB	�4B	q�B	k�B	gmB	bB	TaB	MB	GEB	;�B	1�B	'RB	 \B	�B	 B	�B��B�8B�cB��B��B��B�WB��B�B��B��B��B�B�aB�<B�qB��B��B�qB��B��B��B�RB��B��B��B��B�nB�zB��B��B��B��B�B��B�nB��B��B�*B�kB�0B��B�nB��B��BBɆB�#B��B�B�BΥBΥB�jBбB��B�}BуB֡B�EB�B�KB��BچB�]B��B�/B�dBޞB�vB�5B�B҉B�6B�pB�jB��B��BѷB��B��B�2B�B��B� B�;B�B�B�B�B�(B�PB��B	uB	�B	"B	�B	�B	�B	�B	B	�B	B	B	 \B	%�B	(�B	*0B	+6B	-�B	/B	.�B	0!B	3hB	33B	2�B	2�B	1�B	4B	6zB	6�B	9XB	<6B	9$B	:�B	:^B	;dB	9XB	9�B	:^B	:�B	>�B	@OB	@�B	A B	@�B	C�B	J�B	I�B	H�B	H�B	JXB	L�B	QNB	WsB	[WB	Z�B	ZQB	[�B	^B	^�B	c�B	e�B	h�B	k�B	qvB	rGB	r�B	y	B	z�B	}�B	~(B	�iB	�B	�MB	��B	��B	�=B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	��B	�qB	�IB	��B	��B	�OB	��B	��B	�UB	��B	�LB	��B	�B	�$B	�*B	��B	�$B	��B	��B	��B	��B	��B	�B	�B	��B	�<B	��B	��B	��B	��B	�6B	�jB	��B	�}B	��B	��B	��B	�[B	��B	�tB	ȀB	�0B	��B	�pB	�B	��B	�BB	�B	�&B	ӏB	��B	�yB	خB	�yB	��B	�B	چB	�]B	��B	�]B	ܒB	��B	�dB	ޞB	�B	�B	�B	�B	�B	�B	�B	�>B	�KB	�B	�B	�B	�WB	�B	� B	�B	�oB	�B	��B	�B	�|B	��B	�B	�B	�B	�%B	��B	��B	�+B	�+B	�%B	��B	��B	��B	��B	�2B	�fB	��B	�	B	�rB	��B	�B	��B	��B	�B	�PB	�B	�"B	�]B	��B	��B	��B	��B
 iB
  B	��B	��B
 4B
;B
AB
AB
AB
AB
B
uB
uB
uB
�B
uB
�B
{B
�B
B
SB
SB
YB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
fB
fB
�B
�B
	B
	�B

�B
B
xB
�B
JB
B
�B
�B
�B
�B
�B
�B
�B
�B
�B
\B
�B
�B
.B
�B
B
hB
�B
�B
B
�B
�B
�B
B
�B
YB
YB
�B
_B
+B
+B
+B
�B
�B
�B
�B
�B
�B
�B
�B
1B
eB
�B
�B
�B
�B
B
B
�B
�B
=B
qB
�B
B
CB
�B
�B
B
IB
IB
~B
~B
�B
B
B
�B
�B
�B
�B
�B
�B
 �B
 \B
 �B
!-B
!�B
"hB
#nB
$tB
%B
%�B
&LB
&�B
&B
'RB
'B
'�B
'�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BbBB�BhB:B\B�B�BoB�B4BhB�B:B�BB�B�B4B�BB4B�B4BbB B�BB�B�B B�B�BhBVBbB�B�B BhB�BB�BhBhB(B�BbB�BhBbB�BB�B�B�B�BhB�B�B.B B4BbBB�B�B�B4B�B�B4B�B�B�B\B.B�B�B�B(B�B�BhBhB�B�B B(BB(B�B:B�B�B�B4B�B�B4B\B4B B�BB�B�B�B�BoB�B�B B�B B.B4B(B�B�BB.B�B�BbB(BhB�B�B�BVB�B�B�B.BVB�B.B�B�B\B~B�B�B�BB�B�BJB�BxBBBxBPB�BDBBB�BJB�B�BPB�B�BBDBB~B
�B�B�BDBPBJB�BPBB�BBJBBB�B~B
�BB�BxBB�BDBPBDBJB�BDB�B�BxB�BBBJBJB
�BB�BDB�BBBBPB
�BB�B�B~BJB
rB�B~B
�B~BB
rB~B
=B�BDB
	BB
�B�BxB
	BJB
=B�B~B
	B�BxB
rB�B
�B
�B~B
rB~BJB�BPB~B�B�B�B�B"B�B�B(B�BVB(BPB�B�BPB\B"BPB.B\B�B4B�B�B4B�B(B�B�B B�BuB:BFB�BFBBMB�B�BCB�B'�B1[B0�B4nB7�B=qB=B=<BEBGEBI�BK)BYB`�Bk�Bg�Bt�Bx8B{JB�B��B�FB��B�B{B+B	B	B�B&LB+B?}BB�BCaBD3BIBOBBQBV9BXyBX�BY�B]/B_pB`vB`Bd&Bc�Bd�BiDBm]Bk�Bt�B|�Bm�Bj�Bj�Bn�BwfBpB~�BtTBsBu�B{�B|�B|�BcB~�B}�B}�B~�B~(B}VB��B�uB��B��B�%B��B�SB��B�7B��B��B��B��B��B��B�3B��B��B��B�B��B�LB��B��B��B�aB�UB��B�B�mB�0B��B�B�?B��B��B��B�KB��B��B�BߤB�jB�B�B�|B�BB��B��B�B�NB�B�TB�&B�B��B��B��B�,B� B�B�cB�B�oB��B��B�5B��B�AB�|B��B��B�PB�2B�TB�B��B��B�B� B�B�B�B��B�,B�B�B�ZB�HB��B�B�vB�)B��B�iB�B��B�HB��B�KBʌB��B��B�B��B��B�B��B�FB�@B�aB�B�FB��B�\B�xB�eB�B��B��B�@B�\B��B��B��B��B�zB�nB��B��B��B�!B�B�LB��B��B��B�B�7B��B��B�:B�@B�$B��B��B�+B�_B��B��B��B��B��B��B�B�B�(B��B�YB��B�bB��B�PB�=B�PB��B��B��B�B�YBcB{JB��B}�Br�Bt�B�4BrB}"Bs�BpBt�BrBtTBu�B~�B~]B|PB�%B�oB��B��B�oB�B��B��B�iB��B�AB�B.B��B~�B}VB�B}�B.B}�BzB~�Bu�Br�Bs�B��Br�BrB��Bq�Bl�BjKBcTBa�BaHB^5B_Bd�Bb�B`BBg�B`BY�BZBS[BU�BQ�BG�BH�BFtBH�BP}B@OB?B<B>�B8�B@�B4nB:�B)�B($B&�B$B%B$tB$B+B$tBqB&�BB�B$tB'�B�BPBB �B��B��B��B��B�fB�WB�B�B�B�B�"B��B�WB˒BŢBŢBÖB��B�'B�B��B��B�B�RB��B��B��B�nB��B�IB�B��B�CB��B�YB��B��B�_B�_B�uB�rB�GB{B�B|Br�BqvB��BjBbB]dBW
BV�B[�BXBOBBOvBJ�BI�BJ#BffBaHBA BMjB7B0�B4B6FB/OB,�B,�B1[B$@BB1B�B�B�B�BbB�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                    444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444B�BPBPBB�BBPB~BBB�B�BB�BB�BIB�BB�B�B�BxBxB	�B�B�B�BeBeBeB�B1BeB�B�B�B�B	B
=B
�B~B'BRB7B\]B�pB#�BM�B^�Bl"Br�BzBbB��B�$B�BÕB�
B��B�B��B��B�5B�>B�pB�QB��B��B�B�}B��B�-B��B��B�B��B�PB��Bv�Bp;B}�B}�B|�BzxBu�BdZB^iBPHBA�B1[B!bB�B��B� B�mB�B�}B��B~�BdZBM�BJ�B-wB�B�B
�SB
�B
�B
�B
�KB
��B
��B
rGB
i�B
ZB
C�B
5�B
*�B
bB
�B
{B	��B	��B	�)B	��B	��B	��B	��B	|�B	n.B	h>B	c�B	^iB	P�B	IQB	C�B	7�B	.B	#�B	�B	�B	PB	B�B�B�B�,B�;B�/BקB�BB�jB�/B�KB�9B�[B��B��B��B�'B�<B��B�6B�B��B��B��B��B�3B�!B��B��B��B�B�3B�@B�nB��B��B�B�$B�zB��B��B�*B��B��B��B��B��B�sB�KB�QB�WB��B��BɺB�B�)B��B��B��BԕB��B՛B�>B��BحB�8B�BٴB��B��BڅB�`B��BɆB��BɺB�EB�B�B�2B�)B�B��B�JB�PB�B��B��B� B��B�xB��B�B��B	�B	
rB	�B	B	B	!B	VB	�B	RB	eB	�B	!�B	%B	&�B	'�B	)�B	+kB	+B	,qB	/�B	/�B	/B	.�B	.B	0UB	2�B	33B	5�B	8�B	5tB	6�B	6�B	7�B	5�B	6EB	6�B	6�B	;0B	<�B	<�B	=pB	=B	@B	GB	F?B	EB	D�B	F�B	IB	M�B	S�B	W�B	W
B	V�B	W�B	ZQB	Z�B	`B	a�B	d�B	g�B	m�B	n�B	o5B	uYB	v�B	zB	zxB	|�B	~\B	��B	�B	�B	��B	�=B	�B	��B	�.B	�hB	�FB	�B	��B	��B	�UB	��B	��B	��B	��B	��B	��B	�0B	�B	��B	�B	��B	�9B	�mB	�tB	�zB	�B	�tB	�EB	��B	�B	�B	�)B	�^B	�^B	��B	��B	�KB	�KB	��B	�B	��B	��B	�)B	��B	�<B	��B	�B	��B	�B	��B	��B	ȀB	�#B	��B	�^B	�)B	˒B	�dB	�vB	��B	�B	��B	��B	��B	�8B	�mB	��B	حB	�KB	حB	��B	�EB	ٴB	��B	��B	�B	�B	��B	��B	��B	��B	�B	�B	��B	�lB	��B	�B	��B	�PB	��B	�B	��B	�(B	��B	��B	�5B	��B	�B	�B	�uB	�B	�GB	�{B	�{B	�uB	��B	�GB	�GB	�B	�B	�B	��B	�YB	��B	��B	�`B	�1B	��B	��B	��B	�lB	�rB	��B	�JB	�JB	��B	�B	��B	�PB	�B	�B	��B	��B	��B	��B	��B	��B	�\B	��B	��B	��B	��B	��B	��B	��B
  B
 iB
�B
�B
�B
@B
�B
B
�B
B
�B
�B
MB
MB
MB
�B
�B
�B
�B
B
SB
�B
�B
_B
�B
1B
�B
eB
	7B

	B
	�B

=B

=B

=B
CB
B
CB
�B
IB
�B
~B
�B
VB
�B
!B
!B
VB
'B
'B
�B
bB
�B
�B
�B
B
�B
{B
{B
{B
B
B
B
B
B
B
B
LB
�B
�B
�B
�B
B
�B
RB
RB
B
�B
�B
�B
�B
_B
�B
0B
0B
eB
�B
�B
�B
�B
7B
kB
kB
7B
7B
7B
B
B
7B
B
�B
B
}B
�B
�B
�B
 �B
!bB
"3B
"�B
"�B
"hB
#�B
#nB
#�B
$G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�BVB�B�B�B�B'B�B�BIB�B�B�B�BIBVB�BIB�B
�B\B�BIB�B�BPBBVBB�BPB'B�B�B
�B�B!BIBPB�BCBVBIB�B�BxB!B�B�B�B�BBVB�B�BBB�B
�B!B~BPB�B�B\BBIBCB�BB�B�B
�BCB�B�B~B	�BCBIBxBB
	B�B�BCB�BPBxBhBxB
=B�BB!B�B�BIBB�B�B�BPBBVB�BIB�B�B�BIBBPBCBPB~B�BxB�B
	BVB~B�B!B�BxB�BCBBIB
�BIB
�BB~B
�BCB~B
=BIB�B�B�B4B.BeB
	B�B�B
	B�BeB	kB�B	�B	B�B	kB	kB�B�B
	B�B	�B1B�B	kB�B	kB�B+B	B	7B�B	�B�B�B	�B_B	7B_B�B	kB_B	7B�B+B	kB1B�B	kB�B�B	�B�B�B	7B�B	7B	7B�B1B	kB_B�B�B+B	kB�B�B	7BeB_BeB	�B�B	kB1B�B�B�B�B	7B�B�B�BeB�B�B�B	7B�BYBeB�B�B�BYB�B�B1B�BYB1B�B�B	B+B�B�B�B�B�B1B	�B�B�BCB	B	�B
rB	7B
	BxB	B
�BxB	�BB
=B	�B�B
rB	�B~B�BIB�BIBIB�BBxB�B�BPB'B�B�B�B�B�BnB�B�B�B�B$B$B-�B-B0�B4B9�B9XB9�BAUBC�BF
BGyBUgB\�Bg�Bc�BqBt�Bw�B|B�B��B�B
	B��B{BSBSB
�B"�B'RB;�B?HB?�B@�BEmBK�BMjBR�BT�BU2BVBYB[�B\�B\]B`vB`BaGBe�Bi�Bh>BqBy	BjBg8Bg8Bj�Bs�BlWB{JBp�BoiBq�BxBy>By>B{�Bz�BzBy�B{BzxBy�B|�B~�B~(B�:B�uB��B��B�B��B��B�B�.B�B�B�B��B�3B��B�B�dB��B��B�B��B��B��B��B�NB�WB��BȀB�B�mBB�&B�&B�&BěB�B�;B�cB��BںB�QB�]B��BܒB�GB�AB�BޞB�BߤB�vB��B�8B�B�GB�|B�pB��B�B��B�B�JB�B�B��B�B��B�.B�B��B�B�B�cB�B�B�WB�PB��B�fB��B�NB�|B�B��B�BݘB�/B��B��B�yB�)B�B��B�EB̘B�BěB��B�HB�B�^B�B�'B�RB��B��B��B��B�bB��B��B��B��B��B�RB�=B��B��B��B�B�B�=B�7B��B��B�OB�B�@B�qB�_B��B��B�'B�*B�UB��B�B��B��B��B�tB�$B�B�{B��B�B�B�FB�B��B�B�bB�hB�xB��B��B��B��B�	B��B��B��B��B�IB�@B~\B��B{�Bw�B�BzDBo5BqAB|�BncByrBpBlWBp�BncBp�BrGBz�Bz�Bx�B�uB}�B� B}�B}�B~\B.B}"B|�B~(B~�B{�B{~B|�B{By�B}VBzB{~By�Bv`B{JBq�Bo5BpB�Bo5BncB��Bm�BiBf�B_�B^5B]�BZ�B[WB`�B_;B\�Bc�B\]BVBVmBO�BR BN<BD3BD�BB�BEBL�B<�B;dB8RB;0B5?B=B0�B7KB%�B$tB#9B [B!bB �B [B'RB �B�B#9BeBB �B$@B1B	�B�VB��B��B�MB��B��B�B�B�WB��B�fB��B�rB�KBקB��B��B��B��B�HB�wB�jB�B�B�mB��B��B��B��B��B�B��B�RB��B��B�4B��B�.B�MB��B��B~�B��B�Bw�BbBxlBo5Bm�B��Bf�B^iBY�BSZBS&BXEBT`BK�BK�BGEBE�BFsBb�B]�B=pBI�B3gB,�B0UB2�B+�B)*B(�B-�B �B_B�BBB@BLB�B44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                    444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0036000                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 03 05 2020 132 -0.0036000 0.0006 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230727090048                            20230727090048AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023072709004820230727090048  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072709004820230727090048QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1B83E           383E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023072709004820230727090048QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               