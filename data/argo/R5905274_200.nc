CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-06-30T09:00:58Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230630090058  20230630090058  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�4SH?��@�4SH?��11  @�4Sww� @�4Sww� @0��kP��@0��kP���d!�`�V�d!�`�V11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AA  AA  ?u@   @@  @}p�@��\@�G�@޸R@�p�A�RA\)A,(�A@  A_\)A~�RA��A�  A�  A�  AϮA�  A�  A�\)B�
B(�B  B�
B(Q�B0Q�B8  B?�
BG�BO�BW�
B`(�Bh  Bo�
Bx  B�  B��B�  B�  B��B�  B��B��
B�  B�(�B�{B��B��B��B�  B�{B�{B�{B�(�B�=qB�{B�(�B�  B��B�{B�{B�{B�(�B�{B�{B�{B�  C 
=C{C��C��C  C
  C
=C{C�C�C�C�C  C  C��C��C�C!��C$  C%�C'��C)��C,  C.  C/��C2  C4  C5��C8  C9��C<  C>  C@  CB  CD  CF
=CH  CJ
=CL
=CN  CP
=CR
=CT{CV{CX
=CZ
=C[��C]��C`
=Cb{Cd
=Ce��Ch  Cj  Cl  Cm��Co��Cq��Ct
=Cv
=Cx  Cy��C|  C~  C�C�  C�  C�C���C���C���C���C���C�  C�C�C�  C�  C���C�  C�  C���C�  C�  C���C�  C�  C�  C�  C�  C���C���C�  C�  C�  C�C�C�C�C�  C�  C�  C�C�
=C�C�  C���C���C�  C�  C�  C���C��C���C�  C�  C���C���C�  C�  C�C�
=C�C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C�  C�  C�C�  C�\C�C�C���C�C�C�  C���C���C���C���C���C�
=C�C�C�
=C�C�  C���C���C�C�  C���C���C�  C�
=C�  C�C�C�C�C�  C�C�C�C���C���C�C�  C�  C���C�  C�  C���C�  C�  C���C���C���C���C���C���C�  C���D � D�D}qD�qD� D�D��D  D� D  D�D  D��DD� D  D�D	  D	}qD
  D
��DD� D��D}qD  D��D�D��D  D}qD�qD�DD� D  D}qD  Dz�D�RD}qD�qD��D�D�D�D� D  D}qD�qD��D�D� D�D��D�D�D  Dz�D�D��D�D}qD��D ��D!�D!�D"D"� D#  D#��D$  D$}qD%  D%z�D&  D&� D'  D'��D(�D(z�D(�qD)��D*�D*}qD*�qD+� D,  D,��D,��D-}qD.�D.� D.�qD/� D0�D0�D1�D1� D1��D2}qD3�D3}qD3�RD4� D5  D5}qD5�qD6��D6�qD7}qD8�D8}qD8�RD9}qD:  D:� D;  D;��D;��D<z�D=�D=� D=�qD>}qD?  D?��D@�D@� DA�DA� DB  DB}qDB��DC� DDDD��DD��DE}qDFDF�DG  DG}qDH�DH� DH�qDI��DJ�DJ� DK  DK��DK�qDLz�DL�qDM}qDM�qDNz�DN��DOz�DO�qDP��DQDQ��DR  DR� DS  DS��DT�DT}qDT�RDU� DV�DV� DV�qDWz�DX  DX��DYDY� DZ  DZ� D[  D[� D[��D\� D]�D]z�D^  D^��D_  D_z�D_��D`}qDa  Da}qDa�qDb� Db�qDc}qDc�qDd}qDeDe��De�qDfz�Dg  Dg��Dh�Dh}qDi�Di�DjDj� Dj�qDk� Dl�Dl�Dm  Dm� DnDn� Dn��Do}qDo�qDp� Dq  Dq}qDr  Dr}qDs�Ds}qDs��Dt}qDu  Duz�Du�qDv��DwDw�Dw��Dxz�Dy  Dy}qDy�qDz�Dz�qD{z�D{�qD|}qD}  D}�D}��D~}qD�D� D�qD�=qD�~�D�� D��)D�AHD�� D�D�HD�=qD�� D�D���D�@ D�~�D�� D�HD�=qD�� D���D�HD�AHD�}qD��HD��D�B�D���D��HD�HD�AHD��HD�D�HD�AHD�~�D�� D�HD�B�D���D�� D���D�@ D��HD���D���D�>�D�~�D�D�HD�<)D�~�D��qD���D�>�D��HD�� D�HD�AHD���D�D�  D�@ D��HD�� D��)D�=qD�� D��HD��D�AHD�� D�D��D�AHD���D�D�HD�@ D�� D�� D��D�AHD���D�D�  D�AHD���D��HD�HD�AHD��HD���D��qD�=qD�~�D��qD�  D�>�D��HD��HD�HD�C�D��HD�� D�HD�@ D�~�D�� D�  D�@ D�}qD���D�HD�AHD��HD�� D�  D�AHD�� D���D���D�@ D���D��HD���D�AHD�� D��HD�HD�AHD���D��qD���D�AHD�� D��HD�  D�>�D�~�D��HD��qD�AHD�|)D���D���D�>�D��HD���D��qD�>�D��HD�� D�  D�>�D���D��HD���D�AHD�� D�� D�  D�>�D�� D��HD�  D�>�D�~�D���D���D�>�D�~�D�D�  D�>�D�}qD���D�  D�=qD�� D��HD�  D�>�D��HD�� D�  D�AHD�� D���D�  D�B�D�~�D�� D���D�AHD�� D��HD��D�@ D�� D�� D�HD�B�D��HD���D���D�AHD���D�� D���D�>�D�� D���D�  D�@ D�� D��HD�HD�AHD�~�D�� D�  D�C�D�� D�� D�  D�>�D�� D�� D���D�>�D���D��HD�HD�AHD�� D�� D�  D�>�D�� D�� D�HD�=qD�~�D���D�  D�@ D��HD�D�  D�@ D�~�D��HD�  D�AHD�� D��HD�  D�@ D��HD�� D��D�>�D�~�D��HD��D�AHD�~�D½qD��qD�>�D�}qDýqD���D�@ DĂ�D�D�  D�=qD�~�D�� D���D�>�D�~�Dƾ�D�HD�>�Dǀ D�� D���D�AHDȀ D��HD�HD�AHDɂ�D�� D�HD�@ Dʀ D�� D���D�@ Dˀ D˾�D�  D�>�D́HD�� D�HD�@ D́HD�� D���D�>�D΁HDξ�D��D�@ Dπ D�� D���D�@ DЀ D�� D���D�@ Dр D�� D���D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�>�D�~�D��HD�HD�B�DՁHD�� D�HD�@ Dր DֽqD�HD�>�DׁHD��HD�  D�@ D؂�D��HD�HD�@ Dـ D��HD�HD�AHDڀ Dھ�D���D�>�D�~�D��HD��D�>�D�}qDܾ�D�HD�AHD݀ D�� D���D�AHDށHD��HD���D�AHD�~�D߾�D�  D�@ D�~�DྸD�  D�AHD�HD��HD�HD�>�D�~�D⾸D�  D�>�D�}qD㾸D�  D�@ D�HD��HD�HD�AHD�HD�D���D�@ D� D澸D���D�@ D�~�D羸D�  D�@ D肏D�� D�  D�>�D�~�D龸D���D�@ D� D�� D�  D�@ D� D뾸D�  D�>�D� D�� D�HD�@ D�HD�� D���D�>�D�}qDD�  D�AHD� D��HD�HD�@ D�~�D�� D�  D�>�D� D��HD�  D�>�D� D�� D���D�@ D�HD��HD�  D�@ D� D��qD���D�>�D��HD��qD���D�@ D�~�D�� D�  D�@ D��HD�� D�HD�@ D�� D�� D��qD�<)D�~�D���D�HD�@ D��HD���?\)?aG�?��?���?\?�(�?�@\)@�R@0��@=p�@Q�@aG�@p��@��\@��@�@�p�@�ff@�{@�
=@��R@Ǯ@�\)@�Q�@�  @���@��@���A ��A�AQ�A�A\)A�
AQ�A�HA\)A"�\A&ffA*=qA.{A1�A5A9��A>{AC33AFffAJ=qAN{AQ�AUAZ=qA\��A`  Adz�AhQ�Al(�Ap  Au�Az�HA~�RA�G�A��A�A�\)A�G�A��HA��A�\)A�G�A�33A�p�A�Q�A��\A�z�A��RA���A��HA�p�A�\)A�G�A�33A��A�\)A�G�A��\A���A�
=A���A�33A�p�A�\)A���AÅA�Aȣ�A��HA��A�
=A�G�A�33A��AָRA���A�33A�p�A߮A��A�A�RA��A�A�A�  A��A�(�A��RA���A��\A�(�A�ffB Q�Bp�BffB  B�B=qB�B��B	p�B
ffB�B��BB�HB�
BG�B�RB�B��B{B
=B  B�B{B
=Bz�BB
=B (�B!G�B"ffB#33B$Q�B%G�B&�\B'�B(��B)B*�HB,  B-G�B.ffB/�
B0��B1�B333B4z�B5��B6�RB7�
B9�B9B:�RB<  B<��B>{B?33B@z�BABC
=BD(�BEp�BF�\BG�BH��BIBK
=BL  BMp�BN=qBO�BP��BQBS
=BT(�BU�BU�BW
=BXQ�BYG�BZffB[�B]�B^ffB_\)B`��Ba��Bb�\Bc�Bd��BeBf�HBh(�Bip�Bj�RBk�
Bl��Bn{Bo\)BpQ�Bq��Br�RBt  Bu�Bv{Bv�HBx(�ByG�BzffB{�
B}G�B~ffB�B�ffB���B���B�(�B��RB��B��B�=qB��HB�\)B�{B���B�\)B��B�z�B��B��B�{B���B�33B��B�ffB��B��B�=qB��RB��B��B�(�B��HB��B�{B��\B�33B��B�{B��\B�
=B�B�Q�B��HB�p�B��B�=qB��RB�p�B�{B��\B��B��B�{B��\B��B���B�=qB��HB�p�B��B��\B�
=B�\)B��B�z�B��B�B�ffB���B��B��
B�ffB��HB���B�Q�B���B�p�B��B�ffB��HB�p�B�(�B��RB�G�B��B�(�B���B�G�B�B��\B��B��B�(�B��\B�33B���B�Q�B���B��B�(�B���B���B��B�{B���B�\)B��B�ffB���B�G�B��
B��\B��B�B�{B��\B�
=B�B�z�B��B�p�B��B\B��B��B�z�B���B�p�B�  BƏ\B�G�B��Bȣ�B�
=BɅB�  B���B�p�B�{B�z�B���BͅB�Q�B��HB�p�B��
B�ffB�33B��
B�z�B���B�p�B�{B��HBՅB��B�z�B�33B��B؏\B�
=Bٙ�B�=qB���Bۙ�B�  Bܣ�B�33B�  B޸RB�G�B߮B�Q�B�
=B�B�z�B��HB�p�B�(�B���B�B�  B��B�p�B�(�B���B�G�B��
B�RB�\)B��B�ffB�
=B��B�z�B��B�B�(�B�RB�B�Q�B���B�B�=qB�
=B�B�=qB���B��
B��\B�
=B�B��\B�p�B�(�B���B�\)B�Q�B���B��C {C p�C �HC=qCz�C��CQ�C�RC�CG�C�RC�Cz�CC�C��C  C\)C��C
=Cz�C��C�C�\C	  C	=qC	��C

=C
\)C
��C�Cz�C�C  Cp�C�C�HC(�Cz�C�RC��C  CQ�Cz�C�C��C�HC{C{CG�Cz�C��C�C��C{C33CG�C�C�C�
C  C  C(�Cp�C�\C�C��C��C33CG�CffC��C��C  C  C(�Cp�C��C��CC  C33CQ�Cp�C�\C�
C  C
=C33Cp�C�\C�C�
C
=C(�CG�Cp�C�C�
C�HC{CG�CQ�Cz�CC�C�C{C\)C�C�\C�C��C(�C33CffC��C��C��C{C=qCG�Cz�C�CC�C�CQ�C\)C�CC��C  C�CffC�\C��C�
C
=C{CG�C�C��CC  C(�C=qC\)C�C�
C�HC(�C\)C\)C�\C�
C�HC 
=C Q�C ffC �\C �
C!
=C!{C!33C!�C!��C!C"
=C"=qC"G�C"p�C"�C"�C"��C#�C#ffC#��C#��C#��C${C$33C$\)C$��C$��C$�
C%{C%Q�C%ffC%�C%�
C&  C&{C&G�C&�C&��C&C'  C'(�C'G�C'p�C'�RC'�C'�C(�C(ffC(�\C(��C(��C){C)(�C)G�C)�\C)�RC)�
C*{C*G�C*Q�C*�C*��C*�
C+  C+G�C+ffC+�C+��C+��C,
=C,G�C,z�C,�C,�C,��C-�C-33C-p�C-��C-�C-�
C.�C.33C.Q�C.�\C.�RC.��C/
=C/33C/=qC/z�C/��C/C0  C0(�C0(�C0G�C0�\C0��C0C1
=C1{C133C1p�C1�C1��C1�C2  C2{C2Q�C2�C2�C2�C2��C2��C333C3ffC3ffC3��C3�
C3�
C4  C4=qC4Q�C4ffC4�C4��C4�HC5{C5=qC5Q�C5�C5�C5�C5��C6
=C6�C633C6p�C6�C6��C6�HC6�C7
=C7G�C7ffC7z�C7�RC7�
C7�HC8{C8G�C8=qC8p�C8��C8�C8��C9
=C9�C933C9p�C9�\C9��C9�HC9��C:  C:=qC:\)C:p�C:�C:C:�
C;{C;(�C;=qC;z�C;��C;�C;�C<  C<{C<Q�C<z�C<�C<�RC<�C<��C=
=C=Q�C=p�C=z�C=C=�HC=�C>33C>G�C>\)C>��C>�RC>�HC?�C?(�C?G�C?�\C?��C?��C@
=C@{C@G�C@z�C@�C@��C@�CA  CA=qCAffCAz�CA�CA�HCA��CB(�CBQ�CBffCB�CB��CB�HCC(�CC=qCCffCC�CCCC�HCD(�CD=qCD\)CD��CD�RCD�CE(�CE=qCEz�CE��CE�RCF  CF{CF33CF�CF��CF�
CF�CG�CGffCGp�CG�CG�CH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111411111111111141111111111111111111111111111111411111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111411111111111111141141141141111111111114111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                ?u@   @@  @}p�@��\@�G�@޸R@�p�A�RA\)A,(�A@  A_\)A~�RA��A�  A�  A�  AϮA�  A�  A�\)B�
B(�B  B�
B(Q�B0Q�B8  B?�
BG�BO�BW�
B`(�Bh  Bo�
Bx  B�  B��B�  B�  B��B�  B��B��
B�  B�(�B�{B��B��B��B�  B�{B�{B�{B�(�B�=qB�{B�(�B�  B��B�{B�{B�{B�(�B�{B�{B�{B�  C 
=C{C��C��C  C
  C
=C{C�C�C�C�C  C  C��C��C�C!��C$  C%�C'��C)��C,  C.  C/��C2  C4  C5��C8  C9��C<  C>  C@  CB  CD  CF
=CH  CJ
=CL
=CN  CP
=CR
=CT{CV{CX
=CZ
=C[��C]��C`
=Cb{Cd
=Ce��Ch  Cj  Cl  Cm��Co��Cq��Ct
=Cv
=Cx  Cy��C|  C~  C�C�  C�  C�C���C���C���C���C���C�  C�C�C�  C�  C���C�  C�  C���C�  C�  C���C�  C�  C�  C�  C�  C���C���C�  C�  C�  C�C�C�C�C�  C�  C�  C�C�
=C�C�  C���C���C�  C�  C�  C���C��C���C�  C�  C���C���C�  C�  C�C�
=C�C�C�  C�  C�  C�  C�  C�  C�  C�  C�  C�  C�C�  C�  C�C�  C�\C�C�C���C�C�C�  C���C���C���C���C���C�
=C�C�C�
=C�C�  C���C���C�C�  C���C���C�  C�
=C�  C�C�C�C�C�  C�C�C�C���C���C�C�  C�  C���C�  C�  C���C�  C�  C���C���C���C���C���C���C�  C���D � D�D}qD�qD� D�D��D  D� D  D�D  D��DD� D  D�D	  D	}qD
  D
��DD� D��D}qD  D��D�D��D  D}qD�qD�DD� D  D}qD  Dz�D�RD}qD�qD��D�D�D�D� D  D}qD�qD��D�D� D�D��D�D�D  Dz�D�D��D�D}qD��D ��D!�D!�D"D"� D#  D#��D$  D$}qD%  D%z�D&  D&� D'  D'��D(�D(z�D(�qD)��D*�D*}qD*�qD+� D,  D,��D,��D-}qD.�D.� D.�qD/� D0�D0�D1�D1� D1��D2}qD3�D3}qD3�RD4� D5  D5}qD5�qD6��D6�qD7}qD8�D8}qD8�RD9}qD:  D:� D;  D;��D;��D<z�D=�D=� D=�qD>}qD?  D?��D@�D@� DA�DA� DB  DB}qDB��DC� DDDD��DD��DE}qDFDF�DG  DG}qDH�DH� DH�qDI��DJ�DJ� DK  DK��DK�qDLz�DL�qDM}qDM�qDNz�DN��DOz�DO�qDP��DQDQ��DR  DR� DS  DS��DT�DT}qDT�RDU� DV�DV� DV�qDWz�DX  DX��DYDY� DZ  DZ� D[  D[� D[��D\� D]�D]z�D^  D^��D_  D_z�D_��D`}qDa  Da}qDa�qDb� Db�qDc}qDc�qDd}qDeDe��De�qDfz�Dg  Dg��Dh�Dh}qDi�Di�DjDj� Dj�qDk� Dl�Dl�Dm  Dm� DnDn� Dn��Do}qDo�qDp� Dq  Dq}qDr  Dr}qDs�Ds}qDs��Dt}qDu  Duz�Du�qDv��DwDw�Dw��Dxz�Dy  Dy}qDy�qDz�Dz�qD{z�D{�qD|}qD}  D}�D}��D~}qD�D� D�qD�=qD�~�D�� D��)D�AHD�� D�D�HD�=qD�� D�D���D�@ D�~�D�� D�HD�=qD�� D���D�HD�AHD�}qD��HD��D�B�D���D��HD�HD�AHD��HD�D�HD�AHD�~�D�� D�HD�B�D���D�� D���D�@ D��HD���D���D�>�D�~�D�D�HD�<)D�~�D��qD���D�>�D��HD�� D�HD�AHD���D�D�  D�@ D��HD�� D��)D�=qD�� D��HD��D�AHD�� D�D��D�AHD���D�D�HD�@ D�� D�� D��D�AHD���D�D�  D�AHD���D��HD�HD�AHD��HD���D��qD�=qD�~�D��qD�  D�>�D��HD��HD�HD�C�D��HD�� D�HD�@ D�~�D�� D�  D�@ D�}qD���D�HD�AHD��HD�� D�  D�AHD�� D���D���D�@ D���D��HD���D�AHD�� D��HD�HD�AHD���D��qD���D�AHD�� D��HD�  D�>�D�~�D��HD��qD�AHD�|)D���D���D�>�D��HD���D��qD�>�D��HD�� D�  D�>�D���D��HD���D�AHD�� D�� D�  D�>�D�� D��HD�  D�>�D�~�D���D���D�>�D�~�D�D�  D�>�D�}qD���D�  D�=qD�� D��HD�  D�>�D��HD�� D�  D�AHD�� D���D�  D�B�D�~�D�� D���D�AHD�� D��HD��D�@ D�� D�� D�HD�B�D��HD���D���D�AHD���D�� D���D�>�D�� D���D�  D�@ D�� D��HD�HD�AHD�~�D�� D�  D�C�D�� D�� D�  D�>�D�� D�� D���D�>�D���D��HD�HD�AHD�� D�� D�  D�>�D�� D�� D�HD�=qD�~�D���D�  D�@ D��HD�D�  D�@ D�~�D��HD�  D�AHD�� D��HD�  D�@ D��HD�� D��D�>�D�~�D��HD��D�AHD�~�D½qD��qD�>�D�}qDýqD���D�@ DĂ�D�D�  D�=qD�~�D�� D���D�>�D�~�Dƾ�D�HD�>�Dǀ D�� D���D�AHDȀ D��HD�HD�AHDɂ�D�� D�HD�@ Dʀ D�� D���D�@ Dˀ D˾�D�  D�>�D́HD�� D�HD�@ D́HD�� D���D�>�D΁HDξ�D��D�@ Dπ D�� D���D�@ DЀ D�� D���D�@ Dр D�� D���D�@ DҀ D�� D�  D�@ DӀ D�� D�  D�>�D�~�D��HD�HD�B�DՁHD�� D�HD�@ Dր DֽqD�HD�>�DׁHD��HD�  D�@ D؂�D��HD�HD�@ Dـ D��HD�HD�AHDڀ Dھ�D���D�>�D�~�D��HD��D�>�D�}qDܾ�D�HD�AHD݀ D�� D���D�AHDށHD��HD���D�AHD�~�D߾�D�  D�@ D�~�DྸD�  D�AHD�HD��HD�HD�>�D�~�D⾸D�  D�>�D�}qD㾸D�  D�@ D�HD��HD�HD�AHD�HD�D���D�@ D� D澸D���D�@ D�~�D羸D�  D�@ D肏D�� D�  D�>�D�~�D龸D���D�@ D� D�� D�  D�@ D� D뾸D�  D�>�D� D�� D�HD�@ D�HD�� D���D�>�D�}qDD�  D�AHD� D��HD�HD�@ D�~�D�� D�  D�>�D� D��HD�  D�>�D� D�� D���D�@ D�HD��HD�  D�@ D� D��qD���D�>�D��HD��qD���D�@ D�~�D�� D�  D�@ D��HD�� D�HD�@ D�� D�� D��qD�<)D�~�D���D�HD�@ D��HD���?\)?aG�?��?���?\?�(�?�@\)@�R@0��@=p�@Q�@aG�@p��@��\@��@�@�p�@�ff@�{@�
=@��R@Ǯ@�\)@�Q�@�  @���@��@���A ��A�AQ�A�A\)A�
AQ�A�HA\)A"�\A&ffA*=qA.{A1�A5A9��A>{AC33AFffAJ=qAN{AQ�AUAZ=qA\��A`  Adz�AhQ�Al(�Ap  Au�Az�HA~�RA�G�A��A�A�\)A�G�A��HA��A�\)A�G�A�33A�p�A�Q�A��\A�z�A��RA���A��HA�p�A�\)A�G�A�33A��A�\)A�G�A��\A���A�
=A���A�33A�p�A�\)A���AÅA�Aȣ�A��HA��A�
=A�G�A�33A��AָRA���A�33A�p�A߮A��A�A�RA��A�A�A�  A��A�(�A��RA���A��\A�(�A�ffB Q�Bp�BffB  B�B=qB�B��B	p�B
ffB�B��BB�HB�
BG�B�RB�B��B{B
=B  B�B{B
=Bz�BB
=B (�B!G�B"ffB#33B$Q�B%G�B&�\B'�B(��B)B*�HB,  B-G�B.ffB/�
B0��B1�B333B4z�B5��B6�RB7�
B9�B9B:�RB<  B<��B>{B?33B@z�BABC
=BD(�BEp�BF�\BG�BH��BIBK
=BL  BMp�BN=qBO�BP��BQBS
=BT(�BU�BU�BW
=BXQ�BYG�BZffB[�B]�B^ffB_\)B`��Ba��Bb�\Bc�Bd��BeBf�HBh(�Bip�Bj�RBk�
Bl��Bn{Bo\)BpQ�Bq��Br�RBt  Bu�Bv{Bv�HBx(�ByG�BzffB{�
B}G�B~ffB�B�ffB���B���B�(�B��RB��B��B�=qB��HB�\)B�{B���B�\)B��B�z�B��B��B�{B���B�33B��B�ffB��B��B�=qB��RB��B��B�(�B��HB��B�{B��\B�33B��B�{B��\B�
=B�B�Q�B��HB�p�B��B�=qB��RB�p�B�{B��\B��B��B�{B��\B��B���B�=qB��HB�p�B��B��\B�
=B�\)B��B�z�B��B�B�ffB���B��B��
B�ffB��HB���B�Q�B���B�p�B��B�ffB��HB�p�B�(�B��RB�G�B��B�(�B���B�G�B�B��\B��B��B�(�B��\B�33B���B�Q�B���B��B�(�B���B���B��B�{B���B�\)B��B�ffB���B�G�B��
B��\B��B�B�{B��\B�
=B�B�z�B��B�p�B��B\B��B��B�z�B���B�p�B�  BƏ\B�G�B��Bȣ�B�
=BɅB�  B���B�p�B�{B�z�B���BͅB�Q�B��HB�p�B��
B�ffB�33B��
B�z�B���B�p�B�{B��HBՅB��B�z�B�33B��B؏\B�
=Bٙ�B�=qB���Bۙ�B�  Bܣ�B�33B�  B޸RB�G�B߮B�Q�B�
=B�B�z�B��HB�p�B�(�B���B�B�  B��B�p�B�(�B���B�G�B��
B�RB�\)B��B�ffB�
=B��B�z�B��B�B�(�B�RB�B�Q�B���B�B�=qB�
=B�B�=qB���B��
B��\B�
=B�B��\B�p�B�(�B���B�\)B�Q�B���B��C {C p�C �HC=qCz�C��CQ�C�RC�CG�C�RC�Cz�CC�C��C  C\)C��C
=Cz�C��C�C�\C	  C	=qC	��C

=C
\)C
��C�Cz�C�C  Cp�C�C�HC(�Cz�C�RC��C  CQ�Cz�C�C��C�HC{C{CG�Cz�C��C�C��C{C33CG�C�C�C�
C  C  C(�Cp�C�\C�C��C��C33CG�CffC��C��C  C  C(�Cp�C��C��CC  C33CQ�Cp�C�\C�
C  C
=C33Cp�C�\C�C�
C
=C(�CG�Cp�C�C�
C�HC{CG�CQ�Cz�CC�C�C{C\)C�C�\C�C��C(�C33CffC��C��C��C{C=qCG�Cz�C�CC�C�CQ�C\)C�CC��C  C�CffC�\C��C�
C
=C{CG�C�C��CC  C(�C=qC\)C�C�
C�HC(�C\)C\)C�\C�
C�HC 
=C Q�C ffC �\C �
C!
=C!{C!33C!�C!��C!C"
=C"=qC"G�C"p�C"�C"�C"��C#�C#ffC#��C#��C#��C${C$33C$\)C$��C$��C$�
C%{C%Q�C%ffC%�C%�
C&  C&{C&G�C&�C&��C&C'  C'(�C'G�C'p�C'�RC'�C'�C(�C(ffC(�\C(��C(��C){C)(�C)G�C)�\C)�RC)�
C*{C*G�C*Q�C*�C*��C*�
C+  C+G�C+ffC+�C+��C+��C,
=C,G�C,z�C,�C,�C,��C-�C-33C-p�C-��C-�C-�
C.�C.33C.Q�C.�\C.�RC.��C/
=C/33C/=qC/z�C/��C/C0  C0(�C0(�C0G�C0�\C0��C0C1
=C1{C133C1p�C1�C1��C1�C2  C2{C2Q�C2�C2�C2�C2��C2��C333C3ffC3ffC3��C3�
C3�
C4  C4=qC4Q�C4ffC4�C4��C4�HC5{C5=qC5Q�C5�C5�C5�C5��C6
=C6�C633C6p�C6�C6��C6�HC6�C7
=C7G�C7ffC7z�C7�RC7�
C7�HC8{C8G�C8=qC8p�C8��C8�C8��C9
=C9�C933C9p�C9�\C9��C9�HC9��C:  C:=qC:\)C:p�C:�C:C:�
C;{C;(�C;=qC;z�C;��C;�C;�C<  C<{C<Q�C<z�C<�C<�RC<�C<��C=
=C=Q�C=p�C=z�C=C=�HC=�C>33C>G�C>\)C>��C>�RC>�HC?�C?(�C?G�C?�\C?��C?��C@
=C@{C@G�C@z�C@�C@��C@�CA  CA=qCAffCAz�CA�CA�HCA��CB(�CBQ�CBffCB�CB��CB�HCC(�CC=qCCffCC�CCCC�HCD(�CD=qCD\)CD��CD�RCD�CE(�CE=qCEz�CE��CE�RCF  CF{CF33CF�CF��CF�
CF�CG�CGffCGp�CG�CG�CH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111411111111111141111111111111111111111111111111411111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111411111111111111141141141141111111111114111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�A�%A���A��A��A��A��A��A��`A��/A��
A�AҺ^A҅A�K�A�?}A�9XA�1'A�(�A�+A�&�A� �A� �A�"�A��A��A��A��A��A��A�"�A�5?A�\)A���A�"�A��A�/A�C�A�1'A��A�1A�  A��mA�|�A�K�A΍PA�p�A��A���A�|�A��A���Aʛ�A�{Aȉ7A�bA��A�"�A�5?A�E�A�oAöFA��!A�ƨA�t�A�"�A�ZA�bA�VA��TA�"�A�ĜA��DA�=qA�1'A��A�x�A���A��A��A���A���A�;dA�z�A�A���A���A��;A���A��A�;dA��A���A���A���A��TA���A�ƨA�dZA�;dA�G�A��wA��A� �A�&�A�?}A�M�A���A�?}A��+A�$�A�^5A��-A���A��^A�A�Ayx�AvĜAvVAshsAp�/AmdZAg|�A[�hAX�AV^5AQ��AL�+AIK�AG;dACO�AA/A?�A<�jA;7LA8�A6I�A5t�A4�\A3`BA0{A.bA-G�A,�RA+�A)l�A&5?A$�jA$�A#K�A!�FA 9XA��A1A�-Ax�A�/A�A?}A�jAA�A��A��AQ�A�AoA$�A �A�9A/A��A�A��A~�A-A �A�DA
n�A	��A��A(�A	;dA	%A	A
A�A
~�A�!AE�A9XAz�A�A$�AA�A��AA�A�-At�AoA
z�A
{A	�A
  A	��A	p�A	C�A	
=A�uA�Ax�A{A�A
=A%A�A��A\)A �A 5?A z�A �A  �@�S�@��-@�ƨ@���@���@��h@�@���@��-@�b@�dZ@�K�@�j@�A�@�o@�{@�`B@��j@�Z@�b@��
@��@���@�v�@�x�@��@���@�33@@��@�G�@��@�@��@��@�1@��@��@�^@�@�7L@��m@�33@���@�+@�M�@�p�@��m@�K�@��@��@�R@�33@��H@�n�@�`B@��@�9@�l�@�C�@���@�X@��;@�+@�o@�o@���@�J@��@�bN@���@ם�@�33@�+@ָR@�V@���@��`@ԋD@ӍP@��H@җ�@�^5@Ѻ^@���@���@�dZ@�33@�?}@���@��@� �@�l�@�@���@�X@�/@ȓu@���@� �@�S�@��@��@�n�@��T@�X@���@ļj@�r�@�Q�@�  @Õ�@�C�@¸R@�=q@�{@��7@���@��@��u@��F@�
=@���@�5?@���@���@��@�`B@�/@���@�z�@�(�@�o@��\@�$�@�@���@��-@��@��@�I�@��m@��F@�t�@��y@�5?@���@�7L@�%@�%@�%@�Ĝ@�bN@��@��H@��@���@��@���@���@��j@��@���@�M�@��@�p�@��@���@��@�I�@��
@���@���@��P@�dZ@�+@�@��@�^5@�=q@�$�@���@��@�X@�O�@�/@�&�@��@���@���@�Ĝ@�Z@��@�dZ@�
=@�@��@��H@���@���@�n�@�ff@�M�@�{@��@��7@�x�@�G�@��@���@�j@�bN@�Z@� �@��@�ƨ@��P@�C�@��R@��+@�^5@�@���@�G�@���@���@���@��@��D@�A�@� �@��;@��P@�t�@�\)@�33@��y@�ȴ@�v�@��@���@�x�@�V@�Ĝ@�j@�1@��F@��P@�dZ@���@�E�@���@�p�@�G�@��`@�j@� �@�1@���@��@��w@��@�|�@��@���@�n�@�E�@�5?@�5?@�$�@��@�@���@�`B@��@��@�r�@�Z@�A�@�(�@���@�ƨ@���@�C�@�o@�@���@���@�ff@��T@��h@�p�@�7L@�Ĝ@�Z@�(�@���@���@��w@���@��@�K�@��@��\@�M�@���@��-@��7@�&�@�%@��`@�I�@�b@���@��F@�t�@�"�@���@��!@�v�@�V@�5?@�J@���@��@��-@���@��@�O�@��@���@��@�j@�  @��m@��P@�S�@�33@���@��\@�n�@�V@�E�@�{@��@��-@��h@��@�x�@�hs@�?}@��@�(�@�b@��;@��F@�|�@�33@��@�V@�M�@�E�@�$�@�@��@�@�G�@��@��`@���@��D@�Q�@�9X@�1@\)@~��@~{@}��@}��@}?}@|�/@|z�@{�m@{�@{S�@z�\@z^5@zM�@zJ@y%@x �@w\)@w�@v�R@v5?@u��@t�/@tz�@t1@sdZ@so@r�@r~�@r-@q��@q7L@q%@p�9@pA�@o�;@o�@nȴ@nV@m�T@m�-@m�@m`B@m/@l�@l�j@l��@lZ@l�@k��@k�F@kt�@j��@j��@j~�@j=q@j=q@i��@i�^@ihs@i&�@h��@hb@g
=@fE�@e�T@d��@d�D@c�F@co@b�H@b��@b��@b�\@b~�@bM�@aG�@`1'@_�@_��@_��@_|�@_l�@_
=@^�@^�R@^�+@^$�@^@]�@]@]�@\�@\Z@[33@Z^5@Z�@Y�^@X�`@XQ�@W�@W|�@V��@VV@U��@U�-@Up�@T��@T�@So@R=q@RJ@RJ@Q��@QX@P��@PQ�@Pb@O�@O�;@O��@O�w@O�P@O�@Nȴ@NE�@N@M�@MV@L��@LI�@K�
@Ko@J��@J��@J~�@J�@I��@I7L@H��@H�u@HQ�@H �@G��@G;d@F�@FV@E�@E��@EO�@EV@D�@D��@D��@D�D@Dz�@C��@CdZ@B��@B~�@B^5@A��@A�^@A��@A��@Ahs@A&�@@�`@@�u@?�;@?�P@?|�@?l�@?
=@>ȴ@>V@=�@=�@=`B@<�@<��@<��@<�D@<Z@<1@;ƨ@;��@;�@;t�@;C�@;"�@;o@;@:��@:~�@:n�@:�@9�7@9&�@9�@8��@81'@7�@7|�@7\)@7K�@7
=@6��@6��@6��@6��@6v�@6V@65?@6@5�T@5��@5`B@5�@4�j@49X@4�@3�m@3��@3�@3t�@3S�@3C�@3@2~�@1�#@1&�@0�u@0r�@01'@/�@/�@/�P@/;d@.ȴ@.$�@-`B@,�@,�@,�D@,j@,Z@,I�@,9X@,(�@+��@+S�@+o@+@*��@*��@*�@*J@)��@)��@)�@)%@(��@( �@'�w@'�w@'��@'K�@&�+@&E�@&@&@%�T@%@%��@%��@%p�@%/@$��@$�@$z�@$�@#�m@#�m@#�m@#ƨ@#��@#dZ@#o@"��@"^5@!��@!X@!&�@!%@ ��@ �@ bN@ bN@ Q�@ 1'@�@�@+@��@��@��@ȴ@��@�+@E�@�@@�@�@�j@�@��@��@�D@9X@�m@��@t�@dZ@S�@C�@33@"�@�@��@�!@�\@^5@-@J@�@�^@�7@x�@�@��@�`@��@Ĝ@��@Q�@1'@b@�@��@\)@+@�@�R@�R@v�@{@p�@��@��@�/@�j@j@(�@�@1@��@�
@ƨ@�@C�@"�@o@@��@��@�\@M�@�@hs@%@��@�`@�`@�`@��@��@�@ �@�w@�@K�@
=@��@5?@��@@@@��@�@V@��@�/@�@�D@I�@�@1@�m@��@S�@33A���A���A�  A�A�A�1A�
=A�%A�1A���A��A��A��A��A��A��A��A��A��yA��yA��A��yA��yA��A��A��A��A��A��A��yA��A��A��mA��HA��HA��/A��#A��#A��;A��/A��;A���A�ĜA�A�ƨA���AҺ^AҸRA�ĜAҼjAҸRAҸRAҴ9AҶFAҸRA�ƨA�AҴ9AҬAҧ�AҬAҕ�A�n�A�XA�ZA�ZA�`BA�^5A�M�A�E�A�C�A�C�A�C�A�A�A�?}A�?}A�=qA�=qA�=qA�;dA�;dA�9XA�9XA�9XA�9XA�7LA�7LA�7LA�7LA�5?A�33A�33A�33A�5?A�1'A�1'A�/A�+A�&�A�$�A�$�A�"�A�$�A�&�A�&�A�(�A�+A�-A�/A�/A�-A�+A�&�A�$�A�"�A�"�A�"�A�"�A��A� �A�"�A�"�A�$�A�$�A�"�A� �A��A��A��A��A��A��A� �A�$�A�$�A�$�A�$�A�"�A� �A��A��A��A��A��A� �A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A� �A�"�A�$�A�&�A�-A�33A�33A�5?A�7LA�;dA�A�A�G�A�XA�VA�ZA�dZA�x�A�~�A҉7Aң�A�A��A��yA���A�A�oA�&�A�1'A�-A�&�A�(�A�$�A� �A��A�{A�A�
=A��A� �A� �A�"�A�+A�7LA�9XA�;dA�=qA�?}A�A�A�A�A�C�A�C�A�A�A�?}A�=qA�5?A�/A�/A�-A�+A�&�A�"�A� �A��A�bA�bA�bA�bA�bA�
=A�1A�%A�A�  A�  A�  A�A�A�A�  A���A���A���A��A��A��yA��mA��;A���A�AҲ-Aҕ�A҅A�p�A�ZA�;dA�&�A���A��AѶFA�I�A��;A�ffA��yAω7A��TAΛ�A�x�A�G�A��A�
=A��A��#A���A͓uA�Q�A�9XA��A�1A���A���A��A��A��mA��`A��HA��A��
A���A���A�ĜA̾wA̴9A̰!A̩�A̙�A̅A�r�A�M�A�?}A�5?A�-A�+A��A��A�
=A���A��A��A��A��mA��/A˾wAˬA�~�A�E�A�"�A��A�ȴAʃA�&�A��;A�~�A�9XA� �A��A�%A���A��A��#A���AȶFAȋDA�~�A�ffA�M�A�+A��A�{A�bA�
=A�
=A�1A���A�ȴA�x�A���AƑhA�z�A�p�A�bNA�^5A�VA�;dA�bA��A���Ař�A�t�AŁA�A�A�%A��AĶFAď\A�`BA�C�A�(�A�bA�%A�{A�$�A�1'A�oA�A�  A���A�%A�A��A��
Að!A���A�E�A���A�A���A�dZA�O�A�E�A�oA���A��;A�ĜA�^5A�JA���A���A�x�A�jA�S�A��A���A�r�A�7LA�A��TA��wA��A�=qA��A�=qA��A�bA��
A��7A�/A��HA��hA�jA��A���A�;dA��;A���A�`BA�+A�oA�1A��wA�C�A�A��uA�t�A�"�A�`BA�M�A�$�A���A��FA���A�XA�bA�ĜA��uA�ZA�A��A��DA�t�A��A��A��!A��A�x�A�C�A���A�ĜA��A�A�A��A�A��!A��\A��A�\)A��A�ƨA���A�p�A�A�A�(�A�JA��A��A��`A��A�ĜA��A��7A�E�A���A��HA���A��+A�O�A��A���A��7A�t�A�dZA�M�A�C�A�;dA�33A�&�A�
=A��A��A��wA��A���A��PA�dZA�/A�{A���A���A��7A�M�A�$�A��A�{A���A��HA��#A��A���A���A���A���A��jA��-A���A�r�A�M�A�1'A�"�A��A�oA�bA�bA�JA�
=A�1A�  A��A��HA���A���A���A��FA��A���A��A�dZA�O�A�5?A��A�1A��A��;A���A���A���A��!A���A��hA�~�A�r�A�jA�bNA�ZA�XA�ZA�XA�VA�VA�M�A�;dA�"�A�VA���A��`A��;A��A���A��^A��PA�I�A�bA�A��HA��9A��uA�dZA�M�A�A�A�9XA�(�A�1A��A��A��wA��A���A��PA�z�A�p�A�hsA�S�A�E�A�=qA�-A�{A��A��TA���A�ȴA�ȴA�ƨA��jA��^A��RA��RA��A��A��A���A���A���A���A���A���A��\A�|�A�ffA�M�A�/A�JA��A��/A���A��9A���A���A���A���A��hA��A�r�A�hsA�ZA�I�A�"�A��A��HA���A���A��^A��A��!A��!A��!A���A��A���A���A��A��!A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��uA��hA��\A��\A��+A��A�n�A�VA�A�A�1'A�A���A��9A��A�=qA�1A��^A��A�ZA�?}A�$�A�JA���A��A�A���A��\A�hsA�ZA�XA�VA�K�A�/A�VA��A���A��wA��A���A���A��uA��7A�~�A�ffA�O�A�K�A�A�A�7LA�-A�{A�1A���A���A��9A��PA�\)A�9XA�"�A��A�JA���A��#A��^A���A��DA�t�A�ffA�VA�G�A�33A��A���A��mA�ƨA��A���A��hA�v�A�bNA�XA�K�A�=qA�7LA�/A�"�A�VA��#A���A��A��PA�z�A�hsA�bNA�VA�G�A�?}A�5?A�/A�(�A��A��A�{A�1A�A���A��A��A��A��`A���A��RA��uA�z�A�ZA�7LA�&�A�bA��A���A��wA��9A���A���A��7A��A�p�A�^5A�1'A��A��\A�\)A�"�A�JA���A��A��yA��yA��HA��/A���A�ĜA��jA��9A���A��uA�t�A�;dA�&�A��A��HA��HA�`BA�  A�ȴA��A�ffA�S�A�;dG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                A�A�%A���A��A��A��A��A��A��`A��/A��
A�AҺ^A҅A�K�A�?}A�9XA�1'A�(�A�+A�&�A� �A� �A�"�A��A��A��A��A��A��A�"�A�5?A�\)A���A�"�A��A�/A�C�A�1'A��A�1A�  A��mA�|�A�K�A΍PA�p�A��A���A�|�A��A���Aʛ�A�{Aȉ7A�bA��A�"�A�5?A�E�A�oAöFA��!A�ƨA�t�A�"�A�ZA�bA�VA��TA�"�A�ĜA��DA�=qA�1'A��A�x�A���A��A��A���A���A�;dA�z�A�A���A���A��;A���A��A�;dA��A���A���A���A��TA���A�ƨA�dZA�;dA�G�A��wA��A� �A�&�A�?}A�M�A���A�?}A��+A�$�A�^5A��-A���A��^A�A�Ayx�AvĜAvVAshsAp�/AmdZAg|�A[�hAX�AV^5AQ��AL�+AIK�AG;dACO�AA/A?�A<�jA;7LA8�A6I�A5t�A4�\A3`BA0{A.bA-G�A,�RA+�A)l�A&5?A$�jA$�A#K�A!�FA 9XA��A1A�-Ax�A�/A�A?}A�jAA�A��A��AQ�A�AoA$�A �A�9A/A��A�A��A~�A-A �A�DA
n�A	��A��A(�A	;dA	%A	A
A�A
~�A�!AE�A9XAz�A�A$�AA�A��AA�A�-At�AoA
z�A
{A	�A
  A	��A	p�A	C�A	
=A�uA�Ax�A{A�A
=A%A�A��A\)A �A 5?A z�A �A  �@�S�@��-@�ƨ@���@���@��h@�@���@��-@�b@�dZ@�K�@�j@�A�@�o@�{@�`B@��j@�Z@�b@��
@��@���@�v�@�x�@��@���@�33@@��@�G�@��@�@��@��@�1@��@��@�^@�@�7L@��m@�33@���@�+@�M�@�p�@��m@�K�@��@��@�R@�33@��H@�n�@�`B@��@�9@�l�@�C�@���@�X@��;@�+@�o@�o@���@�J@��@�bN@���@ם�@�33@�+@ָR@�V@���@��`@ԋD@ӍP@��H@җ�@�^5@Ѻ^@���@���@�dZ@�33@�?}@���@��@� �@�l�@�@���@�X@�/@ȓu@���@� �@�S�@��@��@�n�@��T@�X@���@ļj@�r�@�Q�@�  @Õ�@�C�@¸R@�=q@�{@��7@���@��@��u@��F@�
=@���@�5?@���@���@��@�`B@�/@���@�z�@�(�@�o@��\@�$�@�@���@��-@��@��@�I�@��m@��F@�t�@��y@�5?@���@�7L@�%@�%@�%@�Ĝ@�bN@��@��H@��@���@��@���@���@��j@��@���@�M�@��@�p�@��@���@��@�I�@��
@���@���@��P@�dZ@�+@�@��@�^5@�=q@�$�@���@��@�X@�O�@�/@�&�@��@���@���@�Ĝ@�Z@��@�dZ@�
=@�@��@��H@���@���@�n�@�ff@�M�@�{@��@��7@�x�@�G�@��@���@�j@�bN@�Z@� �@��@�ƨ@��P@�C�@��R@��+@�^5@�@���@�G�@���@���@���@��@��D@�A�@� �@��;@��P@�t�@�\)@�33@��y@�ȴ@�v�@��@���@�x�@�V@�Ĝ@�j@�1@��F@��P@�dZ@���@�E�@���@�p�@�G�@��`@�j@� �@�1@���@��@��w@��@�|�@��@���@�n�@�E�@�5?@�5?@�$�@��@�@���@�`B@��@��@�r�@�Z@�A�@�(�@���@�ƨ@���@�C�@�o@�@���@���@�ff@��T@��h@�p�@�7L@�Ĝ@�Z@�(�@���@���@��w@���@��@�K�@��@��\@�M�@���@��-@��7@�&�@�%@��`@�I�@�b@���@��F@�t�@�"�@���@��!@�v�@�V@�5?@�J@���@��@��-@���@��@�O�@��@���@��@�j@�  @��m@��P@�S�@�33@���@��\@�n�@�V@�E�@�{@��@��-@��h@��@�x�@�hs@�?}@��@�(�@�b@��;@��F@�|�@�33@��@�V@�M�@�E�@�$�@�@��@�@�G�@��@��`@���@��D@�Q�@�9X@�1@\)@~��@~{@}��@}��@}?}@|�/@|z�@{�m@{�@{S�@z�\@z^5@zM�@zJ@y%@x �@w\)@w�@v�R@v5?@u��@t�/@tz�@t1@sdZ@so@r�@r~�@r-@q��@q7L@q%@p�9@pA�@o�;@o�@nȴ@nV@m�T@m�-@m�@m`B@m/@l�@l�j@l��@lZ@l�@k��@k�F@kt�@j��@j��@j~�@j=q@j=q@i��@i�^@ihs@i&�@h��@hb@g
=@fE�@e�T@d��@d�D@c�F@co@b�H@b��@b��@b�\@b~�@bM�@aG�@`1'@_�@_��@_��@_|�@_l�@_
=@^�@^�R@^�+@^$�@^@]�@]@]�@\�@\Z@[33@Z^5@Z�@Y�^@X�`@XQ�@W�@W|�@V��@VV@U��@U�-@Up�@T��@T�@So@R=q@RJ@RJ@Q��@QX@P��@PQ�@Pb@O�@O�;@O��@O�w@O�P@O�@Nȴ@NE�@N@M�@MV@L��@LI�@K�
@Ko@J��@J��@J~�@J�@I��@I7L@H��@H�u@HQ�@H �@G��@G;d@F�@FV@E�@E��@EO�@EV@D�@D��@D��@D�D@Dz�@C��@CdZ@B��@B~�@B^5@A��@A�^@A��@A��@Ahs@A&�@@�`@@�u@?�;@?�P@?|�@?l�@?
=@>ȴ@>V@=�@=�@=`B@<�@<��@<��@<�D@<Z@<1@;ƨ@;��@;�@;t�@;C�@;"�@;o@;@:��@:~�@:n�@:�@9�7@9&�@9�@8��@81'@7�@7|�@7\)@7K�@7
=@6��@6��@6��@6��@6v�@6V@65?@6@5�T@5��@5`B@5�@4�j@49X@4�@3�m@3��@3�@3t�@3S�@3C�@3@2~�@1�#@1&�@0�u@0r�@01'@/�@/�@/�P@/;d@.ȴ@.$�@-`B@,�@,�@,�D@,j@,Z@,I�@,9X@,(�@+��@+S�@+o@+@*��@*��@*�@*J@)��@)��@)�@)%@(��@( �@'�w@'�w@'��@'K�@&�+@&E�@&@&@%�T@%@%��@%��@%p�@%/@$��@$�@$z�@$�@#�m@#�m@#�m@#ƨ@#��@#dZ@#o@"��@"^5@!��@!X@!&�@!%@ ��@ �@ bN@ bN@ Q�@ 1'@�@�@+@��@��@��@ȴ@��@�+@E�@�@@�@�@�j@�@��@��@�D@9X@�m@��@t�@dZ@S�@C�@33@"�@�@��@�!@�\@^5@-@J@�@�^@�7@x�@�@��@�`@��@Ĝ@��@Q�@1'@b@�@��@\)@+@�@�R@�R@v�@{@p�@��@��@�/@�j@j@(�@�@1@��@�
@ƨ@�@C�@"�@o@@��@��@�\@M�@�@hs@%@��@�`@�`@�`@��@��@�@ �@�w@�@K�@
=@��@5?@��@@@@��@�@V@��@�/@�@�D@I�@�@1@�m@��@S�@33A���A���A�  A�A�A�1A�
=A�%A�1A���A��A��A��A��A��A��A��A��A��yA��yA��A��yA��yA��A��A��A��A��A��A��yA��A��A��mA��HA��HA��/A��#A��#A��;A��/A��;A���A�ĜA�A�ƨA���AҺ^AҸRA�ĜAҼjAҸRAҸRAҴ9AҶFAҸRA�ƨA�AҴ9AҬAҧ�AҬAҕ�A�n�A�XA�ZA�ZA�`BA�^5A�M�A�E�A�C�A�C�A�C�A�A�A�?}A�?}A�=qA�=qA�=qA�;dA�;dA�9XA�9XA�9XA�9XA�7LA�7LA�7LA�7LA�5?A�33A�33A�33A�5?A�1'A�1'A�/A�+A�&�A�$�A�$�A�"�A�$�A�&�A�&�A�(�A�+A�-A�/A�/A�-A�+A�&�A�$�A�"�A�"�A�"�A�"�A��A� �A�"�A�"�A�$�A�$�A�"�A� �A��A��A��A��A��A��A� �A�$�A�$�A�$�A�$�A�"�A� �A��A��A��A��A��A� �A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A� �A��A� �A�"�A�$�A�&�A�-A�33A�33A�5?A�7LA�;dA�A�A�G�A�XA�VA�ZA�dZA�x�A�~�A҉7Aң�A�A��A��yA���A�A�oA�&�A�1'A�-A�&�A�(�A�$�A� �A��A�{A�A�
=A��A� �A� �A�"�A�+A�7LA�9XA�;dA�=qA�?}A�A�A�A�A�C�A�C�A�A�A�?}A�=qA�5?A�/A�/A�-A�+A�&�A�"�A� �A��A�bA�bA�bA�bA�bA�
=A�1A�%A�A�  A�  A�  A�A�A�A�  A���A���A���A��A��A��yA��mA��;A���A�AҲ-Aҕ�A҅A�p�A�ZA�;dA�&�A���A��AѶFA�I�A��;A�ffA��yAω7A��TAΛ�A�x�A�G�A��A�
=A��A��#A���A͓uA�Q�A�9XA��A�1A���A���A��A��A��mA��`A��HA��A��
A���A���A�ĜA̾wA̴9A̰!A̩�A̙�A̅A�r�A�M�A�?}A�5?A�-A�+A��A��A�
=A���A��A��A��A��mA��/A˾wAˬA�~�A�E�A�"�A��A�ȴAʃA�&�A��;A�~�A�9XA� �A��A�%A���A��A��#A���AȶFAȋDA�~�A�ffA�M�A�+A��A�{A�bA�
=A�
=A�1A���A�ȴA�x�A���AƑhA�z�A�p�A�bNA�^5A�VA�;dA�bA��A���Ař�A�t�AŁA�A�A�%A��AĶFAď\A�`BA�C�A�(�A�bA�%A�{A�$�A�1'A�oA�A�  A���A�%A�A��A��
Að!A���A�E�A���A�A���A�dZA�O�A�E�A�oA���A��;A�ĜA�^5A�JA���A���A�x�A�jA�S�A��A���A�r�A�7LA�A��TA��wA��A�=qA��A�=qA��A�bA��
A��7A�/A��HA��hA�jA��A���A�;dA��;A���A�`BA�+A�oA�1A��wA�C�A�A��uA�t�A�"�A�`BA�M�A�$�A���A��FA���A�XA�bA�ĜA��uA�ZA�A��A��DA�t�A��A��A��!A��A�x�A�C�A���A�ĜA��A�A�A��A�A��!A��\A��A�\)A��A�ƨA���A�p�A�A�A�(�A�JA��A��A��`A��A�ĜA��A��7A�E�A���A��HA���A��+A�O�A��A���A��7A�t�A�dZA�M�A�C�A�;dA�33A�&�A�
=A��A��A��wA��A���A��PA�dZA�/A�{A���A���A��7A�M�A�$�A��A�{A���A��HA��#A��A���A���A���A���A��jA��-A���A�r�A�M�A�1'A�"�A��A�oA�bA�bA�JA�
=A�1A�  A��A��HA���A���A���A��FA��A���A��A�dZA�O�A�5?A��A�1A��A��;A���A���A���A��!A���A��hA�~�A�r�A�jA�bNA�ZA�XA�ZA�XA�VA�VA�M�A�;dA�"�A�VA���A��`A��;A��A���A��^A��PA�I�A�bA�A��HA��9A��uA�dZA�M�A�A�A�9XA�(�A�1A��A��A��wA��A���A��PA�z�A�p�A�hsA�S�A�E�A�=qA�-A�{A��A��TA���A�ȴA�ȴA�ƨA��jA��^A��RA��RA��A��A��A���A���A���A���A���A���A��\A�|�A�ffA�M�A�/A�JA��A��/A���A��9A���A���A���A���A��hA��A�r�A�hsA�ZA�I�A�"�A��A��HA���A���A��^A��A��!A��!A��!A���A��A���A���A��A��!A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��uA��uA��hA��\A��\A��+A��A�n�A�VA�A�A�1'A�A���A��9A��A�=qA�1A��^A��A�ZA�?}A�$�A�JA���A��A�A���A��\A�hsA�ZA�XA�VA�K�A�/A�VA��A���A��wA��A���A���A��uA��7A�~�A�ffA�O�A�K�A�A�A�7LA�-A�{A�1A���A���A��9A��PA�\)A�9XA�"�A��A�JA���A��#A��^A���A��DA�t�A�ffA�VA�G�A�33A��A���A��mA�ƨA��A���A��hA�v�A�bNA�XA�K�A�=qA�7LA�/A�"�A�VA��#A���A��A��PA�z�A�hsA�bNA�VA�G�A�?}A�5?A�/A�(�A��A��A�{A�1A�A���A��A��A��A��`A���A��RA��uA�z�A�ZA�7LA�&�A�bA��A���A��wA��9A���A���A��7A��A�p�A�^5A�1'A��A��\A�\)A�"�A�JA���A��A��yA��yA��HA��/A���A�ĜA��jA��9A���A��uA�t�A�;dA�&�A��A��HA��HA�`BA�  A�ȴA��A�ffA�S�A�;dG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
8�B
:*B
8RB
7B
6zB
6�B
6B
6�B
6FB
5tB
5?B
33B
2-B
0�B
-B
,�B
,�B
,�B
,B
,�B
-CB
-wB
-�B
/�B
/�B
0UB
0UB
1'B
2�B
4nB
7�B
>�B
I�B
d�B
�oB
�SB
��B
��B
ޞB
�/B
��B
ںB
ٴB
��B
��B
�B  BBJB�B�B�B6�B1�B7�BCaBWsBa�B�{B�rB��B��B�aB��B�B�/B�]BuBB"�B/OB-�B-wB*0B(�B'�B&�B#:BqBMBBB iB��B�B�B�yB��B��B�aB��B�-B��B��B�*B��B|�Bn�B\)BJ�B<�B4nB%B�B�B
�)B
��B
��B
��B
poB
k�B
R�B
C-B
qB	��B	�ZB	��B	��B	�9B	�tB	��B	x�B	e�B	%FB	.B		7B	�B��B�B��B�HB�gBӏBȀB��B�zB�EBٴB��B�mB�B�DB��B�>B��B��B֡B�XB��B��B��B�[B��B�B�CB�B�B�CB��B��B�RB��B�_B��B�!B��B�B�OB��B�UB�-B�0B�>B	�B	�B	�B�B��B��B��BƨB�)B�]B�B	�B	{B	,B	=B	<B	Q�B	R B	W�B	Z�B	f2B	gB	iDB	k�B	tTB	t�B	u�B	t�B	}VB	��B	� B	�B	~(B	{JB	yrB	m�B	}�B	��B	tB	t�B	yrB	zB	z�B	|B	zxB	}�B	��B	��B	�~B	�~B	�DB	�rB	��B	��B	��B	��B	�@B	�B	��B	�'B	��B	�[B	�!B	�B	�}B	�OB	��B	�[B	�-B	��B	��B	��B	�?B	��B	��B	��B	�^B	�B	�6B	��B	�qB	��B	�OB	��B	�3B	�B	�aB	��B	�B	�B	��B	�XB	�B	͟B	�B	�)B	�RB	�#B	ɺB	��B	�B	��B	бB	ΥB	�^B	�0B	�jB	�^B	̘B	�B	�vB	ΥB	ΥB	�B	�vB	бB	�HB	ΥB	�HB	�B	�NB	҉B	��B	�[B	�}B	�[B	��B	�9B	��B	��B	�,B	��B	՛B	��B	�TB	�[B	ҽB	�6B	ΥB	�HB	��B	�}B	҉B	�}B	�HB	�B	�mB	�EB	��B	�gB	��B	�yB	��B	ٴB	�KB	�QB	یB	��B	�]B	ޞB	��B	��B	��B	��B	��B	�|B	�NB	��B	�,B	��B	�B	�fB	�fB	��B	�2B	��B	��B	�B	�8B	��B	�DB	�B	��B	�>B	�sB	�KB	�B	�)B	��B	�/B	� B	� B	�B	�/B	�iB	� B	�5B	�5B	�iB	��B	��B	�B	�GB	�oB	�oB	�B	�B	�GB	�B	�GB	��B	��B	�AB	��B	�|B	�B	�MB	��B	��B	��B	��B	�+B	�fB	�	B	�B	��B	�JB	��B	��B	��B	��B
 iB
 iB
B
B
B
�B
B
�B
GB
�B
�B
B
�B
�B
�B
	7B
xB
xB
�B
�B
�B
�B
�B
�B
�B
4B
:B
:B
oB
oB
B
�B
uB
�B
�B
FB
FB
FB
B
�B
�B
�B
B
�B
�B
$B
�B
SB
�B
SB
B
�B
�B
�B
�B
�B
eB
eB
7B
B
�B
B
kB
�B
	B
=B
CB
B
�B
IB
�B
~B
~B
�B
~B
�B
~B
~B
�B
�B
�B
�B
�B
!B
�B
�B
�B
�B
!B
�B
�B
�B
�B
OB
OB
�B
�B
VB
!B
 �B
 �B
 �B
!�B
!-B
 �B
!bB
"4B
!�B
!�B
!�B
 \B
!-B
 �B
!-B
!-B
!-B
!�B
 �B
"hB
!�B
"�B
#B
#:B
#�B
#�B
$�B
%B
&B
($B
'�B
'�B
(�B
(�B
)*B
)�B
*eB
*�B
+B
+6B
+6B
+kB
+kB
+�B
+6B
*�B
*�B
+kB
+6B
+kB
+kB
,B
,B
,�B
,�B
,�B
.B
-�B
-�B
-�B
-�B
.�B
.�B
.�B
/OB
/B
/B
.�B
/OB
1'B
0�B
1'B
1[B
1�B
1�B
2-B
3hB
3hB
3hB
3hB
3�B
3�B
3�B
3�B
49B
49B
4nB
4B
5B
4�B
4�B
5B
5�B
6B
6�B
6�B
6�B
7LB
7LB
7�B
8B
8RB
8RB
9XB
9$B
8�B
9$B
:�B
;0B
<B
<6B
<B
<�B
=<B
=qB
>B
>wB
?B
?HB
?HB
?�B
?�B
@�B
@�B
@�B
A B
A B
AUB
A�B
AUB
B�B
B�B
B�B
B[B
A�B
B�B
B�B
B�B
B'B
B�B
B�B
C-B
C-B
C-B
EB
C�B
D�B
E9B
DgB
EmB
EB
E�B
E�B
FtB
GB
HB
GB
G�B
H�B
HKB
I�B
I�B
I�B
I�B
I�B
IRB
I�B
I�B
K�B
L0B
K�B
LdB
L0B
L0B
L�B
M6B
M6B
MjB
M�B
M�B
M6B
MB
M�B
NpB
M�B
NB
O�B
OvB
N�B
O�B
P�B
PHB
P}B
Q�B
Q�B
P�B
PHB
PB
P�B
QB
QNB
R B
R B
R�B
R B
S[B
S&B
S[B
S[B
R�B
S[B
R�B
R�B
R�B
S�B
T�B
U�B
U2B
T�B
U�B
V�B
WsB
W�B
W�B
YKB
YB
Y�B
Y�B
ZB
ZB
ZQB
Z�B
[�B
[�B
[#B
\�B
[�B
\�B
]/B
]/B
]/B
]�B
]�B
^B
^5B
]�B
^5B
]dB
^B
_B
_B
_;B
_�B
_�B
`B
_pB
_pB
`B
`vB
`B
`vB
bB
a|B
a�B
a|B
bB
bNB
bNB
b�B
cTB
c B
c�B
d&B
cTB
c�B
c�B
d&B
dZB
dZB
d�B
d�B
d�B
d�B
d�B
d�B
e,B
e`B
d�B
e�B
e�B
ffB
e�B
e�B
gmB
gmB
gmB
gmB
g�B
h>B
h
B
h>B
g�B
h
B
h>B
g�B
h
B
h
B
h>B
h�B
iDB
iyB
iDB
i�B
iyB
i�B
j�B
jKB
jB
jB
jB
j�B
j�B
k�B
k�B
k�B
lWB
l�B
l"B
l�B
l"B
l�B
l"B
l�B
k�B
k�B
lWB
m]B
m]B
l�B
m]B
m�B
m�B
m]B
m]B
l�B
l�B
m�B
m)B
m)B
m�B
m)B
m)B
m�B
m�B
m)B
o5B
n�B
n/B
n�B
n�B
o�B
oiB
p;B
o5B
oiB
o�B
o�B
pB
p�B
poB
poB
qAB
q�B
rB
r|B
rB
q�B
rB
r|B
rB
rGB
sB
r�B
s�B
tTB
t�B
tTB
t�B
t�B
uZB
t�B
uZB
t�B
u�B
u�B
v+B
v`B
v+B
u�B
u�B
u�B
v+B
v�B
v�B
v�B
v�B
xB
xB
xlB
x�B
x�B
x8B
xlB
x�B
yrB
yrB
y	B
y>B
y>B
y>B
y>B
y�B
zxB
zxB
zxB
z�B
{B
z�B
z�B
{JB
{�B
{B
{JB
{JB
{B
{B
{B
{B
{�B
|�B
|B
|�B
}"B
}VB
}�B
~(B
}�B
}"B
}�B
~�B
cB
~�B
~�B
~�B
�B
�4B
�4B
�B
�B
�4B
��B
��B
�B
�;B
�;B
�;B
��B
��B
��B
��B
�B
�B
�GB
��B
��B
�GB
�{B
�{B
��B
��B
��B
��B
��B
�B
��B
��B
�B
�YB
��B
�YB
��B
��B
��B
��B
��B
��B
�+B
��B
�_B
��B
�1B
��B
�1B
��B
�fB
�fB
9$B
9XB
9�B
:�B
:�B
9�B
9�B
9�B
9$B
8�B
6B
4�B
5B
6B
7B
7LB
4�B
8B
6�B
7LB
7�B
7B
7B
8RB
6B
5?B
7B
8B
7�B
9XB
6B
6�B
7LB
7�B
6�B
6�B
33B
9XB
5?B
5�B
4B
4nB
49B
5B
/�B
4�B
6�B
/OB
/B
0�B
2�B
33B
3hB
2�B
2�B
,=B
2�B
5B
0�B
/�B
0!B
3�B
.�B
2aB
(�B
.�B
+B
,=B
1[B
+�B
+�B
,B
,qB
,qB
-wB
-CB
-�B
-wB
-�B
-�B
-�B
-�B
-�B
-CB
-�B
-B
,�B
,qB
,qB
+�B
+kB
+B
+kB
,B
+�B
,B
,qB
-�B
-CB
-CB
,=B
,=B
+kB
,B
+�B
+�B
+kB
+kB
,�B
-B
-CB
-�B
-�B
-�B
.IB
.B
.IB
.IB
.IB
-�B
-B
,qB
,qB
,�B
-CB
-�B
.IB
.�B
.�B
.�B
.�B
.B
.B
.�B
.�B
.�B
.B
/�B
0!B
0UB
0�B
0UB
0�B
/�B
/B
.�B
.�B
/�B
0UB
0�B
1[B
1�B
0�B
0�B
0�B
/�B
/�B
/�B
/OB
/B
/B
/�B
/�B
0UB
0�B
0�B
0�B
1'B
2�B
3�B
49B
3hB
3�B
2�B
2aB
2-B
2�B
3hB
3�B
3�B
4�B
5tB
5?B
6zB
7�B
8B
8�B
:�B
:�B
;0B
=�B
?�B
@B
A B
A�B
A�B
A�B
GB
I�B
I�B
IB
G�B
M6B
T,B
S&B
T�B
[WB
e�B
m�B
u%B
v�B
sMB
{JB
�MB
�SB
�B
�B
�_B
�	B
��B
��B
�@B
��B
��B
�B
��B
B
��B
�,B
�gB
רB
�QB
یB
�WB
�dB
��B
�B
��B
�pB
ߤB
��B
��B
ݘB
�/B
��B
�dB
�5B
�/B
ޞB
ݘB
�/B
��B
ܒB
�WB
یB
��B
�WB
�WB
�)B
�]B
�WB
چB
�B
��B
�B
ںB
چB
�WB
�#B
�B
خB
�yB
�EB
��B
�EB
�QB
�#B
�KB
�B
�sB
�B
�sB
ܒB
�KB
ٴB
�sB
�B
��B
�HB
�sB
�vB
�B
��B
�B
�KB
�"B
�vB
��B
��B
�B �B_B	B
�BDB
	BDBB�B�BxBBBDBB�B"B�B~B~B�B�B�B�B�BPB�B�B.BVB�B(B�BVB"B�B B�B7B#nB-wB+B1'B1�B@�BAUB;0BB�B33B,=B/B0UB2�B1'B.B0�B6FB6�B6zB=qB;0B?�B>B?�BAUBC�BFtBIBJ#BMjB_;Bd�BYKBT,BS&BVmB`�BgmBg�Bg�Be,B^�Bc�Bb�B��B�kB�~B��B�B��B�fB��B��B��B�bB�eB�VB�B�KB��B�0B��B�B� B��B�B��B�rB�#B��B�B�[B��B�XB�]B�BݘB�B�B�B�B�B�yB�B�B�B�B��B�B�)B�/B�
B֡B�]B�;B�`B��B��B�]B�JB��B�B�B	7BSB4B1B%FB \B!-B!bB �BBkB(�B'�B*eB)�B'�B5�B2�B)�B.IB*0B+kB*0B3�B,=B,qB+6B-�B.IB.�B%B&LB*�B+�B-�B$�B"�B/�B'B*�B($B/�B!�B(�B+6B$@B#�B'�B,B1'B&LB(�B'�B$B%FB �B!�B"hB �B�B$B%zB(�B�B�B!-B �B$@B \B*�B �BIB�BxBqBqBkB�B�B�B7B�B�B+B�B	B�B�BB�B_BB:B�B�B�B�B�B�BVBB
=BB~BJB�BBB	�B�B%B%BB�BSBBB�B�B�BB�B�BMB{B{B�BuB �B�B;B��B��B��B�B�xB�rB�B��B�`B��B��B�ZB��B��B�%B�B�B�B�MB�B��B�MB�B��B�GB� B�]B�B�5B�|B��B��B�8B�B�B�yB�ZB�B�BBݘB��B�BB�jBںBޞBچB�EB��B�B�mB�2B��BԕB�&BӏBԕBҽB�B�BB�B�B�jB��B�pB�^B�^B͟B��BʌB��B�)B�B��B�RB�B�KB�RB�RB��BǮB��B�gBB�}B��B��B�<B��B��B�BB�jB�qB�<B��B��B�UB��B��B�FB�nB��B��B�[B�'B�-B�B��B��B��B��B��B�-B�-B��B�[B��B��B��B��B��B��B�'B�'B�OB�UB�[B��B�!B�UB�B��B�'B�B�B��B�B�B�}B�B��B��B��B�$B�XB��B��B��B��B�*B��B�!B��B�+B�{B�oB�.B��B�(B��B�DB��B�B�lB��B�AB�oB�AB��B��B��B}�B{�B|�BzDBw�Bx�BxlBu�BxlBv�Bs�Bt�Bs�Br|BqvBp�BncBpoBn�BqvBl�Bh�Bf2Bc�Bb�BaHBf�Bb�B`�B^�B\�B\�BZ�BZBZ�BWsBW
BVBS�BO�BP}BOvBOvBNpBK^BJ�BK^BG�BG�BGzBK�BH�BEBC�BC�B@�B?�B<jB>wB=�B:�B<6B;0B9�B:�B8�B8B9�B7�B6zB6�B49B3�B7LB6�B4B5tB2�B/�B/�B+6B-B-CB(�B%�B%FB$@B"hB!�B�B�B~B!�BCB%B�B�BB	�B	7BBfB	lB%B�B�BB�BuB 4BB�B
��B
��BuB	7B
�B
�KB
�B
�B
�QB
�/B
�)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                B
0�B
26B
0^B
/#B
.�B
.�B
.B
.�B
.RB
-�B
-KB
+?B
*9B
(�B
%B
$�B
$�B
$�B
$B
$�B
%OB
%�B
%�B
'�B
'�B
(aB
(aB
)3B
+B
,zB
/�B
6�B
A�B
\�B
y{B
�_B
��B
�B
֪B
�;B
��B
��B
��B
��B
��B
�B
�BBVB�B�B�B.�B*B/�B;mBOBY�B{�B�~B��B��B�mB��B߭B�;B�iB��BB�B'[B%�B%�B"<B �B�B�BFB}BYB!B�+B�uB�	B�BާBЅB��B��B�mB�B�9B��B��B�6B��Bt�Bf�BT5BB�B4�B,zBB�B
��B
�5B
��B
��B
{�B
h{B
c�B
J�B
;9B
}B	��B	�fB	�
B	��B	�EB	��B	��B	p�B	^
B	RB	:B	CB��B��B�B�
B�TB�sB˛B��B��B��B�QB��B��B�yB�%B�PB��B�JB��B��BέB�dB��B��B��B�gB��B� B�OB�B�'B�OB��B��B�^B��B�kB�B�-B��B�B�[B��B�aB�9B�<B�JB�B	�B�B�%B��B��B��B��B�5B�iB�B��B	�B	$B	5B	4B	I�B	J,B	O�B	R�B	^>B	_B	aPB	c�B	l`B	l�B	m�B	l�B	ubB	x�B	xB	w�B	v4B	sVB	q~B	fB	u�B	x�B	l+B	l�B	q~B	rB	r�B	t(B	r�B	u�B	��B	��B	��B	��B	�PB	�~B	��B	�B	��B	��B	�LB	�!B	��B	�3B	��B	�gB	�-B	� B	��B	�[B	��B	�gB	�9B	��B	��B	��B	�KB	��B	��B	��B	�jB	�B	�BB	��B	�}B	��B	�[B	��B	�?B	�B	�mB	�
B	�B	�#B	��B	�dB	�B	ūB	�B	�5B	�^B	�/B	��B	��B	� B	��B	ȽB	ƱB	�jB	�<B	�vB	�jB	ĤB	�B	ǂB	ƱB	ƱB	�B	ǂB	ȽB	�TB	ƱB	�TB	�&B	�ZB	ʕB	�B	�gB	ȉB	�gB	��B	�EB	�
B	��B	�8B	�
B	ͧB	�
B	�`B	�gB	��B	�BB	ƱB	�TB	�B	ȉB	ʕB	ȉB	�TB	�B	�yB	�QB	��B	�sB	��B	ЅB	��B	��B	�WB	�]B	ӘB	��B	�iB	֪B	��B	��B	��B	��B	��B	وB	�ZB	�B	�8B	��B	ܛB	�rB	�rB	�
B	�>B	�
B	��B	ݡB	�DB	��B	�PB	�"B	��B	�JB	�B	�WB	��B	�5B	� B	�;B	�B	�B	�B	�;B	�uB	�B	�AB	�AB	�uB	��B	��B	�B	�SB	�{B	�{B	�B	�B	�SB	�B	�SB	��B	��B	�MB	��B	�B	�B	�YB	��B	��B	��B	��B	�7B	�rB	�B	�B	��B	�VB	��B	��B	��B	��B	�uB	�uB	�B	�B	�B	��B	�B	��B	�SB	��B	��B	�+B	��B	��B
 	B
CB
�B
�B
�B
�B
�B
�B
�B
�B
�B
	@B

FB

FB

{B

{B
B
�B
�B
�B
�B
RB
RB
RB
B
�B
�B
�B
*B
�B
�B
0B
�B
_B
�B
_B
*B
�B
�B
�B
�B
�B
qB
qB
CB
B
�B
B
wB
�B
B
IB
OB
!B
�B
UB
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
-B
�B
�B
�B
�B
-B
�B
�B
�B
�B
[B
[B
�B
�B
bB
-B
B
B
B
B
9B
�B
nB
@B
B
B
�B
hB
9B
B
9B
9B
9B
�B
�B
tB
�B
�B
B
FB
�B
�B
�B
B
$B
 0B
�B
�B
 �B
 �B
!6B
!�B
"qB
"�B
#B
#BB
#BB
#wB
#wB
#�B
#BB
"�B
"�B
#wB
#BB
#wB
#wB
$B
$B
$�B
$�B
$�B
& B
%�B
%�B
%�B
%�B
&�B
&�B
&�B
'[B
''B
''B
&�B
'[B
)3B
(�B
)3B
)gB
)�B
)�B
*9B
+tB
+tB
+tB
+tB
+�B
+�B
+�B
+�B
,EB
,EB
,zB
,B
-B
,�B
,�B
-B
-�B
.B
.�B
.�B
.�B
/XB
/XB
/�B
0)B
0^B
0^B
1dB
10B
0�B
10B
2�B
3<B
4B
4BB
4B
4�B
5HB
5}B
6B
6�B
7 B
7TB
7TB
7�B
7�B
8�B
8�B
8�B
9,B
9,B
9aB
9�B
9aB
:�B
;B
:�B
:gB
9�B
:�B
;B
:�B
:3B
;B
:�B
;9B
;9B
;9B
=B
<
B
<�B
=EB
<sB
=yB
=B
=�B
=�B
>�B
?B
@#B
?B
?�B
@�B
@WB
A�B
A�B
A�B
A�B
A�B
A^B
A�B
A�B
C�B
D<B
DB
DpB
D<B
D<B
D�B
EBB
EBB
EvB
E�B
E�B
EBB
EB
E�B
F|B
E�B
FB
G�B
G�B
F�B
G�B
H�B
HTB
H�B
I�B
I�B
H�B
HTB
H B
H�B
I&B
IZB
J,B
J,B
J�B
J,B
KgB
K2B
KgB
KgB
J�B
KgB
J�B
J�B
J�B
K�B
L�B
M�B
M>B
M
B
M�B
N�B
OB
O�B
O�B
QWB
Q#B
Q�B
Q�B
R)B
R)B
R]B
R�B
S�B
S�B
S/B
T�B
TB
T�B
U;B
U;B
U;B
U�B
U�B
VB
VAB
U�B
VAB
UpB
VB
WB
WB
WGB
W�B
W�B
XB
W|B
W|B
XB
X�B
XB
X�B
Z%B
Y�B
Y�B
Y�B
Z%B
ZZB
ZZB
Z�B
[`B
[,B
[�B
\2B
[`B
[�B
[�B
\2B
\fB
\fB
\�B
\�B
\�B
]B
]B
]B
]8B
]lB
]B
]�B
^
B
^rB
^
B
^
B
_yB
_yB
_yB
_yB
_�B
`JB
`B
`JB
_�B
`B
`JB
_�B
`B
`B
`JB
`�B
aPB
a�B
aPB
a�B
a�B
a�B
b�B
bWB
b�B
b�B
b"B
b�B
b�B
c�B
c�B
c�B
dcB
d�B
d.B
d�B
d.B
d�B
d.B
d�B
c�B
c�B
dcB
eiB
eiB
e B
eiB
e�B
e�B
eiB
eiB
d�B
e B
e�B
e5B
e5B
e�B
e5B
e5B
fB
e�B
e5B
gAB
f�B
f;B
f�B
f�B
g�B
guB
hGB
gAB
guB
g�B
g�B
hB
h�B
h{B
h{B
iMB
i�B
jB
j�B
jB
i�B
jB
j�B
jB
jSB
k%B
j�B
k�B
l`B
l�B
l`B
l�B
l�B
mfB
l�B
mfB
l�B
m�B
m�B
n7B
nlB
n7B
m�B
nB
nB
n7B
n�B
n�B
n�B
o	B
pB
pB
pxB
p�B
p�B
pDB
pxB
p�B
q~B
q~B
qB
qJB
qJB
qJB
qJB
q�B
r�B
r�B
r�B
r�B
s"B
r�B
r�B
sVB
s�B
s"B
sVB
sVB
s�B
s�B
s�B
s�B
s�B
t�B
t(B
t�B
u.B
ubB
u�B
v4B
u�B
u.B
u�B
v�B
woB
wB
v�B
wB
w�B
x@B
x@B
w�B
w�B
x@B
x�B
x�B
yB
yGB
yGB
yGB
y�B
y�B
y�B
y�B
zB
zB
{SB
z�B
z�B
{SB
{�B
{�B
z�B
z�B
{�B
{�B
{�B
|%B
|�B
|�B
}+B
~eB
~�B
~eB
}�B
}�B
~�B
~�B
~�B
~�B
7B
�B
kB
�B
�=B
�B
�=B
��B
�rB
�rB
10B
1dB
1�B
2�B
2�B
1�B
1�B
1�B
10B
0�B
.B
,�B
-B
.B
/#B
/XB
,�B
0)B
.�B
/XB
/�B
/#B
/#B
0^B
.B
-KB
/#B
0)B
/�B
1dB
.B
.�B
/XB
/�B
.�B
.�B
+?B
1dB
-KB
-�B
,B
,zB
,EB
-B
'�B
,�B
.�B
'[B
''B
(�B
*�B
+?B
+tB
*�B
*�B
$IB
*�B
-B
(�B
'�B
(-B
+�B
&�B
*mB
 �B
&�B
#B
$IB
)gB
#�B
#�B
$B
$}B
$}B
%�B
%OB
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%OB
%�B
%B
$�B
$}B
$}B
#�B
#wB
#B
#wB
$B
#�B
$B
$}B
%�B
%OB
%OB
$IB
$IB
#wB
$B
#�B
#�B
#wB
#wB
$�B
%B
%OB
%�B
%�B
%�B
&UB
& B
&UB
&UB
&UB
%�B
%B
$}B
$}B
$�B
%OB
%�B
&UB
&�B
&�B
&�B
&�B
& B
& B
&�B
&�B
&�B
& B
'�B
(-B
(aB
(�B
(aB
(�B
'�B
''B
&�B
&�B
'�B
(aB
(�B
)gB
)�B
(�B
(�B
(�B
'�B
'�B
'�B
'[B
''B
''B
'�B
'�B
(aB
(�B
(�B
(�B
)3B
*�B
+�B
,EB
+tB
+�B
*�B
*mB
*9B
+B
+tB
+�B
+�B
,�B
-�B
-KB
.�B
/�B
0)B
0�B
2�B
3B
3<B
5�B
7�B
8&B
9,B
9�B
9�B
9�B
?B
A�B
A�B
A)B
?�B
EBB
L8B
K2B
L�B
ScB
]�B
e�B
m1B
o	B
kYB
sVB
|YB
}_B
|%B
}+B
kB
�B
��B
��B
�LB
��B
��B
�#B
��B
��B
��B
�8B
�sB
ϴB
�]B
ӘB
�cB
�pB
��B
�B
��B
�|B
װB
��B
��B
դB
�;B
�B
�pB
�AB
�;B
֪B
դB
�;B
��B
ԞB
�cB
ӘB
��B
�cB
�cB
�5B
�iB
�cB
ҒB
ыB
��B
�)B
��B
ҒB
�cB
�/B
�)B
кB
ЅB
�QB
��B
�QB
�]B
�/B
�WB
�B
�B
ыB
�B
ԞB
�WB
��B
�B
۔B
�B
�TB
�B
�B
۔B
��B
�B
�WB
�.B
�B
��B
�B
�(B
��B
�kBB�BPBBPBB�B�B�BBBPB!B�B.B�B�B�B�B�B�BB�B\B�B�B:BbB�B4B�BbB.B�B	B�BCBzB%�B#B)3B)�B8�B9aB3<B:�B+?B$IB''B(aB*�B)3B& B(�B.RB.�B.�B5}B3<B7�B6B7�B9aB<
B>�BA)BB/BEvBWGB\�BQWBL8BK2BNyBX�B_yB_�B_�B]8BV�B[�BZ�B��B�wB��B��B�B��B�rB��B�B��B�nB�qB�bB�#B�WB��B�<B�B�B�,B��B�B�B�~B�/B�
B� B�gB��B�dB�iB�BդB�B�B�B�B��B�BާB��B��B�B��B�B�5B�;B�BέB�iB�GB�lB��B��B�iB�VB��B��B��BCB�_B	@B=BRBhB9BnB�B'BwB �B�B"qB!�B�B-�B+B"B&UB"<B#wB"<B+�B$IB$}B#BB%�B&UB&�BBXB"�B#�B%�B�B�B'�B*B"�B 0B'�BB �B#BBLB�B�B$B)3BXB �B�BBRB�BBtB�B�BB�B �B�B�B9B�BLBhB"�B�BUB�B�B}B}BwB�B�B�BCB�B�B7B�BB�B�B*B�BkB*B
FB�B�B	�BB�B�BbB!BIBB�BVB�B!B'B�B�B�1B�1B�+B��B�_B�+B�+B��B��B��B�+B��B��B�YB��B��B��B��B��B��B�GB��B��B��B�"B�B�~B�B�B�lB�B�B�fB�B�B�1B�B��B�+B�YB�B��B�YB��B��B�SB�B�iB�B�AB�B��B� B�DB�BާB�B�fB��B�NBդB��B�NB�vB��B֪BҒB�QB��B�B�yB�>B��B̡B�2B˛B̡B��B� B�NB�B�B�vB��B�|B�jB�jBūB�BB��B�5B�)B��B�^B�)B�WB�^B�^B��B��B��B�sB��B��B��B��B�HB��B��B�NB�vB�}B�HB��B�B�aB��B��B�RB�zB��B��B�gB�3B�9B�B��B��B�B��B��B�9B�9B��B�gB��B��B��B��B��B��B�3B�3B�[B�aB�gB��B�-B�aB�'B��B�3B�'B�'B��B� B�B��B� B��B��B�B�0B�dB��B��B��B��B�6B�B�-B��B�7B��B�{B�:B��B�4B��B�PB��B�B�xB{�BzMBy{BzMB{�By�Bz�Bu�Bs�Bt�BrPBo�Bp�BpxBm�BpxBn�Bk�Bl�Bk�Bj�Bi�Bh�BfoBh{Bf�Bi�Bd�B`�B^>B[�BZ�BYTB^�BZ�BX�BV�BT�BT�BR�BR)BR�BOBOBNBK�BG�BH�BG�BG�BF|BCjBB�BCjB?�B?�B?�BDB@�B=B;�B<
B8�B7�B4vB6�B5�B3B4BB3<B1�B2�B0�B0)B1�B/�B.�B.�B,EB+�B/XB.�B,B-�B*�B'�B'�B#BB%B%OB �B�BRBLBtB�B�B�B�B�BOBB�B�B!B�BCBB rBxB
�1B�B �B
�B
��B
��B
�@B
�%B
��B
�B
�B
��BCB
�(B
�WB
ܛB
�%B
�]B
�;B
�5G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230630090058                            20230630090058AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023063009005820230630090058  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023063009005820230630090058QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023063009005820230630090058QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               