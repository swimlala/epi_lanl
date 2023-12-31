CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-12-11T17:09:22Z creation; 2021-03-26T17:01:00Z DMQC;      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20201211170922  20210326170210  4903020 4903020 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               ;   ;AA  AOAO7836_008777_059                 7836_008777_059                 2C  2C  DD  SOLO_II                         SOLO_II                         8777                            8777                            V2.6; SBE602 19Apr19            V2.6; SBE602 19Apr19            853 853 @�N)̍�@�N)̍�11  @�N*!�R�@�N*!�R�@;�+��a@;�+��a�d�Se�X�d�Se�X11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?��H@:�H@}p�@��R@��R@޸R@�p�A  A!G�A-p�A@  A^�RA\)A�Q�A�  A�Q�A�  A�\)A�  A�  A��B(�B�
B�
B�
B'�
B/�
B7�
B@  BH  BP(�BX  B`  Bg�
Bo�
Bx  B�  B��B��B��B��B�  B�{B��B��
B��B��B�  B��B�  B�  B��B��B�{B�(�B�  B�  B�  B�  B�{B�  B�  B�  B�  B�(�B�=qB�{B��B��C  C  C
=C
=C
  C��C
=C
=C�C�HC  C  C��C  C
=C��C!�C#�C&  C(  C)��C,
=C.{C0{C2
=C4
=C5�C7��C:  C<  C=��C?��CB  CD  CF  CH
=CJ
=CL{CN  CP
=CR{CS��CU�CW��CY�C[�HC]�C`  Cb{Cd
=Cf  Ch
=Cj{Cl
=Cn
=Cp  Cr  Ct{Cv{Cx  Cz  C|{C~
=C�  C���C���C�  C�  C���C���C�  C�  C���C���C�  C�C�  C�
=C�
=C�  C�  C�  C�  C�C�C�  C�  C�C�C���C���C�  C�C�
=C�  C���C���C�  C�
=C�C���C���C�C�C�C�  C�C�
=C�C�C�  C�  C�  C�C�  C�C�C�  C���C���C�  C�C���C���C���C�  C�  C�  C�  C�C�C�  C�  C�  C�  C�  C�C�C�
=C�C���C���C�  C�C�C�C���C���C�C�  C�  C���C���C�C�
=C�  C���C���C���C�C���C���C�C�
=C�C�  C���C�  C�C�  C�  C���C�  C�C�  C���C�  C�  C���C�  C���C�  C�
=C�  C���C�  C�
=C�  C���C�  C�  D   D � D�D��D  D}qD�qDz�D��D}qD�D� D�RD}qD  D}qD�qD��D	  D	}qD
  D
� D  D�DD}qD�qD}qD�qD� D  D}qD  D� D  D}qD��D}qD  D}qD��D� D�D��D  D��D�D��D  D}qD��D� D  D}qD��D}qD  D��D�D� D  D��D  Dz�D   D � D �qD!}qD"�D"� D#  D#� D$  D$��D$�qD%� D&  D&}qD'  D'��D(D(� D)�D)�D*�D*� D+  D+}qD+�qD,}qD-�D-��D.  D.�D/�D/� D/�qD0� D1  D1}qD1��D2� D3�D3��D4  D4}qD5  D5� D6  D6� D6��D7}qD8  D8� D8�qD9}qD:  D:�D;�D;� D<  D<}qD=  D=� D>  D>}qD?  D?� D@  D@� D@�qDA��DB  DB��DC�DC� DD  DD� DE  DE��DF�DF}qDF�RDG}qDH  DH}qDH�qDI}qDI��DJz�DJ�qDK}qDK�qDLz�DL�qDM� DN  DN}qDN��DOz�DO��DP}qDQ  DQ}qDQ�qDR}qDS�DS�DTDT� DT�qDU��DVDV��DW  DW}qDX  DX��DY�DY��DY�qDZ� D[  D[z�D[�qD\� D\�qD]}qD^�D^� D^�qD_� D`�D`� D`�qDa}qDb  Db��Dc�Dc��Dd  Dd}qDe�De}qDe�qDf}qDg�Dg��Dh  Dh}qDi  Di� Di�qDj� Dk�Dk��Dl  Dl��Dm�Dmz�Dm��DnxRDn�qDo��Dp  Dp}qDq�Dq� Dq��Dr}qDs�Ds��Dt  Dt}qDu  Du��Dv�Dv� Dv�qDw� Dx  Dx� Dy  Dy�DzDz}qDz�qD{� D|  D|��D}�D}��D~�D~}qD~�qD}qD�  D�AHD�� D��HD�  D�=qD�~�D�� D�  D�>�D�~�D�� D�  D�@ D�~�D���D��D�AHD�� D�D�HD�>�D�~�D��qD���D�AHD��HD��HD�HD�@ D��HD�� D�  D�@ D�� D��HD�HD�@ D�}qD�� D��D�AHD�~�D���D��qD�>�D��HD��HD�HD�@ D�� D��HD��D�C�D�� D��qD�  D�@ D�~�D�� D�  D�AHD�� D�� D���D�=qD�}qD�� D�HD�@ D�� D�� D���D�>�D�� D��HD�HD�@ D�� D���D�  D�AHD�~�D�� D��D�@ D�~�D���D���D�>�D�� D��HD�HD�AHD��HD�� D���D�@ D�~�D�� D�HD�@ D�~�D�� D�  D�@ D�~�D��HD�  D�@ D�� D�� D�  D�>�D�~�D�� D���D�=qD�� D��HD�HD�@ D�� D�� D�HD�B�D��HD��HD�  D�AHD���D�D�HD�>�D�~�D�� D�  D�@ D�� D��HD�HD�@ D�� D���D���D�@ D���D�D�HD�@ D�� D�� D�  D�@ D�� D�� D���D�=qD�~�D���D���D�AHD���D�D���D�@ D���D��HD��D�B�D��HD��HD�  D�AHD���D���D��D�@ D�~�D��qD��qD�=qD�~�D���D�HD�@ D�~�D���D�  D�@ D�� D�D�HD�AHD�}qD�� D�HD�AHD���D�D��D�C�D�� D��qD��qD�@ D�� D���D�HD�B�D��HD�� D�  D�=qD�~�D��HD��D�@ D�}qD�� D�HD�>�D�~�D��qD��)D�<)D�~�D�� D�  D�>�D�}qD��qD���D�@ D�� D��HD�  D�AHD�� D�� D�  D�>�D�� D�� D�  D�@ D�� D���D�  D�B�D��HD�D�HD�@ D�� D�� D��D�AHD�~�D��HD��D�AHD�}qD�� D�HD�AHD D½qD��qD�>�D�}qD�� D�HD�AHDĀ Dľ�D�  D�>�Dŀ D�� D���D�@ D�~�D�� D�  D�AHDǀ DǾ�D�  D�>�D�~�DȽqD�  D�AHDɁHD�� D���D�>�D�~�D�� D�  D�@ DˁHD�D�HD�@ D́HD̾�D���D�AHD́HD��HD��D�@ D�}qDξ�D�  D�AHDρHDϾ�D�  D�@ D�}qDо�D�HD�B�Dр DѾ�D��qD�>�DҀ D�� D���D�=qDӀ D�D�HD�@ D�~�DԾ�D���D�=qD�~�D�� D���D�AHDր D־�D�HD�AHDׂ�D�� D�HD�AHD؁HD��HD�  D�>�Dـ D��HD�  D�@ Dڀ D�� D���D�>�Dۀ D���D�  D�@ D܂�D��HD�  D�AHD݁HDݽqD��qD�@ DށHD��HD�  D�@ D߁HD��HD�  D�@ D�~�DྸD���D�@ D�~�D�qD�  D�AHD�HD�D�HD�@ D� D�� D���D�@ D䂏D��HD�  D�@ D�~�D�qD�  D�AHD�HD�� D�  D�AHD�~�D�� D�HD�AHD�~�D�qD�  D�@ D�}qD�)D���D�@ D�~�D꾸D�HD�>�D�}qD뾸D�  D�AHD� D쾸D�  D�>�D� D��HD�  D�AHD� D��HD�HD�AHD� D�� D�  D�AHD��HD�� D���D�>�D� D��HD�  D�@ D� D�D�HD�AHD�~�D�D�  D�=qD�}qD���D���D�=qD�� D�� D��qD�>�D�� D���D�  D�AHD�� D�� D�HD�@ D�� D��HD�  D�AHD���D�D�HD�B�>�?L��?�  ?��
?���?�ff?�@�@��@(��@:�H@O\)@aG�@n{@xQ�@��
@�{@�
=@�G�@��@���@�@��R@���@�33@�(�@��
@���@�33@��HA�AQ�Ap�A�\AffA�HA{A!�A'
=A,(�A1G�A7
=A<(�AAG�AEAI��AN{AR�\AVffA\(�AaG�AfffAk�Ap  Atz�Ax��A}p�A�Q�A�33A��A�  A�=qA�p�A��A�G�A��A�A�Q�A�=qA�z�A�
=A��A�(�A��RA���A��A�{A�  A��A��
A�{A���A��A�ffA���A�=qA�(�A�{A���A˅A�{AУ�Aҏ\A�z�A�ffA�Q�A��HA�p�A�Q�A��HA��A�
=A���A�33A��A�
=A�G�A��
A�ffA�Q�A��\A��
A�ffB   B ��B�B33BQ�B��B�RB\)B(�B	�B
=qB�B��BB�\B�BQ�BG�B{B
=BQ�B��B�RB�
B��B��B�\B�BQ�B��B�HB (�B!�B"=qB#
=B$(�B%�B&{B'
=B((�B)G�B*ffB+�B,��B-�B.�HB0(�B1G�B2=qB333B4  B4��B5B6�RB8  B9�B:=qB;\)B<z�B=��B>�\B?�B@Q�BAp�BBffBC33BD(�BEp�BF�\BG�
BI�BJffBK�BL��BM��BN�\BO�
BP��BQBR�HBT(�BUG�BV�\BW�BX��BYBZ�\B[�B\��B]��B^�\B_�B`��Ba�Bc33Bdz�Be��Bf�RBg�
Bh��Bi�Bj�HBl  Bl��Bn=qBo
=Bp  BqG�BrffBs�Bt��Bu�Bw
=Bx(�Byp�BzffB{\)B|Q�B}G�B~=qB33B�{B��RB�G�B��
B�ffB���B��B�{B���B�33B��B�=qB���B�33B��B�(�B���B��B��B�(�B���B�G�B�B�Q�B��HB�\)B�  B�z�B�
=B�\)B��
B�=qB��RB�33B���B�{B��\B�
=B�p�B��B�ffB��HB�\)B��B�ffB���B�p�B��B�ffB��HB�p�B��B�ffB��HB�\)B��B�ffB���B��B�  B��\B�
=B���B�(�B��RB�33B��
B�Q�B��HB�p�B�  B�z�B�
=B��B�(�B���B��B��B�=qB��RB�G�B�B�Q�B���B�\)B��B�ffB���B�p�B��B�ffB���B�\)B��B�ffB�
=B���B�=qB���B�p�B�  B��\B�G�B��
B�ffB�
=B���B�(�B���B�\)B��
B�ffB��HB�\)B��B�z�B�33B��
B�z�B�
=B��B�=qB��RB�G�B�B�=qB���B�\)B�  Bģ�B��B��
BƏ\B��BǙ�B�{Bȣ�B�33B��
B�ffB��B�B�ffB�
=B͙�B�{BθRB�G�B��
B�Q�B���BѮB�=qB���Bә�B�(�Bԏ\B�
=Bՙ�B�=qB���Bי�B�=qB��HB�p�B�  B�z�B�
=Bۙ�B�Q�B���Bݙ�B�Q�B���B߅B�{B��B�G�B��
B�ffB���B�B�Q�B��HB噚B�Q�B�
=B�B�Q�B���B�p�B�{B��B�33B��B�\B�33B��
B�\B�33B��
B��\B�G�B��B�z�B��B�B�Q�B��HB���B�Q�B�
=B���B�{B���B��B�(�B��RB��B�=qB��RB�G�B�{B��HB�p�B��C \)C �RC�CffC��C
=Cz�CC
=CQ�CC�Cz�C�RC  C\)CC33C�CC{C\)C��C(�C�\C�
C	�C	ffC	��C
33C
�\C
�
C�C�C�CG�C�\C�
C33C��C{CQ�C�C��C\)C��C{CQ�C�RC(�CffC�C{Cz�CC{Cz�C��C
=C�C�HC�C�C�CQ�C��C�CQ�CC
=CG�C�RC(�Cp�C�RC{C�C�HC(�Cz�C�HCQ�C��C�HC\)CC
=CQ�C�C{Cz�C�HC 33C p�C �
C!=qC!��C!�HC"33C"��C#
=C#Q�C#��C$  C$p�C$�RC%  C%p�C%��C&
=C&ffC&�
C'(�C'p�C'C(33C(�\C(C)33C)��C)�HC*(�C*�\C+  C+=qC+�\C+��C,Q�C,�\C,��C-\)C-�\C.  C.\)C.��C.��C/ffC/��C0  C0\)C0��C0�C1\)C1��C1��C2\)C2��C2�HC3Q�C3��C3�HC4Q�C4�\C4�C5G�C5�\C6  C6=qC6�\C7  C733C7��C7��C8=qC8��C8�
C9Q�C9�C:  C:33C:��C:�C;=qC;��C;�
C<Q�C<z�C<��C=33C=�\C=�C>33C>��C>�
C?G�C?z�C?�HC@33C@�C@�HCA�CA�\CACB33CBp�CB��CC�CCp�CC��CD{CDp�CD�RCE
=CEffCE�CF{CFQ�CF�CF�CG\)CG�\CG��CH=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                ?�  ?��H@:�H@}p�@��R@��R@޸R@�p�A  A!G�A-p�A@  A^�RA\)A�Q�A�  A�Q�A�  A�\)A�  A�  A��B(�B�
B�
B�
B'�
B/�
B7�
B@  BH  BP(�BX  B`  Bg�
Bo�
Bx  B�  B��B��B��B��B�  B�{B��B��
B��B��B�  B��B�  B�  B��B��B�{B�(�B�  B�  B�  B�  B�{B�  B�  B�  B�  B�(�B�=qB�{B��B��C  C  C
=C
=C
  C��C
=C
=C�C�HC  C  C��C  C
=C��C!�C#�C&  C(  C)��C,
=C.{C0{C2
=C4
=C5�C7��C:  C<  C=��C?��CB  CD  CF  CH
=CJ
=CL{CN  CP
=CR{CS��CU�CW��CY�C[�HC]�C`  Cb{Cd
=Cf  Ch
=Cj{Cl
=Cn
=Cp  Cr  Ct{Cv{Cx  Cz  C|{C~
=C�  C���C���C�  C�  C���C���C�  C�  C���C���C�  C�C�  C�
=C�
=C�  C�  C�  C�  C�C�C�  C�  C�C�C���C���C�  C�C�
=C�  C���C���C�  C�
=C�C���C���C�C�C�C�  C�C�
=C�C�C�  C�  C�  C�C�  C�C�C�  C���C���C�  C�C���C���C���C�  C�  C�  C�  C�C�C�  C�  C�  C�  C�  C�C�C�
=C�C���C���C�  C�C�C�C���C���C�C�  C�  C���C���C�C�
=C�  C���C���C���C�C���C���C�C�
=C�C�  C���C�  C�C�  C�  C���C�  C�C�  C���C�  C�  C���C�  C���C�  C�
=C�  C���C�  C�
=C�  C���C�  C�  D   D � D�D��D  D}qD�qDz�D��D}qD�D� D�RD}qD  D}qD�qD��D	  D	}qD
  D
� D  D�DD}qD�qD}qD�qD� D  D}qD  D� D  D}qD��D}qD  D}qD��D� D�D��D  D��D�D��D  D}qD��D� D  D}qD��D}qD  D��D�D� D  D��D  Dz�D   D � D �qD!}qD"�D"� D#  D#� D$  D$��D$�qD%� D&  D&}qD'  D'��D(D(� D)�D)�D*�D*� D+  D+}qD+�qD,}qD-�D-��D.  D.�D/�D/� D/�qD0� D1  D1}qD1��D2� D3�D3��D4  D4}qD5  D5� D6  D6� D6��D7}qD8  D8� D8�qD9}qD:  D:�D;�D;� D<  D<}qD=  D=� D>  D>}qD?  D?� D@  D@� D@�qDA��DB  DB��DC�DC� DD  DD� DE  DE��DF�DF}qDF�RDG}qDH  DH}qDH�qDI}qDI��DJz�DJ�qDK}qDK�qDLz�DL�qDM� DN  DN}qDN��DOz�DO��DP}qDQ  DQ}qDQ�qDR}qDS�DS�DTDT� DT�qDU��DVDV��DW  DW}qDX  DX��DY�DY��DY�qDZ� D[  D[z�D[�qD\� D\�qD]}qD^�D^� D^�qD_� D`�D`� D`�qDa}qDb  Db��Dc�Dc��Dd  Dd}qDe�De}qDe�qDf}qDg�Dg��Dh  Dh}qDi  Di� Di�qDj� Dk�Dk��Dl  Dl��Dm�Dmz�Dm��DnxRDn�qDo��Dp  Dp}qDq�Dq� Dq��Dr}qDs�Ds��Dt  Dt}qDu  Du��Dv�Dv� Dv�qDw� Dx  Dx� Dy  Dy�DzDz}qDz�qD{� D|  D|��D}�D}��D~�D~}qD~�qD}qD�  D�AHD�� D��HD�  D�=qD�~�D�� D�  D�>�D�~�D�� D�  D�@ D�~�D���D��D�AHD�� D�D�HD�>�D�~�D��qD���D�AHD��HD��HD�HD�@ D��HD�� D�  D�@ D�� D��HD�HD�@ D�}qD�� D��D�AHD�~�D���D��qD�>�D��HD��HD�HD�@ D�� D��HD��D�C�D�� D��qD�  D�@ D�~�D�� D�  D�AHD�� D�� D���D�=qD�}qD�� D�HD�@ D�� D�� D���D�>�D�� D��HD�HD�@ D�� D���D�  D�AHD�~�D�� D��D�@ D�~�D���D���D�>�D�� D��HD�HD�AHD��HD�� D���D�@ D�~�D�� D�HD�@ D�~�D�� D�  D�@ D�~�D��HD�  D�@ D�� D�� D�  D�>�D�~�D�� D���D�=qD�� D��HD�HD�@ D�� D�� D�HD�B�D��HD��HD�  D�AHD���D�D�HD�>�D�~�D�� D�  D�@ D�� D��HD�HD�@ D�� D���D���D�@ D���D�D�HD�@ D�� D�� D�  D�@ D�� D�� D���D�=qD�~�D���D���D�AHD���D�D���D�@ D���D��HD��D�B�D��HD��HD�  D�AHD���D���D��D�@ D�~�D��qD��qD�=qD�~�D���D�HD�@ D�~�D���D�  D�@ D�� D�D�HD�AHD�}qD�� D�HD�AHD���D�D��D�C�D�� D��qD��qD�@ D�� D���D�HD�B�D��HD�� D�  D�=qD�~�D��HD��D�@ D�}qD�� D�HD�>�D�~�D��qD��)D�<)D�~�D�� D�  D�>�D�}qD��qD���D�@ D�� D��HD�  D�AHD�� D�� D�  D�>�D�� D�� D�  D�@ D�� D���D�  D�B�D��HD�D�HD�@ D�� D�� D��D�AHD�~�D��HD��D�AHD�}qD�� D�HD�AHD D½qD��qD�>�D�}qD�� D�HD�AHDĀ Dľ�D�  D�>�Dŀ D�� D���D�@ D�~�D�� D�  D�AHDǀ DǾ�D�  D�>�D�~�DȽqD�  D�AHDɁHD�� D���D�>�D�~�D�� D�  D�@ DˁHD�D�HD�@ D́HD̾�D���D�AHD́HD��HD��D�@ D�}qDξ�D�  D�AHDρHDϾ�D�  D�@ D�}qDо�D�HD�B�Dр DѾ�D��qD�>�DҀ D�� D���D�=qDӀ D�D�HD�@ D�~�DԾ�D���D�=qD�~�D�� D���D�AHDր D־�D�HD�AHDׂ�D�� D�HD�AHD؁HD��HD�  D�>�Dـ D��HD�  D�@ Dڀ D�� D���D�>�Dۀ D���D�  D�@ D܂�D��HD�  D�AHD݁HDݽqD��qD�@ DށHD��HD�  D�@ D߁HD��HD�  D�@ D�~�DྸD���D�@ D�~�D�qD�  D�AHD�HD�D�HD�@ D� D�� D���D�@ D䂏D��HD�  D�@ D�~�D�qD�  D�AHD�HD�� D�  D�AHD�~�D�� D�HD�AHD�~�D�qD�  D�@ D�}qD�)D���D�@ D�~�D꾸D�HD�>�D�}qD뾸D�  D�AHD� D쾸D�  D�>�D� D��HD�  D�AHD� D��HD�HD�AHD� D�� D�  D�AHD��HD�� D���D�>�D� D��HD�  D�@ D� D�D�HD�AHD�~�D�D�  D�=qD�}qD���D���D�=qD�� D�� D��qD�>�D�� D���D�  D�AHD�� D�� D�HD�@ D�� D��HD�  D�AHD���D�D�HG�O�>�?L��?�  ?��
?���?�ff?�@�@��@(��@:�H@O\)@aG�@n{@xQ�@��
@�{@�
=@�G�@��@���@�@��R@���@�33@�(�@��
@���@�33@��HA�AQ�Ap�A�\AffA�HA{A!�A'
=A,(�A1G�A7
=A<(�AAG�AEAI��AN{AR�\AVffA\(�AaG�AfffAk�Ap  Atz�Ax��A}p�A�Q�A�33A��A�  A�=qA�p�A��A�G�A��A�A�Q�A�=qA�z�A�
=A��A�(�A��RA���A��A�{A�  A��A��
A�{A���A��A�ffA���A�=qA�(�A�{A���A˅A�{AУ�Aҏ\A�z�A�ffA�Q�A��HA�p�A�Q�A��HA��A�
=A���A�33A��A�
=A�G�A��
A�ffA�Q�A��\A��
A�ffB   B ��B�B33BQ�B��B�RB\)B(�B	�B
=qB�B��BB�\B�BQ�BG�B{B
=BQ�B��B�RB�
B��B��B�\B�BQ�B��B�HB (�B!�B"=qB#
=B$(�B%�B&{B'
=B((�B)G�B*ffB+�B,��B-�B.�HB0(�B1G�B2=qB333B4  B4��B5B6�RB8  B9�B:=qB;\)B<z�B=��B>�\B?�B@Q�BAp�BBffBC33BD(�BEp�BF�\BG�
BI�BJffBK�BL��BM��BN�\BO�
BP��BQBR�HBT(�BUG�BV�\BW�BX��BYBZ�\B[�B\��B]��B^�\B_�B`��Ba�Bc33Bdz�Be��Bf�RBg�
Bh��Bi�Bj�HBl  Bl��Bn=qBo
=Bp  BqG�BrffBs�Bt��Bu�Bw
=Bx(�Byp�BzffB{\)B|Q�B}G�B~=qB33B�{B��RB�G�B��
B�ffB���B��B�{B���B�33B��B�=qB���B�33B��B�(�B���B��B��B�(�B���B�G�B�B�Q�B��HB�\)B�  B�z�B�
=B�\)B��
B�=qB��RB�33B���B�{B��\B�
=B�p�B��B�ffB��HB�\)B��B�ffB���B�p�B��B�ffB��HB�p�B��B�ffB��HB�\)B��B�ffB���B��B�  B��\B�
=B���B�(�B��RB�33B��
B�Q�B��HB�p�B�  B�z�B�
=B��B�(�B���B��B��B�=qB��RB�G�B�B�Q�B���B�\)B��B�ffB���B�p�B��B�ffB���B�\)B��B�ffB�
=B���B�=qB���B�p�B�  B��\B�G�B��
B�ffB�
=B���B�(�B���B�\)B��
B�ffB��HB�\)B��B�z�B�33B��
B�z�B�
=B��B�=qB��RB�G�B�B�=qB���B�\)B�  Bģ�B��B��
BƏ\B��BǙ�B�{Bȣ�B�33B��
B�ffB��B�B�ffB�
=B͙�B�{BθRB�G�B��
B�Q�B���BѮB�=qB���Bә�B�(�Bԏ\B�
=Bՙ�B�=qB���Bי�B�=qB��HB�p�B�  B�z�B�
=Bۙ�B�Q�B���Bݙ�B�Q�B���B߅B�{B��B�G�B��
B�ffB���B�B�Q�B��HB噚B�Q�B�
=B�B�Q�B���B�p�B�{B��B�33B��B�\B�33B��
B�\B�33B��
B��\B�G�B��B�z�B��B�B�Q�B��HB���B�Q�B�
=B���B�{B���B��B�(�B��RB��B�=qB��RB�G�B�{B��HB�p�B��C \)C �RC�CffC��C
=Cz�CC
=CQ�CC�Cz�C�RC  C\)CC33C�CC{C\)C��C(�C�\C�
C	�C	ffC	��C
33C
�\C
�
C�C�C�CG�C�\C�
C33C��C{CQ�C�C��C\)C��C{CQ�C�RC(�CffC�C{Cz�CC{Cz�C��C
=C�C�HC�C�C�CQ�C��C�CQ�CC
=CG�C�RC(�Cp�C�RC{C�C�HC(�Cz�C�HCQ�C��C�HC\)CC
=CQ�C�C{Cz�C�HC 33C p�C �
C!=qC!��C!�HC"33C"��C#
=C#Q�C#��C$  C$p�C$�RC%  C%p�C%��C&
=C&ffC&�
C'(�C'p�C'C(33C(�\C(C)33C)��C)�HC*(�C*�\C+  C+=qC+�\C+��C,Q�C,�\C,��C-\)C-�\C.  C.\)C.��C.��C/ffC/��C0  C0\)C0��C0�C1\)C1��C1��C2\)C2��C2�HC3Q�C3��C3�HC4Q�C4�\C4�C5G�C5�\C6  C6=qC6�\C7  C733C7��C7��C8=qC8��C8�
C9Q�C9�C:  C:33C:��C:�C;=qC;��C;�
C<Q�C<z�C<��C=33C=�\C=�C>33C>��C>�
C?G�C?z�C?�HC@33C@�C@�HCA�CA�\CACB33CBp�CB��CC�CCp�CC��CD{CDp�CD�RCE
=CEffCE�CF{CFQ�CF�CF�CG\)CG�\CG��CH=qG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�VG�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��jA�ƨA�ĜA�ƨA�ȴA�ƨA�ȴA�ȴA�ȴA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ƨA��^A�ƨA��jA��A��uA�n�A�K�A�A�A�?}A�"�A��
A��wA�A�A�  A�I�A��A��mA�JA���A��A��#A��A��9A�S�A�JA��A���A�$�A��A�I�A�O�A��A�XA�I�A��-A�XA���A�dZA��A��A��HA�C�A�z�A�E�A��TA�5?A�r�A�"�A���A�G�A�{A���A��PA�/A��A�ĜA��A�=qA���A���A�\)A��A�bA��DA��`A��uA�G�A��+A�"�Al�A~�A}��A}l�A|jAz�9Aw�At(�ArQ�Aq/Ao;dAnE�Am�PAlA�AkXAj�Ah��AfA�Ae33Ad�Acx�AbȴAbbAa��Aa�Aa/A`Q�A_
=A]��A\�AZ��AY��AY��AY��AYp�AX�AXffAXbAW�
AV�AV-AU�^AU�PAUG�AT�AS�AR�yARȴAQ��AP(�ANn�AL�jALJAK/AJ�HAJbNAI�-AHn�AF�AF��AFbAE33AD(�AB��AB �AA�7AAG�AA�A@v�A?�TA?"�A>-A=x�A<ĜA<�RA<��A<VA<I�A;��A:ĜA9�#A8��A7��A6��A6{A5?}A2��A/��A/G�A.�DA.-A-K�A+|�A*bA)oA(z�A'��A&�A%S�A$�`A$�!A$�+A#�wA#K�A"�HA!�TA ��A�A&�A�jA1A33AVA�;A��AZA��A+A�jA�At�A�\A-A��A�hAG�A��AffA��Al�A�A��Av�A$�A��AdZA;dA��A��A�`A�uA�A�A�#A�AXA
z�A	�A~�A�wA`BA�yAffAVA�;A�Az�A��A Q�@��@���@��`@�Q�@��m@�@��@���@�w@�33@���@�+@���@�^@�1'@�E�@�+@��T@�O�@���@��@柾@���@�@�h@�?}@�%@���@�F@�X@�n�@�O�@�I�@�=q@�1'@֗�@ՙ�@ԛ�@�
=@Ѻ^@�|�@���@�&�@�Ĝ@˥�@�E�@��@ȋD@�1'@���@Ǖ�@���@��/@ě�@�Q�@��@öF@§�@��#@��9@�1'@�1@���@�K�@���@�E�@��#@��@��@�V@��u@���@���@��@���@��R@�X@��@�b@��F@��P@�33@��H@��R@�^5@��@�7L@�1'@��+@���@�?}@��@���@��@���@��y@��#@�G�@���@�z�@�  @�S�@�ȴ@�^5@��-@�&�@��9@�  @�
=@��!@�~�@�=q@��@��9@�b@�|�@�@��+@�n�@�ff@�E�@���@��-@�V@��D@��@��@�|�@�@�ȴ@���@�{@���@�&�@���@�j@��m@�\)@��y@�~�@���@�`B@���@�z�@�z�@�r�@�r�@�j@�j@�j@�j@�r�@�r�@�Q�@��;@��w@�t�@�;d@�@��@���@���@�n�@�@��-@�hs@�O�@�G�@�?}@��@��/@�b@��P@�|�@�S�@�
=@�=q@���@��^@�?}@��/@��j@���@�z�@�1'@��
@���@�;d@���@���@�-@��#@��@�7L@�Ĝ@��@��@��@���@�j@�I�@�1'@�  @��m@�ƨ@���@�o@���@��+@�n�@�{@���@��7@�`B@�`B@�O�@�7L@��`@��9@��@�z�@�Z@�I�@��@�1@�1@�1@l�@~�+@}�h@|I�@{�m@{�
@{ƨ@{t�@z��@zn�@y�#@yX@y&�@y�@y%@xĜ@xbN@w�@vff@u�@uO�@t�D@t(�@s��@s@r��@r�\@r-@qX@p��@pbN@pb@o��@o\)@n��@nv�@nE�@n@mO�@l�/@lZ@k�@k33@k"�@ko@ko@j�@j��@jn�@j=q@jJ@i��@iX@i7L@i&�@h��@h�9@hr�@g�@g;d@f��@fE�@f5?@f5?@f{@e@e�h@e�@e/@dz�@d1@c�
@c��@ct�@cdZ@c"�@c@b��@b�@a�^@ahs@`��@_�@_+@^�R@^$�@]��@\Z@[�@[o@Z=q@ZJ@Y�@Y��@Y7L@X�`@X �@W��@V�@V{@U?}@T�/@Tz�@T(�@S��@Sƨ@S��@SS�@S"�@So@R�H@R�!@R�!@R��@R�\@Rn�@RJ@QG�@Q%@P��@P��@P��@P��@P��@P�`@P��@P�u@Pr�@P1'@O�@O�P@Ol�@N��@Nff@M��@Mp�@M/@L�@L�j@Lz�@Lz�@LZ@LZ@LI�@L1@Kt�@Ko@J��@Jn�@J=q@JM�@JM�@I��@I��@Ihs@IX@I&�@H��@HA�@G��@G|�@G;d@G;d@G;d@G\)@G+@Fȴ@F��@F�+@Fv�@FV@Fff@Fff@FV@E�@E�-@E��@EO�@EO�@E�@D�D@Dj@DI�@C�m@C��@CdZ@C@B�!@BM�@B-@A��@A�^@A��@AX@@��@@bN@@  @?��@?��@?|�@?l�@?l�@?�@>��@>V@>$�@>@=�@=��@=�-@=p�@=/@=V@<��@<�@<z�@<I�@<(�@<1@<1@;��@;ƨ@;ƨ@;�F@;t�@;"�@:�@:��@:=q@:�@9��@9�@9�@9�#@9��@9�^@9��@97L@8Ĝ@8r�@8Q�@8b@7�;@7�w@7\)@6��@6��@6ff@6V@6V@5�@5��@5/@4��@4��@4��@4��@4j@49X@3ƨ@2�@2�\@2n�@2M�@2=q@2=q@1��@1��@1hs@1G�@1�@0�9@0A�@0  @/��@/��@/K�@/K�@.��@.ff@.@-�T@-�@-O�@-/@-�@-V@,�@,�@,�/@,�j@,��@,�D@,�@+�F@+��@+C�@*��@*M�@*�@*�@*J@)�^@)hs@)G�@)&�@)%@(Ĝ@(bN@( �@(  @'�;@'��@&�y@&�+@&v�@&ff@&5?@&$�@%��@%�@$��@$�@$z�@$�@#t�@#dZ@#C�@#"�@#@"��@"��@"��@"�!@"�!@"��@"~�@"n�@"�@!x�@!G�@!G�@!G�@!X@!7L@ ��@ �9@ �@ A�@ 1'@   @�w@l�@ȴ@v�@5?@@��@��@�D@9X@��@"�@�!@�\@n�@��@hs@7L@%@�9@r�@1'@�w@l�@\)@K�@;d@+@+@�@��@ff@$�@{@{@��@��@p�@p�@`B@�@�/@j@Z@(�@�@�
@ƨ@��@��@�@S�@33@@��@~�@�^@x�@&�@��@��@Ĝ@Ĝ@Ĝ@Ĝ@Ĝ@��@bN@Q�@A�@  @�;@�w@��@l�@K�@K�@;d@;d@+@�@
=@��@��@�@�@��@��@��@z�@z�@j@Z@9X@�@1@�m@�m@�F@S�@@
n�@
M�@
M�@
M�@
=q@
-@	�#@	��@	x�@	hs@	hs@	G�@	7L@	%@��@�`@Ĝ@�u@Q�@A�@ �@b@�;@�@|�@\)@K�@K�@+@
=@��@��@��@�@ȴ@��@�+@ff@E�@$�@�@�T@@��@�h@�@p�@O�@�@V@��@�D@j@I�@1@�m@��@��@t�@S�@S�@o@�H@�H@��@��@��@�!@~�@~�A��9A��jA��jA��FA���A���A�ȴA�ĜA�ĜA�A�ĜA�ĜA�ƨA�ȴA�ƨA�ƨA�ƨA�ƨA�ȴA���A�ȴA�ƨA�ĜA�ĜA�ƨA���A���A���A�ȴA�ƨA�ĜA�ƨA�ȴA���A���A���A�ȴA�ƨA�ƨA�ƨA�ƨA�ƨA�ȴA���A���A���A���A���A�ƨA�ƨA�ƨA���A���A���A���A���A���A���A�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A���A���A���A���A���A���A��
A���A���A���A���A���A���A��
A��
A��
A��
A���A���A���A���A���A���A���A���A���A���A���A���A��
A��
A��A��A��
A��A���A���A���A���A���A���A���A��A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��
A��
A��
A���A���A���A���A���A���A���A���A��
A��
A���A��jA���A��jA��^A��FA��A��9A��^A��jA���A�ĜA�ĜA�ȴA���A���A�ƨA���A�ƨA�ȴA���A��^A��A��A��wA��jA�A��A��9A��!A��-A��!A���A���A���A���A���A���A��\A��7A��+A��A��A�t�A�r�A�ffA�\)A�`BA�\)A�S�A�M�A�M�A�K�A�I�A�I�A�I�A�G�A�C�A�A�A�?}A�?}A�?}A�?}A�?}A�?}A�A�A�A�A�A�A�A�A�?}A�?}A�=qA�;dA�1'A��A��A��A��A�%A��HA��/A��
A��
A���A���A���A���A�ȴA�ƨA�ƨA�ĜA�A��wA��9A���A���A���A���A��PA�~�A�M�A��A��hA�VA���A��A�Q�A�A�O�A�(�A�VA��`A��uA��;A���A�M�A�l�A��A�l�A�&�A���A��`A��;A���A��9A���A���A��hA��A�\)A�5?A��A�ffA���A�S�A��A���A�33A�JA��RA�v�A��A��A�9XA�+A���A�A�A�9XA�{A�A��uA�v�A�&�A��RA���A�x�A�{A��7A�bA��^A�t�A�\)A�O�A�A�A�+A�&�A� �A��A��A��A�{A�1A��`A�ȴA���A��7A�p�A�n�A�hsA�`BA�\)A�XA�O�A�A�A�9XA�/A�&�A��A�JA���A��A��yA��;A�ȴA���A��RA��-A��A�I�A��A���A���A�A��!A��PA�t�A�Q�A�7LA�$�A�oA���A��A��
A��jA���A��\A�jA�9XA��A��A��9A��A�VA��A��A��\A�n�A�VA�E�A�C�A�A�A�A�A�=qA�/A�&�A��A��A�VA���A�VA��A�I�A��A��#A�A��A��+A�`BA�9XA��A�A��A��
A��wA��A���A��hA�~�A�v�A�l�A�^5A�VA�I�A�9XA�-A� �A�bA��A��
A��jA���A��7A�t�A�^5A�C�A�7LA�$�A��A�VA��A���A��wA���A�~�A�\)A��A��;A��\A�C�A��A���A���A��!A���A�~�A�z�A�bNA�33A�{A�JA���A��/A���A���A�O�A��FA�x�A�XA�M�A�E�A�7LA� �A���A��A��A��yA��HA���A�jA�`BA�E�A�7LA�bA�ƨA���A��PA�v�A�^5A�C�A�A�A�E�A�7LA�(�A�JA���A��A��A���A�~�A�r�A�jA�dZA�bNA�M�A�;dA�-A��A���A�VA��mA��A���A���A���A�ĜA���A��FA��-A��!A���A���A��PA��A�K�A�7LA�1'A�-A�(�A�$�A�$�A�$�A��A��A��A�1A��`A��A�ȴA��wA��!A���A���A��uA��+A�x�A�p�A�^5A�O�A�A�A�9XA�-A�$�A� �A��A�A��A��TA��#A���A��wA��!A���A��+A��DA��A�l�A�O�A�K�A�E�A�?}A�$�A�"�A�
=A��yA��^A��hA��\A�r�A�JA�A��-A���A���A��uA��PA��+A�x�A�r�A�;dA�1A��mA���A��A�z�A�=qA�VA��HA��hA���A�r�A��A�n�A�$�A��yA���A��jA���A��PA��A�z�A�n�A�^5A�E�A�/A�$�A��A�VA��A��A�FAt�AG�AA~ĜA~~�A~�A~A~  A}�TA}A}�A}��A}��A}�hA}��A}�hA}�A}t�A}p�A}p�A}dZA}\)A}G�A}+A|��A|M�A|  A{��A{C�A{�Az�yAzĜAz�+AzM�Ay�TAy��Ay%Ax-AwC�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                A��jA�ƨA�ĜA�ƨA�ȴA�ƨA�ȴA�ȴA�ȴA�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ƨA��^A�ƨA��jA��A��uA�n�A�K�A�A�A�?}A�"�A��
A��wA�A�A�  A�I�A��A��mA�JA���A��A��#A��A��9A�S�A�JA��A���A�$�A��A�I�A�O�A��A�XA�I�A��-A�XA���A�dZA��A��A��HA�C�A�z�A�E�A��TA�5?A�r�A�"�A���A�G�A�{A���A��PA�/A��A�ĜA��A�=qA���A���A�\)A��A�bA��DA��`A��uA�G�A��+A�"�Al�A~�A}��A}l�A|jAz�9Aw�At(�ArQ�Aq/Ao;dAnE�Am�PAlA�AkXAj�Ah��AfA�Ae33Ad�Acx�AbȴAbbAa��Aa�Aa/A`Q�A_
=A]��A\�AZ��AY��AY��AY��AYp�AX�AXffAXbAW�
AV�AV-AU�^AU�PAUG�AT�AS�AR�yARȴAQ��AP(�ANn�AL�jALJAK/AJ�HAJbNAI�-AHn�AF�AF��AFbAE33AD(�AB��AB �AA�7AAG�AA�A@v�A?�TA?"�A>-A=x�A<ĜA<�RA<��A<VA<I�A;��A:ĜA9�#A8��A7��A6��A6{A5?}A2��A/��A/G�A.�DA.-A-K�A+|�A*bA)oA(z�A'��A&�A%S�A$�`A$�!A$�+A#�wA#K�A"�HA!�TA ��A�A&�A�jA1A33AVA�;A��AZA��A+A�jA�At�A�\A-A��A�hAG�A��AffA��Al�A�A��Av�A$�A��AdZA;dA��A��A�`A�uA�A�A�#A�AXA
z�A	�A~�A�wA`BA�yAffAVA�;A�Az�A��A Q�@��@���@��`@�Q�@��m@�@��@���@�w@�33@���@�+@���@�^@�1'@�E�@�+@��T@�O�@���@��@柾@���@�@�h@�?}@�%@���@�F@�X@�n�@�O�@�I�@�=q@�1'@֗�@ՙ�@ԛ�@�
=@Ѻ^@�|�@���@�&�@�Ĝ@˥�@�E�@��@ȋD@�1'@���@Ǖ�@���@��/@ě�@�Q�@��@öF@§�@��#@��9@�1'@�1@���@�K�@���@�E�@��#@��@��@�V@��u@���@���@��@���@��R@�X@��@�b@��F@��P@�33@��H@��R@�^5@��@�7L@�1'@��+@���@�?}@��@���@��@���@��y@��#@�G�@���@�z�@�  @�S�@�ȴ@�^5@��-@�&�@��9@�  @�
=@��!@�~�@�=q@��@��9@�b@�|�@�@��+@�n�@�ff@�E�@���@��-@�V@��D@��@��@�|�@�@�ȴ@���@�{@���@�&�@���@�j@��m@�\)@��y@�~�@���@�`B@���@�z�@�z�@�r�@�r�@�j@�j@�j@�j@�r�@�r�@�Q�@��;@��w@�t�@�;d@�@��@���@���@�n�@�@��-@�hs@�O�@�G�@�?}@��@��/@�b@��P@�|�@�S�@�
=@�=q@���@��^@�?}@��/@��j@���@�z�@�1'@��
@���@�;d@���@���@�-@��#@��@�7L@�Ĝ@��@��@��@���@�j@�I�@�1'@�  @��m@�ƨ@���@�o@���@��+@�n�@�{@���@��7@�`B@�`B@�O�@�7L@��`@��9@��@�z�@�Z@�I�@��@�1@�1@�1@l�@~�+@}�h@|I�@{�m@{�
@{ƨ@{t�@z��@zn�@y�#@yX@y&�@y�@y%@xĜ@xbN@w�@vff@u�@uO�@t�D@t(�@s��@s@r��@r�\@r-@qX@p��@pbN@pb@o��@o\)@n��@nv�@nE�@n@mO�@l�/@lZ@k�@k33@k"�@ko@ko@j�@j��@jn�@j=q@jJ@i��@iX@i7L@i&�@h��@h�9@hr�@g�@g;d@f��@fE�@f5?@f5?@f{@e@e�h@e�@e/@dz�@d1@c�
@c��@ct�@cdZ@c"�@c@b��@b�@a�^@ahs@`��@_�@_+@^�R@^$�@]��@\Z@[�@[o@Z=q@ZJ@Y�@Y��@Y7L@X�`@X �@W��@V�@V{@U?}@T�/@Tz�@T(�@S��@Sƨ@S��@SS�@S"�@So@R�H@R�!@R�!@R��@R�\@Rn�@RJ@QG�@Q%@P��@P��@P��@P��@P��@P�`@P��@P�u@Pr�@P1'@O�@O�P@Ol�@N��@Nff@M��@Mp�@M/@L�@L�j@Lz�@Lz�@LZ@LZ@LI�@L1@Kt�@Ko@J��@Jn�@J=q@JM�@JM�@I��@I��@Ihs@IX@I&�@H��@HA�@G��@G|�@G;d@G;d@G;d@G\)@G+@Fȴ@F��@F�+@Fv�@FV@Fff@Fff@FV@E�@E�-@E��@EO�@EO�@E�@D�D@Dj@DI�@C�m@C��@CdZ@C@B�!@BM�@B-@A��@A�^@A��@AX@@��@@bN@@  @?��@?��@?|�@?l�@?l�@?�@>��@>V@>$�@>@=�@=��@=�-@=p�@=/@=V@<��@<�@<z�@<I�@<(�@<1@<1@;��@;ƨ@;ƨ@;�F@;t�@;"�@:�@:��@:=q@:�@9��@9�@9�@9�#@9��@9�^@9��@97L@8Ĝ@8r�@8Q�@8b@7�;@7�w@7\)@6��@6��@6ff@6V@6V@5�@5��@5/@4��@4��@4��@4��@4j@49X@3ƨ@2�@2�\@2n�@2M�@2=q@2=q@1��@1��@1hs@1G�@1�@0�9@0A�@0  @/��@/��@/K�@/K�@.��@.ff@.@-�T@-�@-O�@-/@-�@-V@,�@,�@,�/@,�j@,��@,�D@,�@+�F@+��@+C�@*��@*M�@*�@*�@*J@)�^@)hs@)G�@)&�@)%@(Ĝ@(bN@( �@(  @'�;@'��@&�y@&�+@&v�@&ff@&5?@&$�@%��@%�@$��@$�@$z�@$�@#t�@#dZ@#C�@#"�@#@"��@"��@"��@"�!@"�!@"��@"~�@"n�@"�@!x�@!G�@!G�@!G�@!X@!7L@ ��@ �9@ �@ A�@ 1'@   @�w@l�@ȴ@v�@5?@@��@��@�D@9X@��@"�@�!@�\@n�@��@hs@7L@%@�9@r�@1'@�w@l�@\)@K�@;d@+@+@�@��@ff@$�@{@{@��@��@p�@p�@`B@�@�/@j@Z@(�@�@�
@ƨ@��@��@�@S�@33@@��@~�@�^@x�@&�@��@��@Ĝ@Ĝ@Ĝ@Ĝ@Ĝ@��@bN@Q�@A�@  @�;@�w@��@l�@K�@K�@;d@;d@+@�@
=@��@��@�@�@��@��@��@z�@z�@j@Z@9X@�@1@�m@�m@�F@S�@@
n�@
M�@
M�@
M�@
=q@
-@	�#@	��@	x�@	hs@	hs@	G�@	7L@	%@��@�`@Ĝ@�u@Q�@A�@ �@b@�;@�@|�@\)@K�@K�@+@
=@��@��@��@�@ȴ@��@�+@ff@E�@$�@�@�T@@��@�h@�@p�@O�@�@V@��@�D@j@I�@1@�m@��@��@t�@S�@S�@o@�H@�H@��@��@��@�!@~�G�O�A��9A��jA��jA��FA���A���A�ȴA�ĜA�ĜA�A�ĜA�ĜA�ƨA�ȴA�ƨA�ƨA�ƨA�ƨA�ȴA���A�ȴA�ƨA�ĜA�ĜA�ƨA���A���A���A�ȴA�ƨA�ĜA�ƨA�ȴA���A���A���A�ȴA�ƨA�ƨA�ƨA�ƨA�ƨA�ȴA���A���A���A���A���A�ƨA�ƨA�ƨA���A���A���A���A���A���A���A�ȴA�ȴA�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A���A���A���A���A���A���A��
A���A���A���A���A���A���A��
A��
A��
A��
A���A���A���A���A���A���A���A���A���A���A���A���A��
A��
A��A��A��
A��A���A���A���A���A���A���A���A��A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A��
A��
A��
A��
A���A���A���A���A���A���A���A���A��
A��
A���A��jA���A��jA��^A��FA��A��9A��^A��jA���A�ĜA�ĜA�ȴA���A���A�ƨA���A�ƨA�ȴA���A��^A��A��A��wA��jA�A��A��9A��!A��-A��!A���A���A���A���A���A���A��\A��7A��+A��A��A�t�A�r�A�ffA�\)A�`BA�\)A�S�A�M�A�M�A�K�A�I�A�I�A�I�A�G�A�C�A�A�A�?}A�?}A�?}A�?}A�?}A�?}A�A�A�A�A�A�A�A�A�?}A�?}A�=qA�;dA�1'A��A��A��A��A�%A��HA��/A��
A��
A���A���A���A���A�ȴA�ƨA�ƨA�ĜA�A��wA��9A���A���A���A���A��PA�~�A�M�A��A��hA�VA���A��A�Q�A�A�O�A�(�A�VA��`A��uA��;A���A�M�A�l�A��A�l�A�&�A���A��`A��;A���A��9A���A���A��hA��A�\)A�5?A��A�ffA���A�S�A��A���A�33A�JA��RA�v�A��A��A�9XA�+A���A�A�A�9XA�{A�A��uA�v�A�&�A��RA���A�x�A�{A��7A�bA��^A�t�A�\)A�O�A�A�A�+A�&�A� �A��A��A��A�{A�1A��`A�ȴA���A��7A�p�A�n�A�hsA�`BA�\)A�XA�O�A�A�A�9XA�/A�&�A��A�JA���A��A��yA��;A�ȴA���A��RA��-A��A�I�A��A���A���A�A��!A��PA�t�A�Q�A�7LA�$�A�oA���A��A��
A��jA���A��\A�jA�9XA��A��A��9A��A�VA��A��A��\A�n�A�VA�E�A�C�A�A�A�A�A�=qA�/A�&�A��A��A�VA���A�VA��A�I�A��A��#A�A��A��+A�`BA�9XA��A�A��A��
A��wA��A���A��hA�~�A�v�A�l�A�^5A�VA�I�A�9XA�-A� �A�bA��A��
A��jA���A��7A�t�A�^5A�C�A�7LA�$�A��A�VA��A���A��wA���A�~�A�\)A��A��;A��\A�C�A��A���A���A��!A���A�~�A�z�A�bNA�33A�{A�JA���A��/A���A���A�O�A��FA�x�A�XA�M�A�E�A�7LA� �A���A��A��A��yA��HA���A�jA�`BA�E�A�7LA�bA�ƨA���A��PA�v�A�^5A�C�A�A�A�E�A�7LA�(�A�JA���A��A��A���A�~�A�r�A�jA�dZA�bNA�M�A�;dA�-A��A���A�VA��mA��A���A���A���A�ĜA���A��FA��-A��!A���A���A��PA��A�K�A�7LA�1'A�-A�(�A�$�A�$�A�$�A��A��A��A�1A��`A��A�ȴA��wA��!A���A���A��uA��+A�x�A�p�A�^5A�O�A�A�A�9XA�-A�$�A� �A��A�A��A��TA��#A���A��wA��!A���A��+A��DA��A�l�A�O�A�K�A�E�A�?}A�$�A�"�A�
=A��yA��^A��hA��\A�r�A�JA�A��-A���A���A��uA��PA��+A�x�A�r�A�;dA�1A��mA���A��A�z�A�=qA�VA��HA��hA���A�r�A��A�n�A�$�A��yA���A��jA���A��PA��A�z�A�n�A�^5A�E�A�/A�$�A��A�VA��A��A�FAt�AG�AA~ĜA~~�A~�A~A~  A}�TA}A}�A}��A}��A}�hA}��A}�hA}�A}t�A}p�A}p�A}dZA}\)A}G�A}+A|��A|M�A|  A{��A{C�A{�Az�yAzĜAz�+AzM�Ay�TAy��Ay%Ax-AwC�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�BSBB�B�B�BMBMBB�B�BB�B�B{B�BFBB�B�BB�BoBoB�B�B�B B�B@B�B�B@B�B�BkB	B7B�B�B�BSB�B�'B��BGEB:�BBMB�AB��B�,B�,B�6B�B��B��B��B��B��B��B��Bv+B\�BR�BK)BE�B<B3�B+�B1B�B�B�B�)B��B�WB��B�vB��B�dB��B�=B��B��B�B�_B�B��B�	B��B�BuZBh
BaHBS[B8B$�B!B�B�B�BB�(B�/B�BȀB�FB��B��B��B��B��B�B��B{JBl�BbNB^jBX�BVBO�BM�BK^BGzBFtB9�B5�B0�B$�BB�B�B1B�B:B�BB
rB�BBoB
��B
�B
�xB
�|B
�B
�AB
�2B
�BB
��B
�vB
�)B
��B
�B
� B
�qB
�B
��B
��B
��B
��B
�~B
��B
��B
��B
��B
��B
��B
�B
�B
�MB
�iB
cB
~]B
|�B
zB
yrB
r�B
n�B
d�B
`�B
YKB
T�B
QB
K^B
9�B
7B
5B
1'B
-�B
'�B
 �B
�B
�B
�B
oB
fB
1B
�B
GB
�B	��B	��B	�lB	�B	�cB	�QB	��B	�`B	� B	�jB	�]B	�)B	ٴB	�KB	�gB	�,B	��B	��B	�B	�XB	�RB	�B	�B	ĜB	��B	�OB	��B	��B	�dB	��B	��B	�B	�nB	��B	��B	��B	�IB	�B	�kB	�*B	��B	�eB	��B	�'B	��B	��B	�YB	�B	�uB	�B	��B	��B	�DB	�~B	�7B	�lB	�SB	�B	�B	�iB	cB	|PB	|�B	}"B	y	B	v�B	u�B	t�B	s�B	qAB	r�B	p�B	qvB	n�B	m)B	l"B	m]B	l�B	kQB	j�B	jB	i�B	iB	g8B	g�B	h�B	hsB	e,B	e,B	d�B	e�B	c B	c�B	c�B	c�B	e,B	e�B	g8B	d�B	c�B	e�B	e�B	f2B	f2B	e,B	dZB	c�B	h
B	g�B	g8B	g8B	gB	g�B	iB	i�B	kB	kB	j�B	k�B	kQB	kQB	k�B	kQB	m)B	m]B	o�B	r�B	r�B	v+B	y�B	y	B	y�B	{�B	|�B	}�B	}�B	}�B	cB	�B	~�B	�B	.B	�B	��B	�rB	�B	�~B	�JB	�xB	��B	��B	��B	��B	�$B	��B	��B	��B	�CB	�~B	��B	�bB	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�0B	��B	�B	��B	�UB	��B	��B	ÖB	�EB	ɺB	��B	�dB	ΥB	�HB	�B	уB	�aB	�9B	خB	چB	�]B	�B	�B	�&B	��B	�sB	��B	�;B	�B	��B	��B	��B	�B	�AB	��B	�AB	��B	�B	�vB	�B	��B	��B	�lB	�B	�JB	��B	��B	��B
�B
�B
�B
_B
+B
_B
1B
	7B
.B
@B
�B
B
B
B
IB
VB
#nB
&B
&�B
&�B
'�B
*0B
,�B
.}B
0�B
2�B
5B
8RB
:�B
<�B
?}B
B�B
CaB
CaB
CaB
C-B
E�B
F?B
GEB
H�B
IRB
J#B
K�B
O�B
QB
S�B
S�B
V�B
YKB
[�B
\�B
\�B
\�B
]�B
`vB
a�B
a�B
c�B
d�B
d�B
f�B
gmB
g8B
f�B
iDB
l�B
p�B
u�B
w�B
xB
x8B
y�B
|PB
}�B
�B
�{B
�MB
��B
��B
�+B
�1B
��B
�:B
��B
��B
��B
�OB
��B
��B
��B
�B
��B
�B
��B
��B
��B
�aB
��B
�B
�B
��B
�RB
��B
��B
�6B
��B
��B
�B
��B
��B
�UB
��B
ÖB
�3B
ĜB
��B
�zB
�B
�B
��B
ɺB
ʌB
�0B
��B
��B
ԕB
��B
�KB
��B
��B
�B
�B
� B
�B
�8B
�B
�>B
�sB
�B
�B
�KB
��B
��B
� B
�oB
�B
�ZB
�lB
��B
�B
��B iBGBB�B	7B	lB
=BDBJB�B.BB:BoBB�B{BMB�BB�BYB�B�B�B�B�B�BeB�BB�B�B�B�B�B�B�B�B�B�B�B!bB!bB!bB"hB#�B&B&�B'B'�B(�B)*B)*B)*B)*B)*B*eB-CB-wB.IB.�B/OB/B/�B0�B1�B1�B2-B2-B2-B4�B6B6zB7B7�B8RB8RB:�B:�B:^B:�B:�B;�B<jB=B=B=�B>�B?HB?B>�B>�B@�B@OBA�BB[BB�BC-BDgBE9BEBE9BE9BE�BE�BE�BF?BG�BH�BIBI�BJ#BJ#BJ�BLdBNBN�BOBO�BOvBO�BPHBP}BP�BP�BQ�BR BS�BT,BT�BU2BU2BUgBU�BVBU�BVBW?BWsBX�BX�BX�BYBYBYKBYBY�BY�BY�BZ�B[�B\]B\]B\�B]dB]�B^�B_B_�B`BB`BB`BaHBaBb�Bc Bc Bb�BcTBc�Bc�Bd�Bf�BgBgBg8Bg8BgBh
Bh
Bh
Bh>Bh>Bh�BiyBiDBi�Bi�Bi�BiyBj�BjBk�Bl"Bm�Bn�Bn�Bn�Bo BoiBoiBoiBo�BpoBp;Bq�Bq�BrGBr�Bs�Bt�Bt�Bt�Bt�Bu�Bu�Bv+Bv`Bv`Bv�Bw�Bw�BxBx8Bx�BzDBz�BzxBzxBz�BzxB{�B|PB|�B|�B|�B}�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B~�B.B.B�B��B��B��B��B�iB��B�oB�;B��B��B��B�AB�uB��B��B�B�MB�MB��B��B�%B�%B�_B��B��B�fB��B��B��B��B�DB��B��B�JB�B��B��B�PB�PB�PB��B��B��B��B��B��B��B�(B��B�\B�(B��B�\B�\B�.B�.B�bB��B��B� B� B��B��B�4B�4B�hB�hB�B�B�uB��B�B�B�FB�FB�FB�B�B�B�FB��B��B�B�{B��B��B�MB�MB��B��B��B��B��B��B��B��B��B�$B��B��B�_B�_B�_B��B�_B��B��B�1B�eB�eB��B�7B�kB��B��B�qB�qB�qB�qB�B�xB��B�xB��B��B��B�B�B�B�IB��B��B��B�B�B��B��B��B�!B�!B�!B�VB��B��B�VB�VB��B��B��B��B�'B�'B��B��B��B��B��B�-B��B��B�bB��B��B��B�4B�hB�hB��B�B�nB�nB��B��B�nB�@B�@B�@B�@B�tB��B�tB�FB�B�BFB�B�BB�B�B�B�B$B�B�BBMB�BSBB�BMB�B�B�B�BSB�B�BFBBMB�BSB�BB{B�B�B�B�B�B�B�B�B�BBFB{B�B�BSB�B�BFBBBB�BuBFB�BMB�B�BB�B�B�B�B@BB{BB�B{B�B�BFBB@BB{B�BB�B�B:BuBFB{B{BFB@B�B�B�BB@B{B�B�BB�B�B�BuBuB�B�B�B@B:BB�B:B�B�BB@BuB�BhB�B�BuB�B@B@B:BhBhB B�B�B@B:B:BoB�B BhB�B:BBoB�B�B4B�BhBhB B�BoBoBoB�B:B:BB�B B�B�B�B�B4B�B4B:B B4B�B�B(B�B@BB{B�BSB�B�BBSB�B�B:BB4B�BbB:B@B B�B�BB�BSB BuB�BYB�B�BMB�B+BYB�B�BSB�B+B�BeB�B�B7B�B~B�B=BBBxB�B�B�B7B�BkB�B7B�B�B	B7B7BBeB�B_B+B�BYB�BSB�B{BB�B�BuB4B�B�B�B�B~B
rB	�B�B�B�BB�B�BGB��B�cB��B�B�B�+B�B��B��B��B�|B�GB�B�0B��B�-B��B��B��B�B��B��B��B��B[�BO�BF�BL�BA�BFtBC-B@�B>B:^B:^B8RB5BB[B1[B?�B-wB)_B \BB�BBB�BI�B��B�xBB��B��B��B��B�B�%B��B�TB��B�DB�B�TB�B�5B��B��B�EB�sB��B�mBӏB��B�[B҉B�aB՛B�?B�EBҽB�gB�B͟B�6B�jB��B̘B��B�6B��B��B˒B��B�B�EB�B��B�zB�B�[B�9B�'B��B�B��B��B�B��B��B�FB�9B�tB��B�[B�!B�=B��B�qB��B�tB��B��B��B�4B�-B��B�1B�=B��B��B��B�\B�uB�B�~B��B�+B�B��B��B��B�;B~�B�_B��B{�Bp�Bo�Bf2B_�B_B`�B_B[#BZ�BX�BV9BWsBTaBQ�BPBO�BN�BK�BL�BJ�BI�BI�BIBG�BF�BF�BG�BDgBC�B?�B>�B>�B<�B9�B6�B6�B3�B4�B4�B1�B/�B+�B0�B+�B,�B)*B%FB%FBqB7B�B�B�B B�B B�BJB+B�B�B�B�B
=BVB�	B��B�B�5B� B�B�vB�"B�B�>B��B��B��B�,B�>B�NB�TB��B��B�/B��BרB��B֡B�TB�mB�
B�
BΥB� BҽB�B��B˒BɆBȀBŢB��BƨB�aB��B�,B�B��B��B��B��B�B�!B�B��B��B�B��B��B�kB��B��B��B�$B��B�B�B�hB��B�bB��B�bB�LB�\B��B�xB��B��B�=B��B��B��B��B��B��B�MB��B�uB�B��B��B�bB��B�.B��B�B�B�B�rB�=B�fB��B��B��B��B�uB��B��B~�B}"B�uB��B�ABuZBs�BzxB�Bq�BncBl�Bi�BkQBi�BgmBh>Bd�Bu�B[WBc�BW�B\�B_�BYBP�BTaBM�BPHBC�B^5B8�B6�B0�B(�B'�B+B!�B#�B#nB#nB"4B#nB �BOB�B=B~B1B �B�B�B�BSB�B7B BPB�B\B	lB	7B�B	�BGB�B	7B�BGB;BBoB�cB��B�B�(B�B�ZB�`B�B�5B�B�WB�B��B��B��B�
B�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         202103261700362021032617003620210326170036202103261700362021032617003620210326170036SI  SI  ARFMARFM                                                                                                                                                2020121117092220201211170922IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022001143220210220011432QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022001143220210220011432QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021032510164420210325101644IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021032617005020210326170050IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0ARGO_for_DMQC Climatology Version 2020V03                       ARGO_for_DMQC Climatology Version 2020V03                       2021032617005020210326170050IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021032617005020210326170050IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                