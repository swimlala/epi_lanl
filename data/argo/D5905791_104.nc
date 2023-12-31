CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-12-24T20:41:42Z creation; 2022-09-06T18:25:47Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  dh   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �H   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  Ҩ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �(   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � @�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `h   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � hH   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ��   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   �(   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   �(   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   �(   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �(   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �|   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   �8   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �@   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �`   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �h   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �p   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20211224204142  20220907192127  5905791 5905791 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               h   hAA  AOAO7825_008765_104                 7825_008765_104                 2C  2C  DD  SOLO_II                         SOLO_II                         8765                            8765                            V2.6; SBE602 06Mar19            V2.6; SBE602 06Mar19            853 853 @٬�}ke�@٬�}ke�11  @٬��K]�@٬��K]�@4�)�y��@4�)�y���e]�yЦ�e]�yЦ11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @   @B�\@�G�@�  @��R@޸RA   AG�A ��A*=qA>�RA^�RA�  A�Q�A��A��A�  A�  A�  A�  B   B(�B�
B�B   B(  B/�
B8  B@  BH  BP(�BW�
B`  Bh  Bo�
Bx  B�{B�  B�  B�{B�{B�  B�  B�{B�  B�  B��B��B�  B��B��B�  B��B��B�  B��B��B�  B��B�  B�  B�{B�{B�{B�{B�{B�  B�  C 
=C  C�C  C
=C
  C  C  C  C�C��C��C�HC�HC  C{C {C"  C$  C&
=C(  C*
=C,{C.  C/��C1�C3��C6  C8
=C:
=C<  C=��C@  CB  CD
=CF
=CH  CJ  CL  CM��CO��CQ��CT  CU��CW�CZ  C\
=C^  C`  Cb  Cc��Ce��Cg�Cj  Cl  Cn  Cp
=Cr
=Cs��Cu��Cw�Cz  C|
=C~  C�  C�C�C�C�
=C�  C���C�  C�  C�  C���C�  C�C�  C�  C���C�  C�  C���C�C�  C�C�
=C�C�C�  C�  C�C�C�C�C���C���C�
=C�
=C�C�C�C�  C���C�  C�C�  C���C���C���C�  C���C���C�  C�  C�C�C�C�  C���C���C���C���C�  C�C�  C�  C�  C���C�  C�  C���C�  C�  C�  C�C�  C���C�  C�  C�C�C�  C�C�
=C�\C�
=C�
=C�
=C�  C���C�C�  C���C���C���C�  C�  C���C���C�  C�  C�  C���C�  C�C���C�  C�C�C�  C���C���C�  C���C���C�C�C�  C�  C�
=C�C�  C���C�  C�C���C�  C�  C���C���C���C���D � D�D��D�D� D  D��D  D��D  Dz�D��Dz�D��D}qD�D� D	  D	� D	�qD
��D�D}qD�qDz�D  D��D�qD��D�D�D�qDz�D�qD� D�qD� DD� D  D��D�D��D�D��D�D��D�D��D�qD� D  D� D  Dz�D��D}qDD��D�D�DD��D �D ��D!  D!}qD!�qD"��D#�D#��D$  D$��D%�D%}qD&  D&��D'  D'}qD'�qD(� D)D)�D*  D*��D+  D+}qD,D,}qD,�qD-��D.�D.��D/�D/��D0  D0� D1�D1��D2D2� D2��D3}qD4  D4� D5�D5��D6�D6� D7�D7� D7��D8}qD9  D9� D:  D:��D;�D;�D<  D<��D=�D=� D>�D>�D?�D?}qD@  D@��D@�qDA}qDB�DB� DB��DC}qDD  DD� DE  DE��DF�DF}qDF�qDG��DH  DHxRDH�qDI�DJ  DJ}qDJ�qDK� DLDL� DL�qDMz�DM�qDN� DO  DO��DP  DP� DQ  DQ� DRDR� DS  DS� DT  DT}qDU�DU��DV�DV� DV�qDW}qDW�qDX� DY�DY� DY�qDZ� D[�D[��D\  D\}qD]  D]� D^  D^� D^�qD_� D`  D`� Da�Da��Db  Db��Dc  Dcz�Dc��Ddz�De  De��DfDf�DgDg��Dh  Dh}qDh�qDi}qDj  Dj� Dk  Dk}qDl�Dl��Dm  Dm}qDm�qDn� Dn��Do}qDp  Dp}qDq  Dq�Dr�Dr� Ds  Ds� Dt�Dt� Dt�qDu� Du�qDv}qDv�qDw}qDw�qDxz�Dx�qDy� Dy�qDzz�Dz�qD{��D|  D|� D}  D}��D~  D~}qD  D� D�  D�AHD�~�D�� D�  D�>�D�}qD���D�HD�AHD��HD���D���D�AHD���D�� D���D�@ D��HD�D�HD�=qD�}qD���D�HD�AHD��HD��HD���D�=qD�~�D�� D��qD�>�D��HD�D�HD�AHD���D�� D�HD�B�D��HD�� D���D�@ D��HD��HD�  D�@ D�~�D���D�HD�AHD�}qD��qD��)D�>�D�� D�� D���D�=qD�~�D�� D�  D�AHD�� D�� D��D�B�D��HD��HD�  D�>�D��HD��HD���D�>�D�~�D��qD���D�>�D�� D�� D���D�@ D�~�D��qD��qD�>�D�� D��HD�HD�AHD�� D��HD�HD�>�D�� D���D���D�AHD��HD��qD��qD�=qD���D��HD�  D�AHD��HD��HD���D�=qD�}qD���D�  D�AHD��HD��HD�  D�=qD��HD�� D��qD�>�D�� D��qD���D�>�D�|)D���D�  D�@ D�� D�� D���D�@ D���D��HD�  D�AHD�� D���D�  D�@ D�� D�� D���D�>�D�� D�D��D�B�D��HD��HD�HD�AHD��HD��HD�HD�AHD��HD�� D��qD�=qD�~�D���D�  D�=qD�� D��HD���D�@ D���D��HD�  D�AHD��HD�� D��D�B�D��HD�� D���D�>�D�� D�D�HD�@ D�~�D�� D�HD�@ D�~�D�� D�  D�AHD��HD���D��qD�>�D�� D���D���D�@ D�� D���D�HD�@ D�� D��HD�HD�@ D��HD��HD�  D�AHD�� D���D�  D�@ D�� D��HD�HD�>�D�~�D��HD�  D�@ D�� D�� D�  D�=qD�}qD��qD��qD�AHD�� D��qD���D�>�D�� D�� D�HD�AHD�� D���D��qD�>�D��HD�� D��D�C�D���D�� D�  D�@ D�~�D���D���D�<)D�}qD���D��qD�>�DHD�� D�  D�@ DÀ Dþ�D���D�>�D�~�Dľ�D�  D�@ DŁHD��HD�  D�@ Dƀ D�� D���D�@ Dǀ DǾ�D��qD�>�DȀ D�� D�  D�=qD�~�D�� D�HD�@ D�~�Dʾ�D�  D�AHD˂�D�D�  D�@ D́HD�� D�  D�@ D́HD���D��D�@ D�}qDνqD�  D�B�Dπ DϽqD���D�AHDЂ�Dо�D���D�@ D�~�DѽqD���D�>�DҀ D��HD���D�>�DӀ DӾ�D��qD�@ DԀ DԾ�D�  D�AHDՁHD�� D�HD�B�DցHD��HD���D�>�D׀ D׾�D�  D�@ D؁HD��HD�  D�>�D�~�Dپ�D���D�>�Dڀ D��HD�  D�@ Dۀ D��HD�HD�AHD܂�D�� D�HD�AHD݁HD�� D��D�@ D�~�D�� D�  D�AHD߀ D�� D�  D�@ D�� D�� D�  D�>�D� DᾸD��qD�>�D�~�D⾸D�  D�AHD� D�� D�  D�@ D�HD侸D�  D�@ D� D徸D�  D�@ D�~�D��HD�HD�@ D� D��HD�  D�>�D� D��HD���D�=qD�}qD�� D��D�AHD� D꾸D��qD�=qD�~�D�� D�HD�AHD�HD��HD�HD�AHD�~�D���D�HD�@ D�~�D�� D�  D�AHD� D�� D�  D�AHD�� D�D���D�>�D�~�D�D���D�@ D� D�� D��qD�>�D� D��HD�HD�B�D�HD���D�  D�AHD��HD���D��qD�=qD�� D�D�  D�=qD�~�D���D���D�>�D�~�D��HD�HD�=qD�}qD���D�HD�B�D�t{?#�
?L��?�\)?��R?Ǯ?�ff@�\@�@#�
@5@G�@Tz�@k�@xQ�@�ff@�{@�Q�@�p�@�=q@��@�(�@\@���@�@�p�@�@��@��HAG�AA�A�RAz�A�A{A ��A'
=A*=qA0  A333A9��A<��AB�\AFffAK�AP  AU�AZ=qA^{Adz�Ag�An{Aq�Aw�A{�A�G�A��HA�{A��A�33A���A�  A�=qA���A�\)A��A���A�
=A��A�(�A�
=A�G�A�(�A�{A�G�A�33A�ffA�Q�A��A�A���A�33A�Aȣ�A��HA�ffA�  A��
A�p�A���A��HA�ffA�Q�A��
A�ffA��A��
A�A�G�A��HA�ffA�Q�A��A�B z�B��B
=Bz�B��B\)B(�B
{B
�HB��BB\)Bz�BB
=Bz�BB
=Bz�B��B33BQ�B�B�HB ��B!p�B#33B$Q�B%��B'
=B(Q�B)B*�\B,Q�B-G�B/
=B/�
B1��B2�RB4(�B5�B6�RB7�
B9�B:�RB;�B=G�B>{B?�
B@��BB=qBC�BD��BF{BG33BH��BI��BK33BL(�BMBN�RBPQ�BQG�BR�RBS�
BUG�BV�RBW�
BYp�BZ=qB\(�B]�B^�RB_�
BaG�Bb�RBc�
Be��BfffBhQ�BiG�Bj�HBl(�BmG�Bo
=Bp  Bq��Br�HBt(�Bu��Bv�HBxz�Byp�B{33B|(�B}B
=B�=qB���B��B�Q�B���B�B�=qB�
=B��B�=qB��B���B�z�B���B��B�ffB�
=B��
B�Q�B�33B��
B�z�B�G�B��
B���B��B��B���B�G�B�(�B��\B�p�B�{B���B���B�  B��HB��B�(�B�
=B�p�B�ffB��HB���B�ffB��HB��
B�Q�B��B�B�Q�B�G�B�B��\B�33B��
B���B��B�  B���B�G�B�{B��\B��B�  B���B�p�B�{B���B�\)B�Q�B���B��B�=qB���B��B�(�B�
=B��B�=qB���B�p�B�Q�B��HB��B�Q�B��RB���B�=qB���B��B�(�B���B���B�=qB��B���B�z�B�
=B�B�z�B�
=B��B�Q�B�33B��
B�ffB�G�BɮBʏ\B�33B�B̸RB�33B�  BθRB�G�B�(�BиRB�p�B�=qB���B�B�=qB�
=B��
B�Q�B�33B�B�z�B�G�B�Bڣ�B��B��
B܏\B��B��B�z�B�33B��B�z�B�\)B��
B�\B�\)B�  B��HB�\)B�=qB�
=B癚B�z�B�33B��
B���B�\)B�  B���B홚B�(�B��B�B�Q�B�33B�B�\B�G�B�B���B�33B��
B��RB�G�B�  B���B�G�B�(�B���B�p�B�Q�B���B��B�ffB���B��C G�C �C �CQ�C��C  CffC��C  Cz�C�RC�C�CC33C��C�HCG�C�RC��C\)C��C{Cz�C�C	33C	�\C

=C
\)C
�RC33Cz�C�HCQ�C��C
=Cz�CC(�C��C�HCG�CC
=Cp�C�HCG�C��C
=Cp�C�RC33C�\C�CffC�RC{C�\C�
C33C�C  C\)C�
C�C�C  CG�C��C�Cz�CC33C��C�HCG�C�C�C\)C�C�CQ�C��CC�CffC�CC {C =qC \)C ��C �HC!  C!33C!z�C!�RC!�
C"{C"Q�C"z�C"�RC#  C#(�C#ffC#�C#�C$
=C$Q�C$��C$�RC$��C%G�C%p�C%��C%��C&(�C&\)C&��C&�HC'
=C'G�C'�\C'C'��C(=qC(z�C(��C(�HC)(�C)ffC)�C)C*{C*Q�C*z�C*�C+  C+=qC+\)C+��C+�C,�C,G�C,�C,�
C-  C-(�C-p�C-�RC-�
C.�C.ffC.�\C.C/
=C/Q�C/�C/�C0  C0G�C0p�C0�RC1
=C1=qC1ffC1�RC2
=C2(�C2ffC2�RC3
=C333C3\)C3�C4  C433C4ffC4�C5  C5(�C5\)C5��C5�C6(�C6Q�C6��C6�HC7�C7G�C7�\C7�
C8
=C8G�C8��C8��C8��C9=qC9�\C9C9�C:33C:�C:C:�C;(�C;�C;�RC;�C<�C<ffC<�RC<�C={C=\)C=��C=�
C>
=C>Q�C>��C>C?  C?Q�C?�C?�C@
=C@G�C@p�C@�RCA  CA33CAffCA�CB  CB(�CBffCB�CC  CC33CCffCC�CC��CD33CD\)CD��CD��CE(�CEQ�CE��CE�CF�CFQ�CF��CF�
CG  CGG�CG��CG�RCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                          ?�  @   @B�\@�G�@�  @��R@޸RA   AG�A ��A*=qA>�RA^�RA�  A�Q�A��A��A�  A�  A�  A�  B   B(�B�
B�B   B(  B/�
B8  B@  BH  BP(�BW�
B`  Bh  Bo�
Bx  B�{B�  B�  B�{B�{B�  B�  B�{B�  B�  B��B��B�  B��B��B�  B��B��B�  B��B��B�  B��B�  B�  B�{B�{B�{B�{B�{B�  B�  C 
=C  C�C  C
=C
  C  C  C  C�C��C��C�HC�HC  C{C {C"  C$  C&
=C(  C*
=C,{C.  C/��C1�C3��C6  C8
=C:
=C<  C=��C@  CB  CD
=CF
=CH  CJ  CL  CM��CO��CQ��CT  CU��CW�CZ  C\
=C^  C`  Cb  Cc��Ce��Cg�Cj  Cl  Cn  Cp
=Cr
=Cs��Cu��Cw�Cz  C|
=C~  C�  C�C�C�C�
=C�  C���C�  C�  C�  C���C�  C�C�  C�  C���C�  C�  C���C�C�  C�C�
=C�C�C�  C�  C�C�C�C�C���C���C�
=C�
=C�C�C�C�  C���C�  C�C�  C���C���C���C�  C���C���C�  C�  C�C�C�C�  C���C���C���C���C�  C�C�  C�  C�  C���C�  C�  C���C�  C�  C�  C�C�  C���C�  C�  C�C�C�  C�C�
=C�\C�
=C�
=C�
=C�  C���C�C�  C���C���C���C�  C�  C���C���C�  C�  C�  C���C�  C�C���C�  C�C�C�  C���C���C�  C���C���C�C�C�  C�  C�
=C�C�  C���C�  C�C���C�  C�  C���C���C���C���D � D�D��D�D� D  D��D  D��D  Dz�D��Dz�D��D}qD�D� D	  D	� D	�qD
��D�D}qD�qDz�D  D��D�qD��D�D�D�qDz�D�qD� D�qD� DD� D  D��D�D��D�D��D�D��D�D��D�qD� D  D� D  Dz�D��D}qDD��D�D�DD��D �D ��D!  D!}qD!�qD"��D#�D#��D$  D$��D%�D%}qD&  D&��D'  D'}qD'�qD(� D)D)�D*  D*��D+  D+}qD,D,}qD,�qD-��D.�D.��D/�D/��D0  D0� D1�D1��D2D2� D2��D3}qD4  D4� D5�D5��D6�D6� D7�D7� D7��D8}qD9  D9� D:  D:��D;�D;�D<  D<��D=�D=� D>�D>�D?�D?}qD@  D@��D@�qDA}qDB�DB� DB��DC}qDD  DD� DE  DE��DF�DF}qDF�qDG��DH  DHxRDH�qDI�DJ  DJ}qDJ�qDK� DLDL� DL�qDMz�DM�qDN� DO  DO��DP  DP� DQ  DQ� DRDR� DS  DS� DT  DT}qDU�DU��DV�DV� DV�qDW}qDW�qDX� DY�DY� DY�qDZ� D[�D[��D\  D\}qD]  D]� D^  D^� D^�qD_� D`  D`� Da�Da��Db  Db��Dc  Dcz�Dc��Ddz�De  De��DfDf�DgDg��Dh  Dh}qDh�qDi}qDj  Dj� Dk  Dk}qDl�Dl��Dm  Dm}qDm�qDn� Dn��Do}qDp  Dp}qDq  Dq�Dr�Dr� Ds  Ds� Dt�Dt� Dt�qDu� Du�qDv}qDv�qDw}qDw�qDxz�Dx�qDy� Dy�qDzz�Dz�qD{��D|  D|� D}  D}��D~  D~}qD  D� D�  D�AHD�~�D�� D�  D�>�D�}qD���D�HD�AHD��HD���D���D�AHD���D�� D���D�@ D��HD�D�HD�=qD�}qD���D�HD�AHD��HD��HD���D�=qD�~�D�� D��qD�>�D��HD�D�HD�AHD���D�� D�HD�B�D��HD�� D���D�@ D��HD��HD�  D�@ D�~�D���D�HD�AHD�}qD��qD��)D�>�D�� D�� D���D�=qD�~�D�� D�  D�AHD�� D�� D��D�B�D��HD��HD�  D�>�D��HD��HD���D�>�D�~�D��qD���D�>�D�� D�� D���D�@ D�~�D��qD��qD�>�D�� D��HD�HD�AHD�� D��HD�HD�>�D�� D���D���D�AHD��HD��qD��qD�=qD���D��HD�  D�AHD��HD��HD���D�=qD�}qD���D�  D�AHD��HD��HD�  D�=qD��HD�� D��qD�>�D�� D��qD���D�>�D�|)D���D�  D�@ D�� D�� D���D�@ D���D��HD�  D�AHD�� D���D�  D�@ D�� D�� D���D�>�D�� D�D��D�B�D��HD��HD�HD�AHD��HD��HD�HD�AHD��HD�� D��qD�=qD�~�D���D�  D�=qD�� D��HD���D�@ D���D��HD�  D�AHD��HD�� D��D�B�D��HD�� D���D�>�D�� D�D�HD�@ D�~�D�� D�HD�@ D�~�D�� D�  D�AHD��HD���D��qD�>�D�� D���D���D�@ D�� D���D�HD�@ D�� D��HD�HD�@ D��HD��HD�  D�AHD�� D���D�  D�@ D�� D��HD�HD�>�D�~�D��HD�  D�@ D�� D�� D�  D�=qD�}qD��qD��qD�AHD�� D��qD���D�>�D�� D�� D�HD�AHD�� D���D��qD�>�D��HD�� D��D�C�D���D�� D�  D�@ D�~�D���D���D�<)D�}qD���D��qD�>�DHD�� D�  D�@ DÀ Dþ�D���D�>�D�~�Dľ�D�  D�@ DŁHD��HD�  D�@ Dƀ D�� D���D�@ Dǀ DǾ�D��qD�>�DȀ D�� D�  D�=qD�~�D�� D�HD�@ D�~�Dʾ�D�  D�AHD˂�D�D�  D�@ D́HD�� D�  D�@ D́HD���D��D�@ D�}qDνqD�  D�B�Dπ DϽqD���D�AHDЂ�Dо�D���D�@ D�~�DѽqD���D�>�DҀ D��HD���D�>�DӀ DӾ�D��qD�@ DԀ DԾ�D�  D�AHDՁHD�� D�HD�B�DցHD��HD���D�>�D׀ D׾�D�  D�@ D؁HD��HD�  D�>�D�~�Dپ�D���D�>�Dڀ D��HD�  D�@ Dۀ D��HD�HD�AHD܂�D�� D�HD�AHD݁HD�� D��D�@ D�~�D�� D�  D�AHD߀ D�� D�  D�@ D�� D�� D�  D�>�D� DᾸD��qD�>�D�~�D⾸D�  D�AHD� D�� D�  D�@ D�HD侸D�  D�@ D� D徸D�  D�@ D�~�D��HD�HD�@ D� D��HD�  D�>�D� D��HD���D�=qD�}qD�� D��D�AHD� D꾸D��qD�=qD�~�D�� D�HD�AHD�HD��HD�HD�AHD�~�D���D�HD�@ D�~�D�� D�  D�AHD� D�� D�  D�AHD�� D�D���D�>�D�~�D�D���D�@ D� D�� D��qD�>�D� D��HD�HD�B�D�HD���D�  D�AHD��HD���D��qD�=qD�� D�D�  D�=qD�~�D���D���D�>�D�~�D��HD�HD�=qD�}qD���D�HD�B�G�O�?#�
?L��?�\)?��R?Ǯ?�ff@�\@�@#�
@5@G�@Tz�@k�@xQ�@�ff@�{@�Q�@�p�@�=q@��@�(�@\@���@�@�p�@�@��@��HAG�AA�A�RAz�A�A{A ��A'
=A*=qA0  A333A9��A<��AB�\AFffAK�AP  AU�AZ=qA^{Adz�Ag�An{Aq�Aw�A{�A�G�A��HA�{A��A�33A���A�  A�=qA���A�\)A��A���A�
=A��A�(�A�
=A�G�A�(�A�{A�G�A�33A�ffA�Q�A��A�A���A�33A�Aȣ�A��HA�ffA�  A��
A�p�A���A��HA�ffA�Q�A��
A�ffA��A��
A�A�G�A��HA�ffA�Q�A��A�B z�B��B
=Bz�B��B\)B(�B
{B
�HB��BB\)Bz�BB
=Bz�BB
=Bz�B��B33BQ�B�B�HB ��B!p�B#33B$Q�B%��B'
=B(Q�B)B*�\B,Q�B-G�B/
=B/�
B1��B2�RB4(�B5�B6�RB7�
B9�B:�RB;�B=G�B>{B?�
B@��BB=qBC�BD��BF{BG33BH��BI��BK33BL(�BMBN�RBPQ�BQG�BR�RBS�
BUG�BV�RBW�
BYp�BZ=qB\(�B]�B^�RB_�
BaG�Bb�RBc�
Be��BfffBhQ�BiG�Bj�HBl(�BmG�Bo
=Bp  Bq��Br�HBt(�Bu��Bv�HBxz�Byp�B{33B|(�B}B
=B�=qB���B��B�Q�B���B�B�=qB�
=B��B�=qB��B���B�z�B���B��B�ffB�
=B��
B�Q�B�33B��
B�z�B�G�B��
B���B��B��B���B�G�B�(�B��\B�p�B�{B���B���B�  B��HB��B�(�B�
=B�p�B�ffB��HB���B�ffB��HB��
B�Q�B��B�B�Q�B�G�B�B��\B�33B��
B���B��B�  B���B�G�B�{B��\B��B�  B���B�p�B�{B���B�\)B�Q�B���B��B�=qB���B��B�(�B�
=B��B�=qB���B�p�B�Q�B��HB��B�Q�B��RB���B�=qB���B��B�(�B���B���B�=qB��B���B�z�B�
=B�B�z�B�
=B��B�Q�B�33B��
B�ffB�G�BɮBʏ\B�33B�B̸RB�33B�  BθRB�G�B�(�BиRB�p�B�=qB���B�B�=qB�
=B��
B�Q�B�33B�B�z�B�G�B�Bڣ�B��B��
B܏\B��B��B�z�B�33B��B�z�B�\)B��
B�\B�\)B�  B��HB�\)B�=qB�
=B癚B�z�B�33B��
B���B�\)B�  B���B홚B�(�B��B�B�Q�B�33B�B�\B�G�B�B���B�33B��
B��RB�G�B�  B���B�G�B�(�B���B�p�B�Q�B���B��B�ffB���B��C G�C �C �CQ�C��C  CffC��C  Cz�C�RC�C�CC33C��C�HCG�C�RC��C\)C��C{Cz�C�C	33C	�\C

=C
\)C
�RC33Cz�C�HCQ�C��C
=Cz�CC(�C��C�HCG�CC
=Cp�C�HCG�C��C
=Cp�C�RC33C�\C�CffC�RC{C�\C�
C33C�C  C\)C�
C�C�C  CG�C��C�Cz�CC33C��C�HCG�C�C�C\)C�C�CQ�C��CC�CffC�CC {C =qC \)C ��C �HC!  C!33C!z�C!�RC!�
C"{C"Q�C"z�C"�RC#  C#(�C#ffC#�C#�C$
=C$Q�C$��C$�RC$��C%G�C%p�C%��C%��C&(�C&\)C&��C&�HC'
=C'G�C'�\C'C'��C(=qC(z�C(��C(�HC)(�C)ffC)�C)C*{C*Q�C*z�C*�C+  C+=qC+\)C+��C+�C,�C,G�C,�C,�
C-  C-(�C-p�C-�RC-�
C.�C.ffC.�\C.C/
=C/Q�C/�C/�C0  C0G�C0p�C0�RC1
=C1=qC1ffC1�RC2
=C2(�C2ffC2�RC3
=C333C3\)C3�C4  C433C4ffC4�C5  C5(�C5\)C5��C5�C6(�C6Q�C6��C6�HC7�C7G�C7�\C7�
C8
=C8G�C8��C8��C8��C9=qC9�\C9C9�C:33C:�C:C:�C;(�C;�C;�RC;�C<�C<ffC<�RC<�C={C=\)C=��C=�
C>
=C>Q�C>��C>C?  C?Q�C?�C?�C@
=C@G�C@p�C@�RCA  CA33CAffCA�CB  CB(�CBffCB�CC  CC33CCffCC�CC��CD33CD\)CD��CD��CE(�CEQ�CE��CE�CF�CFQ�CF��CF�
CG  CGG�CG��CG�RCG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                          @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�J@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ffA�jA�l�A�l�A�n�A�l�A�dZA�^5A�dZA�hsA�l�A�r�AʋDAʮAʮAʮAʰ!AʮAʰ!Aʰ!Aʲ-Aʲ-Aʲ-Aʴ9Aʴ9AʶFAʲ-Aʲ-Aʴ9Aʲ-Aʰ!Aʰ!AʮAʧ�Aʝ�Aʇ+A�z�A�hsA�dZA�`BA�XA�VA�O�A�A�A�;dA�5?A�1'A�+A�oA�A���A�ĜA�5?AǼjA�A�A�r�A�{A�&�A�ZA�A�A�ȴA�S�A�VA�|�A��A�?}A�n�A��PA�7LA��RA���A��jA���A�VA�A��yA�I�A��A�oA�ĜA�A�;dA��A��mA��DA�oA���A�~�A��A�=qA���A��A��yA���A�JA��A��A�ffA���A�"�A�dZA�ZA���A�t�A�&�A�%A��
A��A~��Az�/Av�+Ar�HAqx�AoVAmhsAj�/Ai�-Ah~�Agt�Ad5?A`5?A^5?A]�7A]C�A\�9A\  AZ�DAYp�AX�AW��AW�-AW33AVr�AV=qAV �AT��ARI�AP~�AMVAI��AFĜAE��AC�^AA�7A@ȴA?��A=�A;�TA;/A:�A:�\A8��A6�A5t�A4^5A4JA3�A3��A3hsA2�yA1�TA0~�A/O�A.��A.�A,ĜA*v�A)�FA(�A&�A&�A%|�A$~�A"��A!+A�wAjA�A�7A\)A��A��A��AffA+A�A��AoA��AVAI�A9XAAx�A�HA��AE�AAS�A;dA	A�\A\)AȴA�
Al�A��Ax�A{A ��@�dZ@���@��m@�bN@�  @�@��`@���@��+@�J@��^@���@���@���@�-@�Q�@@�!@�$�@��@�O�@��m@�5?@�p�@�D@���@�9@��@�+@��#@��@�/@�ƨ@��#@ܣ�@�bN@��#@�b@�M�@�r�@ӕ�@���@�M�@��@�J@�@���@��@�X@�bN@�;d@·+@�X@���@���@�X@�z�@�  @���@�;d@�=q@��@�z�@�t�@�v�@��#@�x�@�Q�@��@�J@��7@�x�@��@��@���@��+@��@��7@�`B@�G�@�/@�V@��/@��@�  @���@�t�@�S�@�o@��H@�ff@�5?@�=q@���@��@�"�@�33@�;d@���@�ff@���@��#@��^@��@���@���@��@��@�~�@�J@���@��h@��7@��@�hs@�`B@�?}@�&�@��/@���@�K�@���@�V@�=q@�-@��@�{@�@��#@�p�@��@�bN@��@�@���@�M�@�@��^@���@��7@�p�@�G�@���@��D@�z�@�j@�Z@�9X@��;@��@���@���@�l�@�;d@��y@���@�M�@�@��-@���@���@�?}@��`@��9@��u@�r�@� �@��@�V@�O�@���@���@�hs@��@���@��-@��^@���@�G�@��`@��@��@���@��@��@�$�@���@���@���@�V@��D@�9X@���@��@��m@�ƨ@�ƨ@���@��@�\)@�C�@�K�@�S�@�t�@���@��P@�K�@��@��T@���@���@�`B@�V@���@��9@���@���@��@��@��
@���@�dZ@��@��@��@�o@��@���@���@��\@�v�@�^5@�$�@�{@��@�`B@�1'@��w@�C�@�@��!@��@��h@�G�@�V@��u@�Q�@� �@�ƨ@��P@�l�@�;d@��@��R@���@���@�V@���@��@���@��@���@��@�Z@��@�33@�+@��@��@�ȴ@���@���@�5?@�@��@��7@�X@��@���@�Ĝ@��9@�z�@�(�@��;@��F@�l�@��y@��R@�n�@�$�@��h@�?}@��@���@���@��@��`@�Ĝ@���@��D@��D@�bN@���@��w@�|�@�;d@�o@���@��+@�V@�{@���@��-@���@��h@�7L@��j@�1'@�1@l�@K�@+@~��@~�@~�+@~{@}��@}`B@}/@|�j@|��@|Z@|�@{��@{C�@{33@{@z�!@z^5@y��@y�7@y7L@y%@x��@x�9@x�@x  @w\)@w�@v�R@v��@u�T@u�h@uO�@t�@t��@tI�@t�@sƨ@s��@s��@s�@s�@sS�@so@rJ@q��@q��@q��@q��@q��@q��@q�7@qX@q�@pbN@o�@o�@o�@o��@o��@o�P@o\)@n��@n{@m@m�h@m�@lI�@l9X@l�@l�@l1@k�
@kdZ@k@j�!@j�\@jM�@i�^@iX@i7L@i�@h��@h��@hr�@h  @g��@g�P@g\)@f��@f�@f�@f�@f�R@fE�@e@d��@d�j@d��@dI�@d1@c�F@cC�@b�@b�!@b��@bn�@b-@a��@`�@_�@^E�@]�T@]p�@]?}@\�@\j@\9X@[�
@[��@[�@[33@Z�@Z�@Z�@Z��@Z^5@Y��@Y�@XĜ@X �@W��@W\)@W
=@V�y@V5?@U��@U�h@UV@T(�@S��@S�
@S��@S"�@R�@R��@R��@R~�@RM�@R-@R�@Q��@QG�@P�u@PQ�@P �@O�@O|�@O\)@O�@N��@M�@M`B@L�/@Lj@K�
@KS�@K"�@K@J�@J��@J�!@J�\@J^5@J�@I��@IG�@I%@H��@HĜ@H�u@Hr�@HQ�@H  @G|�@Fff@E��@EO�@D�/@D�@D��@Dz�@C�
@B�H@B=q@BJ@A�@A��@AG�@A&�@@�u@@ �@?��@?|�@?|�@?;d@?
=@>�+@>5?@>{@=�@=�T@=�T@=�@=�@=�T@=�h@=�@=p�@=p�@=p�@=p�@=p�@=`B@=O�@=?}@<�D@;��@;dZ@:�@:�\@:-@9�7@8��@8��@8��@8��@8Ĝ@8Q�@8 �@8b@7�;@7|�@6��@6ȴ@6�+@6v�@6$�@5O�@49X@3��@3��@3o@2�@1�#@1�^@1hs@17L@1&�@1�@1%@0�9@0b@/�w@/K�@.��@.�+@.{@-��@-�-@-�h@-/@,��@,I�@+�
@+S�@*�!@*J@)hs@(��@( �@'�@&�y@&��@&ff@&5?@&E�@&5?@&{@&@%�@%��@%�h@%�@$��@$�@#��@#�F@#��@#��@#t�@#C�@#33@#C�@#C�@#33@#33@#o@"��@"��@"��@"��@"n�@!�@!��@!x�@!X@!&�@!%@!%@!%@ ��@ �@ r�@ Q�@ b@�@�;@�@�P@|�@|�@|�@l�@\)@�@��@ȴ@�+@ff@E�@{@p�@z�@(�@1@�m@ƨ@ƨ@�F@��@��@�@��@t�@S�@o@"�@o@@@��@^5@-@J@�^@hs@�@�`@�`@Ĝ@Ĝ@��@�u@�@bN@Q�@A�@b@�@\)@+@
=@
=@��@��@�y@�y@��@V@{@��@��@@�h@`B@?}@��@��@��@j@9X@(�@1@��@��@��@o@�\@=q@-@��@�#@��@��@�^@��@�7@x�@x�@hs@X@X@G�@G�@X@7L@7L@7L@%@��@��@�`@r�@1'@�;@��@�P@K�@ȴ@�R@��@��@v�@E�@5?@5?@$�@{@@@{@{@$�@$�@$�@$�@@O�@�@�@�@�@��@�@j@Z@Z@9X@��@dZ@S�@C�@"�@o@o@oA�bNA�ffA�hsA�n�A�hsA�l�A�hsA�l�A�jA�n�A�hsA�p�A�jA�n�A�hsA�p�A�hsA�t�A�jA�p�A�jA�n�A�ffA�n�A�bNA�dZA�\)A�bNA�\)A�bNA�^5A�ffA�dZA�hsA�ffA�jA�hsA�n�A�jA�p�A�p�A�p�A�t�A�t�A�t�A�n�A�r�A�l�Aʇ+Aʥ�Aʰ!Aʩ�Aʰ!AʬAʰ!AʬAʮAʮAʰ!AʮAʮAʮAʬAʰ!Aʩ�Aʰ!Aʩ�Aʰ!AʬAʲ-AʬAʲ-AʬAʲ-AʬAʰ!AʮAʮAʰ!AʬAʰ!AʬAʲ-AʬAʲ-AʮAʲ-Aʰ!Aʰ!Aʰ!Aʰ!Aʰ!Aʰ!Aʲ-AʮAʴ9AʮAʴ9AʮAʲ-Aʲ-Aʲ-Aʲ-AʮAʴ9Aʰ!AʶFAʮAʴ9Aʰ!AʶFAʲ-Aʴ9AʶFAʲ-AʶFAʲ-AʶFAʲ-AʶFAʲ-AʸRAʲ-AʸRAʲ-AʸRAʲ-AʸRAʴ9AʶFAʶFAʲ-AʶFAʰ!AʶFAʰ!Aʴ9Aʰ!Aʴ9Aʲ-Aʲ-Aʲ-Aʰ!Aʴ9Aʲ-AʶFAʰ!AʶFAʰ!AʶFAʰ!Aʴ9Aʴ9Aʲ-AʶFAʮAʲ-Aʰ!Aʲ-AʬAʰ!AʮAʲ-Aʰ!Aʰ!Aʰ!AʮAʲ-AʮAʲ-AʬAʲ-Aʩ�AʮAʩ�Aʩ�Aʩ�Aʥ�AʬAʣ�Aʥ�Aʟ�Aʟ�Aʟ�Aʛ�Aʗ�AʑhAʑhAʇ+AʅAʅAʁAʃA�|�A�|�A�v�A�|�A�v�A�r�A�l�A�dZA�hsA�bNA�ffA�bNA�ffA�bNA�dZA�ffA�bNA�hsA�bNA�`BA�^5A�ZA�^5A�XA�^5A�XA�XA�ZA�S�A�ZA�S�A�XA�S�A�S�A�S�A�M�A�S�A�M�A�O�A�M�A�G�A�K�A�?}A�?}A�=qA�9XA�?}A�9XA�=qA�9XA�7LA�;dA�5?A�7LA�5?A�5?A�7LA�1'A�7LA�1'A�33A�1'A�+A�/A�+A�-A�+A�&�A�+A��A��A�{A�bA�VA�%A�%A�A�A�A���A�A���A�  A���A���A���A���A���A��`A��#A�Aɛ�Aɏ\AɅA�hsA�dZA�A�A�&�A�ƨAȓuA�K�A���AǇ+A�C�AƲ-AƅA�^5A�A�A�-A�{A���A���Aš�A�|�A�S�A�-A���A��#A�jA�bA�ĜA�^5A�A§�A�XA��A��wA��+A�C�A�ȴA�;dA��A��A��
A�ĜA���A�|�A�$�A��FA�n�A�/A�{A�A��TA��A�C�A��;A��A�t�A�(�A���A���A�/A�=qA�5?A�&�A�oA�A��;A�7LA��hA�&�A���A���A��wA��RA���A��A�|�A�^5A�;dA�/A�"�A�{A���A���A�VA�1'A��A��A�
=A��TA��!A�VA��`A��A�(�A��wA���A�I�A�33A�(�A��A�{A��TA�G�A��7A�A���A��uA��DA��PA��\A��A�M�A��+A��A��A�O�A��A���A���A�~�A�?}A���A�v�A�33A�VA��jA�&�A�z�A���A���A��mA�v�A�-A���A��FA��A��uA�x�A�p�A�ffA�JA��A��DA�&�A�A��A�ĜA��DA�A�A� �A���A�v�A�=qA��/A��A���A�A�A�/A��A�%A���A��TA��wA�dZA� �A��A�%A���A��A��A��A��A��A��A��yA��mA��mA��HA��`A��mA��#A���A���A�=qA���A��A��/A���A��uA�bA��wA���A�v�A�\)A�K�A�E�A�A��A�t�A�|�A�n�A�K�A�9XA��A��#A�x�A�bNA�^5A�`BA�bNA�^5A�^5A�ZA�O�A�A�A�/A�(�A�{A�1A�  A��`A��FA���A�t�A�K�A�9XA�&�A�%A��HA�ȴA��9A��A���A���A���A��hA��\A��hA��\A��A��A��A�t�A�G�A��HA��9A��uA��7A�p�A�dZA�M�A�/A�1A��-A�VA�bA�VA���A�E�A��A���A���A�v�A�bNA�{A��`A���A�VA�bA�~�A�I�A��A���A���A�t�A�5?A�bA���A���A��DA�z�A�Q�A��A��A��yA��/A���A�ĜA��A���A��7A��A�v�A�n�A�\)A�K�A�5?A�-A�"�A�A��yA��#A���A��A���A��\A�|�A�hsA�I�A�1'A�A��A���A��wA��A���A��\A�|�A�XA�C�A�(�A�{A��A���A��FA�z�A��9A�33A�;dA�JA���A��RA�^5A�{A��;A���A��-A���A��A�~�A�|�A�~�A�|�A�x�A�x�A�r�A�\)A��A���A��hA�ffA�5?A�ĜA��A�p�A�XA�7LA��A���A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                          A�ffA�jA�l�A�l�A�n�A�l�A�dZA�^5A�dZA�hsA�l�A�r�AʋDAʮAʮAʮAʰ!AʮAʰ!Aʰ!Aʲ-Aʲ-Aʲ-Aʴ9Aʴ9AʶFAʲ-Aʲ-Aʴ9Aʲ-Aʰ!Aʰ!AʮAʧ�Aʝ�Aʇ+A�z�A�hsA�dZA�`BA�XA�VA�O�A�A�A�;dA�5?A�1'A�+A�oA�A���A�ĜA�5?AǼjA�A�A�r�A�{A�&�A�ZA�A�A�ȴA�S�A�VA�|�A��A�?}A�n�A��PA�7LA��RA���A��jA���A�VA�A��yA�I�A��A�oA�ĜA�A�;dA��A��mA��DA�oA���A�~�A��A�=qA���A��A��yA���A�JA��A��A�ffA���A�"�A�dZA�ZA���A�t�A�&�A�%A��
A��A~��Az�/Av�+Ar�HAqx�AoVAmhsAj�/Ai�-Ah~�Agt�Ad5?A`5?A^5?A]�7A]C�A\�9A\  AZ�DAYp�AX�AW��AW�-AW33AVr�AV=qAV �AT��ARI�AP~�AMVAI��AFĜAE��AC�^AA�7A@ȴA?��A=�A;�TA;/A:�A:�\A8��A6�A5t�A4^5A4JA3�A3��A3hsA2�yA1�TA0~�A/O�A.��A.�A,ĜA*v�A)�FA(�A&�A&�A%|�A$~�A"��A!+A�wAjA�A�7A\)A��A��A��AffA+A�A��AoA��AVAI�A9XAAx�A�HA��AE�AAS�A;dA	A�\A\)AȴA�
Al�A��Ax�A{A ��@�dZ@���@��m@�bN@�  @�@��`@���@��+@�J@��^@���@���@���@�-@�Q�@@�!@�$�@��@�O�@��m@�5?@�p�@�D@���@�9@��@�+@��#@��@�/@�ƨ@��#@ܣ�@�bN@��#@�b@�M�@�r�@ӕ�@���@�M�@��@�J@�@���@��@�X@�bN@�;d@·+@�X@���@���@�X@�z�@�  @���@�;d@�=q@��@�z�@�t�@�v�@��#@�x�@�Q�@��@�J@��7@�x�@��@��@���@��+@��@��7@�`B@�G�@�/@�V@��/@��@�  @���@�t�@�S�@�o@��H@�ff@�5?@�=q@���@��@�"�@�33@�;d@���@�ff@���@��#@��^@��@���@���@��@��@�~�@�J@���@��h@��7@��@�hs@�`B@�?}@�&�@��/@���@�K�@���@�V@�=q@�-@��@�{@�@��#@�p�@��@�bN@��@�@���@�M�@�@��^@���@��7@�p�@�G�@���@��D@�z�@�j@�Z@�9X@��;@��@���@���@�l�@�;d@��y@���@�M�@�@��-@���@���@�?}@��`@��9@��u@�r�@� �@��@�V@�O�@���@���@�hs@��@���@��-@��^@���@�G�@��`@��@��@���@��@��@�$�@���@���@���@�V@��D@�9X@���@��@��m@�ƨ@�ƨ@���@��@�\)@�C�@�K�@�S�@�t�@���@��P@�K�@��@��T@���@���@�`B@�V@���@��9@���@���@��@��@��
@���@�dZ@��@��@��@�o@��@���@���@��\@�v�@�^5@�$�@�{@��@�`B@�1'@��w@�C�@�@��!@��@��h@�G�@�V@��u@�Q�@� �@�ƨ@��P@�l�@�;d@��@��R@���@���@�V@���@��@���@��@���@��@�Z@��@�33@�+@��@��@�ȴ@���@���@�5?@�@��@��7@�X@��@���@�Ĝ@��9@�z�@�(�@��;@��F@�l�@��y@��R@�n�@�$�@��h@�?}@��@���@���@��@��`@�Ĝ@���@��D@��D@�bN@���@��w@�|�@�;d@�o@���@��+@�V@�{@���@��-@���@��h@�7L@��j@�1'@�1@l�@K�@+@~��@~�@~�+@~{@}��@}`B@}/@|�j@|��@|Z@|�@{��@{C�@{33@{@z�!@z^5@y��@y�7@y7L@y%@x��@x�9@x�@x  @w\)@w�@v�R@v��@u�T@u�h@uO�@t�@t��@tI�@t�@sƨ@s��@s��@s�@s�@sS�@so@rJ@q��@q��@q��@q��@q��@q��@q�7@qX@q�@pbN@o�@o�@o�@o��@o��@o�P@o\)@n��@n{@m@m�h@m�@lI�@l9X@l�@l�@l1@k�
@kdZ@k@j�!@j�\@jM�@i�^@iX@i7L@i�@h��@h��@hr�@h  @g��@g�P@g\)@f��@f�@f�@f�@f�R@fE�@e@d��@d�j@d��@dI�@d1@c�F@cC�@b�@b�!@b��@bn�@b-@a��@`�@_�@^E�@]�T@]p�@]?}@\�@\j@\9X@[�
@[��@[�@[33@Z�@Z�@Z�@Z��@Z^5@Y��@Y�@XĜ@X �@W��@W\)@W
=@V�y@V5?@U��@U�h@UV@T(�@S��@S�
@S��@S"�@R�@R��@R��@R~�@RM�@R-@R�@Q��@QG�@P�u@PQ�@P �@O�@O|�@O\)@O�@N��@M�@M`B@L�/@Lj@K�
@KS�@K"�@K@J�@J��@J�!@J�\@J^5@J�@I��@IG�@I%@H��@HĜ@H�u@Hr�@HQ�@H  @G|�@Fff@E��@EO�@D�/@D�@D��@Dz�@C�
@B�H@B=q@BJ@A�@A��@AG�@A&�@@�u@@ �@?��@?|�@?|�@?;d@?
=@>�+@>5?@>{@=�@=�T@=�T@=�@=�@=�T@=�h@=�@=p�@=p�@=p�@=p�@=p�@=`B@=O�@=?}@<�D@;��@;dZ@:�@:�\@:-@9�7@8��@8��@8��@8��@8Ĝ@8Q�@8 �@8b@7�;@7|�@6��@6ȴ@6�+@6v�@6$�@5O�@49X@3��@3��@3o@2�@1�#@1�^@1hs@17L@1&�@1�@1%@0�9@0b@/�w@/K�@.��@.�+@.{@-��@-�-@-�h@-/@,��@,I�@+�
@+S�@*�!@*J@)hs@(��@( �@'�@&�y@&��@&ff@&5?@&E�@&5?@&{@&@%�@%��@%�h@%�@$��@$�@#��@#�F@#��@#��@#t�@#C�@#33@#C�@#C�@#33@#33@#o@"��@"��@"��@"��@"n�@!�@!��@!x�@!X@!&�@!%@!%@!%@ ��@ �@ r�@ Q�@ b@�@�;@�@�P@|�@|�@|�@l�@\)@�@��@ȴ@�+@ff@E�@{@p�@z�@(�@1@�m@ƨ@ƨ@�F@��@��@�@��@t�@S�@o@"�@o@@@��@^5@-@J@�^@hs@�@�`@�`@Ĝ@Ĝ@��@�u@�@bN@Q�@A�@b@�@\)@+@
=@
=@��@��@�y@�y@��@V@{@��@��@@�h@`B@?}@��@��@��@j@9X@(�@1@��@��@��@o@�\@=q@-@��@�#@��@��@�^@��@�7@x�@x�@hs@X@X@G�@G�@X@7L@7L@7L@%@��@��@�`@r�@1'@�;@��@�P@K�@ȴ@�R@��@��@v�@E�@5?@5?@$�@{@@@{@{@$�@$�@$�@$�@@O�@�@�@�@�@��@�@j@Z@Z@9X@��@dZ@S�@C�@"�@o@oG�O�A�bNA�ffA�hsA�n�A�hsA�l�A�hsA�l�A�jA�n�A�hsA�p�A�jA�n�A�hsA�p�A�hsA�t�A�jA�p�A�jA�n�A�ffA�n�A�bNA�dZA�\)A�bNA�\)A�bNA�^5A�ffA�dZA�hsA�ffA�jA�hsA�n�A�jA�p�A�p�A�p�A�t�A�t�A�t�A�n�A�r�A�l�Aʇ+Aʥ�Aʰ!Aʩ�Aʰ!AʬAʰ!AʬAʮAʮAʰ!AʮAʮAʮAʬAʰ!Aʩ�Aʰ!Aʩ�Aʰ!AʬAʲ-AʬAʲ-AʬAʲ-AʬAʰ!AʮAʮAʰ!AʬAʰ!AʬAʲ-AʬAʲ-AʮAʲ-Aʰ!Aʰ!Aʰ!Aʰ!Aʰ!Aʰ!Aʲ-AʮAʴ9AʮAʴ9AʮAʲ-Aʲ-Aʲ-Aʲ-AʮAʴ9Aʰ!AʶFAʮAʴ9Aʰ!AʶFAʲ-Aʴ9AʶFAʲ-AʶFAʲ-AʶFAʲ-AʶFAʲ-AʸRAʲ-AʸRAʲ-AʸRAʲ-AʸRAʴ9AʶFAʶFAʲ-AʶFAʰ!AʶFAʰ!Aʴ9Aʰ!Aʴ9Aʲ-Aʲ-Aʲ-Aʰ!Aʴ9Aʲ-AʶFAʰ!AʶFAʰ!AʶFAʰ!Aʴ9Aʴ9Aʲ-AʶFAʮAʲ-Aʰ!Aʲ-AʬAʰ!AʮAʲ-Aʰ!Aʰ!Aʰ!AʮAʲ-AʮAʲ-AʬAʲ-Aʩ�AʮAʩ�Aʩ�Aʩ�Aʥ�AʬAʣ�Aʥ�Aʟ�Aʟ�Aʟ�Aʛ�Aʗ�AʑhAʑhAʇ+AʅAʅAʁAʃA�|�A�|�A�v�A�|�A�v�A�r�A�l�A�dZA�hsA�bNA�ffA�bNA�ffA�bNA�dZA�ffA�bNA�hsA�bNA�`BA�^5A�ZA�^5A�XA�^5A�XA�XA�ZA�S�A�ZA�S�A�XA�S�A�S�A�S�A�M�A�S�A�M�A�O�A�M�A�G�A�K�A�?}A�?}A�=qA�9XA�?}A�9XA�=qA�9XA�7LA�;dA�5?A�7LA�5?A�5?A�7LA�1'A�7LA�1'A�33A�1'A�+A�/A�+A�-A�+A�&�A�+A��A��A�{A�bA�VA�%A�%A�A�A�A���A�A���A�  A���A���A���A���A���A��`A��#A�Aɛ�Aɏ\AɅA�hsA�dZA�A�A�&�A�ƨAȓuA�K�A���AǇ+A�C�AƲ-AƅA�^5A�A�A�-A�{A���A���Aš�A�|�A�S�A�-A���A��#A�jA�bA�ĜA�^5A�A§�A�XA��A��wA��+A�C�A�ȴA�;dA��A��A��
A�ĜA���A�|�A�$�A��FA�n�A�/A�{A�A��TA��A�C�A��;A��A�t�A�(�A���A���A�/A�=qA�5?A�&�A�oA�A��;A�7LA��hA�&�A���A���A��wA��RA���A��A�|�A�^5A�;dA�/A�"�A�{A���A���A�VA�1'A��A��A�
=A��TA��!A�VA��`A��A�(�A��wA���A�I�A�33A�(�A��A�{A��TA�G�A��7A�A���A��uA��DA��PA��\A��A�M�A��+A��A��A�O�A��A���A���A�~�A�?}A���A�v�A�33A�VA��jA�&�A�z�A���A���A��mA�v�A�-A���A��FA��A��uA�x�A�p�A�ffA�JA��A��DA�&�A�A��A�ĜA��DA�A�A� �A���A�v�A�=qA��/A��A���A�A�A�/A��A�%A���A��TA��wA�dZA� �A��A�%A���A��A��A��A��A��A��A��yA��mA��mA��HA��`A��mA��#A���A���A�=qA���A��A��/A���A��uA�bA��wA���A�v�A�\)A�K�A�E�A�A��A�t�A�|�A�n�A�K�A�9XA��A��#A�x�A�bNA�^5A�`BA�bNA�^5A�^5A�ZA�O�A�A�A�/A�(�A�{A�1A�  A��`A��FA���A�t�A�K�A�9XA�&�A�%A��HA�ȴA��9A��A���A���A���A��hA��\A��hA��\A��A��A��A�t�A�G�A��HA��9A��uA��7A�p�A�dZA�M�A�/A�1A��-A�VA�bA�VA���A�E�A��A���A���A�v�A�bNA�{A��`A���A�VA�bA�~�A�I�A��A���A���A�t�A�5?A�bA���A���A��DA�z�A�Q�A��A��A��yA��/A���A�ĜA��A���A��7A��A�v�A�n�A�\)A�K�A�5?A�-A�"�A�A��yA��#A���A��A���A��\A�|�A�hsA�I�A�1'A�A��A���A��wA��A���A��\A�|�A�XA�C�A�(�A�{A��A���A��FA�z�A��9A�33A�;dA�JA���A��RA�^5A�{A��;A���A��-A���A��A�~�A�|�A�~�A�|�A�x�A�x�A�r�A�\)A��A���A��hA�ffA�5?A�ĜA��A�p�A�XA�7LA��A���A��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                          ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B�aB�[BB��B�UB�B�B�BB��B�wB��B��B�gB�B�B�B�9B�9B�9B�B�9B�mB֡B��B�?BخB�B�BٴBںB�#B��B�B��B�DB�KB�"B��B�B�/B�/B�/B�cB��B�cB� B�B��B��B��B��B�B$tB.B1�B6�B2aB3�B1�B+kB~B&LB#nBB�B@B:B�B
=B�cB iB�B�)B�`B�EB��B��B��B��B��B�uB��B��B��B�uBr�Bm)Bi�Bc�BXEBQ�BA�B&LB�B��B�;B�2B��B��B��B��Bu�BgmB^5BH�BB�B9�B!�BB
�B
��B
�jB
��B
��B
�B
�	B
��B
�~B
��B
oiB
`B
[�B
ZQB
XyB
V�B
N�B
L0B
D�B
A�B
@�B
?B
;�B
8RB
6FB
1�B
%�B
�B
B	�.B	�B	�B	�B	�WB	� B	ԕB	�XB	�9B	�B	�BB	�<B	��B	�3B	�OB	��B	�XB	�B	�RB	��B	��B	��B	��B	�1B	��B	��B	��B	�lB	��B	��B	|�B	w�B	s�B	q�B	m]B	f�B	`vB	_B	aHB	m)B	a�B	e,B	jKB	m�B	m)B	i�B	sB	s�B	~�B	rB	j�B	j�B	jB	j�B	l�B	ncB	n�B	poB	r|B	r|B	l�B	bB	XEB	Q�B	M�B	MjB	K^B	MB	I�B	M�B	P�B	QB	PB	R�B	XyB	tTB	~�B	��B	��B	��B	�B	�MB	�B	�%B	��B	�B	�YB	�uB	��B	�B	~�B	�4B	��B	��B	�uB	�SB	�+B	��B	��B	��B	�GB	��B	��B	�fB	��B	��B	�lB	��B	��B	�DB	�DB	��B	�JB	�~B	��B	�~B	�JB	��B	�B	��B	��B	�hB	��B	��B	��B	�B	�_B	��B	�+B	��B	��B	��B	�B	��B	�'B	�-B	�B	�FB	��B	��B	�XB	��B	��B	��B	��B	�IB	��B	�?B	��B	�*B	�^B	��B	��B	��B	��B	��B	�gB	ÖB	�3B	��B	�B	�KB	��B	��B	��B	��B	�B	�B	��B	��B	��B	��B	��B	��B	�B	�B	��B	�B	��B	��B	�PB	��B	��B	��B	��B	�cB	�.B
 4B
 �B
B
B
oB
�B
�B
B
IB
�B
 'B
 �B
$B
'�B
)�B
/B
2aB
4�B
8�B
:�B
;�B
=qB
=�B
=�B
>BB
>�B
A�B
B�B
B�B
B�B
C-B
C�B
FtB
F�B
GB
GB
HB
IB
K�B
L�B
N<B
OBB
PB
PB
PB
R B
TaB
VB
V�B
WsB
Z�B
`�B
^jB
_�B
\]B
\�B
b�B
e�B
g�B
j�B
r|B
v`B
v+B
w�B
y	B
y�B
zB
z�B
{�B
{�B
z�B
zDB
zDB
|�B
{JB
|�B
.B
�4B
��B
�B
�B
�B
�+B
�xB
��B
��B
�bB
�4B
��B
�B
��B
��B
��B
��B
��B
��B
�bB
��B
�B
�@B
��B
�B
��B
�B
�B
�CB
�IB
�IB
��B
��B
��B
��B
��B
�-B
��B
�3B
�B
��B
��B
��B
��B
�zB
��B
�zB
�B
�*B
��B
��B
�6B
�B
��B
�HB
�B
��B
� B
��B
�aB
��B
ÖB
ÖB
�gB
ƨB
ǮB
��B
��B
�B
�B
�RB
�B
�B
ΥB
�HB
бB
�B
��B
�NB
��B
�&B
��B
��B
�2B
�B
��B
��B
�gB
�B
�9B
��B
��B
��B
�B
�KB
ٴB
��B
�#B
یB
��B
��B
��B
��B
��B
�]B
��B
��B
��B
�dB
ޞB
ޞB
�B
�B
�;B
ߤB
�vB
�B
�HB
�B
�B
�NB
�B
� B
��B
�`B
��B
�8B
�8B
�mB
�mB
�B
�
B
�>B
�>B
��B
�
B
�B
�sB
��B
��B
�B
�B
��B
�B
�B
�B
��B
�B
��B
�WB
�"B
�B
�"B
�B
��B
��B
��B
��B
��B
�B
�cB
��B
��B
�;B
�B
�oB
�oB
�;B
�oB
�;B
�oB
��B
��B
�GB
�B
�GB
�B
�B
�B
�B
�B
��B
�B
�MB
�MB
��B
�B
�B
��B
��B
�B
��B
�B
��B
��B
��B
�`B
�`B
�+B
�`B
�`B
�2B
�2B
��B
�fB
��B
�lB
�lB
�lB
��B
�>B
��B
�>B
��B
�DB
��B
�B
��B
��B
��B
��B
�xB
�JB
��B
��B
��B
��B
��B
�VB
��B
�]B
��B
�]B
�]B
��B
�]B
��B
��B iB�B�B�BABuBB�B{B{B{B�B�B�B{B�BBB�B�BBSB�BB�B�BYB�B�B�B�B�B�BfB�B�B�B�B	lB	B	7B�B
=B
rB
rB
rBDBBBBB~B�B�BVB�B(B(B(B\B�B�B�B�B�B�B�B�B�B�B�B�B�B(B�B�B�B:BB�B�B�B�B�B�BMB�B�B�B�B�B�B$BYB$BYB�B_B_B�B�B�B�B�B�B�BeBeBeBeB1B1B1B�B�B�BBBBkB�B	BBB�B�B�BBCBBBB�B�BB�B�B�BOB�B�B�B�B 'B 'B 'B �B �B �B �B \B �B!�B!�B"4B"4B"�B"�B#B#B#B#nB#�B$@B$@B$�B%FB%�B&LB&�B'�B'�B(�B(�B(�B)*B(�B(�B(�B)*B)*B)�B)*B(�B*0B*�B*0B*�B+B+B+6B+B+6B+6B+6B+6B+6B+6B+�B+kB+kB+�B+�B,qB,qB,�B,�B-CB-B-B,�B-CB-wB-�B-wB.B.B-�B.IB.IB.}B.IB.}B.IB.}B.�B.�B.�B/B.�B/B.�B0!B0�B0�B0�B1[B1[B1[B1'B1�B1[B1[B1�B1[B1�B1�B1�B1�B1�B1�B2-B2aB2�B2�B33B3�B4B4B49B49B49B4nB4nB4nB4�B4�B4�B4�B5?B6B5�B5�B5�B5�B5�B5�B5tB6B6FB6�B6zB6zB6zB6�B6zB6zB7B6�B7B7LB7�B7�B7�B8B7�B7�B8�B9XB9�B9�B9�B9�B:*B9�B:^B:^B:�B:�B:^B:�B:�B:�B:�B:�B:�B:�B:�B;0B;dB;�B:�B;0B<B<6B<�B<�B<�B=B=�B=�B=�B=�B>B>BB>BB>B>BB>BB>wB>wB>BB>B>BB>BB>B=�B>B?B?B?B>�B?HB?HB?}B?�B?�B?�B?�B@B@�B@�B@�BA BA BAUBAUB��B�B�aB�OB�aBÖB��B��B��B��B��B�BB��BÖB�UB�gB�}B��B��B��B��B��B��B��B��B�}B�qB��B�B��B��B��B�BB�B�BB��B��B�}B�BB�-B�}B��B�gB��B�aB�'B�tB�aB��B�,B֡B��B�9B�2B��B�gB՛B��B��B�9B�gB֡B��B�
B��B�
B�2B�?B�aB�?B��B�?B�2B֡B�gB�mB�9B��B�
B�2B�
B��B�sB��B�
B՛B�mB��B�9B��B�mB�9B�gB�?B��B�
B�2B��B՛B�mB��B��B�?B՛B�
B�2B��B�2B�
B՛B��B�9B�B�?B�BרB՛B��B՛B��B՛B�B�B�EB��B�EB֡B�B�sB��B�KB�?BٴB�
B��B�EB�B�yBخB�B��BٴB��BٴB�EB�B�EB�QB�B�QB�KB�KB�B��B�WB��B�WB�B�)B�QB��B�B�#B��B�WB��B�B�)BٴB��BچB��B��B��B�B��B�B�B�B�|B�B�TB��B�fB�mB�DB�8B�B�B�B��B�yB��B�B�QB�yB��B�QB�QB�B�B��B�QB�]B�B�]B�B��B��B��B��B�B��B�B��B��B��B�B�/B�)B�iB�)B�iB�)B�B��B��B�B��B� B��B�B�5B�)B��B��B�]B�B��B� B�]B�/B��B��B�5B�B�B��B��B�iB�]B� B�iB� B�B��B�;B�cB�cB�5B��B��B�B��B��B�B�cB��B�)B��B�WB�)B�B�B�KB�QB�B�B�KB�B��B�B�iB�lB�fB�DB��B��B�B�B�B�B$B"�B+�B+B6�B.B-�B+�B,�B.�B/OB3�B/�B2�B0!B2�B0UB0�B?B/�B:*B7LB9�B.}B4B0�B49B+�B/B7�BEmB/�B0�B/B/B+B/OB9XB4�B1�B/�B&B �B�B&�BM�B!bB�B!B 'B!�BVB'�B��B�BVBbB~B.B)*B`B	B�BMBB�BSB�B�B�BB�B�BVB�B�B7BoB�B�B�B�B�B�BB�B�BB%zB�BB{B�B�cB�B \BB�B�"B��B�DB�B�ZB�`BAB �BB�fB��B�B�5B��B��B�%B�B� B�sB�&B�AB��B�B��B��B�NB�B�B�zB� B��B�B�UB��B�*B�RB�0B��B�0B�'B��B��B�B��B�B�B�VB�B��B�	B�-B��B��B��B�uB�uB�B�eB��B�B��B�xB�~B��B��B�B�~B��B�=B��B�B�7B��B�rB��B��B�DB��B��B��B�SB��B��B��B��B�B|�B}"ByrBw�Bq�B{JB�bBn/Bj�Bo5Bl�Bj�Bk�B��BsBk�Bk�BjKBiDBjBjBj�Bi�Bl�BjKBg8Bl�BiBgBg�BjKBcTBjKBf2Ba|B_�Bd�Bc�B^�BZQBY�BZ�BW�BV�BXyBW�BV9BU2BU�BS�BQ�BR�B[�Ba�BN�BH�BFtBFBA�BD3BE9B>BBD3B?HB9�BS�BS&B$tB,�BCB�B�B�B�BB�BJB�B@B iB��B��B��B�B�B�vB�B�yB��B��B�sB�B��BیB��B��B�EBܒBٴB�EB��B��B��B՛B�&B��B�NB�B��B��B�BB��B��BʌB�EB�zBĜB�BŢB�gB�}B�OB��B��B�B��B��B��B�hB��B�B�=B�XB�_B�B��B��B�B�_BzDB|�B�lBrBv�Bo�Bl�BiyBl�Bi�BgmBf2Bg8BffBe�Be�Bd�Bv�Bc B\�B^jB]dBc BYBN�BPBOBN<BH�BE�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                          G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                          G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202209061825322022090618253220220906182532202209061825322022090618253220220906182532SI  SI  ARFMARFM                                                                                                                                                2021122420414220211224204142IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022010320012720220103200127QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2022010320012720220103200127QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022090607225820220906072258IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022090618253720220906182537IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022090618253720220906182537IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022090618253720220906182537IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                