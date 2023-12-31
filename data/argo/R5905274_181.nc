CDF      
      	DATE_TIME         	STRING256         STRING64   @   STRING32       STRING16      STRING8       STRING4       STRING2       N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY             	   title         Argo float vertical profile    institution       AOML   source        
Argo float     history       2023-04-26T22:32:20Z creation      
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
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20230426223220  20230426223220  5905274 5905274 Argo SIO                                                        Argo SIO                                                        DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO7315                            7315                            2B  2B  AA  SOLO_II                         SOLO_II                         8643                            8643                            SBE602 11Jan18                  SBE602 11Jan18                  853 853 @�4�N��@�4�N��11  @�4��� @�4��� @/���{@/���{�d;6P%��d;6P%�11  GPS     GPS     Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 AB  AB  AB  ?u?��H@E�@�G�@�  @�  @�G�A ��A��A   A+�A?\)A^�RA�  A���A�Q�A���A�Q�AϮA�Q�A�\)B   Bz�B  B�
B Q�B((�B0(�B8(�B@(�BH(�BP(�BX  B`(�Bh(�Bp(�Bw�
B�  B�  B�  B�  B��B��
B�B��B�(�B�{B��B��
B��
B�  B�  B��
B��
B��B�  B�{B�=qB�{B��
B��B�{B�{B�{B�{B�(�B�(�B�  B�  B��C��C  C  C�C	�C  C{C{C��C��C  C
=C  C��C
=C 
=C"  C$  C%��C(  C*  C,  C-��C0
=C2
=C4  C6  C8
=C:
=C<
=C>
=C@
=CA��CD  CF
=CH  CI�CL  CN
=CP  CR  CT
=CV
=CX
=CZ
=C\
=C^
=C`{Cb  Cd  Cf
=Ch
=Cj
=Cl
=Cn
=Cp{Cq��Ct  Cv  Cw�Cy��C|{C~  C�C���C���C�  C�  C�  C�C�  C���C�  C�C�
=C�C�  C���C�  C�
=C�C�  C�C�
=C�C�C�C���C���C�  C�  C���C�  C�C�  C�  C�C�  C�  C���C���C���C���C�  C�  C�  C�C�
=C�C�C�C�
=C�
=C�\C�C�C�\C�C�C�C�  C�C�  C���C���C���C���C���C���C���C�  C���C�  C�  C���C���C���C���C�  C�  C�  C���C���C���C�C�  C�  C���C���C���C���C���C���C�  C���C�  C�  C�C�C�  C�  C�  C�  C���C��C���C���C�  C�
=C�C�  C�
=C�C�  C�C�C�  C�  C�  C�  C�  C�C�  C�  C�
=C�C�C���C���C�  C���C���D � D �qDz�D��DxRD�qD� D�qDz�D�RD}qD  D��DD}qD�D��D	�D	� D	��D
z�D�D��D  D� D�qD}qD  D� D  D� D��Dz�DD�DD�D�D��D�D}qD�RD}qD�D� D�RD� D�D�=DD� DD� D�D� D�D� D�D� D�qD��D�D}qD �D � D!  D!}qD"�D"xRD"�qD#��D$�D$��D%�D%� D%�qD&z�D&�qD'��D(  D(� D(�RD)� D*  D*z�D+D+}qD+�qD,��D-�D-� D-��D.��D.�qD/� D0  D0z�D1  D1}qD2�D2� D3�D3z�D3�qD4� D4��D5}qD6  D6}qD7�D7��D7�qD8� D9�D9z�D9��D:� D:�qD;}qD<�D<}qD=  D=�D>  D>� D>�qD?� D@�D@z�DA  DA�DB  DB� DCDC� DD�DD��DE�DE� DF  DF��DG�DG��DH�DH�DI�DI}qDI��DJ}qDKDK��DK�qDL��DM  DM��DM�qDN��DN��DO��DPDP}qDQ�DQ� DQ�qDR�DR�qDS}qDS�qDT}qDU�DU� DV�DV}qDW  DW��DW�qDX� DYDY}qDZ  DZ�DZ�qD[z�D\  D\}qD\�qD]��D]�qD^��D^�qD_��D`�D`}qD`�qDaz�Da��Db}qDb��Dcz�Dc��Dd}qDe�De� De�qDf�Dg  Dg��Dh�Dh� Di  Diz�Dj�Dj� Dj�qDk��Dl  Dl��Dm�Dm��Dn  Dn��Do�Do� Dp�Dp� Dq  Dq� Dr  Dr� Ds�Ds�Dt�Dt� Dt�qDuz�Dv  Dv� Dv�qDw�Dw��Dx� Dx��Dy��Dz�Dz}qDz�qD{��D|  D|� D|�qD}z�D~�D~� D�D��D�HD�@ D�~�D�� D���D�>�D�� D�� D�  D�AHD�� D�� D�  D�AHD�~�D���D�  D�>�D��HD�� D�  D�@ D�~�D���D�  D�AHD�� D��qD�  D�@ D�� D��HD�  D�@ D�� D��HD�  D�@ D��HD�� D�  D�>�D���D��qD�  D�>�D��HD���D�  D�@ D�~�D���D���D�AHD�� D�� D�HD�@ D�~�D���D�HD�AHD��HD��HD�  D�@ D���D�� D�  D�>�D�� D��HD�HD�AHD�}qD�� D�HD�AHD��HD���D�  D�@ D�~�D�� D�HD�>�D�� D��HD���D�=qD�~�D�� D�HD�B�D���D��HD�  D�@ D�� D��HD�HD�AHD��HD�� D��D�@ D�~�D��HD�  D�>�D��HD��HD�  D�>�D��HD��HD��D�AHD��HD�� D���D�AHD�� D�� D�HD�@ D�� D�� D���D�AHD�� D���D�  D�AHD��HD��HD�  D�=qD�� D�� D��D�AHD�� D�� D�  D�AHD�� D�� D��qD�>�D��HD��qD�HD�>�D�� D���D���D�@ D���D��HD�HD�AHD�� D���D���D�AHD���D��HD���D�@ D���D��HD���D�>�D�~�D���D��qD�B�D�� D�� D�  D�=qD�� D�� D��qD�>�D�}qD�D��D�AHD���D��HD��D�B�D��HD��HD���D�>�D�~�D���D���D�AHD��HD�� D�  D�@ D�~�D���D���D�@ D�~�D��HD�HD�AHD�~�D���D��qD�>�D�� D��HD�  D�B�D�~�D��)D���D�=qD�~�D�� D��D�@ D��HD���D�  D�>�D�}qD���D�  D�>�D�~�D��qD��D�AHD���D���D�HD�>�D�� D��)D�  D�>�D���D�� D��)D�>�D��HD���D��)D�<)D�~�D��HD��D�AHD�~�D���D�  D�AHDHD�� D�  D�@ DÀ D��HD���D�<)D�~�D��HD�HD�B�DŁHD�� D���D�@ Dƀ D�� D�  D�@ D�}qD�� D���D�@ D�}qD��HD�HD�@ Dɀ D�� D�  D�@ Dʀ DʽqD�HD�@ Dˀ D˾�D���D�@ D́HD�� D���D�>�D�~�D;�D�  D�@ D�}qD�� D��D�@ Dς�D�D�  D�<)D�~�D��HD���D�=qD�}qDѼ)D��qD�@ DҀ DҾ�D�  D�@ DӀ DӾ�D���D�=qDԁHD�D��qD�@ DՂ�Dվ�D�HD�AHDցHD��HD��D�@ D׀ D��HD��D�AHD�}qDؽqD�HD�AHD�~�D�D��D�AHDځHDھ�D���D�=qDہHD�D�HD�AHD܁HD��HD�HD�@ D�}qD��HD��D�AHDނ�D��HD���D�>�D�~�D߽qD��qD�>�D�� D�� D�HD�AHD� DᾸD���D�>�D�~�D�qD���D�@ D� D㾸D�HD�@ D�}qD�qD�  D�AHD�~�D徸D�  D�AHD� D�� D�  D�@ D�~�D羸D�HD�@ D�HD��HD�  D�>�D�HD�� D���D�AHD� D�� D�  D�B�D�~�D�qD�  D�@ D�~�D쾸D�  D�=qD�~�D�� D�  D�AHD� D��HD�HD�@ D�~�D�� D�HD�>�D�~�D�D���D�@ D� D��HD��D�@ D�}qD�D���D�@ D� D�� D�HD�@ D�~�D��qD��)D�AHD��HD�� D�  D�>�D�}qD��qD��)D�=qD�~�D�� D��D�B�D���D�� D��D�@ D��HD�D��D�>�D�~�D�� >�Q�>���?L��?���?Ǯ?��@z�@+�@J=q@\(�@u@�=q@�
=@��@���@�  @�{@�p�@���@�
=A ��A
=A(�A33A��A\)A%�A,(�A1�A9��A@��AFffAMp�AS�
AX��A]p�Adz�Aj=qAqG�AxQ�A�Q�A��A�
=A��\A�A���A��A�\)A��\A�{A�G�A�p�A�G�A���A�  A��
A�
=A��A�p�A���A���Aȣ�A�(�A�  A��
A׮A�33A�{AᙚA�p�A���A�z�A��A�z�A�  A��A�
=BG�B�RB  B��B�HBQ�B	�B
=Bz�B�B
=B(�B�B{B33B(�B��BB�HB�
B��B��B�RB�B��B�B
=B�
B!�B"=qB#�
B$��B%�B'33B((�B)G�B*ffB+�B,��B-��B.�RB0  B0��B1B2�RB4  B4��B6{B733B8(�B9G�B:�RB;�B<��B>{B?33B@z�BAp�BB�\BC�BD��BEBF�HBG�BH��BIp�BJ�\BK�BL��BMBN�HBO�
BP��BRffBS�BT��BU��BV�\BW\)BXz�BYp�BZ�\B[\)B\��B^{B_33B`(�B`��Ba�Bb�RBc�
Bd��BeBf�RBg�Bhz�Bi��Bj�\Bk�Bl��BmBn�RBo�Bp��Bqp�BrffBs
=Bs�
Bt��BuBv�\Bw�
By�Bz{B{33B|  B}G�B~=qB
=B�B�ffB���B�\)B��
B��\B��B���B�{B��\B��B��B�=qB���B���B�p�B�  B�ffB�
=B�B�=qB���B�\)B�B�(�B���B�33B��B�(�B��HB�\)B��B�Q�B���B�33B��B�(�B���B�\)B��
B�Q�B���B��B���B�(�B���B�G�B�B�Q�B���B�
=B�p�B�  B���B��B��B�(�B��\B��HB�\)B��
B�Q�B���B�G�B��
B�Q�B���B�p�B��B�ffB��HB�33B���B�{B��\B�
=B���B�=qB��RB���B�p�B��B�ffB��B���B�  B�=qB��RB�33B���B�(�B��RB�33B��B�(�B�ffB���B�33B���B�=qB���B�33B��B�  B�Q�B���B�33B��
B�=qB��RB��B�p�B��B�Q�B��HB��B�{B��\B�
=B�G�B��B�(�B��RB�\)B��
B�ffB��RB�
=B��B�(�B��RB�G�B���B��B�z�B�33B��B�=qB£�B��BÙ�B�=qB�
=Bř�B�(�BƏ\B�G�B�  BȸRB�\)B�B�ffB��HB�B�ffB���B�\)B��B���B�p�B��B�ffB���B��
B�z�B��BӅB�(�B���Bՙ�B�=qB��HB�G�B��BظRB�p�B�{B�z�B�33B��
Bܣ�B�G�B�B�ffB�33B��B�Q�B���B�B�z�B�
=B�B�Q�B�33B��B�ffB�
=B�  B��B�\)B��
B��B�p�B�=qB�RB�p�B�Q�B�
=BB�=qB�
=B��
B�\B��B��
B���B��B�{B��RB�B�z�B��B�B��\B�\)B��B��\B�p�B�=qB���B��C 33C �\C �
C(�C�C
=CG�C��C�C�CC{C�\C��C33C�\C
=CffC�C�C�C�
C(�C��C	
=C	G�C	��C
(�C
�C
�
CG�C�RC��CQ�C��C33Cp�C�
CG�C�C�C\)CC  CffC�HC�Cp�C�C=qC�\C
=Cp�C�RC(�C��C�
C33C�C  CQ�C��C(�Cp�C�HC=qC�C�HC\)C�RC
=C�C�C(�C��C
=CG�C�RC33Cp�C�
CG�C�\C�Cz�C�RC �C ��C �
C!33C!�C!��C"\)C"�
C#{C#�\C#��C$33C$��C%�C%Q�C%�RC&33C&�C&�
C'G�C'��C'�C(ffC(�RC)
=C)z�C)�
C*�C*��C*�HC+33C+�C,
=C,Q�C,��C-{C-p�C-�C.33C.�\C/{C/Q�C/��C033C0p�C0�C133C1�C2
=C2\)C2��C3(�C3p�C3�HC4(�C4�C4��C533C5�C5��C6\)C6C7
=C7�C7��C8=qC8��C8�HC9\)C9��C:�C:ffC:�C;(�C;\)C;C<  C<(�C<�\C<�C=
=C=33C=Q�C=�C=�C>  C>{C>\)C>�C>��C>�HC>��C?=qC?\)C?�\C?��C?�HC@33C@=qC@�C@��C@��CA{CA�CAz�CA�CA�
CA�CB33CBQ�CBz�CB��CB�
CC�CC=qCC�CC��CC�HCD
=CD33CDz�CD�\CD�HCD�CE=qCEQ�CE��CE�CF  CF�CFG�CF�\CF��CF��CG  CGQ�CGffCG�RCG��CH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                      ?u?��H@E�@�G�@�  @�  @�G�A ��A��A   A+�A?\)A^�RA�  A���A�Q�A���A�Q�AϮA�Q�A�\)B   Bz�B  B�
B Q�B((�B0(�B8(�B@(�BH(�BP(�BX  B`(�Bh(�Bp(�Bw�
B�  B�  B�  B�  B��B��
B�B��B�(�B�{B��B��
B��
B�  B�  B��
B��
B��B�  B�{B�=qB�{B��
B��B�{B�{B�{B�{B�(�B�(�B�  B�  B��C��C  C  C�C	�C  C{C{C��C��C  C
=C  C��C
=C 
=C"  C$  C%��C(  C*  C,  C-��C0
=C2
=C4  C6  C8
=C:
=C<
=C>
=C@
=CA��CD  CF
=CH  CI�CL  CN
=CP  CR  CT
=CV
=CX
=CZ
=C\
=C^
=C`{Cb  Cd  Cf
=Ch
=Cj
=Cl
=Cn
=Cp{Cq��Ct  Cv  Cw�Cy��C|{C~  C�C���C���C�  C�  C�  C�C�  C���C�  C�C�
=C�C�  C���C�  C�
=C�C�  C�C�
=C�C�C�C���C���C�  C�  C���C�  C�C�  C�  C�C�  C�  C���C���C���C���C�  C�  C�  C�C�
=C�C�C�C�
=C�
=C�\C�C�C�\C�C�C�C�  C�C�  C���C���C���C���C���C���C���C�  C���C�  C�  C���C���C���C���C�  C�  C�  C���C���C���C�C�  C�  C���C���C���C���C���C���C�  C���C�  C�  C�C�C�  C�  C�  C�  C���C��C���C���C�  C�
=C�C�  C�
=C�C�  C�C�C�  C�  C�  C�  C�  C�C�  C�  C�
=C�C�C���C���C�  C���C���D � D �qDz�D��DxRD�qD� D�qDz�D�RD}qD  D��DD}qD�D��D	�D	� D	��D
z�D�D��D  D� D�qD}qD  D� D  D� D��Dz�DD�DD�D�D��D�D}qD�RD}qD�D� D�RD� D�D�=DD� DD� D�D� D�D� D�D� D�qD��D�D}qD �D � D!  D!}qD"�D"xRD"�qD#��D$�D$��D%�D%� D%�qD&z�D&�qD'��D(  D(� D(�RD)� D*  D*z�D+D+}qD+�qD,��D-�D-� D-��D.��D.�qD/� D0  D0z�D1  D1}qD2�D2� D3�D3z�D3�qD4� D4��D5}qD6  D6}qD7�D7��D7�qD8� D9�D9z�D9��D:� D:�qD;}qD<�D<}qD=  D=�D>  D>� D>�qD?� D@�D@z�DA  DA�DB  DB� DCDC� DD�DD��DE�DE� DF  DF��DG�DG��DH�DH�DI�DI}qDI��DJ}qDKDK��DK�qDL��DM  DM��DM�qDN��DN��DO��DPDP}qDQ�DQ� DQ�qDR�DR�qDS}qDS�qDT}qDU�DU� DV�DV}qDW  DW��DW�qDX� DYDY}qDZ  DZ�DZ�qD[z�D\  D\}qD\�qD]��D]�qD^��D^�qD_��D`�D`}qD`�qDaz�Da��Db}qDb��Dcz�Dc��Dd}qDe�De� De�qDf�Dg  Dg��Dh�Dh� Di  Diz�Dj�Dj� Dj�qDk��Dl  Dl��Dm�Dm��Dn  Dn��Do�Do� Dp�Dp� Dq  Dq� Dr  Dr� Ds�Ds�Dt�Dt� Dt�qDuz�Dv  Dv� Dv�qDw�Dw��Dx� Dx��Dy��Dz�Dz}qDz�qD{��D|  D|� D|�qD}z�D~�D~� D�D��D�HD�@ D�~�D�� D���D�>�D�� D�� D�  D�AHD�� D�� D�  D�AHD�~�D���D�  D�>�D��HD�� D�  D�@ D�~�D���D�  D�AHD�� D��qD�  D�@ D�� D��HD�  D�@ D�� D��HD�  D�@ D��HD�� D�  D�>�D���D��qD�  D�>�D��HD���D�  D�@ D�~�D���D���D�AHD�� D�� D�HD�@ D�~�D���D�HD�AHD��HD��HD�  D�@ D���D�� D�  D�>�D�� D��HD�HD�AHD�}qD�� D�HD�AHD��HD���D�  D�@ D�~�D�� D�HD�>�D�� D��HD���D�=qD�~�D�� D�HD�B�D���D��HD�  D�@ D�� D��HD�HD�AHD��HD�� D��D�@ D�~�D��HD�  D�>�D��HD��HD�  D�>�D��HD��HD��D�AHD��HD�� D���D�AHD�� D�� D�HD�@ D�� D�� D���D�AHD�� D���D�  D�AHD��HD��HD�  D�=qD�� D�� D��D�AHD�� D�� D�  D�AHD�� D�� D��qD�>�D��HD��qD�HD�>�D�� D���D���D�@ D���D��HD�HD�AHD�� D���D���D�AHD���D��HD���D�@ D���D��HD���D�>�D�~�D���D��qD�B�D�� D�� D�  D�=qD�� D�� D��qD�>�D�}qD�D��D�AHD���D��HD��D�B�D��HD��HD���D�>�D�~�D���D���D�AHD��HD�� D�  D�@ D�~�D���D���D�@ D�~�D��HD�HD�AHD�~�D���D��qD�>�D�� D��HD�  D�B�D�~�D��)D���D�=qD�~�D�� D��D�@ D��HD���D�  D�>�D�}qD���D�  D�>�D�~�D��qD��D�AHD���D���D�HD�>�D�� D��)D�  D�>�D���D�� D��)D�>�D��HD���D��)D�<)D�~�D��HD��D�AHD�~�D���D�  D�AHDHD�� D�  D�@ DÀ D��HD���D�<)D�~�D��HD�HD�B�DŁHD�� D���D�@ Dƀ D�� D�  D�@ D�}qD�� D���D�@ D�}qD��HD�HD�@ Dɀ D�� D�  D�@ Dʀ DʽqD�HD�@ Dˀ D˾�D���D�@ D́HD�� D���D�>�D�~�D;�D�  D�@ D�}qD�� D��D�@ Dς�D�D�  D�<)D�~�D��HD���D�=qD�}qDѼ)D��qD�@ DҀ DҾ�D�  D�@ DӀ DӾ�D���D�=qDԁHD�D��qD�@ DՂ�Dվ�D�HD�AHDցHD��HD��D�@ D׀ D��HD��D�AHD�}qDؽqD�HD�AHD�~�D�D��D�AHDځHDھ�D���D�=qDہHD�D�HD�AHD܁HD��HD�HD�@ D�}qD��HD��D�AHDނ�D��HD���D�>�D�~�D߽qD��qD�>�D�� D�� D�HD�AHD� DᾸD���D�>�D�~�D�qD���D�@ D� D㾸D�HD�@ D�}qD�qD�  D�AHD�~�D徸D�  D�AHD� D�� D�  D�@ D�~�D羸D�HD�@ D�HD��HD�  D�>�D�HD�� D���D�AHD� D�� D�  D�B�D�~�D�qD�  D�@ D�~�D쾸D�  D�=qD�~�D�� D�  D�AHD� D��HD�HD�@ D�~�D�� D�HD�>�D�~�D�D���D�@ D� D��HD��D�@ D�}qD�D���D�@ D� D�� D�HD�@ D�~�D��qD��)D�AHD��HD�� D�  D�>�D�}qD��qD��)D�=qD�~�D�� D��D�B�D���D�� D��D�@ D��HD�D��D�>�D�~�D�� >�Q�>���?L��?���?Ǯ?��@z�@+�@J=q@\(�@u@�=q@�
=@��@���@�  @�{@�p�@���@�
=A ��A
=A(�A33A��A\)A%�A,(�A1�A9��A@��AFffAMp�AS�
AX��A]p�Adz�Aj=qAqG�AxQ�A�Q�A��A�
=A��\A�A���A��A�\)A��\A�{A�G�A�p�A�G�A���A�  A��
A�
=A��A�p�A���A���Aȣ�A�(�A�  A��
A׮A�33A�{AᙚA�p�A���A�z�A��A�z�A�  A��A�
=BG�B�RB  B��B�HBQ�B	�B
=Bz�B�B
=B(�B�B{B33B(�B��BB�HB�
B��B��B�RB�B��B�B
=B�
B!�B"=qB#�
B$��B%�B'33B((�B)G�B*ffB+�B,��B-��B.�RB0  B0��B1B2�RB4  B4��B6{B733B8(�B9G�B:�RB;�B<��B>{B?33B@z�BAp�BB�\BC�BD��BEBF�HBG�BH��BIp�BJ�\BK�BL��BMBN�HBO�
BP��BRffBS�BT��BU��BV�\BW\)BXz�BYp�BZ�\B[\)B\��B^{B_33B`(�B`��Ba�Bb�RBc�
Bd��BeBf�RBg�Bhz�Bi��Bj�\Bk�Bl��BmBn�RBo�Bp��Bqp�BrffBs
=Bs�
Bt��BuBv�\Bw�
By�Bz{B{33B|  B}G�B~=qB
=B�B�ffB���B�\)B��
B��\B��B���B�{B��\B��B��B�=qB���B���B�p�B�  B�ffB�
=B�B�=qB���B�\)B�B�(�B���B�33B��B�(�B��HB�\)B��B�Q�B���B�33B��B�(�B���B�\)B��
B�Q�B���B��B���B�(�B���B�G�B�B�Q�B���B�
=B�p�B�  B���B��B��B�(�B��\B��HB�\)B��
B�Q�B���B�G�B��
B�Q�B���B�p�B��B�ffB��HB�33B���B�{B��\B�
=B���B�=qB��RB���B�p�B��B�ffB��B���B�  B�=qB��RB�33B���B�(�B��RB�33B��B�(�B�ffB���B�33B���B�=qB���B�33B��B�  B�Q�B���B�33B��
B�=qB��RB��B�p�B��B�Q�B��HB��B�{B��\B�
=B�G�B��B�(�B��RB�\)B��
B�ffB��RB�
=B��B�(�B��RB�G�B���B��B�z�B�33B��B�=qB£�B��BÙ�B�=qB�
=Bř�B�(�BƏ\B�G�B�  BȸRB�\)B�B�ffB��HB�B�ffB���B�\)B��B���B�p�B��B�ffB���B��
B�z�B��BӅB�(�B���Bՙ�B�=qB��HB�G�B��BظRB�p�B�{B�z�B�33B��
Bܣ�B�G�B�B�ffB�33B��B�Q�B���B�B�z�B�
=B�B�Q�B�33B��B�ffB�
=B�  B��B�\)B��
B��B�p�B�=qB�RB�p�B�Q�B�
=BB�=qB�
=B��
B�\B��B��
B���B��B�{B��RB�B�z�B��B�B��\B�\)B��B��\B�p�B�=qB���B��C 33C �\C �
C(�C�C
=CG�C��C�C�CC{C�\C��C33C�\C
=CffC�C�C�C�
C(�C��C	
=C	G�C	��C
(�C
�C
�
CG�C�RC��CQ�C��C33Cp�C�
CG�C�C�C\)CC  CffC�HC�Cp�C�C=qC�\C
=Cp�C�RC(�C��C�
C33C�C  CQ�C��C(�Cp�C�HC=qC�C�HC\)C�RC
=C�C�C(�C��C
=CG�C�RC33Cp�C�
CG�C�\C�Cz�C�RC �C ��C �
C!33C!�C!��C"\)C"�
C#{C#�\C#��C$33C$��C%�C%Q�C%�RC&33C&�C&�
C'G�C'��C'�C(ffC(�RC)
=C)z�C)�
C*�C*��C*�HC+33C+�C,
=C,Q�C,��C-{C-p�C-�C.33C.�\C/{C/Q�C/��C033C0p�C0�C133C1�C2
=C2\)C2��C3(�C3p�C3�HC4(�C4�C4��C533C5�C5��C6\)C6C7
=C7�C7��C8=qC8��C8�HC9\)C9��C:�C:ffC:�C;(�C;\)C;C<  C<(�C<�\C<�C=
=C=33C=Q�C=�C=�C>  C>{C>\)C>�C>��C>�HC>��C?=qC?\)C?�\C?��C?�HC@33C@=qC@�C@��C@��CA{CA�CAz�CA�CA�
CA�CB33CBQ�CBz�CB��CB�
CC�CC=qCC�CC��CC�HCD
=CD33CDz�CD�\CD�HCD�CE=qCEQ�CE��CE�CF  CF�CFG�CF�\CF��CF��CG  CGQ�CGffCG�RCG��CH
=G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�  A�  A�  A���A���A���A���A���A���A���A���A�  A�A�A�A�A�A�A�A�A�%A�%A�%A�1A�1A�
=A�JA�JA�1A�VA�VA�JA�VA�JA�VA��A�&�A�M�A�^5A�|�Aҟ�AҴ9AҶFAҶFAҶFAҶFAҶFAҶFAҸRAҶFAҥ�A�K�Aї�A�XA��A���A�ȴA�oA���A���A�;dA�-A�bA���A�A��A�bNA�p�A��A�r�A�ffA��
A�S�A���A�1'A���A�ȴA���A��!A�A��
A���A��A���A�t�A�bA�I�A��9A��A���A�7LA��jA�|�A�A��RA��A��#A�/A���A��A��jA�$�A���A�A��FA�|�A��A��RA��A���A|��Az�Ax�jAtQ�An��Ah��Ac|�Aa�A_ƨA_%A]�AV��APVAO�AN��AM�AI�ADffAC�
AA��A=��A;%A7�;A4��A2v�A1��A1dZA0��A.�A-
=A+��A*�+A(�+A'��A&�A$�A!��A �DA��Ap�A�AƨA�A�A��AbNA;dA(�Al�A��A�A��A;dA��A��A��A�DA�mAt�A��A��A�FA�FA��A^5A
��A
�A	/A�A	`BA	l�A
=A
�DA
bNA
M�A
=qA	ƨA�`A&�AM�AbAXA��AZA�A��A|�A��AVA&�A ĜA ��A ȴA �j@�K�@���@� �@���@�K�@�33@���@�~�@�$�@���@��@��;@�S�@�;d@�@�@���@�$�@��h@�A�@��@���@�|�@�ȴ@�p�@�9@�@�@�9X@�F@�|�@��@��@�|�@�\)@�@���@�h@���@�I�@�(�@畁@��@�-@��@��@䛦@�1@�P@�l�@��@���@�\@�v�@�V@�h@���@�l�@�$�@�`B@�V@ܼj@�z�@�I�@�(�@��;@ۅ@�ȴ@���@�V@�z�@�l�@֧�@�$�@��@�@Ցh@ԓu@��@�$�@ѡ�@�Ĝ@�(�@��m@�t�@�@���@Χ�@�5?@�@�?}@���@̬@̛�@̋D@��
@��@���@�n�@�@��#@ɑh@�/@ȼj@�j@�b@�\)@��y@�v�@�$�@�$�@�-@��@�O�@î@�
=@\@��@���@�hs@��`@�bN@�I�@��@��
@�K�@�@�ȴ@�n�@�@���@��^@�/@�V@�z�@�t�@��H@�n�@��h@�/@���@�j@���@�;d@�ȴ@���@�V@�$�@��T@���@�hs@�/@�V@��/@�bN@���@���@��+@��@���@�7L@���@��u@�I�@���@��@�"�@��H@��+@�ff@�=q@��h@�7L@�7L@��@���@� �@���@�|�@�K�@�"�@�"�@��y@��R@���@�^5@�J@���@�G�@��9@�(�@�ƨ@���@��@�
=@��@���@���@�n�@�-@��@�O�@��@���@�j@�9X@��F@�33@���@�{@���@��@�?}@�V@��@�z�@�Q�@� �@���@�33@�
=@��@���@��!@���@��+@�5?@���@�?}@�&�@���@��/@�z�@�A�@�1@��P@�K�@�
=@���@�ff@�E�@�-@�J@��T@���@���@�p�@�/@���@��D@�Z@�(�@��m@�S�@���@�{@�x�@�?}@�r�@���@��@�\)@��H@�^5@��@���@��7@�7L@��@��@�/@�&�@�%@���@���@��@�A�@��w@�l�@�C�@��@��!@�{@���@�O�@��@�%@��`@��j@��u@�r�@�9X@�1@��P@�K�@��y@�E�@��@��@��-@�O�@�/@��@���@�A�@�(�@�b@��w@�S�@��@�ȴ@��R@��+@�5?@��T@���@��@�G�@���@��@�r�@�bN@�I�@� �@�1@��F@�S�@���@��R@��!@���@�v�@�V@�-@�@���@�hs@��@��`@���@��j@���@��@�I�@��@�1@��@~�y@~$�@}�@|Z@|(�@{��@{��@{o@y��@yX@xr�@w�@w�w@w�@w\)@vV@v{@u�h@u`B@u�@tZ@sƨ@st�@s33@r�!@r�\@r~�@rn�@r=q@q�^@q7L@q%@pr�@p1'@o�w@o+@n��@nv�@m��@mp�@mp�@m`B@mO�@m/@l��@l�/@l�@lZ@k�m@k��@kS�@j�@j�!@j~�@i�@ihs@h�`@h�9@h�@hb@g��@gK�@g
=@f5?@e?}@d�/@d�D@dI�@c�
@b��@b^5@b=q@b-@b�@bJ@a��@a%@`��@`r�@` �@_�;@_�w@_l�@_
=@^�@^ȴ@]��@]�h@]�@]�@]?}@\�D@\1@[�m@[��@[S�@[33@Z�@Z-@ZJ@Yx�@X��@X�@W�@W�@Vȴ@Vff@V5?@U��@U?}@UV@TZ@S�m@Sƨ@Sƨ@S�@R��@R=q@Q�^@Q�7@Qx�@QX@Q�@P�@PA�@P �@O�@O�w@O;d@N�R@N�+@Nff@NV@N@M�-@M�h@M�@L�@L�/@L�j@L��@LI�@K�m@Ko@J~�@JJ@I�^@Ihs@H��@H �@Hb@G�;@G+@Fȴ@Fv�@FE�@E��@Ep�@E?}@D��@D�@D��@D��@Dz�@C�F@CS�@C33@B��@B��@B^5@A��@A�#@Ax�@@�`@@A�@?��@?|�@?l�@?\)@?K�@?�@>�@>v�@>$�@=�h@=?}@=�@<�@<�D@<1@;�F@;�@;t�@;dZ@;@:��@:n�@:=q@9��@9G�@8�u@81'@8b@7�@7�;@7�;@7��@7\)@6�R@6�R@6��@6��@6$�@5�T@5�-@5��@5/@4�/@49X@3�m@3dZ@2�@2~�@2M�@2=q@1��@1��@1X@0��@0�@0bN@/�@/\)@.��@.v�@-�T@-�-@-�h@-`B@-V@,�j@,�j@,�@,I�@,9X@,1@+�F@+S�@*�H@*�\@*~�@*-@*J@)�#@)��@)x�@)7L@)%@(�`@(�9@(��@(r�@(A�@'�@'�w@'��@'��@'�P@'K�@'
=@&ȴ@&��@&��@&v�@&E�@&$�@&{@%�-@%�h@%�@$��@$j@$1@#��@#��@#dZ@#33@#o@"�@"��@"n�@"=q@"-@"�@!��@!�^@!x�@!hs@!7L@!%@ �`@ Ĝ@ ��@ �u@ r�@ bN@ bN@ A�@ b@�@�w@��@��@l�@+@+@+@ȴ@$�@�@@p�@V@z�@�@�
@��@t�@C�@33@o@�!@=q@-@-@-@J@�#@�7@hs@7L@%@��@��@�@Q�@1'@b@��@l�@�y@��@��@��@V@5?@{@{@@`B@?}@/@��@�@��@�/@�@z�@z�@Z@�@��@��@t�@C�@33@"�@@�H@�H@��@M�@-@�@�#@��@�7@x�@X@X@7L@�`@Ĝ@��@�u@r�@1'@ �@ �@  @�w@|�@\)@K�@+@
=@��@ȴ@��@ff@E�@{@��@��@`B@/@�/@��@�D@z�@j@(�@��@�m@�F@dZ@@
�H@
�H@
�!@
~�@
~�@
n�@
^5@
J@	�#@	��@	hs@	G�@	G�@	%@Ĝ@��@bN@b@�;@�w@�@l�@K�@;d@;d@+@�@
=@��@ȴ@��A�  A�  A�  A�  A�  A���A���A���A���A���A���A���A�  A�  A�  A�  A���A���A���A���A���A���A�  A���A���A�  A�  A�  A�  A���A���A���A���A���A�  A�A�A�%A�A�A�A�A�A�A�A�A�A�A�%A�1A�%A�%A�A�  A�A�A�A�A�%A�%A�%A�A�A�A�  A�A�A�A�A�%A�1A�1A�%A�A�A�A�A�A�%A�1A�1A�
=A�1A�1A�
=A�1A�A�%A�%A�%A�%A�A�%A�%A�%A�1A�1A�1A�
=A�
=A�
=A�JA�JA�JA�JA�JA�JA�
=A�
=A�
=A�
=A�
=A�1A�
=A�
=A�1A�
=A�
=A�
=A�
=A�
=A�
=A�JA�JA�JA�JA�
=A�
=A�1A�JA�VA�VA�VA�JA�
=A�JA�JA�JA�JA�JA�VA�VA�bA�VA�JA�JA�JA�VA�VA�JA�JA�
=A�
=A�
=A�JA�VA�JA�bA�oA�bA�JA�
=A�
=A�
=A�JA�JA�bA�bA�bA�bA�bA�{A�{A�oA�bA�VA�VA�JA�VA�{A�oA��A��A��A� �A��A��A��A��A��A�7LA�;dA�?}A�C�A�E�A�G�A�K�A�S�A�S�A�VA�XA�XA�ZA�ZA�ZA�`BA�bNA�bNA�ffA�hsA�v�A�|�AҁA҇+AҋDAҏ\Aҕ�Aҗ�Aқ�Aҝ�Aҝ�Aҧ�Aң�Aҩ�AҰ!AҰ!AҰ!AҲ-AҴ9AҴ9AҶFAҸRAҸRAҸRAҶFAҴ9AҴ9AҶFAҸRAҺ^AҸRAҶFAҶFAҴ9AҴ9AҶFAҸRAҺ^AҸRAҶFAҴ9AҴ9AҴ9AҴ9AҶFAҶFAҸRAҸRAҸRAҸRAҸRAҸRAҶFAҴ9AҴ9AҴ9AҴ9AҶFAҸRAҸRAҸRAҸRAҶFAҴ9AҴ9AҶFAҸRAҺ^AҸRAҶFAҴ9AҴ9AҸRAҸRAҺ^AҺ^AҺ^AҸRAҶFAҴ9AҴ9AҶFAҸRAҸRAҸRAҶFAҴ9AҲ-AҰ!AҰ!AҲ-AҲ-Aҩ�Aҥ�Aҡ�Aҟ�Aҟ�Aҡ�Aҝ�Aҗ�A�z�A�/A�33A�7LA�K�A�&�A�+A�&�A��A�oAѕ�A�-A�&�A�$�A��A�JA�A��`A��AЃA���A�z�A�C�A�A΃A�oA�bNA��/A���A�|�AɋDA��A���AȺ^Aț�AȃA�^5A�O�A� �A��HAǣ�AǃA�ffA�\)A�S�A�E�A�;dA��A��Aƺ^A�dZA�E�A�+A��AžwAť�A�dZA�A�A�VA�ƨAć+A�5?A��Aã�AÉ7A�l�A�JA©�A�^5A��A�bA��;A�n�A�E�A�+A� �A��
A�E�A�ƨA�O�A�+A��A���A��
A��FA��hA�r�A�VA�$�A��A�Q�A�VA���A���A���A�v�A�VA��A��A��RA�z�A�5?A��
A���A��A�n�A�I�A�;dA�9XA�33A�(�A�$�A�$�A���A��TA��jA��uA�^5A�-A��A���A��hA�E�A�;dA��A��yA��HA��;A��A���A���A�jA��jA�"�A�ȴA�x�A�K�A�1A���A��A��7A�jA�VA�C�A�&�A�VA��HA���A��RA���A�t�A�ZA�;dA�33A�oA��A��A��A���A�|�A��
A��+A�A�A�;dA� �A�  A��^A��A�v�A�A��A��A��mA��yA��mA��HA��/A���A��jA��-A��!A��!A���A��uA�z�A�G�A��A���A��jA�ffA��A���A��wA���A�^5A���A��uA�dZA�XA�Q�A�O�A�?}A�&�A�%A��A���A��RA���A�l�A�A�ĜA�~�A��A���A�bNA�G�A�7LA�-A��A�VA�1A���A��#A���A���A���A���A���A��/A��`A���A��-A��A���A���A�ffA�A�-A���A�p�A�%A�A��#A��A��A���A��TA���A�\)A�
=A��/A��wA���A��A�VA�1'A��^A��A�oA��HA��!A�G�A���A�hsA�;dA�A��^A�^5A��TA��9A�\)A��A��TA��RA��uA�r�A�G�A�;dA�5?A��A�{A��A��A��`A��#A���A���A���A��-A��RA��!A��9A���A���A���A�`BA��A�x�A�-A�K�A���A�x�A�O�A�/A��A�A���A���A��jA���A��7A�l�A�l�A�l�A�^5A�A�A�7LA�/A�1'A�/A�bA��A��A��mA��#A���A���A���A�ȴA�ƨA��jA��-A��!A��!A��A���A���A��hA��PA��PA��+A��DA�~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                      A�  A�  A�  A���A���A���A���A���A���A���A���A�  A�A�A�A�A�A�A�A�A�%A�%A�%A�1A�1A�
=A�JA�JA�1A�VA�VA�JA�VA�JA�VA��A�&�A�M�A�^5A�|�Aҟ�AҴ9AҶFAҶFAҶFAҶFAҶFAҶFAҸRAҶFAҥ�A�K�Aї�A�XA��A���A�ȴA�oA���A���A�;dA�-A�bA���A�A��A�bNA�p�A��A�r�A�ffA��
A�S�A���A�1'A���A�ȴA���A��!A�A��
A���A��A���A�t�A�bA�I�A��9A��A���A�7LA��jA�|�A�A��RA��A��#A�/A���A��A��jA�$�A���A�A��FA�|�A��A��RA��A���A|��Az�Ax�jAtQ�An��Ah��Ac|�Aa�A_ƨA_%A]�AV��APVAO�AN��AM�AI�ADffAC�
AA��A=��A;%A7�;A4��A2v�A1��A1dZA0��A.�A-
=A+��A*�+A(�+A'��A&�A$�A!��A �DA��Ap�A�AƨA�A�A��AbNA;dA(�Al�A��A�A��A;dA��A��A��A�DA�mAt�A��A��A�FA�FA��A^5A
��A
�A	/A�A	`BA	l�A
=A
�DA
bNA
M�A
=qA	ƨA�`A&�AM�AbAXA��AZA�A��A|�A��AVA&�A ĜA ��A ȴA �j@�K�@���@� �@���@�K�@�33@���@�~�@�$�@���@��@��;@�S�@�;d@�@�@���@�$�@��h@�A�@��@���@�|�@�ȴ@�p�@�9@�@�@�9X@�F@�|�@��@��@�|�@�\)@�@���@�h@���@�I�@�(�@畁@��@�-@��@��@䛦@�1@�P@�l�@��@���@�\@�v�@�V@�h@���@�l�@�$�@�`B@�V@ܼj@�z�@�I�@�(�@��;@ۅ@�ȴ@���@�V@�z�@�l�@֧�@�$�@��@�@Ցh@ԓu@��@�$�@ѡ�@�Ĝ@�(�@��m@�t�@�@���@Χ�@�5?@�@�?}@���@̬@̛�@̋D@��
@��@���@�n�@�@��#@ɑh@�/@ȼj@�j@�b@�\)@��y@�v�@�$�@�$�@�-@��@�O�@î@�
=@\@��@���@�hs@��`@�bN@�I�@��@��
@�K�@�@�ȴ@�n�@�@���@��^@�/@�V@�z�@�t�@��H@�n�@��h@�/@���@�j@���@�;d@�ȴ@���@�V@�$�@��T@���@�hs@�/@�V@��/@�bN@���@���@��+@��@���@�7L@���@��u@�I�@���@��@�"�@��H@��+@�ff@�=q@��h@�7L@�7L@��@���@� �@���@�|�@�K�@�"�@�"�@��y@��R@���@�^5@�J@���@�G�@��9@�(�@�ƨ@���@��@�
=@��@���@���@�n�@�-@��@�O�@��@���@�j@�9X@��F@�33@���@�{@���@��@�?}@�V@��@�z�@�Q�@� �@���@�33@�
=@��@���@��!@���@��+@�5?@���@�?}@�&�@���@��/@�z�@�A�@�1@��P@�K�@�
=@���@�ff@�E�@�-@�J@��T@���@���@�p�@�/@���@��D@�Z@�(�@��m@�S�@���@�{@�x�@�?}@�r�@���@��@�\)@��H@�^5@��@���@��7@�7L@��@��@�/@�&�@�%@���@���@��@�A�@��w@�l�@�C�@��@��!@�{@���@�O�@��@�%@��`@��j@��u@�r�@�9X@�1@��P@�K�@��y@�E�@��@��@��-@�O�@�/@��@���@�A�@�(�@�b@��w@�S�@��@�ȴ@��R@��+@�5?@��T@���@��@�G�@���@��@�r�@�bN@�I�@� �@�1@��F@�S�@���@��R@��!@���@�v�@�V@�-@�@���@�hs@��@��`@���@��j@���@��@�I�@��@�1@��@~�y@~$�@}�@|Z@|(�@{��@{��@{o@y��@yX@xr�@w�@w�w@w�@w\)@vV@v{@u�h@u`B@u�@tZ@sƨ@st�@s33@r�!@r�\@r~�@rn�@r=q@q�^@q7L@q%@pr�@p1'@o�w@o+@n��@nv�@m��@mp�@mp�@m`B@mO�@m/@l��@l�/@l�@lZ@k�m@k��@kS�@j�@j�!@j~�@i�@ihs@h�`@h�9@h�@hb@g��@gK�@g
=@f5?@e?}@d�/@d�D@dI�@c�
@b��@b^5@b=q@b-@b�@bJ@a��@a%@`��@`r�@` �@_�;@_�w@_l�@_
=@^�@^ȴ@]��@]�h@]�@]�@]?}@\�D@\1@[�m@[��@[S�@[33@Z�@Z-@ZJ@Yx�@X��@X�@W�@W�@Vȴ@Vff@V5?@U��@U?}@UV@TZ@S�m@Sƨ@Sƨ@S�@R��@R=q@Q�^@Q�7@Qx�@QX@Q�@P�@PA�@P �@O�@O�w@O;d@N�R@N�+@Nff@NV@N@M�-@M�h@M�@L�@L�/@L�j@L��@LI�@K�m@Ko@J~�@JJ@I�^@Ihs@H��@H �@Hb@G�;@G+@Fȴ@Fv�@FE�@E��@Ep�@E?}@D��@D�@D��@D��@Dz�@C�F@CS�@C33@B��@B��@B^5@A��@A�#@Ax�@@�`@@A�@?��@?|�@?l�@?\)@?K�@?�@>�@>v�@>$�@=�h@=?}@=�@<�@<�D@<1@;�F@;�@;t�@;dZ@;@:��@:n�@:=q@9��@9G�@8�u@81'@8b@7�@7�;@7�;@7��@7\)@6�R@6�R@6��@6��@6$�@5�T@5�-@5��@5/@4�/@49X@3�m@3dZ@2�@2~�@2M�@2=q@1��@1��@1X@0��@0�@0bN@/�@/\)@.��@.v�@-�T@-�-@-�h@-`B@-V@,�j@,�j@,�@,I�@,9X@,1@+�F@+S�@*�H@*�\@*~�@*-@*J@)�#@)��@)x�@)7L@)%@(�`@(�9@(��@(r�@(A�@'�@'�w@'��@'��@'�P@'K�@'
=@&ȴ@&��@&��@&v�@&E�@&$�@&{@%�-@%�h@%�@$��@$j@$1@#��@#��@#dZ@#33@#o@"�@"��@"n�@"=q@"-@"�@!��@!�^@!x�@!hs@!7L@!%@ �`@ Ĝ@ ��@ �u@ r�@ bN@ bN@ A�@ b@�@�w@��@��@l�@+@+@+@ȴ@$�@�@@p�@V@z�@�@�
@��@t�@C�@33@o@�!@=q@-@-@-@J@�#@�7@hs@7L@%@��@��@�@Q�@1'@b@��@l�@�y@��@��@��@V@5?@{@{@@`B@?}@/@��@�@��@�/@�@z�@z�@Z@�@��@��@t�@C�@33@"�@@�H@�H@��@M�@-@�@�#@��@�7@x�@X@X@7L@�`@Ĝ@��@�u@r�@1'@ �@ �@  @�w@|�@\)@K�@+@
=@��@ȴ@��@ff@E�@{@��@��@`B@/@�/@��@�D@z�@j@(�@��@�m@�F@dZ@@
�H@
�H@
�!@
~�@
~�@
n�@
^5@
J@	�#@	��@	hs@	G�@	G�@	%@Ĝ@��@bN@b@�;@�w@�@l�@K�@;d@;d@+@�@
=@��@ȴ@��A�  A�  A�  A�  A�  A���A���A���A���A���A���A���A�  A�  A�  A�  A���A���A���A���A���A���A�  A���A���A�  A�  A�  A�  A���A���A���A���A���A�  A�A�A�%A�A�A�A�A�A�A�A�A�A�A�%A�1A�%A�%A�A�  A�A�A�A�A�%A�%A�%A�A�A�A�  A�A�A�A�A�%A�1A�1A�%A�A�A�A�A�A�%A�1A�1A�
=A�1A�1A�
=A�1A�A�%A�%A�%A�%A�A�%A�%A�%A�1A�1A�1A�
=A�
=A�
=A�JA�JA�JA�JA�JA�JA�
=A�
=A�
=A�
=A�
=A�1A�
=A�
=A�1A�
=A�
=A�
=A�
=A�
=A�
=A�JA�JA�JA�JA�
=A�
=A�1A�JA�VA�VA�VA�JA�
=A�JA�JA�JA�JA�JA�VA�VA�bA�VA�JA�JA�JA�VA�VA�JA�JA�
=A�
=A�
=A�JA�VA�JA�bA�oA�bA�JA�
=A�
=A�
=A�JA�JA�bA�bA�bA�bA�bA�{A�{A�oA�bA�VA�VA�JA�VA�{A�oA��A��A��A� �A��A��A��A��A��A�7LA�;dA�?}A�C�A�E�A�G�A�K�A�S�A�S�A�VA�XA�XA�ZA�ZA�ZA�`BA�bNA�bNA�ffA�hsA�v�A�|�AҁA҇+AҋDAҏ\Aҕ�Aҗ�Aқ�Aҝ�Aҝ�Aҧ�Aң�Aҩ�AҰ!AҰ!AҰ!AҲ-AҴ9AҴ9AҶFAҸRAҸRAҸRAҶFAҴ9AҴ9AҶFAҸRAҺ^AҸRAҶFAҶFAҴ9AҴ9AҶFAҸRAҺ^AҸRAҶFAҴ9AҴ9AҴ9AҴ9AҶFAҶFAҸRAҸRAҸRAҸRAҸRAҸRAҶFAҴ9AҴ9AҴ9AҴ9AҶFAҸRAҸRAҸRAҸRAҶFAҴ9AҴ9AҶFAҸRAҺ^AҸRAҶFAҴ9AҴ9AҸRAҸRAҺ^AҺ^AҺ^AҸRAҶFAҴ9AҴ9AҶFAҸRAҸRAҸRAҶFAҴ9AҲ-AҰ!AҰ!AҲ-AҲ-Aҩ�Aҥ�Aҡ�Aҟ�Aҟ�Aҡ�Aҝ�Aҗ�A�z�A�/A�33A�7LA�K�A�&�A�+A�&�A��A�oAѕ�A�-A�&�A�$�A��A�JA�A��`A��AЃA���A�z�A�C�A�A΃A�oA�bNA��/A���A�|�AɋDA��A���AȺ^Aț�AȃA�^5A�O�A� �A��HAǣ�AǃA�ffA�\)A�S�A�E�A�;dA��A��Aƺ^A�dZA�E�A�+A��AžwAť�A�dZA�A�A�VA�ƨAć+A�5?A��Aã�AÉ7A�l�A�JA©�A�^5A��A�bA��;A�n�A�E�A�+A� �A��
A�E�A�ƨA�O�A�+A��A���A��
A��FA��hA�r�A�VA�$�A��A�Q�A�VA���A���A���A�v�A�VA��A��A��RA�z�A�5?A��
A���A��A�n�A�I�A�;dA�9XA�33A�(�A�$�A�$�A���A��TA��jA��uA�^5A�-A��A���A��hA�E�A�;dA��A��yA��HA��;A��A���A���A�jA��jA�"�A�ȴA�x�A�K�A�1A���A��A��7A�jA�VA�C�A�&�A�VA��HA���A��RA���A�t�A�ZA�;dA�33A�oA��A��A��A���A�|�A��
A��+A�A�A�;dA� �A�  A��^A��A�v�A�A��A��A��mA��yA��mA��HA��/A���A��jA��-A��!A��!A���A��uA�z�A�G�A��A���A��jA�ffA��A���A��wA���A�^5A���A��uA�dZA�XA�Q�A�O�A�?}A�&�A�%A��A���A��RA���A�l�A�A�ĜA�~�A��A���A�bNA�G�A�7LA�-A��A�VA�1A���A��#A���A���A���A���A���A��/A��`A���A��-A��A���A���A�ffA�A�-A���A�p�A�%A�A��#A��A��A���A��TA���A�\)A�
=A��/A��wA���A��A�VA�1'A��^A��A�oA��HA��!A�G�A���A�hsA�;dA�A��^A�^5A��TA��9A�\)A��A��TA��RA��uA�r�A�G�A�;dA�5?A��A�{A��A��A��`A��#A���A���A���A��-A��RA��!A��9A���A���A���A�`BA��A�x�A�-A�K�A���A�x�A�O�A�/A��A�A���A���A��jA���A��7A�l�A�l�A�l�A�^5A�A�A�7LA�/A�1'A�/A�bA��A��A��mA��#A���A���A���A�ȴA�ƨA��jA��-A��!A��!A��A���A���A��hA��PA��PA��+A��DA�~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�BB
�wB
�wB
�BB
�BB
�B
�BB
�BB
�BB
�BB
�BB
�B
�BB
�BB
��B
��B
��B
��B
��B
�qB
��B
��B
��B
��B
��B
�BB
�B
��B
�wB
�BB
�BB
��B
��B
��B
��B
��B
�dB
�B
��BMB!�B#�B$@B$tB$@B$@B#�B#nB$B(�BB�Bc�B��B�)Ba�BbB]/B`vBe�BlWB�MB�qB�B�XB��B��B��B��B��B��B��B��B��B�7B��B}�BT�BFtB<jB3�B0�B4B'�B�BoB"B��B�B��B�B�[B��Bt�BK�BA�B#�B�B
��B
�]B
�B
ÖB
�uB
�rB
��B
~�B
x8B
S�B
?HB
,qB
(B	�`B	�B	רB	�$B	��B	�B	s�B	qB	m�B	gB	WsB	:�B	:�B	6B	0!B	(�B		B	eB	�B	�B�VB�|B��B�B�WB��BרB�QBچB�B��B�]B��B��BԕB�jB��B�?B�gB��B�?B�3B�aB��B� B�^BҽB��B�pB�B��B��B��B�yB�QB��B��B�B��B�]B	oB	(B	kB	�B	=B	CB	!�B	/B	B�B	FtB	f�B	l"B	k�B	k�B	m�B	m]B	iDB	iDB	s�B	tB	s�B	qB	qvB	v�B	w�B	xB	yrB	u%B	v`B	x�B	y�B	|B	.B	�DB	��B	�;B	��B	��B	��B	��B	��B	��B	�MB	��B	��B	�fB	��B	��B	�"B	�hB	�:B	��B	��B	�(B	�~B	��B	��B	��B	�eB	�B	�qB	��B	��B	��B	�!B	��B	�IB	�B	��B	��B	��B	��B	�RB	�B	��B	��B	�0B	��B	��B	�wB	��B	�UB	��B	�-B	��B	B	�'B	��B	B	ÖB	��B	��B	˒B	�^B	��B	��B	˒B	˒B	˒B	˒B	�B	�BB	�vB	�}B	�&B	��B	�,B	��B	�[B	ҽB	�aB	�&B	ҽB	�aB	�aB	��B	�2B	�mB	�B	�9B	�
B	�EB	��B	��B	�#B	��B	�#B	چB	��B	��B	�#B	��B	��B	��B	��B	��B	��B	��B	��B	�)B	�WB	�WB	��B	�)B	�/B	�B	��B	�B	��B	�B	ޞB	��B	�B	�|B	�B	�B	��B	�fB	�B	�>B	�B	�KB	�DB	�B	�B	�WB	��B	�B	��B	�)B	�/B	��B	��B	� B	��B	�B	�GB	�B	�B	�B	�B	�TB	�B	��B	�TB	�B	�B	��B	�8B	�lB	�>B	��B	�xB	�B	��B	�JB	�B	�B	��B	��B	��B	�.B	�cB	��B
oB
 4B
  B
B
uB
uB
�B
�B
B
�B
�B
B
�B
GB
GB
uB
uB
�B
�B
B
uB
{B
SB
�B
�B
�B
SB
�B
�B
�B
%B
YB
YB
%B
�B
�B
�B
�B
�B
�B
�B
�B
fB
	�B
	7B

	B

�B

�B
xB
xB
B
�B
B
B
PB
�B
�B
�B
�B
�B
�B
�B
�B
�B
bB
�B
bB
�B
hB
hB
�B
�B
:B
�B
:B
�B
oB
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
kB
�B
7B
�B
CB
�B
�B
~B
!B
�B
�B
 'B
!bB
!-B
!bB
"hB
"hB
#B
$tB
%B
$�B
%�B
&�B
'�B
(�B
)*B
(XB
(�B
)_B
)�B
(�B
)�B
*0B
)�B
+B
+6B
+kB
-wB
,qB
-�B
-wB
.B
.IB
.�B
/�B
0UB
/�B
0!B
0�B
1�B
1�B
2�B
2�B
2�B
3�B
3�B
33B
4B
4nB
4�B
5B
5?B
5tB
5tB
5?B
5?B
5�B
6�B
6�B
6�B
7B
7LB
7LB
6�B
7�B
8�B
8B
8RB
8�B
9$B
9XB
9�B
9�B
9�B
:�B
:�B
:�B
:^B
:^B
:^B
:*B
<6B
<6B
<jB
<6B
<6B
=<B
<�B
>BB
=�B
>BB
>BB
>�B
?HB
>�B
>�B
>�B
?HB
?HB
?}B
?�B
@OB
@�B
@B
?�B
?}B
?�B
@�B
@�B
@OB
AUB
A�B
A�B
A�B
A�B
AUB
B'B
A�B
B'B
B�B
B�B
B�B
B�B
B�B
B�B
B�B
C-B
CaB
C�B
DgB
D3B
D3B
E9B
EmB
FB
EB
E9B
F?B
F�B
GB
FtB
H�B
H�B
G�B
HB
HB
H�B
JXB
JXB
JXB
I�B
I�B
I�B
K)B
K�B
K�B
K^B
K^B
K)B
K^B
LdB
L�B
L�B
L�B
NB
M�B
M�B
MjB
M�B
NB
NB
M�B
N<B
N�B
N<B
N�B
O�B
O�B
PHB
PHB
PB
QNB
RTB
Q�B
Q�B
Q�B
R�B
S[B
S&B
T�B
T,B
S�B
S�B
T�B
VB
T�B
V�B
U�B
V9B
U�B
V�B
W�B
W�B
W?B
W�B
W�B
XB
XEB
XyB
X�B
XB
X�B
YKB
XyB
YB
Y�B
YB
YB
Y�B
YB
Y�B
[WB
Z�B
[�B
[�B
[WB
\�B
]/B
\]B
\�B
]dB
]�B
]�B
]�B
^jB
^B
^�B
^�B
^B
^�B
^5B
^�B
_�B
_;B
_pB
_�B
`B
_�B
`B
`B
`BB
a|B
a�B
a�B
aB
a�B
a|B
aB
a�B
a�B
a�B
bNB
c B
cTB
b�B
c�B
d&B
d&B
e,B
d�B
d�B
d�B
e`B
d�B
e,B
e�B
f2B
e�B
f�B
gB
ffB
ffB
gB
f�B
f�B
h
B
h
B
gmB
h
B
gmB
h�B
h>B
iB
hsB
iDB
iB
i�B
jB
jB
j�B
j�B
kB
kB
kQB
kB
kQB
l"B
l"B
k�B
l�B
l�B
l�B
m�B
ncB
ncB
n�B
o B
o5B
o�B
o5B
oiB
o�B
o�B
pB
p;B
poB
qvB
qvB
p�B
q�B
q�B
qvB
qAB
rGB
rB
r|B
r�B
sB
r�B
sB
r�B
sMB
s�B
sMB
s�B
s�B
tB
s�B
s�B
tB
tB
tB
tTB
tTB
tTB
t�B
t�B
uZB
u%B
u�B
v+B
u�B
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
xB
x8B
xlB
xlB
xlB
x�B
y	B
x�B
y	B
x�B
y>B
yrB
y	B
y>B
y�B
yrB
y�B
zDB
zB
y�B
zxB
y�B
y�B
zxB
{JB
{JB
{B
{B
|B
|�B
}"B
}VB
}�B
}VB
}�B
}VB
}�B
~(B
~�B
~�B
~�B
~�B
~�B
~�B
.B
.B
~�B
.B
cB
�B
�B
� B
� B
� B
�iB
��B
�oB
�;B
�;B
�;B
�oB
�oB
��B
�oB
�B
��B
��B
��B
�B
�GB
�B
�GB
�{B
��B
��B
��B
�B
�B
��B
��B
��B
��B
�B
�SB
�SB
�SB
��B
��B
�%B
�%B
��B
��B
��B
��B
��B
��B
��B
��B
�+B
�_B
��B
��B
��B
��B
��B
�1B
��B
��B
�B
�7B
�lB
�lB
�7B
�lB
��B
��B
��B
�	B
�=B
�=B
��B
��B
�B
�B
�DB
�xB
�xB
��B
��B
��B
�B
�~B
�~B
�~B
�B
�B
�PB
�PB
�PB
��B
��B
�"B
�"B
��B
��B
�VB
��B
��B
��B
��B
�\B
��B
��B
��B
�bB
��B
��B
��B
�bB
��B
��B
��B
��B
��B
��B
��B
�BB
��B
�B
�B
�}B
��B
��B
�B
�B
�B
�<B
�<B
�qB
��B
�B
��B
�B
��B
�wB
��B
��B
�BB
��B
�B
�<B
�qB
��B
�BB
��B
�B
��B
�HB
�wB
�B
�B
�qB
�<B
��B
�BB
�wB
�B
�HB
�B
��B
�wB
��B
�<B
�B
�B
��B
�wB
�B
�B
��B
��B
�BB
�<B
��B
�<B
�BB
�B
�BB
�wB
�BB
�wB
��B
��B
��B
��B
��B
��B
�wB
��B
��B
��B
��B
�BB
�<B
�B
��B
��B
�B
�<B
�<B
��B
��B
��B
��B
��B
��B
��B
�B
��B
�BB
�B
��B
��B
�qB
�<B
��B
�B
�B
��B
��B
�<B
�B
�wB
�wB
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
�BB
�qB
��B
�<B
�<B
�B
�B
��B
��B
��B
�wB
��B
�BB
�}B
��B
�B
�HB
�}B
�B
�B
��B
�BB
��B
�B
��B
�qB
�<B
�<B
�<B
��B
�wB
�}B
��B
�HB
�BB
�}B
�B
�BB
��B
�BB
�qB
��B
��B
�B
�B
�B
�B
��B
�BB
��B
�B
��B
�HB
�HB
�HB
�HB
��B
�}B
�HB
ŢB
��B
ʌB
ƨB
�gB
ƨB
��B
ƨB
�mB
ĜB
�B
��B
��B
��B
�#B
ںB
ߤB
ںB
��B
��B
�B
�B
�B
�B
�B
�B
�oB
��B
�B
�B
�GB
�TB
��B
�cB �B%BB�B�B�B�B�BMBqB�B 'B �B!�B$�B$@B$@B#:B"�B"�B#:B$B$�B$�B$@B#nB"�B#�B$�B$�B%B%B$�B$B#nB$B$tB%B%FB%FB$�B$�B$tB#�B#nB#:B#�B#�B#�B$�B$�B%FB%B$�B$�B$@B#nB#nB#�B$B%FB$�B$@B#�B"�B#B$@B$�B$tB#�B#:B"�B"hB"�B#B$B$tB$@B#nB#B#:B#:B#�B$tB%zB%�B&B$�B$�B&�B(�B*0B+kB+�B*eB+kB-�B9$BIRBK�BG�BA�BM�BI�BIBK�BM�BsMBw2BuZBt�Bt�BsBs�BwfBu�B�B��B��B�B�B�<BĜB�
B�B�B;dBZ�BgmB[�Bg8Bc�Bc�Bb�B`�Bd&Bf�Be`Bd�B]dBZ�BZ�B\�B\�B_�B[�BZQBb�B\�B^jBh
B^�B`vBhsB`vBc�B`BBd�BqBq�Bo�Bh
BhsBv�BdZBgBu�B�B�eB�_B�YB��B�+B��B��B��B��B��B�YB��B�B��B�4B�bB�!B�nB�B�B��B��B�aB��B�B�IB��B��B��B�_B��B��B�6B��B��B�=B��B��B��B�RB��B�tB��B�B�=B��B�'B��B��B�B�FB�'B�XB�UB�B�LB�B��B�@B�$B�B��B�FB��B�kB�SB��B��B��B��B��B��B��B�=B��B��B��B��B�AB�B�B{�Bx8BzxBx8B�MB��B`vBOBB`�BNB@�B:�B8�B>BAUBX�Bv`B=<B5?B3�B4�B33B2�B4nB33B3�B33B2�B0�B/�B49B.�B/�B6B4�B3hB-CB@�B&�B+6B'RB!�B!bB&B�B�B4B\B(B4B�B�BFB�BbB(B4BB_B�B�B��B��B�B�;B�/B�B�B��B��B�mB�8B�ZB�B�NB�B�8B��B�dBĜB��B�BB�jB��B�8B�$B��B�B�B�B��BpoBv�B��Bx�B�BUgBZ�BMjBJXBK�BC-BF�B>�BM�BTaB8�B'�B,qB6�B(XB�BSB�BB�B�BAB�B�B�B
�>B
��B
��B
��B
�5B
�B
�B
��B
��B
��B
�sB
��B
�ZB
��B
�B
��B
�B
��B
�5B
��B
��B
֡B
�B
�B
�BAB
�B
�=B
��B
�'B
��B
��B
��B
�4B
�(B
��B
��B
��B
��B
�B
��B
� B
��B
��B
�fB
�B
��B
��B
�lB
�1B
��B
��B
�+B
�B
�B
�B
��B
��B
�B
�B
�uB
�{B
�B
��B
��B
~(B
�B
�;B
~�B
.G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                      B
�B
�NB
��B
��B
�NB
�NB
�B
�NB
�NB
�NB
�NB
�NB
�B
�NB
�NB
��B
��B
��B
��B
��B
�}B
��B
��B
��B
��B
��B
�NB
�B
��B
��B
�NB
�NB
��B
��B
��B
��B
��B
�pB
�B
��BYBB�BLB�BLBLB�BzBB �B;B[�B��B�5BY�BZ%BU;BX�B]�BdcB|YB�}B�!B�dB��B��B��B��B��B��B��B��B��B�CBz�Bv BL�B>�B4vB+�B(�B,B�B�B
{B.B��B�B�
B�)B�gB��Bl�BC�B9�B�B�B
��B
�iB
�%B
��B
��B
�~B
|�B
wB
pDB
K�B
7TB
$}B
4B	�lB	�B	ϴB	�0B	��B	yB	k�B	iB	e�B	_B	OB	2�B	2�B	.B	(-B	 �B	B	qB	�B	�B�bB�B��B�B�cB��BϴB�]BҒB�)B��B�iB�B��B̡B�vB��B�KB�sB��B�KB�?B�mB�B�,B�jB��B��B�|B�%B��B�B�B�B�]B��B�B�B��B�iB�{B	4B	wB	�B	IB	OB	�B	''B	:�B	>�B	^�B	d.B	c�B	c�B	e�B	eiB	aPB	aPB	k�B	l+B	k�B	iB	i�B	n�B	o�B	pB	q~B	m1B	nlB	p�B	q�B	t(B	w:B	�PB	z�B	yGB	x�B	z�B	|�B	}�B	}�B	|�B	|YB	|�B	}�B	�rB	��B	��B	�.B	�tB	�FB	��B	��B	�4B	��B	��B	��B	��B	�qB	�B	�}B	��B	��B	��B	�-B	��B	�UB	�B	��B	��B	��B	��B	�^B	�)B	��B	��B	�<B	��B	��B	��B	��B	�aB	��B	�9B	��B	��B	�3B	��B	��B	��B	��B	��B	ÞB	�jB	��B	��B	ÞB	ÞB	ÞB	ÞB	�B	�NB	ǂB	ȉB	�2B	��B	�8B	��B	�gB	��B	�mB	�2B	��B	�mB	�mB	�
B	�>B	�yB	�B	�EB	�B	�QB	��B	��B	�/B	��B	�/B	ҒB	�B	��B	�/B	��B	��B	��B	��B	��B	��B	��B	��B	�5B	�cB	�cB	�B	�5B	�;B	�B	��B	ضB	��B	�B	֪B	��B	�B	وB	ڎB	��B	�B	�rB	�B	�JB	�B	�WB	�PB	�"B	��B	�cB	��B	�B	� B	�5B	�;B	��B	��B	�B	��B	�B	�SB	�B	�%B	�%B	�B	�`B	��B	��B	�`B	�+B	�B	��B	�DB	�xB	�JB	��B	�B	�"B	�B	�VB	�B	�B	��B	��B	��B	�:B	�oB	��B	�{B	�@B	�B	�B	��B	��B	��B	��B	�B	��B	��B	�B	��B	�SB	�SB	��B	��B	��B	��B	�B	��B	��B	�_B	��B	��B	��B	�_B	��B	��B	��B	�1B	�eB	�eB	�1B	��B	��B	��B	�B	��B
 �B
 	B
 �B
 rB
�B
CB
B
�B
�B
�B
�B
!B
�B
'B
'B
\B
�B
�B
�B
�B
�B
�B
�B
�B
�B
nB
B
nB
�B
	tB
	tB
	�B
	�B

FB

�B

FB

�B

{B
�B
�B
�B
B
B
�B
�B
�B
�B
�B
�B
wB
�B
CB
�B
OB
�B
�B
�B
-B
�B
�B
3B
nB
9B
nB
tB
tB
B
�B
B
�B
�B
�B
�B
 �B
!6B
 dB
 �B
!kB
!�B
!B
!�B
"<B
!�B
#B
#BB
#wB
%�B
$}B
%�B
%�B
& B
&UB
&�B
'�B
(aB
'�B
(-B
(�B
)�B
*B
*�B
*�B
*�B
+�B
+�B
+?B
,B
,zB
,�B
-B
-KB
-�B
-�B
-KB
-KB
-�B
.�B
.�B
.�B
/#B
/XB
/XB
.�B
/�B
0�B
0)B
0^B
0�B
10B
1dB
1�B
2B
2B
3B
2�B
2�B
2jB
2jB
2jB
26B
4BB
4BB
4vB
4BB
4BB
5HB
4�B
6NB
5�B
6NB
6NB
6�B
7TB
6�B
6�B
6�B
7TB
7TB
7�B
7�B
8[B
8�B
8&B
7�B
7�B
7�B
8�B
8�B
8[B
9aB
9�B
9�B
9�B
9�B
9aB
:3B
9�B
:3B
:�B
:�B
;B
;B
;B
;B
;B
;9B
;mB
;�B
<sB
<?B
<?B
=EB
=yB
>B
=B
=EB
>KB
>�B
?B
>�B
@�B
@�B
?�B
@#B
@#B
@�B
BdB
BdB
BdB
A�B
A�B
A�B
C5B
C�B
C�B
CjB
CjB
C5B
CjB
DpB
D�B
D�B
D�B
FB
E�B
E�B
EvB
E�B
FB
FB
E�B
FHB
F�B
FHB
F�B
G�B
G�B
HTB
HTB
H B
IZB
J`B
I�B
I�B
I�B
J�B
KgB
K2B
L�B
L8B
K�B
K�B
L�B
NB
M
B
N�B
M�B
NEB
M�B
N�B
O�B
O�B
OKB
O�B
O�B
PB
PQB
P�B
P�B
PB
P�B
QWB
P�B
Q#B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
ScB
R�B
S�B
S�B
ScB
T�B
U;B
TiB
UB
UpB
U�B
U�B
U�B
VvB
VB
V�B
V�B
VB
V�B
VAB
V�B
W�B
WGB
W|B
W�B
XB
W�B
XB
XB
XNB
Y�B
Y�B
Y�B
YB
Y�B
Y�B
YB
Y�B
Y�B
Y�B
ZZB
[,B
[`B
Z�B
[�B
\2B
\2B
]8B
\�B
\�B
\�B
]lB
]B
]8B
]�B
^>B
]�B
^�B
_B
^rB
^rB
_B
^�B
^�B
`B
`B
_yB
`B
_yB
`�B
`JB
aB
`B
aPB
aB
a�B
b"B
b�B
b�B
b�B
c(B
c(B
c]B
c(B
c]B
d.B
d.B
c�B
d�B
d�B
e B
e�B
foB
foB
f�B
gB
gAB
g�B
gAB
guB
g�B
g�B
hB
hGB
h{B
i�B
i�B
h�B
i�B
i�B
i�B
iMB
jSB
jB
j�B
j�B
k%B
j�B
k%B
j�B
kYB
k�B
kYB
k�B
k�B
l+B
k�B
k�B
l+B
l+B
l+B
l`B
l`B
l`B
l�B
l�B
mfB
m1B
nB
n7B
m�B
n�B
n�B
n�B
n�B
n�B
o>B
orB
orB
o�B
o�B
pB
pDB
pxB
pxB
pxB
p�B
qB
p�B
qB
p�B
qJB
q~B
qB
qJB
q�B
q~B
q�B
rPB
rB
q�B
r�B
q�B
q�B
r�B
sVB
sVB
s"B
s�B
t(B
t�B
u.B
ubB
u�B
ubB
u�B
ubB
u�B
v4B
v�B
v�B
v�B
v�B
v�B
wB
w:B
w:B
wB
w:B
woB
w�B
w�B
xB
xB
xB
xuB
x�B
y{B
yGB
yGB
yGB
y{B
y{B
y�B
y{B
zB
z�B
z�B
z�B
{B
{SB
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
}+B
}_B
}_B
}_B
}�B
}�B
~1B
~1B
~�B
~�B
~�B
~�B
B
~�B
B
�B
7B
kB
�B
�B
�	B
�	B
�	B
�=B
��B
��B
�B
�CB
�xB
�xB
�CB
�xB
��B
��B
��B
�B
�IB
�IB
��B
��B
�B
�B
�PB
��B
��B
��B
��B
��B
�!B
��B
��B
��B
�'B
�'B
�\B
�\B
�\B
��B
��B
�.B
�.B
��B
��B
�bB
��B
��B
��B
��B
�hB
��B
��B
��B
�nB
�B
��B
�B
�nB
��B
��B
��B
��B
��B
��B
��B
�NB
��B
�B
� B
��B
��B
��B
� B
�B
�B
�HB
�HB
�}B
��B
�B
��B
� B
��B
��B
��B
��B
�NB
��B
�B
�HB
�}B
��B
�NB
��B
� B
��B
�TB
��B
�B
�B
�}B
�HB
��B
�NB
��B
� B
�TB
� B
��B
��B
��B
�HB
�B
�B
��B
��B
�B
�B
��B
��B
�NB
�HB
��B
�HB
�NB
�B
�NB
��B
�NB
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
�NB
�HB
�B
��B
��B
�B
�HB
�HB
��B
��B
��B
��B
��B
��B
��B
�B
��B
�NB
�B
��B
��B
�}B
�HB
��B
�B
�B
��B
��B
�HB
�B
��B
��B
��B
��B
��B
��B
��B
� B
� B
��B
��B
��B
�NB
�}B
��B
�HB
�HB
�B
�B
��B
��B
��B
��B
��B
�NB
��B
��B
� B
�TB
��B
� B
� B
��B
�NB
��B
�B
��B
�}B
�HB
�HB
�HB
��B
��B
��B
��B
�TB
�NB
��B
�&B
�NB
��B
�NB
�}B
��B
��B
� B
� B
�B
�B
��B
�NB
��B
�B
��B
�TB
�TB
�TB
�TB
��B
��B
�TB
��B
��B
B
��B
�sB
��B
��B
��B
�yB
��B
�B
��B
��B
�
B
�/B
��B
װB
��B
��B
��B
ٽB
۔B
ܛB
ݡB
߭B
�B
�{B
� B
�B
�B
�SB
�`B
��B
�oB
��B
�1BB�B�B
�B�B�BYB}B�B3BB�B�BLBLBFB�B�BFBB�B�BLBzB�B�B�B�BBB�BBzBB�BBRBRB�B�B�B�BzBFB�B�B�B�B�BRBB�B�BLBzBzB�BBRB�BLB�B�BBLB�B�B�BFB�BtB�BBB�BLBzBBFBFB�B�B�B�B$B�B�B�B �B"<B#wB#�B"qB#wB%�B10BA^BC�B?�B9�BE�BA�BA)BC�BE�BkYBo>BmfBl�Bl�Bk%Bk�BorBm�B�'B��B��B�$B�B�HB��B�B��B�B3pBR�B_yBTB_DB[�B[�BZ�BX�B\2B^�B]lB\�BUpBR�BR�BT�BT�BW�BS�BR]BZ�BUBVvB`BV�BX�B`BX�B[�BXNB\�BiBi�Bg�B`B`Bo	B\fB_Bm�B{B�qBkB~eB}�B7B��B��B��B��B��B�eB��B�!B��B�@B�nB�-B�zB�B�B��B��B�mB��B�'B�UB��B��B��B�kB��B��B�BB��B��B�IB��B��B��B�^B��B��B��B�*B�IB��B�3B��B��B�B�RB�3B�dB�aB�*B�XB�B��B�LB�0B�$B��B�RB��B�wB�_B�B��B��B�B��B��B�B�IBB�Bz�B�BzMByByBs�BpDBr�BpDB|YB��BX�BGNBX�BFB8�B3B0�B6B9aBP�BnlB5HB-KB+�B,�B+?B*�B,zB+?B+�B+?B*�B(�B'�B,EB&�B'�B.B,�B+tB%OB8�B�B#BB^B�BnB$B�B�B	@BhB4B	@B�B	�BRB	�BnB4B	@BB�kB��B��B��B�B�B�GB�;B�B�B��B��B�yB�DB�fB��B�ZB�B�DB��B�pB��B��B�NB�vB��B�DB�0B��B�B�*B�B}�Bh{Bo	B}�Bp�Bw�BMsBR�BEvBBdBC�B;9B>�B6�BE�BLmB0�B�B$}B.�B dB�B_B�BB�B�B
�MB�B
��B
��B
�JB
��B
�B
�B
�AB
�B
�B
��B
��B
�
B
�B
��B
�fB
�B
۔B
��B
�B
��B
�AB
��B
��B
έB
�#B
�B
߭B
�MB
�B
�IB
��B
�3B
��B
��B
��B
�@B
�4B
��B
��B
��B
��B
�B
��B
�B
��B
��B
�rB
�!B
��B
��B
�xB
�=B
�B
��B
7B
{B
�B
}+B
|�B
�B
}+B
|%B
z�B
{�B
|%B
|�B
y�B
v4B
w�B
yGB
wB
w:G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111144111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                      G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL            none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                      none                                                                                                                                                                                                                                                            none                                                                                                                                                                                                                                                            PSAL_ADJUSTED = salinity + salinity_offset                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    salinity_offset = -0.0077649                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        PSAL ADJUST [dd mm yyyy N S_off stddev] 23 11 2022 179 -0.0077649 0.0005 where N is the number of the delayed-mode profile used to estimate S_off stddev                                                                                                                                    20230426223220                            20230426223220AO  AO  ARCAARCAADJSADJS                                                                                                                                        2023042622322020230426223220  IP  IP                                G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622322020230426223220QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�1F83E           783E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2023042622322020230426223220QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               