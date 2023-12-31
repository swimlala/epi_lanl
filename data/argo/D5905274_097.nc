CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-09-01T14:59:35Z creation; 2023-04-26T19:24:30Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.5   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue        G�O�     �  dt   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �h   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �\   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �@   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � A4   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � h�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �(   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   ��   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   ��   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   ��   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   ��   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � ��   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   �|   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20200901145935  20230426192430  5905274 5905274 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               a   aAA  AOAO7315_008643_097                 7315_008643_097                 2C  2C  DD  SOLO_II                         SOLO_II                         8643                            8643                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @�4�(w�N@�4�(w�N11  @�4�Z�c @�4�Z�c @1�!�Kr�@1�!�Kr��d�]9����d�]9���11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  ?��H@@  @�  @�  @�  @�G�A ��A  A   A+�A@  A^�RA~{A��A�Q�A�  A��AϮA߮A�A��B  B�B�B�
B(  B0(�B8(�B@(�BH(�BP(�BW�B`  Bh(�Bp(�Bw�
B�B��B�{B��B��B�  B��
B��B�{B�  B��
B�  B�{B��B��
B�  B��B�  B�  B�  B�(�B�{B�{B�{B��B�  B�=qB�=qB�  B�  B�  B�{C 
=C  C  C  C  C	��C  C
=C  C��C  C  C
=C{C{C  C��C"  C$  C%�C(  C*{C,  C-��C/��C2  C4
=C6  C7��C:  C;��C=�C?��CB  CD  CE��CH
=CJ
=CL  CM�CO�HCQ�CT  CV  CX  CY��C[��C]�C_�Cb  Cd  Ce��Ch  Ci�Cl  Cn
=Cp  Cq��Ct
=Cv  Cw��Cz  C|  C~
=C�C���C���C���C�C�C���C�  C�  C�  C�  C�  C�C�C�C�C���C��C���C���C���C�  C�  C���C���C�C�
=C�
=C�  C�C�
=C�\C�
=C���C���C�  C�C�C�
=C�\C�C�C�C�C�
=C�
=C���C���C���C�
=C�\C�  C�  C���C���C���C���C�  C�  C���C�  C�C�C�C�  C�  C�  C���C���C�  C�
=C�C���C�C�C�  C�  C���C�C�  C�  C���C���C���C�  C�C�  C�  C�C�  C�  C�C���C���C�C�C�  C�C�C�  C�  C�C�C�C�  C�C�
=C�  C���C���C�C�C���C���C���C�  C�
=C�
=C�  C���C���C���C�C�  C�  C�  C�  C�D   D }qD ��D}qD�D� D  D��D  D}qD�qD}qD  D�D�D� D  D� D�qD	� D	�qD
z�D  D� D�D� D�qD}qD�qD� D  D}qD  D� D  D��D  D}qD�qD}qD  D��D�D}qD  D�D�D� D  D�DD��D�qD}qD�D�D  D}qD  D� D  D��D�qD� D �D ��D ��D!z�D"�D"�D#�D#� D$�D$�D%  D%� D&�D&�D'�D'��D(  D(� D)  D)� D*  D*� D+�D+��D,  D,� D-D-��D.D.�D/�D/�D/��D0xRD0��D1}qD1�qD2}qD2�qD3� D4�D4� D5  D5��D6�D6��D7�D7}qD7��D8z�D9  D9�D:�D:��D;  D;� D<D<��D=�D=�D>  D>��D?�D?�D@�D@}qDA  DA��DB  DB}qDC  DC��DD  DDz�DE  DE}qDE��DF� DF�qDG� DHDH��DI�DIz�DI��DJ� DK  DK� DLDL}qDM  DM� DM��DN� DO  DOz�DO�qDP}qDP�qDQ��DQ�qDR� DR�qDS}qDT  DT� DUDU�DV  DV��DW�DW� DW��DX}qDY  DY��DZ�DZ��DZ�qD[}qD[�qD\� D]  D]� D^  D^}qD_  D_� D`  D`}qDa  Da� Db  Db� Dc  Dc��Dd  Dd��De�De��Df�Df}qDg  Dg� Dg�qDh}qDi  Di��Dj�Dj� Dk  Dk� Dk�qDl}qDm  Dm� Dn  Dn}qDo  Do� Dp�Dp� Dq  Dq��Dr�Dr� Ds�Ds��Ds��Dt}qDu  Du� Dv�Dv}qDv�qDw}qDx  Dx� Dy  Dy� Dy�qDz}qD{  D{� D|  D|��D}�D}��D}�qD~z�D~�qD� D�HD�@ D�~�D���D�HD�B�D�� D���D�  D�>�D�� D��HD�  D�AHD��HD��HD�HD�@ D�� D�� D�HD�@ D�~�D�� D�  D�@ D�� D�� D�  D�>�D�� D�D�HD�>�D�}qD���D�HD�B�D��HD���D��qD�=qD�~�D��HD��D�B�D��HD�D��D�B�D��HD���D���D�>�D�}qD���D��D�AHD�}qD�� D�  D�=qD�� D��HD��D�>�D�~�D��HD�HD�AHD�~�D��HD��D�AHD�~�D��qD���D�B�D��HD��qD�  D�AHD�}qD���D���D�=qD��HD��HD�HD�B�D�� D�� D�  D�AHD��HD��HD�  D�>�D��HD�� D�  D�@ D�� D��qD���D�@ D�~�D���D���D�=qD�}qD�� D�  D�B�D�� D���D���D�>�D�~�D���D�  D�@ D�� D��HD�  D�@ D��HD�� D���D�=qD�~�D���D��qD�=qD�}qD��qD��qD�=qD�}qD��qD�HD�@ D�~�D���D��qD�>�D�� D���D�  D�@ D�� D��HD�  D�@ D��HD���D���D�@ D�~�D��HD��D�AHD�� D���D���D�@ D�~�D��qD���D�@ D�~�D���D���D�@ D�~�D���D�HD�B�D���D��HD�HD�B�D�� D��HD�HD�@ D�~�D�� D�HD�@ D��HD��HD�HD�@ D��HD��HD�  D�AHD��HD�D��D�AHD�� D�� D���D�@ D���D���D��qD�=qD�� D��HD�  D�=qD�� D�� D���D�>�D�� D��HD�HD�B�D��HD���D��qD�>�D�~�D���D�  D�@ D�~�D�� D�  D�=qD�~�D���D���D�@ D�� D��HD�  D�=qD�~�D�� D�  D�>�D�}qD�� D�HD�B�D�}qD���D�HD�AHD�� D���D�HD�@ D�~�D�� D�HD�AHD�� D���D�  D�>�D�~�D½qD��qD�@ D�~�D�� D�HD�B�DĀ D��HD�  D�<)D�~�D��HD���D�AHDƀ D��HD�  D�AHD�~�D�� D�HD�@ DȁHD�� D��qD�>�D�~�Dɾ�D�  D�>�D�~�D��HD�  D�>�D�}qD˾�D�  D�@ D�~�D̾�D���D�AHD͂�D�D�  D�@ D�~�Dξ�D���D�AHDρHD�D�HD�AHDЂ�D�� D��qD�=qD�}qDѽqD��qD�=qD�~�DҽqD��qD�>�D�~�DӼ)D��)D�@ DԀ DԽqD���D�AHDՁHD���D��D�AHD�~�DֽqD��qD�=qD�~�D�� D�  D�AHD؂�D���D��D�>�D�~�DٽqD���D�AHDځHD�� D��D�C�Dۀ D۾�D�  D�@ D�}qDܽqD�  D�>�D�}qDݽqD�HD�AHDށHD�D�  D�=qD�}qD��HD�  D�>�D���D�� D��)D�@ DႏDᾸD��)D�@ D₏D��HD�HD�AHD�~�D�qD�  D�AHD�HD�qD�HD�B�D�~�D�qD�  D�AHD�HD�D���D�@ D炏D�qD�  D�C�D�}qD��HD�HD�=qD�}qD��HD���D�@ D��D꾸D�  D�AHD�HD�� D�HD�>�D�~�D쾸D���D�>�D킏D��HD�  D�AHD�~�DD��)D�@ D�}qD��HD�  D�AHD�~�D�� D�  D�>�D� D�D�  D�>�D� D�� D�HD�>�D�~�D�D���D�>�D�HD��HD�HD�>�D�� D��HD�  D�@ D��HD�� D�  D�@ D�� D�� D��qD�>�D�� D���D��qD�>�D�� D�D�  D�@ D��HD��\>��
>�Q�?.{?�=q?��
?���@   @z�@+�@@  @^�R@s33@��@�\)@��H@��@���@�@�G�@�=q@�
=@��
@���@�
=A   A�A
=qA  Az�A=qA�RA"�\A(��A.{A3�
A8Q�A>{AC�
AH��AN�RATz�AZ=qA_\)Ae�Aj=qAp  Au�Az�HA�Q�A�33A�A���A��\A��A�  A��HA�p�A���A��A�ffA���A�(�A�
=A���A�z�A�\)A��A�z�A�
=A���A��
A��RA�G�A�(�A�
=Aə�A��A�Q�A��HA�Aأ�A�33A�{A�  A��A�(�A�ffA�Q�A�=qA���A�
=A�Q�A��A�A��A�
=A���A�=qA��HA�z�A��RB   B ��B�B33B  B��Bp�B=qB33B(�B	G�B
{B�Bz�Bp�BffB\)Bz�Bp�B{B
=B(�B�B{B�B��BB�RB�
B��B��B�\B�B ��B!B"�RB$(�B%G�B&ffB'�B(��B)��B*�\B+�B,��B-B.�\B/\)B0z�B1��B2�RB3�B4��B6ffB7\)B8z�B9p�B:�\B;�B<z�B=G�B>=qB?\)B@Q�BA��BB�\BC�BD��BEBF�HBHQ�BIG�BJ�RBK�BL��BMBN�HBP  BQ�BQBR�HBT(�BU�BV=qBW33BXQ�BYp�BZ�HB\(�B]�B^=qB_\)B`z�Bap�Bb�\Bc�Bd��BeG�Bf�\Bg�Bh��Bip�Bj�RBk�Bl��Bm�Bo
=Bp  Bq�Br{Bs33Btz�Bu�Bv�RBx  Bx��Bz{B{
=B|(�B}G�B}�B
=B�
B��\B��B���B�{B���B�G�B��B�z�B���B��B�  B�z�B�
=B��B�{B�z�B���B�\)B��
B�ffB��HB�\)B��
B��\B�
=B��B�  B�z�B�
=B��B��
B�ffB���B�\)B��
B�Q�B���B�\)B�B�=qB��RB�33B��B�{B��\B��B�p�B�{B��\B�
=B��B�{B��\B�
=B�p�B�  B�ffB��HB�G�B��
B�=qB��RB�33B��B�{B��\B�
=B�p�B��B�ffB���B�G�B�B�=qB���B��B���B�{B�z�B���B�p�B��B�Q�B���B�33B���B�  B�z�B��HB�G�B�B�(�B���B��B��B�=qB��RB��B��B�(�B��\B�
=B�p�B��
B�Q�B��HB�\)B�B�(�B���B��B���B�{B�z�B���B�\)B��
B�Q�B���B�G�B�B�=qB���B�33B�B�Q�B���B�\)B��
B�ffB��HB�p�B��B��\B�
=B��B�{B�z�B�
=BÙ�B�(�Bģ�B�33B�B�Q�B��HB�p�B�  Bȏ\B��BɮB�=qB���B�\)B��B�ffB���BͮB�ffB��HBυB�(�BиRB�G�B��Bҏ\B��B�B�=qB���Bՙ�B�=qB��HBׅB�{BظRB�G�B��B�z�B�33B�B�ffB�
=B�B�ffB�
=Bߙ�B�Q�B�
=B�B�z�B��B�B�z�B��B�B�ffB���B�B�(�B���B�G�B��
B�Q�B�RB��B�B��B�=qB�z�B���B���B��B�\)B��B�  B�=qB�\B���B�
=B�p�B��
B�{B�ffB�RB�
=B�G�B�B��
B�(�B�ffB��B���B�
=B�G�B�B��
B�(�B�z�B��RB���B�33B�p�B�B�  B�Q�B���B���B�G�B���B��
B�{B�Q�B��\B���B�33B�\)B��B�B��B�=qB�z�B��RB���B�33B�p�B��B�(�B�ffB��RB���B�33B�p�B��B��B��B�{B�ffB��\B��HB�
=B�33B�p�B��
C {C 33C G�C p�C �C ��C C �HC ��C ��C{C33CQ�C\)C�\CC�
C  C
=C33C33CG�CffC�C��C�RC�
C��C{C=qCffC�C��C��C�C��C
=C�C=qCffC�C��C�RC�
C{C33C\)Cp�C�C��C�RC�
C�C{CG�Cp�C�\C�C��C�
C��C{C33CG�Cz�C��C��C�C
=C33CG�CQ�Cz�C�\C�C�HC	{C	=qC	ffC	p�C	�\C	�C	�
C	��C
(�C
\)C
�\C
�C
�
C
�C
=C=qC\)C�C��C�HC{C=qCp�C�C��CC��C{C=qCp�C�C�
C  C33CG�CffC�\C�RC  C�C\)Cz�C�\C�RC�HC  CG�Cz�C��C�
C�HC
=C33C\)Cz�C��C  C(�CQ�CffC��CC�C(�CffC��CC�HC
=C33Cz�C�RC�HC{CG�C\)C�\C�RC�C�Cp�C��C�
C��C{CG�Cp�CC  C(�CQ�Cp�C�C�
C(�C\)C�\C�C�
C
=CG�C�\C��C  C=qC\)C�C�RC�C�CffC��C�
C�C�CQ�C�C��C  C33CQ�Cz�CC  C33Cp�C�CC�C =qC z�C �RC �HC!{C!G�C!��C!�
C"  C"(�C"ffC"�C"��C#33C#Q�C#�C#�C${C$G�C$z�C$��C$�
C%�C%ffC%��C%�
C&  C&33C&z�C&��C'  C'G�C'p�C'��C'��C((�C(ffC(�C(C)  C)33C)�\C)��C)�HC*�C*p�C*�C*��C+{C+G�C+z�C+C,
=C,=qC,ffC,��C,�
C-(�C-p�C-�RC-�HC.
=C.G�C.�\C.�
C/�C/=qC/p�C/�C0  C0=qC0z�C0��C0�
C1(�C1ffC1��C1C2  C2G�C2�C2��C2�C3�C3z�C3�RC3��C4{C4Q�C4��C4�C5(�C5G�C5�C5�HC6(�C6Q�C6�C6��C7{C7Q�C7�C7�RC8{C8\)C8z�C8C9{C9Q�C9�C9�RC:{C:\)C:�\C:C;
=C;\)C;��C;C<  C<ffC<��C<��C=
=C=\)C=�C=�HC>{C>Q�C>�C>��C?{C?\)C?C?��C@(�C@�\C@�
CA
=CAQ�CA��CB  CB�CBp�CB�
CC�CCQ�CC�CC��CD(�CDffCD�
CE�CEG�CE��CE��CF(�CFffCF��CG�CGG�CG�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                       ?�  ?��H@@  @�  @�  @�  @�G�A ��A  A   A+�A@  A^�RA~{A��A�Q�A�  A��AϮA߮A�A��B  B�B�B�
B(  B0(�B8(�B@(�BH(�BP(�BW�B`  Bh(�Bp(�Bw�
B�B��B�{B��B��B�  B��
B��B�{B�  B��
B�  B�{B��B��
B�  B��B�  B�  B�  B�(�B�{B�{B�{B��B�  B�=qB�=qB�  B�  B�  B�{C 
=C  C  C  C  C	��C  C
=C  C��C  C  C
=C{C{C  C��C"  C$  C%�C(  C*{C,  C-��C/��C2  C4
=C6  C7��C:  C;��C=�C?��CB  CD  CE��CH
=CJ
=CL  CM�CO�HCQ�CT  CV  CX  CY��C[��C]�C_�Cb  Cd  Ce��Ch  Ci�Cl  Cn
=Cp  Cq��Ct
=Cv  Cw��Cz  C|  C~
=C�C���C���C���C�C�C���C�  C�  C�  C�  C�  C�C�C�C�C���C��C���C���C���C�  C�  C���C���C�C�
=C�
=C�  C�C�
=C�\C�
=C���C���C�  C�C�C�
=C�\C�C�C�C�C�
=C�
=C���C���C���C�
=C�\C�  C�  C���C���C���C���C�  C�  C���C�  C�C�C�C�  C�  C�  C���C���C�  C�
=C�C���C�C�C�  C�  C���C�C�  C�  C���C���C���C�  C�C�  C�  C�C�  C�  C�C���C���C�C�C�  C�C�C�  C�  C�C�C�C�  C�C�
=C�  C���C���C�C�C���C���C���C�  C�
=C�
=C�  C���C���C���C�C�  C�  C�  C�  C�D   D }qD ��D}qD�D� D  D��D  D}qD�qD}qD  D�D�D� D  D� D�qD	� D	�qD
z�D  D� D�D� D�qD}qD�qD� D  D}qD  D� D  D��D  D}qD�qD}qD  D��D�D}qD  D�D�D� D  D�DD��D�qD}qD�D�D  D}qD  D� D  D��D�qD� D �D ��D ��D!z�D"�D"�D#�D#� D$�D$�D%  D%� D&�D&�D'�D'��D(  D(� D)  D)� D*  D*� D+�D+��D,  D,� D-D-��D.D.�D/�D/�D/��D0xRD0��D1}qD1�qD2}qD2�qD3� D4�D4� D5  D5��D6�D6��D7�D7}qD7��D8z�D9  D9�D:�D:��D;  D;� D<D<��D=�D=�D>  D>��D?�D?�D@�D@}qDA  DA��DB  DB}qDC  DC��DD  DDz�DE  DE}qDE��DF� DF�qDG� DHDH��DI�DIz�DI��DJ� DK  DK� DLDL}qDM  DM� DM��DN� DO  DOz�DO�qDP}qDP�qDQ��DQ�qDR� DR�qDS}qDT  DT� DUDU�DV  DV��DW�DW� DW��DX}qDY  DY��DZ�DZ��DZ�qD[}qD[�qD\� D]  D]� D^  D^}qD_  D_� D`  D`}qDa  Da� Db  Db� Dc  Dc��Dd  Dd��De�De��Df�Df}qDg  Dg� Dg�qDh}qDi  Di��Dj�Dj� Dk  Dk� Dk�qDl}qDm  Dm� Dn  Dn}qDo  Do� Dp�Dp� Dq  Dq��Dr�Dr� Ds�Ds��Ds��Dt}qDu  Du� Dv�Dv}qDv�qDw}qDx  Dx� Dy  Dy� Dy�qDz}qD{  D{� D|  D|��D}�D}��D}�qD~z�D~�qD� D�HD�@ D�~�D���D�HD�B�D�� D���D�  D�>�D�� D��HD�  D�AHD��HD��HD�HD�@ D�� D�� D�HD�@ D�~�D�� D�  D�@ D�� D�� D�  D�>�D�� D�D�HD�>�D�}qD���D�HD�B�D��HD���D��qD�=qD�~�D��HD��D�B�D��HD�D��D�B�D��HD���D���D�>�D�}qD���D��D�AHD�}qD�� D�  D�=qD�� D��HD��D�>�D�~�D��HD�HD�AHD�~�D��HD��D�AHD�~�D��qD���D�B�D��HD��qD�  D�AHD�}qD���D���D�=qD��HD��HD�HD�B�D�� D�� D�  D�AHD��HD��HD�  D�>�D��HD�� D�  D�@ D�� D��qD���D�@ D�~�D���D���D�=qD�}qD�� D�  D�B�D�� D���D���D�>�D�~�D���D�  D�@ D�� D��HD�  D�@ D��HD�� D���D�=qD�~�D���D��qD�=qD�}qD��qD��qD�=qD�}qD��qD�HD�@ D�~�D���D��qD�>�D�� D���D�  D�@ D�� D��HD�  D�@ D��HD���D���D�@ D�~�D��HD��D�AHD�� D���D���D�@ D�~�D��qD���D�@ D�~�D���D���D�@ D�~�D���D�HD�B�D���D��HD�HD�B�D�� D��HD�HD�@ D�~�D�� D�HD�@ D��HD��HD�HD�@ D��HD��HD�  D�AHD��HD�D��D�AHD�� D�� D���D�@ D���D���D��qD�=qD�� D��HD�  D�=qD�� D�� D���D�>�D�� D��HD�HD�B�D��HD���D��qD�>�D�~�D���D�  D�@ D�~�D�� D�  D�=qD�~�D���D���D�@ D�� D��HD�  D�=qD�~�D�� D�  D�>�D�}qD�� D�HD�B�D�}qD���D�HD�AHD�� D���D�HD�@ D�~�D�� D�HD�AHD�� D���D�  D�>�D�~�D½qD��qD�@ D�~�D�� D�HD�B�DĀ D��HD�  D�<)D�~�D��HD���D�AHDƀ D��HD�  D�AHD�~�D�� D�HD�@ DȁHD�� D��qD�>�D�~�Dɾ�D�  D�>�D�~�D��HD�  D�>�D�}qD˾�D�  D�@ D�~�D̾�D���D�AHD͂�D�D�  D�@ D�~�Dξ�D���D�AHDρHD�D�HD�AHDЂ�D�� D��qD�=qD�}qDѽqD��qD�=qD�~�DҽqD��qD�>�D�~�DӼ)D��)D�@ DԀ DԽqD���D�AHDՁHD���D��D�AHD�~�DֽqD��qD�=qD�~�D�� D�  D�AHD؂�D���D��D�>�D�~�DٽqD���D�AHDځHD�� D��D�C�Dۀ D۾�D�  D�@ D�}qDܽqD�  D�>�D�}qDݽqD�HD�AHDށHD�D�  D�=qD�}qD��HD�  D�>�D���D�� D��)D�@ DႏDᾸD��)D�@ D₏D��HD�HD�AHD�~�D�qD�  D�AHD�HD�qD�HD�B�D�~�D�qD�  D�AHD�HD�D���D�@ D炏D�qD�  D�C�D�}qD��HD�HD�=qD�}qD��HD���D�@ D��D꾸D�  D�AHD�HD�� D�HD�>�D�~�D쾸D���D�>�D킏D��HD�  D�AHD�~�DD��)D�@ D�}qD��HD�  D�AHD�~�D�� D�  D�>�D� D�D�  D�>�D� D�� D�HD�>�D�~�D�D���D�>�D�HD��HD�HD�>�D�� D��HD�  D�@ D��HD�� D�  D�@ D�� D�� D��qD�>�D�� D���D��qD�>�D�� D�D�  D�@ D��HG�O�>��
>�Q�?.{?�=q?��
?���@   @z�@+�@@  @^�R@s33@��@�\)@��H@��@���@�@�G�@�=q@�
=@��
@���@�
=A   A�A
=qA  Az�A=qA�RA"�\A(��A.{A3�
A8Q�A>{AC�
AH��AN�RATz�AZ=qA_\)Ae�Aj=qAp  Au�Az�HA�Q�A�33A�A���A��\A��A�  A��HA�p�A���A��A�ffA���A�(�A�
=A���A�z�A�\)A��A�z�A�
=A���A��
A��RA�G�A�(�A�
=Aə�A��A�Q�A��HA�Aأ�A�33A�{A�  A��A�(�A�ffA�Q�A�=qA���A�
=A�Q�A��A�A��A�
=A���A�=qA��HA�z�A��RB   B ��B�B33B  B��Bp�B=qB33B(�B	G�B
{B�Bz�Bp�BffB\)Bz�Bp�B{B
=B(�B�B{B�B��BB�RB�
B��B��B�\B�B ��B!B"�RB$(�B%G�B&ffB'�B(��B)��B*�\B+�B,��B-B.�\B/\)B0z�B1��B2�RB3�B4��B6ffB7\)B8z�B9p�B:�\B;�B<z�B=G�B>=qB?\)B@Q�BA��BB�\BC�BD��BEBF�HBHQ�BIG�BJ�RBK�BL��BMBN�HBP  BQ�BQBR�HBT(�BU�BV=qBW33BXQ�BYp�BZ�HB\(�B]�B^=qB_\)B`z�Bap�Bb�\Bc�Bd��BeG�Bf�\Bg�Bh��Bip�Bj�RBk�Bl��Bm�Bo
=Bp  Bq�Br{Bs33Btz�Bu�Bv�RBx  Bx��Bz{B{
=B|(�B}G�B}�B
=B�
B��\B��B���B�{B���B�G�B��B�z�B���B��B�  B�z�B�
=B��B�{B�z�B���B�\)B��
B�ffB��HB�\)B��
B��\B�
=B��B�  B�z�B�
=B��B��
B�ffB���B�\)B��
B�Q�B���B�\)B�B�=qB��RB�33B��B�{B��\B��B�p�B�{B��\B�
=B��B�{B��\B�
=B�p�B�  B�ffB��HB�G�B��
B�=qB��RB�33B��B�{B��\B�
=B�p�B��B�ffB���B�G�B�B�=qB���B��B���B�{B�z�B���B�p�B��B�Q�B���B�33B���B�  B�z�B��HB�G�B�B�(�B���B��B��B�=qB��RB��B��B�(�B��\B�
=B�p�B��
B�Q�B��HB�\)B�B�(�B���B��B���B�{B�z�B���B�\)B��
B�Q�B���B�G�B�B�=qB���B�33B�B�Q�B���B�\)B��
B�ffB��HB�p�B��B��\B�
=B��B�{B�z�B�
=BÙ�B�(�Bģ�B�33B�B�Q�B��HB�p�B�  Bȏ\B��BɮB�=qB���B�\)B��B�ffB���BͮB�ffB��HBυB�(�BиRB�G�B��Bҏ\B��B�B�=qB���Bՙ�B�=qB��HBׅB�{BظRB�G�B��B�z�B�33B�B�ffB�
=B�B�ffB�
=Bߙ�B�Q�B�
=B�B�z�B��B�B�z�B��B�B�ffB���B�B�(�B���B�G�B��
B�Q�B�RB��B�B��B�=qB�z�B���B���B��B�\)B��B�  B�=qB�\B���B�
=B�p�B��
B�{B�ffB�RB�
=B�G�B�B��
B�(�B�ffB��B���B�
=B�G�B�B��
B�(�B�z�B��RB���B�33B�p�B�B�  B�Q�B���B���B�G�B���B��
B�{B�Q�B��\B���B�33B�\)B��B�B��B�=qB�z�B��RB���B�33B�p�B��B�(�B�ffB��RB���B�33B�p�B��B��B��B�{B�ffB��\B��HB�
=B�33B�p�B��
C {C 33C G�C p�C �C ��C C �HC ��C ��C{C33CQ�C\)C�\CC�
C  C
=C33C33CG�CffC�C��C�RC�
C��C{C=qCffC�C��C��C�C��C
=C�C=qCffC�C��C�RC�
C{C33C\)Cp�C�C��C�RC�
C�C{CG�Cp�C�\C�C��C�
C��C{C33CG�Cz�C��C��C�C
=C33CG�CQ�Cz�C�\C�C�HC	{C	=qC	ffC	p�C	�\C	�C	�
C	��C
(�C
\)C
�\C
�C
�
C
�C
=C=qC\)C�C��C�HC{C=qCp�C�C��CC��C{C=qCp�C�C�
C  C33CG�CffC�\C�RC  C�C\)Cz�C�\C�RC�HC  CG�Cz�C��C�
C�HC
=C33C\)Cz�C��C  C(�CQ�CffC��CC�C(�CffC��CC�HC
=C33Cz�C�RC�HC{CG�C\)C�\C�RC�C�Cp�C��C�
C��C{CG�Cp�CC  C(�CQ�Cp�C�C�
C(�C\)C�\C�C�
C
=CG�C�\C��C  C=qC\)C�C�RC�C�CffC��C�
C�C�CQ�C�C��C  C33CQ�Cz�CC  C33Cp�C�CC�C =qC z�C �RC �HC!{C!G�C!��C!�
C"  C"(�C"ffC"�C"��C#33C#Q�C#�C#�C${C$G�C$z�C$��C$�
C%�C%ffC%��C%�
C&  C&33C&z�C&��C'  C'G�C'p�C'��C'��C((�C(ffC(�C(C)  C)33C)�\C)��C)�HC*�C*p�C*�C*��C+{C+G�C+z�C+C,
=C,=qC,ffC,��C,�
C-(�C-p�C-�RC-�HC.
=C.G�C.�\C.�
C/�C/=qC/p�C/�C0  C0=qC0z�C0��C0�
C1(�C1ffC1��C1C2  C2G�C2�C2��C2�C3�C3z�C3�RC3��C4{C4Q�C4��C4�C5(�C5G�C5�C5�HC6(�C6Q�C6�C6��C7{C7Q�C7�C7�RC8{C8\)C8z�C8C9{C9Q�C9�C9�RC:{C:\)C:�\C:C;
=C;\)C;��C;C<  C<ffC<��C<��C=
=C=\)C=�C=�HC>{C>Q�C>�C>��C?{C?\)C?C?��C@(�C@�\C@�
CA
=CAQ�CA��CB  CB�CBp�CB�
CC�CCQ�CC�CC��CD(�CDffCD�
CE�CEG�CE��CE��CF(�CFffCF��CG�CGG�CG�CG��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                       @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�o@�"G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�\)A�\)A�S�A�E�A�G�A�G�A�K�A�K�A�K�A�M�A�O�A�VA�VA�VA�XA�\)A�\)A�^5A�bNA�ffA�ffA�bNA�`BA�ffA�l�A�p�A�r�A�r�A�n�A�ffA�^5A�\)A�XA�VA�?}A��mA���A� �A�r�AپwA�$�A�=qA��A�ffA���A�bNA�  A��Aө�A�=qA�A�"�A��A�O�A��
A�I�A�$�A��A�?}A�VAȼjA�O�AǴ9A�
=A���A�5?A��
Aŧ�A�7LA���A�7LA�
=A�v�A��`A��9A��A�^5A�JA��jA���A�&�A��A�A���A�O�A��PA�|�A��A��FA�O�A�1A�hsA�\)A��RA�I�A�/A�;dA��/A���A��A���A�oA��A�t�A��RA�?}A��uA�M�A�bA�n�A�A�A�ĜA�ZA�{A�dZA�A��\A�A�A��A�jA�+A�dZA�;dA��A�XA~z�A{�#Az��AvAnA�Ajr�Af�`Abv�A_��AWO�AT��ASXAQ"�AP�DAP�AM�AI�FAH-AGp�AF1AD�jAC|�AA�hA@  A>r�A=|�A;��A;&�A8�`A6�9A5�-A4�A3"�A1ƨA0�+A.�jA-C�A,�DA+�A+VA*(�A(�jA&�`A%�A$z�A$ �A#�;A#O�A"�!A VA`BAI�A��A\)AC�A�\AA�A��A�;A��A�+A�^AK�AVA1A�A33A�;An�A��Ap�A�9A��A�AoA��A|�AVA�mAx�A
�DAĜA��AC�A�;A%A�+AM�A�wAA�A��AVAjA�9A�9AbNAl�A �A E�@���@�E�@�@��@�ƨ@���@���@�p�@�ff@��7@�n�@�I�@���@�D@�@��@�V@��m@��H@�^5@��T@�O�@� �@�$�@���@���@�I�@��@�\)@ݺ^@�Q�@��@۶F@�^5@�%@��/@؛�@؋D@�1@�ȴ@��@��@��;@��
@���@�ƨ@ӥ�@�ff@�9X@�1@У�@�\)@�@���@̬@�Q�@��m@ˮ@�|�@�;d@�@ʰ!@�J@��@ɺ^@ə�@ȣ�@ǶF@ǍP@�K�@�ȴ@�^5@�@��#@�@Ų-@Ų-@ũ�@š�@�?}@Ĵ9@�b@�S�@���@�@�v�@�V@�=q@�=q@�5?@�-@���@�z�@� �@�1@�  @��@���@��F@�\)@�o@��R@�ff@��@���@��T@��-@���@�hs@��@��`@�Z@��P@�S�@�o@�n�@�=q@�x�@���@�Ĝ@�Ĝ@��9@��@���@��D@�Q�@�A�@� �@��@�\)@��@�ȴ@�5?@�@�hs@�G�@�?}@�  @�|�@�K�@��R@���@��\@�v�@�=q@�x�@�O�@�/@��9@�A�@�1'@� �@���@�ƨ@�C�@�ȴ@��R@�^5@���@�X@���@���@�z�@�I�@�1'@� �@�  @��F@��P@�l�@�S�@�;d@�
=@���@�ȴ@��R@�~�@�$�@��@�?}@�/@��9@�Z@���@�K�@��@��@��!@�$�@���@��@�G�@�%@��/@�Ĝ@��u@�z�@�bN@�A�@� �@���@�"�@�@���@���@�n�@�@���@�?}@���@��`@�z�@�bN@� �@��m@�ƨ@���@�|�@�S�@�"�@�
=@��!@�^5@��@�@��^@�7L@���@���@���@�z�@�j@�Z@��@��@�S�@�S�@�K�@�;d@�33@�33@�33@�"�@���@�~�@�=q@�@��@���@�hs@�G�@��/@��D@�Q�@�(�@�  @�l�@�33@�o@��H@���@�5?@��#@���@��h@�G�@�V@���@��@�I�@� �@�  @��w@�|�@�S�@�C�@�33@�"�@��y@�E�@�$�@�$�@��-@�O�@�/@��@�Ĝ@���@��u@��@�z�@�j@�Q�@�9X@�(�@� �@���@���@�;d@��@���@���@���@�ff@��@��@��T@��-@��7@�p�@�O�@��/@��j@�bN@�9X@�1'@�1'@�b@��
@��w@�l�@�"�@��H@��!@�V@���@��^@���@��@�/@���@��@��u@�Q�@��@�  @��m@���@��@�33@�@��y@���@�ff@�@��7@�?}@���@�(�@���@��;@���@��@��@�dZ@�33@�o@�o@�o@�o@���@�ȴ@�v�@�=q@�{@���@�hs@�O�@�7L@��@��@��@�Z@��@~��@}�T@}�@}?}@|�j@{�m@{t�@{"�@{o@{o@z�@zn�@y�@y�#@y�^@yhs@y&�@y%@x��@xQ�@w��@w+@v�y@vȴ@v�+@vV@v@u@u�-@u`B@u�@uV@tj@s�
@so@r��@r~�@r=q@q��@q��@q�@pbN@pb@o�@o|�@o+@n�R@n$�@m�h@m`B@mV@l�j@lZ@l9X@k�m@k�@k@j�\@jn�@j=q@j�@i��@i�#@i��@i�@h�@h�@hQ�@hb@g�w@gl�@gK�@g+@g�@f�y@fȴ@f��@f5?@e�-@e/@dz�@dI�@d(�@d�@c��@c��@b~�@b�@a��@a��@a��@aG�@a�@`��@`�u@`Q�@_�w@_l�@^�y@^V@^@]��@]p�@\�/@\1@[�F@[�@[C�@Z��@Z�@Y�7@Yhs@YG�@X��@X�`@XĜ@Xr�@X1'@W��@W
=@V��@V5?@U�T@U@U�@UV@TZ@S��@SS�@S33@S@R��@R^5@Q�@QG�@Q%@P�`@PĜ@P1'@O�;@O\)@O�@N�R@NE�@M�h@L�@K��@K�@K@J�!@J��@J^5@JJ@I��@I&�@Hr�@HA�@H  @G\)@G
=@F�R@FV@E�@E�-@EO�@D��@D�@C��@C"�@B��@B��@A�@Ax�@Ahs@AG�@@Q�@?�@?|�@?
=@>ff@>$�@=�@=��@=�@=/@<��@;��@;�
@;��@;S�@:�H@:M�@9��@9��@9hs@9&�@8��@8��@8 �@7��@7K�@6�@6��@6E�@6@5�h@5O�@4��@4�j@4�j@4�@4�@4�j@4�j@4�j@4��@49X@41@3��@3��@3�@3dZ@3C�@3"�@3o@2��@2~�@2=q@1�^@1G�@17L@1�@0Ĝ@0r�@0A�@0b@/�;@/�P@/�P@/|�@/\)@.��@.�R@.��@.v�@-��@-O�@,�@,j@,(�@+��@+t�@+t�@+dZ@+C�@*��@*n�@*-@*J@*J@)�@)��@)�7@)X@)7L@)&�@(��@(�@(A�@'�@'��@'��@'|�@'\)@';d@'�@'
=@&ȴ@&��@&��@&��@&��@&v�@&V@&{@%��@%��@%��@%�T@%��@%�h@%?}@%�@$�@$�@$��@$�D@$z�@$z�@$Z@$9X@#�
@#�@#�@#t�@#C�@"�\@"=q@"�@!��@!hs@!&�@ ��@ �@ r�@ r�@ r�@ r�@ Q�@ A�@ b@   @�P@��@��@�+@ff@$�@@@�T@�T@��@�@`B@?}@/@/@�@�@V@�/@Z@�
@�@33@�@�@�@�H@��@J@��@�7@��@��@�@��@��@�u@r�@r�@r�@1'@��@|�@;d@K�@;d@K�@;d@�@�y@��@v�@{@{@@@�T@@�h@?}@��@�/@�j@�j@�@j@(�@(�@�@��@�
@�@o@��@�!@�\@�\@�@�@�@�^@�7@hs@�@%@��@�u@�A�ZA�ZA�ZA�\)A�^5A�^5A�^5A�\)A�ZA�O�A�I�A�C�A�A�A�E�A�G�A�C�A�K�A�K�A�I�A�I�A�I�A�I�A�E�A�I�A�G�A�I�A�I�A�K�A�I�A�K�A�K�A�O�A�Q�A�O�A�Q�A�XA�ZA�\)A�XA�XA�VA�S�A�O�A�Q�A�S�A�Q�A�S�A�Q�A�S�A�VA�XA�ZA�\)A�XA�XA�S�A�^5A�ZA�ZA�\)A�ZA�XA�XA�VA�XA�XA�XA�ZA�`BA�bNA�`BA�bNA�bNA�`BA�^5A�ZA�\)A�^5A�bNA�ffA�dZA�dZA�ffA�hsA�ffA�hsA�hsA�hsA�ffA�dZA�ffA�ffA�ffA�ffA�bNA�bNA�bNA�dZA�ffA�ffA�ffA�bNA�dZA�`BA�^5A�\)A�^5A�bNA�bNA�bNA�bNA�bNA�`BA�`BA�^5A�^5A�`BA�bNA�ffA�hsA�l�A�n�A�p�A�p�A�jA�ffA�ffA�hsA�l�A�n�A�n�A�p�A�r�A�r�A�p�A�p�A�p�A�n�A�n�A�r�A�p�A�r�A�r�A�r�A�r�A�r�A�r�A�r�A�t�A�t�A�t�A�t�A�r�A�n�A�l�A�l�A�l�A�l�A�ffA�jA�l�A�jA�jA�jA�hsA�dZA�dZA�dZA�dZA�`BA�^5A�\)A�\)A�\)A�\)A�ZA�ZA�\)A�ZA�ZA�\)A�^5A�\)A�\)A�\)A�ZA�XA�XA�S�A�Q�A�Q�A�Q�A�S�A�S�A�S�A�VA�VA�VA�S�A�K�A�E�A�?}A�5?A�-A�-A�/A�&�A�bA��A��
A޲-Aޗ�A�z�A�n�A�I�A�C�A�=qA�Aܟ�Aۺ^A�bNA�?}A��A�A��A��HA��
A�Aڰ!A�|�A�jA�XA�A�A�=qA�(�A�  A��;A�ȴAٸRAٰ!A٣�AٓuAه+A�p�A�dZA�G�A�(�A�JA��A���Aؗ�A؋DA�z�A�p�A�dZA�ZA�S�A��A�^5A�S�A�Q�A�E�A�9XA�?}A��A��
AֶFA֡�A֥�A֓uA�z�A�hsA�A�A�/A��A�JA��A��/A���A���A�ĜAվwAմ9A՟�AՋDA�z�A�x�A�l�A�ffA�^5A�O�A�I�A�7LA�bA���A�  A�  A�A���A���A���A��`A��TA��TA��HA��TA��;A��;A��#AԾwA�33A���A���Aӟ�Aӗ�AӓuAӑhAӋDA�t�A�\)A�VA�VA�K�A�7LA�33A�1'A�&�A��A��A��A�oA�oA�%A��A��A���A���AҬAғuA�t�A�ffA�C�AѾwA�"�A���AЍPA�M�A�I�A�C�A�7LA���AϮAϧ�AϓuA�|�A�n�A�I�A�1'A�"�A�oA���A��yA��`A��HA��A���A�ĜAΰ!AΙ�A�~�A�ZA�A�A�1'A��A��A͡�A�l�A�E�A���A��yA��`A��#A̡�A��A���A��A���A˾wA˧�A�S�AʋDA�l�A�/A��yAɕ�A�n�A�I�A� �A�
=A��HA���A�ȴA�ĜA�A�AȾwAȶFAȴ9Aȧ�AȍPA�\)A�=qA�
=A��#A�AǸRAǴ9Aǲ-AǮAǥ�A�t�A�?}A�{A��A��HA��mA��mA��mA��mA��TA��#A���A���A�ȴA�AƶFAƲ-Aư!AƬAơ�AƑhA�|�A�bNA�S�A�G�A�7LA�/A�$�A�{A�  A��A��yA��`A��HA��/A��#A��#A��#A��A��
A���A���A���A���A���A���A���A���A���A���A�Aź^AŴ9Aũ�Aŧ�Aţ�Aŝ�Ař�Aŕ�AœuAŏ\AŋDAŇ+AŁA�z�A�jA�ZA�XA�S�A�Q�A�M�A�;dA�(�A��A�{A�VA�A�  A�  A��A��A��TA��/A��#A��#A��A���A���A�AĶFAĮAĩ�AĴ9AĲ-Aġ�AēuAčPAċDAāA�z�A�v�A�l�A�O�A�Q�A�Q�A�M�A�+A�$�A��A�{A�bA�JA�A���A��A��HA�Aå�AÕ�AÅA�I�A�/A�VA��A�ȴA§�A£�A£�A£�A¡�A£�A�AhA+A�~�A�|�A�|�A�|�A�~�A�~�A�|�A�|�A�|�A�x�A�v�A�ffA�S�A�O�A�Q�A�K�A�E�A�7LA�-A�"�A��A�1A��A��jA���A���A���A���A��hA��A�r�A�dZA�Q�A�?}A�/A��A��A��9A�bNA��A�VA�A��A��A��A��mA��A��wA��FA���A���A���A��PA��7A��A��A�~�A�|�A�z�A�v�A�r�A�p�A�`BA�XA�M�A�K�A�E�A�?}A�7LA�-A�$�A��A��A��A��A�{A�A���A���A��mA��A���A���A���A���A�ĜA�A��^A��^A��-A��A���A�x�A�bNA�5?A�A��mA��
A��^A���A�v�A�dZA�VA�E�A�=qA�=qA�=qA�9XA�1'A�1'A�-A��A��A��jA��uA�ZA�&�A�1A��/A��jA��^A��RA���A���A��hA�z�A�XA�=qA��A���A��`A�ȴA���A�XA�=qA�=qA�7LA�+A��A���A��
A�ĜA��FA���A���A���A��\A��7A�~�A�n�A�`BA��A���A�x�A�hsA�`BA�=qA���A��A�VA���A��FA�jA�oA��A���A�ZA�M�A�I�A�E�A�A�A�;dA�9XA�9XA�7LA�5?A�(�A�oA�%A��A��/A���A�ƨA���A��jA��jA��9A���A��hA�ffA�VA�S�A�VA�VA�S�A�M�A�M�A�E�A�+A��A��A�{A�oA�bA�1A��A��mA��#A�ĜA��A���A�l�A�7LA�(�A�{A��A���A���A��7A�z�A�bNA�5?A���A��/A���A�A��RA��9A��9A��-A��-A��-A��9A��A���A���A�r�A�+A��A�  A��TA��RA���A�l�A���A��RA�XA�dZA�ƨA�^5A��A��PA�v�A�M�A�33A��A��A�ƨA���A��DA�S�A�5?A���A���A�1'A�1A��A���A���A��-A���A��+A�^5A�C�A�"�A�bA��A��A��
A��RA���A��+A��A�v�A�dZA�VA��A�=qA�z�A�ĜA��FA��A�^5A�%A�ĜA���A�\)A���A���A�z�A�1'A��!A��RA��mA��A�C�A��A��PA�(�A�ƨA�t�A�^5A�C�A��A��jA���A�p�A��A�A��PA�x�A�\)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                       A�\)A�\)A�S�A�E�A�G�A�G�A�K�A�K�A�K�A�M�A�O�A�VA�VA�VA�XA�\)A�\)A�^5A�bNA�ffA�ffA�bNA�`BA�ffA�l�A�p�A�r�A�r�A�n�A�ffA�^5A�\)A�XA�VA�?}A��mA���A� �A�r�AپwA�$�A�=qA��A�ffA���A�bNA�  A��Aө�A�=qA�A�"�A��A�O�A��
A�I�A�$�A��A�?}A�VAȼjA�O�AǴ9A�
=A���A�5?A��
Aŧ�A�7LA���A�7LA�
=A�v�A��`A��9A��A�^5A�JA��jA���A�&�A��A�A���A�O�A��PA�|�A��A��FA�O�A�1A�hsA�\)A��RA�I�A�/A�;dA��/A���A��A���A�oA��A�t�A��RA�?}A��uA�M�A�bA�n�A�A�A�ĜA�ZA�{A�dZA�A��\A�A�A��A�jA�+A�dZA�;dA��A�XA~z�A{�#Az��AvAnA�Ajr�Af�`Abv�A_��AWO�AT��ASXAQ"�AP�DAP�AM�AI�FAH-AGp�AF1AD�jAC|�AA�hA@  A>r�A=|�A;��A;&�A8�`A6�9A5�-A4�A3"�A1ƨA0�+A.�jA-C�A,�DA+�A+VA*(�A(�jA&�`A%�A$z�A$ �A#�;A#O�A"�!A VA`BAI�A��A\)AC�A�\AA�A��A�;A��A�+A�^AK�AVA1A�A33A�;An�A��Ap�A�9A��A�AoA��A|�AVA�mAx�A
�DAĜA��AC�A�;A%A�+AM�A�wAA�A��AVAjA�9A�9AbNAl�A �A E�@���@�E�@�@��@�ƨ@���@���@�p�@�ff@��7@�n�@�I�@���@�D@�@��@�V@��m@��H@�^5@��T@�O�@� �@�$�@���@���@�I�@��@�\)@ݺ^@�Q�@��@۶F@�^5@�%@��/@؛�@؋D@�1@�ȴ@��@��@��;@��
@���@�ƨ@ӥ�@�ff@�9X@�1@У�@�\)@�@���@̬@�Q�@��m@ˮ@�|�@�;d@�@ʰ!@�J@��@ɺ^@ə�@ȣ�@ǶF@ǍP@�K�@�ȴ@�^5@�@��#@�@Ų-@Ų-@ũ�@š�@�?}@Ĵ9@�b@�S�@���@�@�v�@�V@�=q@�=q@�5?@�-@���@�z�@� �@�1@�  @��@���@��F@�\)@�o@��R@�ff@��@���@��T@��-@���@�hs@��@��`@�Z@��P@�S�@�o@�n�@�=q@�x�@���@�Ĝ@�Ĝ@��9@��@���@��D@�Q�@�A�@� �@��@�\)@��@�ȴ@�5?@�@�hs@�G�@�?}@�  @�|�@�K�@��R@���@��\@�v�@�=q@�x�@�O�@�/@��9@�A�@�1'@� �@���@�ƨ@�C�@�ȴ@��R@�^5@���@�X@���@���@�z�@�I�@�1'@� �@�  @��F@��P@�l�@�S�@�;d@�
=@���@�ȴ@��R@�~�@�$�@��@�?}@�/@��9@�Z@���@�K�@��@��@��!@�$�@���@��@�G�@�%@��/@�Ĝ@��u@�z�@�bN@�A�@� �@���@�"�@�@���@���@�n�@�@���@�?}@���@��`@�z�@�bN@� �@��m@�ƨ@���@�|�@�S�@�"�@�
=@��!@�^5@��@�@��^@�7L@���@���@���@�z�@�j@�Z@��@��@�S�@�S�@�K�@�;d@�33@�33@�33@�"�@���@�~�@�=q@�@��@���@�hs@�G�@��/@��D@�Q�@�(�@�  @�l�@�33@�o@��H@���@�5?@��#@���@��h@�G�@�V@���@��@�I�@� �@�  @��w@�|�@�S�@�C�@�33@�"�@��y@�E�@�$�@�$�@��-@�O�@�/@��@�Ĝ@���@��u@��@�z�@�j@�Q�@�9X@�(�@� �@���@���@�;d@��@���@���@���@�ff@��@��@��T@��-@��7@�p�@�O�@��/@��j@�bN@�9X@�1'@�1'@�b@��
@��w@�l�@�"�@��H@��!@�V@���@��^@���@��@�/@���@��@��u@�Q�@��@�  @��m@���@��@�33@�@��y@���@�ff@�@��7@�?}@���@�(�@���@��;@���@��@��@�dZ@�33@�o@�o@�o@�o@���@�ȴ@�v�@�=q@�{@���@�hs@�O�@�7L@��@��@��@�Z@��@~��@}�T@}�@}?}@|�j@{�m@{t�@{"�@{o@{o@z�@zn�@y�@y�#@y�^@yhs@y&�@y%@x��@xQ�@w��@w+@v�y@vȴ@v�+@vV@v@u@u�-@u`B@u�@uV@tj@s�
@so@r��@r~�@r=q@q��@q��@q�@pbN@pb@o�@o|�@o+@n�R@n$�@m�h@m`B@mV@l�j@lZ@l9X@k�m@k�@k@j�\@jn�@j=q@j�@i��@i�#@i��@i�@h�@h�@hQ�@hb@g�w@gl�@gK�@g+@g�@f�y@fȴ@f��@f5?@e�-@e/@dz�@dI�@d(�@d�@c��@c��@b~�@b�@a��@a��@a��@aG�@a�@`��@`�u@`Q�@_�w@_l�@^�y@^V@^@]��@]p�@\�/@\1@[�F@[�@[C�@Z��@Z�@Y�7@Yhs@YG�@X��@X�`@XĜ@Xr�@X1'@W��@W
=@V��@V5?@U�T@U@U�@UV@TZ@S��@SS�@S33@S@R��@R^5@Q�@QG�@Q%@P�`@PĜ@P1'@O�;@O\)@O�@N�R@NE�@M�h@L�@K��@K�@K@J�!@J��@J^5@JJ@I��@I&�@Hr�@HA�@H  @G\)@G
=@F�R@FV@E�@E�-@EO�@D��@D�@C��@C"�@B��@B��@A�@Ax�@Ahs@AG�@@Q�@?�@?|�@?
=@>ff@>$�@=�@=��@=�@=/@<��@;��@;�
@;��@;S�@:�H@:M�@9��@9��@9hs@9&�@8��@8��@8 �@7��@7K�@6�@6��@6E�@6@5�h@5O�@4��@4�j@4�j@4�@4�@4�j@4�j@4�j@4��@49X@41@3��@3��@3�@3dZ@3C�@3"�@3o@2��@2~�@2=q@1�^@1G�@17L@1�@0Ĝ@0r�@0A�@0b@/�;@/�P@/�P@/|�@/\)@.��@.�R@.��@.v�@-��@-O�@,�@,j@,(�@+��@+t�@+t�@+dZ@+C�@*��@*n�@*-@*J@*J@)�@)��@)�7@)X@)7L@)&�@(��@(�@(A�@'�@'��@'��@'|�@'\)@';d@'�@'
=@&ȴ@&��@&��@&��@&��@&v�@&V@&{@%��@%��@%��@%�T@%��@%�h@%?}@%�@$�@$�@$��@$�D@$z�@$z�@$Z@$9X@#�
@#�@#�@#t�@#C�@"�\@"=q@"�@!��@!hs@!&�@ ��@ �@ r�@ r�@ r�@ r�@ Q�@ A�@ b@   @�P@��@��@�+@ff@$�@@@�T@�T@��@�@`B@?}@/@/@�@�@V@�/@Z@�
@�@33@�@�@�@�H@��@J@��@�7@��@��@�@��@��@�u@r�@r�@r�@1'@��@|�@;d@K�@;d@K�@;d@�@�y@��@v�@{@{@@@�T@@�h@?}@��@�/@�j@�j@�@j@(�@(�@�@��@�
@�@o@��@�!@�\@�\@�@�@�@�^@�7@hs@�@%@��@�uG�O�A�ZA�ZA�ZA�\)A�^5A�^5A�^5A�\)A�ZA�O�A�I�A�C�A�A�A�E�A�G�A�C�A�K�A�K�A�I�A�I�A�I�A�I�A�E�A�I�A�G�A�I�A�I�A�K�A�I�A�K�A�K�A�O�A�Q�A�O�A�Q�A�XA�ZA�\)A�XA�XA�VA�S�A�O�A�Q�A�S�A�Q�A�S�A�Q�A�S�A�VA�XA�ZA�\)A�XA�XA�S�A�^5A�ZA�ZA�\)A�ZA�XA�XA�VA�XA�XA�XA�ZA�`BA�bNA�`BA�bNA�bNA�`BA�^5A�ZA�\)A�^5A�bNA�ffA�dZA�dZA�ffA�hsA�ffA�hsA�hsA�hsA�ffA�dZA�ffA�ffA�ffA�ffA�bNA�bNA�bNA�dZA�ffA�ffA�ffA�bNA�dZA�`BA�^5A�\)A�^5A�bNA�bNA�bNA�bNA�bNA�`BA�`BA�^5A�^5A�`BA�bNA�ffA�hsA�l�A�n�A�p�A�p�A�jA�ffA�ffA�hsA�l�A�n�A�n�A�p�A�r�A�r�A�p�A�p�A�p�A�n�A�n�A�r�A�p�A�r�A�r�A�r�A�r�A�r�A�r�A�r�A�t�A�t�A�t�A�t�A�r�A�n�A�l�A�l�A�l�A�l�A�ffA�jA�l�A�jA�jA�jA�hsA�dZA�dZA�dZA�dZA�`BA�^5A�\)A�\)A�\)A�\)A�ZA�ZA�\)A�ZA�ZA�\)A�^5A�\)A�\)A�\)A�ZA�XA�XA�S�A�Q�A�Q�A�Q�A�S�A�S�A�S�A�VA�VA�VA�S�A�K�A�E�A�?}A�5?A�-A�-A�/A�&�A�bA��A��
A޲-Aޗ�A�z�A�n�A�I�A�C�A�=qA�Aܟ�Aۺ^A�bNA�?}A��A�A��A��HA��
A�Aڰ!A�|�A�jA�XA�A�A�=qA�(�A�  A��;A�ȴAٸRAٰ!A٣�AٓuAه+A�p�A�dZA�G�A�(�A�JA��A���Aؗ�A؋DA�z�A�p�A�dZA�ZA�S�A��A�^5A�S�A�Q�A�E�A�9XA�?}A��A��
AֶFA֡�A֥�A֓uA�z�A�hsA�A�A�/A��A�JA��A��/A���A���A�ĜAվwAմ9A՟�AՋDA�z�A�x�A�l�A�ffA�^5A�O�A�I�A�7LA�bA���A�  A�  A�A���A���A���A��`A��TA��TA��HA��TA��;A��;A��#AԾwA�33A���A���Aӟ�Aӗ�AӓuAӑhAӋDA�t�A�\)A�VA�VA�K�A�7LA�33A�1'A�&�A��A��A��A�oA�oA�%A��A��A���A���AҬAғuA�t�A�ffA�C�AѾwA�"�A���AЍPA�M�A�I�A�C�A�7LA���AϮAϧ�AϓuA�|�A�n�A�I�A�1'A�"�A�oA���A��yA��`A��HA��A���A�ĜAΰ!AΙ�A�~�A�ZA�A�A�1'A��A��A͡�A�l�A�E�A���A��yA��`A��#A̡�A��A���A��A���A˾wA˧�A�S�AʋDA�l�A�/A��yAɕ�A�n�A�I�A� �A�
=A��HA���A�ȴA�ĜA�A�AȾwAȶFAȴ9Aȧ�AȍPA�\)A�=qA�
=A��#A�AǸRAǴ9Aǲ-AǮAǥ�A�t�A�?}A�{A��A��HA��mA��mA��mA��mA��TA��#A���A���A�ȴA�AƶFAƲ-Aư!AƬAơ�AƑhA�|�A�bNA�S�A�G�A�7LA�/A�$�A�{A�  A��A��yA��`A��HA��/A��#A��#A��#A��A��
A���A���A���A���A���A���A���A���A���A���A�Aź^AŴ9Aũ�Aŧ�Aţ�Aŝ�Ař�Aŕ�AœuAŏ\AŋDAŇ+AŁA�z�A�jA�ZA�XA�S�A�Q�A�M�A�;dA�(�A��A�{A�VA�A�  A�  A��A��A��TA��/A��#A��#A��A���A���A�AĶFAĮAĩ�AĴ9AĲ-Aġ�AēuAčPAċDAāA�z�A�v�A�l�A�O�A�Q�A�Q�A�M�A�+A�$�A��A�{A�bA�JA�A���A��A��HA�Aå�AÕ�AÅA�I�A�/A�VA��A�ȴA§�A£�A£�A£�A¡�A£�A�AhA+A�~�A�|�A�|�A�|�A�~�A�~�A�|�A�|�A�|�A�x�A�v�A�ffA�S�A�O�A�Q�A�K�A�E�A�7LA�-A�"�A��A�1A��A��jA���A���A���A���A��hA��A�r�A�dZA�Q�A�?}A�/A��A��A��9A�bNA��A�VA�A��A��A��A��mA��A��wA��FA���A���A���A��PA��7A��A��A�~�A�|�A�z�A�v�A�r�A�p�A�`BA�XA�M�A�K�A�E�A�?}A�7LA�-A�$�A��A��A��A��A�{A�A���A���A��mA��A���A���A���A���A�ĜA�A��^A��^A��-A��A���A�x�A�bNA�5?A�A��mA��
A��^A���A�v�A�dZA�VA�E�A�=qA�=qA�=qA�9XA�1'A�1'A�-A��A��A��jA��uA�ZA�&�A�1A��/A��jA��^A��RA���A���A��hA�z�A�XA�=qA��A���A��`A�ȴA���A�XA�=qA�=qA�7LA�+A��A���A��
A�ĜA��FA���A���A���A��\A��7A�~�A�n�A�`BA��A���A�x�A�hsA�`BA�=qA���A��A�VA���A��FA�jA�oA��A���A�ZA�M�A�I�A�E�A�A�A�;dA�9XA�9XA�7LA�5?A�(�A�oA�%A��A��/A���A�ƨA���A��jA��jA��9A���A��hA�ffA�VA�S�A�VA�VA�S�A�M�A�M�A�E�A�+A��A��A�{A�oA�bA�1A��A��mA��#A�ĜA��A���A�l�A�7LA�(�A�{A��A���A���A��7A�z�A�bNA�5?A���A��/A���A�A��RA��9A��9A��-A��-A��-A��9A��A���A���A�r�A�+A��A�  A��TA��RA���A�l�A���A��RA�XA�dZA�ƨA�^5A��A��PA�v�A�M�A�33A��A��A�ƨA���A��DA�S�A�5?A���A���A�1'A�1A��A���A���A��-A���A��+A�^5A�C�A�"�A�bA��A��A��
A��RA���A��+A��A�v�A�dZA�VA��A�=qA�z�A�ĜA��FA��A�^5A�%A�ĜA���A�\)A���A���A�z�A�1'A��!A��RA��mA��A�C�A��A��PA�(�A�ƨA�t�A�^5A�C�A��A��jA���A�p�A��A�A��PA�x�A�\)G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                       ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
�B
�B
�B
�B
 B
 B
 B
 B
hB
4B
 B
 B
 B
4B
�B
�B
�B
�B
�B
bB
�B
�B
 B
4B
 B
 B
�B
�B
4B
�B
�B
FB
�B
�B
�B
SB	��B	��B	�eB	�B	�B	p�B	}VB	�IB	��B	��B	��B	��B	��B	��B	�^B	��B	�BB	�NB	�9B	�yB	��B	��B	�B

�B
�B
�B
.}B
CaB
K)B
YKB
d�B
iB
{B
�bB
ŢB
ںB
�QB
��B�B+6B2�B9�B@�BW�Bf�By�B��B��B��B��B�wB�}B��B�B��B��B�6BϫB�6B�^B�$B�B�0B��B��B��B��ByrBe�B[WBR�BM6B8�B,�BFB
�WB
��B
��B
�sB
�zB
��B
�_B
��B
o B
PHB
=qB
)�B
B
�B
�B	�sB	��B	� B	�	B	�B	m�B	T�B	IRB	3hB	�B	oB	�B	 iB�JB	;B� B�B�NB�BܒB�)BٴBخB՛B�2BҽB�BB��B�B�gB�'BĜB�*B�^B��B��B�}B�=B��B�XB��B��B�kB�LB�B��B�nB�\B�tB��B�~B��B��B�1B��B�uB�FB�7B�_B��B��B��B�'B��B�OB��B�6B�OB��B��BϫB�
B�B�B	�B	�B	�B	�B		B	B�`B�B�2B��B�B	 �B�"B	�B	%B	'�B	B	
�B	.B	�B	�B	�B	B	(B	{B��B�"B�B�B�`B�ZB�B	B	+B	bB	JB		7B	�B	�B	�B	B	DB	oB	B	�B	B	�B	xB	"�B	*�B	+B	*eB	+kB	.IB	*0B	($B	&LB	'RB	'RB	(XB	,=B	2�B	3�B	3�B	5B	:�B	@�B	HKB	IB	IRB	IB	H�B	D�B	F?B	R B	T�B	ZB	Y�B	]/B	]�B	aHB	b�B	c�B	e�B	gmB	iyB	l�B	m�B	o B	o B	r�B	u�B	w�B	y	B	|B	~�B	��B	�uB	�B	��B	�B	��B	�MB	��B	�B	��B	��B	�7B	�B	�B	�OB	��B	��B	��B	��B	�B	��B	�XB	��B	��B	�*B	�eB	�6B	��B	�CB	��B	��B	�[B	�-B	�aB	��B	��B	�hB	�nB	�B	�B	��B	�$B	��B	��B	�0B	�HB	�OB	��B	��B	�'B	B	��B	��B	�9B	�mB	ŢB	�tB	�KB	�B	�B	�^B	��B	��B	͟B	��B	�NB	ϫB	��B	�&B	ҽB	��B	�&B	ӏB	�gB	��B	�2B	רB	�EB	�yB	�EB	خB	�KB	��B	��B	��B	��B	�vB	�B	�TB	�B	�B	�ZB	��B	��B	�,B	��B	�2B	��B	�B	��B	�mB	�sB	��B	�DB	�KB	��B	��B	�B	�]B	��B	��B	��B	�B	��B	��B	��B	��B	�8B	��B	��B	��B	��B	��B	�DB	��B	��B	��B	�B	�B	�"B	��B	�(B	�(B	�]B	�.B	��B	��B	��B	��B
 �B
 �B
B
oB
B
AB
uB
�B
AB
uB
B
�B
B
�B
�B
�B
�B
�B
�B
+B
+B
�B
_B
�B
�B
�B
�B
�B
�B
�B
�B
fB
�B
�B
	�B

	B

	B

rB

�B
B
B
~B
B
PB
PB
�B
�B
�B
(B
(B
.B
�B
bB
bB
�B
 B
4B
�B
oB
�B
�B
B
B
{B
FB
FB
B
FB
B
�B
�B
�B
�B
�B
{B
MB
B
MB
�B
�B
�B
�B
B
B
SB
�B
�B
�B
�B
�B
�B
�B
�B
1B
1B
1B
eB
eB
eB
1B
kB
7B
	B
=B
	B
=B
qB
B
B
IB
�B
�B
B
�B
VB
�B
!B
VB
�B
�B
�B
�B
 \B
 'B
 'B
 'B
 �B
 �B
!-B
!bB
!bB
"4B
"�B
#�B
$B
$@B
$�B
&LB
&LB
&�B
&�B
&�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
'�B
(XB
(�B
(�B
)�B
)�B
)�B
*0B
*0B
+�B
+�B
+�B
,qB
,�B
,�B
-B
,�B
-CB
-�B
.B
.B
.}B
.IB
.IB
/OB
.�B
.�B
.�B
/OB
/B
/B
/B
/�B
/�B
0!B
0!B
0UB
0!B
0UB
0�B
0�B
0�B
0�B
0�B
0�B
1[B
1[B
2�B
2-B
2�B
2�B
2�B
2aB
3�B
3�B
3�B
4B
49B
4�B
4�B
5tB
5�B
5tB
6B
5�B
6FB
6zB
6zB
6zB
6zB
6�B
6�B
6�B
6�B
7B
7B
7�B
7�B
8B
7�B
7�B
8RB
8RB
8B
8B
8B
8RB
8RB
8B
8RB
8�B
8�B
9�B
:^B
:*B
:^B
9�B
9�B
:�B
<6B
<6B
<6B
<B
<�B
=B
=B
=qB
=qB
=qB
=�B
=qB
>wB
>wB
>�B
>�B
?HB
?�B
@�B
@�B
@�B
@�B
A�B
B�B
C�B
C�B
C�B
C�B
C�B
C�B
D3B
C�B
D�B
DgB
D�B
E9B
EB
EB
E9B
EmB
F?B
F�B
GB
GEB
GB
G�B
G�B
HB
H�B
H�B
H�B
H�B
I�B
I�B
I�B
I�B
J�B
K)B
K�B
L�B
M6B
M�B
OB
N�B
N�B
O�B
O�B
O�B
P}B
QB
QB
QNB
QB
P�B
QB
P�B
P}B
P}B
QB
Q�B
RTB
RTB
S&B
S[B
S&B
T,B
T�B
T�B
TaB
T�B
TaB
T,B
T�B
U2B
U�B
U�B
U2B
U�B
V9B
VmB
W?B
W
B
W?B
WsB
W�B
XEB
XyB
X�B
X�B
YB
Y�B
Y�B
ZQB
ZQB
[#B
[�B
[�B
[�B
[�B
\]B
]/B
^5B
^�B
_pB
`B
`vB
`�B
`�B
aHB
a�B
bNB
bNB
bNB
c B
b�B
c B
c B
c B
b�B
cTB
c B
c�B
c�B
c�B
c�B
c�B
d&B
dZB
d�B
d�B
d�B
e`B
e,B
e`B
e�B
e�B
ffB
ffB
ffB
gB
f�B
f�B
f2B
f�B
g�B
h
B
h
B
h
B
h>B
h�B
h�B
iyB
iDB
iyB
i�B
i�B
jKB
jB
jB
jB
jKB
jB
jB
j�B
j�B
kB
kQB
kQB
k�B
k�B
k�B
l"B
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
l�B
m)B
m]B
m)B
m]B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
m�B
ncB
ncB
n/B
ncB
oiB
o B
o B
o�B
o�B
o�B
p;B
poB
poB
poB
poB
poB
pB
poB
poB
p�B
p�B
qvB
q�B
q�B
q�B
rB
rB
q�B
rGB
q�B
rB
r�B
rGB
r|B
r�B
rGB
rGB
r|B
rB
q�B
rGB
qvB
qvB
rB
rB
r|B
r|B
r�B
r�B
s�B
sMB
s�B
t�B
uZB
v`B
v�B
v�B
v+B
v�B
v+B
v`B
v+B
v�B
v�B
wfB
v�B
w�B
wfB
w�B
w�B
wfB
xB
w�B
x�B
xB
x�B
x8B
y	B
x�B
y�B
y�B
y�B
zDB
y�B
y�B
y�B
y�B
zxB
zDB
zB
z�B
{JB
{B
{�B
|B
|B
{�B
{JB
{�B
{�B
{�B
|�B
|�B
|PB
}VB
}�B
}�B
~�B
~�B
4B
hB
4B
bB
�B
\B
�B
�B
�B
uB
�B
{B
�B
oB
4B
hB
�B
�B
hB
bB
4B
B
�B
4B
uB
hB
:B
:B
:B
hB
�B
4B
�B
bB
(B
�B
bB
bB
 B
hB
�B
�B
oB
:B
B
�B
hB
:B
�B
:B
hB
bB
.B
.B
bB
 B
�B
�B
4B
hB
�B
�B
:B
�B
B
�B
B
4B
�B
�B
�B
�B
�B
 B
�B
 B
�B
 B
�B
�B
�B
�B
�B
.B
�B
�B
(B
�B
 B
 B
hB
 B
hB
�B
�B
�B
�B
�B
�B
�B
�B
.B
.B
hB
oB
�B
�B
bB
�B
.B
.B
�B
�B
�B
B
B
�B
hB
�B
4B
hB
 B
�B
(B
�B
�B
hB
B
hB
�B
 B
\B
\B
.B
4B
 B
 B
B
�B
4B
hB
 B
�B
 B
4B
�B
�B
bB
\B
�B
�B
�B
.B
�B
hB
hB
�B
�B
�B
:B
hB
oB
�B
�B
uB
�B
B
{B
{B
MB
MB
�B
MB
MB
�B
�B
�B
�B
�B
FB
uB
�B
�B
�B
B
B
@B
uB
{B
{B
FB
B
uB
uB
�B
oB
B
�B
�B
�B
�B
VB
\B
B
B

	B
�B
	lB
AB
{B
MB	��B	�fB	�vB	��B	�B	�B	��B

�B	�B	�-B	�B	��B	�B	��B	��B	��B	��B	�VB	��B	�+B	��B	��B	�uB	��B	��B	�4B	�~B	�=B	��B	��B	��B	��B	�B	�B	�{B	~(B	� B	y�B	}"B	~�B	rB	r|B	o B	p�B	l"B	k�B	�oB	g8B	b�B	c B	h�B	l�B	u�B	�bB	�B	��B	��B	�7B	��B	�	B	�B	�VB	�IB	�!B	��B	�bB	�VB	��B	�~B	��B	��B	�!B	��B	�B	��B	�tB	�$B	��B	�=B	�B	��B	��B	�kB	�OB	�B	��B	�B	��B	��B	�B	�OB	��B	��B	��B	�B	�UB	�OB	��B	��B	� B	��B	�BB	�dB	��B	��B	�LB	��B	��B	��B	��B	��B	�$B	�FB	�B	�B	��B	��B	��B	��B	�^B	��B	�B	��B	�RB	��B	��B	�FB	�LB	�B	��B	��B	��B	�jB	��B	�gB	��B	�B	ΥB	�B	�B	�vB	�B	��B	�}B	�B	ҽB	�NB	�B	ѷB	��B	ӏB	�&B	�,B	��B	֡B	�sB	��B	�yB	�B	ٴB	�gB	��B	�2B	�yB	یB	�NB	�sB	�2B	�B	��B	�}B	רB	�B	ҽB	��B	�TB	�B	�}B	�]B	�B	��B	�B	�2B
�B	��B
SB
�B
B
�B
bB
�B
VB
VB
�B
PB
�B
B
�B
bB
�B
$B
%zB
(�B
(�B
*0B
0�B
0�B
0�B
1'B
>BB
D�B
D3B
E�B
@�B
F?B
FB
F�B
H�B
I�B
K^B
K)B
J�B
LdB
L�B
NpB
L�B
MB
NB
OB
Q�B
RTB
S�B
R�B
UgB
V�B
W?B
X�B
[�B
`�B
c�B
c�B
c�B
d&B
d&B
c�B
c�B
c�B
d�B
dZB
d�B
d�B
dZB
d&B
dZB
d�B
dZB
d�B
d�B
d�B
f�B
f�B
g�B
h>B
g�B
hsB
i�B
j�B
kB
l�B
l�B
m]B
n�B
n/B
n�B
q�B
s�B
r�B
r�B
sB
tTB
v�B
y�B
�4B
��B
��B
��B
�%B
�xB
�PB
��B
�.B
�B
�B
��B
��B
��B
��B
�xB
�~B
��B
��B
�*B
��B
��B
��B
�B
�jB
�BB
�HB
�}B
��B
��B
��B
�aB
�9B
�6B
�zB
�XB
�KB
ǮB
�EB
��B
��B
��B
��B
��B
ΥB
˒B
ϫB
��B
ѷB
��B
�WB
�|B
�`B
�ZB
��B
��B
�ZB
��B
��B
�B
�KB
��B
�B
�B
�B
��B
�B
��B
��B
�B
�B
�B
�;B
�oB
� B
�/B
� B
� B
�;B
��B
�oB
�GB
�TB
��B
�(B
��B
�B
��B
�"B
�cB �BoB{B�B�B_B	B�BB�B$@B �B#:B$�B#�B#nB$B&�B)�B)�B*0B+kB/�B.B.�B/�B.�B.IB.�B.}B/�B0!B0�B4�B3�B5tB49B4B4�B6B7�B7�B8�B7�B7�B7�B8�B9�B:�B<�B=B@B>�B>BB>BB>wB@B9$BAUB@�BA�BA BC�BI�BIBRTBR�BU�BS�BYBXyBa�B`�Bb�Be�Be�Bd�BcTBdZBc�Bc�Bc�BiDBo�BoiBl�Bv`Bw2B{B~�B}�B{�B{JB}"B|�B}VB��B��B�uB�B�AB�B�B��B��B��B�_B�fB�_B�+B�B�DB�B�JB�DB�	B�lB�B�7B��B��B��B�_B��B�@B��B��B�B��B��B�UB�B�FB��B�$B��BĜB��B��B�qB��B�qB��B��B�B�B��B��B��B�UB�[B�'BB��BÖB��B��B�B�?B�RB͟B�RB��B��B�EB�KB�B��BɆB̘BʌB��B˒BʌB��B��BΥB�dB�gB�2B��B��B�yB��B�B�vB�vB�BB��B�#B�B�XB�dB�NB��B�[B�aB�pB��BΥB�vB�B͟B̘B�0B�dB��BٴB�dB�XB�^B�0B��B��B˒BуBBѷB�BɆB��B�jB�XB��B��B��B�tB��B�B�'B��B�B�-B�B��B�'B��B�eB��B�CB�B�B��B��B��B��B�FB��B�OB�tB�qB�VB��B��B�eB��B��B�LB��B��B�_B�MB�uB��B��B��B��B�B�OB��B��B��B��B��B��B�AB~]B}"Bw2B|�BtBncBe�BbBj�BbNBbNBbNBf�Bc�BY�BZQB\]G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                       B

rB

�B
�B
DB

�B

�B

�B

�B
B

�B

�B

�B

�B

�B

rB

�B

�B

rB

=B

#B

rB

�B

�B

�B

�B

�B

XB

rB
)B
�B
�B
"B
�B
6B
�B
9B	��B	�yB	�
B	��B	~�B	p�B	{dB	��B	��B	��B	�B	��B	��B	��B	��B	�B	̈́B	��B	�uB	خB	�aB	�YB	��B
%B
B
�B
+�B
>�B
HKB
UB
_pB
e`B
w�B
��B
ŢB
��B
�B
��B�B&�B./B5�B@ BUMBgBx�B�3B�1B��B��B�DB��B��BĜBɆBөB��B̳BӏB��B�B�B��B��B�IB�vB�nB~(Bg�BY�BT�BOBB=�B9�B!HB
��B
�YB
��B
ՁB
�B
��B
�7B
��B
qvB
U�B
D�B
(�B
:B
qB
�B	�mB	�B	�&B	��B	��B	u�B	ZQB	]�B	6�B	�B	B	�B��B�(B	B�/B�B�B��B�#B�xB�B��B�@B�B�B��B��BƨB�oB�AB�B��B�^B��B�5B��B��B�,B��B�$B��B��B��B�B��B�BB�B��B��B�/B��B�
B��B�B�\B��B��B��B��B�1B��B��B�nB��B��B��B��B�B�GB�	BбB��B�6B	-B	BB	JB		lB	�B	�B�nB�B��B��B�5B��B�|B	�B	 �B	+B	<B	�B	
#B	B	�B	�B	vB	BB	B��B�B�B�B�B�VB��B��B	�B	"B	
XB	�B	�B	�B	 �B�HB	�B	6B	�B	�B	\B	SB	$B	jB	%�B	%�B	%�B	(XB	*B	$�B	"�B	"�B	#�B	!�B	"�B	&fB	-�B	/�B	0�B	0�B	4�B	:�B	BAB	CB	C�B	EmB	FYB	>�B	?}B	NVB	Q4B	U�B	TFB	W�B	XyB	[�B	\�B	^B	`'B	a�B	d�B	f�B	h>B	iDB	j�B	ncB	p;B	q�B	s�B	v�B	yrB	{�B	|�B	}B	}�B	~B	}�B	~�B	��B	�1B	��B	��B	��B	�?B	�+B	�_B	��B	��B	��B	�B	��B	�4B	�hB	��B	��B	�TB	�tB	��B	�B	��B	�0B	�B	��B	�=B	��B	��B	��B	��B	��B	�B	�[B	�3B	��B	��B	�+B	��B	�B	�xB	��B	�B	�B	��B	��B	�B	�.B	�}B	��B	�oB	��B	�aB	�B	�%B	�YB	��B	��B	��B	�B	��B	��B	�6B	̳B	�B	̈́B	οB	ϑB	�(B	��B	�TB	�:B	�oB	�oB	��B	�,B	֡B	�
B	�YB	ٴB	�=B	ܒB	ݘB	�B	�B	�jB	��B	��B	ߊB	��B	�BB	��B	�B	�B	�B	�hB	��B	�B	��B	��B	�>B	�B	�$B	�eB	�B	��B	�/B	��B	� B	�oB	�GB	�aB	�B	�B	�B	��B	��B	�TB	��B	��B	�B	��B	��B	�2B	��B	�RB	�lB	�	B	��B	�>B	��B	��B	�^B	��B	�0B	�JB	�B	�6B	�PB	��B	��B	�PB	��B	��B	��B	�(B	��B	��B	�B	��B	��B
 �B
 B
;B
�B
�B
'B
�B
�B
�B
�B
�B
�B
�B
�B
GB
-B
B
�B
3B
B
B
�B
�B
�B
EB
zB
1B
	7B
�B
	B
	RB

	B

�B

#B

rB

�B
B
DB
^B
0B
�B
�B
6B
jB
<B
pB
<B
<B
pB
BB
(B
�B
�B
B
�B
�B
�B
vB
B
BB
vB
�B
�B
�B
B
B
}B
NB
B
�B
B
�B
B
:B
&B
[B
&B
[B
�B
uB
�B
�B
�B
�B
2B
B
�B
MB
�B
B
�B
�B
B
+B
�B
1B
�B
�B
KB
�B
7B
B
B
QB
�B
7B
7B
kB
�B
=B
WB
qB
�B
�B
~B
jB
�B
B
�B
 vB
 \B
 �B
 �B
!B
!�B
!�B
!�B
!�B
!�B
!�B
!�B
"4B
"hB
"�B
"�B
#TB
#�B
$B
$B
$@B
$�B
%�B
%�B
&LB
'mB
'8B
'B
'B
'8B
'�B
($B
(>B
'�B
(XB
(>B
(�B
)�B
(�B
(�B
)B
)_B
)B
)*B
)yB
)�B
)�B
*0B
*B
*eB
*0B
*B
*�B
*B
*�B
+B
*�B
+6B
+�B
+�B
,�B
,WB
,�B
,�B
-B
,�B
.IB
-�B
-�B
.IB
.cB
.�B
/5B
/�B
/�B
/�B
0;B
0B
0;B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
0�B
1B
1'B
1�B
2GB
1�B
1�B
1�B
2|B
2|B
2B
2B
1�B
2aB
2GB
2-B
2�B
33B
3MB
4B
4nB
4B
49B
3�B
49B
5tB
6`B
6+B
6B
6B
7B
7B
7B
7�B
7�B
7�B
8B
7�B
8�B
8�B
8�B
8�B
9�B
:DB
:�B
:�B
:�B
;0B
<B
="B
=�B
=�B
=�B
=�B
=�B
=�B
>BB
>]B
?.B
>�B
>�B
?cB
>�B
?B
?}B
@ B
@�B
AB
AB
AUB
A;B
A�B
A�B
B�B
B�B
B�B
B�B
B�B
C�B
C�B
C�B
D3B
D�B
E�B
F?B
GEB
GzB
G�B
I7B
H�B
H�B
I�B
I�B
J	B
J�B
K)B
KDB
K�B
KDB
KB
KDB
J�B
J�B
J�B
K�B
K�B
L�B
L�B
MPB
MPB
M�B
NpB
N�B
N�B
OB
OBB
NpB
NpB
O(B
OBB
O�B
O�B
O\B
O�B
P}B
Q B
Q4B
QB
QNB
Q�B
RB
RoB
R�B
R�B
R�B
S�B
S�B
T,B
T{B
T�B
UgB
U�B
VB
U�B
VB
VmB
WsB
XB
X�B
YKB
Y�B
Z7B
Z�B
Z�B
[=B
[�B
\CB
\)B
\xB
]B
\�B
]B
]B
]B
]B
]dB
]/B
]�B
^B
]�B
]�B
^B
^OB
^jB
^�B
^�B
^�B
_;B
_B
_VB
_�B
`B
`\B
`\B
`�B
aHB
aB
`�B
`\B
`�B
a�B
a�B
a�B
a�B
b�B
b�B
b�B
cnB
c B
cnB
c�B
c�B
d@B
dB
c�B
d&B
d�B
d�B
d�B
d�B
d�B
eB
eFB
eFB
e�B
e�B
e�B
fB
e�B
e�B
e�B
e�B
e�B
e�B
ffB
ffB
f�B
f�B
f�B
f�B
g8B
gRB
g8B
gmB
gmB
gmB
gmB
gmB
g�B
g�B
g�B
h
B
h>B
h>B
h>B
h�B
i�B
h�B
iDB
i�B
i�B
jB
j0B
jKB
jKB
jKB
jKB
jeB
i�B
jeB
jeB
j�B
k6B
k�B
k�B
k�B
k�B
lB
k�B
k�B
l"B
k�B
l"B
l�B
l=B
lWB
l�B
l"B
l"B
lWB
l"B
l"B
l�B
k�B
k�B
l"B
k�B
lWB
lWB
l�B
mCB
m�B
m]B
mwB
n�B
o�B
poB
p�B
poB
p!B
p�B
pB
poB
p�B
p�B
p�B
q'B
p�B
q[B
qAB
q�B
q�B
qvB
rB
q�B
r|B
q�B
r|B
r-B
r�B
r�B
tB
s�B
s�B
t9B
s�B
s�B
s�B
s�B
tTB
tB
tB
t�B
utB
uZB
u�B
u�B
vB
u�B
u�B
u�B
u�B
u�B
vzB
v�B
vzB
w2B
w�B
w�B
xlG�O�B

�B
)B

�B

#B
	�B
	B
	RB
	�B

XB
6B
^B
<B
^B
0B

�B
)B
	RB

XB
)B

#B

�B
�B
�B

�B
6B
)B
�B
�B
�B
)B

�B

�B
	�B

#B
�B

XB

#B

#B

�B
)B
�B
�B
0B
�B
�B
�B
)B
�B
�B
�B
)B

#B
	�B
	�B

#B

�B
	�B
�B

�B
)B
�B
�B
�B
^B
�B
�B
�B

�B
	�B
	�B

�B
�B
	�B

�B

XB

�B

�B

�B
�B

XB
^B

�B

�B
	�B
	�B
	�B
�B
	RB

�B

�B
)B

�B
)B
^B

�B
^B
^B

XB
	�B
	RB
	�B
	�B
	�B
)B
0B
^B
^B

#B
	�B
	�B
	�B

XB

�B
�B
�B
�B
^B
)B

�B

�B
)B

�B

XB
�B

XB
�B
)B
�B
)B
^B

�B
	B
	B
	�B

�B

�B

�B
�B
^B

�B
)B

�B

�B

�B

�B

XB

XB

#B
	B
	RB
	RB
	RB
	�B

�B
)B
)B
^B
^B
�B
�B
)B
0B
�B
dB
6B
�B
�B
<B
<B
B
B
BB
B
B
vB
BB
BB
�B
�B
B
6B
�B
�B
�B
�B
�B
B
6B
<B
<B
B
�B
6B
6B
�B
0B
�B
�B
^B
^B
	RB
B
	B
�B
�B
�B
[B
-B	�B	�<B	�B	�B	�'B	�6B	�B	�RB	�tB	�B
�B	��B	��B	��B	�NB	��B	��B	��B	��B	�EB	�B	�WB	��B	��B	�pB	�6B	�vB	�^B	��B	�?B	��B	��B	�OB	~BB	HB	|�B	{�B	}<B	w�B	y�B	s�B	v�B	xRB	k�B	l=B	h�B	j�B	e�B	eFB	{0B	`�B	\xB	\�B	b�B	f�B	o�B	�#B	��B	��B	��B	��B	��B	��B	��B	�B	�
B	��B	�EB	�#B	�B	��B	�?B	��B	��B	��B	��B	��B	��B	�5B	��B	�ZB	��B	��B	��B	��B	�,B	�B	��B	�yB	��B	�yB	��B	��B	�B	��B	��B	��B	��B	�B	�B	��B	��B	��B	�lB	�B	�%B	��B	�vB	�B	�GB	��B	�MB	�oB	��B	��B	�B	��B	��B	�AB	�GB	�|B	�|B	�B	��B	��B	��B	�B	�`B	��B	�B	�B	��B	�WB	��B	ƎB	�+B	��B	�(B	ƎB	��B	�fB	��B	�qB	�7B	��B	ʦB	�=B	��B	�~B	�B	��B	�xB	͹B	�PB	��B	��B	οB	�bB	�4B	өB	�:B	�@B	�uB	�(B	ΊB	��B	�:B	�MB	�B	�4B	��B	��B	żB	�=B	�hB	�IB	�~B	ˬB	�B	��B	�=B	�B	�\B	ބB	�kB	��B	�BB	�RB	�B
�B
�B
pB

#B
	�B
B
B
EB
B
�B
�B
KB

#B
�B
�B
;B
"�B
"NB
#�B
*�B
*KB
*B
*�B
8B
>]B
=�B
?cB
:xB
@ B
?�B
@�B
BAB
C{B
EB
D�B
D�B
F%B
FYB
H1B
F�B
F�B
G�B
H�B
K�B
LB
M�B
L�B
O(B
P�B
Q B
R�B
U�B
Z�B
]~B
]IB
]~B
]�B
]�B
]�B
]~B
]~B
^OB
^B
^OB
^OB
^B
]�B
^B
^OB
^B
^�B
^OB
^�B
`\B
`\B
a�B
a�B
a�B
b4B
cnB
dtB
d�B
fLB
f�B
gB
hXB
g�B
h�B
k�B
m�B
l�B
lqB
l�B
nB
p�B
s�B
y�B
|jB
~wB
�OB
�B
�9B
�B
�fB
��B
��B
��B
��B
��B
�uB
��B
�B
�$B
�7B
�kB
��B
��B
�UB
��B
��B
�B
��B
��B
�$B
��B
�}B
��B
�B
��B
��B
� B
��B
��B
�UB
��B
��B
��B
��B
B
ŢB
�KB
�9B
�RB
͟B
�^B
уB
��B
�#B
�B
�B
ݘB
ݘB
�B
ݘB
�pB
�HB
��B
�B
��B
�,B
�ZB
�B
�TB
�B
�B
�B
�ZB
�,B
��B
�B
�B
��B
�B
�B
��B
�yB
�B
��B
��B
�|B
��B
��B
��B
��B
��B
�	B
�DB
�B
�"B
�]B�BB�BYB�BeB�BkB�B�BIBB�B \B#:B#nB#�B%B)�B'�B(�B)*B(XB'�B(�B($B)_B)�B*�B.IB-CB/B-�B-�B.IB/�B1[B1[B2aB1�B1'B1'B2aB3�B4�B6FB6�B9�B8RB7�B7�B8B9�B2�B:�B:�B;�B:�B=�BC�BB�BK�BL�BOBBM6BS&BR B[�BZQB\�B_�B_;B^5B\�B^B]dB]/B]dBb�BiyBiBffBpBp�Bu%BxlBw2Bu�Bt�Bv�Bv+Bv�BzDB|PB|B}�B{�B|�B~�B�AB��B�;B�B�B�B��B��B��B��B��B��B��B�B��B��B�MB�MB�GB�B��B��B�=B�7B��B��B�SB��B��B��B�qB��B�hB�BB�jB�RB�B�LB�B��B��B��B��B�FB�XB�0B��B�B��B�6B�jB�<B�qB��B��B��B��B�EB��BB��B��B��B��BB�-B�?B�3BŢB�9B�3BÖB�gB�KB�B�B��B�pB̘B� B�}BȴB�B�B��BĜB��B��B��B�B��BΥB�B�B�BȀB�KB�BȴB�EB�?B��B�BŢB�[B�B��B�B��B�mB�qB�9B�)B�6B�^B�)B�-B��B�B��B�-B�aB��B�B�3B��B��B��B��B��B��B�6B��B��B�B��B��B��B��B�VB�:B��B��B��B��B��B�B�B��B��B��B�B��B��B��B�nB��B�B��B�B�4B�:B��B�\B��B��B��B�YB��B��B�qB��B{�BxBv�Bp�Bv�Bm�Bh
B_;B[�BdZB[�B[�B[�B`BB]/BS[BS�BVG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                       <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<o<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<8<e<#�
<#�
<#�
<#�
<#�
<-X�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<<+�<D��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<Y��<���<#�
<#�
<#�
<#�
<{}<|�<���<@��<#�
<#�
<#�
<#�
<F�<��<��R<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�K�<#�
<G�<e��<#�
<#�
<Z��<#�
<#�
<���<�<\<P�<n�<H�v<ؙ<'Ӿ<#�
<#�
<#�
<#�
<#�
<N�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<*\\<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT; PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           NO correction for Conductivity Thermal Mass (CTM) is applied;          PSAL_ADJ = CTM_ADJ_PSAL + dS, dS is calculated from a potential conductivity (ref to 0 dbar) multiplicative adjustment term r.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment; OWC V3.0: r =0.9998(+/-0.0001), vertically averaged dS =-0.006(+/-0.0043)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      NO correction for Conductivity Thermal Mass (CTM) is applied;    OWC V3.0: r =0.9998(+/-0.0001), vertically averaged dS =-0.006(+/-0.0043)                                                                                                                      SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OWC weighted least squares fit is adopted; Map Scales:[x:6/3.0,y:3.0/1.0]; Fit Theta<2.5C; max_breaks=1; Prescribed break at cycle 110;                            PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Significant salinity drift present; OWC weighted least squares fit is adopted; Map Scales:[x:6/3.0,y:3.0/1.0]; Fit Theta<2.5C; max_breaks=1; Prescribed break at cycle 110;                            PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261924182023042619241820230426192418202304261924182023042619241820230426192418SI  SI  ARFMARFM                                                                                                                                                2020090114593520200901145935IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022400401720210224004017QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022400401720210224004017QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2021042714014920210427140149IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619242220230426192422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619242220230426192422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619242220230426192422IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                