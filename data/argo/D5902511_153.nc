CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-11-29T20:47:11Z creation; 2021-04-29T20:27:08Z DMQC;      
references        (http://www.argodatamgt.org/Documentation   comment       	free text      user_manual_version       3.10   Conventions       Argo-3.1 CF-1.6    featureType       trajectoryProfile      comment_on_dac_decoder_version        $Decoded by SIO: Argo SIO SOLOII V2.2   comment_dmqc_operator         bPRIMARY | https://orcid.org/0000-0003-0805-6570 | John Gilson, Scripps Institution of Oceanography        @   	DATA_TYPE                  	long_name         	Data type      conventions       Argo reference table 1     
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
_FillValue        G�O�     `  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \h   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     `  d@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     `  �x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �8   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �p   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     `  �H   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ` �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ` ?�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � _@   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ` g   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �x   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �,   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �4   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �<   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �D   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �L   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �    HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �(Argo profile    3.1 1.2 19500101000000  20201129204711  20210429202817  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_153                 6810_008521_153                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�K3H˒@�K3H˒11  @�K3����@�K3����@2o^ F�e@2o^ F�e�eiDg8�eiDg811  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                              	   	?�  ?�@@  @�G�@�  @��R@�  @��RA  A\)A,��A@��A_\)A\)A�Q�A�Q�A�  A�Q�A�  A�  A�  B Q�B(�B  B(�B   B(  B0(�B8(�B@  BH  BP  BW�
B`  Bg�
Bp  Bx  B�{B�  B��B�  B�  B��B��B�  B�  B�  B��B��B��B��
B�  B�  B�  B�  B�  B�  B��B�  B��
B��
B��B�  B�{B�{B�  B�(�B�  B��C 
=C  C  C��C  C
{C
=C  C
=C
=C  C
=C
=C  C
=C  C   C"{C$
=C%��C'��C*  C,  C.  C0  C2  C4  C6  C8  C9��C<  C>  C@
=CB  CD
=CF  CH  CJ  CK��CM��CO��CR  CT
=CV
=CW��CY�C[��C^  C_�Ca�HCc�Ce��Ch  Cj
=Cl  Cn  Cp
=Cr  Ct
=Cv
=Cw��Cz  C|{C}��C�C�  C�C�  C�C���C���C���C�  C�  C�  C�  C���C���C�C���C�  C�  C�C�
=C�C�  C�  C�C�  C���C�C�  C���C���C���C���C���C���C�  C���C���C���C���C�C�C�  C�  C�
=C�  C�  C�C�  C���C���C�C�  C�  C���C�  C�C�C�C�  C���C�C���C�  C�  C���C���C���C�  C���C�  C���C���C�C�C���C��C��C���C�C���C���C�C�C�  C�C�  C�  C�  C�  C���C�  C���C�C���C���C�C�C���C�  C�
=C�C�  C���C���C���C�  C���C���C���C���C�  C�C�
=C�  C�  C�C�  C���C���C�  C���C���C�  C�C�C�C�  C���D �D ��D�D� D�D��D  D}qD�qD� D  D}qD  D�D�D}qD  D��D	  D	� D
  D
��D�D}qD��Dz�D�qD}qD�qD��D  Dz�D  D�D�D� D  D}qD�qD}qD  D� D�qD� D  D� D�D� D�qD��D�D}qD  D� D�D��D�D�D�D� D  D� D  D��D D ��D!�D!� D"  D"� D#�D#}qD#�qD$��D$�qD%}qD&�D&�D'�D'� D(  D(�D)�D)� D*  D*}qD*��D+}qD+�qD,}qD,�qD-��D.�D.z�D/  D/� D/�qD0��D1D1��D2�D2}qD3  D3��D4�D4� D5�D5�D6�D6��D6�qD7� D8  D8}qD8�qD9��D:  D:z�D:�qD;��D<  D<� D=  D=}qD=�qD>��D?D?� D@  D@}qD@��DA� DB  DB}qDC�DC� DC�qDD}qDD�qDE� DF�DF�DG  DG}qDH  DH�DI�DI� DJ  DJ}qDK  DK��DK�qDL� DM�DM� DN  DN��DO�DO�DP  DP� DP�qDQz�DQ�qDR� DS�DS}qDT�DT� DT��DU� DVDV� DV�qDW}qDW�qDX� DY  DYz�DY�qDZ��D[�D[��D\�D\� D]�D]��D^  D^� D_  D_��D`D`}qD`�qDa��Db  Db}qDb�qDc}qDd  Dd� Dd�qDe}qDf�Df��Dg  Dg}qDg�qDh��Di  Di� Di�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm��Dn}qDo  Do� Dp  Dp��Dq�Dq}qDq��Dr}qDs�Ds� Dt  Dt� Du�Du� Du�qDv� Dw�Dw� Dx�Dx��Dx�qDyz�Dy�qDz� D{  D{� D|  D|}qD}  D}��D~D~�D�D}qD�qD�@ D�� D���D���D�=qD�� D�D�HD�@ D�~�D��HD�HD�>�D�~�D���D�  D�AHD�� D��qD���D�>�D�~�D�� D�  D�AHD��HD��HD��D�AHD�� D�� D�HD�AHD�� D�D�HD�@ D���D��HD��qD�>�D�� D�� D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�>�D��HD��HD�  D�>�D�� D�� D���D�@ D�� D�� D�  D�AHD��HD��HD�  D�AHD�� D���D���D�>�D�� D��HD�  D�>�D�� D�D�  D�@ D��HD�� D���D�@ D��HD�D�HD�B�D���D��HD�  D�>�D�� D�D�HD�>�D�~�D���D�  D�AHD�� D���D���D�>�D�}qD���D�  D�>�D��HD�D�HD�AHD���D��HD���D�@ D���D��HD�  D�@ D��HD��HD�  D�@ D�� D��HD�HD�B�D���D�� D���D�=qD�}qD���D�  D�>�D�~�D��HD��D�@ D�~�D���D�HD�@ D�~�D���D���D�>�D�~�D�� D�HD�AHD���D�D���D�=qD��HD�D�HD�@ D�~�D��HD�HD�AHD��HD��HD�HD�@ D�~�D�� D���D�@ D��HD���D���D�>�D�}qD�� D�  D�=qD�~�D��HD��D�@ D�� D�� D��qD�>�D��HD��HD���D�@ D��HD�� D��qD�AHD��HD�� D�  D�>�D�}qD���D�HD�>�D�|)D��qD���D�>�D�� D��HD�  D�>�D�~�D��qD���D�=qD�~�D�D��D�@ D�� D�D��D�AHD��HD�� D���D�@ D��HD�� D���D�@ D��HD�D��D�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�>�D�}qD�� D�HD�AHD��HD��HD�HD�B�D���D��HD�  D�>�D�� D��qD�  D�C�D�D��HD�HD�>�D�}qDþ�D�HD�@ D�~�DĽqD���D�AHDŀ D�� D���D�@ DƁHD�� D�  D�>�D�~�DǾ�D��qD�@ DȁHD�� D�HD�@ D�}qDɾ�D�  D�AHDʂ�D��HD�HD�B�Dˀ D˾�D�  D�B�D́HD��HD��D�@ D̀ D�� D�  D�AHD΀ D�� D�  D�@ DρHD�� D���D�>�DЀ D��HD�HD�@ DсHD�D���D�>�DҁHD�� D���D�@ DӁHD�D�HD�>�D�~�D�� D��qD�>�D�}qDվ�D���D�>�D�~�D־�D�  D�AHD׀ D׾�D�HD�@ D؀ D��HD�  D�AHDفHD��HD�HD�@ Dڀ D�� D���D�>�Dۀ D��HD��D�@ D܀ D�� D�  D�AHD݁HD��HD�  D�AHDށHD��HD�HD�@ D߀ D�� D�  D�AHD���D�� D���D�>�D�~�DᾸD���D�@ D� D�� D��qD�>�D�~�D�qD���D�>�D�~�D��HD��D�AHD� D�� D�  D�@ D�~�D�� D��qD�@ D�HD�� D�HD�@ D�~�D辸D���D�>�D�~�D�� D���D�=qD�~�D꾸D�  D�B�D�HD뾸D���D�@ D�}qD쾸D���D�@ D� D��HD��D�AHDD��HD���D�@ DD�� D���D�@ D�~�D��HD�HD�>�D�~�D�D���D�@ D�HD��HD�HD�AHD�D�� D�  D�>�D�}qD��qD���D�@ D�� D���D�HD�@ D�� D��HD�  D�>�D�~�D��qD���D�@ D�~�D���D�HD�@ D�n?8Q�?�  ?���?\?Ǯ?��@   @��@�R@(��@0��@E�@Q�@W
=@h��@u@�  @���@��@���@���@�G�@��\@�=q@���@�
=@�(�@��
@���@�33@�
=@�  @�@���@�
=@��RA�AA�Ap�AG�AA��A�A ��A#33A&ffA+�A.�RA1G�A5A9��A<(�AAG�AC33AG
=AL��AO\)AR�\AW�AZ�HA^{Ac33AeAi��An�RAr�\Au�Ay��A}p�A�Q�A��\A���A�{A���A��HA�(�A��RA���A��\A���A�
=A���A�33A�A��RA�G�A��
A�A�\)A��A�z�A�A�  A��A��
A�ffA���A�=qA���A�\)A���A�33A�A�  A�G�A�(�A�ffAϮA�=qA���A�{Aأ�A�33A�z�A�\)A��A�33A�{A�Q�A�=qA��A�
=A��A�A�A��A��\A���A��RB ��B�B�\B�
BG�B{B
=Bz�B	B
�\B�B��B�B
=Bz�B�BffB�
B��BB\)B(�B�B�\B33Bz�BB�\B�B ��B!�B"�RB$(�B%G�B&{B'33B(z�B(��B*ffB+�B,(�B-G�B.�RB/\)B0Q�B1B2�\B333B4z�B5��B6{B7\)B8(�B8��B:{B;
=B;�B=�B=B>�\B?�
B@��BA��BB�\BC�
BDz�BE��BF�RBG�BHz�BIBJ�\BK\)BL��BM��BN=qBO�BP��BQG�BRffBS�BTQ�BU��BV�\BW33BXQ�BY��BZffB[
=B\Q�B]G�B^{B_33B`(�B`��BaBc33Bc�
Bd��BeBf�RBg\)Bh��BiBjffBk�Blz�Bm�BnffBo\)Bp  Bp��Br=qBs
=Bs�
BuG�Bu�Bv�HBx(�Bx��ByB{
=B|(�B|��B}�B33B�
B�ffB���B�p�B��B�Q�B���B��B��B�=qB��RB�
=B�B�=qB��\B�G�B��
B�=qB��RB�p�B��B�=qB�
=B��B�  B�z�B�33B���B�(�B��HB�33B��
B�z�B��HB���B�(�B��\B�
=B�B�=qB���B�33B��B�z�B��HB�p�B�{B���B�
=B�B�Q�B��RB�G�B��B�z�B��HB�G�B�  B���B���B�\)B�  B���B���B�p�B�{B���B��B��B�(�B���B�
=B��B�{B�z�B���B��B�B�=qB���B��B�p�B�{B�ffB���B�G�B���B��B�z�B��HB��B��B�(�B�z�B���B�\)B��B�  B��\B���B�33B��B�(�B�ffB��RB�\)B���B��B�Q�B��HB�\)B���B�  B���B�
=B�\)B�B�ffB��RB��B��B�{B�ffB�
=B��B�  B�Q�B��HB�p�B�B�{B���B�33B��B��B��\B�
=B�\)B��
B�ffB¸RB��BîB�=qBď\B���B�\)B�  B�z�B���B��BǅB�(�Bȣ�B�
=B�G�B��
B�Q�BʸRB�
=BˮB�=qB�z�B��HB�p�B�  B�ffBθRB��BϮB�Q�BУ�B�
=Bљ�B�=qBң�B���BӅB�{Bԏ\B��HB�G�B��B�ffB֣�B��B�B�{B�ffB���Bٙ�B�  B�Q�B���B�G�B��
B�(�B܏\B��B�B�(�Bޏ\B���B߮B�(�B�z�B���B�B�=qB���B��B�B�ffB���B�G�B�B�Q�B���B�p�B��
B�Q�B�
=B�B��B�ffB�
=B�B��B�Q�B��HB�B�  B�Q�B�RB�G�B��
B�ffB���B��B�B�=qB���B�33B�B�  B�\B�33B���B�  B�z�B�
=B��B�{B��\B��B�B�=qB���B�33B��
B�z�B��HB�\)B��B��\B�33B��C   C Q�C ��C �HC{CQ�C�C��C(�CffC��C��CG�C�\C��C
=CG�C�\C�C33CffC��C��C=qC�\C��C  CG�C�\C�HC33CffC��C�C	G�C	�\C	��C
  C
\)C
�C
�HC{C\)C�C��C33CffC�C��CG�C�\C��C  CQ�C�C��C=qCp�C�RC{C\)C��C�
C{Cp�C�RC�HC=qC�\C�
C
=CG�C�C�
C�C\)C�\C�
C33Cp�C��C��CG�C�C�RC
=C\)C��C�HC�Cz�CC��C33C�\C�
C{CQ�C��C��C=qCz�C�RC
=C\)C��C��C{CffC�RC�C(�Cz�C��C  C=qC�\C�HC (�C ffC ��C!  C!G�C!�C!�RC"
=C"\)C"�C"�HC#�C#p�C#�RC$  C$33C$z�C$�
C%{C%G�C%�\C%�HC&(�C&\)C&��C&�C'33C'z�C'�C'�C(33C(�C(��C)  C)=qC)�\C)�HC*(�C*Q�C*�\C*�
C+(�C+ffC+��C+��C,(�C,z�C,C-  C-=qC-�C-�
C.33C.p�C.�C.��C/Q�C/��C/�HC0�C0ffC0C1
=C1G�C1�\C1��C2{C2p�C2C2��C3=qC3��C3�C4�C4ffC4�RC5
=C5ffC5�C5��C633C6z�C6��C7�C7p�C7C8{C8G�C8�\C8�HC9=qC9p�C9�C:
=C:\)C:��C:�HC;=qC;��C;�
C<{C<ffC<C=
=C=G�C=�\C=�C>G�C>��C>�
C?�C?ffC?�RC@{C@ffC@��C@�CAQ�CA��CA�
CB�CBp�CB��CC�CCz�CC�RCD  CD\)CD�RCE  CE=qCE�\CE�CF33CFp�CF�RCG{CGp�CGCH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                               ?�  ?�@@  @�G�@�  @��R@�  @��RA  A\)A,��A@��A_\)A\)A�Q�A�Q�A�  A�Q�A�  A�  A�  B Q�B(�B  B(�B   B(  B0(�B8(�B@  BH  BP  BW�
B`  Bg�
Bp  Bx  B�{B�  B��B�  B�  B��B��B�  B�  B�  B��B��B��B��
B�  B�  B�  B�  B�  B�  B��B�  B��
B��
B��B�  B�{B�{B�  B�(�B�  B��C 
=C  C  C��C  C
{C
=C  C
=C
=C  C
=C
=C  C
=C  C   C"{C$
=C%��C'��C*  C,  C.  C0  C2  C4  C6  C8  C9��C<  C>  C@
=CB  CD
=CF  CH  CJ  CK��CM��CO��CR  CT
=CV
=CW��CY�C[��C^  C_�Ca�HCc�Ce��Ch  Cj
=Cl  Cn  Cp
=Cr  Ct
=Cv
=Cw��Cz  C|{C}��C�C�  C�C�  C�C���C���C���C�  C�  C�  C�  C���C���C�C���C�  C�  C�C�
=C�C�  C�  C�C�  C���C�C�  C���C���C���C���C���C���C�  C���C���C���C���C�C�C�  C�  C�
=C�  C�  C�C�  C���C���C�C�  C�  C���C�  C�C�C�C�  C���C�C���C�  C�  C���C���C���C�  C���C�  C���C���C�C�C���C��C��C���C�C���C���C�C�C�  C�C�  C�  C�  C�  C���C�  C���C�C���C���C�C�C���C�  C�
=C�C�  C���C���C���C�  C���C���C���C���C�  C�C�
=C�  C�  C�C�  C���C���C�  C���C���C�  C�C�C�C�  C���D �D ��D�D� D�D��D  D}qD�qD� D  D}qD  D�D�D}qD  D��D	  D	� D
  D
��D�D}qD��Dz�D�qD}qD�qD��D  Dz�D  D�D�D� D  D}qD�qD}qD  D� D�qD� D  D� D�D� D�qD��D�D}qD  D� D�D��D�D�D�D� D  D� D  D��D D ��D!�D!� D"  D"� D#�D#}qD#�qD$��D$�qD%}qD&�D&�D'�D'� D(  D(�D)�D)� D*  D*}qD*��D+}qD+�qD,}qD,�qD-��D.�D.z�D/  D/� D/�qD0��D1D1��D2�D2}qD3  D3��D4�D4� D5�D5�D6�D6��D6�qD7� D8  D8}qD8�qD9��D:  D:z�D:�qD;��D<  D<� D=  D=}qD=�qD>��D?D?� D@  D@}qD@��DA� DB  DB}qDC�DC� DC�qDD}qDD�qDE� DF�DF�DG  DG}qDH  DH�DI�DI� DJ  DJ}qDK  DK��DK�qDL� DM�DM� DN  DN��DO�DO�DP  DP� DP�qDQz�DQ�qDR� DS�DS}qDT�DT� DT��DU� DVDV� DV�qDW}qDW�qDX� DY  DYz�DY�qDZ��D[�D[��D\�D\� D]�D]��D^  D^� D_  D_��D`D`}qD`�qDa��Db  Db}qDb�qDc}qDd  Dd� Dd�qDe}qDf�Df��Dg  Dg}qDg�qDh��Di  Di� Di�qDj}qDj�qDk}qDk�qDl}qDl�qDm}qDm��Dn}qDo  Do� Dp  Dp��Dq�Dq}qDq��Dr}qDs�Ds� Dt  Dt� Du�Du� Du�qDv� Dw�Dw� Dx�Dx��Dx�qDyz�Dy�qDz� D{  D{� D|  D|}qD}  D}��D~D~�D�D}qD�qD�@ D�� D���D���D�=qD�� D�D�HD�@ D�~�D��HD�HD�>�D�~�D���D�  D�AHD�� D��qD���D�>�D�~�D�� D�  D�AHD��HD��HD��D�AHD�� D�� D�HD�AHD�� D�D�HD�@ D���D��HD��qD�>�D�� D�� D�  D�AHD�� D�� D�  D�@ D�� D�� D�  D�>�D��HD��HD�  D�>�D�� D�� D���D�@ D�� D�� D�  D�AHD��HD��HD�  D�AHD�� D���D���D�>�D�� D��HD�  D�>�D�� D�D�  D�@ D��HD�� D���D�@ D��HD�D�HD�B�D���D��HD�  D�>�D�� D�D�HD�>�D�~�D���D�  D�AHD�� D���D���D�>�D�}qD���D�  D�>�D��HD�D�HD�AHD���D��HD���D�@ D���D��HD�  D�@ D��HD��HD�  D�@ D�� D��HD�HD�B�D���D�� D���D�=qD�}qD���D�  D�>�D�~�D��HD��D�@ D�~�D���D�HD�@ D�~�D���D���D�>�D�~�D�� D�HD�AHD���D�D���D�=qD��HD�D�HD�@ D�~�D��HD�HD�AHD��HD��HD�HD�@ D�~�D�� D���D�@ D��HD���D���D�>�D�}qD�� D�  D�=qD�~�D��HD��D�@ D�� D�� D��qD�>�D��HD��HD���D�@ D��HD�� D��qD�AHD��HD�� D�  D�>�D�}qD���D�HD�>�D�|)D��qD���D�>�D�� D��HD�  D�>�D�~�D��qD���D�=qD�~�D�D��D�@ D�� D�D��D�AHD��HD�� D���D�@ D��HD�� D���D�@ D��HD�D��D�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�>�D�}qD�� D�HD�AHD��HD��HD�HD�B�D���D��HD�  D�>�D�� D��qD�  D�C�D�D��HD�HD�>�D�}qDþ�D�HD�@ D�~�DĽqD���D�AHDŀ D�� D���D�@ DƁHD�� D�  D�>�D�~�DǾ�D��qD�@ DȁHD�� D�HD�@ D�}qDɾ�D�  D�AHDʂ�D��HD�HD�B�Dˀ D˾�D�  D�B�D́HD��HD��D�@ D̀ D�� D�  D�AHD΀ D�� D�  D�@ DρHD�� D���D�>�DЀ D��HD�HD�@ DсHD�D���D�>�DҁHD�� D���D�@ DӁHD�D�HD�>�D�~�D�� D��qD�>�D�}qDվ�D���D�>�D�~�D־�D�  D�AHD׀ D׾�D�HD�@ D؀ D��HD�  D�AHDفHD��HD�HD�@ Dڀ D�� D���D�>�Dۀ D��HD��D�@ D܀ D�� D�  D�AHD݁HD��HD�  D�AHDށHD��HD�HD�@ D߀ D�� D�  D�AHD���D�� D���D�>�D�~�DᾸD���D�@ D� D�� D��qD�>�D�~�D�qD���D�>�D�~�D��HD��D�AHD� D�� D�  D�@ D�~�D�� D��qD�@ D�HD�� D�HD�@ D�~�D辸D���D�>�D�~�D�� D���D�=qD�~�D꾸D�  D�B�D�HD뾸D���D�@ D�}qD쾸D���D�@ D� D��HD��D�AHDD��HD���D�@ DD�� D���D�@ D�~�D��HD�HD�>�D�~�D�D���D�@ D�HD��HD�HD�AHD�D�� D�  D�>�D�}qD��qD���D�@ D�� D���D�HD�@ D�� D��HD�  D�>�D�~�D��qD���D�@ D�~�D���D�HD�@ G�O�?8Q�?�  ?���?\?Ǯ?��@   @��@�R@(��@0��@E�@Q�@W
=@h��@u@�  @���@��@���@���@�G�@��\@�=q@���@�
=@�(�@��
@���@�33@�
=@�  @�@���@�
=@��RA�AA�Ap�AG�AA��A�A ��A#33A&ffA+�A.�RA1G�A5A9��A<(�AAG�AC33AG
=AL��AO\)AR�\AW�AZ�HA^{Ac33AeAi��An�RAr�\Au�Ay��A}p�A�Q�A��\A���A�{A���A��HA�(�A��RA���A��\A���A�
=A���A�33A�A��RA�G�A��
A�A�\)A��A�z�A�A�  A��A��
A�ffA���A�=qA���A�\)A���A�33A�A�  A�G�A�(�A�ffAϮA�=qA���A�{Aأ�A�33A�z�A�\)A��A�33A�{A�Q�A�=qA��A�
=A��A�A�A��A��\A���A��RB ��B�B�\B�
BG�B{B
=Bz�B	B
�\B�B��B�B
=Bz�B�BffB�
B��BB\)B(�B�B�\B33Bz�BB�\B�B ��B!�B"�RB$(�B%G�B&{B'33B(z�B(��B*ffB+�B,(�B-G�B.�RB/\)B0Q�B1B2�\B333B4z�B5��B6{B7\)B8(�B8��B:{B;
=B;�B=�B=B>�\B?�
B@��BA��BB�\BC�
BDz�BE��BF�RBG�BHz�BIBJ�\BK\)BL��BM��BN=qBO�BP��BQG�BRffBS�BTQ�BU��BV�\BW33BXQ�BY��BZffB[
=B\Q�B]G�B^{B_33B`(�B`��BaBc33Bc�
Bd��BeBf�RBg\)Bh��BiBjffBk�Blz�Bm�BnffBo\)Bp  Bp��Br=qBs
=Bs�
BuG�Bu�Bv�HBx(�Bx��ByB{
=B|(�B|��B}�B33B�
B�ffB���B�p�B��B�Q�B���B��B��B�=qB��RB�
=B�B�=qB��\B�G�B��
B�=qB��RB�p�B��B�=qB�
=B��B�  B�z�B�33B���B�(�B��HB�33B��
B�z�B��HB���B�(�B��\B�
=B�B�=qB���B�33B��B�z�B��HB�p�B�{B���B�
=B�B�Q�B��RB�G�B��B�z�B��HB�G�B�  B���B���B�\)B�  B���B���B�p�B�{B���B��B��B�(�B���B�
=B��B�{B�z�B���B��B�B�=qB���B��B�p�B�{B�ffB���B�G�B���B��B�z�B��HB��B��B�(�B�z�B���B�\)B��B�  B��\B���B�33B��B�(�B�ffB��RB�\)B���B��B�Q�B��HB�\)B���B�  B���B�
=B�\)B�B�ffB��RB��B��B�{B�ffB�
=B��B�  B�Q�B��HB�p�B�B�{B���B�33B��B��B��\B�
=B�\)B��
B�ffB¸RB��BîB�=qBď\B���B�\)B�  B�z�B���B��BǅB�(�Bȣ�B�
=B�G�B��
B�Q�BʸRB�
=BˮB�=qB�z�B��HB�p�B�  B�ffBθRB��BϮB�Q�BУ�B�
=Bљ�B�=qBң�B���BӅB�{Bԏ\B��HB�G�B��B�ffB֣�B��B�B�{B�ffB���Bٙ�B�  B�Q�B���B�G�B��
B�(�B܏\B��B�B�(�Bޏ\B���B߮B�(�B�z�B���B�B�=qB���B��B�B�ffB���B�G�B�B�Q�B���B�p�B��
B�Q�B�
=B�B��B�ffB�
=B�B��B�Q�B��HB�B�  B�Q�B�RB�G�B��
B�ffB���B��B�B�=qB���B�33B�B�  B�\B�33B���B�  B�z�B�
=B��B�{B��\B��B�B�=qB���B�33B��
B�z�B��HB�\)B��B��\B�33B��C   C Q�C ��C �HC{CQ�C�C��C(�CffC��C��CG�C�\C��C
=CG�C�\C�C33CffC��C��C=qC�\C��C  CG�C�\C�HC33CffC��C�C	G�C	�\C	��C
  C
\)C
�C
�HC{C\)C�C��C33CffC�C��CG�C�\C��C  CQ�C�C��C=qCp�C�RC{C\)C��C�
C{Cp�C�RC�HC=qC�\C�
C
=CG�C�C�
C�C\)C�\C�
C33Cp�C��C��CG�C�C�RC
=C\)C��C�HC�Cz�CC��C33C�\C�
C{CQ�C��C��C=qCz�C�RC
=C\)C��C��C{CffC�RC�C(�Cz�C��C  C=qC�\C�HC (�C ffC ��C!  C!G�C!�C!�RC"
=C"\)C"�C"�HC#�C#p�C#�RC$  C$33C$z�C$�
C%{C%G�C%�\C%�HC&(�C&\)C&��C&�C'33C'z�C'�C'�C(33C(�C(��C)  C)=qC)�\C)�HC*(�C*Q�C*�\C*�
C+(�C+ffC+��C+��C,(�C,z�C,C-  C-=qC-�C-�
C.33C.p�C.�C.��C/Q�C/��C/�HC0�C0ffC0C1
=C1G�C1�\C1��C2{C2p�C2C2��C3=qC3��C3�C4�C4ffC4�RC5
=C5ffC5�C5��C633C6z�C6��C7�C7p�C7C8{C8G�C8�\C8�HC9=qC9p�C9�C:
=C:\)C:��C:�HC;=qC;��C;�
C<{C<ffC<C=
=C=G�C=�\C=�C>G�C>��C>�
C?�C?ffC?�RC@{C@ffC@��C@�CAQ�CA��CA�
CB�CBp�CB��CC�CCz�CC�RCD  CD\)CD�RCE  CE=qCE�\CE�CF33CFp�CF�RCG{CGp�CGCH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                               @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��#A��mA��;A��/A��TA��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�A�  A�  A�A�A�A�A���A���A�A�A���A�  A�A�1A�1A�JA�JA�VA�1A���Aں^A�A�
=Aէ�A���A�~�A�&�AӃA� �AэPA��A���A���A�ffA�ƨA��A�A�n�A�ĜA�z�A�{A�%A�+A�hsA��yA�VA�A���A�JA�p�A���A�E�A���A�=qA�1A�|�A��7A�VA���A��A�1'A�`BA�1'A�r�A�%A�VA�
=A�p�A��-A���A��A�5?A�l�A��-A�l�A���A��TA��RA��A��A��A��;A���A��A�G�A�$�A���A�I�A�C�A��A���A�A�;dA�M�A��FA��yA�?}A�1'A���A�v�A��yA�x�A��RA;dA~��A|�Ax�HAt5?Ar1'Ap �AoK�AnA�Ak+Ai�FAg�-Ae�Ab�/AaVA`I�A^r�A\=qA[�TAZĜAW�PAU�ASAP{ANA�AN$�AN�AM�-ALĜAL-AG�FABE�A?��A>�+A=�FA=33A<��A<�A<�A:�!A81'A7C�A4�9A0M�A-��A+\)A*5?A*  A)33A(n�A(I�A&�/A#x�A!�PA -A��AVA(�A
=A=qA��A�A�A33Ap�AE�A��A�A1A�yAJA|�A�HA$�A��A&�A��A(�A  A  A	VA��AA�A+A��AbNAƨAA�9AA�A��AI�A�A �9A 5?@��T@���@���@���@�hs@��D@�1@�ƨ@��@�J@�bN@�~�@��T@�/@�A�@��m@�@�\)@�dZ@�|�@�1@��D@�@���@���@�n�@�^@�1@���@��@���@�@��T@�5?@�/@�;d@��@�
=@���@ݩ�@�G�@�Ĝ@�ƨ@�&�@؋D@ؓu@�Z@�bN@�Z@�Z@�1'@�Z@�7L@�v�@ڗ�@���@�^5@�|�@ѩ�@��`@ЋD@���@�I�@� �@�"�@ΰ!@��#@��@�1@�bN@��@·+@͑h@�x�@�`B@�V@̣�@�j@� �@�  @���@�|�@�;d@�"�@�
=@��@�ff@��@�1'@ǥ�@��@Ɵ�@�^5@�M�@�^5@�E�@���@�@őh@�?}@���@��m@�hs@��/@�r�@�z�@�I�@�  @��@�\)@��!@�^5@��T@�hs@�O�@�%@��`@���@��@��@��@�&�@�&�@��@�V@���@��9@�b@��@��@���@�p�@�%@�Ĝ@���@�1@�ƨ@���@�|�@�dZ@�K�@�"�@��R@�~�@��+@�=q@�7L@��j@��@��u@�A�@� �@��@��@��P@��P@�t�@�;d@�@��\@�M�@�@��/@��@�Z@� �@��R@���@��@�@���@���@���@���@��h@��h@��7@�?}@��`@�1@��P@�;d@�
=@��@�ȴ@��+@�=q@��-@�G�@��@��@��`@��`@���@��@�1@�K�@��y@�ȴ@�^5@�-@��T@���@���@���@��h@���@�ƨ@�dZ@�S�@��@��+@�M�@�-@�-@�$�@�{@�@��@��@���@�hs@�V@�bN@� �@�  @��m@��
@�ƨ@�l�@�o@���@��H@�ȴ@��R@��!@��!@���@�E�@�G�@���@�Ĝ@��9@�Q�@��F@�|�@�;d@���@�V@��#@��^@���@��7@�G�@��@��@�&�@�?}@�&�@�V@��`@��j@���@�I�@�S�@���@�ȴ@��\@�v�@�M�@�E�@�@�G�@��@���@�j@�9X@��m@���@���@�C�@��@���@���@��!@��\@��@�O�@�G�@�O�@�O�@��@��;@�dZ@�"�@���@��H@��@��@���@��!@��+@�n�@�E�@�=q@��@���@���@�Ĝ@��9@��D@�bN@�9X@���@�\)@�ȴ@�v�@�5?@��@���@�x�@�hs@�X@��@��/@��@�9X@�(�@��@��w@�t�@�o@���@��@��y@���@�5?@���@�&�@��@���@��`@���@�j@��@�  @��@�|�@�K�@�C�@�+@��y@��@���@�~�@�J@���@�?}@��@�V@��@��9@��@�r�@�bN@�1@�P@\)@\)@~�y@~��@~V@}?}@|I�@{�m@{�F@{�@{t�@{33@{o@z��@zn�@z-@y��@y�@xbN@xb@w�;@wl�@w�@vȴ@v@u/@t�@tz�@t1@s��@s"�@s@r��@r-@q��@q�@p�@p �@o|�@n�y@n5?@n{@m��@m�@mO�@mV@l(�@k��@kS�@ko@j��@j�@i�@i�#@i��@i�7@i&�@hr�@g|�@g
=@f��@f�+@fV@e�@eV@d�D@dz�@c�m@b�@b��@b~�@bM�@bJ@a�@aG�@`��@`�u@`�@`�@`r�@`r�@`A�@_�@^v�@^$�@]�T@]�T@]�T@]�-@\��@\�D@[�m@[S�@[C�@[C�@["�@[@Z�!@ZJ@Y�^@Y�7@Yx�@Yx�@Yx�@Yx�@Yhs@X�9@X  @W
=@V5?@V@U�-@Up�@U?}@U/@UV@T��@St�@R��@R=q@Q��@Qhs@QG�@Q�@P�@P �@P �@Pb@Pb@O�@O�;@O�;@O�;@O��@O;d@O+@N��@N�y@Nȴ@N��@N@M��@Mp�@MV@LI�@L9X@K�
@KC�@Ko@J�H@J�@I��@I��@I%@H��@HQ�@G��@G�@G�P@G;d@F��@E��@E?}@D�@D�j@D�@C��@B�\@B�@A�#@A��@AX@A%@@�9@?��@?+@>�R@=��@=`B@=V@<��@<��@<j@<1@;��@;t�@:�H@:~�@:n�@:^5@:-@9�#@9G�@8�`@81'@7|�@7\)@7K�@7;d@7�@6ȴ@6�R@6�R@6��@6�+@6v�@6v�@6ff@65?@6@5�-@5?}@4�/@4�@3�m@3�
@3�F@3dZ@333@3o@2��@2�!@2�!@2��@2M�@1�#@1��@1��@1�7@1hs@17L@0��@0b@/�;@/��@/��@/�@/��@/�P@/�P@/�P@/�P@/l�@/;d@/
=@.�R@.ff@.@-�T@-�@,��@,�/@,�j@,z�@,Z@,Z@,�@,�@,(�@,1@+�
@+�F@+t�@*�H@*��@*=q@)X@)�@)%@)%@(��@(��@(Q�@'�w@'\)@&�y@&�+@&E�@&E�@%�@%@%��@%`B@$��@$�@$�@$�D@$Z@$9X@$�@#��@#�
@#��@#33@"�@"~�@"n�@"M�@"�@!�@!�7@!x�@!�@ ��@ b@   @|�@�@��@�+@V@{@�@@�@�/@��@�@j@(�@�m@�F@t�@"�@o@o@@��@��@n�@^5@�@��@&�@�9@ �@�@�w@�P@|�@�R@@@@�@�-@`B@`B@p�@p�@p�@p�@`B@O�@�@�D@j@I�@9X@�m@dZ@�@��@��@��@^5@��@X@G�@7L@&�@��@��@Ĝ@��@�@A�@b@��@|�@l�@\)@K�@
=@�@�R@��@ff@{@�@�T@��@p�@/@�@V@V@��@�@��@�j@�D@j@I�@(�@�@1@�@�@1@�mA���A��A��mA��A��A��A��;A��/A��yA��HA��HA��mA��;A��#A��#A��;A��#A��HA��#A��
A��`A��;A��;A��A��A��A��A��A��A��A��A��A��A��A���A���A��A��A���A��A��A���A��A��A���A��A��A���A���A��A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A��A���A���A���A��A���A���A��A���A���A��A���A���A��A���A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A�A���A���A�  A���A���A�  A���A���A�  A���A���A�  A�  A���A�  A�  A���A�  A�A���A�A�%A�  A���A�A�A���A�A�A�  A�A�  A���A�  A���A���A�  A���A���A�A�  A�  A�A�%A�A�A�%A�A�A�A�  A�  A�%A�A�A�%A�%A�  A�A�%A�A�A�%A�  A�A�%A�%A�A�%A�A�  A�A�A���A���A�%A�A���A�A�  A���A�A�  A���A���A�  A���A�  A�A���A���A�  A���A���A�  A���A�A�1A�%A�A�A�1A�A�A�A�  A�  A�A�A�  A�  A�  A���A���A���A�  A���A�A�A���A�  A�  A���A�  A�1A�A�  A�%A�A�  A�  A�1A�%A�A�1A�%A�1A�
=A�%A�1A�JA�1A�1A�JA�
=A�%A�%A�JA�VA�
=A�JA�VA�JA�1A�VA�bA�
=A�
=A�bA�bA�JA�
=A�JA�bA�VA�
=A�JA�bA�JA�
=A�bA�bA�VA�JA�bA�VA�A���A��`A��TA��;A�ƨA�ȴA���A���AھwAھwA���A�AڼjAں^AھwAڶFAں^AڼjAڶFAڶFAڼjAڼjAڰ!AڶFA���A���A���A���A�ĜA�ȴA���A�ȴA���A���Aڝ�A�hsA�1A���A��A���A� �A�A�A��Aղ-A՝�AՋDA�M�A�{A���A��A��A��`A���A���A�ƨAԸRAԬAԡ�Aԛ�Aԙ�Aԏ\AԅA�|�A�x�A�x�A�dZA�^5A�^5A�G�A�33A�(�A�+A��A�1A���A��A��/A���A���AӺ^Aӡ�A�r�A�7LA�
=A���AғuA�`BA�+A��A�A���A��A��`A��/A���A�ĜAѺ^AѮAљ�A�l�A�E�A�+A�A��A��TA��/A��#A���A�ƨA�A�ƨA�ĜAмjAмjA���A���A���A��A��TA��`A��HA��HA��;A��;A��;A��/A�ƨAЛ�A�v�A�
=Aϟ�A�dZA�G�A��A��A���AΝ�A�ffA��AͮA͉7A�z�A�I�A�1'A�-A�-A�$�A��A�oA�1A���A��A��HA���A�ĜA̸RA̲-A̲-A̴9A̴9A̮A̩�A̰!Ḁ�A�|�A�33A��A�oA��A��#A���A�ƨA˸RA˼jA˾wA˴9A˛�AˑhA˲-A˾wAˍPA�l�A�E�A��A�t�A��TAɶFA�ZAȴ9A�S�A��HA�bNA�?}A�=qA�&�A�%A�  A��/AƲ-AƍPA�n�A�;dA�{A��A�oA���A��HA�A�C�A�ȴA�M�Að!A�^5A��A�x�A�?}A�A��;A��RA��A�v�A�\)A��yA��A�jA�A�A��A���A�r�A�\)A�G�A�7LA��A��yA��!A��PA�+A��A���A��RA���A�ffA�?}A�1A�A�p�A�
=A���A��-A���A���A���A��DA��+A��A�v�A�VA�(�A��`A�ĜA��-A���A��A�x�A�x�A�r�A�jA�dZA�ZA�C�A�33A�"�A�bA�  A��mA���A��FA��`A���A�z�A�hsA�VA�I�A�A�A�5?A�-A�$�A��A�{A�oA�JA�A���A���A���A��A��;A�ȴA��A�?}A�"�A���A���A���A��uA��A�z�A�r�A�ffA�\)A�VA�O�A�=qA�JA���A��A���A���A���A���A���A���A���A���A��\A��PA��DA��+A�|�A�v�A�t�A�ffA�XA�O�A�E�A�;dA�bA���A���A��A��#A�r�A�?}A�/A� �A���A���A��A�dZA�O�A�7LA�bA��HA���A��PA�M�A���A���A�1'A��!A�jA� �A�bNA�JA���A��;A��jA���A���A��A�dZA�G�A�C�A�C�A�C�A�9XA�$�A��A�JA�A���A��A���A��A�%A�A�p�A���A�A�A�JA��A��TA���A���A�z�A�^5A�I�A�5?A���A��9A���A�~�A�~�A��A�`BA�9XA� �A���A��9A��hA�~�A�t�A�\)A��A���A��HA�ĜA��hA�|�A�n�A�\)A�S�A�9XA��A�JA��HA��RA��A�`BA�33A�%A��;A���A�^5A�bA���A�A�A���A���A��A�XA�?}A�$�A�JA���A��A��TA�ȴA��FA��\A�`BA� �A�ĜG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                               A��#A��mA��;A��/A��TA��A��A��A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�  A�A�  A�  A�A�A�A�A���A���A�A�A���A�  A�A�1A�1A�JA�JA�VA�1A���Aں^A�A�
=Aէ�A���A�~�A�&�AӃA� �AэPA��A���A���A�ffA�ƨA��A�A�n�A�ĜA�z�A�{A�%A�+A�hsA��yA�VA�A���A�JA�p�A���A�E�A���A�=qA�1A�|�A��7A�VA���A��A�1'A�`BA�1'A�r�A�%A�VA�
=A�p�A��-A���A��A�5?A�l�A��-A�l�A���A��TA��RA��A��A��A��;A���A��A�G�A�$�A���A�I�A�C�A��A���A�A�;dA�M�A��FA��yA�?}A�1'A���A�v�A��yA�x�A��RA;dA~��A|�Ax�HAt5?Ar1'Ap �AoK�AnA�Ak+Ai�FAg�-Ae�Ab�/AaVA`I�A^r�A\=qA[�TAZĜAW�PAU�ASAP{ANA�AN$�AN�AM�-ALĜAL-AG�FABE�A?��A>�+A=�FA=33A<��A<�A<�A:�!A81'A7C�A4�9A0M�A-��A+\)A*5?A*  A)33A(n�A(I�A&�/A#x�A!�PA -A��AVA(�A
=A=qA��A�A�A33Ap�AE�A��A�A1A�yAJA|�A�HA$�A��A&�A��A(�A  A  A	VA��AA�A+A��AbNAƨAA�9AA�A��AI�A�A �9A 5?@��T@���@���@���@�hs@��D@�1@�ƨ@��@�J@�bN@�~�@��T@�/@�A�@��m@�@�\)@�dZ@�|�@�1@��D@�@���@���@�n�@�^@�1@���@��@���@�@��T@�5?@�/@�;d@��@�
=@���@ݩ�@�G�@�Ĝ@�ƨ@�&�@؋D@ؓu@�Z@�bN@�Z@�Z@�1'@�Z@�7L@�v�@ڗ�@���@�^5@�|�@ѩ�@��`@ЋD@���@�I�@� �@�"�@ΰ!@��#@��@�1@�bN@��@·+@͑h@�x�@�`B@�V@̣�@�j@� �@�  @���@�|�@�;d@�"�@�
=@��@�ff@��@�1'@ǥ�@��@Ɵ�@�^5@�M�@�^5@�E�@���@�@őh@�?}@���@��m@�hs@��/@�r�@�z�@�I�@�  @��@�\)@��!@�^5@��T@�hs@�O�@�%@��`@���@��@��@��@�&�@�&�@��@�V@���@��9@�b@��@��@���@�p�@�%@�Ĝ@���@�1@�ƨ@���@�|�@�dZ@�K�@�"�@��R@�~�@��+@�=q@�7L@��j@��@��u@�A�@� �@��@��@��P@��P@�t�@�;d@�@��\@�M�@�@��/@��@�Z@� �@��R@���@��@�@���@���@���@���@��h@��h@��7@�?}@��`@�1@��P@�;d@�
=@��@�ȴ@��+@�=q@��-@�G�@��@��@��`@��`@���@��@�1@�K�@��y@�ȴ@�^5@�-@��T@���@���@���@��h@���@�ƨ@�dZ@�S�@��@��+@�M�@�-@�-@�$�@�{@�@��@��@���@�hs@�V@�bN@� �@�  @��m@��
@�ƨ@�l�@�o@���@��H@�ȴ@��R@��!@��!@���@�E�@�G�@���@�Ĝ@��9@�Q�@��F@�|�@�;d@���@�V@��#@��^@���@��7@�G�@��@��@�&�@�?}@�&�@�V@��`@��j@���@�I�@�S�@���@�ȴ@��\@�v�@�M�@�E�@�@�G�@��@���@�j@�9X@��m@���@���@�C�@��@���@���@��!@��\@��@�O�@�G�@�O�@�O�@��@��;@�dZ@�"�@���@��H@��@��@���@��!@��+@�n�@�E�@�=q@��@���@���@�Ĝ@��9@��D@�bN@�9X@���@�\)@�ȴ@�v�@�5?@��@���@�x�@�hs@�X@��@��/@��@�9X@�(�@��@��w@�t�@�o@���@��@��y@���@�5?@���@�&�@��@���@��`@���@�j@��@�  @��@�|�@�K�@�C�@�+@��y@��@���@�~�@�J@���@�?}@��@�V@��@��9@��@�r�@�bN@�1@�P@\)@\)@~�y@~��@~V@}?}@|I�@{�m@{�F@{�@{t�@{33@{o@z��@zn�@z-@y��@y�@xbN@xb@w�;@wl�@w�@vȴ@v@u/@t�@tz�@t1@s��@s"�@s@r��@r-@q��@q�@p�@p �@o|�@n�y@n5?@n{@m��@m�@mO�@mV@l(�@k��@kS�@ko@j��@j�@i�@i�#@i��@i�7@i&�@hr�@g|�@g
=@f��@f�+@fV@e�@eV@d�D@dz�@c�m@b�@b��@b~�@bM�@bJ@a�@aG�@`��@`�u@`�@`�@`r�@`r�@`A�@_�@^v�@^$�@]�T@]�T@]�T@]�-@\��@\�D@[�m@[S�@[C�@[C�@["�@[@Z�!@ZJ@Y�^@Y�7@Yx�@Yx�@Yx�@Yx�@Yhs@X�9@X  @W
=@V5?@V@U�-@Up�@U?}@U/@UV@T��@St�@R��@R=q@Q��@Qhs@QG�@Q�@P�@P �@P �@Pb@Pb@O�@O�;@O�;@O�;@O��@O;d@O+@N��@N�y@Nȴ@N��@N@M��@Mp�@MV@LI�@L9X@K�
@KC�@Ko@J�H@J�@I��@I��@I%@H��@HQ�@G��@G�@G�P@G;d@F��@E��@E?}@D�@D�j@D�@C��@B�\@B�@A�#@A��@AX@A%@@�9@?��@?+@>�R@=��@=`B@=V@<��@<��@<j@<1@;��@;t�@:�H@:~�@:n�@:^5@:-@9�#@9G�@8�`@81'@7|�@7\)@7K�@7;d@7�@6ȴ@6�R@6�R@6��@6�+@6v�@6v�@6ff@65?@6@5�-@5?}@4�/@4�@3�m@3�
@3�F@3dZ@333@3o@2��@2�!@2�!@2��@2M�@1�#@1��@1��@1�7@1hs@17L@0��@0b@/�;@/��@/��@/�@/��@/�P@/�P@/�P@/�P@/l�@/;d@/
=@.�R@.ff@.@-�T@-�@,��@,�/@,�j@,z�@,Z@,Z@,�@,�@,(�@,1@+�
@+�F@+t�@*�H@*��@*=q@)X@)�@)%@)%@(��@(��@(Q�@'�w@'\)@&�y@&�+@&E�@&E�@%�@%@%��@%`B@$��@$�@$�@$�D@$Z@$9X@$�@#��@#�
@#��@#33@"�@"~�@"n�@"M�@"�@!�@!�7@!x�@!�@ ��@ b@   @|�@�@��@�+@V@{@�@@�@�/@��@�@j@(�@�m@�F@t�@"�@o@o@@��@��@n�@^5@�@��@&�@�9@ �@�@�w@�P@|�@�R@@@@�@�-@`B@`B@p�@p�@p�@p�@`B@O�@�@�D@j@I�@9X@�m@dZ@�@��@��@��@^5@��@X@G�@7L@&�@��@��@Ĝ@��@�@A�@b@��@|�@l�@\)@K�@
=@�@�R@��@ff@{@�@�T@��@p�@/@�@V@V@��@�@��@�j@�D@j@I�@(�@�@1@�@�@1G�O�A���A��A��mA��A��A��A��;A��/A��yA��HA��HA��mA��;A��#A��#A��;A��#A��HA��#A��
A��`A��;A��;A��A��A��A��A��A��A��A��A��A��A��A���A���A��A��A���A��A��A���A��A��A���A��A��A���A���A��A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A��A���A���A���A��A���A���A��A���A���A��A���A���A��A���A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A�A���A���A�  A���A���A�  A���A���A�  A���A���A�  A�  A���A�  A�  A���A�  A�A���A�A�%A�  A���A�A�A���A�A�A�  A�A�  A���A�  A���A���A�  A���A���A�A�  A�  A�A�%A�A�A�%A�A�A�A�  A�  A�%A�A�A�%A�%A�  A�A�%A�A�A�%A�  A�A�%A�%A�A�%A�A�  A�A�A���A���A�%A�A���A�A�  A���A�A�  A���A���A�  A���A�  A�A���A���A�  A���A���A�  A���A�A�1A�%A�A�A�1A�A�A�A�  A�  A�A�A�  A�  A�  A���A���A���A�  A���A�A�A���A�  A�  A���A�  A�1A�A�  A�%A�A�  A�  A�1A�%A�A�1A�%A�1A�
=A�%A�1A�JA�1A�1A�JA�
=A�%A�%A�JA�VA�
=A�JA�VA�JA�1A�VA�bA�
=A�
=A�bA�bA�JA�
=A�JA�bA�VA�
=A�JA�bA�JA�
=A�bA�bA�VA�JA�bA�VA�A���A��`A��TA��;A�ƨA�ȴA���A���AھwAھwA���A�AڼjAں^AھwAڶFAں^AڼjAڶFAڶFAڼjAڼjAڰ!AڶFA���A���A���A���A�ĜA�ȴA���A�ȴA���A���Aڝ�A�hsA�1A���A��A���A� �A�A�A��Aղ-A՝�AՋDA�M�A�{A���A��A��A��`A���A���A�ƨAԸRAԬAԡ�Aԛ�Aԙ�Aԏ\AԅA�|�A�x�A�x�A�dZA�^5A�^5A�G�A�33A�(�A�+A��A�1A���A��A��/A���A���AӺ^Aӡ�A�r�A�7LA�
=A���AғuA�`BA�+A��A�A���A��A��`A��/A���A�ĜAѺ^AѮAљ�A�l�A�E�A�+A�A��A��TA��/A��#A���A�ƨA�A�ƨA�ĜAмjAмjA���A���A���A��A��TA��`A��HA��HA��;A��;A��;A��/A�ƨAЛ�A�v�A�
=Aϟ�A�dZA�G�A��A��A���AΝ�A�ffA��AͮA͉7A�z�A�I�A�1'A�-A�-A�$�A��A�oA�1A���A��A��HA���A�ĜA̸RA̲-A̲-A̴9A̴9A̮A̩�A̰!Ḁ�A�|�A�33A��A�oA��A��#A���A�ƨA˸RA˼jA˾wA˴9A˛�AˑhA˲-A˾wAˍPA�l�A�E�A��A�t�A��TAɶFA�ZAȴ9A�S�A��HA�bNA�?}A�=qA�&�A�%A�  A��/AƲ-AƍPA�n�A�;dA�{A��A�oA���A��HA�A�C�A�ȴA�M�Að!A�^5A��A�x�A�?}A�A��;A��RA��A�v�A�\)A��yA��A�jA�A�A��A���A�r�A�\)A�G�A�7LA��A��yA��!A��PA�+A��A���A��RA���A�ffA�?}A�1A�A�p�A�
=A���A��-A���A���A���A��DA��+A��A�v�A�VA�(�A��`A�ĜA��-A���A��A�x�A�x�A�r�A�jA�dZA�ZA�C�A�33A�"�A�bA�  A��mA���A��FA��`A���A�z�A�hsA�VA�I�A�A�A�5?A�-A�$�A��A�{A�oA�JA�A���A���A���A��A��;A�ȴA��A�?}A�"�A���A���A���A��uA��A�z�A�r�A�ffA�\)A�VA�O�A�=qA�JA���A��A���A���A���A���A���A���A���A���A��\A��PA��DA��+A�|�A�v�A�t�A�ffA�XA�O�A�E�A�;dA�bA���A���A��A��#A�r�A�?}A�/A� �A���A���A��A�dZA�O�A�7LA�bA��HA���A��PA�M�A���A���A�1'A��!A�jA� �A�bNA�JA���A��;A��jA���A���A��A�dZA�G�A�C�A�C�A�C�A�9XA�$�A��A�JA�A���A��A���A��A�%A�A�p�A���A�A�A�JA��A��TA���A���A�z�A�^5A�I�A�5?A���A��9A���A�~�A�~�A��A�`BA�9XA� �A���A��9A��hA�~�A�t�A�\)A��A���A��HA�ĜA��hA�|�A�n�A�\)A�S�A�9XA��A�JA��HA��RA��A�`BA�33A�%A��;A���A�^5A�bA���A�A�A���A���A��A�XA�?}A�$�A�JA���A��A��TA�ȴA��FA��\A�`BA� �A�ĜG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                               ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�B	��B	�B	�B	�B	�JB	�B	��B	��B	�JB	��B	��B	�B	�JB	��B	�JB	�JB	�B	��B	�JB	��B	��B	��B	��B	�B	�B	�B	��B	��B	�B	��B	�B	��B	��B	�PB	�PB	��B	�B	�B	��B	��B	�B	�B	�JB	�JB	�B	�JB	��B	�cB	�"B

�B
NpB
V�B
\]B
`vB
iB
��B
�hB
�B
��B3�Bs�B��B��B�qB�)B��BT�B�.B�;BuZBqvBW�BS�BK�BN�BOvBG�BH�BR�BS�BB�B@�BGEB?�B;�B9$B:�BFBP�BN�BGEBPBI�BGBVmBR�BMBNBK�BHBGEBG�BA�B:^BAUB'RB#�BIB�B�B��B�/B�sB��B�5B��B��B��B�B�B��Bg8BXBK)B2-B2�B
�B
�9B
�B
��B
�!B
�B
��B
�oB
d&B
V�B
E9B
=�B
;�B
)�B
 'B
B
�B
B	�B	�B	�B	چB	רB	�]B	�KB	�}B	�FB	��B	�_B	�SB	��B	�FB	�PB	��B	�1B	kQB	YKB	S�B	PB	LdB	J�B	H�B	EB	D�B	<�B	1'B	2�B	*�B	B	YB	bB	�B	�B		�B	1B	�B	�B	B	 4B	�B	 iB��B��B��B�PB��B�xB		B	�B	@B	:B	�B	�B	�B	�B	IB	�B	=B	�B	B	�B	�B	 'B	+�B	-B	+B	9XB	>BB	<�B	?}B	<6B	?�B	=�B	=B	<�B	;0B	?�B	@�B	F�B	B'B	=<B	;�B	9$B	8�B	:�B	<6B	<jB	@�B	A�B	@�B	>BB	;�B	;dB	9�B	9XB	<6B	@OB	E�B	G�B	P�B	X�B	_pB	bNB	bNB	`B	P�B	OvB	K)B	K)B	M6B	QNB	W�B	`�B	_;B	\)B	]�B	bB	ffB	k�B	jB	i�B	n�B	k�B	iDB	k�B	k�B	kQB	k�B	qAB	{JB	�B	��B	��B	�B	�_B	�\B	��B	�B	�7B	�lB	�@B	��B	��B	��B	��B	��B	�\B	�aB	��B	��B	�-B	��B	ĜB	�B	�B	�B	��B	�B	�BB	ѷB	҉B	҉B	ҽB	��B	��B	�gB	�#B	�WB	��B	��B	��B	��B	�B	�mB	�>B	�B	�WB	��B	��B	�B	�"B	��B	�vB	��B	��B	��B	�B	�B	�ZB	��B	��B	��B	�(B
 4B
B
�B
�B
�B
_B
1B
	B
	lB

=B
B
B
�B
B

�B

�B

�B
JB
�B
�B
�B
�B
.B
�B
�B
�B
�B
4B
@B
�B
�B
�B
IB
xB
�B
�B
�B
 �B
 �B
!�B
%FB
%�B
&B
&B
%�B
&LB
&�B
(�B
(�B
)�B
+kB
+6B
0!B
0!B
/�B
0UB
1[B
1�B
1�B
1�B
1�B
1�B
1[B
1�B
2-B
2aB
3�B
3hB
3hB
2�B
33B
2�B
2�B
33B
2aB
2�B
2aB
2-B
2-B
2�B
2�B
2�B
1�B
3�B
3�B
3�B
3�B
33B
4B
4B
3�B
3�B
5tB
7�B
7�B
7LB
7�B
9XB
8�B
8�B
8�B
8�B
8�B
9$B
9$B
8�B
9�B
:�B
=�B
=qB
=<B
>wB
?�B
AUB
C-B
F?B
H�B
H�B
H�B
H�B
H�B
HB
G�B
GzB
HKB
IB
I�B
H�B
H�B
HB
GzB
GEB
GB
G�B
IB
I�B
J#B
J�B
J�B
L0B
L0B
L�B
NB
PHB
Q�B
Q�B
Q�B
R B
RTB
R�B
U�B
U2B
U�B
U�B
U�B
V9B
VB
VmB
XyB
W�B
W�B
X�B
X�B
X�B
X�B
YB
Y�B
YB
YB
Y�B
Y�B
YB
[�B
\)B
[�B
[�B
[#B
\�B
]�B
\�B
\�B
\)B
\)B
\)B
\)B
\�B
\)B
\]B
\]B
\]B
[�B
\�B
\�B
aB
aB
aHB
bB
bB
b�B
cTB
d&B
d�B
e�B
e,B
e�B
ffB
f2B
ffB
f2B
f�B
f�B
g�B
g�B
g�B
h>B
h
B
h�B
iyB
iB
iDB
h�B
iB
j�B
kQB
k�B
k�B
k�B
k�B
l"B
lWB
l�B
l�B
m)B
m]B
m�B
m]B
m�B
m�B
m�B
m�B
ncB
o B
o�B
p;B
pB
pB
p;B
p�B
p�B
p�B
poB
q�B
q�B
qvB
qvB
q�B
q�B
q�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
s�B
tB
tB
s�B
t�B
t�B
u�B
u�B
u�B
u�B
u�B
u�B
v�B
u�B
uZB
u�B
w2B
w�B
xB
w�B
xB
xB
xlB
w�B
xB
wfB
w2B
w2B
xB
y>B
y�B
zDB
zxB
z�B
{B
{JB
{JB
{�B
|�B
}�B
}�B
}�B
}VB
}�B
~(B
~�B
.B
cB
�B
�B
�4B
�iB
��B
��B
�oB
�B
�uB
�B
�AB
�uB
��B
��B
��B
�B
�B
�MB
��B
�B
��B
��B
��B
��B
�%B
�%B
��B
��B
��B
�+B
��B
��B
��B
��B
��B
��B
��B
�1B
��B
��B
�B
��B
�B
��B
��B
��B
�lB
��B
�B
�DB
�DB
�xB
�xB
��B
�xB
�xB
�B
�B
��B
��B
�VB
��B
�VB
�VB
��B
�\B
�\B
�\B
�\B
��B
��B
��B
�\B
��B
��B
��B
��B
�.B
��B
�bB
��B
�4B
��B
��B
��B
��B
�oB
��B
��B
��B
��B
�@B
��B
�B
�B
��B
�B
��B
�B
�B
��B
��B
��B
��B
��B
�SB
��B
��B
��B
�1B
��B
�eB
�1B
��B
��B
��B
�=B
�B
�B
�CB
�xB
�CB
�xB
��B
�IB
��B
�B
��B
��B
��B
��B
��B
��B
�!B
��B
�'B
��B
��B
��B
�\B
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
�bB
�-B
��B
�hB
�hB
��B
��B
�B
�:B
�nB
�@B
��B
��B
��B
�zB
��B
��B
��B
��B
�B
�B
��B
�B
�B
�B
�B
�RB
�RB
�RB
�RB
�B
�B
�RB
��B
�RB
��B
��B
��B
��B
�$B
��B
��B
�XB
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
�*B
�_B
��B
�B
�B
�B
�B
�B
�6B
�6B
��B
�B
�=B
��B
�B
�B
�wB
��B
��B
��B
�}B
�IB
�IB
�}B
�}B
�}B
�}B
��B
��B
�B
�OB
��B
��B
��B
��B
��B
�!B
��B
�UB
��B
��B
��B
�[B
��B
��B
��B
�aB
��B
��B
��B
��B
�nB
�nB
�nB
��B
�?B
�tB
��B
�B
��B
�LB
�B
�B
�B
��B
��B
��B
��B
�RB
��B
��B
��B
�$B
��B
��B
��B
�XB
��B
�*B
��B
��B
�*B
�^B
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
�^B
��B
�dB
��B
�B
�6B
�6B
�jB
��B
��B
��B
��B
��B
�B
�<B
�<B
�qB
�qB
��B
�B
��B
��B
��B
��B
�B
�HB
��B
��B
��B
��B
�B
�B
��B
��B
��B
�aB
�-B
�-B
�-B
�aB
�aB
�aB
�-B
�-B
��B
�-B
��B
ĜB
��B
��B
��B
�B
�B	�rB	��B	�ZB	��B	��B	�xB	�B	�"B	��B	��B	��B	��B	�B	�VB	��B	�B	��B	�DB	��B	��B	�DB	�JB	�PB	��B	�xB	�JB	�B	��B	�B	��B	��B	�DB	�JB	�PB	�DB	�DB	��B	�B	�B	�B	�B	��B	��B	��B	��B	�B	��B	��B	��B	��B	�B	�B	�B	��B	��B	�B	��B	�JB	�JB	��B	�JB	�B	�B	��B	��B	��B	��B	�PB	�B	�JB	��B	�B	�xB	�B	�B	��B	��B	�DB	�xB	��B	��B	�B	�PB	��B	�DB	�B	��B	�B	�B	�B	�xB	�B	�PB	�JB	�B	��B	�DB	�B	��B	�JB	��B	�B	��B	��B	��B	��B	��B	��B	��B	��B	�JB	��B	�B	�xB	�PB	�JB	�xB	��B	��B	�B	��B	��B	��B	��B	��B	�B	��B	�xB	��B	��B	�B	�JB	��B	��B	�xB	��B	��B	�JB	��B	��B	�PB	�B	��B	��B	��B	��B	��B	��B	��B	��B	�rB	��B	��B	�xB	�B	��B	�DB	�B	�PB	��B	�B	��B	�B	��B	�B	�B	��B	��B	�JB	�B	�B	��B	�DB	��B	��B	�DB	��B	��B	��B	�xB	��B	�xB	��B	��B	��B	�PB	�"B	��B	��B	�PB	�B	�B	�PB	�B	�xB	�PB	��B	�xB	�B	��B	�JB	��B	��B	��B	�B	�PB	��B	��B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�B	��B	�B	�JB	��B	�B	��B	�B	��B	�B	�B	��B	�DB	�B	��B	��B	��B	�"B	��B	��B	�"B	��B	��B	��B	��B	�B	��B	�"B	��B	�PB	��B	��B	��B	��B	�B	�B	��B	��B	�B	�B	��B	��B	�B	��B	��B	��B	��B	��B	��B	�JB	��B	�rB	�B	�"B	��B	�B	�B	��B	�xB	�B	��B	��B	�JB	��B	��B	�B	��B	�B	�DB	�PB	��B	��B	��B	�JB	�B	��B	��B	�xB	�B	��B	�PB	�xB	��B	�B	��B	�DB	�B	��B	��B	�xB	��B	��B	�xB	�xB	��B	��B	�JB	��B	�B	�B	�B	�B	�B	�B	�xB	�B	�B	�B	�>B	�DB	�(B	�B	�VB	��B	��B	�.B	�.B
 4B	�cB
 �B	�cB	��B	�]B
 iB	��B	�(B
B	��B	�]B
 �B	��B	�]B	��B
 �B	�(B	��B	�B	�B	�B	��B	�DB	�>B	��B	�8B	��B	�>B
�B
AB
(B
�B
P�B
@�B
]dB
H�B
IB
J#B
IRB
W?B
PHB
UgB
P�B
R B
UgB
W?B
S�B
W�B
Y�B
Z�B
[WB
\]B
Z�B
Z�B
\]B
\�B
\)B
[#B
_�B
\�B
\�B
_�B
c�B
_;B
^B
b�B
a�B
cTB
aB
a�B
a�B
c�B
d�B
iyB
i�B
o�B
o�B
tTB
}"B
.B
��B
�{B
��B
�B
��B
�oB
�B
��B
�\B
��B
�0B
�-B
��B
��B
� B
�dB
�B
�,B
��B
�8B
��B
�5B
��B
��B
� B
�B
�B
�+B
��B  B�B�B�BFBB�B2-BCaBJ�BU�BYKB^5BqvBrBpoBo�Bw�BzxB|PB�B��B�{B�B�FB��B��B�B�?B�B�FB��B��B�XB�RB��B�B��B�^B��B��B�UB��B�BB� B�UB��B�[B�B��B�B�GB��B��B�|B�+B��BB	B\B�B7B,BA�Bi�BqvB�SB�PB�B��B�B��B��B�=B�CB� B�B�iB��B�uB}�B��Bz�B{JB}�B{JBr|Bp�BsBr�Bo�Bn�BxlB{JBw2BtBd�BrGBdZB[#BW
BW?BU2BS�BL0BTaBV�BR BQ�BL�BQ�B[�BK)BHKBG�BG�BK�BPHBMjBNB\)BOBJ�BG�BJ�BLdBPHBP�BU2B\)BR�BNpBL0BGEBGzBFtBH�BE�BD�BGzBG�BL0BNBF�BEBIBJ�BEmBH�BL0BNpBQBTaBTaBVBU�BS�BP�BOBBW
Bz�BQ�BK^BD�BFBD3BA�BA�BC�BA�BA BA�BB�B?B?�BA�BA�BB[B@�BB�BD�BHBP}BF�BC�BIBHBCaB>�B?B>wB=qB<B<B;0B<6B>B=B@�B8B7LB5tB7�B:�B:^B9$B8�B:^B:�B:^B9$B9�B;�B;0B;dB?BA�BC�BD�BD�BPHBG�BK�BK�BR BZ�BFtBGzBO�BbB[�BX�BS&BM�BK�BM�BG�BC�BAUBA B=�BIBLdBL0BJ�BK�Be,BK�BK)BK�BN<BM6BJ�BK�BM6BL0BI�BHKBGzBGzBI�BHKBHBFtBDgBC�BI�BM�BW�BM�BS�Bk�BR�BXyBOBBP}BS&BU�BO�BU�BL0BL0B^�BF�BL�BK)BHBIRBOBBOBNBNBS�BMjBL�BH�BL�BP�BLdBF�BIRBK)BK^BEmBD�BC�BI�BE�BI�BK)BE�BGEBE�BHBFBE�BD�BEmBI�BH�BL0BEB?}BK�B>B<jB;dB>B;�B7�B8�B;�B:*B>wB9XB=�B<jG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No good data in profile due to conductivity sensor failure after strong salinity drift;                                                                                                                                                                         202104292026552021042920265520210429202655202104292026552021042920265520210429202655SI  SI  ARFMARFM                                                                                                                                                2020112920471120201129204711IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022416401120210224164011QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021022416401120210224164011QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021042910194020210429101940IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021042920270120210429202701IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2021042920270120210429202701IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021042920270120210429202701IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                