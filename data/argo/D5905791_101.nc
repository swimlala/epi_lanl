CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-11-25T02:37:58Z creation; 2022-09-06T18:25:46Z DMQC;      
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20211125023758  20220907192127  5905791 5905791 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               e   eAA  AOAO7825_008765_101                 7825_008765_101                 2C  2C  DD  SOLO_II                         SOLO_II                         8765                            8765                            V2.6; SBE602 06Mar19            V2.6; SBE602 06Mar19            853 853 @٥?�kP�@٥?�kP�11  @٥?���@٥?���@5�Ϫ͟@5�Ϫ͟�e �i�J�e �i�J11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��?��H@@  @�G�@�  @�  @�  @��RA  A ��A,(�A@  A`  A~�RA�  A�Q�A�\)A�\)A�  A��A�Q�B Q�BQ�B(�BQ�B (�B'�
B0(�B8  B@  BH  BP  BX(�B`  Bg�
Bp  Bx(�B�{B�  B�{B�{B�  B��B��B�  B�{B��
B��
B��B�{B�{B�{B�{B�  B��B�  B�{B��
B�  B�  B��
B��B��
B�  B�{B��B�{B�=qB�(�C   C  C
=C  C
=C
  C  C
=C  C��C��C��C�C  C
=C  C 
=C"{C${C&
=C(  C*
=C,  C-��C0
=C2  C4
=C6
=C8  C:{C<
=C>  C@  CB  CC��CE��CH  CJ
=CL  CN
=CP  CR  CT
=CV  CW��CY��C[��C^
=C`{Cb{Cd
=Cf
=Ch  Ci��Ck�Cm�Cp  Cr  Ct  Cv
=Cx  Cy��C|  C}��C��C�  C�C���C�  C�C�C���C���C�  C�C�C���C�  C�  C�  C�C�  C���C���C�  C���C���C�  C�  C�C�C�C�C�  C���C���C���C���C���C���C�C�  C��C���C�  C���C���C�C�C�  C�  C�  C�C�  C���C���C�  C�C�
=C�C�  C�C�
=C�
=C�
=C�
=C�C���C�C�  C���C���C�  C�C�
=C�\C�
=C�  C�  C�  C�
=C�
=C�C�  C�  C�
=C�
=C�C���C�  C�C�  C���C���C���C���C���C�  C�
=C�C�C�C�C�C�
=C�C�  C�
=C�
=C���C���C���C���C���C���C�  C���C�  C�C�C���C�C�
=C�  C���C�C�C�C�  C���C�  C�D �D � D ��D}qD  D��DD� D��D� D�D��D�D� D  D� D  D��D	�D	��D
�D
� D�D��D�D�D  D}qD�qD}qD  D� D�D� D�D� D�qDz�D  D�D  D��D�D� D  D��D�D� D�D� D�qD��DD��D��D}qD�qD� D�qD}qD�D��D  D� D�qD }qD ��D!}qD"  D"�D#�D#� D$�D$� D$�qD%� D%�qD&� D'�D'��D(�D(� D(�qD)��D*�D*� D*�RD+� D,D,�D-�D-��D.  D.� D/  D/� D0  D0z�D0�qD1��D2�D2��D3  D3z�D3�qD4��D5�D5}qD5�qD6��D7  D7}qD7�qD8}qD8��D9� D:  D:}qD;  D;� D;�qD<}qD=  D=}qD=�qD>}qD>�qD?� D@  D@��DADA��DB�DB��DC  DCz�DC��DDz�DD�qDE� DE�qDFz�DG  DG��DH  DH}qDI  DI}qDI��DJ� DK  DKz�DK��DL}qDM  DM� DM�qDN��DO  DO� DO�qDPz�DP�qDQ� DR  DR� DR�qDS}qDT  DT��DU  DU}qDU�qDV}qDW  DW� DX  DX}qDX��DY� DZ�DZ� D[  D[��D\  D\}qD]  D]}qD]�qD^}qD_  D_��D`  D`��Da�Da}qDa��Db}qDcDc��Dd�Dd� Dd��De��Df�Df��Dg�Dg� Dh  Dh}qDh�RDi}qDj  Dj� Dk�Dk�Dl�Dl}qDl��Dm� Dm�qDnz�Dn�qDo��Do�qDp� Dq�Dq� Dr  Dr� Ds  Ds� Dt  Dt��Du  Duz�Du�qDvz�Dw  Dw�Dx  Dx}qDx�qDy� Dy�qDz��D{D{��D|�D|��D|�qD}}qD~�D~}qD~�qD}qD��D�=qD�}qD��qD�HD�AHD�� D�� D�  D�@ D��HD�D�HD�@ D�� D��HD�HD�@ D�~�D�� D���D�>�D�~�D��qD�  D�AHD�� D�� D�  D�>�D��HD�� D��qD�>�D��HD��HD�  D�=qD�}qD�� D�  D�@ D���D�D��D�AHD��HD�D���D�=qD�� D�� D���D�B�D��HD��qD��qD�@ D��HD��HD�HD�C�D��HD���D���D�@ D��HD��HD��D�@ D�}qD��qD���D�@ D�� D���D���D�@ D��HD��HD���D�@ D�� D���D���D�AHD���D��HD�  D�@ D�� D��HD���D�>�D���D���D���D�>�D�}qD��qD�HD�AHD�� D���D��qD�@ D�~�D�� D��D�>�D�~�D��HD�HD�>�D�}qD��HD��D�@ D��HD��HD��D�C�D���D�D��D�>�D�~�D�� D���D�AHD��HD��qD�  D�@ D�~�D��HD�HD�@ D�� D�� D���D�@ D�� D�� D�  D�>�D�~�D�� D���D�@ D���D��HD���D�>�D��HD��HD�HD�B�D���D��HD���D�>�D�~�D���D���D�>�D�� D�� D�  D�@ D���D��HD�HD�AHD�~�D���D�HD�AHD�� D��HD�HD�AHD�� D�� D�  D�AHD�� D��HD�HD�@ D�}qD���D�  D�=qD�� D��HD�HD�B�D��HD�� D���D�@ D��HD�� D���D�<)D�~�D��HD�  D�AHD���D�� D���D�@ D��HD�� D��qD�>�D��HD�D��D�AHD��HD�D�  D�=qD�� D�� D���D�@ D��HD��HD�HD�@ D��HD��HD�  D�B�D���D�� D�  D�AHD�� D�� D���D�AHD���D��HD�  D�@ D��HD�� D�  D�=qD�~�D�� D�HD�@ D�� D��HD�HD�AHD��HD��HD���D�=qD D��HD�  D�=qD�~�D�� D��qD�>�DĀ Dľ�D�  D�AHDŁHD��HD�  D�@ D�~�DƽqD��qD�>�DǁHD�� D�  D�@ D�}qD�� D�HD�AHDɁHD��HD�  D�@ DʁHDʽqD���D�@ DˁHD���D��D�>�D̀ D��HD�HD�AHD̀ D�� D���D�>�D΁HD�D��D�>�D�~�D�� D��qD�@ DЀ Dо�D�HD�B�DсHD��HD��D�C�DҁHD�� D�  D�AHDӁHD��HD�HD�AHDԁHD�� D�  D�AHDՀ D��HD�HD�@ D�~�D־�D���D�=qD�~�D�� D���D�>�D�}qDؽqD�  D�AHDـ D�� D�  D�@ Dڀ Dھ�D���D�@ DہHD�D�HD�@ D܀ Dܾ�D���D�AHD݀ Dݾ�D���D�@ Dހ D��HD��D�AHD�~�D�� D�HD�AHD�� D��HD�HD�>�D�}qDᾸD��D�AHD� D⾸D�  D�@ D�~�D�qD��qD�>�D� D�D�  D�>�D� D徸D��qD�=qD�~�D澸D�  D�@ D� D�� D�  D�@ D�~�D辸D���D�>�D�HD�D���D�>�D�~�D�� D�  D�AHD� D뾸D���D�@ D� D�� D�  D�>�D�~�D�� D�  D�AHD� DD�  D�AHD� D�� D�  D�@ D�~�D�qD��qD�@ D�HD�� D�  D�>�D�~�D�qD��qD�@ D�HD��HD�HD�@ D� D�D��D�@ D��HD���D��D�B�D���D�� D���D�>�D�~�D�� D�HD�AHD��HD�� D�  D�@ D�}qD�� D�  D�@ D�j=?��?L��?u?��R?\?�G�@�@�@&ff@0��@J=q@Q�@h��@z�H@��@�{@�@�G�@��@�z�@���@�ff@˅@�
=@�p�@���@��@��HA�A
=A�A  A�
A��A{A"�\A'
=A,(�A0  A5A9��A>�RAC�
AG�AL��AQG�AW
=AZ�HA`��Ac�
Aj=qAmp�As�
Aw
=A~{A�  A��A��A�Q�A��A���A�ffA���A�33A�{A��A��HA�z�A��A�G�A�(�A�A���A��HA��A�\)A��A�(�A�{A�G�A�33A�{A��A\A�(�AǮA�G�A�(�A�{A���A��HA�{A׮A��HA�z�A߮A�G�A�z�A�{A�G�A��HA�{A�A��HA���A��A��A�z�A�ffB ��B��B33B�
Bp�BffB�B��B
=qB33B��Bp�B
=B  Bp�BffB  B��B=qB33B��B��B33B  B��BffB   B ��B"ffB#33B$��B%��B'33B(  B)��B*=qB,  B,��B.=qB/33B0��B1p�B3
=B3�
B5G�B6=qB7�
B8��B:{B;33B<z�B=��B>�HB@  BA�BBffBC�BD��BEBG\)BH(�BIBJffBL  BL��BNffBO33BP��BQ��BR�HBT  BUG�BV=qBW�BX��BY�BZ�HB\(�B]G�B^�\B_�B`��Ba�Bc33Bd(�Be��Bf�RBg�
Bh��BjffBk\)Bl��Bm�Bo
=Bp(�BqG�Br�HBs�Bup�Bv=qBw�
Bx��Bz{B{
=B|��B}��B
=B�{B��RB�G�B��B��\B��B��
B�=qB�
=B�p�B�Q�B���B�p�B��B��\B�33B��
B�Q�B���B��B�(�B��HB�G�B�(�B��\B�G�B�B�z�B��HB��B�{B���B�\)B��B��\B��B��B�Q�B���B�\)B�(�B�z�B�\)B��B�z�B���B��B�  B���B�
=B��B�Q�B���B�\)B��
B��\B���B��B�{B��RB�G�B��B�ffB��B��B�=qB��HB�\)B�{B�z�B�G�B���B�ffB���B���B�{B��\B�\)B��
B���B��B��
B�ffB�
=B��B�Q�B��RB���B��B���B�\)B��B���B��B�  B�z�B�\)B�B�z�B�G�B�B���B��B��B��\B��B�  B�ffB�\)B�B\B�G�B�Bģ�B��B�  Bƣ�B�G�B�{Bȏ\B�p�B��BʸRB�p�B��B��HB�G�B�{BΣ�B�G�B�{BЏ\B�p�B�  B��HB�p�B�(�B���Bՙ�B�ffB���B��B�z�B�G�B�{Bڣ�Bۙ�B�{B�
=Bݙ�B�z�B�33B��
B�RB�G�B�=qB��HB�B�z�B��B�  B��B�p�B�(�B��HB��
B�Q�B��B�  B�z�B�\)B�{B�RB�B�(�B�
=B�B�ffB�33B�B���B�G�B��B���B��B�  B�Q�B���B���B��B��\B��HB�\)B�B�  B�z�B���B���B�G�B��B�  B�(�B���B��RB�33B�\)B��
C   C (�C \)C p�C �C C �HC{C(�CffCp�C�C��C�C(�CG�CffC�C�RC  C�C=qC�C��C��C
=C�C\)C�C�C�C
=C33Cp�C�C��C�C{CQ�CffC��C�
C��C(�C\)Cz�C�RC��C{C33C\)C��C�C�C	(�C	=qC	�C	��C	�
C

=C
(�C
p�C
�\C
C  C�CffC��C�RC  C�C\)C��C�C�C�C=qC�\C�C�HC�C=qC�C��C�HC{C33Cp�C�C��C{C33Cp�C�RC��C{CG�CffC�RC�HC
=CQ�Cp�C�RC�C
=C\)Cz�C�C�C{CQ�C��C�C��C33CG�C��CC��CG�CffC��C�C
=C\)C�C�RC
=C(�CffC�RC�
C�C\)C�C�
C��C33Cz�C��C�C�CG�C��C��C��CG�Cp�C�C��C{C\)C��CC
=CQ�Cp�C�RC  C�Cz�C��C�
C �C G�C ��C ��C ��C!G�C!z�C!��C!��C"(�C"G�C"��C"��C#  C#G�C#z�C#��C#��C$�C$G�C$��C$��C$��C%G�C%p�C%�C%��C&�C&Q�C&��C&C'
=C'Q�C'p�C'C'��C(�C(p�C(��C(��C)�C)Q�C)z�C)�
C)��C*33C*�C*��C*�C+33C+\)C+�C+��C,{C,ffC,��C,��C-�C-ffC-�\C-��C.(�C.Q�C.�C.�HC/{C/G�C/��C/�
C0
=C0\)C0��C0��C1�C1ffC1�\C1�HC2(�C2Q�C2��C2��C3{C3p�C3�RC3�
C433C4p�C4��C4��C533C5\)C5��C5��C6{C6ffC6�C6�HC7�C7p�C7��C7�
C8(�C8ffC8�\C8��C9�C9ffC9�\C9�C:33C:\)C:��C:��C;(�C;\)C;�C;��C<�C<\)C<�RC<�C=�C=p�C=�RC=�HC>(�C>z�C>��C>�HC?=qC?ffC?��C?��C@(�C@\)C@�RC@�CA�CAp�CA�RCA�HCB33CBz�CB�CB�CCG�CCz�CC�CD  CD=qCDffCD�RCE  CE(�CEp�CECE��CF�CFz�CF�CF�HCG33CGp�CG��CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                ?��?��H@@  @�G�@�  @�  @�  @��RA  A ��A,(�A@  A`  A~�RA�  A�Q�A�\)A�\)A�  A��A�Q�B Q�BQ�B(�BQ�B (�B'�
B0(�B8  B@  BH  BP  BX(�B`  Bg�
Bp  Bx(�B�{B�  B�{B�{B�  B��B��B�  B�{B��
B��
B��B�{B�{B�{B�{B�  B��B�  B�{B��
B�  B�  B��
B��B��
B�  B�{B��B�{B�=qB�(�C   C  C
=C  C
=C
  C  C
=C  C��C��C��C�C  C
=C  C 
=C"{C${C&
=C(  C*
=C,  C-��C0
=C2  C4
=C6
=C8  C:{C<
=C>  C@  CB  CC��CE��CH  CJ
=CL  CN
=CP  CR  CT
=CV  CW��CY��C[��C^
=C`{Cb{Cd
=Cf
=Ch  Ci��Ck�Cm�Cp  Cr  Ct  Cv
=Cx  Cy��C|  C}��C��C�  C�C���C�  C�C�C���C���C�  C�C�C���C�  C�  C�  C�C�  C���C���C�  C���C���C�  C�  C�C�C�C�C�  C���C���C���C���C���C���C�C�  C��C���C�  C���C���C�C�C�  C�  C�  C�C�  C���C���C�  C�C�
=C�C�  C�C�
=C�
=C�
=C�
=C�C���C�C�  C���C���C�  C�C�
=C�\C�
=C�  C�  C�  C�
=C�
=C�C�  C�  C�
=C�
=C�C���C�  C�C�  C���C���C���C���C���C�  C�
=C�C�C�C�C�C�
=C�C�  C�
=C�
=C���C���C���C���C���C���C�  C���C�  C�C�C���C�C�
=C�  C���C�C�C�C�  C���C�  C�D �D � D ��D}qD  D��DD� D��D� D�D��D�D� D  D� D  D��D	�D	��D
�D
� D�D��D�D�D  D}qD�qD}qD  D� D�D� D�D� D�qDz�D  D�D  D��D�D� D  D��D�D� D�D� D�qD��DD��D��D}qD�qD� D�qD}qD�D��D  D� D�qD }qD ��D!}qD"  D"�D#�D#� D$�D$� D$�qD%� D%�qD&� D'�D'��D(�D(� D(�qD)��D*�D*� D*�RD+� D,D,�D-�D-��D.  D.� D/  D/� D0  D0z�D0�qD1��D2�D2��D3  D3z�D3�qD4��D5�D5}qD5�qD6��D7  D7}qD7�qD8}qD8��D9� D:  D:}qD;  D;� D;�qD<}qD=  D=}qD=�qD>}qD>�qD?� D@  D@��DADA��DB�DB��DC  DCz�DC��DDz�DD�qDE� DE�qDFz�DG  DG��DH  DH}qDI  DI}qDI��DJ� DK  DKz�DK��DL}qDM  DM� DM�qDN��DO  DO� DO�qDPz�DP�qDQ� DR  DR� DR�qDS}qDT  DT��DU  DU}qDU�qDV}qDW  DW� DX  DX}qDX��DY� DZ�DZ� D[  D[��D\  D\}qD]  D]}qD]�qD^}qD_  D_��D`  D`��Da�Da}qDa��Db}qDcDc��Dd�Dd� Dd��De��Df�Df��Dg�Dg� Dh  Dh}qDh�RDi}qDj  Dj� Dk�Dk�Dl�Dl}qDl��Dm� Dm�qDnz�Dn�qDo��Do�qDp� Dq�Dq� Dr  Dr� Ds  Ds� Dt  Dt��Du  Duz�Du�qDvz�Dw  Dw�Dx  Dx}qDx�qDy� Dy�qDz��D{D{��D|�D|��D|�qD}}qD~�D~}qD~�qD}qD��D�=qD�}qD��qD�HD�AHD�� D�� D�  D�@ D��HD�D�HD�@ D�� D��HD�HD�@ D�~�D�� D���D�>�D�~�D��qD�  D�AHD�� D�� D�  D�>�D��HD�� D��qD�>�D��HD��HD�  D�=qD�}qD�� D�  D�@ D���D�D��D�AHD��HD�D���D�=qD�� D�� D���D�B�D��HD��qD��qD�@ D��HD��HD�HD�C�D��HD���D���D�@ D��HD��HD��D�@ D�}qD��qD���D�@ D�� D���D���D�@ D��HD��HD���D�@ D�� D���D���D�AHD���D��HD�  D�@ D�� D��HD���D�>�D���D���D���D�>�D�}qD��qD�HD�AHD�� D���D��qD�@ D�~�D�� D��D�>�D�~�D��HD�HD�>�D�}qD��HD��D�@ D��HD��HD��D�C�D���D�D��D�>�D�~�D�� D���D�AHD��HD��qD�  D�@ D�~�D��HD�HD�@ D�� D�� D���D�@ D�� D�� D�  D�>�D�~�D�� D���D�@ D���D��HD���D�>�D��HD��HD�HD�B�D���D��HD���D�>�D�~�D���D���D�>�D�� D�� D�  D�@ D���D��HD�HD�AHD�~�D���D�HD�AHD�� D��HD�HD�AHD�� D�� D�  D�AHD�� D��HD�HD�@ D�}qD���D�  D�=qD�� D��HD�HD�B�D��HD�� D���D�@ D��HD�� D���D�<)D�~�D��HD�  D�AHD���D�� D���D�@ D��HD�� D��qD�>�D��HD�D��D�AHD��HD�D�  D�=qD�� D�� D���D�@ D��HD��HD�HD�@ D��HD��HD�  D�B�D���D�� D�  D�AHD�� D�� D���D�AHD���D��HD�  D�@ D��HD�� D�  D�=qD�~�D�� D�HD�@ D�� D��HD�HD�AHD��HD��HD���D�=qD D��HD�  D�=qD�~�D�� D��qD�>�DĀ Dľ�D�  D�AHDŁHD��HD�  D�@ D�~�DƽqD��qD�>�DǁHD�� D�  D�@ D�}qD�� D�HD�AHDɁHD��HD�  D�@ DʁHDʽqD���D�@ DˁHD���D��D�>�D̀ D��HD�HD�AHD̀ D�� D���D�>�D΁HD�D��D�>�D�~�D�� D��qD�@ DЀ Dо�D�HD�B�DсHD��HD��D�C�DҁHD�� D�  D�AHDӁHD��HD�HD�AHDԁHD�� D�  D�AHDՀ D��HD�HD�@ D�~�D־�D���D�=qD�~�D�� D���D�>�D�}qDؽqD�  D�AHDـ D�� D�  D�@ Dڀ Dھ�D���D�@ DہHD�D�HD�@ D܀ Dܾ�D���D�AHD݀ Dݾ�D���D�@ Dހ D��HD��D�AHD�~�D�� D�HD�AHD�� D��HD�HD�>�D�}qDᾸD��D�AHD� D⾸D�  D�@ D�~�D�qD��qD�>�D� D�D�  D�>�D� D徸D��qD�=qD�~�D澸D�  D�@ D� D�� D�  D�@ D�~�D辸D���D�>�D�HD�D���D�>�D�~�D�� D�  D�AHD� D뾸D���D�@ D� D�� D�  D�>�D�~�D�� D�  D�AHD� DD�  D�AHD� D�� D�  D�@ D�~�D�qD��qD�@ D�HD�� D�  D�>�D�~�D�qD��qD�@ D�HD��HD�HD�@ D� D�D��D�@ D��HD���D��D�B�D���D�� D���D�>�D�~�D�� D�HD�AHD��HD�� D�  D�@ D�}qD�� D�  D�@ G�O�?��?L��?u?��R?\?�G�@�@�@&ff@0��@J=q@Q�@h��@z�H@��@�{@�@�G�@��@�z�@���@�ff@˅@�
=@�p�@���@��@��HA�A
=A�A  A�
A��A{A"�\A'
=A,(�A0  A5A9��A>�RAC�
AG�AL��AQG�AW
=AZ�HA`��Ac�
Aj=qAmp�As�
Aw
=A~{A�  A��A��A�Q�A��A���A�ffA���A�33A�{A��A��HA�z�A��A�G�A�(�A�A���A��HA��A�\)A��A�(�A�{A�G�A�33A�{A��A\A�(�AǮA�G�A�(�A�{A���A��HA�{A׮A��HA�z�A߮A�G�A�z�A�{A�G�A��HA�{A�A��HA���A��A��A�z�A�ffB ��B��B33B�
Bp�BffB�B��B
=qB33B��Bp�B
=B  Bp�BffB  B��B=qB33B��B��B33B  B��BffB   B ��B"ffB#33B$��B%��B'33B(  B)��B*=qB,  B,��B.=qB/33B0��B1p�B3
=B3�
B5G�B6=qB7�
B8��B:{B;33B<z�B=��B>�HB@  BA�BBffBC�BD��BEBG\)BH(�BIBJffBL  BL��BNffBO33BP��BQ��BR�HBT  BUG�BV=qBW�BX��BY�BZ�HB\(�B]G�B^�\B_�B`��Ba�Bc33Bd(�Be��Bf�RBg�
Bh��BjffBk\)Bl��Bm�Bo
=Bp(�BqG�Br�HBs�Bup�Bv=qBw�
Bx��Bz{B{
=B|��B}��B
=B�{B��RB�G�B��B��\B��B��
B�=qB�
=B�p�B�Q�B���B�p�B��B��\B�33B��
B�Q�B���B��B�(�B��HB�G�B�(�B��\B�G�B�B�z�B��HB��B�{B���B�\)B��B��\B��B��B�Q�B���B�\)B�(�B�z�B�\)B��B�z�B���B��B�  B���B�
=B��B�Q�B���B�\)B��
B��\B���B��B�{B��RB�G�B��B�ffB��B��B�=qB��HB�\)B�{B�z�B�G�B���B�ffB���B���B�{B��\B�\)B��
B���B��B��
B�ffB�
=B��B�Q�B��RB���B��B���B�\)B��B���B��B�  B�z�B�\)B�B�z�B�G�B�B���B��B��B��\B��B�  B�ffB�\)B�B\B�G�B�Bģ�B��B�  Bƣ�B�G�B�{Bȏ\B�p�B��BʸRB�p�B��B��HB�G�B�{BΣ�B�G�B�{BЏ\B�p�B�  B��HB�p�B�(�B���Bՙ�B�ffB���B��B�z�B�G�B�{Bڣ�Bۙ�B�{B�
=Bݙ�B�z�B�33B��
B�RB�G�B�=qB��HB�B�z�B��B�  B��B�p�B�(�B��HB��
B�Q�B��B�  B�z�B�\)B�{B�RB�B�(�B�
=B�B�ffB�33B�B���B�G�B��B���B��B�  B�Q�B���B���B��B��\B��HB�\)B�B�  B�z�B���B���B�G�B��B�  B�(�B���B��RB�33B�\)B��
C   C (�C \)C p�C �C C �HC{C(�CffCp�C�C��C�C(�CG�CffC�C�RC  C�C=qC�C��C��C
=C�C\)C�C�C�C
=C33Cp�C�C��C�C{CQ�CffC��C�
C��C(�C\)Cz�C�RC��C{C33C\)C��C�C�C	(�C	=qC	�C	��C	�
C

=C
(�C
p�C
�\C
C  C�CffC��C�RC  C�C\)C��C�C�C�C=qC�\C�C�HC�C=qC�C��C�HC{C33Cp�C�C��C{C33Cp�C�RC��C{CG�CffC�RC�HC
=CQ�Cp�C�RC�C
=C\)Cz�C�C�C{CQ�C��C�C��C33CG�C��CC��CG�CffC��C�C
=C\)C�C�RC
=C(�CffC�RC�
C�C\)C�C�
C��C33Cz�C��C�C�CG�C��C��C��CG�Cp�C�C��C{C\)C��CC
=CQ�Cp�C�RC  C�Cz�C��C�
C �C G�C ��C ��C ��C!G�C!z�C!��C!��C"(�C"G�C"��C"��C#  C#G�C#z�C#��C#��C$�C$G�C$��C$��C$��C%G�C%p�C%�C%��C&�C&Q�C&��C&C'
=C'Q�C'p�C'C'��C(�C(p�C(��C(��C)�C)Q�C)z�C)�
C)��C*33C*�C*��C*�C+33C+\)C+�C+��C,{C,ffC,��C,��C-�C-ffC-�\C-��C.(�C.Q�C.�C.�HC/{C/G�C/��C/�
C0
=C0\)C0��C0��C1�C1ffC1�\C1�HC2(�C2Q�C2��C2��C3{C3p�C3�RC3�
C433C4p�C4��C4��C533C5\)C5��C5��C6{C6ffC6�C6�HC7�C7p�C7��C7�
C8(�C8ffC8�\C8��C9�C9ffC9�\C9�C:33C:\)C:��C:��C;(�C;\)C;�C;��C<�C<\)C<�RC<�C=�C=p�C=�RC=�HC>(�C>z�C>��C>�HC?=qC?ffC?��C?��C@(�C@\)C@�RC@�CA�CAp�CA�RCA�HCB33CBz�CB�CB�CCG�CCz�CC�CD  CD=qCDffCD�RCE  CE(�CEp�CECE��CF�CFz�CF�CF�HCG33CGp�CG��CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A��A�VA���AظRAؕ�A�~�A�ffA�Q�A�E�A�?}A�9XA�33A�-A�+A�(�A�$�A�"�A�"�A�"�A� �A��A��A��A��A��A��A��A��A���A���A״9A�v�A�C�A�
=A֙�A�1AՕ�A�ZA���A��mA��TA��;A���A���AԾwAԁA�K�A�1A�{A���A��TAǕ�A�\)AËDA�oA�33A�A��FA�n�A�  A���A��A�G�A�9XA���A�jA��A�^5A�jA�  A�=qA���A��A��+A�
=A��wA�A�A�v�A���A���A�&�A�bNA��A�hsA�33A�t�A�  A���A��A�"�A��A���A�bA�x�A��DA�jA�^5A��A�VA�=qA��uA�XA�-A�^5A���A��
A�p�A��uA�I�A�O�A���A���A�33A��A��A��A7LA}�A|��AzbAx��Ax(�Aw�wAv��Au;dAr��Ar{Aq�An��Al�!AkXAi|�AhQ�Ag7LAf�Ae�-Ac%A]\)A[p�AZ�AZbAY\)AXZAWS�AU�AR{AO�AL��AI�AG�#AG33AE��AB�+AA%A@�A?K�A=�A;�A:ZA9�A8I�A7��A7hsA7�A6��A6�A5�FA5p�A4ĜA2-A.M�A+�
A*^5A)�A(��A'dZA%��A%K�A$��A"��A!��A!x�A I�A�A�A�mAƨA\)A��A��AdZA?}A�A�`A��A��A�9Al�A�9AA�A�A�9AĜAAM�AAƨAƨA��A|�A�DA5?A�;AhsAĜA��A
�jA
�A	�^A	�hA��AVA�#A�;A��A�FAt�AO�Ar�AjA�A ��@���@��@�b@�x�@��y@���@�K�@��@�@�1@�!@�(�@���@�%@�Z@�1'@��
@�S�@�R@�=q@��T@��@�9X@◍@�M�@�{@��T@�@�p�@�z�@ݺ^@�t�@�n�@ف@�j@ם�@��H@�E�@���@�A�@��@�ff@�x�@�@��@�O�@ʟ�@�$�@ɲ-@�A�@ǝ�@Ǖ�@��@�E�@ř�@�%@ģ�@��;@�~�@��-@�Z@�|�@�K�@�
=@�ȴ@��!@���@�~�@�n�@�n�@�^5@�M�@�M�@�M�@�5?@�@�p�@�z�@��w@�+@���@���@�`B@��j@�j@�1'@��m@���@���@�~�@�E�@�E�@�M�@�E�@�$�@���@�X@���@��j@�bN@�  @���@�C�@�
=@��@�ff@�b@�o@�V@���@��^@��y@���@�M�@�j@� �@��!@��-@��7@��^@��@���@� �@��F@�\)@��@���@�@�V@�Ĝ@�dZ@�33@�
=@�ȴ@���@�33@�M�@�E�@�-@�@��h@�1'@���@��9@��9@���@�r�@�Z@�r�@��@��j@��@���@���@�V@��@�?}@���@��@���@���@�x�@��@��w@���@�hs@���@��/@��@��9@���@��D@�Q�@��@�C�@��@�~�@�n�@�M�@���@���@�hs@�Ĝ@�z�@�j@�A�@�1'@���@��@��@�n�@��@�|�@��!@�/@�Z@���@�t�@�C�@��+@��h@��@�I�@�b@� �@�A�@�Z@��D@��D@�1@��P@�;d@��y@��!@�~�@�^5@�M�@�5?@��#@���@��-@���@���@��h@��@�x�@�p�@�hs@�`B@�G�@��@��`@���@��u@�r�@�Q�@�Q�@�I�@�(�@�  @��@��@��m@��m@��m@��;@���@��@�t�@�S�@�33@�ff@���@�hs@�7L@���@���@�;d@��@���@�M�@��-@�7L@���@��9@�A�@��;@���@�S�@��@���@�{@���@��7@�`B@���@���@��u@�z�@�bN@�I�@� �@��F@�l�@�S�@�;d@��@���@���@�n�@�5?@��-@��/@��@�Q�@���@�C�@�o@�ȴ@��@�x�@�%@�z�@�1'@�(�@� �@�1@���@��m@��;@��
@���@��F@�S�@�@��y@��H@���@��\@��^@�x�@�`B@���@���@�Z@��@�  @�P@;d@~V@}�-@|��@|��@|z�@|j@|j@|Z@|�@{�@{dZ@{C�@z��@z=q@z�@y�^@xQ�@w�w@wK�@w
=@v�R@v5?@u@u`B@t�@tz�@tj@tZ@s��@r~�@r^5@r-@q��@q�7@qX@p��@pĜ@o�P@nȴ@n��@nv�@n@m�T@m�-@l��@k�
@kƨ@kƨ@k��@k��@kt�@kdZ@k33@k@j=q@i��@iX@hA�@gl�@f5?@e@e�-@e�h@e/@d�@d��@d(�@c��@ct�@cdZ@co@b��@b~�@bM�@b�@a�7@a&�@`�`@`�u@`�@`b@^�y@^��@^��@^��@^V@]�T@]?}@\��@\�@\��@\j@\9X@\�@[ƨ@[�F@[��@[�@[S�@[C�@[33@["�@[@Z�@Z�H@Zn�@Y�#@Y�7@Y7L@X�9@XbN@X �@W\)@W
=@V�y@Vȴ@V��@VV@U�-@U�@T�/@T�j@Tz�@TZ@T1@Sƨ@S��@SdZ@R�\@Q�@QG�@P�`@P��@P��@P�9@P�@P�@P �@O��@Ol�@Nȴ@Nv�@N5?@L��@K�
@K�F@K��@K��@K��@K�@Kt�@KC�@K33@K"�@K@J�@J��@J�\@JM�@Ix�@I�@H�9@H��@H�u@Hr�@HA�@H  @H  @G�w@G�P@F�@FE�@F{@E�-@E?}@D�D@C�
@C��@CC�@C@B�!@B^5@A��@AX@@r�@@b@?�;@?��@?l�@>�y@>ȴ@>��@>��@>�+@>v�@>�+@>ff@>$�@=�@=��@=p�@<�@<z�@<Z@<9X@<(�@<1@;ƨ@;��@;��@;�@;C�@;o@:��@:^5@9��@9�@9��@9��@9x�@9X@9�@8��@8�9@8bN@8b@7��@7l�@7
=@6��@6ff@6E�@65?@65?@6$�@5�T@5@5��@5�@5V@4��@4�/@4��@4�j@4�j@4�@4�@4��@4�D@4I�@4(�@4�@41@3t�@2��@1�@1G�@0�9@0bN@0Q�@0 �@0 �@/��@/�P@/\)@/\)@/\)@/\)@/+@/�@.��@.v�@.{@-��@-�-@-�h@-p�@-V@,�D@,�@+�m@+�F@+��@+�@+�@+dZ@+C�@*~�@)x�@)%@(�`@(Ĝ@(�u@'�;@'K�@'
=@%�@%/@$�/@$�j@$�D@$z�@$z�@$Z@$Z@$Z@$I�@#ƨ@#�@#S�@#33@#33@#"�@#o@#@#@#@"�@"�@"�H@"��@"�!@"-@!�#@!�7@ ��@ ��@ r�@ A�@  �@ b@��@l�@��@��@ff@E�@5?@5?@@O�@V@�@9X@�@�
@��@�@t�@t�@t�@S�@"�@"�@o@@o@o@@@@@�H@^5@-@�@��@�@��@�@Ĝ@�u@r�@Q�@ �@�@�w@�@ȴ@�+@V@{@�@�@�T@��@��@�-@�h@O�@�j@�@j@Z@1@�m@�
@�@�!@^5@=q@J@G�@�@�`@Ĝ@��@��@�u@�@�@r�@r�@�@bN@��@K�@��@ff@V@V@E�@5?@{@�T@�T@�T@@�-@O�@��@�@��@9X@�
@��@t�@S�@@
�@
�@
�@
��@
�!@
��@
~�@
n�A�"�A��A��A��A�bA�1A�
=A�oA�
=A�A�A��A���A�Aة�AجAة�Aؗ�A؃A�~�A؁A�~�A�|�A�r�A�hsA�XA�XA�O�A�VA�C�A�G�A�A�A�G�A�?}A�C�A�=qA�A�A�7LA�;dA�1'A�5?A�1'A�5?A�/A�33A�-A�-A�+A�-A�-A�-A�+A�+A�-A�(�A�/A�(�A�+A�&�A�-A�&�A�+A�"�A�(�A�$�A�+A�"�A�+A�"�A�&�A�"�A�&�A� �A�&�A� �A�&�A� �A�&�A� �A�$�A�"�A�$�A�"�A�$�A�"�A�"�A�"�A�$�A� �A�$�A�"�A�"�A�"�A�"�A�"�A� �A� �A� �A� �A��A� �A��A�"�A��A�"�A��A� �A��A� �A��A� �A��A��A��A� �A��A� �A��A� �A��A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A�bA�A���A��A��A��mA��mA��HA��/A���A���A�ƨA�ƨA׾wA׾wA׶FA״9A׬A׬Aן�A׉7AׅA�|�A�dZA�bNA�ffA�\)A�XA�M�A�O�A�;dA�7LA�/A�-A� �A��A���A���A���A���A��A��#Aֺ^A֬A�v�A�VA�+A�$�A�oA�{A�
=A�
=A��A���Aէ�A՟�A՛�AՓuAՋDA�~�A�z�A�t�A�n�A�\)A�Q�A�9XA�(�A�JA�A��A��A��yA��A��yA��mA��A��mA��yA��TA��yA��HA��mA��HA��mA��HA��`A��HA��TA��/A��HA��;A��;A��
A���A��A���A���A���A���A���A���A�ȴA���A���A���A���A���A�ƨA�ƨA�ƨAԾwAԺ^AԴ9AԴ9Aԣ�Aԣ�Aԕ�Aԉ7A�z�A�jA�XA�+A��A��AѶFA�G�A��A��TAЋDA�"�A��A��HA��;A��A϶FA�p�A�%A�bNA���A�`BA�+ȂhA�33A�I�A�M�A�33A�"�A�(�A�"�A��A�%A��`A���Aɧ�A�~�A�`BA�(�A�jAơ�AŲ-A�&�A���A�|�A�bNA�7LA��A��A�Aé�AÑhA�z�A�\)A�M�A�G�A�1'A��A��A��
A�ĜAA�G�A��A�bA��mA�ĜA���A�jA�(�A�A�"�A��A���A��#A���A�~�A�1'A���A��A���A�t�A�E�A�VA���A��+A�K�A�ȴA��A���A�ĜA��!A���A�r�A�A�A� �A��A�
=A��A���A�A���A�/A��jA�v�A�^5A�Q�A�G�A�1'A�+A��A�1A���A��;A���A��hA�ffA���A��-A�t�A���A�7LA��A�1A���A��/A�ĜA���A�t�A�r�A�jA�XA�XA�M�A�/A�
=A��/A��A�|�A�ZA�/A�&�A��A��A��A�{A��A�oA�{A�VA�
=A�
=A�A�%A�  A�A���A��
A��wA���A��\A��A���A��hA�hsA�E�A�JA��yA��;A���A�E�A��mA�;dA�
=A��A�ffA�1'A��mA�|�A��A���A��yA���A��RA�A��9A��9A��-A��!A��A���A���A���A���A���A���A���A���A���A���A���A��PA��\A��7A�bNA� �A�ĜA��wA��A��A��A�9XA�A�r�A�K�A�%A��PA�&�A��A��`A�ȴA��9A���A��7A�v�A�jA�hsA�\)A�\)A�VA�O�A�M�A�E�A�C�A�/A�"�A�"�A��A�oA��A��;A��
A���A�ZA�JA��RA�z�A�^5A�"�A�ĜA��DA�z�A�v�A�hsA�S�A�Q�A�G�A�G�A�+A�oA�oA�A���A��yA���A�A��A���A���A��DA�dZA�K�A�
=A��-A��A�I�A�7LA� �A��A���A�M�A�bA���A�|�A�ZA�C�A� �A�{A�VA�{A�{A�bA�bA�  A���A��TA���A��A��PA�A�A�bA���A��7A��A��A�~�A�r�A�/A���A��TA���A��RA���A���A�|�A�l�A�\)A�`BA�dZA�^5A�ZA�C�A�/A���A���A��A��yA��mA��HA��TA��HA��A��#A���A�ȴA��9A�x�A�;dA�VA��yA��A�G�A��TA��+A�r�A�ffA�bNA�`BA�S�A�=qA�+A��A��A�VA�A���A��`A�ƨA�v�A���A�ĜA��!A��DA�l�A�^5A�VA�M�A�K�A�$�A��A��A�hsA�O�A�&�A��A��mA��^A�bA���A��#A�l�A�I�A�1'A��A��+A�A�A�$�A��A��A��wA��uA��PA��hA��PA��A�z�A�x�A�l�A�n�A�hsA�l�A�l�A�hsA�jA�jA�ffA�dZA�hsA�`BA�`BA�`BA�XA�C�A�E�A�?}A�;dA�5?A�/A��A���A���A���A���A�K�A�A��yA���A�ĜA���A���A��7A�r�A�I�A�"�A�
=A���A��`A�ƨA���A��DA��7A��7A��A��A�x�A�hsA�bNA�bNA�^5A�O�A�M�A�I�A�9XA�33A�&�A��RA�1'A���A��A�?}A���A��!A��A�ZA�O�A�G�A�5?A�$�A��A�{A�oA�A��A��A��mA��TA��TA��HA��;A��
A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                A��A�VA���AظRAؕ�A�~�A�ffA�Q�A�E�A�?}A�9XA�33A�-A�+A�(�A�$�A�"�A�"�A�"�A� �A��A��A��A��A��A��A��A��A���A���A״9A�v�A�C�A�
=A֙�A�1AՕ�A�ZA���A��mA��TA��;A���A���AԾwAԁA�K�A�1A�{A���A��TAǕ�A�\)AËDA�oA�33A�A��FA�n�A�  A���A��A�G�A�9XA���A�jA��A�^5A�jA�  A�=qA���A��A��+A�
=A��wA�A�A�v�A���A���A�&�A�bNA��A�hsA�33A�t�A�  A���A��A�"�A��A���A�bA�x�A��DA�jA�^5A��A�VA�=qA��uA�XA�-A�^5A���A��
A�p�A��uA�I�A�O�A���A���A�33A��A��A��A7LA}�A|��AzbAx��Ax(�Aw�wAv��Au;dAr��Ar{Aq�An��Al�!AkXAi|�AhQ�Ag7LAf�Ae�-Ac%A]\)A[p�AZ�AZbAY\)AXZAWS�AU�AR{AO�AL��AI�AG�#AG33AE��AB�+AA%A@�A?K�A=�A;�A:ZA9�A8I�A7��A7hsA7�A6��A6�A5�FA5p�A4ĜA2-A.M�A+�
A*^5A)�A(��A'dZA%��A%K�A$��A"��A!��A!x�A I�A�A�A�mAƨA\)A��A��AdZA?}A�A�`A��A��A�9Al�A�9AA�A�A�9AĜAAM�AAƨAƨA��A|�A�DA5?A�;AhsAĜA��A
�jA
�A	�^A	�hA��AVA�#A�;A��A�FAt�AO�Ar�AjA�A ��@���@��@�b@�x�@��y@���@�K�@��@�@�1@�!@�(�@���@�%@�Z@�1'@��
@�S�@�R@�=q@��T@��@�9X@◍@�M�@�{@��T@�@�p�@�z�@ݺ^@�t�@�n�@ف@�j@ם�@��H@�E�@���@�A�@��@�ff@�x�@�@��@�O�@ʟ�@�$�@ɲ-@�A�@ǝ�@Ǖ�@��@�E�@ř�@�%@ģ�@��;@�~�@��-@�Z@�|�@�K�@�
=@�ȴ@��!@���@�~�@�n�@�n�@�^5@�M�@�M�@�M�@�5?@�@�p�@�z�@��w@�+@���@���@�`B@��j@�j@�1'@��m@���@���@�~�@�E�@�E�@�M�@�E�@�$�@���@�X@���@��j@�bN@�  @���@�C�@�
=@��@�ff@�b@�o@�V@���@��^@��y@���@�M�@�j@� �@��!@��-@��7@��^@��@���@� �@��F@�\)@��@���@�@�V@�Ĝ@�dZ@�33@�
=@�ȴ@���@�33@�M�@�E�@�-@�@��h@�1'@���@��9@��9@���@�r�@�Z@�r�@��@��j@��@���@���@�V@��@�?}@���@��@���@���@�x�@��@��w@���@�hs@���@��/@��@��9@���@��D@�Q�@��@�C�@��@�~�@�n�@�M�@���@���@�hs@�Ĝ@�z�@�j@�A�@�1'@���@��@��@�n�@��@�|�@��!@�/@�Z@���@�t�@�C�@��+@��h@��@�I�@�b@� �@�A�@�Z@��D@��D@�1@��P@�;d@��y@��!@�~�@�^5@�M�@�5?@��#@���@��-@���@���@��h@��@�x�@�p�@�hs@�`B@�G�@��@��`@���@��u@�r�@�Q�@�Q�@�I�@�(�@�  @��@��@��m@��m@��m@��;@���@��@�t�@�S�@�33@�ff@���@�hs@�7L@���@���@�;d@��@���@�M�@��-@�7L@���@��9@�A�@��;@���@�S�@��@���@�{@���@��7@�`B@���@���@��u@�z�@�bN@�I�@� �@��F@�l�@�S�@�;d@��@���@���@�n�@�5?@��-@��/@��@�Q�@���@�C�@�o@�ȴ@��@�x�@�%@�z�@�1'@�(�@� �@�1@���@��m@��;@��
@���@��F@�S�@�@��y@��H@���@��\@��^@�x�@�`B@���@���@�Z@��@�  @�P@;d@~V@}�-@|��@|��@|z�@|j@|j@|Z@|�@{�@{dZ@{C�@z��@z=q@z�@y�^@xQ�@w�w@wK�@w
=@v�R@v5?@u@u`B@t�@tz�@tj@tZ@s��@r~�@r^5@r-@q��@q�7@qX@p��@pĜ@o�P@nȴ@n��@nv�@n@m�T@m�-@l��@k�
@kƨ@kƨ@k��@k��@kt�@kdZ@k33@k@j=q@i��@iX@hA�@gl�@f5?@e@e�-@e�h@e/@d�@d��@d(�@c��@ct�@cdZ@co@b��@b~�@bM�@b�@a�7@a&�@`�`@`�u@`�@`b@^�y@^��@^��@^��@^V@]�T@]?}@\��@\�@\��@\j@\9X@\�@[ƨ@[�F@[��@[�@[S�@[C�@[33@["�@[@Z�@Z�H@Zn�@Y�#@Y�7@Y7L@X�9@XbN@X �@W\)@W
=@V�y@Vȴ@V��@VV@U�-@U�@T�/@T�j@Tz�@TZ@T1@Sƨ@S��@SdZ@R�\@Q�@QG�@P�`@P��@P��@P�9@P�@P�@P �@O��@Ol�@Nȴ@Nv�@N5?@L��@K�
@K�F@K��@K��@K��@K�@Kt�@KC�@K33@K"�@K@J�@J��@J�\@JM�@Ix�@I�@H�9@H��@H�u@Hr�@HA�@H  @H  @G�w@G�P@F�@FE�@F{@E�-@E?}@D�D@C�
@C��@CC�@C@B�!@B^5@A��@AX@@r�@@b@?�;@?��@?l�@>�y@>ȴ@>��@>��@>�+@>v�@>�+@>ff@>$�@=�@=��@=p�@<�@<z�@<Z@<9X@<(�@<1@;ƨ@;��@;��@;�@;C�@;o@:��@:^5@9��@9�@9��@9��@9x�@9X@9�@8��@8�9@8bN@8b@7��@7l�@7
=@6��@6ff@6E�@65?@65?@6$�@5�T@5@5��@5�@5V@4��@4�/@4��@4�j@4�j@4�@4�@4��@4�D@4I�@4(�@4�@41@3t�@2��@1�@1G�@0�9@0bN@0Q�@0 �@0 �@/��@/�P@/\)@/\)@/\)@/\)@/+@/�@.��@.v�@.{@-��@-�-@-�h@-p�@-V@,�D@,�@+�m@+�F@+��@+�@+�@+dZ@+C�@*~�@)x�@)%@(�`@(Ĝ@(�u@'�;@'K�@'
=@%�@%/@$�/@$�j@$�D@$z�@$z�@$Z@$Z@$Z@$I�@#ƨ@#�@#S�@#33@#33@#"�@#o@#@#@#@"�@"�@"�H@"��@"�!@"-@!�#@!�7@ ��@ ��@ r�@ A�@  �@ b@��@l�@��@��@ff@E�@5?@5?@@O�@V@�@9X@�@�
@��@�@t�@t�@t�@S�@"�@"�@o@@o@o@@@@@�H@^5@-@�@��@�@��@�@Ĝ@�u@r�@Q�@ �@�@�w@�@ȴ@�+@V@{@�@�@�T@��@��@�-@�h@O�@�j@�@j@Z@1@�m@�
@�@�!@^5@=q@J@G�@�@�`@Ĝ@��@��@�u@�@�@r�@r�@�@bN@��@K�@��@ff@V@V@E�@5?@{@�T@�T@�T@@�-@O�@��@�@��@9X@�
@��@t�@S�@@
�@
�@
�@
��@
�!@
��@
~�G�O�A�"�A��A��A��A�bA�1A�
=A�oA�
=A�A�A��A���A�Aة�AجAة�Aؗ�A؃A�~�A؁A�~�A�|�A�r�A�hsA�XA�XA�O�A�VA�C�A�G�A�A�A�G�A�?}A�C�A�=qA�A�A�7LA�;dA�1'A�5?A�1'A�5?A�/A�33A�-A�-A�+A�-A�-A�-A�+A�+A�-A�(�A�/A�(�A�+A�&�A�-A�&�A�+A�"�A�(�A�$�A�+A�"�A�+A�"�A�&�A�"�A�&�A� �A�&�A� �A�&�A� �A�&�A� �A�$�A�"�A�$�A�"�A�$�A�"�A�"�A�"�A�$�A� �A�$�A�"�A�"�A�"�A�"�A�"�A� �A� �A� �A� �A��A� �A��A�"�A��A�"�A��A� �A��A� �A��A� �A��A��A��A� �A��A� �A��A� �A��A� �A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A��A�{A�bA�A���A��A��A��mA��mA��HA��/A���A���A�ƨA�ƨA׾wA׾wA׶FA״9A׬A׬Aן�A׉7AׅA�|�A�dZA�bNA�ffA�\)A�XA�M�A�O�A�;dA�7LA�/A�-A� �A��A���A���A���A���A��A��#Aֺ^A֬A�v�A�VA�+A�$�A�oA�{A�
=A�
=A��A���Aէ�A՟�A՛�AՓuAՋDA�~�A�z�A�t�A�n�A�\)A�Q�A�9XA�(�A�JA�A��A��A��yA��A��yA��mA��A��mA��yA��TA��yA��HA��mA��HA��mA��HA��`A��HA��TA��/A��HA��;A��;A��
A���A��A���A���A���A���A���A���A�ȴA���A���A���A���A���A�ƨA�ƨA�ƨAԾwAԺ^AԴ9AԴ9Aԣ�Aԣ�Aԕ�Aԉ7A�z�A�jA�XA�+A��A��AѶFA�G�A��A��TAЋDA�"�A��A��HA��;A��A϶FA�p�A�%A�bNA���A�`BA�+ȂhA�33A�I�A�M�A�33A�"�A�(�A�"�A��A�%A��`A���Aɧ�A�~�A�`BA�(�A�jAơ�AŲ-A�&�A���A�|�A�bNA�7LA��A��A�Aé�AÑhA�z�A�\)A�M�A�G�A�1'A��A��A��
A�ĜAA�G�A��A�bA��mA�ĜA���A�jA�(�A�A�"�A��A���A��#A���A�~�A�1'A���A��A���A�t�A�E�A�VA���A��+A�K�A�ȴA��A���A�ĜA��!A���A�r�A�A�A� �A��A�
=A��A���A�A���A�/A��jA�v�A�^5A�Q�A�G�A�1'A�+A��A�1A���A��;A���A��hA�ffA���A��-A�t�A���A�7LA��A�1A���A��/A�ĜA���A�t�A�r�A�jA�XA�XA�M�A�/A�
=A��/A��A�|�A�ZA�/A�&�A��A��A��A�{A��A�oA�{A�VA�
=A�
=A�A�%A�  A�A���A��
A��wA���A��\A��A���A��hA�hsA�E�A�JA��yA��;A���A�E�A��mA�;dA�
=A��A�ffA�1'A��mA�|�A��A���A��yA���A��RA�A��9A��9A��-A��!A��A���A���A���A���A���A���A���A���A���A���A���A��PA��\A��7A�bNA� �A�ĜA��wA��A��A��A�9XA�A�r�A�K�A�%A��PA�&�A��A��`A�ȴA��9A���A��7A�v�A�jA�hsA�\)A�\)A�VA�O�A�M�A�E�A�C�A�/A�"�A�"�A��A�oA��A��;A��
A���A�ZA�JA��RA�z�A�^5A�"�A�ĜA��DA�z�A�v�A�hsA�S�A�Q�A�G�A�G�A�+A�oA�oA�A���A��yA���A�A��A���A���A��DA�dZA�K�A�
=A��-A��A�I�A�7LA� �A��A���A�M�A�bA���A�|�A�ZA�C�A� �A�{A�VA�{A�{A�bA�bA�  A���A��TA���A��A��PA�A�A�bA���A��7A��A��A�~�A�r�A�/A���A��TA���A��RA���A���A�|�A�l�A�\)A�`BA�dZA�^5A�ZA�C�A�/A���A���A��A��yA��mA��HA��TA��HA��A��#A���A�ȴA��9A�x�A�;dA�VA��yA��A�G�A��TA��+A�r�A�ffA�bNA�`BA�S�A�=qA�+A��A��A�VA�A���A��`A�ƨA�v�A���A�ĜA��!A��DA�l�A�^5A�VA�M�A�K�A�$�A��A��A�hsA�O�A�&�A��A��mA��^A�bA���A��#A�l�A�I�A�1'A��A��+A�A�A�$�A��A��A��wA��uA��PA��hA��PA��A�z�A�x�A�l�A�n�A�hsA�l�A�l�A�hsA�jA�jA�ffA�dZA�hsA�`BA�`BA�`BA�XA�C�A�E�A�?}A�;dA�5?A�/A��A���A���A���A���A�K�A�A��yA���A�ĜA���A���A��7A�r�A�I�A�"�A�
=A���A��`A�ƨA���A��DA��7A��7A��A��A�x�A�hsA�bNA�bNA�^5A�O�A�M�A�I�A�9XA�33A�&�A��RA�1'A���A��A�?}A���A��!A��A�ZA�O�A�G�A�5?A�$�A��A�{A�oA�A��A��A��mA��TA��TA��HA��;A��
A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B�wB�B��B��B��B�_B�eB��B��B�*B��B��B�_B�*B�*B��B�_B��B�*B��B��B��B��B�0B�0B�0B�0B��B�6B�6B��B��B��B��B��B�0B��B��B�TB�[B��B��B�?B�B֡B��B��B�B*�B[�BZ�By�BgmBR BF?B:*B4nB4B0!B;�B4nB0�BC�B3�B7�BGB;�B5�B5tB49B.}B!�B�BuB�BB��B��B�B��BĜB�XB��B��B�B�1B��B��B��B�GBzxBm)B\]B7�B$@B �BVB�BbBSB�>B�AB�yB�B��B�$B�LB��B�IB��B��B��Bz�Ba�BUgBK^B9$B2�B*�B#:B�BuB�B�B
��B
��B
�B
��B
��B
�B
ɺB
��B
��B
��B
��B
��B
�+B
v`B
g�B
a�B
[�B
ZB
WsB
RTB
H�B
6B
)_B
�B
.B
�B	��B	�JB	�ZB	�B	�B	�vB	ٴB	ԕB	�jB	�XB	��B	�-B	� B	�HB	��B	��B	�B	��B	��B	�*B	��B	��B	�PB	��B	��B	�_B	�{B	~]B	{JB	~]B	u�B	t�B	v�B	t�B	tB	s�B	s�B	tTB	v�B	w2B	w2B	w�B	y	B	x�B	v�B	s�B	}�B	�~B	�SB	��B	��B	��B	�kB	�CB	��B	�kB	��B	�eB	��B	�eB	��B	��B	��B	��B	��B	��B	��B	�MB	�;B	�4B	�4B	�B	� B	~�B	��B	�B	��B	��B	��B	�7B	��B	�PB	��B	��B	}VB	yrB	n�B	i�B	e�B	bB	_;B	]�B	aHB	d�B	`B	`B	^�B	]�B	_;B	]dB	]�B	^5B	^5B	c�B	lWB	t�B	s�B	tTB	tTB	sB	rB	u%B	|�B	|�B	{�B	|�B	{JB	|�B	}�B	~�B	��B	�4B	�"B	��B	��B	��B	��B	�\B	��B	�rB	��B	��B	�\B	�\B	��B	��B	��B	�+B	��B	��B	��B	��B	�B	��B	��B	��B	��B	��B	�?B	�tB	�?B	��B	�tB	�?B	�?B	��B	��B	�nB	�B	��B	��B	�0B	��B	��B	��B	��B	�9B	��B	ǮB	ȴB	ȴB	�B	�B	�BB	�BB	�B	ѷB	��B	҉B	уB	�NB	уB	ѷB	�TB	��B	��B	�mB	��B	�B	��B	�B	��B	��B
�B
FB
!bB
�B
~B
"hB
�B
B
�B
+6B
4B
?HB
CaB
A�B
FB
CaB
@�B
;�B
;�B
7�B
7�B
7�B
7LB
7�B
:�B
@�B
A B
@�B
?�B
?B
?}B
CaB
C�B
C�B
EmB
H�B
K^B
OB
Q�B
S�B
W�B
X�B
YB
YKB
Y�B
Z�B
\]B
^�B
aB
`B
_�B
j�B
j�B
jB
hsB
f�B
ffB
f�B
f�B
f�B
f�B
g8B
d�B
c�B
b�B
aHB
aB
`�B
_B
^jB
^�B
`BB
a�B
c�B
iDB
p�B
w�B
�B
��B
�FB
�@B
�FB
�tB
��B
��B
�!B
�VB
�-B
��B
�VB
��B
��B
��B
�-B
��B
�4B
��B
�FB
��B
��B
��B
��B
�_B
�eB
�6B
��B
�B
��B
�qB
�B
�CB
�B
�CB
�wB
��B
��B
��B
�CB
�wB
�B
�}B
��B
�B
��B
�'B
�'B
��B
�3B
�B
��B
��B
��B
��B
�B
�tB
�B
��B
�XB
�*B
�*B
��B
��B
�B
��B
��B
�B
��B
��B
�3B
�B
�EB
ȴB
�B
��B
��B
��B
��B
ΥB
�BB
�NB
�[B
�aB
�aB
�2B
�mB
�
B
רB
��B
��B
�EB
خB
�B
ںB
چB
چB
��B
یB
�WB
��B
�]B
�5B
�jB
��B
ޞB
�B
��B
�B
�B
��B
�&B
�ZB
��B
��B
��B
��B
�B
�fB
��B
�B
�B
�B
��B
��B
�>B
�>B
�>B
�
B
�sB
��B
��B
�B
�B
�B
�"B
�WB
�B
�)B
�]B
� B
�iB
��B
�oB
�oB
�oB
�oB
�oB
��B
�B
�AB
�B
�|B
�|B
�GB
�B
�TB
��B
��B
��B
��B
��B
��B
�2B
��B
��B
��B
�fB
��B
��B
�rB
��B
��B
�xB
�DB
��B
��B
�"B
�VB
�VB
��B
��B
�(B
�"B
��B
�.B
�.B
�.B
��B
�.B
�.B
�.B
��B
��B  B
��B iB;B�B{B{BGB{B�BBMBSB�B�B�BYB�B�B�B+B�B�B�B1B�B	lB
	B
	B	�B	�B
	B
�B�B�B�B�B�B�B�B"BVB�B�B�B�B�B�B�B�B"B(B(B�B�BbB�B.B�B.B.B�B�B�B�B�B\B\B�B�B4B�B4BhB�B:BoBoBoB:BoBoBB�B�B�BuBB�B�BSBBSBSBBSBSB�BSBSB�BSBSB�B�B_B�B+B+B�B�B+B+B_B_B+B7BeB�B7B�B	B�B�BBCBCBxBB~BB�B�BOB�B�B!B!BVBVBVBVBVB�B�B�B \B!bB!-B!bB!�B!�B!�B"hB"hB"hB"hB"�B"�B"�B#:B#nB#nB#nB#nB#�B#�B$B#�B$B$@B$tB$�B$�B%zB%�B&LB&�B&LB&LB&�B&�B&�B&�B&�B'�B'RB'�B'�B'�B'�B'�B($B($B(XB(�B(�B(XB($B)*B)�B*0B+B+kB,=B,B,B+�B,qB,�B,�B,�B,�B,qB,�B,�B,�B-wB-�B-�B.B.B.IB.}B/B/�B/�B/�B0!B0!B0!B0UB0UB2-B2�B33B3�B3hB3�B4�B5?B5B6�B7�B7�B8B8RB8RB8RB8�B8�B8RB8�B9XB9XB9�B9�B9�B9�B9�B9�B9�B9�B9�B9�B9�B9�B9�B:^B:^B:�B;dB;0B;0B;0B;dB;dB;dB<6B<�B=B=<B=<B=qB=qB=�B>�B>wB?B?}B?}B?�B@B@OB@OB@OB@OB@�B@�B@�B@�B@�B@�B@�B@�B@�B@�B@�B@�BA�BA�BA�BA�BA�BA�BB�BB�BB�BC-BCaBC�BCaBC�BDgBD�BD�BD�BEBE9BE9BE9BE9BEBE9BE9BEmBFtBF?BF�BFtBGBF�BF�BGzBH�BHKBH�BH�BI�BI�BJXBJXBJ�BJXBJXBJ�BJ�BJXBJXBJ#BI�BK)BJ�BL0BL0BL0BL0BL0BL0BLdBL�BLdBLdBLdBL0BM6BMjBM6BM�BNpBN�BN�BN�BOBBOvBOvBO�BO�BO�BO�BPHBPHBP�B�B��B�B�=B��B��B��B��B�=B��B��B�nB��B�wB�IB��B��B��B�B��B�_B�_B�$B�kB�$B�0B��B��B�$B�=B��B��B��B��B�$B��B�$B�0B��B�B��B��B�$B��B��B��B�_B��B�XB�_B��B�_B�_B�$B��B��B�*B��B��B��B�0B�$B��B�$B��B�XB��B��B��B��B�eB��B��B��B�eB��B�0B��B�eB��B�0B��B��B��B�_B��B�_B��B��B��B��B�_B��B�_B�_B��B�_B��B��B�0B��B��B��B��B��B�kB��B�B��B�6B��B��B��B�kB��B�kB�*B�B��B�kB��B�B�*B�B�*B�6B��B�6B�*B��B��B�6B��B��B��B�eB��B��B��B�eB�_B�B�_B�kB�*B��B��B��B��B��B�_B��B��B��B�B��B��B�6B�qB��B��B�B�wB�eB�B�0B��B�*B�B�$B��B��B�eB��B��B�$B��B��B��B��B�eB�XB�RB�B�XB��B��B��B�$B��B��B�CB�XB��B�=B�UB�}B��B�tB�jB�B�0B��B�0B��B��B��BŢBĜB�mBƨB�BɺB�KB��B��B�jB��B�B�B��B�B��B�B��B� B�&B�&B�TB��B҉B�2B��B�gB�&B�BӏB��B�,B՛B�,B�mB��B�9B��B֡B�9BԕB��B�9B�EB�9BخB��BٴB��B�EB��B��BخB�EBרB�9B�
B�2B��B��B��BуB�mB��B�?B�gBרB�B��B�BB��B� B��B�8B
=B�"BoB��B+B�BSB&B*eB.}B#:B49BN�B@�Bs�BhsBYBYKBW�BX�BV9B\�B_B[�B^jBZB[#Bb�B{�B�eB�B|PBn�BhsBe,Bc B]�B]�BXBR�BQ�BP�BMBI�BGEBH�BH�BC�BA�B?�BD�B9�B7�B7�B;0B1[B0�B2�B.}B5�BCaB8�B<�B-�B.�B-wB6B2�B.�B)�B*eB2�B5?B4B9�B2�BA�BC�B8B0UB3�B3�B4nB<�B.IB-�B+6B-wB:�BHKBIB=qB>B49B3�B3hB2�B4�B2�B5tB4B2�B:^B9XB7LB?�BF�B@BA�Bc�BB'B@OB9�B=qB;�B7LB?}B7LB33B5�B7�B2�B5�B4nB?�B;dB6B2�B3�B49B2aB2�B4nB5?B7B4nB6zB4nB6B6B33B5?B1�B49B1[B2�B6B2aB2�B4B?B3�B.B+B+kB+�B'RB#B+6B*0B/�B,B)*B)�B$@B �B'�B%B&�B�B�B�BSB�BSB�B�BBFB�B�B�B{BFB�B{B�B@BhBB�B(B(B+BIB�B
rB�BDB�BBkBYB	�B�B4B�B �B�BoB�B�B�BB��B�]B iB�]B�(B��B�"B��B��B��B��B��B�PB��B��B�rB��B;B��B�%B�fB�B��B�]B�>BרB��B�TB�[B�TB�B�B�B�&B̘B��B�dB��B��B�RB�dBƨB��B��BÖB�?B�OB�^B��B�OB��B��B��B�B�XB��B�nB�jB�^B�!B��B��B��B��B�B��B��B�zB��B�^B�B��B��B�XB��B��B��B�tB��B�~B�VB��B�RB�~B�=B��B�_B��B�$B�7B�{B�7B��B�kB��B�OB��B��B��B��B��B�OB�B��B�xB�qB�OB�=B��B�B�qB��B�VB�1B��B��B�B�CB��B�fB��B�YB��B��B��B�{B�{B��B��B�uB.B}�B�{B�_B��Bx8Bu%Bt�Bv�BrGBo5Bl�Bl"Bv�BhsBxlBjBc B_pBaHBXBZQBa|BcTB]�BI�B=�B;�BCaBC�B4�B1�B+kB&LB+kB%zB#B!�B#:B$�B \B"4B \B!B!�B�B�B!-B�B!B!�B 'B�B�BB~B�B!�B1BqB�B�B1BqB�BB1BBIB�B~B~B�B
	B	B�BB
�B%B�B��B�B�PB	B��B�	B��B�`B�ZB�TB�TB�GB��B�oB�B�cB��B��B�WB��BB��B�;B��BخBܒB��B�)B�EB��B�'B�gB�B�BB�B��B��B��B��B��B��B��B�B��B�$B�RG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202209061825322022090618253220220906182532202209061825322022090618253220220906182532SI  SI  ARFMARFM                                                                                                                                                2021112502375820211125023758IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021120501011620211205010116QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021120501011620211205010116QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022090607225820220906072258IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022090618253620220906182536IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022090618253620220906182536IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022090618253620220906182536IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                