CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-09-08T07:37:17Z creation; 2022-04-26T16:07:00Z DMQC;      
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
_FillValue        G�O�     H  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T  ZP   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     H  a�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T  ~�   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     H  �@   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     H  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T  ��   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     H  �$   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T  �l   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     H  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     H 
   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T 'P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     H .�   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 T K�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     H S@   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` p�   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   p�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   v�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   |�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �<   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �D   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �L   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �T   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �\   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    �    HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �    HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �(   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �0   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    �8Argo profile    3.1 1.2 19500101000000  20210908073717  20220426232409  5906402 5906402 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO8138_008904_030                 8138_008904_030                 2C  2C  DD  SOLO_II                         SOLO_II                         8904                            8904                            V2.6; SBE602 14Jan20            V2.6; SBE602 14Jan20            853 853 @ّ̄���@ّ̄���11  @̵ّ��?@̵ّ��?@-�=p��
@-�=p��
�d�z$�LD�d�z$�LD11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�=q@�\@@  @�G�@�G�@�G�@�  A ��A  A   A,(�A?\)A_\)A�  A�Q�A�  A��A��AϮA߮A�  A��B  B  B  B   B'�
B0  B8  B@  BH  BP  BX(�B`  Bh  Bp  Bx  B�
B��B��B�  B��B��B�{B�{B�(�B�{B�(�B�(�B�(�B�{B�{B�ffB��B�  B��B��B�  B�  B��B��
B�  B�  B��B��B��B��
B��B�  C   C  C
=C
=C  C
  C  C
=C��C�C��C  C��C  C  C  C��C!��C$
=C%��C'�C)��C,  C.
=C0  C2  C4
=C5��C7��C:
=C<
=C>  C@  CB
=CC��CE�CG��CJ
=CL
=CN  CP  CQ��CS��CV
=CX  CZ  C\  C^  C`
=Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp
=Cr{Ct  Cu��Cx
=Cz  C|  C~
=C�C�C�C�C�  C���C�C�C�  C�  C�  C���C���C�  C�C�  C�  C�  C�  C�C�  C���C���C���C�  C���C���C���C���C�  C�  C�C�  C�C�C�  C�  C�C���C���C�  C�C�  C�  C���C���C���C�  C���C�C�  C�  C�  C�  C�  C���C���C�  C�C�  C�C�  C�  C�  C�C�  C�  C���C���C�C�C�  C�  C�C�C�C�C�C���C���C���C�C�  C���C���C���C���C�  C�  C���C���C�  C�  C���C���C�C�  C�  C�C�  C���C���C�  C�  C�  C�  C�  C�  C�C�
=C�
=C�C�  C�C�C�  C���C���C�  C���C���C���C���C���C�  C�  C�  C�  D   D ��D�D� D  D}qD�qD}qD�qD}qD��D� DD��DD� D�qD� D	�D	� D	�qD
� D�D��D�D� D  D�D  D� D�D}qD  D��D  D}qD��D}qD��D� D  D}qD�qD}qD�qD� DD�DD�D  Dz�D�qD� D  D��D�D� D��Dz�D�qD}qD  D��D D ��D!�D!�D"D"��D#�D#� D$�D$��D%�D%��D&D&�D'D'��D(  D(� D)  D)� D)�qD*}qD*�qD+� D,D,��D-�D-� D.�D.�D/  D/� D0�D0��D1  D1� D2�D2� D2�qD3}qD3�qD4� D5  D5� D6�D6��D7  D7� D8�D8� D9  D9� D:  D:� D:�qD;� D<  D<� D<�qD=}qD>  D>}qD?  D?� D@  D@� DA  DA� DB�DB� DC  DC}qDD  DD� DD�qDE}qDE�qDF� DG  DG� DH  DH� DI  DI� DJ�DJ��DK  DK� DK�qDL}qDL�qDM}qDN  DN}qDN��DO}qDP  DP� DQ  DQ� DR  DR}qDR�qDS� DT  DT� DU�DU� DV  DV��DW  DW}qDX  DX� DY�DY��DZ  DZ� D[�D[� D\  D\� D]  D]� D^  D^� D_�D_��D_�qD`� Da  Da��Db  Db� Dc  Dc��Dc�qDd}qDe�De��De�qDf� Df�qDg}qDg�qDh}qDi�Di��Di�qDj� Dk  Dk��Dl�Dl��Dm  Dm� Dn  Dn}qDo�Do�Dp�Dp��Dq  Dq� Dq�qDr� Ds�Ds��Dt�Dt� Du  Du� Du�qDv}qDw  Dw� Dw�qDx� Dy  Dy� DzDz� Dz�qD{� D|�D|� D|�qD}}qD}�qD~��D~�qD}qD�qD�@ D�� D��HD���D�@ D��HD���D�  D�@ D�~�D�� D�  D�@ D�� D�� D�  D�>�D�~�D��HD�HD�@ D�� D�� D�  D�AHD��HD�� D�HD�B�D��HD�� D�HD�B�D��HD���D���D�AHD���D��HD�HD�AHD��HD��HD���D�=qD�~�D�� D���D�@ D��HD�� D�  D�AHD�� D���D�  D�AHD�� D���D��qD�@ D��HD�� D�  D�>�D�}qD�� D�HD�@ D�� D�� D���D�>�D�~�D���D���D�>�D��HD�D�HD�>�D�~�D�� D�  D�>�D�� D�� D�  D�@ D�� D�� D�HD�>�D�}qD���D���D�@ D��HD�� D���D�@ D�� D���D�  D�B�D��HD���D��qD�=qD�~�D��HD�HD�AHD�� D�� D�HD�B�D�� D���D���D�>�D�~�D�� D�HD�@ D�� D��HD���D�>�D�}qD��qD�  D�@ D�~�D�� D���D�>�D��HD��HD��qD�>�D�~�D���D�  D�=qD�~�D�� D���D�AHD���D�� D�  D�>�D�~�D��qD��qD�=qD�� D�D�HD�AHD�� D��HD�  D�AHD��HD��HD�HD�@ D��HD��HD�HD�AHD��HD�� D�  D�@ D�� D�� D�HD�AHD���D��HD�HD�@ D�� D�� D�  D�@ D��HD�D��D�AHD���D�D�  D�@ D�� D��)D�  D�AHD�� D��HD��D�AHD�� D�� D�  D�>�D�� D�� D���D�@ D�� D���D�  D�@ D�~�D���D�  D�@ D�� D���D�  D�@ D�� D���D���D�@ D��HD��HD��D�AHD��HD��HD�HD�AHD��HD�� D�  D�@ D�� D��HD�  D�>�D�~�D�� D���D�>�D�~�D���D�  D�@ D�� D���D���D�AHD��HD��HD��D�AHD�~�D���D�  D�@ D D�� D�  D�AHDÁHD�� D�  D�@ DĀ D��HD�  D�@ D�~�D�� D��D�B�Dƀ Dƾ�D���D�@ DǁHD��HD�  D�@ D�~�D�� D�  D�>�D�~�D��HD�HD�@ D�~�DʽqD���D�AHDˀ D�� D���D�@ D́HD��HD�  D�AHD́HD�� D���D�@ D΀ D�� D�HD�AHDρHD��HD�HD�B�DЁHD��HD�  D�>�Dр D�� D�  D�@ DҀ D�� D�HD�AHDӀ DӾ�D�  D�>�D�}qD�� D�HD�AHD�~�Dվ�D�  D�@ Dր D�� D�HD�@ D�}qD׾�D�  D�>�D؀ D��HD�HD�AHDفHD��HD�HD�@ Dڀ Dھ�D���D�>�D�~�D۾�D�  D�@ D܀ D�� D���D�>�D�~�D�� D�HD�@ D�~�D޾�D�  D�@ D߁HD��HD�HD�@ D�� D�� D�  D�@ D� D��HD�  D�>�D� D��HD�  D�>�D�~�D�qD�  D�AHD�HD�� D�  D�>�D�HD�� D�  D�AHD�HD��HD�  D�@ D� D羸D�  D�AHD肏D辸?#�
?L��?��?�z�?�{?�p�?�(�?�@�\@��@��@#�
@0��@:�H@G�@Q�@^�R@k�@xQ�@��\@���@�\)@�@�p�@��\@�=q@�\)@�
=@�(�@��
@�=q@��@�
=@�  @��@���@�33@��HA   Az�A
=A(�A\)A33AffA�HAp�A!�A%A(��A,��A0  A4z�A7�A<(�A>�RAC33AFffAJ�HAN{AR�\AUAY��A^{Ab�\Ag
=Aj=qAo\)As33AxQ�A|(�A���A��HA��A��A��A���A��RA���A��
A�ffA���A��A�p�A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ?�=q@�\@@  @�G�@�G�@�G�@�  A ��A  A   A,(�A?\)A_\)A�  A�Q�A�  A��A��AϮA߮A�  A��B  B  B  B   B'�
B0  B8  B@  BH  BP  BX(�B`  Bh  Bp  Bx  B�
B��B��B�  B��B��B�{B�{B�(�B�{B�(�B�(�B�(�B�{B�{B�ffB��B�  B��B��B�  B�  B��B��
B�  B�  B��B��B��B��
B��B�  C   C  C
=C
=C  C
  C  C
=C��C�C��C  C��C  C  C  C��C!��C$
=C%��C'�C)��C,  C.
=C0  C2  C4
=C5��C7��C:
=C<
=C>  C@  CB
=CC��CE�CG��CJ
=CL
=CN  CP  CQ��CS��CV
=CX  CZ  C\  C^  C`
=Cb  Cd  Cf  Ch  Cj  Cl  Cn  Cp
=Cr{Ct  Cu��Cx
=Cz  C|  C~
=C�C�C�C�C�  C���C�C�C�  C�  C�  C���C���C�  C�C�  C�  C�  C�  C�C�  C���C���C���C�  C���C���C���C���C�  C�  C�C�  C�C�C�  C�  C�C���C���C�  C�C�  C�  C���C���C���C�  C���C�C�  C�  C�  C�  C�  C���C���C�  C�C�  C�C�  C�  C�  C�C�  C�  C���C���C�C�C�  C�  C�C�C�C�C�C���C���C���C�C�  C���C���C���C���C�  C�  C���C���C�  C�  C���C���C�C�  C�  C�C�  C���C���C�  C�  C�  C�  C�  C�  C�C�
=C�
=C�C�  C�C�C�  C���C���C�  C���C���C���C���C���C�  C�  C�  C�  D   D ��D�D� D  D}qD�qD}qD�qD}qD��D� DD��DD� D�qD� D	�D	� D	�qD
� D�D��D�D� D  D�D  D� D�D}qD  D��D  D}qD��D}qD��D� D  D}qD�qD}qD�qD� DD�DD�D  Dz�D�qD� D  D��D�D� D��Dz�D�qD}qD  D��D D ��D!�D!�D"D"��D#�D#� D$�D$��D%�D%��D&D&�D'D'��D(  D(� D)  D)� D)�qD*}qD*�qD+� D,D,��D-�D-� D.�D.�D/  D/� D0�D0��D1  D1� D2�D2� D2�qD3}qD3�qD4� D5  D5� D6�D6��D7  D7� D8�D8� D9  D9� D:  D:� D:�qD;� D<  D<� D<�qD=}qD>  D>}qD?  D?� D@  D@� DA  DA� DB�DB� DC  DC}qDD  DD� DD�qDE}qDE�qDF� DG  DG� DH  DH� DI  DI� DJ�DJ��DK  DK� DK�qDL}qDL�qDM}qDN  DN}qDN��DO}qDP  DP� DQ  DQ� DR  DR}qDR�qDS� DT  DT� DU�DU� DV  DV��DW  DW}qDX  DX� DY�DY��DZ  DZ� D[�D[� D\  D\� D]  D]� D^  D^� D_�D_��D_�qD`� Da  Da��Db  Db� Dc  Dc��Dc�qDd}qDe�De��De�qDf� Df�qDg}qDg�qDh}qDi�Di��Di�qDj� Dk  Dk��Dl�Dl��Dm  Dm� Dn  Dn}qDo�Do�Dp�Dp��Dq  Dq� Dq�qDr� Ds�Ds��Dt�Dt� Du  Du� Du�qDv}qDw  Dw� Dw�qDx� Dy  Dy� DzDz� Dz�qD{� D|�D|� D|�qD}}qD}�qD~��D~�qD}qD�qD�@ D�� D��HD���D�@ D��HD���D�  D�@ D�~�D�� D�  D�@ D�� D�� D�  D�>�D�~�D��HD�HD�@ D�� D�� D�  D�AHD��HD�� D�HD�B�D��HD�� D�HD�B�D��HD���D���D�AHD���D��HD�HD�AHD��HD��HD���D�=qD�~�D�� D���D�@ D��HD�� D�  D�AHD�� D���D�  D�AHD�� D���D��qD�@ D��HD�� D�  D�>�D�}qD�� D�HD�@ D�� D�� D���D�>�D�~�D���D���D�>�D��HD�D�HD�>�D�~�D�� D�  D�>�D�� D�� D�  D�@ D�� D�� D�HD�>�D�}qD���D���D�@ D��HD�� D���D�@ D�� D���D�  D�B�D��HD���D��qD�=qD�~�D��HD�HD�AHD�� D�� D�HD�B�D�� D���D���D�>�D�~�D�� D�HD�@ D�� D��HD���D�>�D�}qD��qD�  D�@ D�~�D�� D���D�>�D��HD��HD��qD�>�D�~�D���D�  D�=qD�~�D�� D���D�AHD���D�� D�  D�>�D�~�D��qD��qD�=qD�� D�D�HD�AHD�� D��HD�  D�AHD��HD��HD�HD�@ D��HD��HD�HD�AHD��HD�� D�  D�@ D�� D�� D�HD�AHD���D��HD�HD�@ D�� D�� D�  D�@ D��HD�D��D�AHD���D�D�  D�@ D�� D��)D�  D�AHD�� D��HD��D�AHD�� D�� D�  D�>�D�� D�� D���D�@ D�� D���D�  D�@ D�~�D���D�  D�@ D�� D���D�  D�@ D�� D���D���D�@ D��HD��HD��D�AHD��HD��HD�HD�AHD��HD�� D�  D�@ D�� D��HD�  D�>�D�~�D�� D���D�>�D�~�D���D�  D�@ D�� D���D���D�AHD��HD��HD��D�AHD�~�D���D�  D�@ D D�� D�  D�AHDÁHD�� D�  D�@ DĀ D��HD�  D�@ D�~�D�� D��D�B�Dƀ Dƾ�D���D�@ DǁHD��HD�  D�@ D�~�D�� D�  D�>�D�~�D��HD�HD�@ D�~�DʽqD���D�AHDˀ D�� D���D�@ D́HD��HD�  D�AHD́HD�� D���D�@ D΀ D�� D�HD�AHDρHD��HD�HD�B�DЁHD��HD�  D�>�Dр D�� D�  D�@ DҀ D�� D�HD�AHDӀ DӾ�D�  D�>�D�}qD�� D�HD�AHD�~�Dվ�D�  D�@ Dր D�� D�HD�@ D�}qD׾�D�  D�>�D؀ D��HD�HD�AHDفHD��HD�HD�@ Dڀ Dھ�D���D�>�D�~�D۾�D�  D�@ D܀ D�� D���D�>�D�~�D�� D�HD�@ D�~�D޾�D�  D�@ D߁HD��HD�HD�@ D�� D�� D�  D�@ D� D��HD�  D�>�D� D��HD�  D�>�D�~�D�qD�  D�AHD�HD�� D�  D�>�D�HD�� D�  D�AHD�HD��HD�  D�@ D� D羸D�  D�AHD肏G�O�?#�
?L��?��?�z�?�{?�p�?�(�?�@�\@��@��@#�
@0��@:�H@G�@Q�@^�R@k�@xQ�@��\@���@�\)@�@�p�@��\@�=q@�\)@�
=@�(�@��
@�=q@��@�
=@�  @��@���@�33@��HA   Az�A
=A(�A\)A33AffA�HAp�A!�A%A(��A,��A0  A4z�A7�A<(�A>�RAC33AFffAJ�HAN{AR�\AUAY��A^{Ab�\Ag
=Aj=qAo\)As33AxQ�A|(�A���A��HA��A��A��A���A��RA���A��
A�ffA���A��A�p�A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A���A���A���A�  A�1A���A��A���A��A��;A���A���A��#A�ĜAܼjAܓuA�z�A�~�A�p�A�-A�(�A�$�A�"�A��A��A��A��A��A��A��A�{A�oA�bA�bA�VA�VA�
=A���A���A���A��mA�ƨAۙ�A��`A�Q�Aװ!A�7LA��A�v�A�hsA�=qA�9XA�oA�A�&�Aδ9A͛�Aˣ�A�(�A��A��A���Aƛ�A���A��TAþwA¶FA�v�A��7A�p�A���A�\)A�?}A�\)A�A�7LA�p�A�&�A��7A��A�jA��;A��yA���A�ZA��A��HA�ffA�-A�A�jA���A�33A��A�S�A�t�A���A��A�r�A�n�A��9A�C�A�Q�A�A|�`Aw33Aq�Ao"�Al��Ai�
Af�+Abr�A^��A]�AZ�\AW�AT��AR  AO��AL�jAJQ�AHA�AGl�AFQ�AEC�AD�AC`BABE�AA%A?�FA>v�A=�;A=��A=hsA=�A<��A<M�A:�A8�\A4�`A0�DA.VA-\)A,ȴA,�uA,�A+hsA*�A*�A*�RA*��A*ȴA*��A*�9A+�PA+S�A*�A)��A(��A'�7A%XA$z�A#�7A"�`A"��A"��A"ZA"{A!��A!��A!��A!�-A!�A!�A!ƨA ȴA n�A�jA�A�mA7LA�yAM�AƨA��AhsA+A�/A�AA�AhsA�A�AJA��A�-A��A|�AhsAC�A�A�uA�A�^Al�A1'AS�A��A{A�hAoA��AĜA�+A{A
�A
VA
1A
$�A	�hA��A�A^5AJA  A1A�An�A��Az�AAƨA�A{A�A�AAVAQ�A9XA{A�TAt�AK�A�A
=A��A��A��Az�AI�A9XA1'A1Al�A �HA n�A �A 1@�C�@��
@�@���@�hs@�?}@�V@��j@�1'@���@�C�@�ȴ@��@�J@��h@�?}@���@�I�@�(�@��m@�|�@�@�j@���@��@���@�hs@�?}@�u@�dZ@�@�R@��#@�@�Z@��y@�^5@�E�@�5?@�-@�{@�^@�G�@��@���@��@�r�@�A�@��@�S�@�^5@���@ᙚ@�%@�Q�@ߥ�@�
=@���@ާ�@�5?@���@݁@�`B@��@���@���@�z�@�9X@��@�  @�ƨ@�"�@�o@���@���@���@�O�@��@�1@�=q@�x�@��@Դ9@�Q�@ӝ�@�C�@�
=@�ȴ@�v�@��@ѩ�@�X@�V@У�@��@Ϯ@��@��#@�?}@�%@���@̼j@�j@ˍP@�;d@�o@���@�^5@ɺ^@�hs@Ȭ@�z�@�1'@Ǯ@�"�@�E�@ř�@��/@�  @���@Ý�@���@�^5@��#@���@�bN@��m@�;d@���@�v�@�@��h@�p�@��@��@��@��;@�|�@���@��^@�`B@��`@��P@���@�ff@�J@�hs@�&�@�V@���@�j@�A�@�1'@�b@���@��w@��@�dZ@��H@�v�@��@���@��#@� �@��;@��
@�ƨ@��F@�|�@�33@��@�E�@�@��@���@�j@�bN@�A�@�b@��;@�\)@���@�v�@�v�@�n�@�ff@�-@���@��T@���@�x�@��@�Z@�l�@��R@�{@��#@�7L@�V@��9@�(�@��w@�dZ@�@��@���@�v�@�E�@��@��^@�hs@�7L@��@���@��/@�Ĝ@���@��;@���@���@��P@��@�t�@�;d@���@���@��+@�ff@�M�@�{@���@��7@�/@��@�bN@� �@��F@�dZ@�o@��H@���@�n�@�n�@�^5@��^@�O�@�?}@�/@�%@��@�r�@�Q�@�b@���@��H@�V@�@�@�p�@��D@��;@��w@��F@��P@�S�@�C�@�33@�+@�o@�@��@��H@�ȴ@���@��\@�$�@���@�@��^@���@���@�x�@�?}@���@��@�b@��m@��
@�ƨ@��w@��F@�l�@��H@��+@�{@��@�/@���@��`@��j@�1'@�  @��m@���@��w@�C�@��@��y@�^5@���@��@�&�@���@��`@��j@��u@�z�@�b@��F@�|�@�S�@�33@�o@��!@�^5@�M�@�=q@�{@��7@��@��u@�Z@��m@���@���@���@�t�@�S�@�33@�o@��@���@�~�@�-@��-@�&�@���@���@��9@���@�Q�@�  @�  @+@~�R@~5?@}��@}�@}`B@}?}@}�@|��@|��@|j@|�@{�m@{dZ@zJ@y��@xĜ@xbN@w�@wl�@v�y@v��@vff@vV@v{@u?}@tj@s�m@so@q��@qX@p�`@pbN@o|�@oK�@oK�@o
=@nȴ@n�R@n�+@m�@m�h@m`B@m�@l��@l(�@k��@kS�@j�\@i�^@h�u@hr�@h �@g�@g�@g�P@g
=@f�R@fv�@fff@fE�@f{@e�T@e@e�-@e�h@e�@e`B@e?}@e?}@e?}@eV@d�/@d��@d�j@dz�@dj@dj@dj@d9X@d(�@cƨ@c33@c"�@b�@b�\@a��@a%@`�u@` �@_��@^��@]��@\��@\��@\(�@[�F@Z�H@Y�@Y�7@Yhs@YX@YX@YX@Y&�@XA�@W�P@W;d@Vȴ@VV@V$�@V{@U��@UO�@U�@TI�@S�F@S��@S"�@R�!@R�@Qhs@QX@QG�@Q�@P��@PA�@O�@O�P@O�@Nȴ@Nff@NE�@M�h@MO�@L�j@L�@K��@K�m@Kt�@JM�@Ix�@H�u@HQ�@H �@G�@G|�@G�@Fȴ@F�+@FE�@E��@E�-@E/@DZ@Cƨ@B�@Bn�@B=q@A��@A��@A�7@Ax�@@��@@�@@bN@@bN@@bN@@bN@@1'@?l�@?;d@?
=@>�@>�+@>@=�T@=�h@<��@<I�@<�@;�F@;S�@;@:�H@:��@:n�@:�@9�^@9X@9�@8��@8�9@8r�@8A�@8 �@8  @7��@7��@7K�@6��@65?@5��@5��@4�/@4�@4z�@3��@3t�@3o@2��@2n�@1��@1�^@1x�@1X@1�@0��@0��@0��@0�@0bN@0A�@/�@/;d@.V@-�T@-@-�-@-p�@-�@,�/@,I�@+�m@+�@+33@*�H@*�!@*~�@*=q@*J@)�^@)G�@)&�@)&�@)�@)%@)%@(��@(��@( �@'��@'�P@'|�@'|�@'l�@'l�@'\)@'K�@'
=@&��@&V@&5?@&$�@&{@%�@%�T@%�T@%�T@%�T@%��@%�-@%��@%�h@%�@%`B@%�@$�j@$Z@#�m@#ƨ@#��@#dZ@#"�@#o@#o@#o@"��@"^5@"�@!�^@ Ĝ@ r�@  �@�P@\)@�y@��@�+@v�@ff@E�@$�@@�-@��@�@�D@Z@9X@1@ƨ@dZ@C�@33@o@�H@�!@n�@�@��@��@G�@Ĝ@bNA���A���A�A��A���A���A�A���A���A���A���A���A���A���A���A�A���A���A���A���A�1A�VA�oA�A�A�  A�
=A�A�  A��A�A�A���A��A��A��TA��HA���A�
=A���A�JA�
=A��A��`A��yA��A��;A��
A���A���A���A���A���A���A���A���A��A��
A��#A���A��
A���A��A��
A��/A��;A��TA��TA��;A�ƨAܲ-A���AܾwA�ĜAܾwAܸRAܴ9Aܴ9A���A�ȴA���Aܧ�AܾwA�ffAܕ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      A���A���A���A�  A�1A���A��A���A��A��;A���A���A��#A�ĜAܼjAܓuA�z�A�~�A�p�A�-A�(�A�$�A�"�A��A��A��A��A��A��A��A�{A�oA�bA�bA�VA�VA�
=A���A���A���A��mA�ƨAۙ�A��`A�Q�Aװ!A�7LA��A�v�A�hsA�=qA�9XA�oA�A�&�Aδ9A͛�Aˣ�A�(�A��A��A���Aƛ�A���A��TAþwA¶FA�v�A��7A�p�A���A�\)A�?}A�\)A�A�7LA�p�A�&�A��7A��A�jA��;A��yA���A�ZA��A��HA�ffA�-A�A�jA���A�33A��A�S�A�t�A���A��A�r�A�n�A��9A�C�A�Q�A�A|�`Aw33Aq�Ao"�Al��Ai�
Af�+Abr�A^��A]�AZ�\AW�AT��AR  AO��AL�jAJQ�AHA�AGl�AFQ�AEC�AD�AC`BABE�AA%A?�FA>v�A=�;A=��A=hsA=�A<��A<M�A:�A8�\A4�`A0�DA.VA-\)A,ȴA,�uA,�A+hsA*�A*�A*�RA*��A*ȴA*��A*�9A+�PA+S�A*�A)��A(��A'�7A%XA$z�A#�7A"�`A"��A"��A"ZA"{A!��A!��A!��A!�-A!�A!�A!ƨA ȴA n�A�jA�A�mA7LA�yAM�AƨA��AhsA+A�/A�AA�AhsA�A�AJA��A�-A��A|�AhsAC�A�A�uA�A�^Al�A1'AS�A��A{A�hAoA��AĜA�+A{A
�A
VA
1A
$�A	�hA��A�A^5AJA  A1A�An�A��Az�AAƨA�A{A�A�AAVAQ�A9XA{A�TAt�AK�A�A
=A��A��A��Az�AI�A9XA1'A1Al�A �HA n�A �A 1@�C�@��
@�@���@�hs@�?}@�V@��j@�1'@���@�C�@�ȴ@��@�J@��h@�?}@���@�I�@�(�@��m@�|�@�@�j@���@��@���@�hs@�?}@�u@�dZ@�@�R@��#@�@�Z@��y@�^5@�E�@�5?@�-@�{@�^@�G�@��@���@��@�r�@�A�@��@�S�@�^5@���@ᙚ@�%@�Q�@ߥ�@�
=@���@ާ�@�5?@���@݁@�`B@��@���@���@�z�@�9X@��@�  @�ƨ@�"�@�o@���@���@���@�O�@��@�1@�=q@�x�@��@Դ9@�Q�@ӝ�@�C�@�
=@�ȴ@�v�@��@ѩ�@�X@�V@У�@��@Ϯ@��@��#@�?}@�%@���@̼j@�j@ˍP@�;d@�o@���@�^5@ɺ^@�hs@Ȭ@�z�@�1'@Ǯ@�"�@�E�@ř�@��/@�  @���@Ý�@���@�^5@��#@���@�bN@��m@�;d@���@�v�@�@��h@�p�@��@��@��@��;@�|�@���@��^@�`B@��`@��P@���@�ff@�J@�hs@�&�@�V@���@�j@�A�@�1'@�b@���@��w@��@�dZ@��H@�v�@��@���@��#@� �@��;@��
@�ƨ@��F@�|�@�33@��@�E�@�@��@���@�j@�bN@�A�@�b@��;@�\)@���@�v�@�v�@�n�@�ff@�-@���@��T@���@�x�@��@�Z@�l�@��R@�{@��#@�7L@�V@��9@�(�@��w@�dZ@�@��@���@�v�@�E�@��@��^@�hs@�7L@��@���@��/@�Ĝ@���@��;@���@���@��P@��@�t�@�;d@���@���@��+@�ff@�M�@�{@���@��7@�/@��@�bN@� �@��F@�dZ@�o@��H@���@�n�@�n�@�^5@��^@�O�@�?}@�/@�%@��@�r�@�Q�@�b@���@��H@�V@�@�@�p�@��D@��;@��w@��F@��P@�S�@�C�@�33@�+@�o@�@��@��H@�ȴ@���@��\@�$�@���@�@��^@���@���@�x�@�?}@���@��@�b@��m@��
@�ƨ@��w@��F@�l�@��H@��+@�{@��@�/@���@��`@��j@�1'@�  @��m@���@��w@�C�@��@��y@�^5@���@��@�&�@���@��`@��j@��u@�z�@�b@��F@�|�@�S�@�33@�o@��!@�^5@�M�@�=q@�{@��7@��@��u@�Z@��m@���@���@���@�t�@�S�@�33@�o@��@���@�~�@�-@��-@�&�@���@���@��9@���@�Q�@�  @�  @+@~�R@~5?@}��@}�@}`B@}?}@}�@|��@|��@|j@|�@{�m@{dZ@zJ@y��@xĜ@xbN@w�@wl�@v�y@v��@vff@vV@v{@u?}@tj@s�m@so@q��@qX@p�`@pbN@o|�@oK�@oK�@o
=@nȴ@n�R@n�+@m�@m�h@m`B@m�@l��@l(�@k��@kS�@j�\@i�^@h�u@hr�@h �@g�@g�@g�P@g
=@f�R@fv�@fff@fE�@f{@e�T@e@e�-@e�h@e�@e`B@e?}@e?}@e?}@eV@d�/@d��@d�j@dz�@dj@dj@dj@d9X@d(�@cƨ@c33@c"�@b�@b�\@a��@a%@`�u@` �@_��@^��@]��@\��@\��@\(�@[�F@Z�H@Y�@Y�7@Yhs@YX@YX@YX@Y&�@XA�@W�P@W;d@Vȴ@VV@V$�@V{@U��@UO�@U�@TI�@S�F@S��@S"�@R�!@R�@Qhs@QX@QG�@Q�@P��@PA�@O�@O�P@O�@Nȴ@Nff@NE�@M�h@MO�@L�j@L�@K��@K�m@Kt�@JM�@Ix�@H�u@HQ�@H �@G�@G|�@G�@Fȴ@F�+@FE�@E��@E�-@E/@DZ@Cƨ@B�@Bn�@B=q@A��@A��@A�7@Ax�@@��@@�@@bN@@bN@@bN@@bN@@1'@?l�@?;d@?
=@>�@>�+@>@=�T@=�h@<��@<I�@<�@;�F@;S�@;@:�H@:��@:n�@:�@9�^@9X@9�@8��@8�9@8r�@8A�@8 �@8  @7��@7��@7K�@6��@65?@5��@5��@4�/@4�@4z�@3��@3t�@3o@2��@2n�@1��@1�^@1x�@1X@1�@0��@0��@0��@0�@0bN@0A�@/�@/;d@.V@-�T@-@-�-@-p�@-�@,�/@,I�@+�m@+�@+33@*�H@*�!@*~�@*=q@*J@)�^@)G�@)&�@)&�@)�@)%@)%@(��@(��@( �@'��@'�P@'|�@'|�@'l�@'l�@'\)@'K�@'
=@&��@&V@&5?@&$�@&{@%�@%�T@%�T@%�T@%�T@%��@%�-@%��@%�h@%�@%`B@%�@$�j@$Z@#�m@#ƨ@#��@#dZ@#"�@#o@#o@#o@"��@"^5@"�@!�^@ Ĝ@ r�@  �@�P@\)@�y@��@�+@v�@ff@E�@$�@@�-@��@�@�D@Z@9X@1@ƨ@dZ@C�@33@o@�H@�!@n�@�@��@��@G�@ĜG�O�A���A���A�A��A���A���A�A���A���A���A���A���A���A���A���A�A���A���A���A���A�1A�VA�oA�A�A�  A�
=A�A�  A��A�A�A���A��A��A��TA��HA���A�
=A���A�JA�
=A��A��`A��yA��A��;A��
A���A���A���A���A���A���A���A���A��A��
A��#A���A��
A���A��A��
A��/A��;A��TA��TA��;A�ƨAܲ-A���AܾwA�ĜAܾwAܸRAܴ9Aܴ9A���A�ȴA���Aܧ�AܾwA�ffAܕ�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
��B
� B
�4B
��B
� B
� B
� B
�\B
��B
�bB
��B
��B
��B
��B
��B
��B
�B
�B
��B
��B
��B
��B
��B
�xB
�DB
��B
��B
��B
�B
��B
��B
�rB
��B
��B
��B
��B
�B
��B
��B
�lB
��B
��B
��B
{B
t�B
e,B
b�B
d�B
_;B
]�B
\�B
YKB
OvB
F�B
=B
>B
AUB
U�B
`�B
h�B
u�B
�.B
�1B
�RB
��B
�#B
�?B
�B�B(XB5B<6B>�BM6Bv�Bs�BcB�B�+B�ABzxBxlBx�B`BQ�BK�BFB=qB$�B�B
�QB
�BB
�QB
ԕB
҉B
�}B
�'B
��B
��B
}VB
VmB
C-B
B
�B
GB	�]B	�HB	��B	��B	��B	��B	��B	}VB	lWB	f�B	[�B	U�B	H�B	IRB	A�B	CaB	9�B	9�B	6zB	1�B	33B	0�B	1[B	1�B	4�B	4�B	3�B	33B	33B	3�B	3�B	3�B	5B	6�B	>wB	HB	MB	W�B	_B	aHB	d�B	k�B	ncB	sB	y�B	��B	�1B	�oB	��B	��B	�9B	�#B	��B	�HB	�9B	ɆB	�mB	�B	��B	�B	��B	�2B	�B	�;B	�|B	�B	��B
�B
�B
DB

�B
PB
�B
�B
B
�B
�B
�B
	lB
xB
JB
JB
�B
B
�B
B
�B
B
 B
�B
�B
�B
~B
�B
B
xB
uB
�B
.B
\B
:B
�B
�B
oB
@B
�B
FB
�B
xB
�B
$B
�B
�B
FB
�B
hB
�B
�B
{B
�B
1B
7B
!�B
$�B
)�B
&�B
#�B
!bB
xB
�B
)_B
(�B
#:B
$�B
'�B
($B
)�B
&B
&LB
%�B
%zB
%�B
%�B
%�B
&LB
&�B
%�B
%�B
%B
#�B
 �B
!B
B
=B
=B
B
�B
4B
bB
.B
�B
�B
"B
�B
JB
~B
.B
oB
:B
�B
(B
�B
bB
.B
(B
�B
1B
{B
;B
 iB	�cB	�(B	��B	�B	��B	��B	��B	��B	��B	��B	��B	�B	��B	�xB	��B	�B	�B	�xB	�DB	��B	�>B	�>B	�	B	��B	�rB	��B	�B	�>B	�8B	�	B	�	B	�B	��B	�>B	�DB	�xB	�DB	�B	��B	��B	��B	��B	��B	��B	�(B	��B	�]B	��B	��B	��B	�VB	��B	��B	��B	�(B	��B	�cB
 �B
  B
 4B
 iB
 iB
 �B
 iB
 iB
 4B
  B
 �B
B
 4B
�B
�B
AB
�B
uB
�B
B
B
�B
uB
AB
uB
GB
{B
{B
�B
{B
�B
B
SB
�B
SB
YB
%B
YB
�B
+B
_B
fB
�B
	B
	lB
	7B
	lB
	�B

	B
	lB
	�B
	B

rB

=B

=B

=B
xB

rB

�B
B
�B
DB
�B
JB
JB
B
~B
B
B
�B
�B
�B
�B
~B
�B
PB
�B
"B
PB
�B
�B
\B
\B
(B
�B
�B
�B
(B
�B
.B
�B
hB
4B
 B
�B
�B
�B
�B
�B
oB
:B
�B
oB
�B
�B
�B
:B
�B
oB
�B
B
�B
{B
�B
{B
{B
�B
�B
�B
B
�B
SB
�B
�B
�B
$B
�B
�B
�B
+B
�B
_B
�B
�B
7B
kB
kB
7B
B
7B
�B
	B
=B
	B
=B
	B
B
CB
IB
IB
IB
�B
CB
�B
�B
B
�B
IB
IB
�B
�B
OB
B
�B
�B
B
�B
!B
�B
�B
 �B
!-B
!-B
!�B
!�B
!bB
$B
$B
$@B
$B
$@B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
$�B
%B
$�B
&B
&LB
&B
&LB
&LB
&B
&�B
&�B
'B
'�B
(�B
(�B
(�B
(�B
(�B
(�B
)�B
)�B
*�B
+6B
,B
,�B
,�B
,�B
,�B
-CB
-B
-CB
-CB
-B
.IB
-�B
-�B
-wB
.B
.B
.}B
.}B
.}B
.�B
.�B
.�B
0UB
0�B
0�B
0�B
0�B
0�B
1'B
1[B
0�B
0�B
0UB
1�B
1�B
2-B
2�B
3hB
3hB
3�B
4nB
4nB
4�B
4�B
4�B
4nB
4nB
49B
4�B
5�B
6�B
7B
7LB
7B
7B
7�B
7�B
7�B
8�B
8�B
8�B
8�B
9XB
9$B
9�B
9XB
9�B
9XB
9�B
9�B
9�B
:*B
;0B
:�B
<B
;�B
<�B
<jB
<�B
<�B
<�B
<jB
<�B
=qB
=�B
=�B
>BB
@B
?B
?�B
@B
A B
@�B
@�B
@�B
@�B
@�B
@�B
AUB
A�B
AUB
AUB
A�B
B'B
B'B
B[B
B�B
C�B
DgB
D3B
D�B
DgB
DgB
D�B
EmB
EmB
EmB
EmB
E�B
E�B
E�B
E�B
E�B
FB
FB
F?B
F?B
F?B
FB
FtB
F�B
FtB
FtB
F�B
F�B
FtB
FtB
F�B
F�B
GB
GEB
GB
GB
GB
G�B
HKB
HKB
H�B
H�B
I�B
J#B
K)B
J�B
K^B
K^B
LdB
MjB
MB
M6B
MB
L�B
L�B
L�B
M�B
NB
NpB
N�B
N�B
N�B
N�B
OBB
OBB
OBB
PB
P}B
PHB
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R B
R�B
R�B
R�B
R�B
S[B
S[B
S[B
T,B
S�B
T�B
T�B
T�B
T�B
T�B
V9B
V�B
WsB
WsB
W�B
W�B
XB
XB
XEB
XyB
X�B
X�B
X�B
YKB
Y�B
Z�B
[WB
[�B
[�B
[�B
\]B
\)B
\)B
\�B
]/B
]/B
]/B
]/B
\�B
\�B
]�B
]�B
]�B
^B
^5B
^�B
^5B
^�B
_pB
_B
_pB
_�B
`B
`BB
`vB
`vB
`�B
aHB
a|B
a�B
bB
bNB
bNB
b�B
b�B
b�B
b�B
b�B
b�B
c B
c�B
dZB
dZB
d�B
e,B
d�B
d�B
d�B
e,B
e`B
e`B
e�B
f�B
gB
gmB
gmB
g�B
g�B
g�B
h
B
g�B
h
B
h
B
h>B
h�B
iyB
iyB
iyB
i�B
i�B
i�B
i�B
i�B
i�B
jB
jKB
jKB
jB
jB
j�B
kB
k�B
l�B
l"B
lWB
l�B
l�B
l�B
m)B
l�B
n/B
n/B
m�B
m�B
m�B
n/B
m�B
m�B
n/B
ncB
o B
oiB
o5B
oiB
oiB
o�B
o�B
o�B
oiB
oiB
o�B
o�B
o�B
o�B
o�B
o�B
pB
p;B
p�B
p�B
p�B
p�B
qAB
qvB
qvB
qAB
qAB
q�B
q�B
rB
q�B
s�B
sB
s�B
tTB
t�B
uZB
u�B
u�B
u�B
u�B
u�B
u�B
u�B
u�B
w2B
v�B
w2B
wfB
wfB
w�B
w�B
x8B
xlB
x8B
xlB
xlB
x�B
y>B
y�B
y�B
yrB
zDB
zxB
{JB
�.B
��B
��B
��B
��B
��B
��B
��B
�hB
�4B
�bB
�B
��B
��B
�.B
��B
�:B
�hB
�\B
�hB
��B
� B
��B
��B
��B
�B
��B
��B
��B
�uB
�@B
��B
�{B
��B
�bB
��B
�.B
��B
�\B
�B
��B
��B
�JB
�bB
�xB
��B
�:B
�B
��B
��B
�\B
��B
�VB
��B
��B
�"B
��B
��B
��B
�(B
��B
�.B
�~B
�(B
�VB
��B
��B
�"B
��B
��B
��B
��B
�PB
��B
��B
��B
�B
�rB
��B
��B
��B
�hB
��B
�!B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      B
��B
�B
�2B
��B
�hB
�ZB
��B
� B
�8B
��B
�wB
��B
��B
�0B
��B
�NB
�!B
��B
�\B
��B
��B
��B
��B
��B
�VB
��B
��B
��B
�B
��B
��B
��B
��B
��B
��B
��B
�TB
��B
��B
��B
�pB
�RB
�CB
��B
}�B
hdB
d[B
gcB
_�B
_dB
b�B
_�B
V�B
MzB
>�B
AB
F�B
Y�B
c�B
k�B
x�B
�'B
��B
�YB
��B
�/B
��B
�B�B1XB8�B=�BGBgB|�BgB�mB�B��B��B�hB��B��Bi�BS�BN�BQ;BU+BBaB,B
�B
�+B
ܐB
��B
�XB
ƙB
�B
�RB
�DB
�IB
hSB
VB
#�B
1B
UB	�B	��B	ǅB	�PB	��B	�YB	��B	��B	v@B	q�B	f�B	_8B	RIB	TB	K1B	J�B	=4B	>!B	:�B	6#B	6&B	5B	6FB	6�B	9mB	7B	4�B	4B	4cB	5'B	5�B	9�B	<mB	C&B	L�B	O�B	PB	Y�B	_�B	cB	f�B	mIB	oZB	sB	ywB	��B	��B	�B	�oB	�AB	��B	�>B	�7B	�nB	�dB	��B	ȩB	�B	�EB	��B	��B	�2B	�&B	��B	�vB	�LB	�JB
-B
�B
|B
�B
8B
�B
sB
eB
�B
�B
�B

B
.B
/B
_B
�B
�B
B
�B
3B
�B
�B
7B
B
B
�B
�B
B
�B
�B
�B
B
cB
�B
�B
�B
�B
9B
FB
5B
'B
�B
#B
�B
�B
�B
�B
'B
�B
�B
B
�B
�B
�B
�B
!!B
%�B
+oB
'�B
&�B
%UB
MB
 �B
,�B
+TB
#�B
%$B
(�B
)B
+�B
&�B
'!B
%�B
%�B
&rB
&`B
&�B
'B
&�B
&-B
&�B
'�B
&B
"�B
 qB
�B
�B
!�B
�B
�B
�B
�B
�B
^B
�B
OB
kB
kB
�B
�B
�B
7B
@B
B
qB
6B
�B
B
B
�B
�B
B
B
 #B
  B
BB	�EB	��B	�B	�RB	��B
 B	�4B	��B	�FB	�B	��B	��B	�:B	��B	��B	��B	�gB	��B	�?B	��B	��B	��B	�B	��B	�B	��B	�|B	��B	��B	��B	�XB	��B	��B	�	B	��B	�%B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�>B	�lB	��B
'B
 �B	�7B
 #B
 �B
EB
 �B
 �B
(B
PB
�B
�B
PB
B
EB
OB
�B
B
�B
 B
�B
(B
�B
fB
^B
�B
�B
�B
�B
rB
�B
�B
�B
0B
MB
�B
xB
eB
�B
�B
�B
�B
VB
�B
B
�B
	RB
	�B

B
	�B

B

%B

SB

GB
	�B

1B

LB

�B

�B
�B
�B
B
LB
�B
8B
XB
�B
�B
�B
{B
�B
B
aB
CB
B
8B
B
B
+B
�B
6B
tB
tB
�B
�B
B
yB
�B
\B
AB
�B
)B
�B
�B
�B
�B
�B
UB
PB
9B
LB
�B
0B
B
zB
UB
�B
�B
>B
�B
�B
B
�B
B
�B
�B
�B
B
�B
�B
HB
�B
�B
�B
�B
MB
�B
QB
�B
�B
�B
iB
-B
�B
fB
DB
�B
uB
�B
�B
�B
�B
UB
>B
�B
�B
fB
oB
[B
�B
�B
�B
�B
B
�B
vB
`B
;B
�B
mB
�B
�B
�B
[B
B
KB
0B
NB
B
(B
�B
>B
�B
�B
!B
"7B
"_B
!�B
"@B
"�B
#bB
%sB
$bB
$eB
$rB
$�B
$�B
$�B
$�B
%B
$�B
$�B
%B
$�B
%=B
%ZB
%�B
&�B
&xB
&5B
&xB
&~B
&vB
'B
'uB
'�B
(�B
)B
(�B
(�B
)B
(�B
)QB
*�B
*�B
+�B
,uB
,�B
-B
-&B
--B
-�B
-�B
-TB
-�B
-�B
.B
.�B
.zB
.�B
.�B
.�B
.�B
.�B
.�B
.�B
/B
/B
/�B
1*B
1FB
1&B
1KB
1+B
1�B
1�B
1�B
0�B
1JB
1�B
3B
3+B
2�B
3�B
3�B
3~B
4B
4�B
4�B
4�B
4�B
5=B
57B
4�B
5B
5�B
6�B
7+B
7{B
7�B
7\B
7�B
8�B
8B
8�B
9B
9(B
9+B
9"B
9�B
9TB
9�B
9�B
9�B
9�B
:[B
:B
:WB
;�B
;�B
;�B
<�B
<�B
= B
=B
<�B
=)B
<�B
<�B
=�B
>�B
>zB
?B
?�B
@�B
?�B
@zB
A5B
A`B
@�B
@�B
AB
AB
AB
AEB
A�B
A�B
A�B
BB
B�B
B�B
B�B
CwB
C�B
D�B
D�B
D�B
D�B
D�B
D�B
EOB
E�B
E�B
E�B
E�B
E�B
FB
FB
E�B
FB
F*B
F=B
FiB
FHB
FOB
FSB
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
F�B
G<B
G�B
GpB
GkB
G�B
H$B
H�B
H�B
H�B
I�B
I�B
KqB
K9B
K�B
K�B
LB
L�B
M�B
M�B
M5B
MNB
M
B
L�B
MEB
M�B
N�B
N�B
OB
O3B
OB
N�B
OHB
O�B
O�B
P	B
P�B
P�B
P�B
Q'B
Q}B
R$B
RB
RB
R!B
R5B
R�B
R�B
R�B
S`B
SIB
S�B
S�B
TB
TuB
T�B
U`B
U$B
T�B
UB
VB
WB
WwB
W�B
W�B
W�B
XB
XqB
XbB
X�B
X�B
YB
X�B
Y:B
ZB
Z�B
[PB
[�B
[�B
\B
\SB
\rB
\EB
\�B
]fB
]RB
]4B
]4B
]8B
]8B
]�B
^B
^B
^B
^cB
^�B
^�B
^�B
_rB
_�B
_MB
_�B
`B
`gB
`jB
`�B
`�B
aFB
a�B
a�B
a�B
baB
b{B
b�B
b�B
b�B
cB
c'B
c-B
cVB
c�B
ddB
d�B
d�B
e]B
ehB
eB
e\B
eWB
e�B
e�B
e�B
f}B
gB
gMB
g�B
g�B
g�B
g�B
hB
h3B
h B
h:B
hxB
iB
i�B
i�B
i�B
i�B
i�B
jB
jB
jNB
j"B
jRB
jvB
j�B
j�B
j�B
j�B
j�B
k�B
luB
l�B
l.B
lpB
l�B
l�B
l�B
mxB
m�B
n�B
nLB
nB
nB
m�B
n6B
nB
nB
n�B
n�B
odB
o�B
oQB
o�B
o�B
o�B
o�B
o�B
oqB
o�B
o�B
o�B
o�B
o�B
o�B
p1B
p�B
p�B
q+B
qB
qB
q1B
q�B
q�B
q}B
q[B
q�B
r9B
rCB
r�B
sB
s�B
s�B
teB
t�B
uB
u�B
u�B
u�B
u�B
u�B
v&B
u�B
vDB
v�B
w�B
w2B
wsB
w�B
w�B
w�B
xFB
xeB
x�B
xkB
x�B
x�B
yB
y�B
y�B
z#B
y�B
z�B
z�G�O�B
�.B
��B
��B
��B
��B
��B
��B
��B
�hB
�4B
�bB
�B
��B
��B
�.B
��B
�:B
�hB
�\B
�hB
��B
� B
��B
��B
��B
�B
��B
��B
��B
�uB
�@B
��B
�{B
��B
�bB
��B
�.B
��B
�\B
�B
��B
��B
�JB
�bB
�xB
��B
�:B
�B
��B
��B
�\B
��B
�VB
��B
��B
�"B
��B
��B
��B
�(B
��B
�.B
�~B
�(B
�VB
��B
��B
�"B
��B
��B
��B
��B
�PB
��B
��B
��B
�B
�rB
��B
��B
��B
�hB
��B
�!B
�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<%6q<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<]Ǽ<���<#�
<#�
<#�
<#�
<���<#�
<FT[<0�T<�O=<�P)<G�F<kq�=��<k��<)�H<#�
<#�
<A��<��;<<~�<#�
<#�
<#�
<#�
<K*w<#�
<>z<H�<��U<�a4<�>�<�O�<#�
<+�i<�n<��!<9{�<+��<8�<\��<t9�<N��<#�
<.T<BF�<<�l<)�`<&�<:��<$�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<Qo�<qC<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202204261606322022042616063220220426160632202204261606322022042616063220220426160632SI  SI  ARFMARFM                                                                                                                                                2021090807371720210908073717IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021092809012620210928090126QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021092809012620210928090126QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2022042213364920220422133649IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022042616064620220426160646IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022042616064620220426160646IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022042616064620220426160646IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                