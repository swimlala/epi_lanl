CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-01-21T02:42:33Z creation; 2022-04-26T16:06:55Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  Z    PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  a@   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  ~8   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  �x   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �p   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  �h   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ƨ   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @ $�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � ,   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 @ I   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � PH   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` m@   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   m�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   s�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   y�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T �   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    ��Argo profile    3.1 1.2 19500101000000  20210121024233  20220426232401  5906402 5906402 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    DEAN ROEMMICH, SARAH PURKEY, NATHALIE ZILBERMAN, JOHN GILSON    PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO8138_008904_007                 8138_008904_007                 2C  2C  DD  SOLO_II                         SOLO_II                         8904                            8904                            V2.6; SBE602 14Jan20            V2.6; SBE602 14Jan20            853 853 @�XBե�c@�XBե�c11  @�XB��"�@�XB��"�@,��3rT@,��3rT�dG�?���dG�?��11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�\@@  @}p�@�  @�G�@޸RA   A��A   A,(�A@��A`  A\)A�  A�  A�  A��AϮA߮A�B   BQ�B(�B�
B�B'�
B/�
B8  B@  BG�
BO�
BX(�B`  Bh  Bp(�Bx  B�{B�{B�  B�ffB�{B�(�B�  B��B��B��B��B��B�  B��B��B�  B�{B�  B��B�  B��B��B��B�{B�(�B�{B�{B�  B�{B�{B�  B��C   C  C  C  C  C
  C  C  C  C��C��C  C
=C
=C  C  C   C"
=C$  C&  C(  C*  C,
=C.
=C0  C2  C4  C6  C8
=C:
=C<  C>
=C@  CB
=CD
=CF  CH
=CJ  CL  CM��CP  CR
=CT  CU��CX  CZ  C[��C]�C_��Cb  Cd
=Cf
=Ch  Cj  Cl
=Cn  Co��Cq��Cs��Cu��Cx  Cz
=C|  C~  C�  C�  C���C���C�  C�C�  C�  C�  C�  C���C�  C���C���C�C�C�C�  C���C���C�  C�  C�C�C�
=C�
=C�  C���C���C���C���C���C���C���C�  C�C�  C�  C���C���C�  C�C�  C���C�C�  C���C���C�  C�C�C�  C�  C�C�C�  C�  C�  C�C�C���C�  C�  C�  C�  C�  C�  C���C���C���C�  C�C�  C���C���C���C�  C�C�C�  C�  C�  C�  C�  C�C�  C���C���C�  C���C���C���C�  C�  C�C�  C�  C�  C�  C�  C���C���C�  C�  C�  C�  C�  C���C�  C�  C���C���C���C�  C�  C�C�C�C�C�C���C���C�  C���C�C�C���C�  C���D }qD  D}qD�qD� D  D}qD�qD}qD  D��DD��D  Dz�D��D}qD	�D	��D
�D
��D�D��D�D��D  D}qD�qD}qD��D� D�D��D  D}qD��Dz�D  D��D�D��D  D}qD  D��D  D}qD�qD� D�qD� D�D� D  D}qD��D� D�D� D  D��D  D��D �D }qD!�D!��D"  D"}qD#  D#��D$  D$}qD%  D%��D%�qD&� D'  D'��D(�D(}qD)  D)��D*  D*}qD+  D+��D,�D,��D-  D-}qD-�qD.}qD.�qD/� D0  D0� D1�D1� D2  D2��D3  D3}qD4  D4��D5  D5� D6  D6� D7  D7}qD8  D8� D9  D9��D9�qD:� D;  D;� D<  D<� D=  D=��D>  D>}qD?  D?� D@  D@� D@�qDA� DB�DB� DB�qDC� DD  DD� DE�DE��DF  DF� DG  DG}qDG�qDH� DI�DI��DJ�DJ� DJ�qDK� DL  DL� DM�DM��DM�qDN}qDO�DO��DP  DP}qDQ  DQ� DR�DR��DS�DS� DT  DT� DU�DU��DV�DV� DV�qDW}qDX  DX��DY  DY� DZ�DZ}qD[  D[� D[�qD\� D\�qD]� D^  D^� D^�qD_}qD_�qD`}qDa  Da��Db�Db� Db�qDc}qDc�qDd� De�De� De�qDf� Dg  Dg}qDg�qDh}qDi�Di� Dj  Dj��Dk�Dk� Dl  Dl��Dm  Dmz�Dn  Dn��Dn�qDo� Dp  Dp��Dq�Dq��Dr�Dr��Ds  Ds}qDt  Dt��Du  Du}qDu�qDv}qDw  Dw� Dx�Dx� Dy�Dy��Dz  Dz��D{  D{� D|  D|}qD|�qD}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�=qD�~�D��HD�  D�@ D�� D���D�HD�AHD��HD��HD�HD�AHD�~�D���D�  D�@ D��HD�� D���D�@ D���D�� D�  D�B�D�� D���D���D�@ D��HD���D���D�>�D�~�D��HD�HD�>�D�~�D���D�  D�@ D�~�D���D�HD�AHD�� D�� D�HD�@ D�~�D�� D�HD�B�D��HD�� D�  D�>�D�~�D��HD�HD�>�D�~�D�� D���D�@ D�� D��HD��D�AHD�� D���D��qD�>�D��HD�� D�  D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D�  D�@ D��HD��HD�  D�=qD�~�D�� D��D�AHD��HD��HD�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�}qD���D�HD�>�D�~�D�� D���D�AHD���D�� D�HD�B�D�� D��HD��D�@ D��HD��HD���D�@ D�� D�� D�HD�>�D�� D�� D�  D�@ D�� D�� D�HD�AHD��HD��HD���D�@ D�� D�� D�  D�>�D�~�D�� D���D�>�D��HD�� D���D�=qD�}qD�� D�  D�>�D��HD�� D�  D�AHD�� D�� D���D�>�D��HD�� D���D�AHD�� D�� D��D�AHD��HD��HD�HD�AHD�� D��HD�HD�AHD��HD��HD�HD�AHD��HD�� D�HD�@ D�~�D�� D�  D�@ D�� D��HD�HD�AHD���D��HD�  D�AHD��HD��HD�  D�>�D�~�D�� D���D�>�D�� D�� D�  D�AHD�� D��HD�HD�@ D�~�D�� D�  D�@ D�� D���D�  D�@ D��HD��HD�  D�@ D�~�D�� D�HD�AHD�~�D�� D�  D�@ D�� D�� D�HD�B�D�� D���D���D�>�D�� D�� D��qD�@ D�� D���D���D�@ D��HD�� D���D�@ DHD�� D���D�AHDÀ Dþ�D���D�>�DĀ D�� D���D�>�Dŀ D�� D���D�@ Dƀ D�� D�  D�>�DǁHD��HD�HD�@ DȀ D�� D�HD�AHDɀ DɽqD���D�>�Dʀ D�� D�  D�@ D�~�D�� D�HD�AHD́HD̾�D�  D�AHD͂�D��HD�HD�@ D΀ D��HD�  D�@ Dπ D�� D�HD�>�D�~�D�� D�  D�@ Dр D�� D�HD�AHDҀ DҾ�D���D�>�D�~�D�� D�  D�>�D�}qD�� D�HD�AHDՂ�D�D�  D�=qD�}qDֽqD���D�>�D�~�D׾�D�  D�@ D؀ D��HD�HD�@ D�~�Dپ�D�  D�@ D�~�DڽqD���D�>�Dۀ D��HD�  D�>�D܀ D��HD�HD�@ D݀ Dݾ�D���D�@ Dހ D޽qD���D�>�D߀ D�� D�  D�AHD�� D��HD���D�@ D�HDᾸD�  D�@ D�HD�D�HD�>�D� D��HD�  D�AHD䂏D��HD�  D�>�D� D��HD�HD�@ ?�?#�
?u?�\)?�33?���?�@�@z�@(��@8Q�@J=q@W
=@fff@u@�ff@�{@�@�p�@�ff@�\)@���@�  @Ǯ@��@ٙ�@��
@�@�33@��RA33A
=A(�A��A�A��Ap�A"�\A'
=A*=qA/\)A4z�A8Q�A=p�AA�AEAK�AO\)AS�
AX��A]p�Aa�Ag
=Aj�HAp��AuAy��A~{A�G�A��
A�A�Q�A��\A��A�\)A���A�z�A�ffA���A�33A�A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ?��@�\@@  @}p�@�  @�G�@޸RA   A��A   A,(�A@��A`  A\)A�  A�  A�  A��AϮA߮A�B   BQ�B(�B�
B�B'�
B/�
B8  B@  BG�
BO�
BX(�B`  Bh  Bp(�Bx  B�{B�{B�  B�ffB�{B�(�B�  B��B��B��B��B��B�  B��B��B�  B�{B�  B��B�  B��B��B��B�{B�(�B�{B�{B�  B�{B�{B�  B��C   C  C  C  C  C
  C  C  C  C��C��C  C
=C
=C  C  C   C"
=C$  C&  C(  C*  C,
=C.
=C0  C2  C4  C6  C8
=C:
=C<  C>
=C@  CB
=CD
=CF  CH
=CJ  CL  CM��CP  CR
=CT  CU��CX  CZ  C[��C]�C_��Cb  Cd
=Cf
=Ch  Cj  Cl
=Cn  Co��Cq��Cs��Cu��Cx  Cz
=C|  C~  C�  C�  C���C���C�  C�C�  C�  C�  C�  C���C�  C���C���C�C�C�C�  C���C���C�  C�  C�C�C�
=C�
=C�  C���C���C���C���C���C���C���C�  C�C�  C�  C���C���C�  C�C�  C���C�C�  C���C���C�  C�C�C�  C�  C�C�C�  C�  C�  C�C�C���C�  C�  C�  C�  C�  C�  C���C���C���C�  C�C�  C���C���C���C�  C�C�C�  C�  C�  C�  C�  C�C�  C���C���C�  C���C���C���C�  C�  C�C�  C�  C�  C�  C�  C���C���C�  C�  C�  C�  C�  C���C�  C�  C���C���C���C�  C�  C�C�C�C�C�C���C���C�  C���C�C�C���C�  C���D }qD  D}qD�qD� D  D}qD�qD}qD  D��DD��D  Dz�D��D}qD	�D	��D
�D
��D�D��D�D��D  D}qD�qD}qD��D� D�D��D  D}qD��Dz�D  D��D�D��D  D}qD  D��D  D}qD�qD� D�qD� D�D� D  D}qD��D� D�D� D  D��D  D��D �D }qD!�D!��D"  D"}qD#  D#��D$  D$}qD%  D%��D%�qD&� D'  D'��D(�D(}qD)  D)��D*  D*}qD+  D+��D,�D,��D-  D-}qD-�qD.}qD.�qD/� D0  D0� D1�D1� D2  D2��D3  D3}qD4  D4��D5  D5� D6  D6� D7  D7}qD8  D8� D9  D9��D9�qD:� D;  D;� D<  D<� D=  D=��D>  D>}qD?  D?� D@  D@� D@�qDA� DB�DB� DB�qDC� DD  DD� DE�DE��DF  DF� DG  DG}qDG�qDH� DI�DI��DJ�DJ� DJ�qDK� DL  DL� DM�DM��DM�qDN}qDO�DO��DP  DP}qDQ  DQ� DR�DR��DS�DS� DT  DT� DU�DU��DV�DV� DV�qDW}qDX  DX��DY  DY� DZ�DZ}qD[  D[� D[�qD\� D\�qD]� D^  D^� D^�qD_}qD_�qD`}qDa  Da��Db�Db� Db�qDc}qDc�qDd� De�De� De�qDf� Dg  Dg}qDg�qDh}qDi�Di� Dj  Dj��Dk�Dk� Dl  Dl��Dm  Dmz�Dn  Dn��Dn�qDo� Dp  Dp��Dq�Dq��Dr�Dr��Ds  Ds}qDt  Dt��Du  Du}qDu�qDv}qDw  Dw� Dx�Dx� Dy�Dy��Dz  Dz��D{  D{� D|  D|}qD|�qD}� D~  D~� D  D� D�  D�@ D�� D�� D�  D�=qD�~�D��HD�  D�@ D�� D���D�HD�AHD��HD��HD�HD�AHD�~�D���D�  D�@ D��HD�� D���D�@ D���D�� D�  D�B�D�� D���D���D�@ D��HD���D���D�>�D�~�D��HD�HD�>�D�~�D���D�  D�@ D�~�D���D�HD�AHD�� D�� D�HD�@ D�~�D�� D�HD�B�D��HD�� D�  D�>�D�~�D��HD�HD�>�D�~�D�� D���D�@ D�� D��HD��D�AHD�� D���D��qD�>�D��HD�� D�  D�>�D�~�D���D���D�>�D�~�D���D���D�>�D�~�D���D�  D�@ D��HD��HD�  D�=qD�~�D�� D��D�AHD��HD��HD�  D�@ D�� D���D�  D�@ D�� D�� D�  D�@ D�}qD���D�HD�>�D�~�D�� D���D�AHD���D�� D�HD�B�D�� D��HD��D�@ D��HD��HD���D�@ D�� D�� D�HD�>�D�� D�� D�  D�@ D�� D�� D�HD�AHD��HD��HD���D�@ D�� D�� D�  D�>�D�~�D�� D���D�>�D��HD�� D���D�=qD�}qD�� D�  D�>�D��HD�� D�  D�AHD�� D�� D���D�>�D��HD�� D���D�AHD�� D�� D��D�AHD��HD��HD�HD�AHD�� D��HD�HD�AHD��HD��HD�HD�AHD��HD�� D�HD�@ D�~�D�� D�  D�@ D�� D��HD�HD�AHD���D��HD�  D�AHD��HD��HD�  D�>�D�~�D�� D���D�>�D�� D�� D�  D�AHD�� D��HD�HD�@ D�~�D�� D�  D�@ D�� D���D�  D�@ D��HD��HD�  D�@ D�~�D�� D�HD�AHD�~�D�� D�  D�@ D�� D�� D�HD�B�D�� D���D���D�>�D�� D�� D��qD�@ D�� D���D���D�@ D��HD�� D���D�@ DHD�� D���D�AHDÀ Dþ�D���D�>�DĀ D�� D���D�>�Dŀ D�� D���D�@ Dƀ D�� D�  D�>�DǁHD��HD�HD�@ DȀ D�� D�HD�AHDɀ DɽqD���D�>�Dʀ D�� D�  D�@ D�~�D�� D�HD�AHD́HD̾�D�  D�AHD͂�D��HD�HD�@ D΀ D��HD�  D�@ Dπ D�� D�HD�>�D�~�D�� D�  D�@ Dр D�� D�HD�AHDҀ DҾ�D���D�>�D�~�D�� D�  D�>�D�}qD�� D�HD�AHDՂ�D�D�  D�=qD�}qDֽqD���D�>�D�~�D׾�D�  D�@ D؀ D��HD�HD�@ D�~�Dپ�D�  D�@ D�~�DڽqD���D�>�Dۀ D��HD�  D�>�D܀ D��HD�HD�@ D݀ Dݾ�D���D�@ Dހ D޽qD���D�>�D߀ D�� D�  D�AHD�� D��HD���D�@ D�HDᾸD�  D�@ D�HD�D�HD�>�D� D��HD�  D�AHD䂏D��HD�  D�>�D� D��HD�HG�O�?�?#�
?u?�\)?�33?���?�@�@z�@(��@8Q�@J=q@W
=@fff@u@�ff@�{@�@�p�@�ff@�\)@���@�  @Ǯ@��@ٙ�@��
@�@�33@��RA33A
=A(�A��A�A��Ap�A"�\A'
=A*=qA/\)A4z�A8Q�A=p�AA�AEAK�AO\)AS�
AX��A]p�Aa�Ag
=Aj�HAp��AuAy��A~{A�G�A��
A�A�Q�A��\A��A�\)A���A�z�A�ffA���A�33A�A�  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A·+A΋DA΋DA�n�A�7LA�+A��A��A���AͶFAͬA͟�A͏\AͅÁA�~�A�z�A�z�A�x�A�r�A�r�A�n�A�l�A�l�A�l�A�l�A�l�A�jA�l�A�jA�hsA�ffA�hsA�ffA�hsA�hsA�ffA�ffA�ffA�ffA�ffA�dZA�bNA�O�AƁA���A��^A���A�VA�\)A� �A��wA��yA�^5A�^5A��
A��A��PA�Q�A�C�A�9XA��A��wA��
A��+A�Q�A���A�%A��#A��RA��yA�ĜA�  A�;dA��A�x�A��A��jA�n�A�-A�&�A��/A���A���A�/A�C�A�Az=qAwC�Au�Ao��Ai�#Ad��Ab��A_�;A]��A[�PAX�RAW;dAU�#ASoAP��AO�AJr�AFVAD=qAB�HA@ĜA>��A<�`A<�\A<��A;�A<-A<�A;��A9t�A6�RA6�uA5��A3�PA1�A1�^A2A�A2ĜA2-A1?}A.�A-+A,��A(��A& �A%S�A$��A"�A"=qA!�TA!�FA �/A {A��A��A��A=qAI�A�A��AffA�yA%A"�A�A~�A�A��A�7A"�A�`A��A^5A1A��Ap�A�AbNA��A;dA�/A�\AjA�A�A�^AdZA�AVA��A�An�AE�A1A��A?}AA�!AffA5?A  A��A��AK�A��AE�A�A�A�A=qA  Ax�A��An�AbA�#A��AK�A�A
~�A
=qA	��A	�A	\)A��A�jA��A�+An�A�Al�A+A%A��Ar�AM�A9XA �AbA�#A��AK�A��A9XA��A��A��A7LA�`A�uA�A�wA�7A ��A n�A I�A bA   @��F@��@�7L@��@�Ĝ@���@���@��T@��@�j@�9X@��m@���@�
=@��R@�~�@�{@��7@��`@��j@�A�@��@�dZ@�@���@�x�@��@��
@�S�@�
=@�$�@���@�x�@�?}@��/@�Ĝ@��@�@��T@�7L@�@�w@�C�@�"�@��y@��@�-@�p�@���@�Ĝ@�z�@��
@��@�+@�J@��@�?}@�j@߮@߅@�@�n�@�J@��T@ݲ-@�/@���@ܛ�@�Q�@��@�
=@١�@�/@���@�%@���@�j@׾w@�+@�ff@ղ-@Դ9@�S�@�n�@���@с@�O�@��@мj@мj@Гu@���@ϝ�@��y@�{@��@ͺ^@�`B@̬@�Z@�b@���@˅@���@��y@ʰ!@�n�@���@ɑh@�%@ȴ9@��@ǍP@�+@ƸR@�J@�p�@�?}@���@� �@�S�@¸R@�ff@�-@��@��-@�O�@��@��/@��D@��m@�@��\@�E�@�J@���@�7L@�V@��u@�1@��w@��P@�dZ@�K�@���@��@���@�r�@�9X@��@��y@���@�5?@���@�?}@��@���@�bN@�b@�ƨ@�|�@�o@��H@���@�ff@�=q@���@��#@���@�`B@��@��@���@��u@�1'@��@� �@��@�b@�b@�  @��;@��w@���@��P@��!@�~�@�@���@�/@�1@��@��@���@���@�\)@��@�@��@��R@���@�ff@�$�@��h@��@��/@��u@��D@��@�z�@�r�@�Q�@�9X@�1'@�(�@��@�b@�  @��@��;@�t�@�+@��y@�E�@���@�`B@�7L@�%@��j@��9@��`@��@�bN@�9X@��@�1@��@���@���@��R@���@���@��+@�M�@�J@��^@��7@�X@���@�r�@�9X@� �@�1@��P@�;d@��H@��@���@���@���@��^@�V@�Z@�  @���@��@�@�5?@��#@�x�@���@��@��F@���@�^5@���@�hs@�&�@��@��@�r�@�Z@��
@�|�@�;d@��H@���@��\@��\@��+@�{@�hs@��/@��9@��j@�9X@�ƨ@��@���@�v�@�V@�{@��^@���@��@�?}@���@��@�Q�@� �@�1@��;@���@�dZ@�;d@��H@�v�@�^5@�M�@�E�@��@��@���@�@��-@��7@���@��u@�A�@�b@�ƨ@���@�+@��!@��+@��@��-@��@�hs@�G�@�7L@��@���@���@��/@��@�b@��@�
=@���@���@���@�~�@�^5@�$�@��^@���@��@��@��@���@�j@�(�@�b@�@�P@�@~v�@~5?@~{@}�@}O�@}?}@}?}@|�/@|(�@|1@{�
@{��@{S�@{@z�!@z�@y�^@y7L@x1'@w�@v��@v5?@u��@u�@t9X@t1@s��@st�@s"�@r�\@r-@q�#@q��@qX@p��@p�u@p1'@o�;@o;d@n��@n�+@n5?@m�@m@m�-@m�-@m��@m�@l�@l(�@k��@k@i�^@h��@h�@g�;@g\)@f�@fv�@f5?@e��@e?}@eV@d�@d�j@dj@c�m@c��@c"�@b��@a��@a�#@a��@a�@`bN@`b@_�;@_�@_|�@_K�@_�@_
=@^�@^5?@]�-@]V@\�@\I�@[��@[@Z�!@Z��@Y��@YG�@YG�@Y7L@X��@X�9@XQ�@W��@W+@V�y@V�R@VV@U�-@Up�@U/@Tz�@TI�@T(�@S�F@S"�@S@R�H@R~�@R-@Q��@Q�^@Q�^@Q�^@Q�7@P��@O�P@N��@M�@M�-@MO�@L��@L��@LZ@K��@Kt�@KdZ@Ko@J��@J�@Ihs@H��@H��@H�@HbN@H  @G��@G�@F�y@F�+@E��@Ep�@E/@D(�@C�
@C�@CC�@B�@B��@BJ@A��@AG�@@�9@@r�@@bN@@Q�@@Q�@@Q�@@A�@?�@?�@?
=@>�y@>ȴ@>��@>ff@=@=`B@=V@<�@<�@;o@:�H@:n�@:-@9��@9�^@9�@8��@8��@8�`@8Ĝ@8�@8bN@8Q�@8 �@81'@7��@7;d@6��@6E�@6$�@5@5�@5�@4��@4�@4�@4j@49X@4(�@41@3dZ@2��@2=q@1��@1�@1�#@1�^@1�^@1��@1��@1��@1��@1��@1��@1hs@0�`@0b@/|�@/�@.�y@.��@-��@-O�@,��@,��@,��@,�@,�/@,(�@+�@+dZ@+33@+@*��@*M�@*-@)��@)��@)��@)�7@)x�@)X@)G�@)7L@)�@(�9@(Q�@(A�@( �@(  @(  @'�;@'��@'K�@'
=@&�R@&v�@&@%��@%�@%�@%�@%�@%O�@$�@$�@#ƨ@#dZ@#@"��@"=q@!�@!��@!��@!�7@!G�@ ��@ ��@ �9@ �@ bN@ bN@ 1'@�@�P@\)@+@��@�R@ff@@��@��@?}@��@z�@j@�@��@S�@33@o@@@�@�H@�HAΉ7A΅AΉ7A΃A·+A΃A΋DA΍PA΍PA΋DA΋DA΋DA΋DA΍PA�dZA�^5A�K�A�5?A�9XA�5?A�&�A�-A�$�A�/A�-A�&�A��A���A���A���A��mA��A��TA���A�Aͺ^AͶFAͶFAͰ!Aͩ�Aͩ�A͡�A͡�A͡�A͝�A͛�A͛�A͗�A͕�A͓uA͏\A͍PA͋DA͉7A͋DA͇+A͇+A͇+A̓ÁA̓ÁA�~�A̓A�~�A�~�ÁA�|�A�~�ÁA�~�A�~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         A·+A΋DA΋DA�n�A�7LA�+A��A��A���AͶFAͬA͟�A͏\AͅÁA�~�A�z�A�z�A�x�A�r�A�r�A�n�A�l�A�l�A�l�A�l�A�l�A�jA�l�A�jA�hsA�ffA�hsA�ffA�hsA�hsA�ffA�ffA�ffA�ffA�ffA�dZA�bNA�O�AƁA���A��^A���A�VA�\)A� �A��wA��yA�^5A�^5A��
A��A��PA�Q�A�C�A�9XA��A��wA��
A��+A�Q�A���A�%A��#A��RA��yA�ĜA�  A�;dA��A�x�A��A��jA�n�A�-A�&�A��/A���A���A�/A�C�A�Az=qAwC�Au�Ao��Ai�#Ad��Ab��A_�;A]��A[�PAX�RAW;dAU�#ASoAP��AO�AJr�AFVAD=qAB�HA@ĜA>��A<�`A<�\A<��A;�A<-A<�A;��A9t�A6�RA6�uA5��A3�PA1�A1�^A2A�A2ĜA2-A1?}A.�A-+A,��A(��A& �A%S�A$��A"�A"=qA!�TA!�FA �/A {A��A��A��A=qAI�A�A��AffA�yA%A"�A�A~�A�A��A�7A"�A�`A��A^5A1A��Ap�A�AbNA��A;dA�/A�\AjA�A�A�^AdZA�AVA��A�An�AE�A1A��A?}AA�!AffA5?A  A��A��AK�A��AE�A�A�A�A=qA  Ax�A��An�AbA�#A��AK�A�A
~�A
=qA	��A	�A	\)A��A�jA��A�+An�A�Al�A+A%A��Ar�AM�A9XA �AbA�#A��AK�A��A9XA��A��A��A7LA�`A�uA�A�wA�7A ��A n�A I�A bA   @��F@��@�7L@��@�Ĝ@���@���@��T@��@�j@�9X@��m@���@�
=@��R@�~�@�{@��7@��`@��j@�A�@��@�dZ@�@���@�x�@��@��
@�S�@�
=@�$�@���@�x�@�?}@��/@�Ĝ@��@�@��T@�7L@�@�w@�C�@�"�@��y@��@�-@�p�@���@�Ĝ@�z�@��
@��@�+@�J@��@�?}@�j@߮@߅@�@�n�@�J@��T@ݲ-@�/@���@ܛ�@�Q�@��@�
=@١�@�/@���@�%@���@�j@׾w@�+@�ff@ղ-@Դ9@�S�@�n�@���@с@�O�@��@мj@мj@Гu@���@ϝ�@��y@�{@��@ͺ^@�`B@̬@�Z@�b@���@˅@���@��y@ʰ!@�n�@���@ɑh@�%@ȴ9@��@ǍP@�+@ƸR@�J@�p�@�?}@���@� �@�S�@¸R@�ff@�-@��@��-@�O�@��@��/@��D@��m@�@��\@�E�@�J@���@�7L@�V@��u@�1@��w@��P@�dZ@�K�@���@��@���@�r�@�9X@��@��y@���@�5?@���@�?}@��@���@�bN@�b@�ƨ@�|�@�o@��H@���@�ff@�=q@���@��#@���@�`B@��@��@���@��u@�1'@��@� �@��@�b@�b@�  @��;@��w@���@��P@��!@�~�@�@���@�/@�1@��@��@���@���@�\)@��@�@��@��R@���@�ff@�$�@��h@��@��/@��u@��D@��@�z�@�r�@�Q�@�9X@�1'@�(�@��@�b@�  @��@��;@�t�@�+@��y@�E�@���@�`B@�7L@�%@��j@��9@��`@��@�bN@�9X@��@�1@��@���@���@��R@���@���@��+@�M�@�J@��^@��7@�X@���@�r�@�9X@� �@�1@��P@�;d@��H@��@���@���@���@��^@�V@�Z@�  @���@��@�@�5?@��#@�x�@���@��@��F@���@�^5@���@�hs@�&�@��@��@�r�@�Z@��
@�|�@�;d@��H@���@��\@��\@��+@�{@�hs@��/@��9@��j@�9X@�ƨ@��@���@�v�@�V@�{@��^@���@��@�?}@���@��@�Q�@� �@�1@��;@���@�dZ@�;d@��H@�v�@�^5@�M�@�E�@��@��@���@�@��-@��7@���@��u@�A�@�b@�ƨ@���@�+@��!@��+@��@��-@��@�hs@�G�@�7L@��@���@���@��/@��@�b@��@�
=@���@���@���@�~�@�^5@�$�@��^@���@��@��@��@���@�j@�(�@�b@�@�P@�@~v�@~5?@~{@}�@}O�@}?}@}?}@|�/@|(�@|1@{�
@{��@{S�@{@z�!@z�@y�^@y7L@x1'@w�@v��@v5?@u��@u�@t9X@t1@s��@st�@s"�@r�\@r-@q�#@q��@qX@p��@p�u@p1'@o�;@o;d@n��@n�+@n5?@m�@m@m�-@m�-@m��@m�@l�@l(�@k��@k@i�^@h��@h�@g�;@g\)@f�@fv�@f5?@e��@e?}@eV@d�@d�j@dj@c�m@c��@c"�@b��@a��@a�#@a��@a�@`bN@`b@_�;@_�@_|�@_K�@_�@_
=@^�@^5?@]�-@]V@\�@\I�@[��@[@Z�!@Z��@Y��@YG�@YG�@Y7L@X��@X�9@XQ�@W��@W+@V�y@V�R@VV@U�-@Up�@U/@Tz�@TI�@T(�@S�F@S"�@S@R�H@R~�@R-@Q��@Q�^@Q�^@Q�^@Q�7@P��@O�P@N��@M�@M�-@MO�@L��@L��@LZ@K��@Kt�@KdZ@Ko@J��@J�@Ihs@H��@H��@H�@HbN@H  @G��@G�@F�y@F�+@E��@Ep�@E/@D(�@C�
@C�@CC�@B�@B��@BJ@A��@AG�@@�9@@r�@@bN@@Q�@@Q�@@Q�@@A�@?�@?�@?
=@>�y@>ȴ@>��@>ff@=@=`B@=V@<�@<�@;o@:�H@:n�@:-@9��@9�^@9�@8��@8��@8�`@8Ĝ@8�@8bN@8Q�@8 �@81'@7��@7;d@6��@6E�@6$�@5@5�@5�@4��@4�@4�@4j@49X@4(�@41@3dZ@2��@2=q@1��@1�@1�#@1�^@1�^@1��@1��@1��@1��@1��@1��@1hs@0�`@0b@/|�@/�@.�y@.��@-��@-O�@,��@,��@,��@,�@,�/@,(�@+�@+dZ@+33@+@*��@*M�@*-@)��@)��@)��@)�7@)x�@)X@)G�@)7L@)�@(�9@(Q�@(A�@( �@(  @(  @'�;@'��@'K�@'
=@&�R@&v�@&@%��@%�@%�@%�@%�@%O�@$�@$�@#ƨ@#dZ@#@"��@"=q@!�@!��@!��@!�7@!G�@ ��@ ��@ �9@ �@ bN@ bN@ 1'@�@�P@\)@+@��@�R@ff@@��@��@?}@��@z�@j@�@��@S�@33@o@@@�@�HG�O�AΉ7A΅AΉ7A΃A·+A΃A΋DA΍PA΍PA΋DA΋DA΋DA΋DA΍PA�dZA�^5A�K�A�5?A�9XA�5?A�&�A�-A�$�A�/A�-A�&�A��A���A���A���A��mA��A��TA���A�Aͺ^AͶFAͶFAͰ!Aͩ�Aͩ�A͡�A͡�A͡�A͝�A͛�A͛�A͗�A͕�A͓uA͏\A͍PA͋DA͉7A͋DA͇+A͇+A͇+A̓ÁA̓ÁA�~�A̓A�~�A�~�ÁA�|�A�~�ÁA�~�A�~�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	%�B	%�B	%zB	&�B	&�B	$�B	%zB	$tB	$�B	#B	#B	"�B	"�B	"4B	"hB	"hB	"4B	"hB	"hB	"hB	"hB	"4B	"hB	"4B	"4B	"hB	"�B	"�B	"�B	"�B	"�B	"�B	"�B	"�B	"�B	"�B	#B	#B	#B	#B	#:B	#�B	#�B	&�B
�.B
��B
�sB
��B
�oB	�BB�B�B%B3�BM6Bh�By>B�B�GB|Bz�B��B��B�{B��B��BgBOB1�B5B!bBMBxB
�cB
�dB
��B
�RB
�B
��B
S�B
(�B
�B
�B	�cB	��B	�/B	��B	��B	��B	��B	m�B	R B	EB	;�B	/�B	)�B	!�B	B	�B	{B	�B	�B�]B		lB	�B	&B	D�B	kQB	�OB	��B	��B	�3B	�DB
B
-CB
'�B
_B
kB
%�B
�B
 B
�B
"�B
9XB
7LB
0!B
"4B
�B
�B	��B	��B	��B	�fB	�B	�&B	�fB	�B	�`B	�B	�B	�B	�)B	�yB	�B	��B	�B
 �B
!�B
(�B
,�B
1[B
2�B
8�B
8�B
<jB
@OB
@�B
B'B
B'B
B�B
@�B
?�B
?�B
FB
I�B
I�B
L�B
MjB
NpB
O�B
P�B
R�B
R�B
S�B
S�B
T,B
TaB
TaB
TaB
TaB
T�B
T�B
S�B
R�B
O�B
NpB
MjB
O�B
S�B
R�B
O�B
MB
H�B
EmB
B�B
CaB
D�B
EB
B�B
B[B
B�B
CaB
EB
C�B
F?B
C�B
A�B
AUB
A�B
@�B
AUB
@�B
@�B
@OB
?HB
?}B
>B
=<B
=B
>B
=B
>wB
?B
?B
>�B
?}B
>�B
>wB
=�B
;�B
;�B
:�B
:�B
;0B
:�B
:�B
;�B
;�B
<jB
;�B
7�B
9�B
9$B
7�B
7�B
6zB
2�B
2-B
2�B
1[B
/�B
.IB
,qB
+�B
+6B
*�B
)�B
*�B
)*B
)_B
(�B
(XB
'RB
&B
&�B
$B
"�B
"�B
!�B
 \B
 'B
!-B
�B
�B
B
CB
qB
�B
�B
xB
�B
�B
�B
1B
_B
1B
�B
+B
�B
_B
+B
$B
�B
B
�B
B
MB
�B
�B
B
�B
�B
�B
@B
@B
B
�B
.B
bB
�B
�B
�B
�B
VB
�B
PB
JB
~B
\B
�B
�B
(B
"B
�B
�B
B
PB
�B
B

=B
	�B
	�B
	7B
	B
	B
	�B
�B

rB

	B

	B

=B

�B
DB

�B
B
DB
�B
JB
�B
B
B
�B
B
�B
xB
JB
B
B
�B
�B
�B
DB

�B
�B
�B
JB
B
B
xB
�B
B
JB
B
JB
~B
�B
�B
~B
~B
�B
~B
JB
�B
~B
�B
JB
B
�B
B
�B
PB
�B
B
~B
~B
JB
�B
�B
�B
�B
~B
PB
�B
B
�B
B
B
�B
B
B
�B
B
�B
"B
�B
"B
"B
(B
(B
�B
hB
�B
�B
�B
�B
oB
:B
:B
B
oB
hB
�B
B
uB
�B
uB
�B
uB
B
@B
uB
@B
B
B
B
�B
@B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
�B
_B
_B
�B
+B
+B
+B
�B
�B
�B
�B
�B
�B
1B
�B
�B
qB
xB
CB
B
IB
B
IB
�B
�B
VB
!B
!B
�B
�B
!B
 �B
 �B
 \B
 �B
 'B
 \B
 'B
!bB
!�B
"hB
"�B
"�B
!�B
!�B
"�B
#�B
%zB
$tB
$@B
%FB
%B
%�B
&B
$�B
$tB
"hB
"hB
�B
�B
!B
�B
 �B
 �B
 �B
 'B
�B
!B
!-B
 �B
!-B
!�B
!�B
#�B
$B
$B
&LB
%FB
$�B
%zB
'B
&�B
%B
%�B
%�B
&LB
'RB
'�B
(�B
(�B
(�B
)_B
)�B
*�B
*0B
)�B
)�B
)�B
*eB
*eB
+kB
,B
,�B
,�B
,�B
,�B
-wB
-CB
-�B
-�B
-wB
-�B
/�B
/�B
/�B
0!B
0!B
/�B
1'B
0�B
0�B
2-B
2�B
2�B
2�B
3hB
33B
33B
3hB
2�B
2�B
2�B
3hB
3�B
4�B
4�B
4�B
5B
5?B
5?B
5�B
6�B
6FB
6zB
7LB
7B
7�B
7�B
8B
8RB
8�B
8�B
9�B
:*B
:*B
:*B
;0B
:�B
:�B
:^B
:�B
;0B
:�B
:�B
;0B
;0B
:�B
:�B
;0B
;0B
:�B
:�B
:�B
;0B
;�B
<jB
=<B
>B
=qB
=qB
=<B
>B
?HB
?HB
?HB
?HB
?�B
?�B
?�B
?�B
@B
@OB
A B
@�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
A�B
B�B
C�B
CaB
E9B
E�B
F?B
F?B
F�B
F�B
GEB
G�B
G�B
GEB
G�B
G�B
HKB
HKB
HKB
I�B
IRB
I�B
IRB
J#B
J�B
J�B
J�B
J�B
K)B
K)B
K)B
K)B
J�B
K�B
K�B
LdB
L0B
MB
MjB
M�B
M�B
MjB
N�B
NpB
NpB
N�B
N�B
NpB
N�B
NpB
OB
N�B
N�B
OB
OB
N�B
OB
PHB
PHB
P�B
Q�B
R B
Q�B
Q�B
R B
RTB
R B
R B
R B
R B
Q�B
R�B
R�B
R�B
S&B
R�B
S&B
S�B
S�B
TaB
T�B
U�B
U�B
VmB
VmB
V�B
WsB
W�B
W�B
WsB
W�B
W�B
W�B
XEB
XEB
X�B
X�B
X�B
YB
ZQB
ZB
ZQB
Z�B
Z�B
Z�B
[WB
[#B
[�B
\)B
\)B
[�B
\)B
[�B
[�B
[�B
\�B
\�B
\�B
\�B
\�B
\�B
\�B
]dB
]�B
]�B
]dB
^5B
^jB
^5B
^�B
^�B
^�B
_B
_pB
_;B
_;B
_pB
_pB
_�B
_�B
_�B
_�B
_;B
_�B
`vB
`�B
`�B
`�B
aHB
aHB
a�B
a�B
a�B
a�B
bB
bB
a�B
bB
c B
c�B
c�B
c�B
d&B
d&B
d&B
dZB
dZB
d&B
d&B
d&B
d&B
d&B
dZB
d�B
e�B
e�B
e�B
e�B
e�B
g8B
g8B
gmB
g8B
g8B
gB
g8B
h>B
hsB
hsB
h�B
h�B
iB
iyB
iyB
jB
jB
i�B
i�B
jB
jB
jB
jB
jB
jB
j�B
j�B
j�B
j�B
j�B
kB
kQB
k�B
k�B
l"B
lWB
l�B
l�B
m)B
l�B
l�B
l�B
l�B
m�B
m�B
n/B
ncB
n�B
o5B
o�B
o�B
o�B
o�B
o�B
pB
p;B
p;B
poB
p�B
p�B
poB
p�B
p�B
q�B
q�B
q�B
q�B
rB
r|B
r�B
sB
sB
sMB
s�B
tB
s�B
t�B
uZB
u%B
uZB
u�B
u�B
v`B
v�B
v�B
w2B	%B	'B	%�B	'RB	$�B	'RB	#�B	%�B	&�B	%�B	%B	%B	%B	$�B	-�B	)�B	%�B	-�B	"�B	%zB	&�B	#nB	&�B	"�B	%zB	%FB	&�B	%FB	#�B	$B	%�B	#:B	&B	%zB	"�B	#nB	#�B	"hB	#nB	#�B	"4B	#:B	"�B	"hB	#�B	"�B	!�B	#:B	!�B	!�B	#�B	"�B	"4B	"�B	 �B	"hB	"�B	!�B	"�B	"�B	!�B	"�B	#B	!�B	"�B	"�B	!�B	"�B	"hB	!�B	"�B	"hG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         B	%�B	&!B	&�B	(qB	'%B	%�B	'
B	%�B	%�B	#�B	#wB	#7B	"�B	"`B	"�B	"�B	"KB	"�B	"�B	"~B	"�B	"SB	"yB	"FB	"GB	"{B	"�B	"�B	"�B	"�B	"�B	"�B	"�B	"�B	"�B	"�B	#B	#B	#B	#B	#PB	#�B	$B	0�B
��B
�B
�pB
ܡB
��BMB�BaBhB+wB=YBWBr"B��B�4B�HB��B�UB��B��B�B��B�	Bz�B\UB>OBF�B'
B�B �BB
�B
�sB
��B
��B
��B
bWB
0:B
�B

:B
�B	��B	�B	�B	�QB	��B	��B	y�B	W-B	LB	A#B	5B	0�B	%�B	!�B	!3B	�B	�B	�B	�B	�B	*B	+{B	J%B	o�B	��B	��B	�KB	��B	�B
 %B
2�B
.B
B
�B
*�B
!�B
�B
gB
!ZB
;�B
;�B
9�B
'`B
RB
)�B
B	�CB	�gB	�JB	��B	�B	�B	��B	�B	�BB	�B	��B	��B	�^B	��B	�?B	�B	�B
 �B
(vB
-�B
3B
4�B
9�B
9�B
=�B
ABB
B B
C'B
C�B
D>B
A�B
A_B
B�B
H�B
K&B
KcB
M�B
NB
O�B
P�B
Q�B
T0B
S�B
TB
TB
UAB
UXB
UB
UgB
U�B
V3B
VB
UAB
S�B
P~B
OGB
N/B
P�B
U;B
T�B
RB
P'B
K#B
G*B
D7B
D�B
GB
G�B
DB
C�B
C�B
DcB
FYB
D�B
H�B
D�B
CvB
B�B
BEB
B:B
B_B
AbB
A;B
@�B
AkB
AqB
?*B
=�B
>B
?gB
=�B
>�B
?{B
?rB
?�B
@tB
@]B
A'B
?JB
<�B
<gB
;�B
<"B
<�B
<rB
<�B
=FB
<�B
>�B
=�B
8zB
:�B
9�B
8�B
;LB
7�B
3B
3B
4�B
30B
1�B
/�B
-�B
,B
+�B
+=B
+<B
+MB
)�B
*TB
*(B
)�B
'�B
'iB
'�B
$�B
$#B
%?B
"�B
!�B
#B
"xB
�B
 �B
MB
�B
B
�B
UB
�B
!pB
qB
�B
B
IB
PB
B
�B
dB
�B
�B
SB
'B
B
PB
HB
}B
!B
�B
iB
�B
�B
�B
�B
�B
�B
SB
�B
�B
IB
�B
�B
MB
�B
�B
~B
�B
�B
B
 B
�B
�B
,B
�B
�B
kB
�B
xB
�B

�B

�B

NB
	bB
	�B

�B

�B

�B
XB

�B

�B
_B
RB
 B
xB
�B
#B
B
�B
�B
�B

B
oB
�B
|B
�B
8B
�B
�B
B
�B
B
�B
UB
;B
�B
�B
�B
~B
�B
VB
�B
�B
�B
mB
�B
�B
5B
�B
FB
�B
�B
$B
�B
B
B
�B
XB
B
B
�B
-B
`B
PB
�B
B
B
�B
�B
vB
$B
GB
�B
sB
�B
5B
}B
�B
�B
�B
�B
8B
�B
WB
�B
ZB
jB
�B
�B
dB
�B
|B
�B
�B
�B
B
�B
zB
�B
�B
�B
lB
�B
=B
�B
aB
�B
�B
�B
�B
�B
�B
�B
ZB
]B
~B
~B
qB
�B
oB
B

B
B
B
B
9B
�B
�B
�B
B
xB
�B
�B
lB
B
�B
:B
XB
�B
EB
�B
=B
�B
�B
�B
�B
B
�B
^B
wB
_B
B
�B
 EB
�B
<B
TB
pB
�B
�B
!B
!AB
!+B
"	B
 �B
 �B
 zB
"^B
"�B
#FB
$B
#gB
!�B
"B
##B
%IB
&�B
%=B
$�B
%�B
&�B
'JB
&�B
%�B
%�B
$AB
#XB
!XB
 �B
 NB
 �B
![B
!HB
!�B
 �B
 HB
 <B
!�B
!�B
!�B
",B
"B
#�B
$>B
%#B
'�B
&_B
%?B
%�B
(3B
'�B
&�B
&�B
&B
&�B
'�B
(�B
(�B
(�B
)jB
*B
+B
+B
*�B
*GB
*B
*�B
*�B
*�B
,MB
,�B
-B
-B
- B
-B
-�B
-�B
-�B
-�B
.B
/8B
0�B
0�B
0wB
0�B
0�B
1B
2CB
1|B
1�B
3#B
3B
3B
3PB
3�B
3�B
3�B
3yB
3QB
3�B
4QB
4�B
4�B
5ZB
4�B
5!B
5vB
5�B
5�B
6�B
7:B
6�B
7bB
7�B
7�B
8B
8�B
8cB
8�B
9
B
9�B
:JB
:�B
:oB
:�B
;hB
:�B
:�B
:�B
;�B
;gB
;BB
;TB
;�B
;�B
;tB
;�B
;�B
<B
<B
;oB
;�B
;�B
<mB
=2B
>HB
>aB
=�B
=�B
=�B
>�B
?�B
?�B
?�B
?�B
@]B
@KB
@nB
@=B
@�B
@�B
A�B
A_B
A�B
A�B
B
B
A�B
A�B
A�B
B_B
B�B
B�B
B�B
DcB
D�B
C�B
FB
F�B
F�B
F�B
GBB
G<B
G�B
G�B
G�B
G�B
H-B
H�B
H�B
H�B
IB
JKB
I�B
I�B
JB
J�B
J�B
J�B
K
B
K<B
KmB
KlB
KRB
K�B
K�B
L�B
L�B
L�B
MB
M�B
N(B
NB
M�B
NuB
OrB
N�B
N�B
OB
N�B
OB
O^B
OAB
OgB
N�B
OB
O�B
OVB
O)B
O�B
P{B
PtB
Q(B
RB
RGB
RB
RNB
RwB
R�B
R7B
R&B
R*B
RcB
R�B
S�B
S�B
S�B
SoB
S$B
S�B
S�B
TB
T�B
UIB
U�B
V)B
V�B
V�B
W�B
W�B
W�B
W�B
W�B
XB
XDB
X]B
XB
X�B
YZB
YKB
Y6B
ZB
Z�B
ZsB
Z�B
Z�B
Z�B
[;B
[�B
[�B
\B
\iB
\>B
\	B
\-B
[�B
\B
\�B
])B
]B
\�B
]$B
\�B
]SB
]�B
]�B
]�B
^B
^"B
_@B
^�B
^�B
^�B
^�B
_,B
_�B
_�B
_CB
_TB
_�B
_�B
_�B
_�B
_�B
_�B
_�B
`�B
a(B
a:B
`�B
aB
a�B
a�B
a�B
a�B
a�B
b1B
bQB
b5B
b#B
b�B
c�B
c�B
d8B
dB
d>B
dJB
d.B
dnB
d^B
d+B
d*B
d-B
dCB
dnB
d�B
e|B
f6B
f:B
fB
f)B
f�B
g�B
g�B
grB
g?B
gRB
g5B
hB
h�B
h�B
h�B
h�B
iB
i�B
i�B
i�B
j?B
j.B
i�B
i�B
j?B
j0B
j3B
jMB
j�B
j�B
kB
kB
kB
j�B
kB
kqB
k�B
lB
k�B
l}B
l�B
mdB
mB
m-B
l�B
l�B
l�B
mQB
nsB
nhB
n�B
n�B
oB
o�B
o�B
o�B
pB
o�B
p(B
pjB
phB
pjB
p�B
p�B
p�B
p�B
q0B
qKB
q�B
q�B
q�B
r8B
r|B
r�B
r�B
s_B
s�B
s�B
tB
tCB
t^B
uRB
u�B
uUB
u�B
u�B
u�B
vwB
v�B
wG�O�B	%B	'B	%�B	'RB	$�B	'RB	#�B	%�B	&�B	%�B	%B	%B	%B	$�B	-�B	)�B	%�B	-�B	"�B	%zB	&�B	#nB	&�B	"�B	%zB	%FB	&�B	%FB	#�B	$B	%�B	#:B	&B	%zB	"�B	#nB	#�B	"hB	#nB	#�B	"4B	#:B	"�B	"hB	#�B	"�B	!�B	#:B	!�B	!�B	#�B	"�B	"4B	"�B	 �B	"hB	"�B	!�B	"�B	"�B	!�B	"�B	#B	!�B	"�B	"�B	!�B	"�B	"hB	!�B	"�B	"hG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<-x<#�
<M��<#�
<+��<$Fz<#�
<#�
<#�
<#�
<#�
<)�g<4��<%��<#�
<#�
<#�
<#�
<��i<B�<#�
<#�
<2��<��_<� �<aY�<V>�<�3�<#�
<#�
<��Z<#�
<#�
<Vj<u�<���<�<ro<#�
<#�
<#�
<#�
<N��<E�Y<#�
<#�
<h�m<b��<PU�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<@�<1�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<-�4<#�
<#�
<oΘ<&��<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202204261606322022042616063220220426160632202204261606322022042616063220220426160632SI  SI  ARFMARFM                                                                                                                                                2021012102423320210121024233IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021013103004720210131030047QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021013103004720210131030047QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2022042213364420220422133644IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022042616063920220426160639IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022042616063920220426160639IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022042616063920220426160639IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                