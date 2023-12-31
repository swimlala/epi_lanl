CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-08-06T19:16:57Z creation; 2022-02-04T23:30:02Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  \�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  d|   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �h   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � �   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 9�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � Al   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � `�   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � h�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` �p   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ��   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �$   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �,   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �4   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �<   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �D   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        �   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        �   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       �   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20210806191657  20220204223515  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_178                 6810_008521_178                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @ى����{@ى����{11  @ى�Ov@ى�Ov@2s(�z@2s(�z�d�`��:�d�`��:11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@   @B�\@��\@�  @�  @޸RA   A  A�RA,(�A@��A`��A���A���A���A�  A�  A�  A߮A�  B z�B(�B  B�
B�B(  B0(�B7�B?�
BH  BP  BX  B_�
Bg�
Bp  BxQ�B�{B�{B�  B��B�  B�  B�  B�{B�  B�  B�  B�{B�(�B�  B�{B�  B��B�(�B�  B�  B��B��
B��B�  B�  B�  B�{B�  B��B�  B�{B�{C 
=C  C��C��C
=C
  C�C��C��C��C  C��C  C  C
=C�C {C"
=C$  C&
=C(
=C*{C,
=C.
=C0  C1��C3��C6{C8
=C9��C<  C>  C@
=CB{CD{CF  CG��CJ  CL
=CN
=CP
=CR
=CS��CU��CX  CZ  C\
=C^
=C`  Ca��Cc��Cf  Ch  Cj
=Cl  Cm��Cp  Cr{Ct  Cv  Cx
=Cy��C|  C~
=C��C���C�C���C�  C�  C�C���C�C�  C�  C���C���C���C���C���C�  C�  C�C�C�C���C���C�  C�  C�C���C�C�  C�C�C�  C�  C�  C�  C�  C�  C�C���C���C�C�  C�C�  C���C���C�  C���C���C�C�  C�  C�  C���C�  C�  C�C�C�  C�  C�C�  C�  C�  C�C�C�
=C�C���C���C�C�  C���C�  C�  C�  C�C�C�  C�  C�
=C�C�  C���C���C�  C�C�C�  C�  C�C�  C�
=C���C�C�C�  C�  C�C�C�C�  C�
=C�C�  C�
=C���C�C�  C�  C�C�
=C�  C���C�C���C�  C�C�  C�  C�C�C�C�  C�  C�  C���C�  D �D �DD��DD�D  Dz�D�qD� D  D}qD  D��D  D� D  D}qD��D	}qD
  D
}qD�D�D�D��D�qD}qD�qD� D  D��D�D��D�D� D�qDz�D�qD}qD  D��D  D��D�qDz�D  D��D�qD� DD��D  D� D��D� D�D� D�qDz�D�qD}qD�qD� D �D ��D ��D!z�D!��D"� D#  D#� D$  D$��D%D%��D&  D&� D'�D'� D'�qD(� D)D)��D*  D*� D+D+��D,  D,��D-  D-��D.D.� D.�qD/z�D/�qD0� D1�D1}qD1�qD2� D3  D3z�D4  D4}qD5  D5� D6  D6��D7�D7��D8�D8� D8�qD9}qD9�qD:� D;  D;��D<  D<� D=D=��D=�qD>}qD?�D?��D@  D@�DADA��DB  DB�DC�DC� DD�DD��DE  DE��DF�DF� DF�qDG}qDH  DH��DI  DI� DI�qDJ}qDK  DK�DLDL��DM�DM}qDN  DN}qDN��DO� DP  DP� DQ  DQ� DR�DR��DS�DS}qDS�qDT� DU  DU��DV  DV��DW  DW� DX  DX}qDX�qDY}qDZ�DZ��D[�D[��D[��D\� D]�D]}qD^  D^��D_�D_� D_�qD`}qDa�Da��Db  Db}qDb�qDc��Dd  Dd��De�De��DfDf}qDg  Dg� Dh�Dh��Dh�qDi��Dj�Dj��Dk  Dk}qDk�qDl}qDm  Dm� Dn  Dn� Do�Do� Dp  Dp� Dq  Dq� Dr  Dr�DsDs}qDt  Dt� Dt��Duz�Du�qDv� Dv�qDw}qDw�qDx� Dy  Dyz�Dz  Dz�D{D{�D|�D|�D}  D}z�D}�qD~� D  D}qD�HD�>�D�}qD���D���D�>�D�~�D��HD�HD�>�D�~�D��HD��D�@ D�}qD�� D�HD�@ D�~�D���D�HD�@ D��HD�� D���D�>�D�~�D�� D�  D�@ D�� D��HD��D�AHD��HD��HD�HD�B�D�~�D�� D�HD�>�D�� D�� D���D�@ D���D��HD�  D�>�D�� D�� D�HD�>�D�~�D���D�  D�AHD��HD��HD�HD�AHD��HD��HD���D�>�D�}qD���D�HD�>�D�� D��HD�HD�@ D�}qD��qD���D�>�D�}qD���D�HD�>�D�~�D��HD�HD�B�D�� D���D���D�>�D��HD��HD�  D�@ D�~�D�� D�HD�B�D��HD�� D�  D�@ D��HD�� D�  D�AHD�� D��HD�  D�=qD�~�D�� D��D�AHD�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D���D�>�D�� D�D�  D�=qD�~�D���D���D�>�D�� D�D�HD�>�D�� D�� D�  D�@ D�}qD��qD���D�AHD��HD�� D�  D�@ D��HD���D��qD�@ D�� D���D�  D�@ D�~�D�� D�HD�@ D��HD�� D�  D�>�D��HD��HD�  D�AHD��HD�� D�  D�@ D�� D��HD�HD�AHD��HD���D���D�>�D�~�D�� D�  D�@ D�� D���D���D�@ D�� D�� D���D�=qD�~�D�� D�  D�AHD�~�D���D�  D�AHD���D��HD�  D�AHD���D��HD�  D�@ D�� D�� D��D�B�D�� D�� D�  D�@ D���D��HD���D�>�D��HD��HD���D�@ D�� D���D���D�AHD��HD�� D��D�AHD�� D���D���D�@ D�~�D���D�  D�@ D�~�D���D�  D�AHD�~�D���D�  D�AHD��HD�� D�  D�B�D��HD���D���D�>�D�~�D���D�  D�@ D�� D�� D���D�@ DHD��HD�HD�>�D�~�Dþ�D�  D�AHD�~�D�� D�HD�B�Dŀ Dž�D���D�@ Dƀ DƽqD��qD�>�D�~�D�� D�  D�@ DȀ D��HD��D�AHDɀ Dɾ�D�  D�@ Dʀ Dʾ�D���D�>�D�}qD�� D���D�=qD�~�D��HD�HD�>�D̀ D�� D�HD�AHD�~�Dξ�D���D�@ DρHD��HD�  D�@ DЀ D�� D�  D�@ Dр DѾ�D��qD�>�D�~�D�� D�HD�B�DӁHD�� D�HD�B�DԂ�D�� D��qD�@ DՁHD��HD�  D�>�Dր D־�D�  D�>�D�~�D׾�D�HD�AHD؀ D�� D�  D�@ D�~�D�� D�  D�=qD�~�Dھ�D���D�=qDۀ D��HD���D�AHD�~�D�� D�HD�>�D�}qD�� D�HD�AHD�~�D�� D�  D�>�D߀ D߾�D��qD�@ D��HD��HD��D�AHD�HD�� D���D�>�D� D��HD�  D�@ D� D��HD�HD�>�D� D��HD�HD�B�D�HD�� D�  D�AHD�~�D澸D�HD�AHD�~�D羸D�  D�@ D�~�D�qD�  D�AHD� D龸D�  D�AHD�HD꾸D�  D�>�D�~�D뾸D���D�@ D�HD�� D�HD�AHD�~�D�� D���D�@ D�HD�� D�  D�AHD�HD�� D���D�@ D���D��HD���D�>�D� D�� D�  D�@ D�~�D�D�HD�@ D�~�D�D��qD�>�D�}qD��qD��qD�@ D��HD���D�  D�AHD�� D���D���D�>�D�� D�� D�  D�@ D�� D�� D���D�>�D��HD��HD�  D�>�D�� D���D��
>�?B�\?��?��
?Ǯ?��@�@��@5@B�\@Q�@n{@�G�@��@��@��R@���@���@��H@�=q@��@��H@�@�z�@�p�A33A
=qA\)A�
A��A{A!�A(Q�A-p�A1G�A7
=A<(�A@  AE�AK�AO\)AS�
AZ=qA]p�Aa�AhQ�Al��Ap��AvffA{�A\)A�=qA��A�
=A�G�A�z�A�ffA�  A��A�p�A�\)A�=qA���A��RA���A��
A�A��A��\A���A�ffA���A��A�p�A�Q�A��\A�(�A��RA�G�A��HA�p�A�  A��A˅A�ffA�Q�A�=qA��
A�ffAأ�A�=qA�(�A�
=A�G�A��HA���A�\)A�G�A�\A�p�A�\)A�G�A�(�A�{A��A��HA���A��RB ��B{B
=B  Bp�B�\B�B��B
{B
�HB(�B��B�\B�B��B�B�HB(�Bp�B=qB\)B��B�B�RBQ�B�B{B\)B ��B!G�B"�RB$  B$��B&{B'\)B((�B)p�B*�RB+�B,��B.{B.�HB/�
B1�B2{B2�HB4(�B5p�B5�B7
=B8z�B9G�B:{B;\)B<z�B=�B>�RB?�B@z�BA��BB�HBC�BD��BE�BG
=BG�BH��BI�BK
=BK�BL��BN=qBN�HBP  BQG�BR{BS
=BTz�BU�BV{BW33BXQ�BX��BZffB[�B\(�B]p�B^�\B_33B`z�Ba��Bb=qBc�Bd��Be��BfffBg�Bh��Bip�BjffBk�
Bl��Bmp�Bn�RBp  Bp��Bq�Bs33Bt  Bt��Bv=qBw\)Bx  ByG�Bz�\B{\)B|Q�B}��B~�HB�B�=qB���B�\)B�B�z�B�
=B�p�B��B��\B�33B��B�(�B���B��B�{B�z�B�33B��
B�=qB��RB�p�B�  B�z�B���B��B�Q�B��RB�33B��B��\B���B�p�B�(�B���B�\)B�B�Q�B�
=B���B�{B���B�G�B�  B��\B�
=B���B�ffB��HB�\)B�{B���B�\)B��B�ffB�33B��
B�Q�B���B�p�B�(�B���B�G�B��
B���B�G�B��
B�Q�B�
=B�B�ffB���B��B�=qB���B�\)B�{B��HB�\)B�  B���B�\)B��B��\B�G�B�  B�z�B�G�B�  B�z�B�
=B�B�z�B���B��B�Q�B��HB�p�B�  B���B�\)B��B��\B�G�B�  B��\B�
=B��B�z�B��B���B�=qB��HBÙ�B�=qB���B�G�B��Bƣ�B�\)B��B�z�B�
=BɮB�ffB��B�B�=qB���BͅB�=qB���B�\)B��BУ�B�p�B�{Bҏ\B�33B��Bԏ\B�
=B�B�z�B���BׅB�=qB���Bٙ�B�{Bڣ�B�\)B�{Bܣ�B��B��
B�z�B��Bߙ�B�(�B��HBᙚB�=qB�RB�G�B�  B�RB�G�B�B��B�G�B�B�Q�B���B�B�=qB�RB�G�B�{B��B��B��B�z�B��B�B�Q�B��HB�\)B�{B�RB��B�B�\B��B���B�(�B���B�\)B��B��RB�G�B��
B�z�B�33B��B�=qB��B���B�{B��HB��B��C ffC �C �C=qC��C��C=qC�C�HCG�C��C��C33C�\C�HC(�Cz�C�HC(�CffCC(�CffC�C
=CffC�RC��C	G�C	��C	��C
33C
p�C
�
C(�Cp�C�C  CffCC
=CQ�C�C
=CffCC
=CQ�C��C  CQ�C��C�
C=qC��C�HC33C�C�C=qC�\C�
C=qC��C��CG�C�\C��C\)C�RC  CQ�C�C�Cz�C��C{Cp�C�HC=qC�\C�
C33C��C
=C\)C�C  C\)CC(�Cz�C��C�C�\C��C Q�C ��C �C!G�C!�C"
=C"p�C"��C#{C#p�C#��C$(�C$��C$��C%Q�C%��C&  C&p�C&��C'{C'ffC'C(33C(�C(��C)(�C)�\C)�C*Q�C*��C*��C+G�C+��C,�C,p�C,C-{C-�C-�HC.G�C.��C.�HC/G�C/�C0{C0ffC0�RC1  C1ffC1C233C2�\C2�HC3=qC3�\C3��C4\)C4C5(�C5��C5�C6=qC6��C6��C7Q�C7�RC8(�C8�\C8�HC933C9�C9��C:\)C:C;{C;ffC;C<33C<��C<��C=Q�C=��C>  C>Q�C>�RC?(�C?�\C?�C@33C@�C@�HCA=qCA��CB  CBG�CB�\CB��CC(�CC�CC�
CD
=CDG�CD��CD�CE�CEQ�CE�CE��CF
=CFG�CFffCF�\CF�
CG
=CG33CGQ�CG�CGCH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                    ?��@   @B�\@��\@�  @�  @޸RA   A  A�RA,(�A@��A`��A���A���A���A�  A�  A�  A߮A�  B z�B(�B  B�
B�B(  B0(�B7�B?�
BH  BP  BX  B_�
Bg�
Bp  BxQ�B�{B�{B�  B��B�  B�  B�  B�{B�  B�  B�  B�{B�(�B�  B�{B�  B��B�(�B�  B�  B��B��
B��B�  B�  B�  B�{B�  B��B�  B�{B�{C 
=C  C��C��C
=C
  C�C��C��C��C  C��C  C  C
=C�C {C"
=C$  C&
=C(
=C*{C,
=C.
=C0  C1��C3��C6{C8
=C9��C<  C>  C@
=CB{CD{CF  CG��CJ  CL
=CN
=CP
=CR
=CS��CU��CX  CZ  C\
=C^
=C`  Ca��Cc��Cf  Ch  Cj
=Cl  Cm��Cp  Cr{Ct  Cv  Cx
=Cy��C|  C~
=C��C���C�C���C�  C�  C�C���C�C�  C�  C���C���C���C���C���C�  C�  C�C�C�C���C���C�  C�  C�C���C�C�  C�C�C�  C�  C�  C�  C�  C�  C�C���C���C�C�  C�C�  C���C���C�  C���C���C�C�  C�  C�  C���C�  C�  C�C�C�  C�  C�C�  C�  C�  C�C�C�
=C�C���C���C�C�  C���C�  C�  C�  C�C�C�  C�  C�
=C�C�  C���C���C�  C�C�C�  C�  C�C�  C�
=C���C�C�C�  C�  C�C�C�C�  C�
=C�C�  C�
=C���C�C�  C�  C�C�
=C�  C���C�C���C�  C�C�  C�  C�C�C�C�  C�  C�  C���C�  D �D �DD��DD�D  Dz�D�qD� D  D}qD  D��D  D� D  D}qD��D	}qD
  D
}qD�D�D�D��D�qD}qD�qD� D  D��D�D��D�D� D�qDz�D�qD}qD  D��D  D��D�qDz�D  D��D�qD� DD��D  D� D��D� D�D� D�qDz�D�qD}qD�qD� D �D ��D ��D!z�D!��D"� D#  D#� D$  D$��D%D%��D&  D&� D'�D'� D'�qD(� D)D)��D*  D*� D+D+��D,  D,��D-  D-��D.D.� D.�qD/z�D/�qD0� D1�D1}qD1�qD2� D3  D3z�D4  D4}qD5  D5� D6  D6��D7�D7��D8�D8� D8�qD9}qD9�qD:� D;  D;��D<  D<� D=D=��D=�qD>}qD?�D?��D@  D@�DADA��DB  DB�DC�DC� DD�DD��DE  DE��DF�DF� DF�qDG}qDH  DH��DI  DI� DI�qDJ}qDK  DK�DLDL��DM�DM}qDN  DN}qDN��DO� DP  DP� DQ  DQ� DR�DR��DS�DS}qDS�qDT� DU  DU��DV  DV��DW  DW� DX  DX}qDX�qDY}qDZ�DZ��D[�D[��D[��D\� D]�D]}qD^  D^��D_�D_� D_�qD`}qDa�Da��Db  Db}qDb�qDc��Dd  Dd��De�De��DfDf}qDg  Dg� Dh�Dh��Dh�qDi��Dj�Dj��Dk  Dk}qDk�qDl}qDm  Dm� Dn  Dn� Do�Do� Dp  Dp� Dq  Dq� Dr  Dr�DsDs}qDt  Dt� Dt��Duz�Du�qDv� Dv�qDw}qDw�qDx� Dy  Dyz�Dz  Dz�D{D{�D|�D|�D}  D}z�D}�qD~� D  D}qD�HD�>�D�}qD���D���D�>�D�~�D��HD�HD�>�D�~�D��HD��D�@ D�}qD�� D�HD�@ D�~�D���D�HD�@ D��HD�� D���D�>�D�~�D�� D�  D�@ D�� D��HD��D�AHD��HD��HD�HD�B�D�~�D�� D�HD�>�D�� D�� D���D�@ D���D��HD�  D�>�D�� D�� D�HD�>�D�~�D���D�  D�AHD��HD��HD�HD�AHD��HD��HD���D�>�D�}qD���D�HD�>�D�� D��HD�HD�@ D�}qD��qD���D�>�D�}qD���D�HD�>�D�~�D��HD�HD�B�D�� D���D���D�>�D��HD��HD�  D�@ D�~�D�� D�HD�B�D��HD�� D�  D�@ D��HD�� D�  D�AHD�� D��HD�  D�=qD�~�D�� D��D�AHD�� D���D���D�@ D�� D�� D�  D�@ D�� D�� D���D�>�D�� D�D�  D�=qD�~�D���D���D�>�D�� D�D�HD�>�D�� D�� D�  D�@ D�}qD��qD���D�AHD��HD�� D�  D�@ D��HD���D��qD�@ D�� D���D�  D�@ D�~�D�� D�HD�@ D��HD�� D�  D�>�D��HD��HD�  D�AHD��HD�� D�  D�@ D�� D��HD�HD�AHD��HD���D���D�>�D�~�D�� D�  D�@ D�� D���D���D�@ D�� D�� D���D�=qD�~�D�� D�  D�AHD�~�D���D�  D�AHD���D��HD�  D�AHD���D��HD�  D�@ D�� D�� D��D�B�D�� D�� D�  D�@ D���D��HD���D�>�D��HD��HD���D�@ D�� D���D���D�AHD��HD�� D��D�AHD�� D���D���D�@ D�~�D���D�  D�@ D�~�D���D�  D�AHD�~�D���D�  D�AHD��HD�� D�  D�B�D��HD���D���D�>�D�~�D���D�  D�@ D�� D�� D���D�@ DHD��HD�HD�>�D�~�Dþ�D�  D�AHD�~�D�� D�HD�B�Dŀ Dž�D���D�@ Dƀ DƽqD��qD�>�D�~�D�� D�  D�@ DȀ D��HD��D�AHDɀ Dɾ�D�  D�@ Dʀ Dʾ�D���D�>�D�}qD�� D���D�=qD�~�D��HD�HD�>�D̀ D�� D�HD�AHD�~�Dξ�D���D�@ DρHD��HD�  D�@ DЀ D�� D�  D�@ Dр DѾ�D��qD�>�D�~�D�� D�HD�B�DӁHD�� D�HD�B�DԂ�D�� D��qD�@ DՁHD��HD�  D�>�Dր D־�D�  D�>�D�~�D׾�D�HD�AHD؀ D�� D�  D�@ D�~�D�� D�  D�=qD�~�Dھ�D���D�=qDۀ D��HD���D�AHD�~�D�� D�HD�>�D�}qD�� D�HD�AHD�~�D�� D�  D�>�D߀ D߾�D��qD�@ D��HD��HD��D�AHD�HD�� D���D�>�D� D��HD�  D�@ D� D��HD�HD�>�D� D��HD�HD�B�D�HD�� D�  D�AHD�~�D澸D�HD�AHD�~�D羸D�  D�@ D�~�D�qD�  D�AHD� D龸D�  D�AHD�HD꾸D�  D�>�D�~�D뾸D���D�@ D�HD�� D�HD�AHD�~�D�� D���D�@ D�HD�� D�  D�AHD�HD�� D���D�@ D���D��HD���D�>�D� D�� D�  D�@ D�~�D�D�HD�@ D�~�D�D��qD�>�D�}qD��qD��qD�@ D��HD���D�  D�AHD�� D���D���D�>�D�� D�� D�  D�@ D�� D�� D���D�>�D��HD��HD�  D�>�D�� D���G�O�>�?B�\?��?��
?Ǯ?��@�@��@5@B�\@Q�@n{@�G�@��@��@��R@���@���@��H@�=q@��@��H@�@�z�@�p�A33A
=qA\)A�
A��A{A!�A(Q�A-p�A1G�A7
=A<(�A@  AE�AK�AO\)AS�
AZ=qA]p�Aa�AhQ�Al��Ap��AvffA{�A\)A�=qA��A�
=A�G�A�z�A�ffA�  A��A�p�A�\)A�=qA���A��RA���A��
A�A��A��\A���A�ffA���A��A�p�A�Q�A��\A�(�A��RA�G�A��HA�p�A�  A��A˅A�ffA�Q�A�=qA��
A�ffAأ�A�=qA�(�A�
=A�G�A��HA���A�\)A�G�A�\A�p�A�\)A�G�A�(�A�{A��A��HA���A��RB ��B{B
=B  Bp�B�\B�B��B
{B
�HB(�B��B�\B�B��B�B�HB(�Bp�B=qB\)B��B�B�RBQ�B�B{B\)B ��B!G�B"�RB$  B$��B&{B'\)B((�B)p�B*�RB+�B,��B.{B.�HB/�
B1�B2{B2�HB4(�B5p�B5�B7
=B8z�B9G�B:{B;\)B<z�B=�B>�RB?�B@z�BA��BB�HBC�BD��BE�BG
=BG�BH��BI�BK
=BK�BL��BN=qBN�HBP  BQG�BR{BS
=BTz�BU�BV{BW33BXQ�BX��BZffB[�B\(�B]p�B^�\B_33B`z�Ba��Bb=qBc�Bd��Be��BfffBg�Bh��Bip�BjffBk�
Bl��Bmp�Bn�RBp  Bp��Bq�Bs33Bt  Bt��Bv=qBw\)Bx  ByG�Bz�\B{\)B|Q�B}��B~�HB�B�=qB���B�\)B�B�z�B�
=B�p�B��B��\B�33B��B�(�B���B��B�{B�z�B�33B��
B�=qB��RB�p�B�  B�z�B���B��B�Q�B��RB�33B��B��\B���B�p�B�(�B���B�\)B�B�Q�B�
=B���B�{B���B�G�B�  B��\B�
=B���B�ffB��HB�\)B�{B���B�\)B��B�ffB�33B��
B�Q�B���B�p�B�(�B���B�G�B��
B���B�G�B��
B�Q�B�
=B�B�ffB���B��B�=qB���B�\)B�{B��HB�\)B�  B���B�\)B��B��\B�G�B�  B�z�B�G�B�  B�z�B�
=B�B�z�B���B��B�Q�B��HB�p�B�  B���B�\)B��B��\B�G�B�  B��\B�
=B��B�z�B��B���B�=qB��HBÙ�B�=qB���B�G�B��Bƣ�B�\)B��B�z�B�
=BɮB�ffB��B�B�=qB���BͅB�=qB���B�\)B��BУ�B�p�B�{Bҏ\B�33B��Bԏ\B�
=B�B�z�B���BׅB�=qB���Bٙ�B�{Bڣ�B�\)B�{Bܣ�B��B��
B�z�B��Bߙ�B�(�B��HBᙚB�=qB�RB�G�B�  B�RB�G�B�B��B�G�B�B�Q�B���B�B�=qB�RB�G�B�{B��B��B��B�z�B��B�B�Q�B��HB�\)B�{B�RB��B�B�\B��B���B�(�B���B�\)B��B��RB�G�B��
B�z�B�33B��B�=qB��B���B�{B��HB��B��C ffC �C �C=qC��C��C=qC�C�HCG�C��C��C33C�\C�HC(�Cz�C�HC(�CffCC(�CffC�C
=CffC�RC��C	G�C	��C	��C
33C
p�C
�
C(�Cp�C�C  CffCC
=CQ�C�C
=CffCC
=CQ�C��C  CQ�C��C�
C=qC��C�HC33C�C�C=qC�\C�
C=qC��C��CG�C�\C��C\)C�RC  CQ�C�C�Cz�C��C{Cp�C�HC=qC�\C�
C33C��C
=C\)C�C  C\)CC(�Cz�C��C�C�\C��C Q�C ��C �C!G�C!�C"
=C"p�C"��C#{C#p�C#��C$(�C$��C$��C%Q�C%��C&  C&p�C&��C'{C'ffC'C(33C(�C(��C)(�C)�\C)�C*Q�C*��C*��C+G�C+��C,�C,p�C,C-{C-�C-�HC.G�C.��C.�HC/G�C/�C0{C0ffC0�RC1  C1ffC1C233C2�\C2�HC3=qC3�\C3��C4\)C4C5(�C5��C5�C6=qC6��C6��C7Q�C7�RC8(�C8�\C8�HC933C9�C9��C:\)C:C;{C;ffC;C<33C<��C<��C=Q�C=��C>  C>Q�C>�RC?(�C?�\C?�C@33C@�C@�HCA=qCA��CB  CBG�CB�\CB��CC(�CC�CC�
CD
=CDG�CD��CD�CE�CEQ�CE�CE��CF
=CFG�CFffCF�\CF�
CG
=CG33CGQ�CG�CGCH  G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                    @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@��@��@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A޸RA�ƨA���A���A�ȴA���A���A���A���A���A���A���A���A���A���A��
A��A��A���A���A��
A��#A��;A��;A��;A��HA��HA��TA��HA��HA���A�I�A۶FAٍPA؉7Aְ!AӶFAѡ�A�dZA���A�&�Ȧ+A�=qAʛ�A�jAȺ^A�M�Aư!A�hsA���A�O�A��A���A��yA�;dA���A�~�A�(�A�ȴA�oA�-A��!A�|�A�/A��A���A��!A���A�hsA�A��A��A��TA�+A���A�O�A��A�+A��A�dZA�"�A�l�A��A���A�?}A��;A���A�A��A���A�  A�1'A��A�`BA�O�A���A�K�A�v�A���A��`A��9A�?}A�x�A��A�t�A��A}��AzȴAx9XAv �Au"�As+Aq�Ap��AnM�Ai�-Af9XAc"�Ab1A`�A_XA\1AZM�AYG�AW�;AT�RASK�AQ��AM��AL�uAL �AK�AKp�AJȴAJJAH�!AD�+A@VA?XA=�-A;O�A7x�A4n�A2~�A0=qA.$�A,��A+��A+oA(ZA'�wA'��A&�A%K�A#��A!��A �/A�A~�A�An�AA|�AoA�A�uA^5A=qA��A��A�;A`BAA��A�AƨA�PA\)A�`A�!A(�Al�An�A�mA�^A�-A��AS�A�/A��A��A��A�!A��A=qAx�AXA
�`A
(�A
M�A	��A	&�A�A(�A��A�9AA��Av�An�A=qA�A+AK�A�9Az�AbA �/A ��A {@�l�@���@�  @��D@���@���@���@�"�@���@���@�Z@�b@���@�b@�t�@�o@��H@�-@��@�b@�l�@���@��@�x�@�v�@� �@�{@�@�ȴ@��#@���@��@�@��-@�`B@�z�@�ƨ@�^5@�hs@���@�V@�-@���@�7L@��@��@��@�n�@���@��@��;@��y@�5?@�ff@ڇ+@�5?@���@ٲ-@�G�@�V@�p�@�"�@���@�z�@���@���@�"�@·+@�?}@��/@���@��@��H@���@��`@�x�@�@ѩ�@��
@��y@��@�-@���@���@�5?@�v�@��y@˅@˾w@�  @�b@�V@���@ƸR@�$�@��@�?}@�Ĝ@���@�dZ@��@�o@���@���@\@�-@�ff@���@�
=@�M�@���@��@��9@�  @�l�@�S�@���@��@���@���@�%@�b@�o@�5?@���@��h@�v�@�n�@���@��@���@��T@��7@��@�p�@�O�@�&�@���@��u@�1'@��F@�|�@�o@�ȴ@�`B@�(�@���@��P@�
=@���@�{@���@���@���@���@���@�X@��@��`@��@�9X@��@��@���@�S�@��R@�^5@�=q@�-@���@�O�@�Ĝ@��;@�"�@�~�@�ff@�=q@���@��@�X@�&�@���@�Ĝ@���@�r�@�r�@�Z@�(�@��;@�;d@��R@���@��@���@�x�@�/@��`@��D@� �@��@�"�@�
=@�
=@���@��@��H@�~�@�E�@�5?@���@�G�@���@��/@�Ĝ@��j@�z�@��m@��F@��P@�S�@�"�@�ȴ@�M�@�J@�X@�?}@�/@���@�Ĝ@�r�@�  @���@�S�@�+@�"�@�"�@��@��y@�E�@�J@��7@�X@�&�@��@���@��9@���@�bN@�b@�l�@�K�@�;d@�33@�"�@��@���@�ff@��@�@���@�x�@�%@���@�Ĝ@�Ĝ@��u@�r�@��@��w@��P@��@�V@��@�`B@�%@��@�(�@�1@��@��@��
@��@�dZ@�;d@�
=@�ȴ@��\@�E�@�5?@��@��#@�@���@�`B@���@�bN@���@���@���@���@��@�\)@�@�v�@�ff@�^5@�M�@�5?@��@�@��@��T@���@��-@��h@�p�@�X@�&�@��@���@��`@��j@�r�@���@�
=@��@�v�@�$�@��#@���@��@��@��@���@���@�r�@�A�@��;@���@���@�dZ@��y@�-@���@���@�x�@�`B@�O�@�/@�%@���@��@�  @|�@~��@}�@}�h@|�/@|z�@|(�@{�F@{o@z�H@z�\@y��@yhs@y&�@x�`@x�@x1'@w��@w\)@w;d@w�@v��@vȴ@vff@v5?@v{@v@v@u��@u`B@t�D@t(�@s�
@s"�@r��@r�@qG�@p�`@p�@ol�@o;d@n��@nv�@n@m�T@m�-@m`B@m�@l�/@lj@l1@k�@kdZ@ko@j~�@i��@i�@h��@h�u@hA�@g��@gK�@g�@f�@fȴ@f��@f��@f�+@e�@e�T@e��@e@e��@e�h@d�@dZ@c�
@c�@c33@bn�@bJ@a��@ahs@`��@`r�@`A�@_�@_l�@_
=@^ȴ@^�R@^v�@^V@^$�@]�@]��@]�-@]`B@]�@\�/@\�D@[��@[��@Z��@Z^5@ZJ@Y�#@Y�7@Yx�@YG�@Y�@Y%@X��@X��@X�u@XbN@X �@W��@W�@W|�@W\)@V�@V�R@Vv�@Vff@U�@U?}@U�@T��@T�@T�@T��@T9X@S�m@R��@R~�@Q��@Qx�@Q�@PĜ@PbN@O�@O;d@N�+@Nv�@Nv�@NV@N{@N@M�@M�T@M��@M�-@M�@MV@L�j@L��@LI�@L�@K��@K�F@Kt�@K33@J�@J~�@Jn�@J^5@JM�@J=q@J=q@J-@J-@JJ@I��@I��@Ix�@Ihs@I7L@H��@H�9@H�@G�;@G\)@F�@F�+@Fv�@FE�@E�@E�-@E`B@D��@D�j@DI�@D9X@D�@C�m@Ct�@CC�@C33@B��@B�\@Bn�@B�@A�#@A�7@A&�@@��@@��@@Q�@@1'@@  @?�;@?�w@?�P@?l�@?\)@>�y@>��@>5?@=�@<�j@;�
@;�F@;C�@:��@:��@:n�@:=q@:J@9��@9��@9x�@97L@9�@8��@8r�@81'@7�;@7�@7�P@7|�@7\)@7+@6�y@6ȴ@6��@6�+@6�+@6�+@6V@5�h@3��@3�F@3�@3dZ@3C�@3C�@3"�@3o@3@2�@2�H@2n�@1hs@17L@0�`@0�@0A�@/�P@.�@.ȴ@.��@.�+@.ff@.V@.5?@-�T@-@-p�@,��@,Z@,�@+�m@+��@+S�@+@*��@*�\@*-@)�@)��@)X@(Ĝ@(��@(��@(�u@(�@(Q�@(1'@( �@( �@(b@(  @'�;@'|�@&��@&v�@&V@&{@%��@%�h@%/@$�@$�@$z�@$z�@$I�@#�
@#dZ@#@"�\@"=q@!��@!�#@!��@!hs@!�@ ��@ �u@ �@ r�@ bN@ bN@ Q�@�@�w@�@l�@K�@+@�@�@�R@��@�+@E�@5?@{@@��@�@�D@z�@�@�
@��@t�@dZ@dZ@S�@"�@�@�H@�!@�@�#@��@��@��@�7@hs@G�@�@�@��@�u@�@ �@|�@
=@��@V@$�@{@{@��@/@V@��@�@�/@��@�@�@�D@j@�@�
@��@�@dZ@dZ@S�@C�@"�@�@��@��@�!@~�@n�@^5@�@�^@x�@G�@&�@%@��@Ĝ@1'@b@�@�;@�P@+@�@v�@E�@5?@{@�@�-@�h@p�@`B@O�A�ƨAާ�A޾wA޼jA޸RA޾wA���A���A���A���A�ĜA���A���A�ȴA�ȴA�ȴA�ȴA�ƨA�ȴA���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A��#A���A��
A��/A��
A��
A��#A��A��
A��#A��#A��
A��#A��#A���A��
A��#A���A��A��A���A���A���A���A���A���A���A��A���A���A��
A��
A���A���A��
A��
A���A��
A��
A���A��#A��#A��A��;A��#A��A��/A��;A��#A��#A��;A��/A��#A��;A��HA��#A��;A��TA��/A��/A��HA��;A��/A��;A��TA��/A��/A��HA��HA��/A��TA��HA��/A��HA��TA��;A��TA��TA��;A��HA��`A��;A��HA��TA��HA��;A��`A��TA��HA��`A��TA��;A��TA��`A��;A��HA��TA��HA��/A��TA��TA��HA��`A��HA��/A��;A��;A��A���A��#A���A�ƨA�ȴA�ȴA�ƨA޸RAރA�ZA�VA�;dAܛ�A�/A�bA��A�%A�A���A���A۬Aۉ7A��A��A��HA���A٬AًDA�hsA�$�A��
A��A���A���AضFA؅A�O�A� �A��A׸RAכ�A��A�r�A�bNA�/A�{A�(�A���Aԣ�A�;dAӃA��A��AҰ!A�v�A�&�A�AѓuA�ffA�\)A�G�A�&�A�&�A���AЍPA�M�A�/A���A�ƨAϋDA�A�A�A��#A�ĜAθRAάAΣ�AΡ�AΟ�A�ffA�1A���A� �A̺^A̙�ȂhA̋DA�|�A�l�A�ffA�dZA�bNA�ZA�M�A�?}A�/A���A�ĜAˋDA�"�A�A�A��A�A��A��HA���Aɥ�A�XA�&�A�VA�A���A���A���A��mAȴ9A�VA�v�A�dZA�ZA�O�A�I�A�=qA�JA��A���A�AƼjAư!A�XAţ�A�/A�VA��A�JA���A���A���A�A���A��!A���A�\)A��A��A�&�A�JA���A��mA���A�ȴA��wA��-A���A��+A�M�A�JA���A���A�p�A�S�A�?}A�$�A�A���A��HA��^A���A�r�A�dZA�ZA�7LA��A�oA�  A��`A��
A��RA��DA�bNA�{A��A��\A�~�A�n�A�bNA�\)A�O�A�?}A�33A� �A��A�1A��A��`A���A�ƨA���A��9A���A�t�A�9XA���A��A��mA��HA���A��A�S�A�bA��HA�ƨA�A��jA��FA��A���A��A���A��7A�|�A��A��A��DA�v�A�bNA�M�A�5?A�/A��A��A�"�A�&�A��A��A� �A��A�bA�1A�
=A���A��TA�ƨA���A��DA�K�A�=qA��A�ĜA�r�A�33A�VA�bA�oA��A��^A��FA���A�XA���A�x�A�?}A�;dA�oA��A�\)A�M�A�/A��A��A���A�z�A�^5A�A�A�+A�1A��yA��jA�5?A���A��TA��;A���A���A�|�A�&�A��`A��A��DA�|�A�jA�K�A�9XA�-A�&�A�JA��#A�;dA��A��A��TA���A�dZA���A��jA�dZA��FA�+A�G�A�hsA�A�A�  A���A��RA��9A��A�1'A�+A�(�A�"�A��A�A���A�|�A�1'A���A�5?A���A�ĜA��RA�n�A�A�A��A���A�C�A��^A�A�jA�(�A�1A��A�bNA�K�A�A�A�$�A��A��A��A��A��A�%A���A��A���A��^A��-A���A��+A�n�A�S�A�33A��/A��7A�t�A�VA��A�S�A�jA�  A��A���A�p�A�;dA��#A�~�A���A�E�A�?}A�E�A�9XA��A�1A�1A�%A���A���A��HA�A���A�|�A�K�A�"�A�%A���A��/A��A��A�ffA� �A�1A��;A���A��9A���A��A�O�A� �A���A���A�ZA��A��A��A��FA�`BA�-A���A��jA�K�A�%A��!A�\)A��wA���A��A�bNA�=qA�VA��A���A���A�x�A�VA���A��wA���A���A��PA��hA��hA��A���A��wA���A���A�?}A�7LA���A�1A�=qA��/A���A�Q�A�VA��TA���A���A�1'A�7LA��`A��/A��
A��A�\)A�K�A��A��yA�ĜA��A���A��7A��A�x�A�p�A�p�A�jA�`BA�^5A�M�A�9XA�"�A�{A��A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                    A޸RA�ƨA���A���A�ȴA���A���A���A���A���A���A���A���A���A���A��
A��A��A���A���A��
A��#A��;A��;A��;A��HA��HA��TA��HA��HA���A�I�A۶FAٍPA؉7Aְ!AӶFAѡ�A�dZA���A�&�Ȧ+A�=qAʛ�A�jAȺ^A�M�Aư!A�hsA���A�O�A��A���A��yA�;dA���A�~�A�(�A�ȴA�oA�-A��!A�|�A�/A��A���A��!A���A�hsA�A��A��A��TA�+A���A�O�A��A�+A��A�dZA�"�A�l�A��A���A�?}A��;A���A�A��A���A�  A�1'A��A�`BA�O�A���A�K�A�v�A���A��`A��9A�?}A�x�A��A�t�A��A}��AzȴAx9XAv �Au"�As+Aq�Ap��AnM�Ai�-Af9XAc"�Ab1A`�A_XA\1AZM�AYG�AW�;AT�RASK�AQ��AM��AL�uAL �AK�AKp�AJȴAJJAH�!AD�+A@VA?XA=�-A;O�A7x�A4n�A2~�A0=qA.$�A,��A+��A+oA(ZA'�wA'��A&�A%K�A#��A!��A �/A�A~�A�An�AA|�AoA�A�uA^5A=qA��A��A�;A`BAA��A�AƨA�PA\)A�`A�!A(�Al�An�A�mA�^A�-A��AS�A�/A��A��A��A�!A��A=qAx�AXA
�`A
(�A
M�A	��A	&�A�A(�A��A�9AA��Av�An�A=qA�A+AK�A�9Az�AbA �/A ��A {@�l�@���@�  @��D@���@���@���@�"�@���@���@�Z@�b@���@�b@�t�@�o@��H@�-@��@�b@�l�@���@��@�x�@�v�@� �@�{@�@�ȴ@��#@���@��@�@��-@�`B@�z�@�ƨ@�^5@�hs@���@�V@�-@���@�7L@��@��@��@�n�@���@��@��;@��y@�5?@�ff@ڇ+@�5?@���@ٲ-@�G�@�V@�p�@�"�@���@�z�@���@���@�"�@·+@�?}@��/@���@��@��H@���@��`@�x�@�@ѩ�@��
@��y@��@�-@���@���@�5?@�v�@��y@˅@˾w@�  @�b@�V@���@ƸR@�$�@��@�?}@�Ĝ@���@�dZ@��@�o@���@���@\@�-@�ff@���@�
=@�M�@���@��@��9@�  @�l�@�S�@���@��@���@���@�%@�b@�o@�5?@���@��h@�v�@�n�@���@��@���@��T@��7@��@�p�@�O�@�&�@���@��u@�1'@��F@�|�@�o@�ȴ@�`B@�(�@���@��P@�
=@���@�{@���@���@���@���@���@�X@��@��`@��@�9X@��@��@���@�S�@��R@�^5@�=q@�-@���@�O�@�Ĝ@��;@�"�@�~�@�ff@�=q@���@��@�X@�&�@���@�Ĝ@���@�r�@�r�@�Z@�(�@��;@�;d@��R@���@��@���@�x�@�/@��`@��D@� �@��@�"�@�
=@�
=@���@��@��H@�~�@�E�@�5?@���@�G�@���@��/@�Ĝ@��j@�z�@��m@��F@��P@�S�@�"�@�ȴ@�M�@�J@�X@�?}@�/@���@�Ĝ@�r�@�  @���@�S�@�+@�"�@�"�@��@��y@�E�@�J@��7@�X@�&�@��@���@��9@���@�bN@�b@�l�@�K�@�;d@�33@�"�@��@���@�ff@��@�@���@�x�@�%@���@�Ĝ@�Ĝ@��u@�r�@��@��w@��P@��@�V@��@�`B@�%@��@�(�@�1@��@��@��
@��@�dZ@�;d@�
=@�ȴ@��\@�E�@�5?@��@��#@�@���@�`B@���@�bN@���@���@���@���@��@�\)@�@�v�@�ff@�^5@�M�@�5?@��@�@��@��T@���@��-@��h@�p�@�X@�&�@��@���@��`@��j@�r�@���@�
=@��@�v�@�$�@��#@���@��@��@��@���@���@�r�@�A�@��;@���@���@�dZ@��y@�-@���@���@�x�@�`B@�O�@�/@�%@���@��@�  @|�@~��@}�@}�h@|�/@|z�@|(�@{�F@{o@z�H@z�\@y��@yhs@y&�@x�`@x�@x1'@w��@w\)@w;d@w�@v��@vȴ@vff@v5?@v{@v@v@u��@u`B@t�D@t(�@s�
@s"�@r��@r�@qG�@p�`@p�@ol�@o;d@n��@nv�@n@m�T@m�-@m`B@m�@l�/@lj@l1@k�@kdZ@ko@j~�@i��@i�@h��@h�u@hA�@g��@gK�@g�@f�@fȴ@f��@f��@f�+@e�@e�T@e��@e@e��@e�h@d�@dZ@c�
@c�@c33@bn�@bJ@a��@ahs@`��@`r�@`A�@_�@_l�@_
=@^ȴ@^�R@^v�@^V@^$�@]�@]��@]�-@]`B@]�@\�/@\�D@[��@[��@Z��@Z^5@ZJ@Y�#@Y�7@Yx�@YG�@Y�@Y%@X��@X��@X�u@XbN@X �@W��@W�@W|�@W\)@V�@V�R@Vv�@Vff@U�@U?}@U�@T��@T�@T�@T��@T9X@S�m@R��@R~�@Q��@Qx�@Q�@PĜ@PbN@O�@O;d@N�+@Nv�@Nv�@NV@N{@N@M�@M�T@M��@M�-@M�@MV@L�j@L��@LI�@L�@K��@K�F@Kt�@K33@J�@J~�@Jn�@J^5@JM�@J=q@J=q@J-@J-@JJ@I��@I��@Ix�@Ihs@I7L@H��@H�9@H�@G�;@G\)@F�@F�+@Fv�@FE�@E�@E�-@E`B@D��@D�j@DI�@D9X@D�@C�m@Ct�@CC�@C33@B��@B�\@Bn�@B�@A�#@A�7@A&�@@��@@��@@Q�@@1'@@  @?�;@?�w@?�P@?l�@?\)@>�y@>��@>5?@=�@<�j@;�
@;�F@;C�@:��@:��@:n�@:=q@:J@9��@9��@9x�@97L@9�@8��@8r�@81'@7�;@7�@7�P@7|�@7\)@7+@6�y@6ȴ@6��@6�+@6�+@6�+@6V@5�h@3��@3�F@3�@3dZ@3C�@3C�@3"�@3o@3@2�@2�H@2n�@1hs@17L@0�`@0�@0A�@/�P@.�@.ȴ@.��@.�+@.ff@.V@.5?@-�T@-@-p�@,��@,Z@,�@+�m@+��@+S�@+@*��@*�\@*-@)�@)��@)X@(Ĝ@(��@(��@(�u@(�@(Q�@(1'@( �@( �@(b@(  @'�;@'|�@&��@&v�@&V@&{@%��@%�h@%/@$�@$�@$z�@$z�@$I�@#�
@#dZ@#@"�\@"=q@!��@!�#@!��@!hs@!�@ ��@ �u@ �@ r�@ bN@ bN@ Q�@�@�w@�@l�@K�@+@�@�@�R@��@�+@E�@5?@{@@��@�@�D@z�@�@�
@��@t�@dZ@dZ@S�@"�@�@�H@�!@�@�#@��@��@��@�7@hs@G�@�@�@��@�u@�@ �@|�@
=@��@V@$�@{@{@��@/@V@��@�@�/@��@�@�@�D@j@�@�
@��@�@dZ@dZ@S�@C�@"�@�@��@��@�!@~�@n�@^5@�@�^@x�@G�@&�@%@��@Ĝ@1'@b@�@�;@�P@+@�@v�@E�@5?@{@�@�-@�h@p�@`BG�O�A�ƨAާ�A޾wA޼jA޸RA޾wA���A���A���A���A�ĜA���A���A�ȴA�ȴA�ȴA�ȴA�ƨA�ȴA���A���A�ȴA���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A��A���A���A���A���A���A���A���A���A���A���A���A���A��#A���A��
A��/A��
A��
A��#A��A��
A��#A��#A��
A��#A��#A���A��
A��#A���A��A��A���A���A���A���A���A���A���A��A���A���A��
A��
A���A���A��
A��
A���A��
A��
A���A��#A��#A��A��;A��#A��A��/A��;A��#A��#A��;A��/A��#A��;A��HA��#A��;A��TA��/A��/A��HA��;A��/A��;A��TA��/A��/A��HA��HA��/A��TA��HA��/A��HA��TA��;A��TA��TA��;A��HA��`A��;A��HA��TA��HA��;A��`A��TA��HA��`A��TA��;A��TA��`A��;A��HA��TA��HA��/A��TA��TA��HA��`A��HA��/A��;A��;A��A���A��#A���A�ƨA�ȴA�ȴA�ƨA޸RAރA�ZA�VA�;dAܛ�A�/A�bA��A�%A�A���A���A۬Aۉ7A��A��A��HA���A٬AًDA�hsA�$�A��
A��A���A���AضFA؅A�O�A� �A��A׸RAכ�A��A�r�A�bNA�/A�{A�(�A���Aԣ�A�;dAӃA��A��AҰ!A�v�A�&�A�AѓuA�ffA�\)A�G�A�&�A�&�A���AЍPA�M�A�/A���A�ƨAϋDA�A�A�A��#A�ĜAθRAάAΣ�AΡ�AΟ�A�ffA�1A���A� �A̺^A̙�ȂhA̋DA�|�A�l�A�ffA�dZA�bNA�ZA�M�A�?}A�/A���A�ĜAˋDA�"�A�A�A��A�A��A��HA���Aɥ�A�XA�&�A�VA�A���A���A���A��mAȴ9A�VA�v�A�dZA�ZA�O�A�I�A�=qA�JA��A���A�AƼjAư!A�XAţ�A�/A�VA��A�JA���A���A���A�A���A��!A���A�\)A��A��A�&�A�JA���A��mA���A�ȴA��wA��-A���A��+A�M�A�JA���A���A�p�A�S�A�?}A�$�A�A���A��HA��^A���A�r�A�dZA�ZA�7LA��A�oA�  A��`A��
A��RA��DA�bNA�{A��A��\A�~�A�n�A�bNA�\)A�O�A�?}A�33A� �A��A�1A��A��`A���A�ƨA���A��9A���A�t�A�9XA���A��A��mA��HA���A��A�S�A�bA��HA�ƨA�A��jA��FA��A���A��A���A��7A�|�A��A��A��DA�v�A�bNA�M�A�5?A�/A��A��A�"�A�&�A��A��A� �A��A�bA�1A�
=A���A��TA�ƨA���A��DA�K�A�=qA��A�ĜA�r�A�33A�VA�bA�oA��A��^A��FA���A�XA���A�x�A�?}A�;dA�oA��A�\)A�M�A�/A��A��A���A�z�A�^5A�A�A�+A�1A��yA��jA�5?A���A��TA��;A���A���A�|�A�&�A��`A��A��DA�|�A�jA�K�A�9XA�-A�&�A�JA��#A�;dA��A��A��TA���A�dZA���A��jA�dZA��FA�+A�G�A�hsA�A�A�  A���A��RA��9A��A�1'A�+A�(�A�"�A��A�A���A�|�A�1'A���A�5?A���A�ĜA��RA�n�A�A�A��A���A�C�A��^A�A�jA�(�A�1A��A�bNA�K�A�A�A�$�A��A��A��A��A��A�%A���A��A���A��^A��-A���A��+A�n�A�S�A�33A��/A��7A�t�A�VA��A�S�A�jA�  A��A���A�p�A�;dA��#A�~�A���A�E�A�?}A�E�A�9XA��A�1A�1A�%A���A���A��HA�A���A�|�A�K�A�"�A�%A���A��/A��A��A�ffA� �A�1A��;A���A��9A���A��A�O�A� �A���A���A�ZA��A��A��A��FA�`BA�-A���A��jA�K�A�%A��!A�\)A��wA���A��A�bNA�=qA�VA��A���A���A�x�A�VA���A��wA���A���A��PA��hA��hA��A���A��wA���A���A�?}A�7LA���A�1A�=qA��/A���A�Q�A�VA��TA���A���A�1'A�7LA��`A��/A��
A��A�\)A�K�A��A��yA�ĜA��A���A��7A��A�x�A�p�A�p�A�jA�`BA�^5A�M�A�9XA�"�A�{A��A���G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                                                                                                                                                    ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�BA�B@�BAUB@�BAUBAUBA�BA�BA B@�B@�BAUBAUBAUBA�BA BAUBAUBA�BA�BAUBA BAUBAUBA BA BA B@�B@B?�BC�B��B�mB�B&�B)�B)�B7�BB[BK�Bf�B~�B�4B�B�:B��B��B��B��B�UB�YB��B|�BuZB{�B�iB��B��B��B��B��B�B�gBʌB�?BŢB��B�B��B�B��B�fB}VBncB`�BR�BK)BD�BC-B-�B%zB
	B �B�]B�PB��B�B��B��B��B��B��B�bB� Bl"BXEB8�B.B~B�B  B
ĜB
��B
��B
��B
��B
.B
kQB
^5B
S[B
J�B
C-B
9XB
33B
)�B
�B	��B	�B	�B	�]B	�gB	�B	�HB	�qB	�FB	��B	�IB	�1B	��B	cB	}"B	zxB	w2B	r�B	m]B	f�B	\�B	G�B	?�B	:�B	2aB	(�B	B	+B	B	�B	�B	�B	�B	�B	oB�cB�.B�B��B��B�B�MB�B�oB�oB�AB��B�AB�B�B�B�B��B�B��B��B�/B��B�cB��B�cB�/B��B�]B�B�oB�B�B��B��B��B��B	�B		7B	
rB	JB	�B		B	B	 �B	"hB	*�B	1�B	A�B	IRB	IB	LdB	K^B	GzB	EmB	S�B	Y�B	Y�B	[�B	]�B	_B	^�B	WsB	O�B	R B	W
B	Q�B	W�B	T,B	U�B	P}B	Q�B	_�B	e`B	f�B	k�B	ncB	o�B	zxB	��B	�B	��B	}�B	~]B	{B	~]B	�B	�B	�SB	��B	�+B	��B	��B	�oB	w2B	�lB	�eB	�B	��B	�CB	�B	�wB	�qB	�B	�qB	��B	��B	�[B	�B	��B	�=B	��B	�wB	��B	�B	�B	��B	�VB	��B	�VB	�B	��B	��B	�\B	��B	�B	�RB	��B	�6B	��B	�OB	��B	��B	�YB	�B	��B	� B	�B	��B	��B	��B	��B	��B	��B	�OB	�mB	ɺB	�tB	�9B	ŢB	�B	�BB	��B	�UB	ÖB	ǮB	�#B	�dB	��B	уB	�EB	�#B	��B	��B	�)B	��B	�#B	�)B	��B	�]B	�B	�B	��B	�B	�B	�2B	�B	�5B	�AB	�MB	�MB	��B	��B	��B	�2B	��B	�%B	�GB	�B	��B	�B	�|B	��B	�B	��B	�"B
;B
�B
	lB
	�B

	B
	7B
	lB
	lB
	7B
�B
~B
PB
PB
�B
VB
�B
"B
B
�B
�B
{B
FB
�B
B
B
{B
B
B
MB
B
�B
�B
�B
�B
�B
�B
�B
B
�B
�B
B
�B
xB
OB
�B
!-B
#nB
$�B
$�B
%�B
'RB
'�B
'�B
(�B
(�B
)�B
)�B
*0B
+�B
-B
-�B
-�B
.IB
.}B
.}B
/�B
/�B
1'B
0UB
0�B
1�B
3�B
5�B
5�B
5�B
5�B
5�B
5�B
6FB
7LB
6�B
7�B
8RB
9XB
9�B
9�B
:*B
:�B
<�B
=<B
=�B
=�B
>�B
>�B
?�B
@OB
A�B
C�B
C�B
C�B
C�B
D�B
E9B
F?B
FB
G�B
G�B
GEB
GEB
F�B
GEB
GEB
F�B
F�B
F�B
F�B
FtB
F�B
FtB
FtB
F�B
GB
H�B
G�B
G�B
G�B
G�B
G�B
H�B
IRB
I�B
I�B
I�B
J�B
K)B
K^B
K)B
K)B
K^B
K)B
K�B
K�B
K�B
NB
M6B
OB
OB
OBB
P}B
P}B
P}B
P}B
P}B
P�B
Q�B
P�B
QNB
QNB
QNB
Q�B
R B
Q�B
Q�B
R B
Q�B
Q�B
R B
R�B
S&B
S�B
S�B
S�B
S�B
S�B
S�B
U�B
U�B
U�B
U�B
U�B
V9B
VB
VB
VmB
V9B
V9B
VB
V�B
VB
VmB
VmB
VmB
V9B
VB
VB
VmB
YB
XB
W�B
W?B
XB
W�B
X�B
XyB
XyB
W�B
X�B
XEB
Y�B
YB
Z�B
ZB
Y�B
ZQB
[�B
\�B
]dB
]dB
]/B
]dB
]/B
]�B
^B
]�B
^�B
_�B
_�B
`vB
aHB
`�B
bB
a�B
bB
b�B
b�B
b�B
c B
c�B
d&B
d&B
d�B
d�B
d�B
e�B
e�B
e�B
e�B
e�B
e�B
f�B
f�B
f�B
f�B
f�B
g8B
hsB
h�B
iB
h�B
i�B
iyB
jKB
j�B
j�B
k�B
l�B
lWB
l�B
m)B
m)B
m)B
m]B
m�B
m�B
m�B
ncB
n�B
o B
n�B
o B
o5B
pB
qAB
r|B
r�B
sB
s�B
s�B
s�B
sMB
sMB
sB
r�B
r�B
s�B
sMB
sMB
sMB
s�B
s�B
s�B
tTB
s�B
s�B
s�B
r|B
q�B
q�B
rB
r�B
r�B
r|B
r�B
sB
sMB
s�B
sMB
s�B
s�B
s�B
tB
tB
s�B
t�B
t�B
t�B
t�B
uZB
uZB
v�B
v�B
wfB
wfB
w�B
w�B
xB
x8B
x8B
xlB
xlB
x�B
x�B
y	B
y>B
yrB
y�B
yrB
zxB
zDB
zDB
zDB
{�B
{JB
{B
{JB
{B
{JB
{�B
{�B
{�B
}"B
|�B
}"B
}�B
}�B
~(B
~]B
~�B
cB
.B
.B
.B
.B
cB
cB
cB
cB
cB
�B
cB
�B
�4B
�4B
��B
��B
��B
�B
�;B
�;B
��B
�AB
�AB
�AB
�AB
�uB
�uB
�uB
�uB
��B
�B
�B
�GB
�B
�{B
��B
��B
��B
�MB
��B
�B
�SB
�SB
��B
��B
��B
�%B
�YB
��B
��B
��B
��B
�+B
��B
��B
��B
�1B
�1B
�fB
��B
��B
��B
�lB
��B
��B
�=B
�=B
�=B
�rB
��B
��B
��B
��B
�DB
�DB
�xB
�xB
�B
��B
�PB
��B
�"B
�"B
�"B
�VB
��B
��B
��B
�(B
�(B
�(B
�\B
��B
��B
�.B
�.B
�.B
�bB
�bB
��B
� B
� B
�4B
� B
� B
��B
��B
��B
�B
�@B
��B
�uB
�B
��B
��B
��B
�B
��B
��B
�{B
��B
��B
�B
�MB
�MB
��B
��B
��B
�$B
��B
�$B
�YB
�YB
��B
��B
�+B
��B
��B
��B
�1B
�eB
��B
��B
�B
�7B
�kB
��B
��B
�qB
��B
��B
��B
��B
�B
�CB
�xB
��B
�xB
�xB
��B
�xB
�B
��B
�IB
��B
��B
��B
�B
�OB
��B
��B
��B
��B
��B
��B
��B
�'B
��B
��B
�-B
�-B
�bB
��B
��B
��B
�4B
�4B
�4B
�hB
�4B
�4B
�B
��B
��B
�:B
�:B
�nB
�:B
��B
��B
��B
��B
�@B
�B
�B
�B
�tB
�zB
��B
�zB
��B
�LB
��B
��B
�LB
�LB
�LB
�LB
�LB
�LB
��B
�RB
�RB
�B
�RB
��B
�RB
��B
��B
��B
��B
��B
��B
��B
�XB
��B
�*B
�_B
�_B
��B
��B
�_B
�0B
�0B
��B
��B
��B
�0B
�eB
�eB
��B
�6B
�kB
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
�B
�B
�=B
�B
�=B
��B
��B
��B
�B
�B
�B
�wB
�B
�IB
�IB
�IB
��B
�B
��B
�!B
�!B
�!B
�UB
�UB
��B
��B
��B
�'B
�'BDgBC�BD�B@OBCaB@�B@�BA�B@B@�BC-B@OB?BA�BB'BA�B?�BB'BA�B@BAUBB[B@�B@�BB'BA�B@�B@�BB[B@BA BA�BA B@�BB'BAUB@BA�BA�B@�BAUB@�BA�BA�BB[B?BAUBB'B?}B@�B@�BB[B@BA�BB�B?}B@BB�BAUBB�BCaB@B@BB[BA�B@OBA�BA�B@OBA�BB�B@�B@�BB�B@�B@BB'BA�B@�BB[BA BA BB[BB[B@�B@�BB'BB�B@�B@OBAUBB'B@�B@�BB�BB'B@�B@�BB�B@�BA BB[B?}BA BB'B@BA�BB�B@B@BA�BA�B@�BA�BB�B@�B@OBB[BAUB?�BA�BB'B@�BA BB[BA B@OBB'BB'B@OB@OBB�B@OBA BB�B@�B@�BA�B@�B@OBB[BA B@BA�BA B?�BA BA�B?�B@�BA�B@B@�BA�B@B?}BA B@�B?B?�BA B>�B?B@�B>wB?�BA B@B?BA�BB'B@OBB�BF?BEmBD3BC�BGEBUgB^�Br|B�B�KB�DB�pB�)B�BB�dB��B��B��B�8B�"B�B�B��BSBAB��BPB�B	BqB�B)�B/B2�B-�B*�B(�B'RB0UB,=BOB*�BqBIRB�B \B)_B3�B2�B.B-�B1[B:�B4nB9�B9�B4�B9�B7B4�BE�B@�BF�BAUBF�BH�BJ�BS�BX�BXBWsBYBӏBWsBW
BV�B`BBk�Bj�B�uB��B�B~(B}�B}�B~]B�B~]B}VB|�B.B�B~]B�=B�1B�lB��B�*B��B�\B�\B�OB��B�zB�B�VB�@B�4B��B�!B��B��B�FB��B��B��B�eB��B��B�*B��B��B��B��B��B��B�}B�mB�dB�^B�OB�eB�kB�IB�0B�dB�6B��B��B�=B��B�B�B��B��B��B�_B�+B��B��B�{B��B��B��B�;By�BxBu�BpBuZBrGBl�BtBy	B{�B~(B{JBzDB}VB{BzB.By�Bw�BcB�iB��B��B�DB��B�;B��B�B��B��B��B��B��B�B�MB�B��B�GB�iB�B��B��B�_B�B�+B��B�B�AB��B��B��B�\B�!B�IB�~B��B�'B�tB��B��B��B�$B��B��B��BȀB�BB�pBҽB�vB�KB�B��B��B��BȴB�tB��BƨB�3B��B�'B�zB��B��B��BÖB��B�*B�jB͟B�jBȴB��B�}B�6B��B��B�FB��B��BĜB�9B��B��B�B��B��B��B��B��B��B�7B��B�@B�B��B��B��B�MB�VB��B�{B��B�;B�uB�;B��B��B{�Bx�BsBq�BrGBm�Bo5BjBjBp�Bl�Ba�B`BXEBW�BV�BU�Bo�BO�B6B8RBT,Bw2B@�BQ�BH�BFtBB�BR�BEmBB�B@�B@B?�B@�BK)BC�BI�B>BBIB:^B*eB,=B2aB1[B&�B8�B,�B>�B$�B�BFB�B�BPBMB�BB�BoB��B��B�.B�B�.B��BB��B�B�(B�"B�B��B��B��B�B�B��B��B��B��B��BӏB�KB��B��BҽB��B��B�*B��B��B�$B�<B�hB��B��B��B�3B��B�}B��B�aB��B�eB�eB��B�nB��B�B�hB�:B��B�kB��B��B�"B��B��B��B��B�_By�Bx8BxBu%Bm�B~�Bd�Bd�Bk�BaHBZ�BY�BNpBO�B=�B9�B8�B7�B6�B4�B,�B7�B#:B.}B-wBVB#nBxBB�B�B�B9XB�B�B%�B�BkB
�B�B
�sB
�,B
��B
��B
ĜB
�^B
��B
��B
�EB
�#B
�tB
��B
��B
��B
�B
��B
�LB
��B
�'B
�IB
�=B
��B
��B
��B
�B
�MB
�MB
�SB
��B
�:B
�oB
��B
��B
��B
�PG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                                                                                                                                                    G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Rapid salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                    202202042329482022020423294820220204232948202202042329482022020423294820220204232948SI  SI  ARFMARFM                                                                                                                                                2021080619165720210806191657IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021081622010920210816220109QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021081622010920210816220109QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2022012609365120220126093651IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2022020423295320220204232953IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2022020423295320220204232953IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2022020423295320220204232953IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                