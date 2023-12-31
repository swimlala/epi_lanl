CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2018-10-29T01:35:46Z creation; 2023-04-26T19:14:27Z DMQC;      
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
_FillValue                    �xArgo profile    3.1 1.2 19500101000000  20181029013546  20230426191427  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL                  AA  AOAO7316_008644_029                 7316_008644_029                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @،�C�\�@،�C�\�11  @،�r� �@،�r� �@*> ѷY@*> ѷY�d~��"��d~��"�11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�\)@   @@  @�  @�G�@�G�@�  A ��A��A!G�A,(�A@  AaG�A�  A�  A�Q�A�  A�  A�  A�  A�  A��B�
B�
B  B (�B(Q�B0(�B8  B@  BH  BP  BX(�B`  Bg�Bp  BxQ�B�  B�  B�=qB�  B�  B��B��B�{B�{B��
B��B�{B�(�B�{B�  B��B�{B�{B�{B��B��B��B�  B�{B�{B�{B�(�B�{B�  B��B��
B�  C   C  C
=C  C  C	��C��C��C�C  C
=C
=C
=C
=C{C
=C   C"  C$  C&
=C({C*
=C,  C-��C/��C2  C4{C6{C8  C:  C<  C>  C@  CA��CD
=CF  CG��CJ  CL
=CM��CP  CR{CT
=CV
=CX
=CZ  C[��C^
=C`
=Cb  Cd  Cf  Ch  Ci��Ck��Cn  Cp{Cr
=Cs��Cv  Cx{Cz
=C|  C}��C�C�  C�C�C�  C���C���C���C���C���C�  C�
=C�C�  C�C�C�
=C�C���C���C�  C�  C���C���C�  C�  C�C�C�  C�  C���C���C�  C�C�  C�  C�C�C���C�  C�C�C�C�  C�  C�C�  C���C�  C�C�  C�  C���C���C���C�  C�  C�  C�  C���C���C�  C�C�C�C�C�  C�  C�  C�  C�  C�  C���C���C���C���C�  C�  C���C�  C�C�C���C���C�C�
=C�
=C�  C���C�  C�  C�C�C�  C���C���C���C���C���C���C���C���C���C���C�  C�C�C�  C���C���C���C���C�  C�  C�C�
=C�
=C�C�  C���C���C���C�  C�  C���C�C�  C�  D   D � D  D}qD�D��D  D}qD�qD}qD  D� D�qD}qD  D}qD  D��D	  D	� D
  D
}qD  D� D�D� D�qD}qD��D� DD��D�D��D�qD}qD  D}qD  D��D�D��D�D� D�D��D�D�D  Dz�D�D��D  D� D�D� D�qD� D�D� D�qD� D  D��D�qD z�D �qD!}qD!��D"� D#  D#}qD#��D$� D%  D%� D&�D&�D'�D'� D(D(��D)�D)��D*�D*� D+�D+�D,�D,� D,�qD-z�D.  D.}qD.��D/}qD/�qD0}qD0��D1}qD2  D2��D2�qD3}qD4  D4}qD5  D5��D6  D6�D7�D7��D8D8� D8��D9z�D:  D:� D:��D;z�D;�qD<}qD=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DA�qDB}qDB�qDC� DD  DD}qDD�qDE� DE�qDF� DG  DG}qDH  DH� DI�DI��DJ�DJ�DK  DK}qDL  DL� DM  DM� DM�qDN}qDN�qDO��DP  DP� DQ  DQ}qDQ��DR}qDS  DS� DT  DT� DU�DU� DV  DV��DW  DWz�DW��DX}qDY  DY}qDZ  DZ� D[  D[� D[�qD\}qD\�qD]��D^�D^� D_  D_��D`�D`}qD`�qDa��Db  Db}qDc  Dc� Dd  Dd� De  De� De�qDf}qDg�Dg� Dh  Dh� Di  Di� Dj  Dj��Dk�Dk}qDk�qDl}qDl��Dm}qDn  Dn��Do  Do��Dp�Dp� Dp�qDq}qDr�Dr��Ds  Ds}qDt  Dt� Dt��Du}qDv  Dv� Dw  Dw}qDw�qDx� Dy  Dy� Dz  Dz��D{�D{� D|  D|� D}  D}� D~  D~z�D  D��D�  D�@ D�~�D�� D�HD�@ D�� D�� D�  D�AHD�~�D��qD�HD�@ D�}qD��qD���D�>�D�� D��HD��D�AHD�� D�� D�  D�AHD�}qD��qD�  D�AHD��HD��HD�  D�@ D��HD�D���D�>�D��HD�� D�  D�AHD��HD�� D�HD�AHD�~�D��qD�  D�AHD��HD�� D�  D�@ D�� D�� D�  D�=qD�}qD�� D�  D�@ D�� D�� D�HD�AHD�~�D���D�  D�@ D�~�D��HD�HD�@ D�~�D���D���D�AHD�� D�� D�  D�@ D��HD�� D�  D�>�D�~�D�� D�  D�AHD��HD�� D���D�AHD��HD�� D�  D�>�D�� D���D���D�@ D�~�D�� D�HD�>�D�~�D�� D���D�>�D�� D�� D���D�@ D�� D�� D���D�>�D�� D���D���D�>�D�� D�� D��qD�>�D�~�D�� D���D�=qD�� D�� D�HD�AHD��HD�� D�HD�AHD��HD��HD�  D�AHD���D�D�  D�>�D�� D�� D�  D�@ D�� D���D���D�>�D�� D��HD�HD�@ D��HD�� D���D�@ D�� D���D�  D�AHD�� D��HD��D�B�D�~�D�� D���D�@ D�� D��HD��D�B�D��HD��HD�HD�B�D�� D��qD��qD�>�D��HD�� D���D�AHD�� D���D�HD�@ D�� D��HD�HD�AHD�� D��HD��D�AHD�� D��qD�  D�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD��HD�D�HD�>�D�� D��HD�  D�>�D��HD�� D�  D�AHD��HD�� D��D�B�D��HD��HD���D�>�D�}qD��qD�  D�@ D�� D�� D��qD�>�D��HD��HD�  D�@ D��HD�� D�HD�B�D��HD���D�  D�@ D�}qD���D�HD�@ D��HD�D�HD�@ D D�� D�  D�>�D�~�DýqD��qD�>�D�~�DĽqD���D�@ Dŀ D�� D���D�>�D�~�Dƾ�D�HD�C�Dǂ�D��HD�  D�>�D�~�DȽqD���D�>�Dɀ Dɾ�D���D�@ Dʀ D�� D�HD�AHDˁHD�� D�  D�>�D�~�D̾�D��qD�=qD�}qD�� D��D�AHD΀ Dξ�D��qD�@ DρHD�� D���D�@ DЁHDо�D�  D�AHD�}qD�� D�HD�AHDҁHD�� D�  D�AHDӀ D�� D�  D�@ DԁHD�� D�  D�@ DՁHD��HD�  D�=qD�~�D־�D�  D�>�DׁHD�� D�  D�@ D؀ D�� D�  D�@ D�}qDٽqD�  D�>�D�~�D��HD�HD�@ D�~�D۾�D���D�AHD܀ Dܾ�D��qD�>�D�~�Dݾ�D���D�@ Dހ D��HD�  D�>�D߀ D�� D�  D�@ D�� D��HD�HD�@ D�HD��HD�  D�>�D�~�D�� D�HD�AHD� D㾸D�  D�@ D� D�� D�  D�AHD� D�� D�  D�>�D� D�� D�  D�@ D� D�� D�HD�@ D� D�� D�  D�@ D� D��HD�  D�=qD�~�D�� D���D�>�D�~�D뾸D�  D�>�D� D��HD�HD�AHD킏D�� D�  D�AHD� DD�  D�@ D� D�� D�  D�@ D�~�D��HD��D�AHD� D�� D�  D�@ D� D�� D�HD�@ D� D�� D�  D�AHD� D���D�  D�AHD�� D�� D�  D�@ D�~�D���D�HD�AHD�~�D�� D�  D�>�D�~�D�� D�  D�>�D�~�D�� D�HD�<)D���>�?u?���?�Q�?�G�@�@��@+�@B�\@^�R@n{@��@�{@�Q�@��@�\)@���@Ǯ@�33@�(�@���@�AG�A�A�A�\A�Ap�A$z�A*�HA0  A6ffA<��AC�
AI��AO\)AVffA\��Ab�\Ah��Ao\)AuA{�A���A�(�A��RA��A��A��A��A��A�
=A�G�A�(�A�A��A��A��
A�A��A��A��
A��A��A���A�33A��A��A�G�A��HA��A�
=A���A��HA���A�ffA�Q�Aʏ\A�z�A�{A�Q�A��AӅA�{A�  A�G�A�33A�p�A�
=A��A��HA���A�ffA��A�\A��
A�A�A�A��HA��A��RA�Q�A�=qA�(�A�A�
=B ��B��B{B
=B(�B��BB�RB\)B(�B	G�B	�B
�RB�
B��BG�BffB
=B�
B��BB�\B33B(�B��BBffB�BQ�B��BB�RB\)B(�B�B{B�RB\)B Q�B!G�B!�B"�\B#�B$z�B%�B%�B'
=B'�B(Q�B)G�B*=qB*�HB+�B,��B-G�B.{B/
=B/�
B0z�B1G�B2=qB3
=B3�B4z�B5G�B5�B6�RB7�B8z�B8��B9B:�RB;�B<(�B<��B=�B>�\B?33B@Q�BA�BABB�RBC�
BDz�BEp�BFffBG33BH(�BI�BJ=qBK\)BL(�BM�BNffBO�BP��BQp�BR�HBT  BT��BV{BW\)BXz�BY��BZ�\B[�
B]�B^=qB_\)B`z�Ba��Bc
=Bd(�Be�BfffBg�
Bi�Bj{Bk\)Bl��Bn=qBo�Bpz�Bq�Bs\)Bt��Bv{Bw33Bxz�By�B{�B|��B}�B\)B�ffB��B�B�Q�B�
=B�B�Q�B��HB�p�B�  B��\B�
=B�\)B��B�{B�z�B��RB���B�33B��B�B�  B�=qB�z�B���B�33B�\)B���B��B�=qB��\B���B�
=B�G�B��B��B�Q�B���B��HB�
=B�\)B�B�{B�ffB��\B��HB�G�B���B��B�(�B�ffB��RB��B�p�B�B��B�=qB��\B���B�G�B���B��
B�{B�ffB���B��B�\)B���B�  B�Q�B��RB���B�G�B���B��B�=qB��\B���B�G�B��B��
B�(�B�z�B���B�33B��B��B�=qB��\B���B��B�p�B��
B�(�B��\B���B��B�\)B���B�  B�Q�B��RB�
=B�\)B��B��B�(�B��\B��HB�G�B���B�  B�ffB���B�
=B�G�B��B�{B�z�B��HB�33B���B��B�=qB���B�
=B�p�B��
B�Q�B���B���B�G�B��B�  B�z�B��HB�G�B��B�{B�z�B���B��B��B��B�ffB���B�G�B���B�{B�ffB��HB�33B���B��B�ffB���B�\)B�B�(�B��\B���B�\)B�B�{B���B�
=B���B�  B�z�B��HB�\)B��B�{B�z�B���B�p�B��B�Q�B���B�G�B�B�=qB¸RB��BÙ�B�  B�ffB��HB�G�B�B�(�Bƣ�B��BǅB�  B�z�B���B�\)B��
B�Q�B���B�G�B�B�=qB̸RB�33BͮB�=qBΣ�B��Bϙ�B�(�BЏ\B�
=Bљ�B�(�Bң�B�
=BӅB�  B�z�B���B�p�B��B�ffB��HB�\)B�B�Q�BظRB�33BٮB�(�Bڣ�B��Bۙ�B�{Bܣ�B��Bݙ�B�(�Bޣ�B��Bߙ�B�(�B��B��BᙚB�{B�\B��B㙚B�  B�\B�
=B�B�  B�z�B���B�p�B�  B�z�B���B�p�B��B�ffB���B�\)B��B�ffB��HB�\)B��
B�ffB���B�\)B��
B�Q�B��HB�G�B��
B�Q�B���B�G�B�B�=qB��RB�33B��B�(�B��RB��B��B�(�B��RB�G�B�B�=qB���B�\)B��
B�ffB��HB�p�B�  B�z�B�
=B��C   C G�C �C ��C
=CQ�C��C�
C{CQ�C��C�HC�C\)C��C�C(�Cp�C�RC��C=qCz�CC
=CQ�C��C�C33C�C��C{C\)C��C�C	33C	z�C	��C
{C
ffC
�RC
=CQ�C��C�
C(�Cp�CC{CffC�C��C=qC�C��C�Cp�C�RC
=CQ�C��C�
C(�C�C�
C�C\)C�C��CQ�C��C��C=qCz�C��C{CffC�RC{CffC�C��CG�C��C��CG�C��C��C=qC�C�
C(�Cz�C�
C(�Cz�CC{CQ�C�C  C\)C��C  CG�C�\C�
C33C�\C�HC 33C �C ��C!�C!ffC!��C"�C"p�C"�RC#
=C#ffC#C${C$ffC$�C%
=C%ffC%C&
=C&\)C&�C&��C'\)C'�RC(
=C(Q�C(��C(��C)Q�C)��C)��C*=qC*�\C*�C+G�C+��C+�HC,33C,�\C,�C-=qC-�\C-�HC.33C.�\C.�C/33C/�C/�
C0=qC0��C0�C1=qC1�\C1�C2G�C2��C2��C3G�C3��C4  C4\)C4��C4��C5Q�C5�C6
=C6Q�C6��C7  C7\)C7�C8  C8Q�C8�C9  C9Q�C9�C:
=C:\)C:��C:��C;Q�C;�C<  C<Q�C<�C=
=C=\)C=��C>  C>\)C>�C>��C?\)C?�RC@  C@G�C@�CA  CAQ�CA��CB  CBQ�CB��CB��CCQ�CC��CC�CD=qCD��CD��CE=qCE�\CE��CFG�CF�CF�HCG=qCG�\CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                     ?�\)@   @@  @�  @�G�@�G�@�  A ��A��A!G�A,(�A@  AaG�A�  A�  A�Q�A�  A�  A�  A�  A�  A��B�
B�
B  B (�B(Q�B0(�B8  B@  BH  BP  BX(�B`  Bg�Bp  BxQ�B�  B�  B�=qB�  B�  B��B��B�{B�{B��
B��B�{B�(�B�{B�  B��B�{B�{B�{B��B��B��B�  B�{B�{B�{B�(�B�{B�  B��B��
B�  C   C  C
=C  C  C	��C��C��C�C  C
=C
=C
=C
=C{C
=C   C"  C$  C&
=C({C*
=C,  C-��C/��C2  C4{C6{C8  C:  C<  C>  C@  CA��CD
=CF  CG��CJ  CL
=CM��CP  CR{CT
=CV
=CX
=CZ  C[��C^
=C`
=Cb  Cd  Cf  Ch  Ci��Ck��Cn  Cp{Cr
=Cs��Cv  Cx{Cz
=C|  C}��C�C�  C�C�C�  C���C���C���C���C���C�  C�
=C�C�  C�C�C�
=C�C���C���C�  C�  C���C���C�  C�  C�C�C�  C�  C���C���C�  C�C�  C�  C�C�C���C�  C�C�C�C�  C�  C�C�  C���C�  C�C�  C�  C���C���C���C�  C�  C�  C�  C���C���C�  C�C�C�C�C�  C�  C�  C�  C�  C�  C���C���C���C���C�  C�  C���C�  C�C�C���C���C�C�
=C�
=C�  C���C�  C�  C�C�C�  C���C���C���C���C���C���C���C���C���C���C�  C�C�C�  C���C���C���C���C�  C�  C�C�
=C�
=C�C�  C���C���C���C�  C�  C���C�C�  C�  D   D � D  D}qD�D��D  D}qD�qD}qD  D� D�qD}qD  D}qD  D��D	  D	� D
  D
}qD  D� D�D� D�qD}qD��D� DD��D�D��D�qD}qD  D}qD  D��D�D��D�D� D�D��D�D�D  Dz�D�D��D  D� D�D� D�qD� D�D� D�qD� D  D��D�qD z�D �qD!}qD!��D"� D#  D#}qD#��D$� D%  D%� D&�D&�D'�D'� D(D(��D)�D)��D*�D*� D+�D+�D,�D,� D,�qD-z�D.  D.}qD.��D/}qD/�qD0}qD0��D1}qD2  D2��D2�qD3}qD4  D4}qD5  D5��D6  D6�D7�D7��D8D8� D8��D9z�D:  D:� D:��D;z�D;�qD<}qD=  D=� D>  D>� D?  D?� D@  D@� DA  DA� DA�qDB}qDB�qDC� DD  DD}qDD�qDE� DE�qDF� DG  DG}qDH  DH� DI�DI��DJ�DJ�DK  DK}qDL  DL� DM  DM� DM�qDN}qDN�qDO��DP  DP� DQ  DQ}qDQ��DR}qDS  DS� DT  DT� DU�DU� DV  DV��DW  DWz�DW��DX}qDY  DY}qDZ  DZ� D[  D[� D[�qD\}qD\�qD]��D^�D^� D_  D_��D`�D`}qD`�qDa��Db  Db}qDc  Dc� Dd  Dd� De  De� De�qDf}qDg�Dg� Dh  Dh� Di  Di� Dj  Dj��Dk�Dk}qDk�qDl}qDl��Dm}qDn  Dn��Do  Do��Dp�Dp� Dp�qDq}qDr�Dr��Ds  Ds}qDt  Dt� Dt��Du}qDv  Dv� Dw  Dw}qDw�qDx� Dy  Dy� Dz  Dz��D{�D{� D|  D|� D}  D}� D~  D~z�D  D��D�  D�@ D�~�D�� D�HD�@ D�� D�� D�  D�AHD�~�D��qD�HD�@ D�}qD��qD���D�>�D�� D��HD��D�AHD�� D�� D�  D�AHD�}qD��qD�  D�AHD��HD��HD�  D�@ D��HD�D���D�>�D��HD�� D�  D�AHD��HD�� D�HD�AHD�~�D��qD�  D�AHD��HD�� D�  D�@ D�� D�� D�  D�=qD�}qD�� D�  D�@ D�� D�� D�HD�AHD�~�D���D�  D�@ D�~�D��HD�HD�@ D�~�D���D���D�AHD�� D�� D�  D�@ D��HD�� D�  D�>�D�~�D�� D�  D�AHD��HD�� D���D�AHD��HD�� D�  D�>�D�� D���D���D�@ D�~�D�� D�HD�>�D�~�D�� D���D�>�D�� D�� D���D�@ D�� D�� D���D�>�D�� D���D���D�>�D�� D�� D��qD�>�D�~�D�� D���D�=qD�� D�� D�HD�AHD��HD�� D�HD�AHD��HD��HD�  D�AHD���D�D�  D�>�D�� D�� D�  D�@ D�� D���D���D�>�D�� D��HD�HD�@ D��HD�� D���D�@ D�� D���D�  D�AHD�� D��HD��D�B�D�~�D�� D���D�@ D�� D��HD��D�B�D��HD��HD�HD�B�D�� D��qD��qD�>�D��HD�� D���D�AHD�� D���D�HD�@ D�� D��HD�HD�AHD�� D��HD��D�AHD�� D��qD�  D�AHD��HD��HD�  D�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD��HD�D�HD�>�D�� D��HD�  D�>�D��HD�� D�  D�AHD��HD�� D��D�B�D��HD��HD���D�>�D�}qD��qD�  D�@ D�� D�� D��qD�>�D��HD��HD�  D�@ D��HD�� D�HD�B�D��HD���D�  D�@ D�}qD���D�HD�@ D��HD�D�HD�@ D D�� D�  D�>�D�~�DýqD��qD�>�D�~�DĽqD���D�@ Dŀ D�� D���D�>�D�~�Dƾ�D�HD�C�Dǂ�D��HD�  D�>�D�~�DȽqD���D�>�Dɀ Dɾ�D���D�@ Dʀ D�� D�HD�AHDˁHD�� D�  D�>�D�~�D̾�D��qD�=qD�}qD�� D��D�AHD΀ Dξ�D��qD�@ DρHD�� D���D�@ DЁHDо�D�  D�AHD�}qD�� D�HD�AHDҁHD�� D�  D�AHDӀ D�� D�  D�@ DԁHD�� D�  D�@ DՁHD��HD�  D�=qD�~�D־�D�  D�>�DׁHD�� D�  D�@ D؀ D�� D�  D�@ D�}qDٽqD�  D�>�D�~�D��HD�HD�@ D�~�D۾�D���D�AHD܀ Dܾ�D��qD�>�D�~�Dݾ�D���D�@ Dހ D��HD�  D�>�D߀ D�� D�  D�@ D�� D��HD�HD�@ D�HD��HD�  D�>�D�~�D�� D�HD�AHD� D㾸D�  D�@ D� D�� D�  D�AHD� D�� D�  D�>�D� D�� D�  D�@ D� D�� D�HD�@ D� D�� D�  D�@ D� D��HD�  D�=qD�~�D�� D���D�>�D�~�D뾸D�  D�>�D� D��HD�HD�AHD킏D�� D�  D�AHD� DD�  D�@ D� D�� D�  D�@ D�~�D��HD��D�AHD� D�� D�  D�@ D� D�� D�HD�@ D� D�� D�  D�AHD� D���D�  D�AHD�� D�� D�  D�@ D�~�D���D�HD�AHD�~�D�� D�  D�>�D�~�D�� D�  D�>�D�~�D�� D�HD�<)G�O�>�?u?���?�Q�?�G�@�@��@+�@B�\@^�R@n{@��@�{@�Q�@��@�\)@���@Ǯ@�33@�(�@���@�AG�A�A�A�\A�Ap�A$z�A*�HA0  A6ffA<��AC�
AI��AO\)AVffA\��Ab�\Ah��Ao\)AuA{�A���A�(�A��RA��A��A��A��A��A�
=A�G�A�(�A�A��A��A��
A�A��A��A��
A��A��A���A�33A��A��A�G�A��HA��A�
=A���A��HA���A�ffA�Q�Aʏ\A�z�A�{A�Q�A��AӅA�{A�  A�G�A�33A�p�A�
=A��A��HA���A�ffA��A�\A��
A�A�A�A��HA��A��RA�Q�A�=qA�(�A�A�
=B ��B��B{B
=B(�B��BB�RB\)B(�B	G�B	�B
�RB�
B��BG�BffB
=B�
B��BB�\B33B(�B��BBffB�BQ�B��BB�RB\)B(�B�B{B�RB\)B Q�B!G�B!�B"�\B#�B$z�B%�B%�B'
=B'�B(Q�B)G�B*=qB*�HB+�B,��B-G�B.{B/
=B/�
B0z�B1G�B2=qB3
=B3�B4z�B5G�B5�B6�RB7�B8z�B8��B9B:�RB;�B<(�B<��B=�B>�\B?33B@Q�BA�BABB�RBC�
BDz�BEp�BFffBG33BH(�BI�BJ=qBK\)BL(�BM�BNffBO�BP��BQp�BR�HBT  BT��BV{BW\)BXz�BY��BZ�\B[�
B]�B^=qB_\)B`z�Ba��Bc
=Bd(�Be�BfffBg�
Bi�Bj{Bk\)Bl��Bn=qBo�Bpz�Bq�Bs\)Bt��Bv{Bw33Bxz�By�B{�B|��B}�B\)B�ffB��B�B�Q�B�
=B�B�Q�B��HB�p�B�  B��\B�
=B�\)B��B�{B�z�B��RB���B�33B��B�B�  B�=qB�z�B���B�33B�\)B���B��B�=qB��\B���B�
=B�G�B��B��B�Q�B���B��HB�
=B�\)B�B�{B�ffB��\B��HB�G�B���B��B�(�B�ffB��RB��B�p�B�B��B�=qB��\B���B�G�B���B��
B�{B�ffB���B��B�\)B���B�  B�Q�B��RB���B�G�B���B��B�=qB��\B���B�G�B��B��
B�(�B�z�B���B�33B��B��B�=qB��\B���B��B�p�B��
B�(�B��\B���B��B�\)B���B�  B�Q�B��RB�
=B�\)B��B��B�(�B��\B��HB�G�B���B�  B�ffB���B�
=B�G�B��B�{B�z�B��HB�33B���B��B�=qB���B�
=B�p�B��
B�Q�B���B���B�G�B��B�  B�z�B��HB�G�B��B�{B�z�B���B��B��B��B�ffB���B�G�B���B�{B�ffB��HB�33B���B��B�ffB���B�\)B�B�(�B��\B���B�\)B�B�{B���B�
=B���B�  B�z�B��HB�\)B��B�{B�z�B���B�p�B��B�Q�B���B�G�B�B�=qB¸RB��BÙ�B�  B�ffB��HB�G�B�B�(�Bƣ�B��BǅB�  B�z�B���B�\)B��
B�Q�B���B�G�B�B�=qB̸RB�33BͮB�=qBΣ�B��Bϙ�B�(�BЏ\B�
=Bљ�B�(�Bң�B�
=BӅB�  B�z�B���B�p�B��B�ffB��HB�\)B�B�Q�BظRB�33BٮB�(�Bڣ�B��Bۙ�B�{Bܣ�B��Bݙ�B�(�Bޣ�B��Bߙ�B�(�B��B��BᙚB�{B�\B��B㙚B�  B�\B�
=B�B�  B�z�B���B�p�B�  B�z�B���B�p�B��B�ffB���B�\)B��B�ffB��HB�\)B��
B�ffB���B�\)B��
B�Q�B��HB�G�B��
B�Q�B���B�G�B�B�=qB��RB�33B��B�(�B��RB��B��B�(�B��RB�G�B�B�=qB���B�\)B��
B�ffB��HB�p�B�  B�z�B�
=B��C   C G�C �C ��C
=CQ�C��C�
C{CQ�C��C�HC�C\)C��C�C(�Cp�C�RC��C=qCz�CC
=CQ�C��C�C33C�C��C{C\)C��C�C	33C	z�C	��C
{C
ffC
�RC
=CQ�C��C�
C(�Cp�CC{CffC�C��C=qC�C��C�Cp�C�RC
=CQ�C��C�
C(�C�C�
C�C\)C�C��CQ�C��C��C=qCz�C��C{CffC�RC{CffC�C��CG�C��C��CG�C��C��C=qC�C�
C(�Cz�C�
C(�Cz�CC{CQ�C�C  C\)C��C  CG�C�\C�
C33C�\C�HC 33C �C ��C!�C!ffC!��C"�C"p�C"�RC#
=C#ffC#C${C$ffC$�C%
=C%ffC%C&
=C&\)C&�C&��C'\)C'�RC(
=C(Q�C(��C(��C)Q�C)��C)��C*=qC*�\C*�C+G�C+��C+�HC,33C,�\C,�C-=qC-�\C-�HC.33C.�\C.�C/33C/�C/�
C0=qC0��C0�C1=qC1�\C1�C2G�C2��C2��C3G�C3��C4  C4\)C4��C4��C5Q�C5�C6
=C6Q�C6��C7  C7\)C7�C8  C8Q�C8�C9  C9Q�C9�C:
=C:\)C:��C:��C;Q�C;�C<  C<Q�C<�C=
=C=\)C=��C>  C>\)C>�C>��C?\)C?�RC@  C@G�C@�CA  CAQ�CA��CB  CBQ�CB��CB��CCQ�CC��CC�CD=qCD��CD��CE=qCE�\CE��CFG�CF�CF�HCG=qCG�\CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                     @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@�2@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�Q�A�K�A�O�A�M�A�I�A�;dA�;dA�(�A���A��;A���A�ĜA�-A�PA�r�A�bNA�VA�O�A�I�A�A�A�;dA�;dA�;dA�;dA�9XA�7LA�7LA�33A�33A�`BA��A��HA��/A�JA�C�Aқ�A�`BA�C�A���A�Q�A��A�A�%A�t�A�ĜA�ZA�v�A�=qA��A��wA��wA�n�A�%A�=qA���A�n�A�`BA���A���A���A���A���A��A�|�A��9A���A��PA��A�K�A��A��A��A�7LA�1'A�bNA���A�p�A�XAVAx�\As�#Aot�Ak&�AiS�Ag�hAa�;A\�AV��AQ�
AN-AL=qAI��AF9XAC�A>M�A=dZA=C�A;t�A7l�A5|�A3x�A2�A0�DA/`BA.�`A.z�A-�^A,�A+�A*�RA)t�A)�A+oA,bNA,�/A+��A)��A(��A(�A(=qA'�A&�uA%/A$�jA$�A#�A"~�A!hsA  �A I�A ZA ��A ��A �A 1A {A 5?A JAt�AVAĜAbAO�A�/A�+A=qA��A��AbA�wA��A&�A�A��A�RA��A`BA
=A�yA��A�jA�A��A\)A��A$�A"�AA�AȴA��A�\A~�A��AA�/A�A7LA�/A�HA%A��A��A�-A�AhsA��A~�A$�AAt�AO�A�7AdZA\)AK�AȴA�\AE�A�^A7LA
�yA
n�A
-A	A	G�A�A5?A��A��AC�A�A��Ar�A�A|�AhsA�A��A��A^5A�
A�A"�A�A�+A�A��AS�A �9A ~�A VA 1@�K�@�+@���@��@�I�@�t�@�@�~�@��#@���@��D@�1'@�l�@���@��^@��/@�bN@��@�@�dZ@��@��@�`B@��;@��@��@홚@�/@�9@�j@� �@�+@�5?@�@�X@�G�@�Q�@�C�@��@�\@�V@�-@��@�x�@�z�@�A�@��@��@�\)@�R@�5?@���@�7@�7L@�%@��@�Q�@��@��@��@�hs@�%@�I�@۶F@�o@ڸR@���@��@���@�Ĝ@ج@�I�@�1@ׅ@��@���@�ff@��#@�hs@���@Ԭ@�bN@� �@Ӆ@�K�@�"�@��@�5?@���@с@�`B@�Ĝ@�j@�9X@��
@���@ϥ�@�;d@Η�@�=q@͙�@˥�@��@���@ʇ+@���@ɩ�@�X@�/@�V@��@ȴ9@�A�@��m@�+@�{@őh@���@Ĵ9@ă@�Q�@��m@î@�dZ@��y@�n�@��@��@��T@���@��@���@�bN@��@�S�@��H@���@�E�@���@�O�@�&�@��`@��@�b@�|�@�o@��H@��!@��\@�n�@�@�X@��@��@�ƨ@�"�@���@�n�@�J@�@���@��@��@�j@���@�+@��H@�~�@�$�@���@�Ĝ@��m@�S�@�@���@�n�@�^5@�E�@�$�@��@�@�7L@��`@��u@�(�@�C�@���@�$�@��-@���@��7@��7@�x�@���@��@�1@��@�S�@���@��@���@�G�@��j@�j@�  @�|�@�+@���@�5?@���@��@�Z@��@��y@���@�~�@�=q@�J@��T@���@�p�@�V@���@���@�j@�9X@��m@��@�t�@�l�@�dZ@�K�@�o@���@�n�@��@��h@�/@���@�z�@�I�@�b@��@���@�;d@�@���@�E�@���@���@��7@�hs@�7L@�&�@�%@���@��@��D@��@��
@�t�@�\)@�C�@��H@���@�E�@��#@���@�G�@�/@���@���@�j@�A�@�(�@��@���@��F@�dZ@�=q@��^@�X@�?}@�/@�V@���@���@��u@�b@��@��@��@�@�ȴ@���@���@�~�@�~�@�V@�@�/@��@���@��`@���@���@��@�1'@��m@��
@�S�@���@�~�@�E�@�$�@���@���@��7@�p�@�O�@�V@���@��j@��u@�1@��P@��@���@��+@�v�@�-@��^@�p�@�&�@��/@��j@���@�r�@�Q�@�A�@�@~ȴ@~v�@}�@}/@|��@|Z@|�@{ƨ@{C�@{@z�\@y��@y�@x�9@xr�@x1'@w��@w|�@v��@u�-@uO�@u/@tZ@s��@sdZ@r�@r^5@q�@q��@q7L@pĜ@pbN@p1'@o�@o��@o�P@o
=@n�@n��@n$�@mp�@mV@lz�@k��@j�@j��@jM�@j�@i��@ix�@iX@g�;@f��@fv�@fff@fV@fE�@f5?@f$�@e�@e�T@e��@e`B@d�D@c33@b�!@b^5@a�@a��@`��@`Q�@`b@_��@_l�@_
=@^�+@^ff@^@]�h@]/@\�@\�@\Z@[�
@[@Z��@Z��@Z�!@ZJ@Y7L@Y7L@Y7L@Y�@X��@XbN@W�w@Wl�@W
=@V�R@V$�@U�-@U/@T��@T9X@S��@Sƨ@S@R�@Q��@Q��@Qx�@Q&�@PĜ@P1'@Pb@O|�@O;d@N��@NV@N$�@N@MO�@MV@L�/@LI�@K�m@K��@K"�@J�@J��@J�!@J�!@J�\@J~�@JM�@Ix�@IX@H��@H��@H�@Hb@G�;@GK�@F�y@F�+@FV@F5?@E��@D��@DI�@D(�@C��@Cƨ@Co@B��@B~�@B-@A�7@A�@@�`@@��@@A�@?��@?�@?�P@?K�@?;d@>ȴ@>5?@=��@=��@=O�@<��@<��@<Z@<1@;��@;ƨ@;��@;33@:��@:��@:=q@:�@9�#@9��@9X@8��@8�u@8Q�@81'@7�@7��@7�P@6�y@6E�@6@5�-@5`B@5�@4��@4j@4I�@41@3��@3"�@2��@2��@2�\@2~�@2M�@2J@1�#@1��@1X@0�u@0bN@0Q�@0bN@0Q�@/�@/��@/�w@/�@/\)@.�@.�+@.ff@.$�@.{@-��@-?}@,�j@,z�@,Z@,I�@,�@+ƨ@+��@+�@+S�@+@*��@*=q@)��@)��@)x�@)&�@(��@(�@(b@'��@'l�@';d@&�@&��@&�+@&5?@&{@%�@%�h@%p�@%`B@%V@$�j@$z�@$I�@$1@#�
@#�
@#�F@#t�@#S�@#"�@"�@"�!@"n�@!��@!�@ �u@�;@�w@�@l�@K�@;d@
=@�@ȴ@�+@V@5?@{@�@�@`B@V@�@�/@��@j@1@�m@�F@S�@"�@@�@��@��@n�@-@�^@��@x�@7L@��@�9@bN@1'@�@��@|�@�@ȴ@��@v�@E�@@�T@�-@`B@V@�@�D@I�@1@�
@��@"�@@�@�!@~�@n�@�@��@�7@hs@X@G�@7L@%@Ĝ@�@b@�@��@|�@l�@K�@�@��@��@�+@V@$�@�@��@�@?}@/@V@�j@Z@9X@��@ƨ@�F@t�@C�@o@
�H@
n�@
�@	�@	�@	�#@	�^@	��@	�7@	�7@	hs@	G�@	&�@��@�`@��@��@Ĝ@��@�@r�@bN@Q�@1'@ �@  @�@�;@��@�w@�@�P@|�@l�@;d@+@��@ȴ@��@�+@v�@v�@ff@E�@�T@@@�-@��@�h@�h@�hA�S�A�VA�M�A�I�A�O�A�I�A�K�A�Q�A�O�A�M�A�Q�A�O�A�E�A�M�A�O�A�A�A�C�A�7LA�5?A�9XA�A�A�9XA�"�A��A���A��A��A��A��#A���A��
A���A�jA�jA�^A�jA�ĜA�wA��A�A䟾A䗍A䗍A�\A�A�|�A�A�x�A�n�A�n�A�p�A�jA�l�A�l�A�bNA�^5A�bNA�^5A�XA�ZA�ZA�S�A�O�A�VA�S�A�S�A�XA�XA�O�A�O�A�Q�A�M�A�O�A�S�A�O�A�K�A�O�A�O�A�G�A�I�A�K�A�C�A�E�A�I�A�C�A�A�A�G�A�E�A�?}A�A�A�E�A�?}A�=qA�?}A�9XA�9XA�=qA�=qA�9XA�9XA�?}A�;dA�9XA�?}A�=qA�9XA�;dA�?}A�=qA�9XA�?}A�=qA�;dA�?}A�=qA�9XA�;dA�;dA�7LA�;dA�=qA�;dA�;dA�?}A�;dA�;dA�=qA�=qA�9XA�7LA�;dA�7LA�5?A�9XA�=qA�7LA�7LA�9XA�5?A�33A�7LA�9XA�5?A�33A�5?A�9XA�7LA�5?A�5?A�9XA�7LA�5?A�7LA�7LA�5?A�33A�9XA�7LA�5?A�9XA�5?A�1'A�33A�5?A�1'A�1'A�33A�5?A�1'A�1'A�5?A�33A�/A�33A�5?A�33A�33A�7LA�5?A�1'A�/A�-A�"�A��HA㝲A�t�A�5?A��;A⛦A�33AᙚA��
A�dZA�%A���Aߥ�A�G�A��HA�jA�`BA�"�A���A�t�A�(�A��/A���AٍPA�ZA�%A؉7A�r�A�ffA�dZA�bNA�Q�A�-A��A��Aײ-A�Q�A��Aֺ^A�+A�ȴAթ�A���A�|�AҾwA�|�A�ZA�+A��A��Aѡ�A�`BA�A�A�-A�-A�+A��A�`BA�1A���A���A��A�|�A��mẠ�A�x�A�M�A��A��AˬA˅A�\)A�K�A�=qA�(�A�oA�VA�A���A��TA��
A�ȴAʴ9Aʛ�A�~�A�ffA�Q�A�1'A���A���Aɉ7A�ZA�bA���A�l�A���A�9XAƗ�A�XA�bAŸRA�\)A�JA���Aĉ7AđhAħ�A���A���A���Aĥ�A�z�A�;dA��A�ĜAß�A�t�A�E�A��A©�A�S�A� �A��A��wA���A���A��DA�p�A�VA�5?A��A�dZA�A���A��A���A���A���A�%A��A��DA�|�A�z�A�dZA�I�A�bA���A���A�z�A�dZA�Q�A�C�A�33A�(�A�"�A��A�VA�JA��A��A��TA��A�ƨA��uA�dZA�?}A�VA���A�|�A�K�A�oA�K�A��7A�^5A�1'A���A���A�bNA�;dA��A���A��A�Q�A�  A��
A��^A��A�VA��A���A��+A�jA�=qA�-A��A��A��FA���A�~�A�VA�O�A�7LA���A��RA�?}A��wA��A�\)A�/A��A�
=A���A���A��A�ƨA�hsA�+A��A� �A�+A�
=A��yA��A��uA�hsA�M�A��A��RA��;A���A�^5A��A��-A��TA���A�ZA�;dA�1A��#A��RA�x�A�XA�"�A�A��A���A�v�A�&�A���A��A���A��A�^5A��yA��A���A�XA��`A�K�A��A�K�A���A�v�A��A��PA���A���A��A��A�A� �A�$�A�A�A�I�A�`BA�l�A�O�A��#A�`BA���A���A�z�A�p�A�v�A��\A���A��!A��!A���A���A�n�A�Q�A� �A��mA��^A��uA�hsA�-A��
A���A�XA�A���A��-A��DA�jA�VA�A�A�-A��A�
=A��yA��!A��A�VA�9XA��A��
A�ffA�$�A���A��A��A��`A��
A���A�A��FA���A�v�A�hsA� �A��A�33A�M�A��
A���A���A��DA�l�A�VA��wA���A�ZA�5?A��HA���A�A�A��A�"�A�+A�
=A�XA��`A���A��A�r�A�l�A�ffA�ZA�K�A���A���A�x�A�`BA�O�A�C�A�/A��A�|�A�1A��!A�&�A���A�`BA��A���A�{A���A��hA�Q�A���A��wA��DA�bNA�(�A���A���A�p�A�A�A���A��RA�VA���A��hA�p�A�  A��/A��RA���A��A�r�A�ffA�\)A�M�A�=qA�33A�33A�1'A�{A��A���A���A��hA��A�t�A�jA�hsA�dZA�bNA�`BA�^5A�ZA�XA�VA�Q�A�C�A��A���A�ffA�7A}�A{K�Az~�Az$�Ay�-Ay33Axn�AwhsAv��Au�mAu/At�uAs��Asl�ArffAq�hAqO�ApjAox�Ao33AnA�Al�/Al5?AkAkG�Aj�Ajr�AjA�AjAi�#Ai�-Ail�Ai�Ah�!Ah�uAh9XAg��Ag��AgS�Ag�Af�RAf1Ad�`A`E�A_�
A_��A_�wA_�-A_�A^�HA]33AY��AXAW�^AWhsAW33AV��AV��AVv�AVbAT��AR�+AQ�APZAO�AOAN�/AN�RANQ�AM�
AM�AMhsAM7LALZAK��AK�;AK��AK�
AK�^AK��AJ�+AHQ�AGx�AG�AGVAF�HAF��AE��AES�AE7LAEAD-AB�AB5?ABAA�A?�A>v�A>{A=��A=��A=t�A=p�A=dZA=`BA=`BA=`BA=\)A=O�A=G�A=G�A=C�A=;dA=�A<�jA<1'A;��A;dZA9|�A8bNA8�A7�^A7p�A7
=A6�9A6��A6�A6bNA6bA5
=A3�
A2��A3VA3�PA3�#A3�FA3��A3p�A3K�A333A3"�A2�HA2VA1�^A1S�A0�A0��A0{A/�-A/l�A/hsA/dZA/`BA/`BA/O�A/K�A//A/A.�HA.��A.�RA.��A.�\A.�\A.�+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                     A�Q�A�K�A�O�A�M�A�I�A�;dA�;dA�(�A���A��;A���A�ĜA�-A�PA�r�A�bNA�VA�O�A�I�A�A�A�;dA�;dA�;dA�;dA�9XA�7LA�7LA�33A�33A�`BA��A��HA��/A�JA�C�Aқ�A�`BA�C�A���A�Q�A��A�A�%A�t�A�ĜA�ZA�v�A�=qA��A��wA��wA�n�A�%A�=qA���A�n�A�`BA���A���A���A���A���A��A�|�A��9A���A��PA��A�K�A��A��A��A�7LA�1'A�bNA���A�p�A�XAVAx�\As�#Aot�Ak&�AiS�Ag�hAa�;A\�AV��AQ�
AN-AL=qAI��AF9XAC�A>M�A=dZA=C�A;t�A7l�A5|�A3x�A2�A0�DA/`BA.�`A.z�A-�^A,�A+�A*�RA)t�A)�A+oA,bNA,�/A+��A)��A(��A(�A(=qA'�A&�uA%/A$�jA$�A#�A"~�A!hsA  �A I�A ZA ��A ��A �A 1A {A 5?A JAt�AVAĜAbAO�A�/A�+A=qA��A��AbA�wA��A&�A�A��A�RA��A`BA
=A�yA��A�jA�A��A\)A��A$�A"�AA�AȴA��A�\A~�A��AA�/A�A7LA�/A�HA%A��A��A�-A�AhsA��A~�A$�AAt�AO�A�7AdZA\)AK�AȴA�\AE�A�^A7LA
�yA
n�A
-A	A	G�A�A5?A��A��AC�A�A��Ar�A�A|�AhsA�A��A��A^5A�
A�A"�A�A�+A�A��AS�A �9A ~�A VA 1@�K�@�+@���@��@�I�@�t�@�@�~�@��#@���@��D@�1'@�l�@���@��^@��/@�bN@��@�@�dZ@��@��@�`B@��;@��@��@홚@�/@�9@�j@� �@�+@�5?@�@�X@�G�@�Q�@�C�@��@�\@�V@�-@��@�x�@�z�@�A�@��@��@�\)@�R@�5?@���@�7@�7L@�%@��@�Q�@��@��@��@�hs@�%@�I�@۶F@�o@ڸR@���@��@���@�Ĝ@ج@�I�@�1@ׅ@��@���@�ff@��#@�hs@���@Ԭ@�bN@� �@Ӆ@�K�@�"�@��@�5?@���@с@�`B@�Ĝ@�j@�9X@��
@���@ϥ�@�;d@Η�@�=q@͙�@˥�@��@���@ʇ+@���@ɩ�@�X@�/@�V@��@ȴ9@�A�@��m@�+@�{@őh@���@Ĵ9@ă@�Q�@��m@î@�dZ@��y@�n�@��@��@��T@���@��@���@�bN@��@�S�@��H@���@�E�@���@�O�@�&�@��`@��@�b@�|�@�o@��H@��!@��\@�n�@�@�X@��@��@�ƨ@�"�@���@�n�@�J@�@���@��@��@�j@���@�+@��H@�~�@�$�@���@�Ĝ@��m@�S�@�@���@�n�@�^5@�E�@�$�@��@�@�7L@��`@��u@�(�@�C�@���@�$�@��-@���@��7@��7@�x�@���@��@�1@��@�S�@���@��@���@�G�@��j@�j@�  @�|�@�+@���@�5?@���@��@�Z@��@��y@���@�~�@�=q@�J@��T@���@�p�@�V@���@���@�j@�9X@��m@��@�t�@�l�@�dZ@�K�@�o@���@�n�@��@��h@�/@���@�z�@�I�@�b@��@���@�;d@�@���@�E�@���@���@��7@�hs@�7L@�&�@�%@���@��@��D@��@��
@�t�@�\)@�C�@��H@���@�E�@��#@���@�G�@�/@���@���@�j@�A�@�(�@��@���@��F@�dZ@�=q@��^@�X@�?}@�/@�V@���@���@��u@�b@��@��@��@�@�ȴ@���@���@�~�@�~�@�V@�@�/@��@���@��`@���@���@��@�1'@��m@��
@�S�@���@�~�@�E�@�$�@���@���@��7@�p�@�O�@�V@���@��j@��u@�1@��P@��@���@��+@�v�@�-@��^@�p�@�&�@��/@��j@���@�r�@�Q�@�A�@�@~ȴ@~v�@}�@}/@|��@|Z@|�@{ƨ@{C�@{@z�\@y��@y�@x�9@xr�@x1'@w��@w|�@v��@u�-@uO�@u/@tZ@s��@sdZ@r�@r^5@q�@q��@q7L@pĜ@pbN@p1'@o�@o��@o�P@o
=@n�@n��@n$�@mp�@mV@lz�@k��@j�@j��@jM�@j�@i��@ix�@iX@g�;@f��@fv�@fff@fV@fE�@f5?@f$�@e�@e�T@e��@e`B@d�D@c33@b�!@b^5@a�@a��@`��@`Q�@`b@_��@_l�@_
=@^�+@^ff@^@]�h@]/@\�@\�@\Z@[�
@[@Z��@Z��@Z�!@ZJ@Y7L@Y7L@Y7L@Y�@X��@XbN@W�w@Wl�@W
=@V�R@V$�@U�-@U/@T��@T9X@S��@Sƨ@S@R�@Q��@Q��@Qx�@Q&�@PĜ@P1'@Pb@O|�@O;d@N��@NV@N$�@N@MO�@MV@L�/@LI�@K�m@K��@K"�@J�@J��@J�!@J�!@J�\@J~�@JM�@Ix�@IX@H��@H��@H�@Hb@G�;@GK�@F�y@F�+@FV@F5?@E��@D��@DI�@D(�@C��@Cƨ@Co@B��@B~�@B-@A�7@A�@@�`@@��@@A�@?��@?�@?�P@?K�@?;d@>ȴ@>5?@=��@=��@=O�@<��@<��@<Z@<1@;��@;ƨ@;��@;33@:��@:��@:=q@:�@9�#@9��@9X@8��@8�u@8Q�@81'@7�@7��@7�P@6�y@6E�@6@5�-@5`B@5�@4��@4j@4I�@41@3��@3"�@2��@2��@2�\@2~�@2M�@2J@1�#@1��@1X@0�u@0bN@0Q�@0bN@0Q�@/�@/��@/�w@/�@/\)@.�@.�+@.ff@.$�@.{@-��@-?}@,�j@,z�@,Z@,I�@,�@+ƨ@+��@+�@+S�@+@*��@*=q@)��@)��@)x�@)&�@(��@(�@(b@'��@'l�@';d@&�@&��@&�+@&5?@&{@%�@%�h@%p�@%`B@%V@$�j@$z�@$I�@$1@#�
@#�
@#�F@#t�@#S�@#"�@"�@"�!@"n�@!��@!�@ �u@�;@�w@�@l�@K�@;d@
=@�@ȴ@�+@V@5?@{@�@�@`B@V@�@�/@��@j@1@�m@�F@S�@"�@@�@��@��@n�@-@�^@��@x�@7L@��@�9@bN@1'@�@��@|�@�@ȴ@��@v�@E�@@�T@�-@`B@V@�@�D@I�@1@�
@��@"�@@�@�!@~�@n�@�@��@�7@hs@X@G�@7L@%@Ĝ@�@b@�@��@|�@l�@K�@�@��@��@�+@V@$�@�@��@�@?}@/@V@�j@Z@9X@��@ƨ@�F@t�@C�@o@
�H@
n�@
�@	�@	�@	�#@	�^@	��@	�7@	�7@	hs@	G�@	&�@��@�`@��@��@Ĝ@��@�@r�@bN@Q�@1'@ �@  @�@�;@��@�w@�@�P@|�@l�@;d@+@��@ȴ@��@�+@v�@v�@ff@E�@�T@@@�-@��@�h@�hG�O�A�S�A�VA�M�A�I�A�O�A�I�A�K�A�Q�A�O�A�M�A�Q�A�O�A�E�A�M�A�O�A�A�A�C�A�7LA�5?A�9XA�A�A�9XA�"�A��A���A��A��A��A��#A���A��
A���A�jA�jA�^A�jA�ĜA�wA��A�A䟾A䗍A䗍A�\A�A�|�A�A�x�A�n�A�n�A�p�A�jA�l�A�l�A�bNA�^5A�bNA�^5A�XA�ZA�ZA�S�A�O�A�VA�S�A�S�A�XA�XA�O�A�O�A�Q�A�M�A�O�A�S�A�O�A�K�A�O�A�O�A�G�A�I�A�K�A�C�A�E�A�I�A�C�A�A�A�G�A�E�A�?}A�A�A�E�A�?}A�=qA�?}A�9XA�9XA�=qA�=qA�9XA�9XA�?}A�;dA�9XA�?}A�=qA�9XA�;dA�?}A�=qA�9XA�?}A�=qA�;dA�?}A�=qA�9XA�;dA�;dA�7LA�;dA�=qA�;dA�;dA�?}A�;dA�;dA�=qA�=qA�9XA�7LA�;dA�7LA�5?A�9XA�=qA�7LA�7LA�9XA�5?A�33A�7LA�9XA�5?A�33A�5?A�9XA�7LA�5?A�5?A�9XA�7LA�5?A�7LA�7LA�5?A�33A�9XA�7LA�5?A�9XA�5?A�1'A�33A�5?A�1'A�1'A�33A�5?A�1'A�1'A�5?A�33A�/A�33A�5?A�33A�33A�7LA�5?A�1'A�/A�-A�"�A��HA㝲A�t�A�5?A��;A⛦A�33AᙚA��
A�dZA�%A���Aߥ�A�G�A��HA�jA�`BA�"�A���A�t�A�(�A��/A���AٍPA�ZA�%A؉7A�r�A�ffA�dZA�bNA�Q�A�-A��A��Aײ-A�Q�A��Aֺ^A�+A�ȴAթ�A���A�|�AҾwA�|�A�ZA�+A��A��Aѡ�A�`BA�A�A�-A�-A�+A��A�`BA�1A���A���A��A�|�A��mẠ�A�x�A�M�A��A��AˬA˅A�\)A�K�A�=qA�(�A�oA�VA�A���A��TA��
A�ȴAʴ9Aʛ�A�~�A�ffA�Q�A�1'A���A���Aɉ7A�ZA�bA���A�l�A���A�9XAƗ�A�XA�bAŸRA�\)A�JA���Aĉ7AđhAħ�A���A���A���Aĥ�A�z�A�;dA��A�ĜAß�A�t�A�E�A��A©�A�S�A� �A��A��wA���A���A��DA�p�A�VA�5?A��A�dZA�A���A��A���A���A���A�%A��A��DA�|�A�z�A�dZA�I�A�bA���A���A�z�A�dZA�Q�A�C�A�33A�(�A�"�A��A�VA�JA��A��A��TA��A�ƨA��uA�dZA�?}A�VA���A�|�A�K�A�oA�K�A��7A�^5A�1'A���A���A�bNA�;dA��A���A��A�Q�A�  A��
A��^A��A�VA��A���A��+A�jA�=qA�-A��A��A��FA���A�~�A�VA�O�A�7LA���A��RA�?}A��wA��A�\)A�/A��A�
=A���A���A��A�ƨA�hsA�+A��A� �A�+A�
=A��yA��A��uA�hsA�M�A��A��RA��;A���A�^5A��A��-A��TA���A�ZA�;dA�1A��#A��RA�x�A�XA�"�A�A��A���A�v�A�&�A���A��A���A��A�^5A��yA��A���A�XA��`A�K�A��A�K�A���A�v�A��A��PA���A���A��A��A�A� �A�$�A�A�A�I�A�`BA�l�A�O�A��#A�`BA���A���A�z�A�p�A�v�A��\A���A��!A��!A���A���A�n�A�Q�A� �A��mA��^A��uA�hsA�-A��
A���A�XA�A���A��-A��DA�jA�VA�A�A�-A��A�
=A��yA��!A��A�VA�9XA��A��
A�ffA�$�A���A��A��A��`A��
A���A�A��FA���A�v�A�hsA� �A��A�33A�M�A��
A���A���A��DA�l�A�VA��wA���A�ZA�5?A��HA���A�A�A��A�"�A�+A�
=A�XA��`A���A��A�r�A�l�A�ffA�ZA�K�A���A���A�x�A�`BA�O�A�C�A�/A��A�|�A�1A��!A�&�A���A�`BA��A���A�{A���A��hA�Q�A���A��wA��DA�bNA�(�A���A���A�p�A�A�A���A��RA�VA���A��hA�p�A�  A��/A��RA���A��A�r�A�ffA�\)A�M�A�=qA�33A�33A�1'A�{A��A���A���A��hA��A�t�A�jA�hsA�dZA�bNA�`BA�^5A�ZA�XA�VA�Q�A�C�A��A���A�ffA�7A}�A{K�Az~�Az$�Ay�-Ay33Axn�AwhsAv��Au�mAu/At�uAs��Asl�ArffAq�hAqO�ApjAox�Ao33AnA�Al�/Al5?AkAkG�Aj�Ajr�AjA�AjAi�#Ai�-Ail�Ai�Ah�!Ah�uAh9XAg��Ag��AgS�Ag�Af�RAf1Ad�`A`E�A_�
A_��A_�wA_�-A_�A^�HA]33AY��AXAW�^AWhsAW33AV��AV��AVv�AVbAT��AR�+AQ�APZAO�AOAN�/AN�RANQ�AM�
AM�AMhsAM7LALZAK��AK�;AK��AK�
AK�^AK��AJ�+AHQ�AGx�AG�AGVAF�HAF��AE��AES�AE7LAEAD-AB�AB5?ABAA�A?�A>v�A>{A=��A=��A=t�A=p�A=dZA=`BA=`BA=`BA=\)A=O�A=G�A=G�A=C�A=;dA=�A<�jA<1'A;��A;dZA9|�A8bNA8�A7�^A7p�A7
=A6�9A6��A6�A6bNA6bA5
=A3�
A2��A3VA3�PA3�#A3�FA3��A3p�A3K�A333A3"�A2�HA2VA1�^A1S�A0�A0��A0{A/�-A/l�A/hsA/dZA/`BA/`BA/O�A/K�A//A/A.�HA.��A.�RA.��A.�\A.�\A.�+G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                     ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B	�@B	�nB	�nB	��B	��B	�B	��B	��B	��B	��B	��B	�tB	�tB	�B	�:B	�4B	��B	�'B	��B	�!B	�!B	��B	��B	��B	��B	�'B	�\B	�VB	�IB	�B
VB
_�B
k�B
h�B
q�B
� B
z�B
��B
��B
�:B
��B
��BB<jBc�Bt�B}�B�VB��B�B�\B�!B��B��BTaB1�B	�B \BQ�B��B��B��B��B|�Bm)BJ�B+B
�B
�B
�B
�_B
hsB
T�B
33B
!�B
IB
�B
�B
�B	��B	��B	�B	�B	kB	Y�B	H�B	49B		lB	�B�B�gB��B�'B�B�yBΥB�0BچB�NB��B��B	\B	�B��B�B��B��B	�B	1�B	GzB	A B	R�B	wfB	�*B	�B	��B	�B	�BB	��B	��B	��B	�?B	��B	�BB	�B	ĜB	�?B	ǮB	��B	�B	�B	�QB	��B	�|B	�oB	�	B
�B

=B
PB
:B
�B
 �B
�B
�B
�B
�B
,B
3�B
7B
7�B
7�B
8�B
7LB
7�B
8�B
5�B
2�B
1�B
1�B
:�B
B[B
A�B
@B
@�B
@B
>�B
7�B
7�B
;0B
>B
?�B
?�B
?}B
AUB
A�B
<�B
6FB
4�B
5�B
:�B
?}B
>wB
9�B
8�B
8�B
8RB
7�B
5tB
5B
4�B
5�B
7B
?B
B'B
EmB
H�B
IB
H�B
H�B
HB
FtB
DgB
CaB
B'B
A�B
@�B
?�B
>�B
<jB
;0B
9$B
6�B
7�B
7�B
:�B
6�B
6zB
6�B
6zB
5�B
5tB
5B
33B
1�B
1�B
2�B
0UB
.}B
1�B
2-B
0�B
0UB
/�B
-B
,�B
,=B
*eB
)�B
&LB
%�B
$tB
#�B
"4B
$@B
%�B
$�B
"hB
 \B
�B
�B
 \B
 �B
 �B
�B
�B
OB
�B
B
�B
xB
CB
�B
	B
B
�B
1B
_B
YB
B
�B
B
@B
FB
FB
�B
uB
B
B
B
�B
�B
�B
hB
�B
B
�B
B
�B
B
oB
oB
�B
�B
hB
4B
�B
�B
�B
(B
�B
�B
\B
�B
�B
(B
�B
(B
"B
VB
�B
�B
�B
�B
�B
�B
JB
�B
�B
B
�B
PB
�B
JB
�B
B
�B
�B
�B
xB
xB
�B
B

�B

�B
�B
B

�B
DB
�B
xB
�B
xB
B
xB

�B
�B

rB
JB
B
JB
�B
�B
JB
~B
~B
JB
~B
�B
�B
~B
JB
B
JB
�B
JB
�B
B
~B
~B
�B
�B
�B
B
~B
B
B
PB
"B
�B
�B
"B
�B
�B
�B
\B
�B
�B
 B
hB
4B
 B
4B
4B
 B
�B
�B
 B
�B
B
B
�B
hB
�B
�B
B
uB
�B
FB
FB
�B
FB
FB
B
{B
�B
�B
�B
�B
B
�B
$B
$B
YB
$B
$B
�B
�B
�B
+B
�B
�B
�B
eB
�B
eB
B
B
�B
=B
qB
	B
7B
kB
kB
7B
kB
�B
	B
xB
�B
�B
�B
�B
�B
IB
~B
�B
B
OB
!B
VB
VB
VB
VB
!B
 'B
 �B
!bB
"4B
"�B
#:B
#�B
#�B
$B
$B
$B
$@B
$�B
$�B
%zB
%zB
&LB
&LB
&LB
&�B
&�B
&�B
&�B
'B
'B
'RB
'�B
($B
($B
($B
($B
(�B
(�B
)*B
)�B
)�B
)�B
)�B
)�B
*eB
*�B
*�B
*eB
*eB
*0B
*�B
*0B
,�B
+�B
*�B
*eB
*�B
*eB
*0B
*0B
*eB
*eB
*0B
+B
*�B
*�B
+6B
*�B
+B
*�B
*eB
*�B
,=B
,B
,B
,=B
,=B
,qB
,�B
,�B
-�B
-�B
-CB
.�B
.�B
/�B
/�B
/�B
0!B
0!B
/�B
/�B
/�B
0�B
0�B
0�B
0UB
1�B
2-B
3�B
3hB
3hB
33B
49B
4�B
5B
5tB
5�B
5tB
5�B
6B
6B
5�B
6�B
6�B
6�B
7LB
8B
7�B
8RB
8RB
9$B
9$B
9XB
9�B
:^B
:�B
:�B
:�B
:�B
;0B
;dB
<jB
=B
<�B
<jB
=�B
=�B
=qB
>B
>wB
>wB
>�B
?B
?}B
?�B
?�B
?�B
?�B
?�B
@OB
?�B
?�B
@OB
@�B
@�B
@�B
AUB
AUB
A B
B'B
B�B
B�B
B[B
A�B
C�B
C-B
CaB
C-B
C-B
C-B
C-B
B�B
C-B
B�B
C-B
B�B
C�B
EB
D�B
D�B
E9B
D�B
FB
E�B
E�B
FB
F?B
F�B
F�B
F�B
GEB
GEB
G�B
GzB
G�B
HB
H�B
IRB
IB
IB
IRB
JXB
J�B
JXB
J#B
JXB
JXB
J�B
K)B
K)B
K)B
K^B
K�B
L0B
L�B
MB
M6B
M6B
MB
NB
N�B
N�B
N�B
N�B
OB
OBB
O�B
OvB
PHB
O�B
QB
P�B
P�B
P�B
Q�B
Q�B
Q�B
Q�B
Q�B
R B
Q�B
Q�B
Q�B
Q�B
Q�B
Q�B
RTB
S&B
T,B
S�B
TaB
TaB
TaB
T�B
TaB
U�B
U2B
UgB
UgB
UgB
U�B
V�B
W
B
W
B
W
B
W?B
WsB
V�B
VmB
V�B
W�B
W�B
W�B
XB
XyB
X�B
YB
YB
YB
Y�B
Z�B
[WB
[WB
[#B
[�B
[�B
[�B
\�B
[�B
[�B
[�B
\)B
\�B
]�B
]dB
^B
^B
]�B
^B
^5B
^�B
^�B
_B
_;B
_B
_;B
_pB
`vB
`�B
`�B
`�B
a|B
a�B
b�B
a�B
a�B
bNB
b�B
b�B
cTB
c B
c B
cTB
cTB
c�B
c�B
c�B
c�B
d�B
d�B
d�B
d�B
e,B
e�B
e�B
e�B
e�B
f2B
f�B
f�B
gB
gB
f�B
gB
h>B
h>B
hsB
h>B
h>B
hsB
h�B
iB
h�B
h�B
iyB
i�B
i�B
jB
jKB
j�B
kB
kQB
k�B
k�B
lWB
lWB
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
l�B
m]B
m�B
m�B
m)B
n/B
n�B
ncB
o5B
p;B
p;B
p�B
p�B
p�B
p�B
o�B
p�B
qB
p�B
poB
poB
qB
qvB
qvB
qB
qAB
qAB
q�B
q�B
r|B
r|B
r|B
sB
s�B
tTB
tB
tTB
tTB
tB
s�B
tB
tB
t�B
t�B
t�B
u%B
uZB
uZB
uZB
u�B
v`B
v�B
v�B
v�B
w�B
wfB
w�B
xB
x8B
x8B
x�B
x�B
y>B
y>B
y�B
y�B
y�B
y�B
y�B
zB
zxB
{JB
zxB
{�B
{�B
|B
|PB
}VB
}VB
}"B
}VB
}�B
~(B
~(B
~(B
~(B
~�B
cB
cB
cB
cB
� B
�4B
�iB
�B
�;B
�;B
�oB
�oB
�oB
�oB
�oB
��B
�B
�AB
�uB
��B
��B
�GB
�GB
�B
��B
�B
��B
��B
��B
�B
��B
��B
�B
��B
��B
��B
�%B
��B
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
�1B
��B
��B
��B
��B
�B
�7B
�lB
�7B
�lB
�lB
�lB
�lB
��B
��B
�	B
��B
�	B
��B
��B
��B
�	B
�=B
�=B
�=B
�rB
�rB
��B
��B
��B
��B
��B
�B
�B
��B	�@B	�nB	��B	�tB	��B	��B	��B	�-B	�B	�B	��B	��B	��B	��B	�4B	�B	��B	�B	��B	��B	��B	��B	��B	��B	�eB	�hB	��B	��B	�:B	��B	�@B	��B	�B	��B	��B	��B	�hB	�tB	�tB	�hB	��B	�zB	��B	�hB	�B	��B	�-B	��B	��B	�B	��B	�@B	��B	��B	�@B	�hB	��B	��B	��B	�-B	��B	�-B	��B	�OB	��B	�bB	��B	��B	��B	�-B	�!B	��B	�\B	�!B	�\B	��B	�VB	�VB	�bB	��B	��B	�\B	�!B	��B	�VB	�'B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�OB	��B	��B	�OB	�'B	�\B	��B	��B	��B	��B	��B	��B	��B	�OB	��B	�\B	��B	��B	��B	��B	��B	�\B	��B	��B	�\B	�\B	��B	��B	��B	��B	��B	��B	��B	�!B	��B	��B	��B	��B	��B	��B	��B	�\B	�-B	��B	�!B	��B	�bB	��B	�VB	��B	��B	��B	�!B	��B	�bB	��B	��B	��B	�bB	�VB	�\B	��B	�VB	��B	�bB	�VB	��B	�'B	��B	��B	�OB	�VB	�!B	�IB	��B	�!B	��B	��B	�IB	�IB	�qB	��B	�B	��B	�B	�-B	��B	�B	��B	�EB	��B	�aB	�HB	��B
�B
%B
	�B
xB
�B
�B
'�B
33B
X�B
l"B
^B
d&B
^�B
b�B
y�B
m)B
gB
qB
qAB
i�B
iyB
f2B
e,B
gmB
k�B
jKB
f�B
h�B
tB
j�B
n�B
~]B
iB
h�B
�=B
��B
�SB
v�B
xlB
z�B
v`B
t�B
�B
x�B
��B
v�B
u�B
tTB
v�B
��B
|�B
�;B
��B
�7B
�$B
�B
��B
��B
��B
��B
��B
�tB
�tB
��B
��B
�:B
��B
��B
��B
�4B
��B
�-B
��B
�-B
��B
�:B
�B
��B
��B
��B
��B
��B
�B
�hB
�hB
��B
��B
�_B
�B
�B
�:B
��B
�B
��B
��B
��B
��B
�zB
��B
�8B�B�B�B{B�B!�B~BxB#B"4B*�B*�B7B6FB8�B=�B8�B7LB8�B6�B8RB:�BFBR�BPB^�B`vBf2Bw�Bs�BjKBL0BT�BX�Be�BiBh
BrBw�Bv`BuZBt�Bt�Br|BtBrGBu%BuZBt�BsMBy�BsMBq�Bs�BxlB�B�uB�uB�B�SB�B��B��B�B��B�SB�kB��B�RB�*B��B��B�wB��B��B�6B�LB�B��B��B�~B�VB��B�FB��B��B�=B��B�"B��B��B��B�kB��B�hB�:B��B��B�_B��B�B��B��B�B�=B�VB��B��B�1B��B��B��B��B��B��B��B�\B�kB��B��B��B��B��B�eB�FB��B�+Bt�BpoBo�Be`Bb�Bc�BY�B[WBO�BO�BL0BVBC�B@OB7�B>�BA�B>�B&�B!�B%�BqB�B�B�B�B�B
��BuB�BDB�BxB~B �B.IB.}B,=B,qB0�B;dBN�Ba|B^5B`Bc�Bh
Bk�B|�B��B�@B�B��B�:B��B�B��B�XB��B��B��B��B��B�B�xB��B�B��B��B��B��B��B��B�MB�GB��B�{B�_B� B� B{�B}"B��B��B~(ByrBt�BtTBtTBsBpoBm�Bm)Bk�BffB^5Bo�B{B\]B`vB3�B)�B)*B$B%zB)�B!-B�B�BBbB
��B �B_B
�]B�B
��B
��B
��B
�XB
��B
�UB
�qB
�B
�*B
�_B
�B
�B
��B
�VB
��B
��B
�+B
��B
�IB
�bB
��B
�=B
�MB
��B
y�B
��B
v�B
r|B
m�B
o B
l�B
d&B
aB
[�B
^�B
_�B
V9B
R�B
R�B
Q�B
PHB
RTB
AUB
5tB
7LB
;�B
(�B
'�B
%FB
$B
#nB
"�B
!�B
 �B
!�B
CB
=B
qB
%B
xB
B
	B
�B
�B
�B
:B
�B
hB
�B
�B
bB
�B
~B

=B
	B
	�B
	lB
	�B
~B
PB
xB
 �B	��B	�B	�B	��B	�jB	��B	ΥB	��B	�XB	ÖB	�?B	�'B	�FB	��B	�RB	�FB	�\B	��B	��B	��B	�PB	�{B	��B	}"B	wfB	sB	r�B	m�B	lWB	l�B	j�B	dZB	a�B	c�B	\�B	\�B	S[B	P�B	Q�B	XEB	U�B	t�B	+6B	*�B	&�B	%B	%FB	2aB	Q�B	L�B	qB	bB	�B	PB	�B	B��B	%B	B	(B�VB	 �B�iB�ZB�mB�B�B�B�QB�B��B�BуB�BϫB��B�6B�EB��B��B�HB�[B��B��B��B�jB��B��B��B� B��BɆB�*B��B�BרB�TB�B��B�B�B�HB��B��B�6B�pB��B�dB��B��B��B��B�&B��B�6B�B	bB�gB�}B��BҽBуB�B��B�)B�BѷB�]B�B�<BɺB�B�rB	�B	�B	�B	�B	JB	
�B	 B	{B	�B	B	�B	YB	
rB	MB��B�VB�VB�B�VB��B��B��B��B�B�B�PB��B��B�lB��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                     B	�ZB	�nB	��B	��B	�nB	�ZB	��B	��B	��B	�zB	�nB	��B	�FB	��B	��B	��B	��B	�\B	��B	�VB	�;B	��B	��B	�B	�B	�BB	��B	��B	�:B	�4B
$&B
oB
qAB
s�B
�MB
��B
��B
��B
��B
�=B
��B
�ZB$&BU�Bq�B{�B��B�cB�]B�OB��B�*B��B��Bd�BIB�B#BW�B��B��B��B��B�B~�BZkB.�B
�*B
��B
��B
�?B
t9B
`�B
8�B
$�B
 �B
�B
�B
�B	��B	�BB	��B	�tB	r|B	l�B	Z�B	F�B	7B	vB��B��B��B͟B�FB�qB��BӏB�B�EBݘB�hB	YB	~B��B��B�jB	 B	sB	5�B	K)B	?�B	N�B	sMB	��B	�B	ѝB	�zB	�}B	�HB	��B	��B	��B	��B	��B	�HB	�KB	�XB	ˬB	�aB	бB	�)B	�"B	��B	�B	�UB	��B
+B
dB
�B
�B
B
#B
�B
�B
�B
�B
,"B
5�B
88B
8lB
9>B
9rB
7fB
8�B
;�B
7LB
4B
2-B
1�B
<B
D�B
C�B
A;B
C�B
A�B
A�B
8lB
7�B
;�B
>�B
@B
@B
?HB
C�B
E�B
@�B
8B
6B
5�B
:xB
@�B
@�B
;B
9�B
9rB
9�B
9XB
6�B
6zB
5�B
6FB
6zB
?}B
B[B
E�B
JrB
J	B
J	B
J�B
I�B
G�B
F%B
DgB
C�B
C�B
B�B
A�B
@OB
=B
<�B
:^B
7�B
8�B
8�B
<�B
72B
7�B
8B
7LB
6�B
7fB
6FB
4�B
2�B
3MB
5B
1�B
/�B
3�B
2�B
1vB
1vB
0�B
-wB
-wB
-�B
-CB
+B
'8B
&�B
%�B
%FB
#B
$�B
'B
%�B
$�B
!�B
�B
�B
 vB
!B
"B
!HB
 'B
!B
 vB
�B
�B
IB
/B
CB
�B
�B
CB
B
1B
�B
�B
�B
�B
�B
�B
�B
aB
aB
�B
{B
uB
�B
FB
�B
TB
�B
�B
uB
uB
�B
uB
@B
FB
{B
�B
:B
�B
�B
 B
�B
�B
4B
B
�B
BB
vB
�B
vB
�B
�B
(B
�B
�B
VB
"B
�B
jB
jB
PB
B
�B
"B
"B
6B
�B
�B
�B
JB
PB
�B
�B
JB
�B
�B
B
"B
�B
^B
�B
JB
~B
B
�B
�B
^B
�B
�B
dB
�B
<B
"B
jB
�B
B
�B
PB
B
�B
jB
�B
�B
�B
~B
�B
PB
�B
�B
6B
�B
PB
B
PB
�B
�B
jB
B
�B
B
VB
�B
VB
"B
pB
<B
VB
�B
.B
}B
B
:B
TB
�B
�B
hB
B
B
oB
:B
:B
�B
�B
�B
�B
oB
@B
FB
B
B
aB
�B
{B
B
�B
aB
�B
�B
�B
�B
�B
�B

B
$B
�B
YB
sB
$B
YB
�B
+B
�B
�B
�B
KB
�B
B
�B
kB
�B
�B
�B
�B
xB
�B
WB
�B
�B
#B
CB
WB
WB
�B
B
�B
dB
IB
�B
�B
�B
B
�B
�B
�B
pB
pB
pB
�B
�B
 �B
!|B
"hB
"�B
#TB
#�B
$ZB
$B
$tB
$ZB
$�B
%B
%`B
%�B
&2B
&LB
&�B
&�B
&�B
'B
&�B
'B
'RB
'mB
'mB
($B
(>B
(�B
(XB
(sB
(�B
)�B
)_B
)�B
*B
*0B
*0B
*B
*�B
*�B
*�B
*�B
*�B
*�B
*�B
+kB
,WB
-�B
,WB
+B
*�B
*�B
*�B
*B
*�B
+QB
*�B
+B
+�B
+B
+6B
+kB
*�B
+6B
*�B
*�B
+�B
-CB
,=B
,=B
,qB
,qB
,�B
,�B
-CB
./B
-�B
.IB
/�B
/OB
0!B
0B
0UB
0�B
0UB
0!B
0;B
0oB
1AB
0�B
0�B
1[B
2�B
3MB
49B
3�B
3�B
3�B
5B
5?B
5�B
5�B
5�B
5�B
6FB
6`B
6FB
6zB
8B
7LB
7fB
8B
8�B
88B
8�B
8�B
9�B
9rB
9�B
:xB
:�B
:�B
;B
;B
;0B
;�B
<PB
=<B
=qB
=B
=<B
>]B
=�B
=�B
>�B
>�B
>�B
?B
?}B
?�B
?�B
@ B
@4B
@ B
@iB
@�B
@4B
@iB
AB
A B
AUB
A�B
BAB
A�B
A�B
B[B
B�B
B�B
B�B
CaB
D�B
CaB
C{B
CGB
CGB
CGB
CGB
C-B
CGB
CGB
C{B
C�B
D�B
E�B
D�B
EB
E�B
E�B
F�B
E�B
F%B
FtB
F�B
G+B
F�B
GB
G�B
G�B
G�B
G�B
H1B
H�B
IRB
I�B
I7B
I7B
J	B
KB
J�B
JXB
JXB
J�B
J�B
K^B
KxB
K�B
KxB
K�B
L~B
L�B
M6B
MjB
M�B
M�B
M�B
N�B
OBB
N�B
OB
O(B
OvB
O�B
O�B
PB
P�B
P}B
QhB
QB
P�B
QhB
R:B
Q�B
R:B
RTB
R B
R�B
Q�B
Q�B
Q�B
Q�B
Q�B
RB
R�B
S�B
TaB
TaB
T�B
T�B
T�B
U2B
T�B
VB
U�B
U�B
U�B
U�B
VmB
WsB
W?B
W?B
W?B
W�B
W�B
V�B
V�B
WsB
XB
XB
X+B
XyB
X�B
YB
YKB
YKB
Y�B
ZkB
[WB
[�B
[�B
[qB
\B
[�B
\]B
\�B
\B
\)B
\)B
\�B
]dB
^B
]�B
^B
^OB
^B
^OB
^�B
_!B
_B
_;B
_pB
_;B
_�B
`B
aB
`�B
`�B
`�B
a�B
b4B
b�B
a�B
a�B
b�B
b�B
cTB
cnB
c:B
c:B
c�B
c�B
c�B
c�B
c�B
d�B
e,B
d�B
d�B
d�B
e�B
fB
fB
e�B
fB
f�B
gB
gB
g8B
gB
gB
g�B
h�B
h�B
h�B
hXB
hsB
h�B
h�B
i*B
h�B
i*B
i�B
jB
j0B
jKB
j�B
kB
kkB
k�B
k�B
lWB
l�B
l�B
m]B
m)B
l�B
mCB
mB
l�B
m)B
mB
mB
m�B
m�B
m�B
m]B
ncB
o B
n}B
oOB
poB
poB
qB
qB
p�B
p�B
p�B
q[B
q�B
q[B
p�B
p�B
q[B
q�B
q�B
qAB
qvB
q[B
q�B
rB
r�B
r�B
r�B
s3B
tnB
t�B
t9B
tnB
t�B
tTB
tB
t9B
tTB
t�B
t�B
uB
u?B
utB
u�B
u�B
vB
v�B
v�B
v�B
wB
w�B
w�B
xB
x8B
xlB
xlB
y$B
y	B
y�B
yrB
y�B
y�B
y�B
y�B
y�B
z^B
z�B
{B
z�B
|6B
|6B
|PB
|�B
}�B
}qB
}<B
}�B
}�B
~BB
~wB
~wB
~wB
B
}B
}B
}B
�B
�OB
��B
��B
�oB
�UB
�UB
��B
��B
��B
��B
��B
��B
�AB
�uB
��B
��B
�-B
�{B
�aB
�GB
�-B
�{B
��B
��B
�B
�3B
��B
�B
�SB
��B
��B
�B
�YB
��B
��B
��B
��B
�B
��B
�B
�B
�B
�_B
�zB
��B
��B
�B
�KB
��B
��B
��B
��B
�B
�RB
��B
�RB
��B
��B
��B
��B
��B
��B
�#B
�	B
�#B
�	B
��B
��B
�#B
�XB
�=B
�XB
��B
��B
��B
��B
��B
��B
��B
�B
�G�O�B	�@B	�nB	��B	�tB	��B	��B	��B	�-B	�B	�B	��B	��B	��B	��B	�4B	�B	��B	�B	��B	��B	��B	��B	��B	��B	�eB	�hB	��B	��B	�:B	��B	�@B	��B	�B	��B	��B	��B	�hB	�tB	�tB	�hB	��B	�zB	��B	�hB	�B	��B	�-B	��B	��B	�B	��B	�@B	��B	��B	�@B	�hB	��B	��B	��B	�-B	��B	�-B	��B	�OB	��B	�bB	��B	��B	��B	�-B	�!B	��B	�\B	�!B	�\B	��B	�VB	�VB	�bB	��B	��B	�\B	�!B	��B	�VB	�'B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	��B	�OB	��B	��B	�OB	�'B	�\B	��B	��B	��B	��B	��B	��B	��B	�OB	��B	�\B	��B	��B	��B	��B	��B	�\B	��B	��B	�\B	�\B	��B	��B	��B	��B	��B	��B	��B	�!B	��B	��B	��B	��B	��B	��B	��B	�\B	�-B	��B	�!B	��B	�bB	��B	�VB	��B	��B	��B	�!B	��B	�bB	��B	��B	��B	�bB	�VB	�\B	��B	�VB	��B	�bB	�VB	��B	�'B	��B	��B	�OB	�VB	�!B	�IB	��B	�!B	��B	��B	�IB	�IB	�qB	��B	�B	��B	�B	�-B	��B	�B	��B	�EB	��B	�aB	�HB	��B
�B
%B
	�B
xB
�B
�B
'�B
33B
X�B
l"B
^B
d&B
^�B
b�B
y�B
m)B
gB
qB
qAB
i�B
iyB
f2B
e,B
gmB
k�B
jKB
f�B
h�B
tB
j�B
n�B
~]B
iB
h�B
�=B
��B
�SB
v�B
xlB
z�B
v`B
t�B
�B
x�B
��B
v�B
u�B
tTB
v�B
��B
|�B
�;B
��B
�7B
�$B
�B
��B
��B
��B
��B
��B
�tB
�tB
��B
��B
�:B
��B
��B
��B
�4B
��B
�-B
��B
�-B
��B
�:B
�B
��B
��B
��B
��B
��B
�B
�hB
�hB
��B
��B
�_B
�B
�B
�:B
��B
�B
��B
��B
��B
��B
�zB
��B
�8B�B�B�B{B�B!�B~BxB#B"4B*�B*�B7B6FB8�B=�B8�B7LB8�B6�B8RB:�BFBR�BPB^�B`vBf2Bw�Bs�BjKBL0BT�BX�Be�BiBh
BrBw�Bv`BuZBt�Bt�Br|BtBrGBu%BuZBt�BsMBy�BsMBq�Bs�BxlB�B�uB�uB�B�SB�B��B��B�B��B�SB�kB��B�RB�*B��B��B�wB��B��B�6B�LB�B��B��B�~B�VB��B�FB��B��B�=B��B�"B��B��B��B�kB��B�hB�:B��B��B�_B��B�B��B��B�B�=B�VB��B��B�1B��B��B��B��B��B��B��B�\B�kB��B��B��B��B��B�eB�FB��B�+Bt�BpoBo�Be`Bb�Bc�BY�B[WBO�BO�BL0BVBC�B@OB7�B>�BA�B>�B&�B!�B%�BqB�B�B�B�B�B
��BuB�BDB�BxB~B �B.IB.}B,=B,qB0�B;dBN�Ba|B^5B`Bc�Bh
Bk�B|�B��B�@B�B��B�:B��B�B��B�XB��B��B��B��B��B�B�xB��B�B��B��B��B��B��B��B�MB�GB��B�{B�_B� B� B{�B}"B��B��B~(ByrBt�BtTBtTBsBpoBm�Bm)Bk�BffB^5Bo�B{B\]B`vB3�B)�B)*B$B%zB)�B!-B�B�BBbB
��B �B_B
�]B�B
��B
��B
��B
�XB
��B
�UB
�qB
�B
�*B
�_B
�B
�B
��B
�VB
��B
��B
�+B
��B
�IB
�bB
��B
�=B
�MB
��B
y�B
��B
v�B
r|B
m�B
o B
l�B
d&B
aB
[�B
^�B
_�B
V9B
R�B
R�B
Q�B
PHB
RTB
AUB
5tB
7LB
;�B
(�B
'�B
%FB
$B
#nB
"�B
!�B
 �B
!�B
CB
=B
qB
%B
xB
B
	B
�B
�B
�B
:B
�B
hB
�B
�B
bB
�B
~B

=B
	B
	�B
	lB
	�B
~B
PB
xB
 �B	��B	�B	�B	��B	�jB	��B	ΥB	��B	�XB	ÖB	�?B	�'B	�FB	��B	�RB	�FB	�\B	��B	��B	��B	�PB	�{B	��B	}"B	wfB	sB	r�B	m�B	lWB	l�B	j�B	dZB	a�B	c�B	\�B	\�B	S[B	P�B	Q�B	XEB	U�B	t�B	+6B	*�B	&�B	%B	%FB	2aB	Q�B	L�B	qB	bB	�B	PB	�B	B��B	%B	B	(B�VB	 �B�iB�ZB�mB�B�B�B�QB�B��B�BуB�BϫB��B�6B�EB��B��B�HB�[B��B��B��B�jB��B��B��B� B��BɆB�*B��B�BרB�TB�B��B�B�B�HB��B��B�6B�pB��B�dB��B��B��B��B�&B��B�6B�B	bB�gB�}B��BҽBуB�B��B�)B�BѷB�]B�B�<BɺB�B�rB	�B	�B	�B	�B	JB	
�B	 B	{B	�B	B	�B	YB	
rB	MB��B�VB�VB�B�VB��B��B��B��B�B�B�PB��B��B�lB��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                     <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<�9�<�a�<{�<#�
<=��<��.<#�
<#�
<��<#�
<#�
<�n<Uf�<xp<�T<c�D<#�
<��.<xp<2�<M|�<#�
<*��<�<k�~<�o�<���<#�
<#�
<#�
<#�
<,��<#�
<#�
<#�
<�F�<~֟<���<�P<#�
<���<�t�<E'D<N�w<#�
<#�
<#�
<#�
<F��<�Nh<�t�<p�<^�x<#�
<#�
<��p<�'�<��<���<A�<#�
<#�
<E'D<@kk<{JI<#�
<#�
<#�
<[Y�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2018102901354620181029013546IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019010620010220190106200102QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019010620010220190106200102QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2019052107551120190521075511IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142020230426191420IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                