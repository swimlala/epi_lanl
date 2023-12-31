CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2019-12-10T01:00:39Z creation; 2023-04-26T19:14:29Z DMQC;      
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
_FillValue                    �0Argo profile    3.1 1.2 19500101000000  20191210010039  20230426191429  5905275 5905275 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               F   FAA  AOAO7316_008644_070                 7316_008644_070                 2C  2C  DD  SOLO_II                         SOLO_II                         8644                            8644                            V2.5; SBE602 11Jan18            V2.5; SBE602 11Jan18            853 853 @��;���@��;���11  @��;���@��;���@*C��҉@*C��҉�dH@�r�dH@�r11  GPS     GPS     BA  BA  BA  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@�@G�@��\@�  @�G�@�  A   A  A\)A+�A@��A`��A�  A�Q�A�Q�A�  A��AϮA�\)A�\)A��B  B�
B�
B   B'�
B0  B8(�B@(�BH(�BP  BW�
B_�
Bg�
Bp  Bw�
B�B��B��B�  B�(�B�=qB�  B�  B��B��B��B�{B�{B�  B�  B�  B�  B�  B�  B�  B�  B�(�B�  B�{B�  B��B�{B�{B�  B�  B�  B��C   C  C��C  C  C
  C  C  C
=C
=C  C  C  C
=C
=C  C 
=C"  C$  C&
=C(  C)��C+�C-�C/�C1��C3�C5�C8  C:  C;��C=��C@  CB
=CD  CE��CH  CJ
=CL{CN
=CP
=CR  CS��CV
=CX{CZ
=C\  C^  C`  Ca��Cd  Cf  Ch  Ci��Ck��Cn  Cp  Cq��Cs��Cu��Cw��Cy��C|  C~
=C�C�C�  C���C�  C�  C���C���C���C�C�  C�  C�C���C���C���C���C�C�C�C�C���C���C�  C�  C�C�C�C���C���C�  C�  C�C�  C���C���C���C�  C���C�  C�
=C�C�C�C�C�C�C�C�C�  C���C�  C�  C�  C�  C�C�C�C�
=C�\C�
=C�C�  C���C�  C�  C�  C�C�
=C�C���C�  C�
=C�\C�C�C�  C���C�  C�C�C�
=C�
=C�C���C�  C�  C���C�  C�
=C�  C���C�  C�C�C�
=C�C���C�  C�C�  C���C�  C�  C�  C�
=C�C�  C�C���C���C�  C���C���C�  C�C�  C���C�  C�
=C�C�  C�
=C�C�C�  C���C�  D   D }qD ��D� D  D� D�qDz�D  D}qD�qD��D�D� D�D��D�D��D	  D	}qD
�D
}qD
�qD��D�D��D  D}qD��Dz�D��D}qD�qD� D  D��DD� D�RDz�D�qD� D�D�D�D}qD  D� D�D��D  D}qD�D��D  D� D�qD}qD��D��D�D��D�D� D �D � D �qD!z�D"  D"� D"�qD#}qD#�qD$}qD$��D%��D&D&�D&�qD'z�D(  D(}qD(�qD)z�D)��D*� D+�D+��D,�D,z�D,�qD-}qD-�qD.� D/  D/� D0  D0}qD0�qD1}qD1�qD2}qD2�qD3� D4�D4��D5  D5� D5�qD6� D7  D7� D8  D8}qD8�qD9}qD:  D:��D;�D;��D<  D<��D=D=��D>  D>��D?D?��D?�qD@z�D@�qDA� DB  DB}qDB�qDC�DD�DD��DE�DE}qDE�qDF}qDG  DG��DHDH��DH�qDI� DJ�DJ� DK  DK� DK�qDL}qDL��DM� DN  DN� DO�DO� DP  DP��DQ�DQ��DR�DR��DS  DS}qDT  DT� DU  DU� DV  DV}qDW  DW��DX  DX� DY  DY� DZ�DZ��D[�D[� D[��D\}qD]  D]��D^  D^z�D_  D_��D`  D`� D`�qDa}qDa��Db}qDc  Dc��DdDd� De  De� Df  Df� Dg�Dg��Dh  Dh� Di  Di}qDj  Dj��Dk  Dk��Dl  Dl� Dl�qDm}qDn  Dn� Dn�qDo� Dp  Dp� Dp�qDq}qDq�qDr}qDs  Ds}qDs�qDt� Du  Du��Du�qDv� Dw�Dw� Dx  Dx��Dy�Dy� Dy�qDz� D{D{� D|  D|}qD|�qD}� D~  D~� D�D� D�qD�AHD�� D�� D�  D�@ D�� D��HD��D�@ D�~�D�� D�  D�>�D�~�D�� D���D�>�D��HD�D�HD�AHD��HD�� D��qD�>�D�� D�� D�HD�@ D�� D��HD�  D�@ D�~�D���D�  D�>�D�~�D�� D�  D�@ D��HD�� D���D�AHD���D��HD�  D�@ D�~�D��HD��D�AHD��HD�� D�  D�AHD��HD��HD�  D�@ D�� D�� D�  D�>�D�� D�� D���D�AHD��HD��HD�  D�AHD��HD�� D��qD�@ D�� D���D�  D�>�D�}qD�� D�HD�B�D��HD���D�  D�AHD�� D���D�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�=qD�~�D�� D���D�AHD���D��HD�HD�AHD��HD��HD�HD�AHD��HD�D�HD�>�D�� D��HD�  D�>�D��HD�� D���D�AHD��HD�� D�  D�@ D��HD��HD�HD�@ D�� D�� D�HD�AHD�~�D��qD��qD�>�D�� D��HD���D�@ D�� D���D�HD�B�D��HD�� D���D�=qD�|)D���D���D�@ D��HD���D��qD�@ D��HD��HD�  D�@ D�� D�� D�HD�AHD��HD��HD�  D�AHD��HD��HD�HD�AHD��HD�� D��qD�>�D�~�D���D���D�>�D�~�D�� D�HD�AHD�� D��qD���D�@ D�� D��HD�  D�=qD�~�D���D���D�>�D�� D��HD�  D�>�D��HD���D���D�@ D���D��HD�  D�@ D�~�D��HD�  D�@ D��HD�D��D�@ D��HD��HD�  D�AHD�� D�� D�  D�@ D��HD���D��qD�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�� D�� D�  D�AHD�� D�� D�HD�=qD�}qD��HD�HD�>�D�~�D��HD�HD�AHD���D�� D�  D�@ D���D�� D��qD�@ DHD¾�D��qD�@ DÀ D�� D��qD�>�DĀ D�� D�HD�@ D�~�D��HD��D�@ Dƀ Dƾ�D��qD�>�DǁHD��HD�HD�@ DȁHD�D�  D�>�D�~�DɽqD���D�B�DʁHD��HD���D�AHD˂�D�D�  D�=qD�~�D�� D��D�AHD̀ D�� D���D�@ D΁HD��HD��D�B�Dπ DϾ�D��D�AHDЁHD�� D�HD�B�DсHD�� D�  D�>�DҀ D�� D�HD�B�DӁHD�� D�  D�AHDԀ D�� D���D�@ DՁHD�� D�  D�>�Dր D־�D�  D�AHDׁHD�� D�HD�>�D�}qDؾ�D���D�@ D�~�Dپ�D���D�@ D�~�Dھ�D�HD�B�DہHD�D�  D�@ D܀ D�� D�HD�@ D�~�Dݾ�D�  D�@ D�~�D�� D�HD�AHD߁HD�� D�  D�>�D�� D��HD���D�>�D�HD��HD�HD�B�D�HD�� D�  D�@ D� D��HD�HD�@ D� D�� D�HD�@ D�~�D�� D�HD�@ D� D�� D�  D�AHD�HD��HD�  D�>�D�~�D��HD�  D�@ D�~�D龸D�  D�@ D� D꾸D��qD�@ D낏D�D�  D�@ D�HD��HD�  D�>�D�~�D���D��qD�>�D�~�D�� D�  D�>�D�~�D�� D�HD�AHD��HD�� D�HD�AHD�HD��HD�  D�@ D�HD��HD�  D�@ D�HD�� D���D�@ D� D�� D���D�@ D���D��HD�  D�@ D�~�D��qD�HD�@ D�� D���D�  D�AHD�� D���D���D�@ D��HD�� D�  D�N>u>�?L��?��
?�(�@�@�R@:�H@Tz�@p��@��@���@�  @�\)@�(�@�ff@�z�@��
@�33@��RAffA{AA(�A"�\A)��A1G�A7�A>{AE�AL��AS33AY��A`��Ag�An{As�
Ay��A�Q�A�33A�A�Q�A�33A�A��A��A�z�A�ffA�Q�A�=qA�z�A�{A�  A�=qA�z�A�ffA��A�=qA�(�A�{A�  A�=qA�z�A�ffA�  A��A��
A�ffA�Q�A��A��
A�ffA�Q�A��A��
A�ffA�Q�A��A��
A�{A�Q�Aٙ�A��
A�{A�Q�A��A��
A�ffA�Q�A��A��
A�{A�  A�=qA��
A�A�  A�=qA��
A�A��B ��B�B�RB�B��B�B�HB�B��B	B
�HB�B��BB�RB�Bz�Bp�B�RB�Bz�Bp�BffB\)Bz�Bp�B=qB33Bz�BG�B=qB
=B (�B!G�B"{B"�HB$  B%�B&=qB'
=B(  B)�B*=qB+33B,(�B,��B.{B/\)B0Q�B1G�B2{B333B4z�B5p�B6ffB733B8z�B9p�B:ffB;33B<(�B=p�B>=qB?33B@  BA�BB=qBB�HBC�BD��BEBFffBG\)BHz�BIG�BJ=qBK
=BL  BM�BN{BO
=BO�
BP��BR{BS\)BTQ�BU�BV{BW33BXQ�BYp�BZffB[\)B\Q�B]p�B^�\B_�B`��Bap�Bb�\Bc�Bd��Be�Bf�HBg�
Bh��BiBj�HBl(�BmG�Bn=qBo33BpQ�Bqp�Br�HBt  BuG�Bv�\Bw�Bx��By�B{33B|Q�B}��B33B�=qB��HB�p�B�{B��RB�\)B�  B��RB�p�B�{B���B�p�B�(�B��RB�p�B�{B���B�p�B�(�B���B���B�=qB��HB��B�(�B���B�\)B��B�ffB���B�G�B���B�  B�=qB�z�B���B�
=B�G�B���B�B�  B�=qB�z�B��RB���B�G�B��B�B�  B�=qB�z�B���B��HB�
=B�G�B�p�B��B��
B�{B�=qB�z�B���B��HB�
=B�G�B�p�B��B��
B�  B�(�B�ffB���B���B���B�33B�p�B���B��
B�{B�=qB�ffB���B��HB�
=B�G�B��B�B�  B�=qB�z�B��RB���B�33B��B�B�  B�=qB�z�B��RB���B�33B�p�B��B��B�=qB�z�B��RB�
=B�G�B��B��
B�(�B�z�B��RB�
=B�\)B��B�{B�Q�B���B�
=B�\)B��B�  B�Q�B���B���B�G�B���B��B�Q�B���B���B�\)B��B�  B�ffB��RB��B�p�B�B�(�B�z�B��HB�33B��B��B�=qB��\B�
=B�\)B��B�{B�ffB���B�33B��B��
B�=qB���B�
=B�p�B�B�(�B�z�B���B�33B��B��B�=qB���B���B�\)B�B�(�B��\B���B�\)B�B�{B�z�B���B�33B��B��B�Q�B��RB��B���B�  B�ffB��HB�G�BîB�{B�z�B��HB�G�BŮB�  B�ffB���B�33BǮB�{Bȏ\B�
=BɅB�  B�ffB��HB�G�B��
B�=qB̸RB�33B͙�B�{B�ffB��HB�\)B��
B�Q�B���B�G�B�B�Q�B���B�G�BӮB�(�Bԏ\B�
=BՅB�  B�z�B���BׅB�  B؏\B��Bٙ�B�(�Bڏ\B�
=BۅB��B�z�B���B݅B�  Bޏ\B�
=Bߙ�B�{B��\B�
=BᙚB�{B�\B�
=B�p�B��B�ffB��HB�\)B��
B�Q�B���B�\)B��B�ffB��HB�G�B��
B�Q�B�RB�33B뙚B�(�B��B��B홚B�(�B��B��BB�{B��\B�
=B�B��B�z�B��HB�p�B��B�ffB���B��B�{B���B��B���B�  B��\B�
=B���B�(�B��RB�33B�B�Q�B��RB�G�B�B�=qB��RB�G�B��C 33C z�C �RC ��C=qCz�CC{C\)C��C�C�CffC��C�C33C�C��C{C\)C��C�HC�CffC��C��C=qC�C��C�C\)C�C�C	(�C	p�C	�C
  C
G�C
��C
�HC(�CffC�C�HC33Cz�CC
=CQ�C�\C�
C�CffC�C  C=qC�CC
=CQ�C��C�C(�Cp�C��C��C=qC�\C�
C�C\)C��C�HC33C�C��C{CQ�C��C�C=qC�CC
=C\)C�C��C33Cz�C�
C�CffC�C�C=qC�\C�HC(�Cp�C�RC
=C\)C��C�HC(�Cp�CC{C\)C��C�HC33C�C�
C �C \)C �C ��C!Q�C!��C!�HC"�C"p�C"C#{C#\)C#��C#�HC$=qC$�C$��C%{C%Q�C%�C%��C&=qC&�C&��C'{C'p�C'C(
=C(G�C(��C(�C)G�C)�C)��C*{C*ffC*�RC+
=C+G�C+��C+��C,=qC,�C,��C-�C-p�C-��C.{C.\)C.�C/  C/Q�C/��C/�C0=qC0��C0�HC1(�C1p�C1��C2�C2ffC2�C3
=C3ffC3�RC3��C4G�C4��C5  C5Q�C5��C5�HC6=qC6��C6�C7=qC7z�C7��C8�C8z�C8�
C9�C9p�C9�RC:
=C:ffC:�RC;
=C;\)C;��C;�C<=qC<��C<�HC=(�C=p�C=�
C>(�C>z�C>�RC?
=C?ffC?C@
=C@Q�C@��C@��CAG�CA��CA�
CB33CB�CB�
CC�CCffCCCD�CDffCD�CE
=CE\)CE�CE��CFG�CF��CF�CG33CG�CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                            ?��@�@G�@��\@�  @�G�@�  A   A  A\)A+�A@��A`��A�  A�Q�A�Q�A�  A��AϮA�\)A�\)A��B  B�
B�
B   B'�
B0  B8(�B@(�BH(�BP  BW�
B_�
Bg�
Bp  Bw�
B�B��B��B�  B�(�B�=qB�  B�  B��B��B��B�{B�{B�  B�  B�  B�  B�  B�  B�  B�  B�(�B�  B�{B�  B��B�{B�{B�  B�  B�  B��C   C  C��C  C  C
  C  C  C
=C
=C  C  C  C
=C
=C  C 
=C"  C$  C&
=C(  C)��C+�C-�C/�C1��C3�C5�C8  C:  C;��C=��C@  CB
=CD  CE��CH  CJ
=CL{CN
=CP
=CR  CS��CV
=CX{CZ
=C\  C^  C`  Ca��Cd  Cf  Ch  Ci��Ck��Cn  Cp  Cq��Cs��Cu��Cw��Cy��C|  C~
=C�C�C�  C���C�  C�  C���C���C���C�C�  C�  C�C���C���C���C���C�C�C�C�C���C���C�  C�  C�C�C�C���C���C�  C�  C�C�  C���C���C���C�  C���C�  C�
=C�C�C�C�C�C�C�C�C�  C���C�  C�  C�  C�  C�C�C�C�
=C�\C�
=C�C�  C���C�  C�  C�  C�C�
=C�C���C�  C�
=C�\C�C�C�  C���C�  C�C�C�
=C�
=C�C���C�  C�  C���C�  C�
=C�  C���C�  C�C�C�
=C�C���C�  C�C�  C���C�  C�  C�  C�
=C�C�  C�C���C���C�  C���C���C�  C�C�  C���C�  C�
=C�C�  C�
=C�C�C�  C���C�  D   D }qD ��D� D  D� D�qDz�D  D}qD�qD��D�D� D�D��D�D��D	  D	}qD
�D
}qD
�qD��D�D��D  D}qD��Dz�D��D}qD�qD� D  D��DD� D�RDz�D�qD� D�D�D�D}qD  D� D�D��D  D}qD�D��D  D� D�qD}qD��D��D�D��D�D� D �D � D �qD!z�D"  D"� D"�qD#}qD#�qD$}qD$��D%��D&D&�D&�qD'z�D(  D(}qD(�qD)z�D)��D*� D+�D+��D,�D,z�D,�qD-}qD-�qD.� D/  D/� D0  D0}qD0�qD1}qD1�qD2}qD2�qD3� D4�D4��D5  D5� D5�qD6� D7  D7� D8  D8}qD8�qD9}qD:  D:��D;�D;��D<  D<��D=D=��D>  D>��D?D?��D?�qD@z�D@�qDA� DB  DB}qDB�qDC�DD�DD��DE�DE}qDE�qDF}qDG  DG��DHDH��DH�qDI� DJ�DJ� DK  DK� DK�qDL}qDL��DM� DN  DN� DO�DO� DP  DP��DQ�DQ��DR�DR��DS  DS}qDT  DT� DU  DU� DV  DV}qDW  DW��DX  DX� DY  DY� DZ�DZ��D[�D[� D[��D\}qD]  D]��D^  D^z�D_  D_��D`  D`� D`�qDa}qDa��Db}qDc  Dc��DdDd� De  De� Df  Df� Dg�Dg��Dh  Dh� Di  Di}qDj  Dj��Dk  Dk��Dl  Dl� Dl�qDm}qDn  Dn� Dn�qDo� Dp  Dp� Dp�qDq}qDq�qDr}qDs  Ds}qDs�qDt� Du  Du��Du�qDv� Dw�Dw� Dx  Dx��Dy�Dy� Dy�qDz� D{D{� D|  D|}qD|�qD}� D~  D~� D�D� D�qD�AHD�� D�� D�  D�@ D�� D��HD��D�@ D�~�D�� D�  D�>�D�~�D�� D���D�>�D��HD�D�HD�AHD��HD�� D��qD�>�D�� D�� D�HD�@ D�� D��HD�  D�@ D�~�D���D�  D�>�D�~�D�� D�  D�@ D��HD�� D���D�AHD���D��HD�  D�@ D�~�D��HD��D�AHD��HD�� D�  D�AHD��HD��HD�  D�@ D�� D�� D�  D�>�D�� D�� D���D�AHD��HD��HD�  D�AHD��HD�� D��qD�@ D�� D���D�  D�>�D�}qD�� D�HD�B�D��HD���D�  D�AHD�� D���D�HD�AHD�� D�� D�  D�@ D�� D�� D�  D�=qD�~�D�� D���D�AHD���D��HD�HD�AHD��HD��HD�HD�AHD��HD�D�HD�>�D�� D��HD�  D�>�D��HD�� D���D�AHD��HD�� D�  D�@ D��HD��HD�HD�@ D�� D�� D�HD�AHD�~�D��qD��qD�>�D�� D��HD���D�@ D�� D���D�HD�B�D��HD�� D���D�=qD�|)D���D���D�@ D��HD���D��qD�@ D��HD��HD�  D�@ D�� D�� D�HD�AHD��HD��HD�  D�AHD��HD��HD�HD�AHD��HD�� D��qD�>�D�~�D���D���D�>�D�~�D�� D�HD�AHD�� D��qD���D�@ D�� D��HD�  D�=qD�~�D���D���D�>�D�� D��HD�  D�>�D��HD���D���D�@ D���D��HD�  D�@ D�~�D��HD�  D�@ D��HD�D��D�@ D��HD��HD�  D�AHD�� D�� D�  D�@ D��HD���D��qD�@ D�� D�� D�  D�@ D�� D�� D�  D�AHD�� D�� D�  D�AHD�� D�� D�HD�=qD�}qD��HD�HD�>�D�~�D��HD�HD�AHD���D�� D�  D�@ D���D�� D��qD�@ DHD¾�D��qD�@ DÀ D�� D��qD�>�DĀ D�� D�HD�@ D�~�D��HD��D�@ Dƀ Dƾ�D��qD�>�DǁHD��HD�HD�@ DȁHD�D�  D�>�D�~�DɽqD���D�B�DʁHD��HD���D�AHD˂�D�D�  D�=qD�~�D�� D��D�AHD̀ D�� D���D�@ D΁HD��HD��D�B�Dπ DϾ�D��D�AHDЁHD�� D�HD�B�DсHD�� D�  D�>�DҀ D�� D�HD�B�DӁHD�� D�  D�AHDԀ D�� D���D�@ DՁHD�� D�  D�>�Dր D־�D�  D�AHDׁHD�� D�HD�>�D�}qDؾ�D���D�@ D�~�Dپ�D���D�@ D�~�Dھ�D�HD�B�DہHD�D�  D�@ D܀ D�� D�HD�@ D�~�Dݾ�D�  D�@ D�~�D�� D�HD�AHD߁HD�� D�  D�>�D�� D��HD���D�>�D�HD��HD�HD�B�D�HD�� D�  D�@ D� D��HD�HD�@ D� D�� D�HD�@ D�~�D�� D�HD�@ D� D�� D�  D�AHD�HD��HD�  D�>�D�~�D��HD�  D�@ D�~�D龸D�  D�@ D� D꾸D��qD�@ D낏D�D�  D�@ D�HD��HD�  D�>�D�~�D���D��qD�>�D�~�D�� D�  D�>�D�~�D�� D�HD�AHD��HD�� D�HD�AHD�HD��HD�  D�@ D�HD��HD�  D�@ D�HD�� D���D�@ D� D�� D���D�@ D���D��HD�  D�@ D�~�D��qD�HD�@ D�� D���D�  D�AHD�� D���D���D�@ D��HD�� D�  G�O�>u>�?L��?��
?�(�@�@�R@:�H@Tz�@p��@��@���@�  @�\)@�(�@�ff@�z�@��
@�33@��RAffA{AA(�A"�\A)��A1G�A7�A>{AE�AL��AS33AY��A`��Ag�An{As�
Ay��A�Q�A�33A�A�Q�A�33A�A��A��A�z�A�ffA�Q�A�=qA�z�A�{A�  A�=qA�z�A�ffA��A�=qA�(�A�{A�  A�=qA�z�A�ffA�  A��A��
A�ffA�Q�A��A��
A�ffA�Q�A��A��
A�ffA�Q�A��A��
A�{A�Q�Aٙ�A��
A�{A�Q�A��A��
A�ffA�Q�A��A��
A�{A�  A�=qA��
A�A�  A�=qA��
A�A��B ��B�B�RB�B��B�B�HB�B��B	B
�HB�B��BB�RB�Bz�Bp�B�RB�Bz�Bp�BffB\)Bz�Bp�B=qB33Bz�BG�B=qB
=B (�B!G�B"{B"�HB$  B%�B&=qB'
=B(  B)�B*=qB+33B,(�B,��B.{B/\)B0Q�B1G�B2{B333B4z�B5p�B6ffB733B8z�B9p�B:ffB;33B<(�B=p�B>=qB?33B@  BA�BB=qBB�HBC�BD��BEBFffBG\)BHz�BIG�BJ=qBK
=BL  BM�BN{BO
=BO�
BP��BR{BS\)BTQ�BU�BV{BW33BXQ�BYp�BZffB[\)B\Q�B]p�B^�\B_�B`��Bap�Bb�\Bc�Bd��Be�Bf�HBg�
Bh��BiBj�HBl(�BmG�Bn=qBo33BpQ�Bqp�Br�HBt  BuG�Bv�\Bw�Bx��By�B{33B|Q�B}��B33B�=qB��HB�p�B�{B��RB�\)B�  B��RB�p�B�{B���B�p�B�(�B��RB�p�B�{B���B�p�B�(�B���B���B�=qB��HB��B�(�B���B�\)B��B�ffB���B�G�B���B�  B�=qB�z�B���B�
=B�G�B���B�B�  B�=qB�z�B��RB���B�G�B��B�B�  B�=qB�z�B���B��HB�
=B�G�B�p�B��B��
B�{B�=qB�z�B���B��HB�
=B�G�B�p�B��B��
B�  B�(�B�ffB���B���B���B�33B�p�B���B��
B�{B�=qB�ffB���B��HB�
=B�G�B��B�B�  B�=qB�z�B��RB���B�33B��B�B�  B�=qB�z�B��RB���B�33B�p�B��B��B�=qB�z�B��RB�
=B�G�B��B��
B�(�B�z�B��RB�
=B�\)B��B�{B�Q�B���B�
=B�\)B��B�  B�Q�B���B���B�G�B���B��B�Q�B���B���B�\)B��B�  B�ffB��RB��B�p�B�B�(�B�z�B��HB�33B��B��B�=qB��\B�
=B�\)B��B�{B�ffB���B�33B��B��
B�=qB���B�
=B�p�B�B�(�B�z�B���B�33B��B��B�=qB���B���B�\)B�B�(�B��\B���B�\)B�B�{B�z�B���B�33B��B��B�Q�B��RB��B���B�  B�ffB��HB�G�BîB�{B�z�B��HB�G�BŮB�  B�ffB���B�33BǮB�{Bȏ\B�
=BɅB�  B�ffB��HB�G�B��
B�=qB̸RB�33B͙�B�{B�ffB��HB�\)B��
B�Q�B���B�G�B�B�Q�B���B�G�BӮB�(�Bԏ\B�
=BՅB�  B�z�B���BׅB�  B؏\B��Bٙ�B�(�Bڏ\B�
=BۅB��B�z�B���B݅B�  Bޏ\B�
=Bߙ�B�{B��\B�
=BᙚB�{B�\B�
=B�p�B��B�ffB��HB�\)B��
B�Q�B���B�\)B��B�ffB��HB�G�B��
B�Q�B�RB�33B뙚B�(�B��B��B홚B�(�B��B��BB�{B��\B�
=B�B��B�z�B��HB�p�B��B�ffB���B��B�{B���B��B���B�  B��\B�
=B���B�(�B��RB�33B�B�Q�B��RB�G�B�B�=qB��RB�G�B��C 33C z�C �RC ��C=qCz�CC{C\)C��C�C�CffC��C�C33C�C��C{C\)C��C�HC�CffC��C��C=qC�C��C�C\)C�C�C	(�C	p�C	�C
  C
G�C
��C
�HC(�CffC�C�HC33Cz�CC
=CQ�C�\C�
C�CffC�C  C=qC�CC
=CQ�C��C�C(�Cp�C��C��C=qC�\C�
C�C\)C��C�HC33C�C��C{CQ�C��C�C=qC�CC
=C\)C�C��C33Cz�C�
C�CffC�C�C=qC�\C�HC(�Cp�C�RC
=C\)C��C�HC(�Cp�CC{C\)C��C�HC33C�C�
C �C \)C �C ��C!Q�C!��C!�HC"�C"p�C"C#{C#\)C#��C#�HC$=qC$�C$��C%{C%Q�C%�C%��C&=qC&�C&��C'{C'p�C'C(
=C(G�C(��C(�C)G�C)�C)��C*{C*ffC*�RC+
=C+G�C+��C+��C,=qC,�C,��C-�C-p�C-��C.{C.\)C.�C/  C/Q�C/��C/�C0=qC0��C0�HC1(�C1p�C1��C2�C2ffC2�C3
=C3ffC3�RC3��C4G�C4��C5  C5Q�C5��C5�HC6=qC6��C6�C7=qC7z�C7��C8�C8z�C8�
C9�C9p�C9�RC:
=C:ffC:�RC;
=C;\)C;��C;�C<=qC<��C<�HC=(�C=p�C=�
C>(�C>z�C>�RC?
=C?ffC?C@
=C@Q�C@��C@��CAG�CA��CA�
CB33CB�CB�
CC�CCffCCCD�CDffCD�CE
=CE\)CE�CE��CFG�CF��CF�CG33CG�CG�HG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                            @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�ȴA���A�~�A���A��A��#A���A���A�ĜAߏ\A�S�A��A�l�A�33A�+A�
=A���A�{A�%A�A�A���A��A��A�  A��AݾwA݋DA�ffA�hsA�jAۣ�A�Q�A���A�9XAԓuA�ffA�p�A�VA��A��wA�E�A��PA�A�VA�M�A��A�I�A�|�A��;A���A���A�C�A��/A��Az��Ap��Aj��AfAc��AaS�A`v�A_/A]��A\�AXn�AS`BAN�9ANJAM��AJ�`AHQ�AG�wAG�7AGhsAGO�AG�AF��AF1'AE��AEdZAE?}ADn�ADr�AE`BAE�
AE�AD�\AD=qAA�
A?�A>  A;hsA9A9oA5&�A3��A3��A3�A4I�A4��A6 �A61A3��A21A1�FA0��A0bNA0�DA0Q�A/A.VA,��A+�wA+��A,A+��A*��A*  A'��A'`BA'�A'hsA'C�A&��A&~�A&-A%��A%�A$�A$M�A#�A"��A!�A!�hA!p�A!hsA!S�A ��A �A =qA bA�wAO�A�A��A�/A��A�jA��An�AffAbNA9XA�A�
A�mA�hAoA��A��A�Ar�AQ�A��At�A�A��A�A�uA1'A��A�A��A�HA�DAbA��A��A�A&�A%A��A��A�A�jAr�A�#A?}A��A�\A1'A{A�FA��A�At�AXA&�A�yAffAA�A��A�+A-A��A�A��AbA�#A�A��Ap�A%A
�jA
1'A	�wA	33A��AA�A��A�
A��A��A��A�jA�An�A �A�TA�
A�-AK�A�uAffAQ�AE�A�A��AO�A�+A�A��AK�AVA �`A ĜA ��A z�A jA Q�A 9XA $�A {@���@��w@�|�@��@�@�x�@�V@���@��@��;@��@�V@���@�j@��w@�C�@��@��\@�x�@�ƨ@��@�@��@�9@�I�@�I�@�F@�+@���@���@�V@��@�9@�1'@�|�@�@�G�@��`@�D@��@�ƨ@��@���@�=q@��@���@�u@�@�bN@�l�@�o@�v�@ᙚ@�?}@��@�b@ߍP@޸R@�-@�@�O�@���@�r�@�t�@�5?@��/@�I�@��;@ץ�@׍P@�t�@�"�@��T@��@�z�@���@ӥ�@�l�@�+@��@Ұ!@�$�@Ѳ-@�X@�V@��`@���@У�@Ѓ@�A�@��@�t�@�ȴ@�ff@��T@ͩ�@�%@�1'@���@�o@��T@���@�r�@� �@�S�@�~�@�$�@�?}@�Ĝ@ċD@�r�@�Z@�I�@� �@þw@�\)@§�@�E�@�@��^@�V@��j@��@�A�@�1@���@��@��R@���@�/@�Ĝ@�bN@���@��F@��P@�\)@��R@�@�@�x�@�O�@�O�@�G�@�7L@��@���@� �@���@��@��@��R@�v�@��@��#@���@�7L@��j@�Z@���@���@��@��@���@�t�@��@���@��y@���@�ȴ@���@���@��\@��+@�v�@�^5@�M�@�-@�J@���@�x�@��@�bN@���@�l�@�
=@��@�hs@�%@���@��u@�1@��w@�dZ@�+@��\@��^@���@��@��@�z�@�Q�@�1'@�1@�t�@��@�v�@�ff@�M�@�-@��T@���@�O�@��@��9@���@���@��u@��D@�j@�Z@�1'@���@��@�t�@��@�~�@��T@���@�p�@�X@�O�@�&�@��j@�z�@�t�@��H@���@�5?@��T@�`B@��@��@�r�@��m@��@�|�@�t�@�33@���@��@��R@��\@�-@�J@��#@��^@���@��h@�X@��@�I�@�  @��m@��;@��
@���@��w@��@�K�@��H@��+@�ff@�E�@�5?@�J@��h@���@�r�@�Z@��w@�t�@�S�@�@�V@���@���@�7L@�%@�r�@�1'@��@�l�@�"�@��@��R@�^5@��@�@�X@���@��D@�Z@�I�@�(�@�  @��
@��w@��w@���@�|�@�S�@�;d@��@�@��H@���@�ff@�E�@���@���@���@��7@�x�@�`B@�G�@���@���@���@��@���@�dZ@�"�@�o@��y@�ȴ@���@�v�@���@�@��-@��@�%@��`@���@�Ĝ@�j@�1'@��@�w@;d@~�@~�+@~5?@}O�@|��@{��@{C�@z��@z^5@zJ@y��@y��@yhs@y%@x�@x1'@w�;@w��@wl�@v�y@vV@v@u@u�h@u?}@t��@t�D@s�
@sS�@r��@r^5@rM�@q�#@qG�@p�`@p�@pQ�@o�;@o�w@o|�@o+@nȴ@n5?@mp�@m/@m�@l�j@lj@lI�@k��@kƨ@k�F@kt�@kC�@j�@i��@i�^@i&�@h�u@h1'@g�;@g��@g\)@f�y@f�+@fV@f{@e��@e�h@ep�@d��@d��@d�@d��@dz�@dz�@dj@d9X@c�m@c��@c��@c��@ct�@c@b-@a7L@`�9@`r�@`1'@_�w@_\)@^��@^�R@^@]��@]��@]�@]O�@\�@[��@[S�@[@Zn�@Z^5@Z=q@Y��@X��@XQ�@X1'@W�@Wl�@WK�@Vȴ@Vv�@Vff@U�T@Up�@U?}@U�@T�@T�/@Tz�@Sƨ@SS�@S@R��@R�!@Rn�@Q��@Q��@Q��@Q�^@Qx�@Q�@P��@P��@Pr�@P �@O�;@O�P@Ol�@OK�@O
=@Nv�@N5?@M��@M`B@L��@LI�@Kƨ@KdZ@K"�@J�@J�!@JJ@I�@I�#@I�@H�u@HbN@HQ�@HA�@G�@GK�@F�y@F��@F$�@E@E/@E�@D�@D�j@D��@D1@C��@CC�@C@B�!@BM�@B�@A�@A��@AG�@@Ĝ@@��@@�@@r�@@A�@@b@?�@?�@?;d@>ȴ@=��@<�@<1@;ƨ@;C�@:��@:n�@:�@9�#@9�7@9x�@9hs@9G�@9G�@9%@8��@8�9@8�@8A�@7�;@7�@6��@6v�@6E�@5@5`B@5�@4��@4��@4j@4�@3�m@3��@3�@333@2�@2^5@2�@1��@1hs@0��@0��@0�9@0r�@0b@/�@/|�@/;d@.��@.��@.5?@.@-�T@-@-p�@,�@,�/@,��@,�j@,��@,j@,I�@+��@+�m@+ƨ@+��@+t�@+"�@*�!@*n�@)�@)x�@)G�@)7L@)&�@(��@(�9@(bN@(1'@(b@'�w@'\)@'�@&ȴ@&��@&�+@&v�@&5?@&@%�@%��@%�-@%`B@%/@$�j@$(�@#dZ@#"�@"�@"��@"��@"n�@"-@!�#@!��@!��@!�7@!hs@!&�@ ��@ �u@ A�@ b@ b@�;@�w@|�@�@��@ȴ@ff@{@��@@��@O�@/@�j@z�@Z@9X@(�@�@1@ƨ@�@S�@o@�!@M�@-@�@�#@x�@x�@hs@G�@7L@&�@�`@��@r�@bN@A�@1'@  @�w@|�@K�@;d@+@��@�y@ȴ@�R@�R@��@ff@��@�h@`B@p�@p�@?}@V@�j@�@z�@9X@�
@��@dZ@C�@o@o@��@^5@J@��@��@x�@G�@&�@�@�@��@��@�u@r�@bN@A�@b@  @�@�w@K�@
=@ȴ@��A��A��+A���A��A���A�ȴA���A�!A�`BA�A��A���A���A��A��;A���A���A���A���A���A���A���A߰!Aߡ�A�~�A�\)A�O�A�I�A�;dA�oA�Aޡ�A�|�A�n�A�S�A�C�A�9XA�5?A�1'A�-A�-A�1'A�1'A�+A�(�A�-A�+A��A��A��A�oA�
=A�
=A�
=A�%A�  A�A�A�  A���A���A���A���A���A�
=A�oA��A��A��A�{A��A�oA�
=A�%A�1A�%A�A�A�A�%A�A�  A�A�A�A���A�A�A�  A���A�  A�A�A�  A���A�  A�A���A���A���A�  A���A��A��A��A��A��A��A��A��A��A��yA��yA��A��A��yA��yA��A���A���A���A���A���A�%A�%A�A�  A���A�A�A���A���A���A���A��A��A��mA��TA���A�AݼjAݼjAݼjAݶFAݬAݣ�Aݟ�Aݛ�AݑhA݇+A�~�A�~�A�v�A�hsA�ffA�dZA�dZA�hsA�hsA�dZA�dZA�hsA�hsA�ffA�dZA�ffA�jA�jA�ffA�hsA�jA�dZA�`BA�dZA�hsA�jA�n�A�p�A�v�A�Q�Aܰ!A�VAۍPA�"�Aڙ�A�$�A�ȴAٓuA�v�A�VA�9XA�&�A�bA�A���A��A��mA���AؼjAز-Aا�Aؙ�A؋DA�x�A�n�A�S�A�+A�oA׾wA�  A�dZA��A��A�O�A�5?AѓuAоwAϡ�AΏ\A͕�A̕�A���Aʲ-A���A�~�A��A�+A�dZA���A�VA�ĜAŁA�=qAė�A���A¾wA��7A�9XA�l�A��!A��
A�jA�%A�A�A���A���A�Q�A�=qA��A��PA��A��A��jA�t�A�z�A��A�M�A��A���A���A���A�=qA��!A��`A��PA�  A�ZA�z�A�oA��A��A��/A���A�=qA��mA��uA�C�A��A��wA���A��A�t�A�hsA�G�A�"�A��A���A���A��RA��9A��A��hA�v�A�jA�ZA�I�A�9XA�(�A��A�%A��TA��A���A���A��+A�K�A��A���A��7A��A��A�G�A��A�hsA��wA�;dA�VA�I�A�A��9A�A�A��\A�A��RA�z�A�7LA�
=A��mA�ĜA���A��+A�jA�;dA�1A���A��PA��A���A�ffA���A���A�bNA��A�|�A��jA�A���A�&�A�ȴA��A�?}A���A��A��
A�K�A���A��A�l�A�E�A�A��;A���A��A�VA�"�A���A��TA���A���A��jA���A�XA���A��RA��\A�v�A�^5A�-A��A�I�A��TA�~�A�S�A�/A�1A���A��A�/A���A��A�1'A��A��hA�ffA�33A�1A��mA��jA�z�A�5?A�+A��A�  A�ĜA�VA�ȴA�A�bA�?}A���A��PA�G�A���A���A�Q�A���A�ffA��jA�~�A�G�A��A��A��^A�;dA�7A~I�A}p�A}G�A|��A|-A{��AzȴAxĜAv��AuG�Atv�As�PAr�\ApȴAo�wAn�An=qAm��Am33Al�Ak��Ak33Aj�/AjbNAi��Ai
=AhVAg�7Af��Af-Ae�
Ae�^Ae��Ae��Ae��Ae�AedZAe+Ad��Ad  Ac�-AcC�Aa�;Aa�FAa��Aa�Aat�AadZAa33Aa�AaA`�`A`�jA`��A`v�A`n�A`bNA`E�A`5?A`{A_��A_��A_|�A_`BA_�A^�jA^M�A^=qA]��A]�A]��A]��A]��A]dZA\�A]
=A\�/A\��A\��A\��A\�+A\z�A\ZA[��A[7LAY��AX��AXJAW��AW�
AW�wAW��AWdZAW/AV�jAU�ATA�AQG�AOhsAO�AN��AN�AN�ANȴAN�9AN��AN�ANffANI�AN5?AN�AN  AM��AM��AM�AM�TAM�#AM�
AM��AM��AM�^AM��AM�AL��AL�DAK�#AK/AJ�jAJA�AI��AH��AH��AH~�AHM�AHE�AH9XAH-AH�AG�AG��AGAG�wAG�^AG�-AG��AG��AG��AG�hAG�PAG�7AG�AGx�AGt�AGt�AGp�AGl�AGdZAG`BAG\)AG\)AG\)AGXAGXAGS�AGK�AGG�AGG�AG?}AG7LAG+AG"�AGVAG%AF��AF��AF�HAFȴAF��AF�uAF�AFz�AFv�AFr�AFffAFVAFA�AF�AFAE�mAE��AE��AEƨAEAE�
AE�AE�TAE��AE��AEK�AEG�AEO�AES�AEO�AEO�AEK�AEK�AEC�AE/AE;dAE/AE�AD��AD��ADz�AD^5AC�AC��AC�FAC�#AC�mAD�ADȴAE7LAE7LAE33AE33AE33AE33AEdZAE�wAE�#AE�AF-AF=qAE��AE��AEx�AEdZAEXAEG�AE+AE
=AD�yADȴAD��AD��AD��AD�DAD�AD�AD�AD~�ADr�ADjADjAD^5AD �AC�FACG�AB�`AB1'AA��AAC�A@��A@��A@jA@9XA@�A?��A?��A?p�A?+A>��A>jA=��A=�A=O�A<��A<Q�A<(�A;l�A:��A:ȴA:��A:9XA9��A9�TA9�A9��A9�7A9|�A9x�A9\)A9/A9&�A8�A8�A7�^A6��A5��A4 �A3�#A3��A3ƨA3��A3��A3��A3��A3��A3��A3�
A3��A3�PA3\)A3t�A3�PA3��A3��A3�A3�A3�wA3�wA3�
A4�A4ffA4v�A4v�A4M�A4^5A4^5A4bNA4v�A4��A5"�A5�;A5��A6�A6�A6(�A6-A6-A6(�A6(�A6$�A6{A6JA6A5�#A5x�A4��A41A3+A2ȴA2I�A2$�A2(�A2�A2A1�A1�A1�A1�A1�A1�TG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                            A�ȴA���A�~�A���A��A��#A���A���A�ĜAߏ\A�S�A��A�l�A�33A�+A�
=A���A�{A�%A�A�A���A��A��A�  A��AݾwA݋DA�ffA�hsA�jAۣ�A�Q�A���A�9XAԓuA�ffA�p�A�VA��A��wA�E�A��PA�A�VA�M�A��A�I�A�|�A��;A���A���A�C�A��/A��Az��Ap��Aj��AfAc��AaS�A`v�A_/A]��A\�AXn�AS`BAN�9ANJAM��AJ�`AHQ�AG�wAG�7AGhsAGO�AG�AF��AF1'AE��AEdZAE?}ADn�ADr�AE`BAE�
AE�AD�\AD=qAA�
A?�A>  A;hsA9A9oA5&�A3��A3��A3�A4I�A4��A6 �A61A3��A21A1�FA0��A0bNA0�DA0Q�A/A.VA,��A+�wA+��A,A+��A*��A*  A'��A'`BA'�A'hsA'C�A&��A&~�A&-A%��A%�A$�A$M�A#�A"��A!�A!�hA!p�A!hsA!S�A ��A �A =qA bA�wAO�A�A��A�/A��A�jA��An�AffAbNA9XA�A�
A�mA�hAoA��A��A�Ar�AQ�A��At�A�A��A�A�uA1'A��A�A��A�HA�DAbA��A��A�A&�A%A��A��A�A�jAr�A�#A?}A��A�\A1'A{A�FA��A�At�AXA&�A�yAffAA�A��A�+A-A��A�A��AbA�#A�A��Ap�A%A
�jA
1'A	�wA	33A��AA�A��A�
A��A��A��A�jA�An�A �A�TA�
A�-AK�A�uAffAQ�AE�A�A��AO�A�+A�A��AK�AVA �`A ĜA ��A z�A jA Q�A 9XA $�A {@���@��w@�|�@��@�@�x�@�V@���@��@��;@��@�V@���@�j@��w@�C�@��@��\@�x�@�ƨ@��@�@��@�9@�I�@�I�@�F@�+@���@���@�V@��@�9@�1'@�|�@�@�G�@��`@�D@��@�ƨ@��@���@�=q@��@���@�u@�@�bN@�l�@�o@�v�@ᙚ@�?}@��@�b@ߍP@޸R@�-@�@�O�@���@�r�@�t�@�5?@��/@�I�@��;@ץ�@׍P@�t�@�"�@��T@��@�z�@���@ӥ�@�l�@�+@��@Ұ!@�$�@Ѳ-@�X@�V@��`@���@У�@Ѓ@�A�@��@�t�@�ȴ@�ff@��T@ͩ�@�%@�1'@���@�o@��T@���@�r�@� �@�S�@�~�@�$�@�?}@�Ĝ@ċD@�r�@�Z@�I�@� �@þw@�\)@§�@�E�@�@��^@�V@��j@��@�A�@�1@���@��@��R@���@�/@�Ĝ@�bN@���@��F@��P@�\)@��R@�@�@�x�@�O�@�O�@�G�@�7L@��@���@� �@���@��@��@��R@�v�@��@��#@���@�7L@��j@�Z@���@���@��@��@���@�t�@��@���@��y@���@�ȴ@���@���@��\@��+@�v�@�^5@�M�@�-@�J@���@�x�@��@�bN@���@�l�@�
=@��@�hs@�%@���@��u@�1@��w@�dZ@�+@��\@��^@���@��@��@�z�@�Q�@�1'@�1@�t�@��@�v�@�ff@�M�@�-@��T@���@�O�@��@��9@���@���@��u@��D@�j@�Z@�1'@���@��@�t�@��@�~�@��T@���@�p�@�X@�O�@�&�@��j@�z�@�t�@��H@���@�5?@��T@�`B@��@��@�r�@��m@��@�|�@�t�@�33@���@��@��R@��\@�-@�J@��#@��^@���@��h@�X@��@�I�@�  @��m@��;@��
@���@��w@��@�K�@��H@��+@�ff@�E�@�5?@�J@��h@���@�r�@�Z@��w@�t�@�S�@�@�V@���@���@�7L@�%@�r�@�1'@��@�l�@�"�@��@��R@�^5@��@�@�X@���@��D@�Z@�I�@�(�@�  @��
@��w@��w@���@�|�@�S�@�;d@��@�@��H@���@�ff@�E�@���@���@���@��7@�x�@�`B@�G�@���@���@���@��@���@�dZ@�"�@�o@��y@�ȴ@���@�v�@���@�@��-@��@�%@��`@���@�Ĝ@�j@�1'@��@�w@;d@~�@~�+@~5?@}O�@|��@{��@{C�@z��@z^5@zJ@y��@y��@yhs@y%@x�@x1'@w�;@w��@wl�@v�y@vV@v@u@u�h@u?}@t��@t�D@s�
@sS�@r��@r^5@rM�@q�#@qG�@p�`@p�@pQ�@o�;@o�w@o|�@o+@nȴ@n5?@mp�@m/@m�@l�j@lj@lI�@k��@kƨ@k�F@kt�@kC�@j�@i��@i�^@i&�@h�u@h1'@g�;@g��@g\)@f�y@f�+@fV@f{@e��@e�h@ep�@d��@d��@d�@d��@dz�@dz�@dj@d9X@c�m@c��@c��@c��@ct�@c@b-@a7L@`�9@`r�@`1'@_�w@_\)@^��@^�R@^@]��@]��@]�@]O�@\�@[��@[S�@[@Zn�@Z^5@Z=q@Y��@X��@XQ�@X1'@W�@Wl�@WK�@Vȴ@Vv�@Vff@U�T@Up�@U?}@U�@T�@T�/@Tz�@Sƨ@SS�@S@R��@R�!@Rn�@Q��@Q��@Q��@Q�^@Qx�@Q�@P��@P��@Pr�@P �@O�;@O�P@Ol�@OK�@O
=@Nv�@N5?@M��@M`B@L��@LI�@Kƨ@KdZ@K"�@J�@J�!@JJ@I�@I�#@I�@H�u@HbN@HQ�@HA�@G�@GK�@F�y@F��@F$�@E@E/@E�@D�@D�j@D��@D1@C��@CC�@C@B�!@BM�@B�@A�@A��@AG�@@Ĝ@@��@@�@@r�@@A�@@b@?�@?�@?;d@>ȴ@=��@<�@<1@;ƨ@;C�@:��@:n�@:�@9�#@9�7@9x�@9hs@9G�@9G�@9%@8��@8�9@8�@8A�@7�;@7�@6��@6v�@6E�@5@5`B@5�@4��@4��@4j@4�@3�m@3��@3�@333@2�@2^5@2�@1��@1hs@0��@0��@0�9@0r�@0b@/�@/|�@/;d@.��@.��@.5?@.@-�T@-@-p�@,�@,�/@,��@,�j@,��@,j@,I�@+��@+�m@+ƨ@+��@+t�@+"�@*�!@*n�@)�@)x�@)G�@)7L@)&�@(��@(�9@(bN@(1'@(b@'�w@'\)@'�@&ȴ@&��@&�+@&v�@&5?@&@%�@%��@%�-@%`B@%/@$�j@$(�@#dZ@#"�@"�@"��@"��@"n�@"-@!�#@!��@!��@!�7@!hs@!&�@ ��@ �u@ A�@ b@ b@�;@�w@|�@�@��@ȴ@ff@{@��@@��@O�@/@�j@z�@Z@9X@(�@�@1@ƨ@�@S�@o@�!@M�@-@�@�#@x�@x�@hs@G�@7L@&�@�`@��@r�@bN@A�@1'@  @�w@|�@K�@;d@+@��@�y@ȴ@�R@�R@��@ff@��@�h@`B@p�@p�@?}@V@�j@�@z�@9X@�
@��@dZ@C�@o@o@��@^5@J@��@��@x�@G�@&�@�@�@��@��@�u@r�@bN@A�@b@  @�@�w@K�@
=@ȴG�O�A��A��+A���A��A���A�ȴA���A�!A�`BA�A��A���A���A��A��;A���A���A���A���A���A���A���A߰!Aߡ�A�~�A�\)A�O�A�I�A�;dA�oA�Aޡ�A�|�A�n�A�S�A�C�A�9XA�5?A�1'A�-A�-A�1'A�1'A�+A�(�A�-A�+A��A��A��A�oA�
=A�
=A�
=A�%A�  A�A�A�  A���A���A���A���A���A�
=A�oA��A��A��A�{A��A�oA�
=A�%A�1A�%A�A�A�A�%A�A�  A�A�A�A���A�A�A�  A���A�  A�A�A�  A���A�  A�A���A���A���A�  A���A��A��A��A��A��A��A��A��A��A��yA��yA��A��A��yA��yA��A���A���A���A���A���A�%A�%A�A�  A���A�A�A���A���A���A���A��A��A��mA��TA���A�AݼjAݼjAݼjAݶFAݬAݣ�Aݟ�Aݛ�AݑhA݇+A�~�A�~�A�v�A�hsA�ffA�dZA�dZA�hsA�hsA�dZA�dZA�hsA�hsA�ffA�dZA�ffA�jA�jA�ffA�hsA�jA�dZA�`BA�dZA�hsA�jA�n�A�p�A�v�A�Q�Aܰ!A�VAۍPA�"�Aڙ�A�$�A�ȴAٓuA�v�A�VA�9XA�&�A�bA�A���A��A��mA���AؼjAز-Aا�Aؙ�A؋DA�x�A�n�A�S�A�+A�oA׾wA�  A�dZA��A��A�O�A�5?AѓuAоwAϡ�AΏ\A͕�A̕�A���Aʲ-A���A�~�A��A�+A�dZA���A�VA�ĜAŁA�=qAė�A���A¾wA��7A�9XA�l�A��!A��
A�jA�%A�A�A���A���A�Q�A�=qA��A��PA��A��A��jA�t�A�z�A��A�M�A��A���A���A���A�=qA��!A��`A��PA�  A�ZA�z�A�oA��A��A��/A���A�=qA��mA��uA�C�A��A��wA���A��A�t�A�hsA�G�A�"�A��A���A���A��RA��9A��A��hA�v�A�jA�ZA�I�A�9XA�(�A��A�%A��TA��A���A���A��+A�K�A��A���A��7A��A��A�G�A��A�hsA��wA�;dA�VA�I�A�A��9A�A�A��\A�A��RA�z�A�7LA�
=A��mA�ĜA���A��+A�jA�;dA�1A���A��PA��A���A�ffA���A���A�bNA��A�|�A��jA�A���A�&�A�ȴA��A�?}A���A��A��
A�K�A���A��A�l�A�E�A�A��;A���A��A�VA�"�A���A��TA���A���A��jA���A�XA���A��RA��\A�v�A�^5A�-A��A�I�A��TA�~�A�S�A�/A�1A���A��A�/A���A��A�1'A��A��hA�ffA�33A�1A��mA��jA�z�A�5?A�+A��A�  A�ĜA�VA�ȴA�A�bA�?}A���A��PA�G�A���A���A�Q�A���A�ffA��jA�~�A�G�A��A��A��^A�;dA�7A~I�A}p�A}G�A|��A|-A{��AzȴAxĜAv��AuG�Atv�As�PAr�\ApȴAo�wAn�An=qAm��Am33Al�Ak��Ak33Aj�/AjbNAi��Ai
=AhVAg�7Af��Af-Ae�
Ae�^Ae��Ae��Ae��Ae�AedZAe+Ad��Ad  Ac�-AcC�Aa�;Aa�FAa��Aa�Aat�AadZAa33Aa�AaA`�`A`�jA`��A`v�A`n�A`bNA`E�A`5?A`{A_��A_��A_|�A_`BA_�A^�jA^M�A^=qA]��A]�A]��A]��A]��A]dZA\�A]
=A\�/A\��A\��A\��A\�+A\z�A\ZA[��A[7LAY��AX��AXJAW��AW�
AW�wAW��AWdZAW/AV�jAU�ATA�AQG�AOhsAO�AN��AN�AN�ANȴAN�9AN��AN�ANffANI�AN5?AN�AN  AM��AM��AM�AM�TAM�#AM�
AM��AM��AM�^AM��AM�AL��AL�DAK�#AK/AJ�jAJA�AI��AH��AH��AH~�AHM�AHE�AH9XAH-AH�AG�AG��AGAG�wAG�^AG�-AG��AG��AG��AG�hAG�PAG�7AG�AGx�AGt�AGt�AGp�AGl�AGdZAG`BAG\)AG\)AG\)AGXAGXAGS�AGK�AGG�AGG�AG?}AG7LAG+AG"�AGVAG%AF��AF��AF�HAFȴAF��AF�uAF�AFz�AFv�AFr�AFffAFVAFA�AF�AFAE�mAE��AE��AEƨAEAE�
AE�AE�TAE��AE��AEK�AEG�AEO�AES�AEO�AEO�AEK�AEK�AEC�AE/AE;dAE/AE�AD��AD��ADz�AD^5AC�AC��AC�FAC�#AC�mAD�ADȴAE7LAE7LAE33AE33AE33AE33AEdZAE�wAE�#AE�AF-AF=qAE��AE��AEx�AEdZAEXAEG�AE+AE
=AD�yADȴAD��AD��AD��AD�DAD�AD�AD�AD~�ADr�ADjADjAD^5AD �AC�FACG�AB�`AB1'AA��AAC�A@��A@��A@jA@9XA@�A?��A?��A?p�A?+A>��A>jA=��A=�A=O�A<��A<Q�A<(�A;l�A:��A:ȴA:��A:9XA9��A9�TA9�A9��A9�7A9|�A9x�A9\)A9/A9&�A8�A8�A7�^A6��A5��A4 �A3�#A3��A3ƨA3��A3��A3��A3��A3��A3��A3�
A3��A3�PA3\)A3t�A3�PA3��A3��A3�A3�A3�wA3�wA3�
A4�A4ffA4v�A4v�A4M�A4^5A4^5A4bNA4v�A4��A5"�A5�;A5��A6�A6�A6(�A6-A6-A6(�A6(�A6$�A6{A6JA6A5�#A5x�A4��A41A3+A2ȴA2I�A2$�A2(�A2�A2A1�A1�A1�A1�A1�A1�TG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                            ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B=B!bBIB$tB)�B-wB-�B3�B49B+6B#�B�B�B,=B;dBAUBO�BXyB[�B[�B[�B[�Ba|BoiB}VB�B��B�*B�dB��B	a�B	��B	��B	�1B	�^B	�]B	��B
�B
]�B
e`B
a�B
aB
0�B
7B
�B
#B
$@B
~B
4B
�B	��B	��B	ƨB	��B	�_B	��B	{B	n�B	m)B	b�B	`vB	_B	TaB	IB	CaB	9�B	�B	=B	�B	)_B	-�B	4�B	6�B	8RB	9�B	?HB	OBB	e�B	�B	��B	��B	�'B	��B
�B
+6B
<B
=�B
@OB
K�B
D�B
6B
�B
�B
MB
;B	�xB
;B
�B
"�B
5B
`BB
b�B
W
B
?HB
>�B
=B
A�B
Q�B
^jB
`�B
[�B
O�B
C-B
J�B
W�B
]�B
\�B
YB
ZB
d�B
xB
�B
��B
�SB
�MB
�AB
�AB
�AB
�4B
��B
��B
}VB
y	B
uZB
u�B
y>B
|�B
��B
�B
.B
}�B
~(B
~]B
}"B
��B
�;B
�AB
�B
�uB
�uB
�B
��B
��B
�MB
�B
��B
��B
�VB
�VB
�VB
�	B
|�B
}�B
|B
|PB
x�B
w�B
v�B
xB
v+B
v�B
t�B
rGB
q�B
s�B
r|B
poB
qB
r�B
w�B
v`B
u�B
u�B
u�B
v+B
zDB
x�B
w�B
u%B
v`B
t�B
v+B
t�B
s�B
sB
r�B
r�B
sB
tB
sB
qB
o�B
n�B
n/B
m�B
k�B
iDB
hsB
g�B
ffB
e�B
e`B
e�B
cTB
aHB
a|B
_pB
\]B
Z�B
YB
W
B
V9B
U�B
V9B
V9B
S[B
S�B
S�B
S[B
R B
Q�B
Q�B
R�B
O�B
NB
MjB
L�B
L�B
K�B
K)B
H�B
FtB
D�B
C�B
B�B
B'B
A�B
A�B
@�B
@OB
@�B
@B
?�B
?HB
?HB
>�B
=�B
>B
=�B
<6B
;dB
9�B
9�B
9�B
:^B
8RB
9XB
7B
5B
4�B
2�B
2�B
1�B
1�B
.B
-�B
-wB
.�B
.�B
0UB
1�B
0UB
.�B
.�B
.B
,�B
,B
(�B
(�B
%zB
#�B
#:B
#nB
$�B
%�B
&LB
(XB
($B
($B
'�B
&LB
%�B
%zB
&�B
$tB
$�B
#�B
#:B
#nB
"�B
"�B
"hB
!-B
!bB
 �B
 'B
 \B
!bB
 �B
 �B
OB
!B
B
�B
IB
B
�B
�B
�B
CB
xB
�B
B
�B
qB
�B
�B
�B
	B
�B
�B
7B
7B
B
eB
=B
�B
=B
B
�B
CB
B
	B
B
B
B
kB
	B
B
�B
�B
�B
�B
�B
�B
�B
xB
CB
CB
CB
�B
xB
xB
B
B
CB
CB
CB
�B
CB
�B
�B
�B
�B
�B
�B
�B
kB
7B
�B
kB
	B
�B
	B
=B
	B
�B
�B
kB
�B
kB
kB
kB
kB
�B
�B
�B
B
�B
7B
eB
eB
�B
�B
�B
YB
_B
+B
�B
_B
�B
�B
_B
_B
�B
�B
�B
�B
_B
�B
_B
�B
�B
�B
�B
B
7B
kB
�B
�B
xB
xB
CB
�B
B
~B
~B
�B
OB
�B
OB
B
VB
!B
!B
�B
!B
�B
 \B
 'B
�B
 'B
 'B
 �B
 �B
!-B
!bB
!�B
!�B
!bB
!�B
!�B
!�B
!�B
!�B
!�B
"4B
"4B
"�B
$B
$@B
$tB
$�B
$tB
$@B
$tB
$�B
$@B
'B
%�B
&B
&LB
&�B
'B
'B
'B
'B
($B
'�B
'�B
'RB
'�B
'�B
'�B
'�B
(XB
(�B
(�B
)*B
)_B
)_B
)�B
*�B
,=B
-�B
.B
.IB
.B
.IB
.B
.B
.B
.�B
/�B
/�B
/OB
/OB
/B
/B
/�B
1�B
0�B
0�B
1�B
1'B
0�B
1[B
1[B
1[B
1[B
2-B
1�B
2�B
2aB
2�B
3�B
49B
4B
49B
4�B
5B
5?B
6zB
7B
7�B
7�B
7�B
8B
8�B
8�B
9�B
9�B
:^B
:�B
:�B
:�B
;0B
;0B
;0B
<6B
<6B
<jB
=B
=<B
=qB
=qB
=�B
=�B
=�B
>�B
>wB
>�B
?B
A B
AUB
A�B
AUB
A�B
A�B
A�B
B'B
B�B
B�B
B�B
C-B
C�B
CaB
C-B
C-B
D3B
D3B
D3B
D�B
EB
E9B
E9B
E9B
F?B
F�B
GzB
GzB
G�B
HB
HB
HKB
HB
H�B
H�B
H�B
IB
I�B
IRB
IRB
J#B
J#B
J�B
JXB
J�B
J�B
J�B
K^B
K�B
L0B
L0B
L0B
K�B
L�B
L�B
MB
MB
M6B
MjB
M6B
M�B
M�B
M�B
NpB
N�B
NpB
N�B
OB
N�B
OB
OvB
OBB
OB
OvB
OB
O�B
P�B
PHB
P�B
Q�B
QNB
Q�B
Q�B
Q�B
Q�B
R�B
RTB
R�B
R�B
R�B
R�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
S�B
TaB
TaB
T,B
T,B
S�B
T�B
UgB
VB
VB
V9B
V9B
V9B
V�B
V�B
V�B
W�B
W�B
WsB
W�B
W?B
XyB
XyB
W�B
X�B
X�B
XyB
X�B
X�B
ZQB
ZB
ZB
Z�B
Z�B
Z�B
[WB
Z�B
[#B
\]B
\]B
\�B
\]B
\]B
\]B
\�B
]dB
]�B
]�B
]�B
]�B
^5B
^�B
^jB
^5B
^jB
^jB
_B
_B
^�B
_B
_;B
_�B
_�B
_�B
_�B
`B
`vB
`vB
`�B
`vB
aHB
a|B
a�B
bB
bNB
bB
bNB
c B
b�B
b�B
c�B
c�B
c�B
c�B
c B
d&B
c�B
dZB
dZB
e,B
e,B
e�B
e�B
e`B
e�B
e�B
f�B
f2B
ffB
f�B
f�B
e�B
e�B
gB
gB
g�B
g�B
g�B
g�B
g�B
h
B
g�B
g�B
g�B
h>B
g�B
iyB
jB
i�B
i�B
j�B
kB
kQB
kQB
kQB
k�B
kQB
k�B
k�B
kB
k�B
k�B
k�B
l"B
k�B
lWB
m�B
m)B
m)B
m�B
ncB
m�B
n�B
m�B
n�B
o B
ncB
o B
oiB
o B
o5B
o5B
o B
o B
n�B
p�B
pB
p;B
pB
poB
p�B
p�B
qB
q�B
qvB
qvB
q�B
q�B
q�B
q�B
rB
r|B
rGB
rGB
r|B
r|B
r�B
r�B
s�B
r�B
sMB
s�B
sMB
s�B
s�B
s�B
tTB
t�B
tTB
tTB
tTB
t�B
t�B
t�B
t�B
t�B
u%B
u%B
u�B
u�B
u�B
u�B
u�B
v+B
v`B
v`B
v`B
v`B
v�B
v�B
wfB
w�B
x�B
x�B
xlB
x�B
xlB
x�B
y	B
zB
y�B
y�B
zB
zxB
z�B
zxB
{�B
|PB
|�B
|PB
}VB
}VB
}�B
~(B
~(B
~(B
~�B
~�B
cB
~�B
cB
�B
.B
�iB
��B
��B
��B
�B
�B
��B
�iB
��B
�;B
�B
�AB
�AB
�uB
�uB
��B
�B
��B
�B
�B
��B
��B
�GB
�GB
��B
�{B
��B
��B
�B
��B
��B
��B
�B
�SB
��B
��B
�YB
�%B
�%B
�%B
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
�1B
��B
��B
��B
��B
�1B
�1B
�7B
�lB
�lB
�7B
�7B
�7B
�B
�7B
�lB
�lB
�lB
�7B
��B
�	B
�=B
��B
��B
�rB
�=B
��B
�DB
�DB
�xB
�JB
�xB/�BWsBgBeB�BOBeB�B$�BCB�B!�B#�B&�B(�B+�B+kB,qB-CB/�B.�B2-B7B4nB7B,B$B#:B#�B,B�B�BCBMB�B�B�B�BBxB �B#nB$�B(�B,�B.B/�B49B7B6zB8�B;�B;�B<B=<B?}B?HB>�B@OBB[BB[B?�BAUBCaBF�BJ�BK�BNpBPBR�BR BT�BWsBXEBV�BW�BYBY�BX�BX�BZ�B[�B[#BZ�B[�B\�B[�B[WB\]B]/B\]B[#B[WB\�B\�B\)BZ�B\]B]�BZ�BZQB[�B\)B\)BZ�BZQB[#B\)B\�B[WB[�B\�B]dB\�B\�B_;BdZBc�Bb�Be�Bf�BiDBh>Bj�Bm)Bp;Bs�Bu�Bu�BwfBzxB|�B|�B|�B~�B�iB�AB�uB��B�%B�1B�fB��B��B��B�@B��B�+B�OB�B��B��B�wB��B��B�dB��B�XB�XB��B��B��B��B��B��B�dB��B��B�6B�dB��B��B�qB��B�6B�6B��B�BB��B�/B	MB	bNB	~�B	~�B	��B	�uB	��B	��B	��B	�bB	�\B	�:B	�@B	��B	��B	�'B	�!B	�@B	��B	�~B	�=B	��B	�7B	�$B	�B	�B	��B	�uB	�B	�B	��B	��B	��B	��B	��B	��B	�pB	�&B	�jB	��B	�NB	�B	��B	��B	՛B	��B	�fB	�B	�TB
�B
�B
�B
DB
#nB
6FB
N�B
XEB
lWB
[WB
^5B
g�B
ZB
_B
j�B
k�B
q�B
ZQB
XEB
e,B
f�B
g8B
]�B
a�B
ZQB
�fB
{�B
`B
d&B
_;B
dZB
L0B
H�B
A�B
A�B
YKB
}"B
NB
[�B
E�B
EmB
;0B
,qB
)�B
&�B
'RB
'�B
($B
(�B
(�B
#B
#B
!�B
 \B
!�B
%�B
$�B
OB
�B
�B
�B
qB
�B
�B
�B
�B
	B
YB
�B
�B
B
SB
�B
�B
�B
:B
�B
+B
�B
�B
*�B
�B
1B
B
'B
'RB
�B
qB
8B
�B
%B
'�B
6B
*0B
($B
$�B
&�B
#nB
!�B
!-B
�B
VB
�B
�B
�B
OB
�B
($B
$tB
 'B
'�B
VB
!bB
(�B
!bB
6zB
*�B
�B
)�B
OB
�B
�B
B
$�B
.IB
%B
�B
eB
eB
4B
�B
B
@B
uB
@B
�B
�B
�B
.B
�B
�B
�B
�B
�B
�B
\B
xB

	B
�B
�B
IB

=B
JB
;B	��B	��B	��B
�B	��B	�B
AB	��B	�xB	��B	��B	�B	�B	�B	�B	�fB	��B	��B	��B	�B	�#B	��B	�B	�NB	��B	�#B	�-B	��B	�<B	�BB	��B	��B	�tB	�B	�IB	�_B	�B	�bB	��B	��B	��B	��B	��B	�B	��B	��B	��B	�VB	��B	�wB	��B	�B	�xB	��B	��B	�hB	��B	�uB	��B	��B	y	B	�iB	~(B	{B	v�B	zB	y�B	|B	z�B	u�B	zB	u�B	o B	j�B	i�B	iDB	g�B	iyB	iDB	l"B	q�B	n/B	ffB	m�B	rGB	bB	b�B	bB	bB	b�B	dZB	b�B	b�B	c B	c�B	b�B	aHB	`B	_pB	`vB	^�B	_�B	c�B	^5B	[�B	]�B	^5B	f�B	R B	cTB	P�B	R�B	R�B	S�B	O�B	N�B	K^B	JXB	L�B	GB	K�B	PHB	H�B	E�B	GEB	J�B	X�B	X�B	V�B	C�B	9�B	7�B	8RB	6B	4nB	2�B	8�B	4nB	QB	aHB	'�B	�B	+B	�B	B	SB	�B	�B	7B	�B	�B	�B	CB	B	kB	�B	7B	CB	B	B	�B	�B	IB	 �B	VB	IB	%zB	*eB	'B	)*B	&LB	4nB	,B	+B	+6B	/OB	.B	.�B	.�B	/�B	2aB	4B	3�B	4nB	3�B	4�B	5tB	6B	7�B	6�B	6FB	6zB	6�B	6FB	7B	7B	7B	8RB	8�B	9$B	:^B	8�B	8�B	8�B	8�B	9XB	:*B	:�B	:�B	;�B	;�B	=B	>wB	@�B	A B	B'B	B�B	E�B	I�B	N<B	QB	S�B	T�B	UgB	V�B	YB	\�B	bNB	lWB	p�B	t�B	t�B	tTB	v�B	|�B	�uB	�B	��B	��B	�FB	�FB	��B	�uB	�FB	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	�nB	�[B	�zB	��B	��B	��B	�mB	�KB	�WB	��B	�sB
 4B
B
�B
AB
AB	��B
�B
oB
B
MB
&�B
7�B
;0B
:*B
:�B
:�B
:^B
;0B
<jB
<�B
?B
>B
=B
=�B
>BB
?HB
=qB
<�B
<�B
>B
>BB
<jB
=�B
C�B
GEB
C-B
L�B
OB
QNB
IRB
K�B
L�B
K^B
C�B
E�B
A�B
@�B
=�B
C�B
?B
8�B
7�B
,qB
0�B
-B
 �B
'�B
5tB
�B
{B
uB
�B
�B

	B

rB
�B
�B
MB
�B
�B
�B
 iB
B
�B
�B
GB
�B
�B	�B	�B	��B	��B	�rB	�xB	�JB	�PB	�PB	�PB	��B
�B	��B	��B
+B
	7B

�B
PB
VB
�B
�B
~B
�B
 �B
'�B
-�B
4B
*�B
.}B
/B
33B
.B
1'B
FtB
VmB
[�B
b�B
aHB
`�B
b�B
c�B
dZB
cTB
b�B
c�B
`BB
e�B
`�B
g�B
b�B
W�B
MB
GB
@OB
>�B
?�B
@B
?�B
>�B
=<B
=<B
>BB
>�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                            BΊB�B%BB%.B*+B-�B/5B5�B6�B-�B'7BBB,�B;�BA BO�BX�B[�B\ B[�B[�BaFBo�B~mB�)B��B�B�nB�TB	l>B	�UB	��B	��B	�B	�lB	�QB
7B
tMB
uJB
�VB
�B
C�B
4JB
A}B
>�B
EoB
.FB
 HB
qB
B	��B	��B	�.B	��B	��B	��B	vB	uDB	fHB	d�B	d	B	Y#B	WOB	T-B	HVB	)B	mB	&�B	1�B	/�B	5ZB	7-B	8�B	:�B	AB	P�B	g/B	��B	�;B	��B	��B	��B
�B
-�B
=�B
?�B
H�B
R6B
K�B
>�B
%�B
+B
AB
�B	�KB
 �B
�B
 B
1QB
`�B
j�B
\BB
@�B
AB
?B
ACB
R�B
b�B
c`B
`�B
R�B
C*B
JiB
X�B
a�B
_�B
a2B
[B
c�B
y�B
��B
��B
��B
��B
��B
��B
�AB
�EB
��B
��B
�B
z`B
u�B
u�B
y�B
}�B
��B
�B
�B
~�B
�B
�B
}(B
�
B
�qB
��B
�B
�B
��B
�6B
�FB
�@B
�$B
�B
�B
�B
�@B
�iB
�UB
��B
}@B
~�B
}�B
}�B
y B
w�B
xCB
yaB
w�B
yB
u�B
r�B
s'B
ueB
s�B
p�B
quB
s�B
xB
v�B
u�B
vB
vB
w^B
|NB
z�B
yKB
v:B
w�B
uB
wbB
uVB
t!B
sUB
s B
smB
tB
u�B
uIB
r2B
rQB
o�B
oB
o�B
m�B
j�B
j�B
h�B
g	B
f/B
fB
gB
dB
cOB
c9B
a^B
]�B
\�B
ZB
W�B
VhB
V�B
XJB
WB
S�B
T�B
T�B
T9B
RaB
RYB
S�B
T�B
P�B
N^B
M�B
M�B
NMB
L�B
M�B
J�B
G�B
E�B
D�B
C/B
B�B
B"B
BB
A4B
@�B
@�B
@lB
?�B
?�B
?�B
?2B
>�B
?�B
>�B
<�B
;�B
:�B
:�B
;<B
;B
9�B
;�B
8QB
5�B
5DB
3�B
4�B
4�B
3B
/B
/9B
/�B
/�B
.�B
1]B
2�B
0�B
/EB
/�B
/B
/	B
-B
*CB
+%B
&�B
$�B
#�B
$�B
%B
&9B
'�B
)�B
)hB
)�B
(B
&yB
&DB
')B
'�B
%�B
&bB
$�B
$�B
$PB
#�B
$%B
#fB
"B
"=B
!�B
!NB
"JB
#�B
"�B
"B
 B
�B
UB
&B
B
fB
 �B
B
�B
�B
�B
`B
�B
@B
kB
�B
UB
kB
\B
�B
�B
�B
�B
jB
�B
nB
�B
1B
�B
�B
�B
�B
�B
:B
�B
B
3B
�B
�B
wB
�B
�B
B
B
�B
�B
�B
B
B
�B
kB
B
B
LB
�B
zB
B
�B
�B
1B
wB
B
(B
{B
�B
gB
"B
�B
�B
B
�B
�B
dB
XB
EB
!B
�B
B
cB
�B
�B
B
yB
(B
0B
�B
�B
>B
�B
!B
7B
�B
�B
ZB
�B
gB
^B
�B
�B
�B
�B
�B
xB
�B
�B
�B
�B
�B
�B
�B
�B
B
<B
�B
VB
�B
BB
NB
�B
�B
0B
�B
B
�B
�B
)B
B
�B
�B
 B
�B
�B
 xB
B
iB
WB
 9B
 �B
!7B
 RB
 -B
 rB
 �B
!B
!7B
!�B
!�B
!�B
!�B
!zB
!�B
!�B
!�B
!�B
"@B
"�B
"�B
"�B
#�B
%'B
$�B
$�B
$�B
$�B
$�B
%CB
%OB
&,B
(.B
&fB
&�B
&�B
'|B
'�B
'�B
'�B
(B
(�B
(B
'�B
'�B
(`B
(7B
(B
(LB
)	B
)AB
)XB
)qB
)�B
)�B
*MB
+�B
,�B
.9B
.JB
.^B
.+B
.^B
.;B
.HB
.�B
/�B
0*B
/�B
/�B
/zB
/�B
0"B
15B
2%B
1B
2B
2NB
1wB
1vB
2�B
2B
2B
20B
2�B
2�B
3�B
2�B
3�B
45B
4�B
4YB
4�B
5cB
5�B
6B
7uB
7�B
7�B
8B
8/B
8qB
8�B
9#B
9�B
9�B
:�B
:�B
;0B
;B
;eB
;{B
;�B
<�B
<�B
<�B
=jB
=�B
=�B
=�B
=�B
=�B
>CB
>�B
>�B
?<B
@jB
A�B
A�B
A�B
A�B
BB
A�B
B"B
CB
CjB
B�B
CB
DB
DB
C�B
C`B
C�B
D�B
DoB
D�B
E"B
EgB
E�B
E�B
FB
F�B
G�B
G�B
G�B
H!B
HjB
H\B
H�B
HTB
H�B
I3B
I>B
IsB
I�B
I�B
I�B
J�B
JzB
KB
J�B
J�B
K>B
K=B
LB
LQB
L�B
L�B
LOB
LCB
M'B
L�B
McB
M?B
M�B
M�B
MB
M�B
N	B
NiB
O)B
OB
N�B
OB
O]B
OB
O`B
O�B
O\B
ORB
O�B
OpB
P�B
QB
P�B
QwB
Q�B
Q�B
Q�B
Q�B
R`B
RRB
R�B
R�B
S2B
S5B
SB
SaB
S�B
S�B
S�B
S�B
S�B
S�B
S�B
TB
T�B
TiB
TCB
TXB
TCB
UoB
VNB
V�B
VLB
V�B
V�B
V�B
W9B
V�B
W}B
W�B
W�B
W�B
W�B
W�B
YuB
X�B
X:B
YjB
X�B
X�B
YaB
Y�B
Z�B
ZMB
Z�B
Z�B
Z�B
[B
[�B
[B
[�B
\�B
\�B
\�B
\�B
\zB
\�B
]oB
]�B
^B
]�B
]�B
]�B
^�B
^�B
^rB
^NB
^�B
^�B
_oB
_B
_B
_YB
_�B
_�B
_�B
_�B
`#B
`�B
`�B
a B
a'B
`�B
a�B
bB
bHB
b^B
b�B
bfB
b�B
cDB
b�B
c@B
doB
c�B
c�B
c�B
c~B
d�B
c�B
d�B
d�B
e�B
e�B
e�B
e�B
e�B
e�B
f)B
g,B
f�B
f�B
f�B
f�B
f8B
f8B
gGB
gnB
hNB
g�B
g�B
g�B
g�B
h;B
g�B
g�B
hHB
h�B
h�B
jgB
j�B
i�B
j1B
k0B
krB
k�B
k�B
k�B
lB
kiB
k�B
k�B
kcB
lB
lB
l&B
ljB
laB
mB
n>B
mRB
miB
n}B
n�B
nB
n�B
n6B
n�B
oOB
n�B
oAB
o�B
oUB
oB
o�B
oGB
o/B
o'B
qB
p0B
pbB
pOB
p�B
q:B
p�B
qSB
q�B
q�B
q�B
q�B
q�B
q�B
rB
r�B
r�B
r\B
r^B
r�B
r�B
sB
sB
s�B
sB
srB
s�B
s�B
t$B
tB
t=B
t�B
t�B
tjB
tmB
t�B
t�B
t�B
t�B
uB
uFB
u�B
umB
vB
vB
u�B
u�B
v<B
v^B
vvB
v�B
v�B
v�B
wB
wCB
w�B
xXB
x�B
x�B
x�B
x�B
x�B
yB
y^B
z>B
y�B
y�B
z9B
z�B
{B
z�B
|B
|�B
|�B
|�B
}zB
}�B
}�B
~MB
~aB
~�B
~�B
~�B
uB
%B
�B
�B
�B
��B
��B
��B
��B
�B
�B
�B
��B
��B
��B
�kB
��B
�gB
��B
��B
�9B
�B
��B
�4B
�&B
��B
��B
��B
�{B
��B
��B
��B
��B
�_B
��B
��B
��B
�5B
��B
�B
��B
�iB
�+B
�=B
�mB
��B
�B
��B
��B
��B
�+B
�bB
��B
�B
��B
�xB
��B
��B
�.B
��B
�`B
�?B
��B
��B
��B
�yB
�]B
�lB
�8B
�ZB
�B
�sB
��B
�aB
�B
�.B
�SB
��B
�B
��B
�VB
�B
��B
��B
��B
�bG�O�B/�BWsBgBeB�BOBeB�B$�BCB�B!�B#�B&�B(�B+�B+kB,qB-CB/�B.�B2-B7B4nB7B,B$B#:B#�B,B�B�BCBMB�B�B�B�BBxB �B#nB$�B(�B,�B.B/�B49B7B6zB8�B;�B;�B<B=<B?}B?HB>�B@OBB[BB[B?�BAUBCaBF�BJ�BK�BNpBPBR�BR BT�BWsBXEBV�BW�BYBY�BX�BX�BZ�B[�B[#BZ�B[�B\�B[�B[WB\]B]/B\]B[#B[WB\�B\�B\)BZ�B\]B]�BZ�BZQB[�B\)B\)BZ�BZQB[#B\)B\�B[WB[�B\�B]dB\�B\�B_;BdZBc�Bb�Be�Bf�BiDBh>Bj�Bm)Bp;Bs�Bu�Bu�BwfBzxB|�B|�B|�B~�B�iB�AB�uB��B�%B�1B�fB��B��B��B�@B��B�+B�OB�B��B��B�wB��B��B�dB��B�XB�XB��B��B��B��B��B��B�dB��B��B�6B�dB��B��B�qB��B�6B�6B��B�BB��B�/B	MB	bNB	~�B	~�B	��B	�uB	��B	��B	��B	�bB	�\B	�:B	�@B	��B	��B	�'B	�!B	�@B	��B	�~B	�=B	��B	�7B	�$B	�B	�B	��B	�uB	�B	�B	��B	��B	��B	��B	��B	��B	�pB	�&B	�jB	��B	�NB	�B	��B	��B	՛B	��B	�fB	�B	�TB
�B
�B
�B
DB
#nB
6FB
N�B
XEB
lWB
[WB
^5B
g�B
ZB
_B
j�B
k�B
q�B
ZQB
XEB
e,B
f�B
g8B
]�B
a�B
ZQB
�fB
{�B
`B
d&B
_;B
dZB
L0B
H�B
A�B
A�B
YKB
}"B
NB
[�B
E�B
EmB
;0B
,qB
)�B
&�B
'RB
'�B
($B
(�B
(�B
#B
#B
!�B
 \B
!�B
%�B
$�B
OB
�B
�B
�B
qB
�B
�B
�B
�B
	B
YB
�B
�B
B
SB
�B
�B
�B
:B
�B
+B
�B
�B
*�B
�B
1B
B
'B
'RB
�B
qB
8B
�B
%B
'�B
6B
*0B
($B
$�B
&�B
#nB
!�B
!-B
�B
VB
�B
�B
�B
OB
�B
($B
$tB
 'B
'�B
VB
!bB
(�B
!bB
6zB
*�B
�B
)�B
OB
�B
�B
B
$�B
.IB
%B
�B
eB
eB
4B
�B
B
@B
uB
@B
�B
�B
�B
.B
�B
�B
�B
�B
�B
�B
\B
xB

	B
�B
�B
IB

=B
JB
;B	��B	��B	��B
�B	��B	�B
AB	��B	�xB	��B	��B	�B	�B	�B	�B	�fB	��B	��B	��B	�B	�#B	��B	�B	�NB	��B	�#B	�-B	��B	�<B	�BB	��B	��B	�tB	�B	�IB	�_B	�B	�bB	��B	��B	��B	��B	��B	�B	��B	��B	��B	�VB	��B	�wB	��B	�B	�xB	��B	��B	�hB	��B	�uB	��B	��B	y	B	�iB	~(B	{B	v�B	zB	y�B	|B	z�B	u�B	zB	u�B	o B	j�B	i�B	iDB	g�B	iyB	iDB	l"B	q�B	n/B	ffB	m�B	rGB	bB	b�B	bB	bB	b�B	dZB	b�B	b�B	c B	c�B	b�B	aHB	`B	_pB	`vB	^�B	_�B	c�B	^5B	[�B	]�B	^5B	f�B	R B	cTB	P�B	R�B	R�B	S�B	O�B	N�B	K^B	JXB	L�B	GB	K�B	PHB	H�B	E�B	GEB	J�B	X�B	X�B	V�B	C�B	9�B	7�B	8RB	6B	4nB	2�B	8�B	4nB	QB	aHB	'�B	�B	+B	�B	B	SB	�B	�B	7B	�B	�B	�B	CB	B	kB	�B	7B	CB	B	B	�B	�B	IB	 �B	VB	IB	%zB	*eB	'B	)*B	&LB	4nB	,B	+B	+6B	/OB	.B	.�B	.�B	/�B	2aB	4B	3�B	4nB	3�B	4�B	5tB	6B	7�B	6�B	6FB	6zB	6�B	6FB	7B	7B	7B	8RB	8�B	9$B	:^B	8�B	8�B	8�B	8�B	9XB	:*B	:�B	:�B	;�B	;�B	=B	>wB	@�B	A B	B'B	B�B	E�B	I�B	N<B	QB	S�B	T�B	UgB	V�B	YB	\�B	bNB	lWB	p�B	t�B	t�B	tTB	v�B	|�B	�uB	�B	��B	��B	�FB	�FB	��B	�uB	�FB	�B	�B	��B	��B	��B	�B	��B	��B	��B	��B	�nB	�[B	�zB	��B	��B	��B	�mB	�KB	�WB	��B	�sB
 4B
B
�B
AB
AB	��B
�B
oB
B
MB
&�B
7�B
;0B
:*B
:�B
:�B
:^B
;0B
<jB
<�B
?B
>B
=B
=�B
>BB
?HB
=qB
<�B
<�B
>B
>BB
<jB
=�B
C�B
GEB
C-B
L�B
OB
QNB
IRB
K�B
L�B
K^B
C�B
E�B
A�B
@�B
=�B
C�B
?B
8�B
7�B
,qB
0�B
-B
 �B
'�B
5tB
�B
{B
uB
�B
�B

	B

rB
�B
�B
MB
�B
�B
�B
 iB
B
�B
�B
GB
�B
�B	�B	�B	��B	��B	�rB	�xB	�JB	�PB	�PB	�PB	��B
�B	��B	��B
+B
	7B

�B
PB
VB
�B
�B
~B
�B
 �B
'�B
-�B
4B
*�B
.}B
/B
33B
.B
1'B
FtB
VmB
[�B
b�B
aHB
`�B
b�B
c�B
dZB
cTB
b�B
c�B
`BB
e�B
`�B
g�B
b�B
W�B
MB
GB
@OB
>�B
?�B
@B
?�B
>�B
=<B
=<B
>BB
>�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111114111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                            <#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<M�><@�<#�
<#�
<�4=��<��<��-<���<�1�<���=�=E�q<�?R<��=	07<�j=�g<��<�=�<���<��<�1<��p<�k[=�#<���<x��<#�
<#�
<#�
<#�
<#�
<#�
<t�w<��A<wq�<#�
<#�
<+z�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<%��<#�
<#�
<b;�<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
<#�
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
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            PSAL_ADJ corrects Cnd. Thermal Mass (CTM), Johnson et al., 2007, JAOT;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   CTM: alpha=0.141C, tau=6.89s with error equal to the adjustment;                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                NO correction for Conductivity Thermal Mass (CTM) is applied;                                                                                                                                                                                                   SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     No significant salinity drift detected;                                                                                                                                                                PSAL_ADJ_ERR: max(0.01, OWC + CTM + resolution error)    202304261914182023042619141820230426191418202304261914182023042619141820230426191418SI  SI  ARFMARFM                                                                                                                                                2019121001003920191210010039IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019121016241220191210162412QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2019121016241220191210162412QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020010906573220200109065732IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV3.1V3.1                                                                                                                                2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V3.0V3.0CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2023042619142120230426191421IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                