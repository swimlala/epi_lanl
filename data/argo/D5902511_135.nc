CDF   	   
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  e   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2020-06-02T22:23:54Z creation; 2020-07-07T21:55:47Z DMQC;      
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
_FillValue        G�O�     (  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  X0   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     (  ^�   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  z$   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     (  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     (  �   TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �@   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     (  �   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  �4   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     (  �    PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     (  �(   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � P   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     (    PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 8D   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     ( ?   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` Z8   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   Z�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   `�   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   f�   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T l�   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   l�   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   l�   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   l�   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   m   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � m   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   m�   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   m�   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    m�   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        m�   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        m�   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       m�   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    m�Argo profile    3.1 1.2 19500101000000  20200602222354  20210429202812  5902511 5902511 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               �   �AA  AOAO6810_008521_135                 6810_008521_135                 2C  2C  DD  SOLO_II                         SOLO_II                         8521                            8521                            V2.2; SBE602 27Jul16            V2.2; SBE602 27Jul16            853 853 @�8�h@�8�h11  @�8���?@�8���?@3:�]�y�@3:�]�y��e(3:}��e(3:}�11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?��@   @E�@�G�@��R@�  @�\A   A  A!G�A,(�A@  A`��A���A���A���A�Q�A�  AϮA�  A�  A��B�B�
B  B   B(  B0(�B8  B@  BG�
BP  BW�
B_�
Bg�
Bo�
Bx  B�{B�  B��B�  B��B��
B��
B��B�  B��B��B��
B�  B�{B�  B�{B�{B�{B�  B�  B�{B��
B��B�{B�  B��B��B�  B�  B��B��B��B��C  C  C��C��C

=C��C��C  C��C  C  C  C
=C��C��C   C!��C$  C&  C'�C*
=C,
=C.  C0
=C2{C4
=C6  C8
=C9��C;��C>  C@  CA��CC�CE�CG�CI�CL  CN{CP{CQ��CS��CV  CX
=CY��C[��C]��C`{Ca��Cd  Ce��Ch  Cj{Cl  Cn  Cp
=Cq��Ct  Cv
=Cx  Cz
=C{��C~  C�C�  C�C���C���C���C�  C�  C�  C�  C���C�  C�  C�  C���C�C�C���C���C�  C�  C�C�  C�C���C���C���C�  C�C�C�  C���C�  C�C�C�  C�C�  C���C�  C�  C�  C�  C���C���C���C���C���C�C�C�  C�
=C�  C���C�  C���C�  C�
=C�  C�  C�C�C���C�  C�  C�C�  C���C���C�  C�  C�C�C���C�  C���C�C�  C���C�C�C�  C���C���C�C���C�  C�  C�  C�C�  C�C���C���C�C�  C�
=C�C���C�  C�C�  C�  C���C���C�C�C�C�  C�  C�C�C���C�  C�C���C���C���C�  C�  C���C���C�C�C���C���C�  C�
=D D � D  Dz�D�D��D�D� D��D� D  Dz�D��Dz�D�qD� DD��D	  D	� D	�qD
� D
��D� D��Dz�D�qD��D�D}qD  D��D  D� D�D}qD��D}qD�qDz�D�qD� D�D� D�qDz�D  D� D�qDz�D  D��D  D}qD�qDz�D�D��D  D}qD�qD}qD  D�D   D z�D!  D!��D"  D"� D"�qD#z�D#�qD$}qD%  D%�D&D&� D'  D'��D(�D(}qD(�qD)� D*  D*}qD*�qD+}qD+�qD,z�D,�qD-� D.�D.� D.��D/� D0  D0��D1D1��D2  D2��D3D3��D4�D4� D5�D5� D5�qD6}qD7  D7}qD7��D8xRD8��D9z�D9�qD:� D:�qD;}qD<  D<��D=  D=}qD>  D>� D?�D?� D?�qD@z�D@��DA}qDA��DBz�DC  DC��DD  DD� DD�qDE� DF�DF��DG�DG��DH�DH��DH�qDIz�DI�qDJ� DK  DK}qDL�DL� DL�qDM}qDN  DN}qDN�qDO� DP�DP� DQ  DQ}qDR  DR��DS�DS}qDS��DT}qDU  DU��DV�DV��DW  DW}qDW�qDX��DY�DY}qDY�qDZ}qDZ�qD[� D\  D\� D]  D]��D^�D^� D_�D_}qD_�qD`��DaDa��Db  Db� Dc  Dc� Dc��Ddz�Dd�qDe� Df�Df��Dg�Dg�Dh�Dh��Dh�qDi}qDj  Dj� Dj�qDk� Dl�Dl}qDl�qDm}qDm�qDn��Do�Do��Dp  Dpz�Dp�qDq� Dr  Dr��Ds  Ds��Dt�Dt��Du�Du� Du�qDv��Dw�Dw}qDw�qDx� Dy  Dy� Dy�qDz}qDz�qD{� D|  D|� D}�D}� D~  D~��D~�qD� D�HD�@ D�� D�D�HD�@ D�~�D���D�  D�@ D�~�D�� D�HD�>�D�� D�� D�HD�@ D�~�D��qD���D�>�D�� D���D���D�=qD�~�D���D���D�>�D�~�D�� D�HD�AHD�� D�� D���D�@ D�� D��HD�HD�>�D�~�D�� D�HD�@ D�� D��HD�  D�>�D�~�D�� D�HD�@ D�~�D���D�  D�AHD��HD�� D�  D�@ D�� D�� D�  D�>�D�� D�� D���D�>�D�� D��HD�HD�>�D��HD��HD�  D�@ D��HD�� D���D�@ D�~�D��HD�HD�AHD�~�D���D�  D�@ D��HD��HD��D�B�D�~�D���D�HD�>�D�~�D���D���D�AHD�� D���D�HD�C�D�� D���D�HD�AHD�� D�� D�  D�>�D�~�D�� D�  D�AHD�� D�� D�HD�@ D�~�D���D�  D�@ D�� D�� D�  D�AHD��HD��HD�  D�@ D��HD���D���D�>�D�~�D�� D�  D�>�D��HD���D���D�@ D�� D�� D���D�>�D�~�D��qD�  D�@ D�� D���D��qD�>�D�~�D�� D��D�@ D�� D��HD�HD�@ D�� D��HD�  D�>�D�� D�� D�  D�@ D�� D���D�HD�AHD��HD�� D���D�@ D�� D�� D�  D�>�D�~�D��HD���D�@ D�� D��HD�  D�@ D�~�D�� D�HD�>�D�� D�� D���D�>�D�~�D���D���D�=qD�~�D���D�  D�@ D��HD��HD�HD�>�D�~�D�� D�  D�>�D�� D��qD��qD�AHD���D�D�HD�@ D�~�D���D��qD�>�D�� D�� D�HD�B�D��HD��HD�  D�B�D��HD���D��qD�@ D��HD�� D���D�AHD���D��HD���D�=qD�~�D��qD��qD�=qD�~�D��HD��D�>�D�}qD�� D�HD�AHD�� D���D���D�@ D�~�D¾�D�HD�B�DÁHDþ�D���D�@ DĀ D�� D�  D�>�DŁHD�� D��qD�@ D�~�D�� D�  D�>�Dǀ D��HD�  D�@ D�~�D�� D�  D�>�D�}qD�� D�HD�@ D�~�D�� D�HD�>�Dˀ D��HD���D�>�D̀ D̾�D���D�>�D̀ D��HD�  D�@ D�~�D�� D�  D�AHD�~�D�� D�  D�>�DЀ D�� D�HD�@ Dр D�D�  D�AHDҀ D�� D�  D�@ DӀ D��HD�  D�AHDԁHD�� D��qD�>�D�~�Dվ�D���D�@ D�~�D־�D�  D�@ D׀ D׹�>�Q�?8Q�?��?���?Ǯ?�@�@�R@333@G�@Tz�@fff@z�H@���@��@�Q�@�G�@�{@�
=@�  @Ǯ@�33@�p�@�@�\)@�
=AG�AffA(�A  A33A��A\)A$z�A'�A,(�A1G�A7
=A;�A?\)AC�
AH��AO\)AS�
AW�A\(�Aa�Ag
=Aj�HAo\)Atz�Az=qA\)A�=qA�(�A�ffA���A��
A�ffA�Q�A��\A�p�A�  A��HA���A�
=A���A���A�
=A���A�33A�{A���A��HA���A�
=A���A�z�A�
=A�G�A�33A�p�A�  A�33A�p�AϮAљ�A�(�A�\)A�G�AۅA�p�A��A�33A�p�A�A陚A�z�A�
=A��A��
A�A�Q�A�33A�{A��B ��B{B\)B��B{B
=B(�B	G�B
�RB  BG�B{B33B��B{B
=B  Bp�B�HB  B��B�B33Bz�B�B
=B   B!�B"=qB#�B$��B&{B'\)B(Q�B)p�B*ffB+�B-�B.ffB/\)B0Q�B1G�B2�\B3�
B5G�B6=qB7\)B8Q�B9��B;
=B<z�B=p�B>�\B?�
BA�BB�RBC�
BD��BE�BG33BHz�BI�BK
=BL  BMG�BN�RBP(�BQp�BRffBS�BT��BU�BW\)BX��BY��BZ�RB\  B]p�B^�RB_�
B`��Bb=qBc�Bd��Bf{Bg33Bh(�Bip�Bj�HBlz�Bmp�Bn�RBo�
Bqp�Br�HBtQ�BuG�BvffBw�
ByG�Bz�RB|  B|��B~{B�B�z�B��B��B�=qB��HB�p�B�{B��HB��B�(�B���B�33B��
B��\B�G�B��
B�ffB���B�B�z�B���B��B�=qB��HB��B�{B��RB�G�B�{B��RB�33B�B�ffB�
=B��B�ffB��HB�p�B�{B��RB�p�B�{B��RB�\)B��
B�z�B��B��
B��\B�G�B��
B�ffB�
=B��
B�z�B��B��B�(�B���B��B�(�B���B�G�B��
B�ffB��B��
B�ffB���B�p�B�{B���B��B�{B��\B��B��B�z�B�33B�B�Q�B���B�p�B�(�B���B�\)B��
B�ffB�33B��
B�ffB��HB�\)B�{B���B�\)B��
B�=qB���B�G�B�  B��\B���B�\)B��B�(�B�z�B�
=B���B��B�=qB£�B��BÙ�B�{B�z�B���B��BŅB��B�ffB��HB�\)BǮB�{B�Q�Bȣ�B��Bə�B�{B�z�B���B�
=B�p�B��
B�=qḄ�B�33BͅB�B�(�BΏ\B�
=B�p�B��B�Q�BЏ\B��HB�33Bљ�B�{B�z�B���B�G�Bә�B��B�Q�B���B�\)B�B�=qB�z�B���B�G�B�B�=qBظRB�33Bٙ�B��B�=qBڣ�B�
=Bۙ�B�{B�z�B���B�33B݅B��B�ffB���B�p�B��
B�(�B��\B��HB�G�B��
B�Q�B���B�33B㙚B��B�Q�B��B�
=B噚B�  B�z�B���B�p�B�B�(�B��B�
=B�B�{B�RB��B뙚B�{B�ffB��HB�\)B�B�ffB��HB�p�B��
B�(�B��\B�
=B�B�(�B��B�
=B�p�B��
B�=qB���B��B��B�{B��\B�
=B�\)B�B�(�B��\B��B���B�{B���B���B�\)B�B�(�B���B��B��B�=qB��\B���B�p�B��
C 33C p�C �C ��C33CffC��C��C{C\)C��C�HC�C\)C�\CC  CG�C�\C�
C{C=qCz�C�RC  CG�C�\C��C  C33Cp�C�C��C=qC�CC��C	(�C	\)C	��C	�C
33C
p�C
��C
�
C{CffC��C��C
=CQ�C��C�HC�CG�Cz�CC
=CQ�C�\C�RC��C33Cp�C�RC��C33CffC��C�HC(�Cp�C��C��C
=C=qC�\C��C
=C=qCp�C��C�C(�Cp�C�C�
C
=CQ�C�\C��C  C(�CffC�C��C(�C\)C�CC
=CQ�C�C�C�HC(�Cp�C�C�HC
=CG�C�\C��C��C(�CffC�C�C{C=qCz�C�RC  CG�C�C�RC�C(�Cz�CC��C�C\)C��C�HC (�C ffC �C C!{C!\)C!�\C!C!��C"=qC"�C"��C#  C#33C#z�C#C${C$Q�C$�C$�RC%  C%G�C%�\C%��C&  C&33C&p�C&�C'  C'=qC'ffC'��C'�HC(�C(ffC(�C(�C)(�C)\)C)��C)�HC*33C*z�C*�C*�HC+�C+\)C+�C+��C,(�C,\)C,��C,�C-33C-ffC-��C-�
C.�C.ffC.��C.��C/{C/ffC/��C/C/��C0G�C0��C0��C1  C133C1p�C1C2{C2Q�C2�C2C3  C3G�C3��C3�HC4�C4Q�C4�\C4�
C5(�C5ffC5�\C5C6
=C6Q�C6��C6��C7  C7G�C7�\C7�HC8(�C8\)C8��C8�HC933C9�C9C:  C:=qC:�\C:�HC;33C;p�C;��C;�C<=qC<�\C<��C=  C==qC=�\C=�
C>33C>p�C>�C>�C?33C?�\C?�
C@
=C@G�C@��C@�CA=qCAz�CA�RCA��CB=qCB�\CB�CC33CCffCC�CC��CDQ�CD��CD�CE(�CEffCE�CE��CFG�CF��CF�CG(�CGffCG�CG�CH33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                 ?��@   @E�@�G�@��R@�  @�\A   A  A!G�A,(�A@  A`��A���A���A���A�Q�A�  AϮA�  A�  A��B�B�
B  B   B(  B0(�B8  B@  BG�
BP  BW�
B_�
Bg�
Bo�
Bx  B�{B�  B��B�  B��B��
B��
B��B�  B��B��B��
B�  B�{B�  B�{B�{B�{B�  B�  B�{B��
B��B�{B�  B��B��B�  B�  B��B��B��B��C  C  C��C��C

=C��C��C  C��C  C  C  C
=C��C��C   C!��C$  C&  C'�C*
=C,
=C.  C0
=C2{C4
=C6  C8
=C9��C;��C>  C@  CA��CC�CE�CG�CI�CL  CN{CP{CQ��CS��CV  CX
=CY��C[��C]��C`{Ca��Cd  Ce��Ch  Cj{Cl  Cn  Cp
=Cq��Ct  Cv
=Cx  Cz
=C{��C~  C�C�  C�C���C���C���C�  C�  C�  C�  C���C�  C�  C�  C���C�C�C���C���C�  C�  C�C�  C�C���C���C���C�  C�C�C�  C���C�  C�C�C�  C�C�  C���C�  C�  C�  C�  C���C���C���C���C���C�C�C�  C�
=C�  C���C�  C���C�  C�
=C�  C�  C�C�C���C�  C�  C�C�  C���C���C�  C�  C�C�C���C�  C���C�C�  C���C�C�C�  C���C���C�C���C�  C�  C�  C�C�  C�C���C���C�C�  C�
=C�C���C�  C�C�  C�  C���C���C�C�C�C�  C�  C�C�C���C�  C�C���C���C���C�  C�  C���C���C�C�C���C���C�  C�
=D D � D  Dz�D�D��D�D� D��D� D  Dz�D��Dz�D�qD� DD��D	  D	� D	�qD
� D
��D� D��Dz�D�qD��D�D}qD  D��D  D� D�D}qD��D}qD�qDz�D�qD� D�D� D�qDz�D  D� D�qDz�D  D��D  D}qD�qDz�D�D��D  D}qD�qD}qD  D�D   D z�D!  D!��D"  D"� D"�qD#z�D#�qD$}qD%  D%�D&D&� D'  D'��D(�D(}qD(�qD)� D*  D*}qD*�qD+}qD+�qD,z�D,�qD-� D.�D.� D.��D/� D0  D0��D1D1��D2  D2��D3D3��D4�D4� D5�D5� D5�qD6}qD7  D7}qD7��D8xRD8��D9z�D9�qD:� D:�qD;}qD<  D<��D=  D=}qD>  D>� D?�D?� D?�qD@z�D@��DA}qDA��DBz�DC  DC��DD  DD� DD�qDE� DF�DF��DG�DG��DH�DH��DH�qDIz�DI�qDJ� DK  DK}qDL�DL� DL�qDM}qDN  DN}qDN�qDO� DP�DP� DQ  DQ}qDR  DR��DS�DS}qDS��DT}qDU  DU��DV�DV��DW  DW}qDW�qDX��DY�DY}qDY�qDZ}qDZ�qD[� D\  D\� D]  D]��D^�D^� D_�D_}qD_�qD`��DaDa��Db  Db� Dc  Dc� Dc��Ddz�Dd�qDe� Df�Df��Dg�Dg�Dh�Dh��Dh�qDi}qDj  Dj� Dj�qDk� Dl�Dl}qDl�qDm}qDm�qDn��Do�Do��Dp  Dpz�Dp�qDq� Dr  Dr��Ds  Ds��Dt�Dt��Du�Du� Du�qDv��Dw�Dw}qDw�qDx� Dy  Dy� Dy�qDz}qDz�qD{� D|  D|� D}�D}� D~  D~��D~�qD� D�HD�@ D�� D�D�HD�@ D�~�D���D�  D�@ D�~�D�� D�HD�>�D�� D�� D�HD�@ D�~�D��qD���D�>�D�� D���D���D�=qD�~�D���D���D�>�D�~�D�� D�HD�AHD�� D�� D���D�@ D�� D��HD�HD�>�D�~�D�� D�HD�@ D�� D��HD�  D�>�D�~�D�� D�HD�@ D�~�D���D�  D�AHD��HD�� D�  D�@ D�� D�� D�  D�>�D�� D�� D���D�>�D�� D��HD�HD�>�D��HD��HD�  D�@ D��HD�� D���D�@ D�~�D��HD�HD�AHD�~�D���D�  D�@ D��HD��HD��D�B�D�~�D���D�HD�>�D�~�D���D���D�AHD�� D���D�HD�C�D�� D���D�HD�AHD�� D�� D�  D�>�D�~�D�� D�  D�AHD�� D�� D�HD�@ D�~�D���D�  D�@ D�� D�� D�  D�AHD��HD��HD�  D�@ D��HD���D���D�>�D�~�D�� D�  D�>�D��HD���D���D�@ D�� D�� D���D�>�D�~�D��qD�  D�@ D�� D���D��qD�>�D�~�D�� D��D�@ D�� D��HD�HD�@ D�� D��HD�  D�>�D�� D�� D�  D�@ D�� D���D�HD�AHD��HD�� D���D�@ D�� D�� D�  D�>�D�~�D��HD���D�@ D�� D��HD�  D�@ D�~�D�� D�HD�>�D�� D�� D���D�>�D�~�D���D���D�=qD�~�D���D�  D�@ D��HD��HD�HD�>�D�~�D�� D�  D�>�D�� D��qD��qD�AHD���D�D�HD�@ D�~�D���D��qD�>�D�� D�� D�HD�B�D��HD��HD�  D�B�D��HD���D��qD�@ D��HD�� D���D�AHD���D��HD���D�=qD�~�D��qD��qD�=qD�~�D��HD��D�>�D�}qD�� D�HD�AHD�� D���D���D�@ D�~�D¾�D�HD�B�DÁHDþ�D���D�@ DĀ D�� D�  D�>�DŁHD�� D��qD�@ D�~�D�� D�  D�>�Dǀ D��HD�  D�@ D�~�D�� D�  D�>�D�}qD�� D�HD�@ D�~�D�� D�HD�>�Dˀ D��HD���D�>�D̀ D̾�D���D�>�D̀ D��HD�  D�@ D�~�D�� D�  D�AHD�~�D�� D�  D�>�DЀ D�� D�HD�@ Dр D�D�  D�AHDҀ D�� D�  D�@ DӀ D��HD�  D�AHDԁHD�� D��qD�>�D�~�Dվ�D���D�@ D�~�D־�D�  D�@ D׀ G�O�>�Q�?8Q�?��?���?Ǯ?�@�@�R@333@G�@Tz�@fff@z�H@���@��@�Q�@�G�@�{@�
=@�  @Ǯ@�33@�p�@�@�\)@�
=AG�AffA(�A  A33A��A\)A$z�A'�A,(�A1G�A7
=A;�A?\)AC�
AH��AO\)AS�
AW�A\(�Aa�Ag
=Aj�HAo\)Atz�Az=qA\)A�=qA�(�A�ffA���A��
A�ffA�Q�A��\A�p�A�  A��HA���A�
=A���A���A�
=A���A�33A�{A���A��HA���A�
=A���A�z�A�
=A�G�A�33A�p�A�  A�33A�p�AϮAљ�A�(�A�\)A�G�AۅA�p�A��A�33A�p�A�A陚A�z�A�
=A��A��
A�A�Q�A�33A�{A��B ��B{B\)B��B{B
=B(�B	G�B
�RB  BG�B{B33B��B{B
=B  Bp�B�HB  B��B�B33Bz�B�B
=B   B!�B"=qB#�B$��B&{B'\)B(Q�B)p�B*ffB+�B-�B.ffB/\)B0Q�B1G�B2�\B3�
B5G�B6=qB7\)B8Q�B9��B;
=B<z�B=p�B>�\B?�
BA�BB�RBC�
BD��BE�BG33BHz�BI�BK
=BL  BMG�BN�RBP(�BQp�BRffBS�BT��BU�BW\)BX��BY��BZ�RB\  B]p�B^�RB_�
B`��Bb=qBc�Bd��Bf{Bg33Bh(�Bip�Bj�HBlz�Bmp�Bn�RBo�
Bqp�Br�HBtQ�BuG�BvffBw�
ByG�Bz�RB|  B|��B~{B�B�z�B��B��B�=qB��HB�p�B�{B��HB��B�(�B���B�33B��
B��\B�G�B��
B�ffB���B�B�z�B���B��B�=qB��HB��B�{B��RB�G�B�{B��RB�33B�B�ffB�
=B��B�ffB��HB�p�B�{B��RB�p�B�{B��RB�\)B��
B�z�B��B��
B��\B�G�B��
B�ffB�
=B��
B�z�B��B��B�(�B���B��B�(�B���B�G�B��
B�ffB��B��
B�ffB���B�p�B�{B���B��B�{B��\B��B��B�z�B�33B�B�Q�B���B�p�B�(�B���B�\)B��
B�ffB�33B��
B�ffB��HB�\)B�{B���B�\)B��
B�=qB���B�G�B�  B��\B���B�\)B��B�(�B�z�B�
=B���B��B�=qB£�B��BÙ�B�{B�z�B���B��BŅB��B�ffB��HB�\)BǮB�{B�Q�Bȣ�B��Bə�B�{B�z�B���B�
=B�p�B��
B�=qḄ�B�33BͅB�B�(�BΏ\B�
=B�p�B��B�Q�BЏ\B��HB�33Bљ�B�{B�z�B���B�G�Bә�B��B�Q�B���B�\)B�B�=qB�z�B���B�G�B�B�=qBظRB�33Bٙ�B��B�=qBڣ�B�
=Bۙ�B�{B�z�B���B�33B݅B��B�ffB���B�p�B��
B�(�B��\B��HB�G�B��
B�Q�B���B�33B㙚B��B�Q�B��B�
=B噚B�  B�z�B���B�p�B�B�(�B��B�
=B�B�{B�RB��B뙚B�{B�ffB��HB�\)B�B�ffB��HB�p�B��
B�(�B��\B�
=B�B�(�B��B�
=B�p�B��
B�=qB���B��B��B�{B��\B�
=B�\)B�B�(�B��\B��B���B�{B���B���B�\)B�B�(�B���B��B��B�=qB��\B���B�p�B��
C 33C p�C �C ��C33CffC��C��C{C\)C��C�HC�C\)C�\CC  CG�C�\C�
C{C=qCz�C�RC  CG�C�\C��C  C33Cp�C�C��C=qC�CC��C	(�C	\)C	��C	�C
33C
p�C
��C
�
C{CffC��C��C
=CQ�C��C�HC�CG�Cz�CC
=CQ�C�\C�RC��C33Cp�C�RC��C33CffC��C�HC(�Cp�C��C��C
=C=qC�\C��C
=C=qCp�C��C�C(�Cp�C�C�
C
=CQ�C�\C��C  C(�CffC�C��C(�C\)C�CC
=CQ�C�C�C�HC(�Cp�C�C�HC
=CG�C�\C��C��C(�CffC�C�C{C=qCz�C�RC  CG�C�C�RC�C(�Cz�CC��C�C\)C��C�HC (�C ffC �C C!{C!\)C!�\C!C!��C"=qC"�C"��C#  C#33C#z�C#C${C$Q�C$�C$�RC%  C%G�C%�\C%��C&  C&33C&p�C&�C'  C'=qC'ffC'��C'�HC(�C(ffC(�C(�C)(�C)\)C)��C)�HC*33C*z�C*�C*�HC+�C+\)C+�C+��C,(�C,\)C,��C,�C-33C-ffC-��C-�
C.�C.ffC.��C.��C/{C/ffC/��C/C/��C0G�C0��C0��C1  C133C1p�C1C2{C2Q�C2�C2C3  C3G�C3��C3�HC4�C4Q�C4�\C4�
C5(�C5ffC5�\C5C6
=C6Q�C6��C6��C7  C7G�C7�\C7�HC8(�C8\)C8��C8�HC933C9�C9C:  C:=qC:�\C:�HC;33C;p�C;��C;�C<=qC<�\C<��C=  C==qC=�\C=�
C>33C>p�C>�C>�C?33C?�\C?�
C@
=C@G�C@��C@�CA=qCAz�CA�RCA��CB=qCB�\CB�CC33CCffCC�CC��CDQ�CD��CD�CE(�CEffCE�CE��CFG�CF��CF�CG(�CGffCG�CG�CH33G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                 @��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��@��G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�Q�A� �A�{A�1A��A��A��
A���A���A���A���A���A���A���A���A�ȴA�ƨA�ƨA�ƨA�ƨA�ƨA�ȴA�ȴA�ȴA�ĜA׼jA׬A���A��A��A�v�A�dZA�`BA���A���A�|�AэPA��yA��A��A��
A�A�M�AЮAϩ�A�9XA��A���A�+A�I�A��A��yA�\)A��A�oA�A�%A���A��/A���A�v�A��AȸRA�&�A�v�A�jAǋDAȃA��A���A�XA���AǛ�A�?}A��AƍPAŬA���Aġ�A�O�A�  A��A�  A���A��A�S�A��A�`BA�^5A���A��jA�JA��A���A�~�A��A��A��9A�\)A���A�{A�E�A��/A��A��A�n�A��wA�v�A��A���A�;dA��#A�bNA���A�;dA��DA���A�hsA���A�dZA�~�A���A�^5A���A�dZA��HA��A��DA��
A��A��A�-A�
=A��A�7LA��yA��A���A�  A~��A}�FA}C�A|jAz�AxZAwG�Av��At�9As&�Aml�Akt�AjȴAiAhĜAgS�AdE�AcC�Ab�`Ab�9A^JAY�wAW`BAU��ASAP��AN(�AJ9XAH��AG�-AG�AFbAD��AC"�A?�
A<I�A:r�A9;dA7�
A69XA4�A4bA2$�A0r�A-&�A*�jA)��A)33A)
=A(E�A&ĜA&A�A&1A%oA$z�A#?}A"�A!p�A�^AS�A��A�A��AbA��A��A?}A�A�-A��A�A\)A �A�;A�A�AQ�AJAG�A��A"�A�HA`BAE�Az�A�9A(�A;dA
��A
M�A	��A	��A	��A	
=A�A-A�`A��A��A��A��AM�A=qA�A��AhsA �/A v�@�|�@�E�@���@��@��+@�@��@��u@� �@�
=@��^@��/@��@�@�@�Z@�-@�t�@�@�ȴ@�p�@�j@��@�F@�dZ@�
=@�!@�E�@��T@�(�@ݑh@�&�@�j@�+@ج@��m@ׅ@�|�@�t�@׍P@և+@և+@�n�@֏\@�ȴ@�V@�J@�j@Ұ!@��@�j@���@��@��T@�Q�@�@ʟ�@ʏ\@�ff@��T@��@�z�@���@�"�@�"�@Ə\@�v�@���@�O�@�/@�7L@ēu@���@ļj@�9X@�dZ@�@�V@�{@�X@�?}@�7L@�/@���@�bN@�9X@�(�@�(�@�1@�o@�^5@��@��@�b@��;@�ƨ@��@���@��@�\)@�@�@�&�@�Ĝ@�Q�@�1'@�1@�ƨ@��@�33@��@�n�@�{@��T@���@��T@��T@��#@�@��^@��h@�G�@��/@�A�@��;@��F@��P@�t�@�dZ@�"�@��@��!@�J@�&�@��`@�r�@�(�@���@��@���@��R@��\@�=q@��@��@�I�@�b@��w@�33@��y@��+@�J@��T@��@��@���@��@�z�@�z�@�j@�A�@��m@���@�dZ@�+@���@���@��!@�M�@��@���@�/@�%@��@�(�@��w@�S�@���@���@��@���@�ff@�{@�@���@��h@�hs@�X@���@�j@�Q�@� �@��@��w@�t�@�K�@�+@�
=@���@��y@��R@�M�@���@�hs@�/@��/@�Ĝ@�z�@�(�@���@�S�@���@���@�M�@�$�@���@��@�%@��D@�Z@�1'@��@��m@�ƨ@���@�t�@�"�@��@���@�ff@��^@�?}@�&�@��@���@���@�z�@�(�@��;@���@�+@�o@�@���@���@�v�@�{@���@�7L@���@�I�@�(�@� �@��m@���@�t�@�
=@��!@�E�@���@�@��@�?}@���@��@�1'@��m@���@��@���@�|�@�
=@��R@���@���@���@�n�@�^5@�M�@�5?@��@���@��^@�x�@�X@��@��`@��j@�z�@�1'@�  @��w@��@�K�@�;d@��@��y@�n�@�=q@���@��T@��#@��#@���@���@�@���@�hs@��@���@��9@��u@��D@�bN@�1'@��m@��
@���@���@�C�@�+@�+@�@��y@���@��R@��\@�v�@�^5@�M�@�M�@�=q@�5?@��@��@��T@��#@��^@��@�hs@�O�@��@��`@��u@��u@��@�z�@�bN@�Q�@�I�@�9X@�1@�w@|�@;d@~��@~5?@}��@}p�@|Z@{��@{o@z�@zn�@z=q@z�@y�#@y&�@xQ�@x  @w�@w�@w�w@w��@wK�@w
=@v��@v��@v��@v��@w
=@w
=@w
=@w
=@v��@v�@vv�@u�-@u�@u/@t��@t��@t��@tj@tI�@t�@s�
@st�@r��@r^5@q��@q�^@q�7@qhs@qG�@q&�@p��@p �@o��@o;d@n{@l�@lz�@l9X@l1@k�F@kdZ@kS�@j��@i��@i��@i7L@hĜ@h�u@hr�@hQ�@g�;@g
=@fff@f$�@f@e?}@d�D@c��@c�F@c��@c��@b�\@bJ@a�#@a��@ax�@`�9@_��@_K�@_
=@^�R@^E�@^@]?}@[33@Zn�@Y��@YX@Y&�@X�9@X1'@W�@W�P@W
=@V$�@U@U�h@UO�@U�@UV@T�/@Tj@S�F@R�H@R�\@R^5@R�@Q��@Q�@Pb@O\)@N��@N{@M�h@L�j@L1@K�@KC�@K33@Ko@K@J�H@J~�@I��@H  @G�@G��@G\)@G�@G
=@Fv�@F$�@F@E�@E/@D9X@Ct�@B��@A��@Ax�@A7L@@��@@bN@?�@?��@?�P@?;d@>��@>��@>E�@>@=�h@=O�@=V@<�@<�D@<j@<Z@<Z@<I�@<9X@<�@;��@;��@;@:�\@:-@9��@9G�@8Ĝ@81'@8  @7��@6V@6@5�@5�T@5��@5/@4I�@3�
@333@3o@2�@2��@2�\@2~�@2^5@2-@1��@1�^@1�7@1x�@1G�@1%@0�u@0r�@01'@0b@0  @0  @/�;@/��@/K�@/;d@/;d@/+@/
=@.��@.�y@.ȴ@.ȴ@.�R@.�R@.��@.v�@.5?@-�-@,�/@,�D@,(�@,(�@,(�@,(�@,�@,�@,1@+�m@+�
@+�F@+�F@+dZ@+33@+@*~�@*^5@*^5@*^5@*M�@)G�@(1'@'�@'��@'l�@'l�@'\)@'\)@'K�@'K�@'�@'�@&ȴ@&��@&��@&�+@&��A�XA�XA�ZA�E�A�(�A�$�A��A��A�$�A�VA�1A�
=A�1A�%A�A��A��TA��HA��/A���A���A���A��
A��
A���A���A���A���A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA���A���A���A���A�ƨA�ȴA�ȴA���A�ȴA�ƨA�ƨA���A�ȴA�ĜA�ƨA���A���A�ȴA�ĜA�A�ĜA�ƨA�ȴA�ƨA�ĜA�A�ĜA�ȴA�ȴA�ƨA�ĜA�ƨA�ȴA�ƨA�ƨA�ĜA�ȴA���A�ȴA�ƨA�ĜA�ƨA�ȴA�ȴA�ȴA�ĜA�ƨA�ȴA���A�ȴA�ƨA�ĜA�ƨA�ȴA���A�ƨA�ĜA�ƨA�ȴA���A�ȴA�ĜA�ƨA�ȴA���A�ȴA�A�ƨA�ȴA�ȴA�ĜA�A�A׾wA���A���A׾wA׺^A׸RA׺^A׸RA׶FA׼jA׶FAײ-Aכ�A�x�A�l�A�`BA��`A֡�AցA�Q�A�A�~�A�S�A��A�VA�AԁA�M�A��A�A��/Aө�AӋDAӉ7AӅA�x�A�z�A�v�A�r�A�hsA�bNA�bNA�bNA�ffA�ffA�bNA�`BA�`BA�bNA�dZA�bNA�\)A�XA�VA�O�A��A��`A�x�A�^5A�G�A�1'A�"�A�A��A��
AѺ^Aѡ�Aч+A�t�A�p�A�x�A�z�A�dZA�ffA�`BAхAѮAѾwA���A���A���A��A���A���A���A��A��A��yA��mA��mA��yA��mA��yA��A��A��A��A��yA��;A��
A��A��
A���A���A���A���A���A�ȴA���Aѩ�Aя\A�v�A�`BA�I�A�C�A�-A���A��`A���AоwAв-AХ�A�ffA��yA���Aϴ9Aϰ!Aϰ!Aϟ�A�1'A�ĜA�`BA�
=A���A���A���A���A���A��A��A��A��A��A��A��A��TA�ȴAʹ9A͛�A�|�A�XA�A�A�-A�A�Ả7A�n�A�Q�A�A�A�9XA�-A�&�A�"�A��A��A��A��A�
=A�A���A���A��A��mA��;A��
A���A˰!A˅A�Q�A�5?A�$�A��A� �A��A��A��A�{A�oA�oA�{A�{A��A�oA�oA�{A��A��A��A�{A�JA�
=A�A�A�A�A�A�A�  A�  A�A�A�%A�%A�%A�A�A�A�A�%A�1A�%A�A�  A�  A�  A�A�A�A���A���A���A���A���A���A���A��A��yA��;A���A�ĜAʶFAʥ�Aʙ�A�ffA�9XA�%A��/A�ȴAɺ^Aɧ�Aə�Aɏ\Aɉ7Aɇ+AɁA�~�A�|�A�|�A�v�A�l�A�`BA�G�A�$�A��A��A��HA��#A��A���A���A���A���A���A���A�ȴA���AȼjAȲ-Aȟ�AȅA�v�A�n�A�ffA�I�A�5?A�VA�  A��HA���Aǥ�AǋDA�x�A�r�A�l�A�jA�n�A�jA�jA�hsA�bNA�dZA�hsA�ffA�p�A�r�A�t�A�x�AǋDAǍPAǓuAǍPAǉ7AǁAǃAǙ�AǴ9A���A��A�{A�7LAȣ�A��A�K�A�M�Aɧ�A���A�1A�$�A�7LA�=qA�=qA�9XA�33A�/A�+A�&�A�oA��A��
A���A���A�ȴA���AɶFAɝ�Aɇ+A�|�A�7LA��Aȝ�A�^5A�$�A�JA��A��`A��;A��/A���A�ƨAǼjAǡ�AǛ�AǙ�AǕ�Aǉ7A�v�A�l�A�bNA�XA�O�A�E�A�1'A�"�A��A��A�VA�
=A�  A��A��A��A��;A�ȴA���A�ĜA���AƸRAƙ�A�p�A�9XA��A�  A��A���A�ĜAŲ-Aŕ�AŅA�VA�-A��A��A���A���A��A��mA���A���A�ȴA���AĮAģ�AđhAċDAċDAć+AăA�|�A�r�A�dZA�VA�9XA�33A�1'A��A��A�oA�oA�%A�A���A��A��A���AÁA��#A�VA��A�A��A���A��PA��+A�t�A�bNA�S�A�33A��A��A��!A�dZA�%A���A�|�A��A���A���A�bNA�1A�(�A��A��/A��A��uA�S�A���A�A���A�O�A�bA��yA��A���A�\)A�oA��A�p�A�bNA�A�A�&�A�A��yA���A�/A��A���A��RA���A�|�A�jA�VA� �A���A���A��A��A�hsA�O�A�=qA�7LA�(�A�(�A��A��A���A��-A�z�A�C�A��A��A��hA���A��jA��9A��A��+A�`BA�I�A�5?A�(�A�"�A��A�A���A��A��;A���A��-A���A��+A�v�A�bNA�Q�A�E�A�=qA�-A��A���A��A��A��A��`A��PA��A��yA��^A���A�|�A�1'A�A��9A��A�~�A�G�A� �A��A���A�O�A��A��-A�~�A�M�A��A��TA���A��A�JA���A��A��-A��9A��hA��A��+A��A�|�A�ffA�E�A�S�A�M�A�/A���A�ƨA���A���A���A��A�z�A�n�A�ffA�XA�G�A�"�A��HA���A�|�A�XA�O�A�M�A�G�A�A�A�/A�"�A��A�A��`A��A���A��9A��7A�(�A��;A���A�K�A��A��A�%A��wA�ƨA�-A�+A�$�A��A��A���A��A�t�A�hsA�`BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                 A�Q�A� �A�{A�1A��A��A��
A���A���A���A���A���A���A���A���A�ȴA�ƨA�ƨA�ƨA�ƨA�ƨA�ȴA�ȴA�ȴA�ĜA׼jA׬A���A��A��A�v�A�dZA�`BA���A���A�|�AэPA��yA��A��A��
A�A�M�AЮAϩ�A�9XA��A���A�+A�I�A��A��yA�\)A��A�oA�A�%A���A��/A���A�v�A��AȸRA�&�A�v�A�jAǋDAȃA��A���A�XA���AǛ�A�?}A��AƍPAŬA���Aġ�A�O�A�  A��A�  A���A��A�S�A��A�`BA�^5A���A��jA�JA��A���A�~�A��A��A��9A�\)A���A�{A�E�A��/A��A��A�n�A��wA�v�A��A���A�;dA��#A�bNA���A�;dA��DA���A�hsA���A�dZA�~�A���A�^5A���A�dZA��HA��A��DA��
A��A��A�-A�
=A��A�7LA��yA��A���A�  A~��A}�FA}C�A|jAz�AxZAwG�Av��At�9As&�Aml�Akt�AjȴAiAhĜAgS�AdE�AcC�Ab�`Ab�9A^JAY�wAW`BAU��ASAP��AN(�AJ9XAH��AG�-AG�AFbAD��AC"�A?�
A<I�A:r�A9;dA7�
A69XA4�A4bA2$�A0r�A-&�A*�jA)��A)33A)
=A(E�A&ĜA&A�A&1A%oA$z�A#?}A"�A!p�A�^AS�A��A�A��AbA��A��A?}A�A�-A��A�A\)A �A�;A�A�AQ�AJAG�A��A"�A�HA`BAE�Az�A�9A(�A;dA
��A
M�A	��A	��A	��A	
=A�A-A�`A��A��A��A��AM�A=qA�A��AhsA �/A v�@�|�@�E�@���@��@��+@�@��@��u@� �@�
=@��^@��/@��@�@�@�Z@�-@�t�@�@�ȴ@�p�@�j@��@�F@�dZ@�
=@�!@�E�@��T@�(�@ݑh@�&�@�j@�+@ج@��m@ׅ@�|�@�t�@׍P@և+@և+@�n�@֏\@�ȴ@�V@�J@�j@Ұ!@��@�j@���@��@��T@�Q�@�@ʟ�@ʏ\@�ff@��T@��@�z�@���@�"�@�"�@Ə\@�v�@���@�O�@�/@�7L@ēu@���@ļj@�9X@�dZ@�@�V@�{@�X@�?}@�7L@�/@���@�bN@�9X@�(�@�(�@�1@�o@�^5@��@��@�b@��;@�ƨ@��@���@��@�\)@�@�@�&�@�Ĝ@�Q�@�1'@�1@�ƨ@��@�33@��@�n�@�{@��T@���@��T@��T@��#@�@��^@��h@�G�@��/@�A�@��;@��F@��P@�t�@�dZ@�"�@��@��!@�J@�&�@��`@�r�@�(�@���@��@���@��R@��\@�=q@��@��@�I�@�b@��w@�33@��y@��+@�J@��T@��@��@���@��@�z�@�z�@�j@�A�@��m@���@�dZ@�+@���@���@��!@�M�@��@���@�/@�%@��@�(�@��w@�S�@���@���@��@���@�ff@�{@�@���@��h@�hs@�X@���@�j@�Q�@� �@��@��w@�t�@�K�@�+@�
=@���@��y@��R@�M�@���@�hs@�/@��/@�Ĝ@�z�@�(�@���@�S�@���@���@�M�@�$�@���@��@�%@��D@�Z@�1'@��@��m@�ƨ@���@�t�@�"�@��@���@�ff@��^@�?}@�&�@��@���@���@�z�@�(�@��;@���@�+@�o@�@���@���@�v�@�{@���@�7L@���@�I�@�(�@� �@��m@���@�t�@�
=@��!@�E�@���@�@��@�?}@���@��@�1'@��m@���@��@���@�|�@�
=@��R@���@���@���@�n�@�^5@�M�@�5?@��@���@��^@�x�@�X@��@��`@��j@�z�@�1'@�  @��w@��@�K�@�;d@��@��y@�n�@�=q@���@��T@��#@��#@���@���@�@���@�hs@��@���@��9@��u@��D@�bN@�1'@��m@��
@���@���@�C�@�+@�+@�@��y@���@��R@��\@�v�@�^5@�M�@�M�@�=q@�5?@��@��@��T@��#@��^@��@�hs@�O�@��@��`@��u@��u@��@�z�@�bN@�Q�@�I�@�9X@�1@�w@|�@;d@~��@~5?@}��@}p�@|Z@{��@{o@z�@zn�@z=q@z�@y�#@y&�@xQ�@x  @w�@w�@w�w@w��@wK�@w
=@v��@v��@v��@v��@w
=@w
=@w
=@w
=@v��@v�@vv�@u�-@u�@u/@t��@t��@t��@tj@tI�@t�@s�
@st�@r��@r^5@q��@q�^@q�7@qhs@qG�@q&�@p��@p �@o��@o;d@n{@l�@lz�@l9X@l1@k�F@kdZ@kS�@j��@i��@i��@i7L@hĜ@h�u@hr�@hQ�@g�;@g
=@fff@f$�@f@e?}@d�D@c��@c�F@c��@c��@b�\@bJ@a�#@a��@ax�@`�9@_��@_K�@_
=@^�R@^E�@^@]?}@[33@Zn�@Y��@YX@Y&�@X�9@X1'@W�@W�P@W
=@V$�@U@U�h@UO�@U�@UV@T�/@Tj@S�F@R�H@R�\@R^5@R�@Q��@Q�@Pb@O\)@N��@N{@M�h@L�j@L1@K�@KC�@K33@Ko@K@J�H@J~�@I��@H  @G�@G��@G\)@G�@G
=@Fv�@F$�@F@E�@E/@D9X@Ct�@B��@A��@Ax�@A7L@@��@@bN@?�@?��@?�P@?;d@>��@>��@>E�@>@=�h@=O�@=V@<�@<�D@<j@<Z@<Z@<I�@<9X@<�@;��@;��@;@:�\@:-@9��@9G�@8Ĝ@81'@8  @7��@6V@6@5�@5�T@5��@5/@4I�@3�
@333@3o@2�@2��@2�\@2~�@2^5@2-@1��@1�^@1�7@1x�@1G�@1%@0�u@0r�@01'@0b@0  @0  @/�;@/��@/K�@/;d@/;d@/+@/
=@.��@.�y@.ȴ@.ȴ@.�R@.�R@.��@.v�@.5?@-�-@,�/@,�D@,(�@,(�@,(�@,(�@,�@,�@,1@+�m@+�
@+�F@+�F@+dZ@+33@+@*~�@*^5@*^5@*^5@*M�@)G�@(1'@'�@'��@'l�@'l�@'\)@'\)@'K�@'K�@'�@'�@&ȴ@&��@&��@&�+G�O�A�XA�XA�ZA�E�A�(�A�$�A��A��A�$�A�VA�1A�
=A�1A�%A�A��A��TA��HA��/A���A���A���A��
A��
A���A���A���A���A��
A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A���A�ȴA�ȴA���A���A���A���A�ƨA�ȴA�ȴA���A�ȴA�ƨA�ƨA���A�ȴA�ĜA�ƨA���A���A�ȴA�ĜA�A�ĜA�ƨA�ȴA�ƨA�ĜA�A�ĜA�ȴA�ȴA�ƨA�ĜA�ƨA�ȴA�ƨA�ƨA�ĜA�ȴA���A�ȴA�ƨA�ĜA�ƨA�ȴA�ȴA�ȴA�ĜA�ƨA�ȴA���A�ȴA�ƨA�ĜA�ƨA�ȴA���A�ƨA�ĜA�ƨA�ȴA���A�ȴA�ĜA�ƨA�ȴA���A�ȴA�A�ƨA�ȴA�ȴA�ĜA�A�A׾wA���A���A׾wA׺^A׸RA׺^A׸RA׶FA׼jA׶FAײ-Aכ�A�x�A�l�A�`BA��`A֡�AցA�Q�A�A�~�A�S�A��A�VA�AԁA�M�A��A�A��/Aө�AӋDAӉ7AӅA�x�A�z�A�v�A�r�A�hsA�bNA�bNA�bNA�ffA�ffA�bNA�`BA�`BA�bNA�dZA�bNA�\)A�XA�VA�O�A��A��`A�x�A�^5A�G�A�1'A�"�A�A��A��
AѺ^Aѡ�Aч+A�t�A�p�A�x�A�z�A�dZA�ffA�`BAхAѮAѾwA���A���A���A��A���A���A���A��A��A��yA��mA��mA��yA��mA��yA��A��A��A��A��yA��;A��
A��A��
A���A���A���A���A���A�ȴA���Aѩ�Aя\A�v�A�`BA�I�A�C�A�-A���A��`A���AоwAв-AХ�A�ffA��yA���Aϴ9Aϰ!Aϰ!Aϟ�A�1'A�ĜA�`BA�
=A���A���A���A���A���A��A��A��A��A��A��A��A��TA�ȴAʹ9A͛�A�|�A�XA�A�A�-A�A�Ả7A�n�A�Q�A�A�A�9XA�-A�&�A�"�A��A��A��A��A�
=A�A���A���A��A��mA��;A��
A���A˰!A˅A�Q�A�5?A�$�A��A� �A��A��A��A�{A�oA�oA�{A�{A��A�oA�oA�{A��A��A��A�{A�JA�
=A�A�A�A�A�A�A�  A�  A�A�A�%A�%A�%A�A�A�A�A�%A�1A�%A�A�  A�  A�  A�A�A�A���A���A���A���A���A���A���A��A��yA��;A���A�ĜAʶFAʥ�Aʙ�A�ffA�9XA�%A��/A�ȴAɺ^Aɧ�Aə�Aɏ\Aɉ7Aɇ+AɁA�~�A�|�A�|�A�v�A�l�A�`BA�G�A�$�A��A��A��HA��#A��A���A���A���A���A���A���A�ȴA���AȼjAȲ-Aȟ�AȅA�v�A�n�A�ffA�I�A�5?A�VA�  A��HA���Aǥ�AǋDA�x�A�r�A�l�A�jA�n�A�jA�jA�hsA�bNA�dZA�hsA�ffA�p�A�r�A�t�A�x�AǋDAǍPAǓuAǍPAǉ7AǁAǃAǙ�AǴ9A���A��A�{A�7LAȣ�A��A�K�A�M�Aɧ�A���A�1A�$�A�7LA�=qA�=qA�9XA�33A�/A�+A�&�A�oA��A��
A���A���A�ȴA���AɶFAɝ�Aɇ+A�|�A�7LA��Aȝ�A�^5A�$�A�JA��A��`A��;A��/A���A�ƨAǼjAǡ�AǛ�AǙ�AǕ�Aǉ7A�v�A�l�A�bNA�XA�O�A�E�A�1'A�"�A��A��A�VA�
=A�  A��A��A��A��;A�ȴA���A�ĜA���AƸRAƙ�A�p�A�9XA��A�  A��A���A�ĜAŲ-Aŕ�AŅA�VA�-A��A��A���A���A��A��mA���A���A�ȴA���AĮAģ�AđhAċDAċDAć+AăA�|�A�r�A�dZA�VA�9XA�33A�1'A��A��A�oA�oA�%A�A���A��A��A���AÁA��#A�VA��A�A��A���A��PA��+A�t�A�bNA�S�A�33A��A��A��!A�dZA�%A���A�|�A��A���A���A�bNA�1A�(�A��A��/A��A��uA�S�A���A�A���A�O�A�bA��yA��A���A�\)A�oA��A�p�A�bNA�A�A�&�A�A��yA���A�/A��A���A��RA���A�|�A�jA�VA� �A���A���A��A��A�hsA�O�A�=qA�7LA�(�A�(�A��A��A���A��-A�z�A�C�A��A��A��hA���A��jA��9A��A��+A�`BA�I�A�5?A�(�A�"�A��A�A���A��A��;A���A��-A���A��+A�v�A�bNA�Q�A�E�A�=qA�-A��A���A��A��A��A��`A��PA��A��yA��^A���A�|�A�1'A�A��9A��A�~�A�G�A� �A��A���A�O�A��A��-A�~�A�M�A��A��TA���A��A�JA���A��A��-A��9A��hA��A��+A��A�|�A�ffA�E�A�S�A�M�A�/A���A�ƨA���A���A���A��A�z�A�n�A�ffA�XA�G�A�"�A��HA���A�|�A�XA�O�A�M�A�G�A�A�A�/A�"�A��A�A��`A��A���A��9A��7A�(�A��;A���A�K�A��A��A�%A��wA�ƨA�-A�+A�$�A��A��A���A��A�t�A�hsA�`BG�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111411111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                 ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B
\�B
^�B
[�B
[#B
[�B
[#B
Z�B
[#B
Z�B
Z�B
[�B
Z�B
[#B
[#B
[#B
Z�B
[WB
[�B
[WB
[#B
\)B
\)B
[�B
[WB
[#B
Y�B
XyB
R�B
@�B
9$B
5�B
5?B
5B
9�B
;0B
I�B
v�B
�xB
��B
�UB
��B
��B
��B
�B
��B
o�B
\�B
YKB
X�B
R�B
MB
LdB
R�B
S�B
V�B
YB
[�B
_pB
o B
yrB
v�B
}�B
�4B
��B
��B
��B
ݘB3�B�WB�B,BYKBb�Bk�Bs�B~(B��B�FB�CB�'B�UB�Bm)BQ�B�B	�B�B��B��B��B��B�ZB�B�B�;B��B�pBҽB�B�6BǮB�B��B��B��B��B��B�7B��B�GB��Bk�BcTBYKBOvB8B%�B �B�BVB�B �B
�JB
�PB
�B
�BB
͟B
�}B
�B
�XB
��B
�qB
��B
��B
��B
��B
��B
��B
{JB
uZB
t�B
r|B
r|B
lWB
U�B
GzB
>�B
1�B
 \B	�fB	�9B	�[B	�B	��B	��B	��B	�1B	�B	��B	�SB	g�B	X�B	U�B	P�B	A B	>BB	2�B	+B	)_B	!�B	=B	�B	JB	VB	{B��B�%B��B��B�DB�B� B�BB�mBΥBɆBʌBȴB�B�9BBB��B��B��B��B�wB�BB�B�#B�B�BB��B��BĜB�XB��B��B�B�EB��B�DB�	B��B�B�B�;B��B�jB�BȀB�vB��B�ZB	�B	_B	
�B		�B	
=B	
	B	
	B		�B	�B	�B	{B��B�8B�+B��B	  B	;B	�B	;B	;B	uB	{B	�B	
�B	xB	�B	�B	B	~B	�B	1B		�B	fB	_B	
	B	
�B		7B	�B	{B�"B�]B��B	B	�B	.B	�B	@B	uB	�B	B	4B	B	
rB	�B	�B	B	  B��B��B�VB	 4B	�B		lB	DB	B	�B	 B	�B	�B	�B	"B	�B	B	kB	7B	B	B	�B	$@B	+B	,B	.B	2-B	3�B	7LB	;0B	>wB	@�B	@�B	D�B	DgB	F�B	K�B	MjB	P�B	U�B	W�B	[�B	\�B	^5B	_;B	bB	c B	c�B	dZB	g�B	jB	sMB	w�B	y	B	y�B	~(B	~�B	��B	��B	��B	�PB	��B	�.B	��B	��B	��B	�uB	��B	�bB	��B	�nB	�:B	��B	��B	�B	�FB	�B	��B	�eB	��B	�CB	��B	��B	��B	�[B	�B	�zB	��B	�*B	�B	�qB	�qB	�<B	��B	��B	�OB	B	�9B	��B	�B	�BB	��B	ѷB	��B	�mB	֡B	֡B	��B	רB	��B	�;B	ߤB	�HB	�B	� B	��B	�2B	�B	�B	�B	�B	��B	�B	�QB	�B	�B	�)B	�5B	��B	�B	��B	�GB	�|B	�B	�B	�B	��B	��B	��B	�`B	�fB	�B	�>B	�B	�rB	��B	�xB	��B	�B	��B	��B	�B	�PB	��B	��B	��B	�(B	��B	��B	��B
 4B
 �B
B
;B
oB
oB
B
B
B
MB
�B
SB
B
�B
%B
�B
�B
fB
�B
	�B
	�B
	�B

�B
B
xB
DB
xB
DB
DB
�B
JB
JB
�B
VB
"B
�B
bB
 B
4B
�B
�B
4B
�B
�B
oB
�B
{B
B
B
�B
�B
MB
�B
SB
�B
_B
1B
1B
�B
�B
�B
B
	B
=B
CB
xB
�B
B
�B
�B
B
!B
!B
!B
�B
VB
�B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
 �B
!-B
!�B
!�B
!�B
"hB
"hB
"�B
"�B
"�B
#:B
#�B
#�B
$@B
$tB
$tB
$@B
$�B
$�B
%zB
%FB
%�B
%�B
%�B
%�B
%�B
%�B
%�B
%�B
&B
'B
&�B
'B
'B
'B
'RB
'�B
(XB
'�B
'�B
'�B
(�B
(�B
)*B
)*B
)�B
)�B
)�B
*0B
*eB
*0B
*�B
*�B
*eB
*�B
*�B
*�B
+B
*�B
+6B
+6B
+6B
+kB
,B
,qB
-CB
,�B
-CB
-B
-CB
-wB
-wB
-�B
-�B
-�B
.B
.IB
.}B
/�B
/OB
/�B
1[B
1�B
1�B
1�B
2aB
2�B
2aB
2aB
33B
3�B
3�B
4B
3�B
3�B
3�B
49B
4B
4B
3�B
4B
3�B
3�B
4B
4B
3�B
3�B
3�B
4�B
5B
4�B
5tB
5tB
5�B
5tB
6B
5�B
5�B
6B
6FB
7LB
7�B
7�B
7�B
8B
7�B
7�B
7�B
7�B
8�B
8�B
8�B
:*B
:�B
:�B
;dB
;�B
<B
;�B
;�B
<jB
=<B
=<B
=�B
=�B
>B
>BB
>B
>�B
?B
?�B
?}B
?}B
@�B
@�B
A�B
A�B
A�B
AUB
B�B
B[B
B�B
B�B
B�B
C�B
DgB
D3B
DgB
DgB
D�B
C�B
EB
FtB
F�B
F�B
GEB
F�B
GB
G�B
GzB
G�B
HB
H�B
IB
I�B
I�B
I�B
I�B
I�B
JXB
J�B
K�B
K�B
LdB
L0B
L�B
MB
NB
NB
OB
N�B
O�B
P}B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
P�B
QB
S&B
R�B
R�B
R�B
R�B
R�B
S�B
S[B
S[B
S&B
T,B
U2B
U�B
VmB
WsB
WsB
W�B
XB
X�B
YB
YKB
YB
YB
ZB
ZQB
Z�B
Z�B
[WB
[WB
[�B
[�B
[�B
\)B
\)B
[�B
\)B
\)B
\)B
[�B
\�B
]/B
]/B
]�B
^B
^5B
^5B
_B
^�B
_;B
`�B
`vB
`�B
`vB
`�B
aHB
bB
b�B
b�B
b�B
b�B
cTB
b�B
cTB
c�B
c�B
c�B
d&B
d&B
d&B
dZB
d�B
e,B
e,B
e�B
e�B
e�B
e�B
e�B
f2B
f2B
f2B
ffB
ffB
ffB
ffB
f�B
f�B
f�B
f�B
f�B
f�B
f�B
gB
g�B
h
B
h�B
h�B
h�B
iB
h�B
h�B
iB
h�B
iDB
iDB
iDB
iB
iDB
i�B
jB
jKB
jKB
jKB
jB
i�B
k�B
l"B
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
m�B
m]B
m]B
m�B
m)B
_;B
[�B
\]B
_;B
b�B
_�B
\)B
Y�B
\�B
[�B
\)B
[#B
Z�B
Y�B
Y�B
^�B
[�B
Z�B
Z�B
\�B
[�B
Z�B
Z�B
Y�B
[�B
[�B
Z�B
ZB
ZB
[#B
\)B
[#B
ZQB
Y�B
[�B
\)B
[#B
Z�B
Y�B
[WB
\]B
[�B
Y�B
Z�B
[�B
[�B
ZQB
ZQB
Z�B
\]B
[�B
Z�B
Z�B
Z�B
[WB
\)B
[#B
Y�B
Y�B
[#B
\�B
[�B
Z�B
ZB
[WB
[#B
[�B
ZQB
Z�B
\]B
[WB
ZB
ZQB
Z�B
\]B
\�B
\)B
[�B
ZQB
[WB
[�B
\�B
[WB
Z�B
Z�B
[�B
\]B
[�B
Z�B
Z�B
[�B
\)B
Z�B
ZQB
Z�B
[�B
\�B
\]B
[�B
[#B
[�B
\�B
\�B
[�B
[WB
[�B
\�B
]/B
\�B
[WB
Z�B
\]B
]/B
\)B
[#B
Z�B
[#B
\]B
\)B
[#B
ZB
[WB
\]B
[WB
ZB
ZB
[�B
[�B
[�B
[�B
YKB
YB
ZQB
Z�B
Z�B
Y�B
Y�B
WsB
V�B
X�B
XEB
W�B
YB
P�B
R�B
i�B
N�B
L�B
OB
R B
V9B
=qB
?�B
7�B
:�B
B�B
8�B
9�B
9XB
<B
7�B
3hB
3�B
5�B
8�B
5B
49B
3�B
6B
6FB
6FB
5tB
4nB
49B
4�B
5�B
5�B
5B
3hB
4�B
5?B
5tB
5�B
5B
8RB
8B
@B
9�B
8B
6zB
6zB
;dB
;0B
<6B
=qB
=<B
=�B
B�B
C�B
P�B
\]B
]�B
a|B
iDB
pB
��B
�xB
��B
�_B
�YB
��B
��B
��B
�:B
�B
��B
�B
�_B
�$B
�XB
�_B
��B
��B
�hB
�B
��B
��B
�B
�RB
��B
��B
��B
�RB
�B
�FB
��B
�B
��B
�0B
�0B
��B
�dB
�qB
��B
��B
��B
��B
��B
��B
��B
�}B
�B
�6B
��B
��B
�B
�$B
��B
�\B
�PB
zB
g�B
^jB
`BB
[�B
\]B
[�B
\)B
]/B
^B
\�B
Y�B
X�B
ZB
Z�B
Z�B
VmB
U�B
UgB
Z�B
V9B
S�B
Y�B
cTB
]�B
Y�B
YB
P�B
P�B
N<B
L0B
M�B
M�B
M6B
L0B
K)B
NB
MjB
M6B
K�B
K�B
L0B
K�B
M6B
MjB
S&B
R�B
U2B
U2B
T,B
S&B
R B
Q�B
RTB
T,B
T�B
U�B
U2B
T�B
S�B
TaB
V9B
W
B
VmB
U�B
UgB
U�B
V�B
YB
X�B
YKB
X�B
XB
W�B
XB
XyB
ZQB
ZQB
Y�B
YB
XEB
YKB
[WB
\]B
\�B
\]B
\�B
\�B
\)B
\�B
]�B
^�B
^5B
^�B
]dB
]/B
]/B
^jB
^�B
c B
f2B
e�B
gmB
j�B
l�B
pB
qAB
tB
tTB
tB
sB
t�B
zDB
z�B
}�B
{B
yrB
y�B
z�B
y	B
x�B
x�B
w�B
w2B
v+B
tTB
t�B
uZB
v`B
v�B
zDB
{JB
cB
}VB
}VB
}�B
}�B
.B
~�B
~(B
}VB
}"B
}"B
}�B
.B
.B
�;B
��B
�YB
�_B
�+B
�+B
�PB
��B
�B
�MB
�	B
��B
��B
�LB
��B
�B
��B
�0B
�}B
�!B
��B
�-B
��B
�aB
�B
��B
�HB
��B
�aB
�<B
�HB
�#B
�5B
�HB
�BB
�vB
�B
��B
��B
��B�BB�B%BT,B�hB�SBx�B�BԕB�B�8B��B��B��B�B�`B�B�VB	7B@BB1B�B�B�B�B"hB%FB%zB5tB>BI�BPBS�BYKB[#B[#B[WBZ�B^�B]/B`Be�BbBbNBb�Bc�Be�BgmBh�BjBi�BlWBm]Bm�Bm�Bo5Bp;Bq�BsMBr�BqBqvBv�Bz�ByrBw�By>Bz�B�;B��B�fB�MB�B��B�JB�JB��B�B��B�~B��B��B�\B��B��B�@B��B�kB��B��B�_B��B��B�!B��B�}B�B��B��B��B�OB�-B��B��B�[B�aB��B��B�'B��B��B��B�!B��B��B�6BĜB��B��B~(B}VB~]Bx8BxlBzBncBu�Bl"BgmBiDB^Bn�Bg�BT�BJ�BK�BF�BM�BOBBR�BaHBR�B"�B�BB�B1[BYB�B�B�B�B	�B�BBPBDBuB�BBB��BB�B�B(B�B��B��B��B�	B��B�B��B�>B�	B��B�+B�+B��B�ZB��B�AB�B��B�B�8B��B��B�+B�B	7B1B�B�2B��B��B��B�B�fB�%B��B�B��B�B��B��B�B��B��B�`B�B�B�B�B�B�B�B�B�B�B��B�cB�rB�B��B��B��B�cB�8B�B��B�yB�GB�cB�B�B�B�B�B�pBݘB�B՛BרB�QBѷB��B�B�BB�NB��BԕBҽB�B��B��B�,B̘B�RB�TB�mB��B��B�6B�)B�?BɺB�BȴB�zB�B�zB��BϫB�aB�mB��B��B�B��B�qB��B��B�XB��B��B��B��B�B�B�B�LB��B��B�IB��B��B�?B�aB�!B��B��B��B��B��B�{B�B��B��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�4444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                 G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202007072154312020070721543120200707215431202007072154312020070721543120200707215431SI  SI  ARFMARFM                                                                                                                                                2020060222235420200602222354IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020061300003520200613000035QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2020061300003520200613000035QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�0               0               SI  SI  ARFMARFM                                                                                                                                                2020070612390620200706123906IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI      ARSQ    SIQC    V2.1                                                                                                                                    20200707214335              CF      PSAL                            ?��G�O�D׹�G�O�?�  G�O�Bad PSAL Drift                      SI      ARSQ    SIQC    V2.1                                                                                                                                              20200707214831    CF                  PSAL            G�O�>�Q�G�O�CH33G�O�?�                  Bad PSAL Drift  SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2020070721551920200707215519IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOW  OW  V1.0V1.0CTD_for_DMQC_2020V01                                            CTD_for_DMQC_2020V01                                            2020070721551920200707215519IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2020070721551920200707215519IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                