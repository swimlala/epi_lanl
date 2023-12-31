CDF      
      	DATE_TIME         STRING2       STRING4       STRING8       STRING16      STRING32       STRING64   @   	STRING256         N_PROF        N_PARAM       N_LEVELS  �   N_CALIB       	N_HISTORY                title         Argo float vertical profile    institution       #Scripps Institution of Oceanography    source        
Argo float     history       :2021-03-05T16:36:02Z creation; 2021-10-15T19:29:27Z DMQC;      
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
_FillValue        G�O�     �  =   PRES_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  [�   PRES_ADJUSTED            
      
   	long_name         )Sea water pressure, equals 0 at sea-level      standard_name         sea_water_pressure     units         decibar    	valid_min                	valid_max         F;�    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   axis      Z      
_FillValue        G�O�     �  cH   PRES_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   PRES_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         decibar    C_format      %7.2f      FORTRAN_format        F7.2   
resolution        =#�
   
_FillValue        G�O�     �  ��   TEMP         
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �    TEMP_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  Ƹ   TEMP_ADJUSTED            
      	   	long_name         $Sea temperature in-situ ITS-90 scale   standard_name         sea_water_temperature      units         degree_Celsius     	valid_min         �      	valid_max         B      C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  �`   TEMP_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 �  ��   TEMP_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         degree_Celsius     C_format      %10.3f     FORTRAN_format        F10.3      
resolution        :�o   
_FillValue        G�O�     �  ��   PSAL         
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 8   PSAL_QC          
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � 1�   PSAL_ADJUSTED            
      	   	long_name         Practical salinity     standard_name         sea_water_salinity     units         psu    	valid_min         @      	valid_max         B$     C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � 9x   PSAL_ADJUSTED_QC         
         	long_name         quality flag   conventions       Argo reference table 2     
_FillValue                 � X   PSAL_ADJUSTED_ERROR          
         	long_name         VContains the error on the adjusted values as determined by the delayed mode QC process     units         psu    C_format      %10.4f     FORTRAN_format        F10.4      
resolution        9Q�   
_FillValue        G�O�     � _�   	PARAMETER               	            	long_name         /List of parameters with calibration information    conventions       Argo reference table 3     
_FillValue                  ` ~P   SCIENTIFIC_CALIB_EQUATION               	            	long_name         'Calibration equation for this parameter    
_FillValue                   ~�   SCIENTIFIC_CALIB_COEFFICIENT            	            	long_name         *Calibration coefficients for this equation     
_FillValue                   ��   SCIENTIFIC_CALIB_COMMENT            	            	long_name         .Comment applying to this parameter calibration     
_FillValue                   ��   SCIENTIFIC_CALIB_DATE               	             	long_name         Date of calibration    conventions       YYYYMMDDHHMISS     
_FillValue                  T ��   HISTORY_INSTITUTION                      	long_name         "Institution which performed action     conventions       Argo reference table 4     
_FillValue                   �   HISTORY_STEP                     	long_name         Step in data processing    conventions       Argo reference table 12    
_FillValue                   �   HISTORY_SOFTWARE                     	long_name         'Name of software which performed action    conventions       Institution dependent      
_FillValue                   �   HISTORY_SOFTWARE_RELEASE                     	long_name         2Version/release of software which performed action     conventions       Institution dependent      
_FillValue                   �   HISTORY_REFERENCE                        	long_name         Reference of database      conventions       Institution dependent      
_FillValue                  � �$   HISTORY_DATE                      	long_name         #Date the history record was created    conventions       YYYYMMDDHHMISS     
_FillValue                   ��   HISTORY_ACTION                       	long_name         Action performed on data   conventions       Argo reference table 7     
_FillValue                   ��   HISTORY_PARAMETER                        	long_name         (Station parameter action is performed on   conventions       Argo reference table 3     
_FillValue                    ��   HISTORY_START_PRES                    	long_name          Start pressure action applied on   
_FillValue        G�O�   units         decibar        ��   HISTORY_STOP_PRES                     	long_name         Stop pressure action applied on    
_FillValue        G�O�   units         decibar        ��   HISTORY_PREVIOUS_VALUE                    	long_name         +Parameter/Flag previous value before action    
_FillValue        G�O�       ��   HISTORY_QCTEST                       	long_name         <Documentation of tests performed, tests failed (in hex form)   conventions       EWrite tests performed when ACTION=QCP$; tests failed when ACTION=QCF$      
_FillValue                    � Argo profile    3.1 1.2 19500101000000  20210305163602  20211015173717  5905790 5905790 US ARGO PROJECT                                                 US ARGO PROJECT                                                 DEAN ROEMMICH                                                   DEAN ROEMMICH                                                   PRES            TEMP            PSAL            PRES            TEMP            PSAL               j   jAA  AOAO7824_008764_106                 7824_008764_106                 2C  2C  DD  SOLO_II                         SOLO_II                         8764                            8764                            V2.6; SBE602 06Mar19            V2.6; SBE602 06Mar19            853 853 @�c&R�-�@�c&R�-�11  @�c&��p;@�c&��p;@6�(����@6�(�����d�3�	W�d�3�	W11  GPS     GPS     BA  BA  FF  Primary sampling: averaged [nominal   2 dbar binned data sampled at 1.0 Hz from a SBE41CP; bin detail from 0 dbar (number bins/bin width):   10/  1; 500/  2;remaining/  2]                                                                                     Near-surface sampling: discrete, pumped [data sampled at 0.5Hz from the same SBE41CP]                                                                                                                                                                                 ?�  @�\@B�\@�G�@�  @��R@�G�A ��A  A ��A,(�A@  A`��A�Q�A�  A�  A�Q�A�Q�A�Q�A�  A�Q�B   B�
B  B(�B (�B(  B0(�B8(�B@(�BH(�BP(�BX  B_�
Bh  Bp  Bw�
B�  B�  B��B��B�  B�  B��B��B�  B�{B�{B��B��
B��B�{B�  B��B��B��B�  B��B��B��B�  B�  B��B�  B�  B�  B��B��
B��C 
=C  C��C
=C
=C
  C
=C��C  C  C  C��C  C
=C  C  C   C"
=C$  C&  C'��C*  C+��C-��C0  C2
=C4
=C6
=C8  C:  C<  C>  C@
=CB  CD  CE��CG��CJ  CK��CM��CP  CR  CT
=CV
=CX  CZ  C\  C^  C`  Cb  Cd  Ce��Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw��Cz  C|  C~  C�  C�  C�  C�  C�C�C���C���C�  C�  C�C�  C�  C�C�
=C�  C���C���C���C���C�  C�C�  C���C�  C�  C�  C�  C�  C�  C�C�C���C���C���C�  C�  C�  C�  C���C���C���C�  C�C�C�  C�  C���C�  C���C���C�  C���C���C���C�C�C�C�C�  C�
=C�C���C���C�  C�  C�  C�  C�  C���C�  C�  C���C�  C�C�C�  C�C�C�C�  C�  C�  C�  C���C�  C�C�  C�C�C�  C�  C�  C���C�  C�C�  C�C�C�C�  C�  C�  C���C���C���C�  C�  C�  C�  C���C���C���C�  C���C���C�  C�C�C�C�  C�  C�  C�C���C���C�  C���C���D � D ��D}qD  D}qD  D��D�D�D�D}qD�qD}qD��D� D�D��D�qD	z�D	��D
}qD�D}qD�qD� D�D� D  D}qD  D��D  D� D  D� D  D� D  D� D  D� D  D��D�D��D  D� D  D� D�D��D  D� D�qD� D�D� D�qD��D  D� D  D� D   D � D!�D!� D"  D"��D#  D#� D$  D$� D$�qD%� D&�D&� D'  D'��D'�qD(� D)  D)}qD)�qD*� D+�D+� D,  D,� D-  D-��D.  D.� D.�qD/}qD0  D0��D1  D1}qD1��D2z�D3  D3��D4�D4� D5  D5}qD5�qD6}qD6�qD7� D8�D8}qD9  D9��D:�D:� D:�qD;� D<�D<��D=�D=� D>�D>��D?  D?� D@  D@� DA  DA� DB�DB� DB�qDC}qDC�qDDz�DD�qDE��DF  DF}qDG  DG}qDH  DH��DH�qDI� DJ  DJ� DJ�qDK� DL�DL��DM�DM� DN  DN� DO�DO��DO�qDP}qDQ�DQ��DR�DR��DR�qDS}qDT  DT}qDU  DU��DV�DV� DW  DW��DX�DX� DX�qDY� DZ�DZ}qD[  D[� D[�qD\}qD\��D]z�D^  D^�D_  D_� D_�qD`}qDa  Da� Db  Db� Db�qDc� Dd  Dd}qDe  De�Df�Df��Dg  Dg� Dh  Dh��Dh�qDi� Dj�Dj��Dk  Dk}qDl  Dl��Dm�Dm}qDn  Dn� Dn�qDo}qDo�qDp� Dq  Dq}qDq�qDr��Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dv�qDw}qDw�qDx}qDx�qDy}qDz  Dz}qDz�qD{}qD|  D|� D}  D}� D~  D~� D  D� D�HD�AHD��HD��HD�HD�B�D��HD�� D�HD�@ D�~�D���D�  D�@ D�� D�� D�HD�AHD�� D��HD��D�@ D�~�D�� D�HD�@ D�� D���D���D�@ D��HD��HD��D�AHD�� D��HD�  D�>�D�� D��HD�  D�>�D�� D�� D�HD�@ D�~�D���D�  D�>�D�~�D�� D�  D�@ D�~�D���D�  D�AHD�� D�� D�  D�AHD��HD�� D�HD�AHD�� D�� D�  D�>�D�� D��HD�HD�AHD��HD��HD�  D�>�D�~�D�� D���D�>�D�� D��HD�  D�>�D�� D��HD�HD�@ D��HD���D�  D�@ D��HD��HD�HD�>�D�}qD�� D�HD�@ D�� D�� D�  D�@ D�� D�� D���D�>�D�~�D���D���D�@ D���D�� D���D�@ D��HD�� D���D�@ D��HD�D�  D�@ D�� D���D���D�@ D�� D�� D�HD�AHD�� D���D�  D�AHD�� D���D�  D�AHD�� D�� D���D�@ D��HD��HD�  D�@ D�� D�� D���D�>�D�� D��HD�HD�@ D��HD��HD�HD�AHD��HD��HD�HD�AHD�� D�� D�  D�AHD�� D���D���D�>�D�~�D���D���D�@ D�� D��HD��D�AHD�� D�� D��qD�@ D�� D���D�  D�AHD���D��HD�  D�>�D�~�D�� D�HD�AHD��HD�� D�  D�AHD�� D�� D��D�@ D�~�D���D�  D�>�D�� D��HD�  D�>�D�}qD�� D�HD�AHD�~�D��HD��D�B�D��HD��HD�  D�@ D�� D�� D�  D�B�D�� D���D�  D�AHD�� D���D�HD�B�D�� D���D���D�AHD��HD�D�  D�>�D�� D�� D���D�=qD�~�D���D���D�@ D�� D���D���D�AHD�� D�� D�  D�@ D�� D�� D�  D�>�D D�� D�  D�>�D�~�Dþ�D�  D�AHDāHD�� D���D�@ Dŀ Dž�D�  D�@ Dƀ D��HD�HD�@ Dǀ D��HD�HD�>�DȀ D��HD�  D�@ D�~�D�� D�  D�@ Dʀ D��HD�HD�@ D�~�D˾�D�  D�@ D́HD��HD�  D�AHD̀ D;�D���D�@ D�~�Dξ�D�  D�>�D�~�D�� D�HD�AHDЀ D�� D�  D�@ D�~�DѾ�D�  D�AHDҀ D�� D�  D�@ DӀ D�� D�HD�@ DԀ D��HD�  D�@ DՀ D�� D�  D�@ D�~�DֽqD��qD�@ DׁHD�� D�  D�AHD؀ D�� D���D�@ DفHD��HD�  D�@ DځHD�D�HD�@ Dۀ D۾�D���D�@ D܁HD�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�>�D�~�D�� D�  D�@ D�� D�� D�  D�AHD� DᾸD���D�@ D�HD�� D�  D�@ D� D㾸D�  D�AHD� D侸D�  D�@ D� D�� D���D�=qD�~�D�� D�HD�@ D� D羸D��qD�@ D�~�D�� D�HD�>�D�}qD龸D���D�>�D� D�� D�HD�AHD�HD��HD�HD�B�D삏D��HD��qD�>�D�HD�� D�HD�AHD� D�� D�HD�B�D�HD��HD�  D�@ D�~�D�qD���D�@ D�HD�� D��qD�>�D�HD��HD�  D�0�>���?�?u?���?��?��H@\)@&ff@@  @O\)@fff@}p�@�=q@�z�@�  @��@�
=@�G�@���@��H@��
@�\)@��HA33A��A�RAz�A��A\)A%A*�HA0  A6ffA<(�A@��AG
=AL(�AQ�AW�A]p�Ac�
Ah��Ao\)AuA{�A���A�(�A�
=A��A��A�  A��HA�A�Q�A��
A�ffA���A�(�A�
=A��A�z�A��A�=qA��A��A��HA�p�A���A�33A�ffA���A��
A�
=Aљ�A�z�A׮A�=qA�p�A�  A��HA�{A��A�33A�{A��A�\A��A�\)A��A��
A�{B (�B�B�B
=B(�B�B{B33B(�B	�B
=qB\)B(�B�B=qB33B(�B�B�B�HB  B��B�B�RB�B��B��B�\B�Bz�Bp�BffB�B z�B!G�B"=qB#\)B$(�B%�B&{B'
=B(Q�B)�B*=qB+33B,(�B-G�B.ffB/\)B0Q�B1G�B2ffB3\)B4z�B5��B6�\B7�B8��B9B:�HB;�
B=�B>{B?
=B@(�BAG�BB=qBC\)BDz�BEp�BF�\BG�BH��BIBJ�RBK�
BL��BMBN�HBP  BP��BQ�BR�RBS�
BT��BUBV�HBW�BX��BYBZ�RB[�B\z�B]p�B^�\B_�B`��Ba��Bb�\Bc�Bd��BeBf�RBg�Bh��BiBj�HBk�
Bl��Bn{Bo
=BpQ�Bqp�Br�\Bs\)Btz�Bu��Bv�RBw�
Bx��Bz{B{33B|Q�B}p�B~ffB�B�=qB���B�\)B��B�z�B���B��B�  B��\B��B���B�(�B��RB�G�B��
B�ffB��HB�p�B�  B��\B��B��B�=qB��RB�G�B��
B�ffB��HB�p�B�  B��\B�
=B���B�(�B���B�33B��B�=qB���B�\)B��
B�ffB���B��B�{B��\B��B��B�Q�B���B�G�B��B�ffB���B��B�{B��\B��B��B�Q�B���B�\)B��
B�z�B���B�p�B�{B���B�33B�B�(�B��RB�G�B��
B�ffB��HB�p�B��B�z�B�
=B���B�{B���B�33B��B�=qB���B�G�B��
B�ffB���B��B�{B��\B��B��B�=qB��RB�G�B��B�ffB���B��B�{B���B��B�B�=qB���B�G�B��B�Q�B���B��B�  B��\B��B���B�(�B��RB�G�B��
B�Q�B��HBÅB�{Bģ�B��B�B�Q�B���B�\)B��B�z�B�
=BɅB�{Bʣ�B�33BˮB�Q�B���B�\)B��B�ffB���Bϙ�B�{BУ�B�33B�B�Q�B���B�p�B�  Bԣ�B��B��
B�ffB���Bי�B�(�B��HB�p�B�  Bڣ�B�G�B��
B�z�B�
=B�B�Q�B��HB߅B�(�B���B�p�B�{B��B�G�B��B��B�33B��
B�ffB�
=B�B�Q�B���B�B�(�B�RB�p�B�  B�\B�G�B��B�z�B��B��
B�z�B�
=B�B�z�B��B�B�ffB�
=B�B�ffB�
=B��B�ffB�
=B�B�Q�B�
=B���B�=qB��HB��B�=qB��HB�p�C 
=C \)C �RC  CQ�C�RC  C\)C�C
=C\)C�RC
=Cp�C�RC{CffCC�Cp�C��C33C�C�
C33C�C�C	=qC	��C	��C
G�C
��C
��CG�C��C�C33Cz�CC
=CG�Cz�CC��C33Cp�C�C�HC�C\)C�\C�
C
=C=qCz�CC�C(�CffC��C�
C{CG�C�CC��C(�Cp�C��C�
C�CQ�C�\C��C  C33Cp�C�C�HC�CQ�C��C��C
=CG�Cz�C�C�C33CffC��C�HC�C\)C�\CC
=C=qCz�C�C��C(�CffC��C�C�C\)C��C�
C{CQ�C�CC  C33Cp�C�RC�C�CffC�C�HC �C ffC ��C �
C!{C!Q�C!�\C!��C"
=C"G�C"z�C"�RC#  C#33C#ffC#�C#�HC$(�C$Q�C$��C$�
C%
=C%G�C%z�C%�RC%�C&(�C&\)C&��C&�
C'  C'G�C'z�C'�RC'�C((�C(\)C(��C(��C)
=C)=qC)p�C)�C)�HC*{C*Q�C*�C*�C*��C+�C+\)C+�C+��C+��C,(�C,\)C,�\C,��C-  C-33C-ffC-��C-�
C.
=C.=qC.z�C.��C.�
C/{C/G�C/p�C/�C/�C0�C0G�C0�\C0C0��C1(�C1ffC1��C1��C2  C2=qC2z�C2�C2�HC3�C3Q�C3�\C3C3��C433C4p�C4��C4�
C5
=C5=qC5z�C5�C5�C6(�C6\)C6�\C6C7  C733C7p�C7�C7�C8�C8Q�C8�\C8C9  C9=qC9z�C9�C9�HC:�C:Q�C:�\C:��C:��C;33C;p�C;�C;�C<(�C<\)C<�\C<��C=  C=G�C=�C=�RC=��C>33C>p�C>�C>�HC?�C?Q�C?�\C?��C@
=C@G�C@�C@�RC@��CA33CAp�CA��CA�HCB{CB\)CB��CB��CC
=CCQ�CC�CCCD  CD=qCDp�CDCD�CE(�CEffCE��CE�CF�CF\)CF��CF�HCG�CG\)CG��CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                               ?�  @�\@B�\@�G�@�  @��R@�G�A ��A  A ��A,(�A@  A`��A�Q�A�  A�  A�Q�A�Q�A�Q�A�  A�Q�B   B�
B  B(�B (�B(  B0(�B8(�B@(�BH(�BP(�BX  B_�
Bh  Bp  Bw�
B�  B�  B��B��B�  B�  B��B��B�  B�{B�{B��B��
B��B�{B�  B��B��B��B�  B��B��B��B�  B�  B��B�  B�  B�  B��B��
B��C 
=C  C��C
=C
=C
  C
=C��C  C  C  C��C  C
=C  C  C   C"
=C$  C&  C'��C*  C+��C-��C0  C2
=C4
=C6
=C8  C:  C<  C>  C@
=CB  CD  CE��CG��CJ  CK��CM��CP  CR  CT
=CV
=CX  CZ  C\  C^  C`  Cb  Cd  Ce��Ch  Cj  Cl  Cn  Cp  Cr  Ct  Cv  Cw��Cz  C|  C~  C�  C�  C�  C�  C�C�C���C���C�  C�  C�C�  C�  C�C�
=C�  C���C���C���C���C�  C�C�  C���C�  C�  C�  C�  C�  C�  C�C�C���C���C���C�  C�  C�  C�  C���C���C���C�  C�C�C�  C�  C���C�  C���C���C�  C���C���C���C�C�C�C�C�  C�
=C�C���C���C�  C�  C�  C�  C�  C���C�  C�  C���C�  C�C�C�  C�C�C�C�  C�  C�  C�  C���C�  C�C�  C�C�C�  C�  C�  C���C�  C�C�  C�C�C�C�  C�  C�  C���C���C���C�  C�  C�  C�  C���C���C���C�  C���C���C�  C�C�C�C�  C�  C�  C�C���C���C�  C���C���D � D ��D}qD  D}qD  D��D�D�D�D}qD�qD}qD��D� D�D��D�qD	z�D	��D
}qD�D}qD�qD� D�D� D  D}qD  D��D  D� D  D� D  D� D  D� D  D� D  D��D�D��D  D� D  D� D�D��D  D� D�qD� D�D� D�qD��D  D� D  D� D   D � D!�D!� D"  D"��D#  D#� D$  D$� D$�qD%� D&�D&� D'  D'��D'�qD(� D)  D)}qD)�qD*� D+�D+� D,  D,� D-  D-��D.  D.� D.�qD/}qD0  D0��D1  D1}qD1��D2z�D3  D3��D4�D4� D5  D5}qD5�qD6}qD6�qD7� D8�D8}qD9  D9��D:�D:� D:�qD;� D<�D<��D=�D=� D>�D>��D?  D?� D@  D@� DA  DA� DB�DB� DB�qDC}qDC�qDDz�DD�qDE��DF  DF}qDG  DG}qDH  DH��DH�qDI� DJ  DJ� DJ�qDK� DL�DL��DM�DM� DN  DN� DO�DO��DO�qDP}qDQ�DQ��DR�DR��DR�qDS}qDT  DT}qDU  DU��DV�DV� DW  DW��DX�DX� DX�qDY� DZ�DZ}qD[  D[� D[�qD\}qD\��D]z�D^  D^�D_  D_� D_�qD`}qDa  Da� Db  Db� Db�qDc� Dd  Dd}qDe  De�Df�Df��Dg  Dg� Dh  Dh��Dh�qDi� Dj�Dj��Dk  Dk}qDl  Dl��Dm�Dm}qDn  Dn� Dn�qDo}qDo�qDp� Dq  Dq}qDq�qDr��Ds  Ds� Dt  Dt� Du  Du� Dv  Dv� Dv�qDw}qDw�qDx}qDx�qDy}qDz  Dz}qDz�qD{}qD|  D|� D}  D}� D~  D~� D  D� D�HD�AHD��HD��HD�HD�B�D��HD�� D�HD�@ D�~�D���D�  D�@ D�� D�� D�HD�AHD�� D��HD��D�@ D�~�D�� D�HD�@ D�� D���D���D�@ D��HD��HD��D�AHD�� D��HD�  D�>�D�� D��HD�  D�>�D�� D�� D�HD�@ D�~�D���D�  D�>�D�~�D�� D�  D�@ D�~�D���D�  D�AHD�� D�� D�  D�AHD��HD�� D�HD�AHD�� D�� D�  D�>�D�� D��HD�HD�AHD��HD��HD�  D�>�D�~�D�� D���D�>�D�� D��HD�  D�>�D�� D��HD�HD�@ D��HD���D�  D�@ D��HD��HD�HD�>�D�}qD�� D�HD�@ D�� D�� D�  D�@ D�� D�� D���D�>�D�~�D���D���D�@ D���D�� D���D�@ D��HD�� D���D�@ D��HD�D�  D�@ D�� D���D���D�@ D�� D�� D�HD�AHD�� D���D�  D�AHD�� D���D�  D�AHD�� D�� D���D�@ D��HD��HD�  D�@ D�� D�� D���D�>�D�� D��HD�HD�@ D��HD��HD�HD�AHD��HD��HD�HD�AHD�� D�� D�  D�AHD�� D���D���D�>�D�~�D���D���D�@ D�� D��HD��D�AHD�� D�� D��qD�@ D�� D���D�  D�AHD���D��HD�  D�>�D�~�D�� D�HD�AHD��HD�� D�  D�AHD�� D�� D��D�@ D�~�D���D�  D�>�D�� D��HD�  D�>�D�}qD�� D�HD�AHD�~�D��HD��D�B�D��HD��HD�  D�@ D�� D�� D�  D�B�D�� D���D�  D�AHD�� D���D�HD�B�D�� D���D���D�AHD��HD�D�  D�>�D�� D�� D���D�=qD�~�D���D���D�@ D�� D���D���D�AHD�� D�� D�  D�@ D�� D�� D�  D�>�D D�� D�  D�>�D�~�Dþ�D�  D�AHDāHD�� D���D�@ Dŀ Dž�D�  D�@ Dƀ D��HD�HD�@ Dǀ D��HD�HD�>�DȀ D��HD�  D�@ D�~�D�� D�  D�@ Dʀ D��HD�HD�@ D�~�D˾�D�  D�@ D́HD��HD�  D�AHD̀ D;�D���D�@ D�~�Dξ�D�  D�>�D�~�D�� D�HD�AHDЀ D�� D�  D�@ D�~�DѾ�D�  D�AHDҀ D�� D�  D�@ DӀ D�� D�HD�@ DԀ D��HD�  D�@ DՀ D�� D�  D�@ D�~�DֽqD��qD�@ DׁHD�� D�  D�AHD؀ D�� D���D�@ DفHD��HD�  D�@ DځHD�D�HD�@ Dۀ D۾�D���D�@ D܁HD�� D�  D�@ D݀ D�� D�  D�@ Dހ D�� D�  D�>�D�~�D�� D�  D�@ D�� D�� D�  D�AHD� DᾸD���D�@ D�HD�� D�  D�@ D� D㾸D�  D�AHD� D侸D�  D�@ D� D�� D���D�=qD�~�D�� D�HD�@ D� D羸D��qD�@ D�~�D�� D�HD�>�D�}qD龸D���D�>�D� D�� D�HD�AHD�HD��HD�HD�B�D삏D��HD��qD�>�D�HD�� D�HD�AHD� D�� D�HD�B�D�HD��HD�  D�@ D�~�D�qD���D�@ D�HD�� D��qD�>�D�HD��HD�  G�O�>���?�?u?���?��?��H@\)@&ff@@  @O\)@fff@}p�@�=q@�z�@�  @��@�
=@�G�@���@��H@��
@�\)@��HA33A��A�RAz�A��A\)A%A*�HA0  A6ffA<(�A@��AG
=AL(�AQ�AW�A]p�Ac�
Ah��Ao\)AuA{�A���A�(�A�
=A��A��A�  A��HA�A�Q�A��
A�ffA���A�(�A�
=A��A�z�A��A�=qA��A��A��HA�p�A���A�33A�ffA���A��
A�
=Aљ�A�z�A׮A�=qA�p�A�  A��HA�{A��A�33A�{A��A�\A��A�\)A��A��
A�{B (�B�B�B
=B(�B�B{B33B(�B	�B
=qB\)B(�B�B=qB33B(�B�B�B�HB  B��B�B�RB�B��B��B�\B�Bz�Bp�BffB�B z�B!G�B"=qB#\)B$(�B%�B&{B'
=B(Q�B)�B*=qB+33B,(�B-G�B.ffB/\)B0Q�B1G�B2ffB3\)B4z�B5��B6�\B7�B8��B9B:�HB;�
B=�B>{B?
=B@(�BAG�BB=qBC\)BDz�BEp�BF�\BG�BH��BIBJ�RBK�
BL��BMBN�HBP  BP��BQ�BR�RBS�
BT��BUBV�HBW�BX��BYBZ�RB[�B\z�B]p�B^�\B_�B`��Ba��Bb�\Bc�Bd��BeBf�RBg�Bh��BiBj�HBk�
Bl��Bn{Bo
=BpQ�Bqp�Br�\Bs\)Btz�Bu��Bv�RBw�
Bx��Bz{B{33B|Q�B}p�B~ffB�B�=qB���B�\)B��B�z�B���B��B�  B��\B��B���B�(�B��RB�G�B��
B�ffB��HB�p�B�  B��\B��B��B�=qB��RB�G�B��
B�ffB��HB�p�B�  B��\B�
=B���B�(�B���B�33B��B�=qB���B�\)B��
B�ffB���B��B�{B��\B��B��B�Q�B���B�G�B��B�ffB���B��B�{B��\B��B��B�Q�B���B�\)B��
B�z�B���B�p�B�{B���B�33B�B�(�B��RB�G�B��
B�ffB��HB�p�B��B�z�B�
=B���B�{B���B�33B��B�=qB���B�G�B��
B�ffB���B��B�{B��\B��B��B�=qB��RB�G�B��B�ffB���B��B�{B���B��B�B�=qB���B�G�B��B�Q�B���B��B�  B��\B��B���B�(�B��RB�G�B��
B�Q�B��HBÅB�{Bģ�B��B�B�Q�B���B�\)B��B�z�B�
=BɅB�{Bʣ�B�33BˮB�Q�B���B�\)B��B�ffB���Bϙ�B�{BУ�B�33B�B�Q�B���B�p�B�  Bԣ�B��B��
B�ffB���Bי�B�(�B��HB�p�B�  Bڣ�B�G�B��
B�z�B�
=B�B�Q�B��HB߅B�(�B���B�p�B�{B��B�G�B��B��B�33B��
B�ffB�
=B�B�Q�B���B�B�(�B�RB�p�B�  B�\B�G�B��B�z�B��B��
B�z�B�
=B�B�z�B��B�B�ffB�
=B�B�ffB�
=B��B�ffB�
=B�B�Q�B�
=B���B�=qB��HB��B�=qB��HB�p�C 
=C \)C �RC  CQ�C�RC  C\)C�C
=C\)C�RC
=Cp�C�RC{CffCC�Cp�C��C33C�C�
C33C�C�C	=qC	��C	��C
G�C
��C
��CG�C��C�C33Cz�CC
=CG�Cz�CC��C33Cp�C�C�HC�C\)C�\C�
C
=C=qCz�CC�C(�CffC��C�
C{CG�C�CC��C(�Cp�C��C�
C�CQ�C�\C��C  C33Cp�C�C�HC�CQ�C��C��C
=CG�Cz�C�C�C33CffC��C�HC�C\)C�\CC
=C=qCz�C�C��C(�CffC��C�C�C\)C��C�
C{CQ�C�CC  C33Cp�C�RC�C�CffC�C�HC �C ffC ��C �
C!{C!Q�C!�\C!��C"
=C"G�C"z�C"�RC#  C#33C#ffC#�C#�HC$(�C$Q�C$��C$�
C%
=C%G�C%z�C%�RC%�C&(�C&\)C&��C&�
C'  C'G�C'z�C'�RC'�C((�C(\)C(��C(��C)
=C)=qC)p�C)�C)�HC*{C*Q�C*�C*�C*��C+�C+\)C+�C+��C+��C,(�C,\)C,�\C,��C-  C-33C-ffC-��C-�
C.
=C.=qC.z�C.��C.�
C/{C/G�C/p�C/�C/�C0�C0G�C0�\C0C0��C1(�C1ffC1��C1��C2  C2=qC2z�C2�C2�HC3�C3Q�C3�\C3C3��C433C4p�C4��C4�
C5
=C5=qC5z�C5�C5�C6(�C6\)C6�\C6C7  C733C7p�C7�C7�C8�C8Q�C8�\C8C9  C9=qC9z�C9�C9�HC:�C:Q�C:�\C:��C:��C;33C;p�C;�C;�C<(�C<\)C<�\C<��C=  C=G�C=�C=�RC=��C>33C>p�C>�C>�HC?�C?Q�C?�\C?��C@
=C@G�C@�C@�RC@��CA33CAp�CA��CA�HCB{CB\)CB��CB��CC
=CCQ�CC�CCCD  CD=qCDp�CDCD�CE(�CEffCE��CE�CF�CF\)CF��CF�HCG�CG\)CG��CG�
G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                               @�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�@�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�A�dZA�l�A�r�A�r�A�r�A�l�A�p�A�v�A�r�A�p�A�p�A�v�A�v�A�z�A�z�A�z�A�|�A�~�A�~�A�z�A��A��A��A��A��+A��+A��A�t�A�r�A�n�A�l�A�^5A�M�A�E�A�VA��
A�E�A�+A� �A�1A��A���A��PA��TA�dZA�A�A���A��HA��7A���A��FA�hsA��A�\)A���A��A�-A��A�$�A�\)A��A���A�ffA���A��^A�JA��TA�^5A�bNA�hsA��A���A�O�A��DA�^5A��^A��+A�oA�|�A���A�7LA�{A�+A�A�|�A���A�Q�A���A��wA�  A��\A���A�XA�A�A���A��`A�$�A��FA�
=A���A��\A�r�A�bNA�9XA�A��#A���A���A���A�A�A��uA���A%A}%A|ffA{��A{33Az1'Aw�-At{AsXAr5?An��Am�7Al{Aj�uAhĜAg�7Af1'Af �Ae��Ae?}Ad��AdA�Ac�AcVAb1AahsA`�A`jA^��A\{AYAX�AWdZAV�uAUoAR�AP��AN��AL�AK+AI|�AI/AHz�AG|�AFv�AEdZAD��AC"�AAhsA@ �A?S�A=O�A;�^A:��A8�A6�yA6�A5%A3S�A0�RA-��A,��A,�A+x�A*�/A*(�A*1A)A)��A)��A)�PA)�A)&�A'`BA&�!A%�^A$E�A#�FA"ȴA �HA ��A bNA E�A 5?A  �A�AhsAz�A��A��A;dA��A�A��A�A�RA�yAA�RA��A��AĜA�jAZA$�A�TA\)A�HA�RA��AjA�A�+A�A �y@���@�&�@�bN@��@�r�@��
@��@�S�@�^5@�?}@�@�=q@�@��@�h@�O�@�V@�j@�A�@�l�@��H@�J@��T@��@��@��@�j@���@��;@�V@�J@�7L@�z�@�(�@�1@ۍP@�33@ڧ�@�J@١�@��@���@�I�@�\)@��@��@Ӿw@ҧ�@�$�@�z�@ΰ!@͉7@���@��/@�Ĝ@�z�@�Z@�Z@�9X@��@�1@��@˕�@�33@�
=@ʸR@�^5@ɺ^@�A�@��;@Ǯ@�dZ@�K�@��y@�E�@�M�@Ł@�p�@�p�@�`B@�7L@��/@ě�@�Z@��@�~�@���@� �@��^@�p�@��@��@�9X@���@�"�@��@��H@�n�@���@�bN@�b@��
@��P@���@���@��!@�=q@���@��@�&�@���@��@�r�@�Q�@�Q�@�1'@��@���@���@���@�t�@�S�@�33@��H@�n�@�p�@��;@��@�o@��+@�{@��h@�x�@��@���@��@���@��u@��D@�r�@�I�@�  @��;@�ƨ@���@�K�@�o@�@��@���@���@��@��#@�x�@�7L@�/@�&�@��`@��D@�A�@�1@���@��F@���@�dZ@�"�@���@��R@���@�v�@�^5@�5?@��^@��h@�V@��/@�Q�@�S�@���@��y@���@�~�@�@���@�ȴ@��T@��h@�v�@�@�33@��@�M�@���@���@���@�~�@�5?@��-@�&�@�1@�C�@��y@���@��y@�ȴ@�V@�@���@�hs@��@�I�@���@�;d@�~�@�-@���@�hs@���@�A�@��@�1@���@�b@� �@���@��m@��;@��F@��@���@���@�v�@�V@�=q@�5?@�$�@�J@���@�`B@��@���@��@�Ĝ@���@��D@�r�@�bN@�Q�@� �@���@��;@��
@�ƨ@���@��P@�\)@�+@��@��y@���@�~�@�M�@��@���@��7@�/@��@��@���@��9@�A�@�  @���@��@��;@��
@��@���@��@�t�@�+@��@��\@�ff@��@��@��T@���@��-@���@�`B@�&�@��@��`@��/@��j@���@��D@�z�@�r�@�bN@�bN@�bN@�Z@�Z@�A�@�1@�P@K�@~�y@~��@~��@~V@}�@}�h@|��@|�@|1@{�F@{�@{t�@{@z^5@z=q@z�@y��@x��@x�@w�P@w�@v�@v��@u�T@uO�@uV@t�/@t9X@s��@r�\@rJ@q��@q&�@pĜ@pQ�@o�w@o|�@n��@nE�@n{@l��@l9X@l1@k��@j��@j-@ix�@iG�@h�`@h��@h �@g�w@gl�@gK�@g
=@f�y@fV@e�@e�h@eO�@eV@dI�@d1@c�m@c�
@c�@c33@co@b�@b�!@b��@a�#@a7L@a%@`Ĝ@` �@_�w@_�@^�@^��@^{@]��@^{@]�@]`B@\�/@\��@\�D@\Z@[��@[33@Z�\@Z^5@Y��@Y�7@Yx�@YG�@X�`@X  @W�@V�@VE�@U�@U�T@U��@U@U��@U?}@T��@T�@T9X@S��@S�m@Sƨ@S��@S�@SdZ@S33@So@R��@RM�@R�@RJ@Q�^@Q��@Q��@Qx�@QG�@P�`@P��@P�u@P�u@P�@Pr�@PA�@Pb@O�@O+@M��@MV@L�/@L�@LZ@LI�@L(�@L1@K�m@K�F@Kt�@KS�@KS�@KS�@J�@J��@J=q@I��@I�^@I7L@H��@H��@H��@H1'@G�@G�P@G|�@G;d@F��@Fv�@F@E`B@D�@D�j@Dz�@DI�@C��@CS�@C"�@Bn�@B=q@B�@BJ@BJ@A��@A��@A�@A�#@A��@Ax�@AX@AG�@A&�@@��@@�u@@�@@r�@@bN@@A�@@A�@@1'@@b@?l�@?
=@>��@>�y@>�y@>�y@>��@>ff@>5?@=�T@=O�@<��@<1@;S�@;"�@;@:�@:�H@:��@:n�@:-@8Ĝ@7;d@7\)@7K�@7
=@6��@6ȴ@6ff@6{@5p�@4�@4�D@4�D@4z�@4(�@3�F@3t�@3S�@3"�@3o@2�@2��@2^5@2J@1��@1%@0�9@/�;@/K�@/
=@/
=@.��@.ff@.{@-�@-�@-?}@-V@,�/@,z�@,9X@,�@,1@+��@*�H@*��@*��@*��@*n�@*=q@)��@)�^@)��@)x�@(�`@(r�@(A�@(b@'�@'�;@'�;@'�w@'�P@'\)@'\)@'K�@'+@&��@&��@&�+@&V@&$�@&$�@&{@&{@%�@%�T@%��@%��@%�-@%p�@$z�@$1@#��@#��@#�
@#��@#�@#�@#�@#�@#dZ@#o@#@"�@"�@"�H@"�\@"M�@"J@!��@!hs@ Ĝ@ �@ r�@ Q�@ 1'@  �@�P@�P@|�@l�@K�@+@
=@
=@ȴ@�R@�R@��@�+@$�@��@�@?}@V@�@�@Z@ƨ@��@C�@�@��@=q@X@G�@�@Ĝ@�@ �@  @  @�@��@�P@�@�@��@�+@V@5?@5?@�T@`B@�@��@�/@�j@z�@9X@��@��@�m@�F@��@t�@S�@S�@C�@33@33@33@"�@33@33@33@33@33@33@33@33@33@"�@�H@�!@�\@~�@~�@^5@M�@��@��@��@x�@x�@X@&�@%@�`@Ĝ@�@�@�@�u@�u@�@r�@r�@Q�@Q�@A�@1'@1'@ �@b@  @�@  @�@�;@�;A�S�A�XA�hsA�jA�hsA�jA�p�A�p�A�p�A�t�A�t�A�p�A�r�A�t�A�t�A�n�A�jA�jA�n�A�r�A�p�A�r�A�v�A�t�A�t�A�r�A�r�A�r�A�n�A�n�A�p�A�x�A�v�A�v�A�v�A�r�A�r�A�p�A�r�A�x�A�r�A�z�A�z�A�z�A�x�A�z�A�x�A�z�A�~�A�|�A�z�A�x�A�x�A�|�A�|�A�|�A�z�A�x�A�x�A�z�A�|�A�~�A�~�A�|�A�z�A�~�A�~�A��A�~�A�|�A�|�A�|�A�|�A�~�A��A��A�|�A�|�A�x�A�v�A�z�A�|�A�|�A��A��+A��A��A��A��+A��A��A��A��A�~�A��A��A��A��A��+A��A��A��A��+A��A��A��+A��7A��A��A��+A��A��A��A��7A��A��A��+A��7A��A��A��7A��7A��A��+A��7A��+A��A��A��7A��7A��7A��A��A�~�A�z�A�t�A�r�A�t�A�v�A�v�A�t�A�r�A�r�A�t�A�t�A�r�A�r�A�r�A�t�A�t�A�r�A�p�A�p�A�t�A�n�A�jA�n�A�n�A�jA�l�A�n�A�n�A�jA�l�A�n�A�l�A�l�A�jA�ffA�`BA�`BA�^5A�VA�Q�A�Q�A�S�A�K�A�M�A�M�A�VA�O�A�C�A�=qA�-A���A�|�A�?}A��yA��/A��jA��FA���A�n�A�5?A�VA���A��#A���A�\)A��A�%A��A��`A��FA��A�p�A�jA�`BA�S�A�E�A�33A�(�A�&�A�(�A�&�A�&�A�-A�+A�(�A�-A�-A�&�A�$�A�$�A� �A��A��A��A�oA�oA�VA�
=A�1A���A���A��A��/A��A��#A���A���A���A���A���A���A�ĜA�A���A��9A��!A��A���A���A���A��\A��PA�x�A�G�A�$�A�VA�JA��;A�ƨA���A�|�A�t�A�n�A�hsA�bNA�\)A�ZA�VA�O�A�O�A�M�A�E�A�E�A�;dA�-A�&�A��A�1A���A��A��A��A��A��yA��A��yA��mA��mA��A���A���A��RA��A���A��PA�jA�ZA�I�A�+A�
=A�  A���A��yA��`A��HA��;A��/A���A��jA��RA���A��DA��A�x�A�n�A�l�A�jA�bNA�ZA�\)A�K�A�/A�"�A��A�A���A���A��A�ȴA���A�r�A�"�A�oA�A��A��;A���A��A���A��uA��A�p�A�(�A��9A���A��hA��A��A��A�~�A�~�A�|�A�\)A�/A��A���A��uA�bNA�A�A� �A��A���A��\A�^5A�ƨA�ZA�(�A�oA��A��^A�t�A��A�/A��A�1A�$�A�JA���A���A��A��A��yA��HA��/A��
A���A���A���A�ȴA��wA���A�(�A�A�VA��A���A��A�dZA��A�1A��A���A���A��7A�M�A��A��!A�;dA�-A��A��yA���A�E�A���A�JA�JA�A���A��`A��
A���A��9A��hA��PA��A�ZA�$�A���A��A��\A�ffA�M�A�-A� �A��A�VA��A�ȴA�M�A�z�A��`A�1'A�ȴA�z�A�33A���A���A�M�A�bA���A��+A��A���A��A�bNA�A�A�(�A�oA�%A���A��^A�v�A�(�A��;A�A���A�z�A�O�A�A��FA�M�A�%A�ȴA��uA�l�A�1'A���A���A���A��hA��+A�n�A�`BA�K�A�1'A�1'A�5?A�1'A�oA�  A�A�%A�A��;A���A���A��A���A�ffA�G�A�G�A�A�A�33A��A���A��;A�ȴA��A�z�A�Q�A�33A��9A��A�p�A�XA�C�A�(�A�1A��A���A��A���A��uA�x�A�/A���A�ĜA�p�A�ZA�G�A�;dA�5?A�1'A�1'A�+A��A��A�  A���A�^5A���A���A���A���A��A��A���A���A��PA��PA�~�A�n�A�dZA�bNA�ZA�Q�A�5?A���A���A���A���A��7A�t�A�l�A�dZA�bNA�VA�O�A�K�A�I�A�?}A�-A��A�bA�%A���A��A��mA��mA��;A��A��
A��
A���A�ȴA���A��!A���A��uA�l�A�A�A��A�VA���A��HA��A���A���A���A��jA��-A��!A���A��uA��+A�t�A�O�A�7LA�&�A��A��yA��TA���A���A�^5A��A�A��TA���A�n�A�n�A�S�A�I�A�;dA�-A�"�A�oA�%A��;A��9A��uA�x�A�\)A�+A��9A��DA��A�+A�
=A�A���A�z�A�v�A�ffA�\)A�Q�A�K�A�33A���A��A��/A���A��RA��9A��!A��A���A��+A�^5A�I�A�$�A���A��9A��-A��hA�K�A���A�ƨA���A��A�z�A�r�A�ffA�\)A�=qA��A�oA�A���A���A��mA��HA��/A���A���A���A�ȴA�ȴA�A���A��wA��^A��FA��A���A���A�t�A�r�A�t�A�t�A�t�A�v�A�r�A�t�A�r�A�n�A�p�A�l�A�n�A�hsA�dZA�jA�jA�jA�bNA�S�A�Q�A�G�A�C�A�;dA�=qA�;dA�7LA�-A�/A�(�A�$�A��A�{A�
=A��A��A��yA��;A��HA��/A��HA��;G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                               A�dZA�l�A�r�A�r�A�r�A�l�A�p�A�v�A�r�A�p�A�p�A�v�A�v�A�z�A�z�A�z�A�|�A�~�A�~�A�z�A��A��A��A��A��+A��+A��A�t�A�r�A�n�A�l�A�^5A�M�A�E�A�VA��
A�E�A�+A� �A�1A��A���A��PA��TA�dZA�A�A���A��HA��7A���A��FA�hsA��A�\)A���A��A�-A��A�$�A�\)A��A���A�ffA���A��^A�JA��TA�^5A�bNA�hsA��A���A�O�A��DA�^5A��^A��+A�oA�|�A���A�7LA�{A�+A�A�|�A���A�Q�A���A��wA�  A��\A���A�XA�A�A���A��`A�$�A��FA�
=A���A��\A�r�A�bNA�9XA�A��#A���A���A���A�A�A��uA���A%A}%A|ffA{��A{33Az1'Aw�-At{AsXAr5?An��Am�7Al{Aj�uAhĜAg�7Af1'Af �Ae��Ae?}Ad��AdA�Ac�AcVAb1AahsA`�A`jA^��A\{AYAX�AWdZAV�uAUoAR�AP��AN��AL�AK+AI|�AI/AHz�AG|�AFv�AEdZAD��AC"�AAhsA@ �A?S�A=O�A;�^A:��A8�A6�yA6�A5%A3S�A0�RA-��A,��A,�A+x�A*�/A*(�A*1A)A)��A)��A)�PA)�A)&�A'`BA&�!A%�^A$E�A#�FA"ȴA �HA ��A bNA E�A 5?A  �A�AhsAz�A��A��A;dA��A�A��A�A�RA�yAA�RA��A��AĜA�jAZA$�A�TA\)A�HA�RA��AjA�A�+A�A �y@���@�&�@�bN@��@�r�@��
@��@�S�@�^5@�?}@�@�=q@�@��@�h@�O�@�V@�j@�A�@�l�@��H@�J@��T@��@��@��@�j@���@��;@�V@�J@�7L@�z�@�(�@�1@ۍP@�33@ڧ�@�J@١�@��@���@�I�@�\)@��@��@Ӿw@ҧ�@�$�@�z�@ΰ!@͉7@���@��/@�Ĝ@�z�@�Z@�Z@�9X@��@�1@��@˕�@�33@�
=@ʸR@�^5@ɺ^@�A�@��;@Ǯ@�dZ@�K�@��y@�E�@�M�@Ł@�p�@�p�@�`B@�7L@��/@ě�@�Z@��@�~�@���@� �@��^@�p�@��@��@�9X@���@�"�@��@��H@�n�@���@�bN@�b@��
@��P@���@���@��!@�=q@���@��@�&�@���@��@�r�@�Q�@�Q�@�1'@��@���@���@���@�t�@�S�@�33@��H@�n�@�p�@��;@��@�o@��+@�{@��h@�x�@��@���@��@���@��u@��D@�r�@�I�@�  @��;@�ƨ@���@�K�@�o@�@��@���@���@��@��#@�x�@�7L@�/@�&�@��`@��D@�A�@�1@���@��F@���@�dZ@�"�@���@��R@���@�v�@�^5@�5?@��^@��h@�V@��/@�Q�@�S�@���@��y@���@�~�@�@���@�ȴ@��T@��h@�v�@�@�33@��@�M�@���@���@���@�~�@�5?@��-@�&�@�1@�C�@��y@���@��y@�ȴ@�V@�@���@�hs@��@�I�@���@�;d@�~�@�-@���@�hs@���@�A�@��@�1@���@�b@� �@���@��m@��;@��F@��@���@���@�v�@�V@�=q@�5?@�$�@�J@���@�`B@��@���@��@�Ĝ@���@��D@�r�@�bN@�Q�@� �@���@��;@��
@�ƨ@���@��P@�\)@�+@��@��y@���@�~�@�M�@��@���@��7@�/@��@��@���@��9@�A�@�  @���@��@��;@��
@��@���@��@�t�@�+@��@��\@�ff@��@��@��T@���@��-@���@�`B@�&�@��@��`@��/@��j@���@��D@�z�@�r�@�bN@�bN@�bN@�Z@�Z@�A�@�1@�P@K�@~�y@~��@~��@~V@}�@}�h@|��@|�@|1@{�F@{�@{t�@{@z^5@z=q@z�@y��@x��@x�@w�P@w�@v�@v��@u�T@uO�@uV@t�/@t9X@s��@r�\@rJ@q��@q&�@pĜ@pQ�@o�w@o|�@n��@nE�@n{@l��@l9X@l1@k��@j��@j-@ix�@iG�@h�`@h��@h �@g�w@gl�@gK�@g
=@f�y@fV@e�@e�h@eO�@eV@dI�@d1@c�m@c�
@c�@c33@co@b�@b�!@b��@a�#@a7L@a%@`Ĝ@` �@_�w@_�@^�@^��@^{@]��@^{@]�@]`B@\�/@\��@\�D@\Z@[��@[33@Z�\@Z^5@Y��@Y�7@Yx�@YG�@X�`@X  @W�@V�@VE�@U�@U�T@U��@U@U��@U?}@T��@T�@T9X@S��@S�m@Sƨ@S��@S�@SdZ@S33@So@R��@RM�@R�@RJ@Q�^@Q��@Q��@Qx�@QG�@P�`@P��@P�u@P�u@P�@Pr�@PA�@Pb@O�@O+@M��@MV@L�/@L�@LZ@LI�@L(�@L1@K�m@K�F@Kt�@KS�@KS�@KS�@J�@J��@J=q@I��@I�^@I7L@H��@H��@H��@H1'@G�@G�P@G|�@G;d@F��@Fv�@F@E`B@D�@D�j@Dz�@DI�@C��@CS�@C"�@Bn�@B=q@B�@BJ@BJ@A��@A��@A�@A�#@A��@Ax�@AX@AG�@A&�@@��@@�u@@�@@r�@@bN@@A�@@A�@@1'@@b@?l�@?
=@>��@>�y@>�y@>�y@>��@>ff@>5?@=�T@=O�@<��@<1@;S�@;"�@;@:�@:�H@:��@:n�@:-@8Ĝ@7;d@7\)@7K�@7
=@6��@6ȴ@6ff@6{@5p�@4�@4�D@4�D@4z�@4(�@3�F@3t�@3S�@3"�@3o@2�@2��@2^5@2J@1��@1%@0�9@/�;@/K�@/
=@/
=@.��@.ff@.{@-�@-�@-?}@-V@,�/@,z�@,9X@,�@,1@+��@*�H@*��@*��@*��@*n�@*=q@)��@)�^@)��@)x�@(�`@(r�@(A�@(b@'�@'�;@'�;@'�w@'�P@'\)@'\)@'K�@'+@&��@&��@&�+@&V@&$�@&$�@&{@&{@%�@%�T@%��@%��@%�-@%p�@$z�@$1@#��@#��@#�
@#��@#�@#�@#�@#�@#dZ@#o@#@"�@"�@"�H@"�\@"M�@"J@!��@!hs@ Ĝ@ �@ r�@ Q�@ 1'@  �@�P@�P@|�@l�@K�@+@
=@
=@ȴ@�R@�R@��@�+@$�@��@�@?}@V@�@�@Z@ƨ@��@C�@�@��@=q@X@G�@�@Ĝ@�@ �@  @  @�@��@�P@�@�@��@�+@V@5?@5?@�T@`B@�@��@�/@�j@z�@9X@��@��@�m@�F@��@t�@S�@S�@C�@33@33@33@"�@33@33@33@33@33@33@33@33@33@"�@�H@�!@�\@~�@~�@^5@M�@��@��@��@x�@x�@X@&�@%@�`@Ĝ@�@�@�@�u@�u@�@r�@r�@Q�@Q�@A�@1'@1'@ �@b@  @�@  @�@�;G�O�A�S�A�XA�hsA�jA�hsA�jA�p�A�p�A�p�A�t�A�t�A�p�A�r�A�t�A�t�A�n�A�jA�jA�n�A�r�A�p�A�r�A�v�A�t�A�t�A�r�A�r�A�r�A�n�A�n�A�p�A�x�A�v�A�v�A�v�A�r�A�r�A�p�A�r�A�x�A�r�A�z�A�z�A�z�A�x�A�z�A�x�A�z�A�~�A�|�A�z�A�x�A�x�A�|�A�|�A�|�A�z�A�x�A�x�A�z�A�|�A�~�A�~�A�|�A�z�A�~�A�~�A��A�~�A�|�A�|�A�|�A�|�A�~�A��A��A�|�A�|�A�x�A�v�A�z�A�|�A�|�A��A��+A��A��A��A��+A��A��A��A��A�~�A��A��A��A��A��+A��A��A��A��+A��A��A��+A��7A��A��A��+A��A��A��A��7A��A��A��+A��7A��A��A��7A��7A��A��+A��7A��+A��A��A��7A��7A��7A��A��A�~�A�z�A�t�A�r�A�t�A�v�A�v�A�t�A�r�A�r�A�t�A�t�A�r�A�r�A�r�A�t�A�t�A�r�A�p�A�p�A�t�A�n�A�jA�n�A�n�A�jA�l�A�n�A�n�A�jA�l�A�n�A�l�A�l�A�jA�ffA�`BA�`BA�^5A�VA�Q�A�Q�A�S�A�K�A�M�A�M�A�VA�O�A�C�A�=qA�-A���A�|�A�?}A��yA��/A��jA��FA���A�n�A�5?A�VA���A��#A���A�\)A��A�%A��A��`A��FA��A�p�A�jA�`BA�S�A�E�A�33A�(�A�&�A�(�A�&�A�&�A�-A�+A�(�A�-A�-A�&�A�$�A�$�A� �A��A��A��A�oA�oA�VA�
=A�1A���A���A��A��/A��A��#A���A���A���A���A���A���A�ĜA�A���A��9A��!A��A���A���A���A��\A��PA�x�A�G�A�$�A�VA�JA��;A�ƨA���A�|�A�t�A�n�A�hsA�bNA�\)A�ZA�VA�O�A�O�A�M�A�E�A�E�A�;dA�-A�&�A��A�1A���A��A��A��A��A��yA��A��yA��mA��mA��A���A���A��RA��A���A��PA�jA�ZA�I�A�+A�
=A�  A���A��yA��`A��HA��;A��/A���A��jA��RA���A��DA��A�x�A�n�A�l�A�jA�bNA�ZA�\)A�K�A�/A�"�A��A�A���A���A��A�ȴA���A�r�A�"�A�oA�A��A��;A���A��A���A��uA��A�p�A�(�A��9A���A��hA��A��A��A�~�A�~�A�|�A�\)A�/A��A���A��uA�bNA�A�A� �A��A���A��\A�^5A�ƨA�ZA�(�A�oA��A��^A�t�A��A�/A��A�1A�$�A�JA���A���A��A��A��yA��HA��/A��
A���A���A���A�ȴA��wA���A�(�A�A�VA��A���A��A�dZA��A�1A��A���A���A��7A�M�A��A��!A�;dA�-A��A��yA���A�E�A���A�JA�JA�A���A��`A��
A���A��9A��hA��PA��A�ZA�$�A���A��A��\A�ffA�M�A�-A� �A��A�VA��A�ȴA�M�A�z�A��`A�1'A�ȴA�z�A�33A���A���A�M�A�bA���A��+A��A���A��A�bNA�A�A�(�A�oA�%A���A��^A�v�A�(�A��;A�A���A�z�A�O�A�A��FA�M�A�%A�ȴA��uA�l�A�1'A���A���A���A��hA��+A�n�A�`BA�K�A�1'A�1'A�5?A�1'A�oA�  A�A�%A�A��;A���A���A��A���A�ffA�G�A�G�A�A�A�33A��A���A��;A�ȴA��A�z�A�Q�A�33A��9A��A�p�A�XA�C�A�(�A�1A��A���A��A���A��uA�x�A�/A���A�ĜA�p�A�ZA�G�A�;dA�5?A�1'A�1'A�+A��A��A�  A���A�^5A���A���A���A���A��A��A���A���A��PA��PA�~�A�n�A�dZA�bNA�ZA�Q�A�5?A���A���A���A���A��7A�t�A�l�A�dZA�bNA�VA�O�A�K�A�I�A�?}A�-A��A�bA�%A���A��A��mA��mA��;A��A��
A��
A���A�ȴA���A��!A���A��uA�l�A�A�A��A�VA���A��HA��A���A���A���A��jA��-A��!A���A��uA��+A�t�A�O�A�7LA�&�A��A��yA��TA���A���A�^5A��A�A��TA���A�n�A�n�A�S�A�I�A�;dA�-A�"�A�oA�%A��;A��9A��uA�x�A�\)A�+A��9A��DA��A�+A�
=A�A���A�z�A�v�A�ffA�\)A�Q�A�K�A�33A���A��A��/A���A��RA��9A��!A��A���A��+A�^5A�I�A�$�A���A��9A��-A��hA�K�A���A�ƨA���A��A�z�A�r�A�ffA�\)A�=qA��A�oA�A���A���A��mA��HA��/A���A���A���A�ȴA�ȴA�A���A��wA��^A��FA��A���A���A�t�A�r�A�t�A�t�A�t�A�v�A�r�A�t�A�r�A�n�A�p�A�l�A�n�A�hsA�dZA�jA�jA�jA�bNA�S�A�Q�A�G�A�C�A�;dA�=qA�;dA�7LA�-A�/A�(�A�$�A��A�{A�
=A��A��A��yA��;A��HA��/A��HA��;G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�11111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111141111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111                                                                                                                                                                                                                                               ;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��;��G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�B��B�AB�uB�AB�uB��B�B�B�AB�uB�B��B�B��B��B��B�B�B�B�;B��B�;B�;B�;B�;B�;B��B�4B� B� B�B�iB~�B�IB��B�B��B�yB�"B��B�WB�B�QB�B�B�`B�|B�B�,B�B��B��B��B�]B�JB�DB�B�B�B��B�aB�CB�OB�(B�B��B|B|Bt�Bu�B^5BM6B:^B7�B2aB'�B:B�B�B �B��B�B��B��B��B��B�_B�B��B�7B�uB�B{JBn/BO�BB�B8�B)*BB�BMBB�B�B
�BYBB�cB�2B�QB�[B�LB�*B�IB��B��B��B�SBxBa|BZ�BS[BEmB7�B0!B*�BCB�B�B�B	BB
��B
�xB
�B
��B
��B
�sB
�B
�jB
�9B
�RB
�0B
��B
��B
��B
�B
��B
��B
yrB
m)B
gmB
]�B
Y�B
WsB
R�B
K^B
F�B
>�B
:�B
0�B
)_B
$@B
"hB
�B
(B

rB
 �B	�B	�B	�;B	�fB	�B	�KB	�
B	�,B	�[B	�B	�pB	�B	��B	̘B	��B	ʌB	��B	ǮB	�[B	B	�BB	��B	��B	��B	�hB	��B	��B	��B	�UB	��B	�CB	�0B	��B	�nB	�bB	��B	�OB	��B	��B	��B	��B	��B	��B	��B	�{B	�;B	��B	~�B	}�B	}"B	{�B	zDB	y�B	y>B	w�B	w�B	v`B	s�B	r�B	l�B	jB	jB	h
B	e�B	c�B	c�B	b�B	c�B	d�B	c�B	l"B	q�B	poB	qAB	p�B	qB	qvB	rB	sB	u�B	x�B	x8B	w�B	w�B	y�B	y�B	{B	|�B	~�B	�4B	�;B	��B	��B	��B	��B	�MB	��B	��B	��B	��B	�7B	��B	��B	�\B	�{B	��B	��B	�FB	�0B	��B	��B	��B	�B	��B	�XB	��B	�XB	�*B	�eB	��B	��B	��B	��B	�CB	��B	�B	�'B	�XB	�qB	��B	�B	˒B	̘B	�B	�aB	�QB	��B	�B	�B	�B	��B	�`B	�`B	��B	�DB	�B	�B	��B	�DB	��B	��B	�5B	�oB	��B	�B	��B	�rB	�B	��B	��B	�"B	��B
�B
�B
�B
�B
�B
�B
�B
�B
:B
�B
�B
�B
�B
SB
$B
�B
�B
�B
�B
	B
IB
!B
$�B
+B
-B
/�B
2aB
49B
6FB
6FB
8�B
9�B
:^B
:�B
:�B
;0B
;�B
<�B
>�B
?�B
@OB
A�B
C-B
D3B
D�B
D�B
EmB
FtB
I�B
K)B
N�B
PHB
P}B
P�B
S�B
ZB
]�B
`B
bB
cTB
d&B
e�B
i�B
m�B
l�B
m�B
o�B
pB
rGB
y>B
zxB
|�B
}"B
cB
�B
�B
��B
��B
��B
�B
��B
~]B
�B
�B
��B
��B
��B
��B
�B
�B
��B
��B
�(B
��B
�PB
��B
��B
�:B
�B
��B
�	B
�B
�'B
��B
��B
�B
��B
��B
��B
��B
�!B
�UB
�tB
�B
�tB
�B
��B
��B
�$B
�6B
��B
�}B
��B
�OB
�'B
��B
ÖB
�B
��B
��B
��B
��B
��B
��B
�B
��B
�TB
��B
�[B
��B
�B
֡B
�sB
רB
��B
��B
ٴB
�B
�KB
�B
چB
یB
�B
ޞB
�jB
�B
��B
�NB
�&B
�B
�B
�sB
�DB
��B
�)B
��B
��B
�GB
��B
��B
�B
��B
�%B
�`B
��B
�2B
��B
�>B
�xB
��B
�VB
�.B
��B
��B 4B �B �BAB{BBMBMB�BSB�B�B�BYBYB%B�B�B+B�B	lB	�B
�B
�BDB�B�BB(B.B�B�B�B�BoB�B�B�B@BFB{B�B�BYBYB�B�B�BqB�B�B!-B!�B"4B"4B#B$@B%zB&B&�B'�B'�B*�B*eB*eB+B,B-CB.�B/�B/�B0UB1�B2-B2�B2�B33B3hB49B49B4�B4�B5?B6�B7LB7�B7�B8B8B8B8RB8RB7�B9�B:�B:�B<B<6B<�B=B>B?B?�B@�BB�BC�BDgBD3BD3BC�BDgBC�BC�BC�BC-BC�BB�BB�BB�BB�BCaBB�BC�BDgBEBEBE9BEmBEBF?BF�BF�BG�BH�BH�BH�BIBIBIRBI�BI�BI�BK^BK�BK�BL0BK�BL0BMjBNpBPBPHBP}BPHBPHBP}BP�BP}BQNBQ�BS�BS�BTaBT�BT�BT�BT�BT�BT�BT�BUgBUgBT�BT�BU�BVBVBV�BW
BW�BWsBV�BW�BX�BZQBZQBY�BY�BZ�B[WBZ�B[�B[�B[�B[�B[�B\�B]dB]/B^B^jB^B^5B^jB^jB^B^jB^5B^5B^�B^5B^5B^B^�B^�B_;B_;B_pB_�B_�B_pB_�B`B_�B_�B`BB`�B`vBa�Ba|BbBa�Bb�Bc�Bc�Bc�Bc�Bd&Bd&Bc�Bc�Bc�BbNBd&Bb�Bc�Bc�Bc�Bc�Bc�BcTBcTBd&BcTBcTBc Bb�BcTBc�Bc�Bc�Bd�Bd�Bd�Be�Be`Be�Be�Bf2Bf2BgBhsBh�BhsBiyBiyBi�Bi�BjKBjBj�Bj�Bk�Bk�Bk�BlWBm�Bm�BncBn�Bm�Bn�Bn�Bo Bo5BoiBoiBqBqvBq�BrBr�Br�BrGBsBs�Bs�Bs�Bs�BtTBtBtTBs�Bt�Bt�Bt�Bt�Bt�Bt�Bu%Bt�Bt�Bt�Bt�Bv+Bv+Bv+Bu�Bv�Bw2BwfBwfBwfBv�BwfBw�Bw�BxBw�Bw2BwfBwfBw�Bx�By�BzxBzxBzxBz�Bz�Bz�B{JBzDBz�B{B{�B{�B{�B{B|B{B{�B{�B{�B|�B|PB|�B}VB}"B}"B}"B}VB~(B}�B~(B~]B~�BcB�iB�4B��B��B�;B�;B��B�;B�B�;B�B�;B�oB�uB��B��B��B�uB��B�GB��B�B�MB��B��B��B��B�B�SB�B��B�SB�SB�SB��B��B��B��B�YB�%B��B��B�SB��B��B��B��B��B��B�YB�YB��B�+B��B�_B��B��B��B��B�fB��B��B��B��B��B�B�lB�7B�lB�B�7B�B�7B�7B�7B�7B�7B�7B�7B�7B�lB�7B��B�7B��B��B�	B��B�{B�B�oB��B�B��B��B�B��B��B�GB��B��B��B�uB��B�B�oB�uB�uB�B��B��B��B�oB��B��B��B�uB�B.B�B�B��B�{B�uB�{B�B�B�uB�iB��B�uB�uB�B�B�oB�B�B�oB�uB�uB�;B�4B��B�oB�uB�uB�oB�4B� B� B��B��B��B��B� B��B�oB��B�AB��B��B� B��B�oB��B�B��B�B�4B� B~(B�iB�B�AB�;B��B��B�;B�B��B�B�iB�4B�B��B�iB�;B�AB�oB��B�oB�B�;B��B�oB�B��B�;B�AB��B� B�B�B�iB��B�uB�B��B��B�B�;B�iB�;B��B��B�B�B��B�;B�uB�B�iB��B��B�iB~�B~�B�4B��B��B�B�B�iB��B��B.B~�B�4B�B�B.B�B�B�BcB��B�BcB.B��BcB~�B~�B�BcB�B�;B�4BcB�B��B� B~�B�B�B~�B}�B.B�B~�B�B�7B�hB�nB�'B�4B��B�_B��B�B��B��B�wB��B̘B�B�;B��B�B�HB��B�B�KB�8B�B�B�B�B�yB�>B�B�B�DB��B�yB�B�yB��B��B��B�B�WB�]B�B��B� B�cB��B�B��B�B�/B�/B��B�WB�KB�WB�WB�B�B�B��B�"B�B��B�"B��B�B��B�yB�B�B�B�]B�;B�B�B�B� B��B�B�"B�B�
B��B��B�mB��B�B�B�`B�`B��B��B�B�&B��B�HB�ZB�B�B�HB��B�pB�BߤB�B�vB��B�B��B�B�B��B�B��B��B�>B�KB�vB��B�MB�ZB��B�B��B�+B��B�2B�	B�fB�B�B�B�B�B��B��B�B�B�rB��B�(B�PB�JB�"B��B�rB��B �B�.B��B  B��B�B�"B��B��B�B�B�xB�rB�DB
	B��B��B�DB��B�B�B�B�B�;B��B�B�JB�B��B�B�AB�]B�B�B�B�B	7B�B�B�B�ZB�&B�B�B�B�B�B��B�zB�FB�3B�?B��B�UB�[B��B�OB�IB��B�CB�qB�qB�-B�BB��B�B��B��B�B�7B��B��B�B��B��B��B�fB�B�B}�BqvBt�Bu�B�B��B��B�B|PB|�B}"B{B|�ByrBv+By>BwfBy	B�iB|�B.Bu%Bv+Bw2Bs�Bq�BpBn�Bk�BpBi�B�1B��B�Bh�B\]BV�BWsBT�BL0BT�BIBJ�BK�BK�BA�B;dB=<B9$B9$B5tB33B4B<jB9�B=<B5B2�B2aB2-B/�B7B0!B3�B($B$�B&�BVB �BCBB�B�B�B�B�BbBBB	�B~BPBxB	B	�B�B\B�B	B�BDBB_BB�BGB �B�B�B��B%B��B�DB��B1B�5B�B��B�yB�yB�DB�2B��B�B��B��B�B�|B�|B�;B��B՛B�NBѷB��B�pB�B�jB͟BʌB˒B��B�9BĜB��B�XB�}B�0B�^B�jB�B�XB��B��B�B��B��B�tB�?B��B�B�LB�-B�=B�wB�qB��B�eB��B�*B�eB�$B��B�B�kB�$B�B�B��B�:B��B��B�:B�-B��B��B��B��B�!B��B�B��B��B��B�IB�1B�	B��B��B�B��B��B�B�uB�B�FB�B��B�4B�B��B��B��B��B��B��B��B��B��B��B�{B�DB�uBxlBv�BxlBu�Bv�Bu%Br�Bu%Bf�BzxBm]BjBh>BjBm�B^�B��BW?BT�BP�BK�BJ�BH�BH�BF�BG�BE�BI�BH�B@�BE�BAUB@�B=�B<�B;dB<BB�B:�B8�B<6B=B0UB-wB2-B:�B/OB-wB'RB&�B!�B!�B!�B �B"�B \B�B�BqB	BCBB�BeB_B_B�B$B�BYBSB�BYBMBYB�BYBoB B�BoB4BBhB:B�B�BB�B�B BVB B4B�B B�B.B�B�B~B�B�B�B�BPB~BJB~B
rBB�BfB�B%B�BSB�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�44444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444444                                                                                                                                                                                                                                               G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�G�O�PRES            TEMP            PSAL            PRES            TEMP            PSAL                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          SIO SOLO floats auto-correct mild pressure drift by zeroing the pressure sensor while on the surface. Additional correction was unnecessary in DMQC;                                                   PRES_ADJ_ERR: SBE sensor accuracy + resolution error     No significant temperature drift detected;                                                                                                                                                             TEMP_ADJ_ERR: SBE sensor accuracy + resolution error     Salinity drift determined to be uncorrectable in DMQC;                                                                                                                                                                                                          202110151929172021101519291720211015192917202110151929172021101519291720211015192917SI  SI  ARFMARFM                                                                                                                                                2021030516360220210305163602IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021031517005420210315170054QCP$QCP$                                G�O�G�O�G�O�G�O�G�O�G�O�5F03E           703E            AO  AO  ARGQARGQQCPLQCPL                                                                                                                                        2021031517005420210315170054QCF$QCF$                                G�O�G�O�G�O�G�O�G�O�G�O�8000            0               SI  SI  ARFMARFM                                                                                                                                                2021101413074520211014130745IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�V3.1 profile    V3.1 profile    SI  SI  ARCAARCASIQCSIQCV2.1V2.1                                                                                                                                2021101519292020211015192920IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARSQARSQOWC OWC V1.1V1.1CTD_for_DMQC_2021V01                                            CTD_for_DMQC_2021V01                                            2021101519292020211015192920IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                SI  SI  ARDUARDU                                                                                                                                                2021101519292020211015192920IP  IP                                  G�O�G�O�G�O�G�O�G�O�G�O�                                